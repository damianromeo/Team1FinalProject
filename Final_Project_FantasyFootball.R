#############################Librarys#####################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinysky)
library(sqldf)
library(DT)
library("devtools")
require("devtools")
library(fmsb)
library(plotly)


#############################Data Wrangling##############################################
#setwd("C:/Users/MPDAV/Desktop/MS BA - Purdue/Summer 2021/R/Final Project/finalproduct")

MADDEN_raw <- read.csv("MADDEN.csv", header = TRUE, sep = ",")


###MADDEN
MADDEN_V1 <- MADDEN_raw%>%
  filter(position=="QB"|position=="HB"|position=="TE"|position=="WR")%>%
  select(1,2,4,5,6,8,9,12,13,14,18,19,20,21,22,23,24,29,32,35,36,37,39,40,41,42,44,45,46,48,53,57,58,59,63)

colnames(MADDEN_V1)<-c('NAME',
                       'POSITION',
                       'OVERALL',
                       'TEAM',
                       'AGE',
                       'AWARENESS',
                       'THROW_POWER',
                       'STRENGTH',
                       'VISION',
                       'CATCH_IN_TRAFFIC',
                       'MEDIUM_ROUTE_RUNNING',
                       'CATCHING',
                       'ACCELERATION',
                       'SPINMOVE',
                       'HEIGHT',
                       'FINESSEMOVES',
                       'SPECTACULAR CATCH',
                       'WEIGHT',
                       'DEEP-ROUTE_RUNNING',
                       'TRUCKING',
                       'THROW_ACCURACY_SHORT',
                       'JUKE_MOVE',
                       'SHORT_ROUTE_RUNNING',
                       'JERSEY_NUM',
                       'BREAK_SACK',
                       'SPEED',
                       'JUMPING',
                       'TOUGHNESS',
                       'THROW_ON_THE_RUN',
                       'STIFF_ARM',
                       'THROW_ACCURACY_MID',
                       'STAMINA',
                       'CARRYING',
                       'BREAK TACKLE',
                       'THROW_UNDER_PRESSURE'
)


rankings=read.csv("VORdata.csv")
rankings <- subset(rankings,position == 'QB' | position == 'DST' | 
                     position == 'RB' | 
                     position == 'WR' | 
                     position == 'TE' | 
                     position == 'K' )









player_ids = as.character(rankings$player)

rankings$player=as.character(rankings$player)

player_ids = as.character(rankings$player)

rankings$player=as.character(rankings$player)






###########################  FUNCTIONS #########################################

####DELETE PLAYER####
delete_player=function(file, player){
  file[file$player!=player,]
}


####GET STARTER Function ####
get_starter=function(file,position){
  subs=file[file$position==position,]
  plot_ly(subs,
          x = ~risk,
          y = ~lower,
          color = ~dropoff,
          type='scatter',
          size = ~points,
          text = ~paste("Name: ",
                        player,
                        "<br>VOR: ",
                        vor,"<br>Points: ",
                        points),
                        mode = "markers",
                        hoverinfo = 'text')
}


#####GET BENCH F#####
get_bench=function(file,position){
  ##high vor & upper
  subs=file[file$position==position,]
  plot_ly(subs,
          x = ~vor,
          y = ~upper,
          color = ~dropoff,
          size = ~points,
          text = ~paste("Name: ",
                        player,
                        "<br> VOR: ",
                        vor),
                        type='scatter',
                        mode="markers",
                        hoverinfo="text")
}






#################################Begin App######################################


################################  UI  ##########################################

ui <- dashboardPage(skin = "green",
                    
                    #### This creates the Header at the top left
                    dashboardHeader(
                      title = "Welcome to Draft Night", titleWidth = 300),
                    
                    #### This creates the crap on the side
                    dashboardSidebar(
                      sidebarMenu(
                        ########Tab 1
                        menuItem("Draft Picks",
                                 tabName = "draft",
                                 icon = icon("football-ball")),
                        ########Tab 2
                        menuItem("MADDEN COMAPRISON",
                                 tabName = "MADDEN",
                                 icon = icon("football-ball"))
                    
                      )
                    ),
                    
                    ####This creates the bodys of the tabs
                    dashboardBody(skin = "green",
                                  tabItems(
########    PAGE 1      ############################################
                                    tabItem(tabName = "draft",
                                            h2("Team Builder"),
                                            fluidRow(
                                              box(column(width=12,
                                                         
                                                         h2("Draft Order Inputs"),
                                                         hr(),
                                                         helpText("Drafted Player from other Team"),
                                                         textInput.typeahead(id="delete_pick",
                                                                             placeholder="Type players name",
                                                                             local=data.frame(name=c(player_ids)),
                                                                             valueKey = "name",
                                                                             limit=5,
                                                                             tokens=c(1:length(player_ids)),
                                                                             template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                                         ),
                                                         actionButton("button2", "Submit"),
                                                         helpText("My Pick"),
                                                         textInput.typeahead(id="add_to_team",
                                                                             placeholder="Type players name",
                                                                             local=data.frame(name=c(player_ids)),
                                                                             valueKey = "name",
                                                                             limit=5,
                                                                             tokens=c(1:length(player_ids)),
                                                                             template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                                         ),
                                                         actionButton("button1", "Submit"),
                                                         helpText("Graph Type"),
                                                         selectInput("starter_type",choices=c("Starter","Bench"),selected="Starter",label=NULL),
                                                         hr(),
                                                        DT::dataTableOutput('team_table'),
                                                        htmlOutput("sum"))),
                                              hr(),
                                              fluidRow(column(width=12,h3("QB"),plotlyOutput("QB")),
                                                       column(width=12,h3("RB"),plotlyOutput("RB"))),
                                              hr(),
                                              fluidRow(column(width=12,h3("WR"),plotlyOutput("WR")),
                                                       column(width=12,h3("TE"),plotlyOutput("TE"))),
                                              hr(),
                                              fluidRow(column(width=12,h3("K"),plotlyOutput("K")),
                                                       column(width=12,h3("DST"),plotlyOutput("DST")))
                                              ,hr(),
                                              
                                              h3("All Available Players"),
                                              DT::dataTableOutput('tbl')),
                                              hr()),

######### PAGE 2 ########

                                tabItem(tabName = "MADDEN",
                                  h2("MADDEN PLAYER COMPARISON"),
                                     fluidRow(
                                         box(column(width=12,
                                           h2("Pick players to compare:"),
                                           hr(),
                                            helpText("Choose a player to compare:"),
                                              textInput.typeahead(id="player1",
                                                   placeholder="Type players name",
                                                   local=data.frame(name=c(player_ids)),
                                                   valueKey = "name",
                                                   limit=5,
                                                   tokens=c(1:length(player_ids)),
                                                   template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                                    ),
                                                   helpText("Choose a player to compare:"),
                                              textInput.typeahead(id="player2",
                                                    placeholder="Choose a second player to compare:",
                                                    local=data.frame(name=c(player_ids)),
                                                    valueKey = "name",
                                                    limit=5,
                                                    tokens=c(1:length(player_ids)),
                                                    template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                                     ),
                                                    hr(),
                                      fluidRow(
                                          plotOutput(outputId = "radarplot",
                                          width = 1000,
                                          height = 1000)
))))))))

                                             




###############################   SERVER   ########################################################
server <- function(input, output) { 
  
####SERVER PAGE 1####
  team=reactiveValues()
  team$df=as.data.frame(NULL)
  rankings_1=reactiveValues()
  rankings_1$df=rankings
  
  observeEvent(input$button2,{
    rankings_1$df=delete_player(rankings_1$df,as.character(input$delete_pick))
  })
  observeEvent(input$button1,{
    adder=rankings_1$df[rankings_1$df$player==input$add_to_team,]
    rankings_1$df=delete_player(rankings_1$df,as.character(input$add_to_team))
    
    team$df=rbind(team$df,adder)
    team$df$position=factor(team$df$position,levels=c("QB","RB","WR","TE","DST","K"))
    team$df=team$df[order(team$df$position,team$df$points),]
    my_players =  datatable(team$df,
                            style = 'bootstrap',
                            extensions = 'FixedColumns',
                            class = "display nowrap",
                            options = list(lengthchange= FALSE,
                                           dom = 't',
                                           scrollX = TRUE,
                                           fixedColumns = TRUE
                            ),
                            rownames= FALSE)
    
    output$team_table = DT::renderDataTable(my_players)
    output$sum=renderUI({
      HTML(paste("<b><h5>Total Team Points:",  round(sum(team$df$points),2),"<br><font color='red'>Lower End Team Points: ",round(sum(team$df$lower),2), "</font><br><font color='green'>Upper End Team Points: ",
                 round(sum(team$df$upper),2),"</font></h5></b>" ))
    })
    
  })
  observe({
    if(input$starter_type=='Starter'){
      output$QB <- renderPlotly({get_starter(rankings_1$df,"QB") %>% config(displayModeBar = F)})
      output$RB <- renderPlotly({get_starter(rankings_1$df,"RB") %>% config(displayModeBar = F)})
      output$WR <- renderPlotly({get_starter(rankings_1$df,"WR") %>% config(displayModeBar = F)})
      output$TE <- renderPlotly({get_starter(rankings_1$df,"TE") %>% config(displayModeBar = F)})
      output$K <- renderPlotly({get_starter(rankings_1$df,"K") %>% config(displayModeBar = F)})
      output$DST <- renderPlotly({get_starter(rankings_1$df,"DST") %>% config(displayModeBar = F)})
    } else if(input$starter_type=='Bench'){
      output$QB <- renderPlotly({get_bench(rankings_1$df,"QB") %>% config(displayModeBar = F)})
      output$RB <- renderPlotly({get_bench(rankings_1$df,"RB") %>% config(displayModeBar = F)})
      output$WR <- renderPlotly({get_bench(rankings_1$df,"WR") %>% config(displayModeBar = F)})
      output$TE <- renderPlotly({get_bench(rankings_1$df,"TE") %>% config(displayModeBar = F)})
      output$K <- renderPlotly({get_bench(rankings_1$df,"K") %>% config(displayModeBar = F)})
      output$DST <- renderPlotly({get_bench(rankings_1$df,"DST") %>% config(displayModeBar = F)})
    }
    
    all_the_players =  datatable(rankings_1$df,filter = 'top',style = 'bootstrap',extensions = 'FixedColumns',
                                 options = list(pageLength = 30,
                                                
                                                scrollX = TRUE,
                                                fixedColumns = TRUE
                                 ),rownames= FALSE)
    output$tbl = DT::renderDataTable(all_the_players)
  })
  

###############################################################################


###### Server Page 2

  output$radarplot<- renderPlot({
    
    
      player1<-input$player1
      player2<-input$player2
      
      #### PLAYER 1 ######
      qb_ovr1<- as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(OVERALL))
      qb_spd1<- as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(SPEED))
      qb_str1<- as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(STRENGTH))
      qb_awr1<- as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(AWARENESS))
      qbacc1<-as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(ACCELERATION))
      qbtuff1<-as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(TOUGHNESS))
      qbstam1<-as.numeric(MADDEN_V1%>%filter(NAME==player1)%>%select(STAMINA))
      #### PLAYER 2 #####
      qb_ovr2<- as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(OVERALL))
      qb_spd2<- as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(SPEED))
      qb_str2<- as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(STRENGTH))
      qb_awr2<- as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(AWARENESS))
      qbacc2<-as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(ACCELERATION))
      qbtuff2<-as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(TOUGHNESS))
      qbstam2<-as.numeric(MADDEN_V1%>%filter(NAME==player2)%>%select(STAMINA))
      #### QB ####
      data_radar <- data.frame(Overall = c(99,0,qb_ovr1,qb_ovr2),
                               Speed = c(99, 0, qb_spd1, qb_spd2),
                               Strength = c(99, 0, qb_str1, qb_str2),
                               Awareness = c(99, 0, qb_awr1, qb_awr2),
                               Acceleration= c(99, 0, qbacc1, qbacc2),
                               Toughness= c(99, 0, qbtuff1, qbtuff2),
                               Stamina= c(99, 0, qbstam1, qbstam2))
      
      ### DEFINE COLOR
      colors_fill<- c(scales::alpha("blue", 0.1),
                      scales::alpha("red", 0.1))
      
      #### DEFINE LINE
      colors_line<- c(scales::alpha("blue", 0.9),
                      scales::alpha("red", 0.9))
      
      
      #### Create Chart
      radarchart(data_radar,
                 seg = 10,
                 title = "Player Comparison",
                 pfcol = colors_fill,
                 pcol = colors_line,
                 plwd = 4)
      
      #####Create Legend
      legend(x=0.3,
             y=1.38,
             legend = c(player1,player2),
             bty="n",
             pch=20,
             col = colors_line,
             cex =.8,
             pt.cex = 2)
  })
  
}





shinyApp(ui, server)
