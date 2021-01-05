library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
require(maps)
library(leaflet)
library(sp)
library(highcharter)
library(scales)

database = read.csv("Rdatabases.csv", header = TRUE,  sep = ",")
database = database[!database$Gender == "Unknown",]

server <- function(input, output, session) {
  
  database = read.csv("Rdatabases.csv", header = TRUE,  sep = ",")
  database = database[!database$Gender == "Unknown",]
  
  View(database)
  
  v <- reactiveValues(value=NULL)
  
  observe({
    c <- input$categories
    t <- input$types
    v$value <- paste(c,t,sep="")
  })
  
  data1 <- reactive({
    req(input$countries)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(CountryName, Gender, Age_Group) %>% filter(CountryName %in% input$countries) %>% filter(Age_Group %in% input$age_group) %>% group_by(CountryName, Gender) %>% summarise(Number_of_User = n())
   
  })
  
  data2 <- reactive({
    req(input$countries)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(Age, CountryName, Age_Group) %>% filter(CountryName %in% input$countries) %>% filter(Age_Group %in% input$age_group)
    df <- within(df, Age_Group <- factor(Age_Group, levels=names(sort(table(Age_Group), 
                                                            decreasing=TRUE))))
  })

  data3 <- reactive({
    req(input$countries)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(UserID, Gender, CountryName, February, March, April, May, June, July, August, September, Age_Group) %>% filter(Age_Group %in% input$age_group)
    df <- df %>% tidyr::gather(month, "value", 4:11) %>% 
      arrange(UserID) %>% 
      filter(CountryName %in% input$countries) %>%
      group_by(month) %>% 
      summarise(size = sum(value, na.rm=TRUE)) 
    
  
  })
  

  
  data4 <- reactive({
    req(input$countries, input$age_group)
    
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(UserID, Age_Group, CountryName, SPbets, CAbets, GAbets, PObets) %>% 
      filter(Age_Group %in% input$age_group) %>% select(-Age_Group) %>%  group_by(CountryName) %>%
      summarise(Casino = sum(CAbets, na.rm=TRUE),Games = sum(GAbets, na.rm=TRUE), Sports = sum(SPbets, na.rm=TRUE), Poker = sum(PObets, na.rm=TRUE))
    
    df <- df %>% tidyr::gather(type, "Number_of_Bets", 2:5) %>% arrange(CountryName) %>%
      filter(CountryName %in% input$countries)
    
  })
  
  dataStakesAmount <- reactive({
    req(input$countries, input$age_group)
    
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(UserID, Age_Group, CountryName, SPstakes, CAstakes, GAstakes, POstakes) %>% 
      filter(Age_Group %in% input$age_group) %>% select(-Age_Group) %>%  group_by(CountryName) %>%
      summarise(Casino = sum(CAstakes, na.rm=TRUE),Games = sum(GAstakes, na.rm=TRUE), Sports = sum(SPstakes, na.rm=TRUE), Poker = sum(POstakes, na.rm=TRUE))
    
    df <- df %>% tidyr::gather(type, "Amount_Stakes", 2:5) %>% arrange(CountryName) %>%
      filter(CountryName %in% input$countries)
    
  })
  
  dataWins <- reactive({
    req(input$countries, input$age_group)
    
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(UserID, Age_Group, CountryName, SPwin, CAwin, GAwin, POwin) %>% 
      filter(Age_Group %in% input$age_group) %>% select(-Age_Group) %>%  group_by(CountryName) %>%
      summarise(Casino = sum(CAwin, na.rm=TRUE),Games = sum(GAwin, na.rm=TRUE), Sports = sum(SPwin, na.rm=TRUE), Poker = sum(POwin, na.rm=TRUE))
    
    df <- df %>% tidyr::gather(type, "Amount_Wins", 2:5) %>% arrange(CountryName) %>%
      filter(CountryName %in% input$countries)
    
  })
  
  
  data5 <- reactive({
    req(input$countries, input$age_group)
    
    df <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% select(UserID, Age_Group, CountryName, SPbets, CAbets, GAbets, PObets) %>%
      filter(Age_Group %in% input$age_group) %>% select(-Age_Group) %>%  group_by(CountryName) %>% filter(CountryName %in% input$countries)
    df[df ==0] <- NA
    
    df <- df %>% summarise(Casino = sum(!is.na(CAbets)),Games = sum(!is.na(GAbets)), Sports = sum(!is.na(SPbets)), Poker = sum(!is.na(PObets)))
    
    df <- df %>% tidyr::gather(type, "Number_of_Player", 2:5) %>% arrange(CountryName) %>%
      filter(CountryName %in% input$countries)
    
  })
  

  
  data6 <- reactive({
    req(input$applicationIDs, input$countries)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs2) %>% filter(CountryName %in% input$countries2) %>% filter(Age_Group %in% input$age_group2)
  })
  
  dataMap <- reactive({
    req(input$applicationIDs, input$countries)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs2) %>% filter(CountryName %in% input$countries2) %>% filter(Age_Group %in% input$age_group2)
    # df$lat <- as.numeric(df$lat)
    # df$lon <- as.numeric(df$lon)
    
    # df$SP <- SpatialPointsDataFrame(df[,c('lat','lon')], df[, -c('lat','lon')])
    
  })


  
  data7 <- reactive({
    req(input$countries, v$value, input$categories, input$types)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs3) %>% filter(Age_Group %in% input$age_group3) %>% group_by(CountryName) %>% filter(CountryName %in% input$countries3)
  })
  
  
  data8 <- reactive({
    req(input$countries, v$value, input$categories, input$types)

    test <- database%>% filter(ApplicationID %in% input$applicationIDs) %>% filter(Age_Group %in% input$age_group) %>% filter(CountryName %in% input$countries) %>% 
      select(UserID, Age_Group,CountryName, LastSp, LastCa, LastGa, LastPo)
    # test$LastPo <- as.Date(test$LastPo,"%Y%m%d")
    
    test$recencyGa = difftime(mdy(09302005),test$LastGa, units="days")
    test$recencyPo = difftime(mdy(09302005),test$LastPo, units="days")
    test$recencyCa = difftime(mdy(09302005),test$LastCa, units="days")
    test$recencySp = difftime(mdy(09302005),test$LastSp, units="days")
    
 
    
    head(test)
    test <- test %>% select(-Age_Group,-LastSp, -LastCa, -LastGa, -LastPo) %>%  group_by(CountryName) %>%
      summarise(Casino = mean(recencyCa, na.rm=TRUE), Games = mean(recencyGa, na.rm=TRUE), 
                Sports = mean(recencySp, na.rm=TRUE), Poker = mean(recencyPo, na.rm=TRUE))
    
    test <- test %>% tidyr::gather(month, "days", 2:5) %>% arrange(CountryName)
    

})
  
  datacompare <- reactive({
    req(input$countries, v$value, input$categories, input$types)
    df <- database %>% filter(ApplicationID %in% input$applicationIDs3) %>% filter(Age_Group %in% input$age_group3) %>% group_by(CountryName) %>% filter(CountryName %in% input$countries3)
  })
  
  totalProfit <- reactive({
    dataWins = dataWins()
    dataStakesAmount = dataStakesAmount()
    wins_stakes <- merge(dataWins,dataStakesAmount,by=c("type","CountryName"))
    
    wins_stakes$Profit <-  wins_stakes$Amount_Stakes  - wins_stakes$Amount_Wins
    
    wins_stakes$Profit <- round(wins_stakes$Profit, digits =0)
    
    wins_stakes[is.na(wins_stakes)] <- 0
    
    total <- sum(wins_stakes$Profit)
    total <- formatC(total, format="f", big.mark=",", digits=1)
    
    
  })
  
  totalUsers <- reactive({
    req(input$countries)
    totalUsers <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% 
                  select(UserID,CountryName, Age_Group, ApplicationID) %>%
                  filter(CountryName %in% input$countries)%>%
                  filter(Age_Group %in% input$age_group)%>%
                  summarise(Number_of_Users = n())
    
    totalUsers <- sum(totalUsers$Number_of_Users)
    totalUsers <- formatC(totalUsers, format="f", big.mark=",", digits=1)
  })
  
  avgAge <- reactive({
    req(input$countries)
    data <- database %>% filter(ApplicationID %in% input$applicationIDs) %>% 
      select(UserID,CountryName,ApplicationID, Age) %>%
      filter(CountryName %in% input$countries)%>%
      summarise(AverageAge = mean(Age))
    
    AverageAge <- sum(data$AverageAge)
    AverageAge <- formatC(AverageAge, format="f", big.mark=",", digits=0)
  })
  
  
  output$totalProfit <- renderText({
    totalProfit ()
  })
  
  output$totalUsers <- renderText({
    totalUsers ()
  })
  
  output$avgAge <- renderText({
    avgAge ()
  })
  
  ########Plot 
  
  output$plot1 <- renderPlot({
    g <- ggplot(data1(), aes( x ="", y = Number_of_User, fill=Gender) )+
                ggtitle("Number of users by gender") +
                theme(axis.title.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.ticks.y=element_blank()) +
      theme_void()+
      scale_fill_manual(values=c("#f6cd61","#517693"))
  
    g + geom_bar(stat = "identity", width=1)+
    coord_polar("y", start=0)
  })
  
  output$plot2 <- renderPlot({
    
    g <- ggplot(data2(), aes(Age_Group))+
      ggtitle("Number of users by Age group")+
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    g + geom_bar(fill = "#517693") 
  })
  
  output$plot3 <- renderPlot({
    g <- ggplot(data3(), aes( y = size, x =reorder(factor(month), -size), group=2)) + 
      ggtitle("Total amount of poker stakes")+
      scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-5)) +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank())
    #   +ylab("Amount stakes") 
    g+  geom_line(color="grey")+ geom_point(shape=21, color="517693", fill="#517693", size=4)
     # g + geom_bar(stat = "identity", fill = "#517693")
     
    
  }) 
  
  
  
  output$plot4 <- renderPlot({
    dataStakesAmount = dataStakesAmount()
    data4 = data4()
    
    data4_dataStakesAmount <- merge(data4,dataStakesAmount,by=c("type","CountryName"))
    
    
    data4_dataStakesAmount$StakesPerBets <-  data4_dataStakesAmount$Amount_Stakes/data4_dataStakesAmount$Number_of_Bets
    data4_dataStakesAmount$StakesPerBets <- round(data4_dataStakesAmount$StakesPerBets, digits =0)
    data4_dataStakesAmount[is.na(data4_dataStakesAmount)] <- 0
    
    g <- ggplot(data4_dataStakesAmount, aes(x=reorder(type, -StakesPerBets), y=StakesPerBets)) +
      ggtitle("Total amount of stakes out of total bets") +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    g + geom_bar(stat = "identity", fill = "#517693") 
  
  })
  
  output$plot5 <- renderPlot({
    data4 = data4()
    data5 = data5()
    data4data5 <- merge(data4,data5,by=c("type","CountryName"))
    
    data4data5$BetsPerNoPlayers <-  data4data5$Number_of_Bets/data4data5$Number_of_Player
    data4data5$BetsPerNoPlayers <- round(data4data5$BetsPerNoPlayers, digits =0)
    data4data5[is.na(data4data5)] <- 0

  
    g <-  ggplot(data4data5, aes(x=reorder(type, -BetsPerNoPlayers), y=BetsPerNoPlayers))+
      ggtitle("Number of bets out of total players") +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    g + geom_bar(stat = "identity",  fill = "#517693") 
  })
  
  output$datacompare <- renderPlot({
    g <- ggplot(datacompare(), aes_string(x="CountryName", y=v$value))
    g + geom_bar(stat = "identity")
  })
  
  output$plot8 <- renderPlot({
    g <- ggplot(data8(), aes(x=reorder(month, -days), y=days)) +
    ggtitle("Average number of days since last bet")  +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    g + geom_bar(stat = "identity",  fill = "#517693") 
  })
  
  output$plot6 <- renderPlot({
    g <- ggplot(data6(), aes_string( y = v$value, x = input$measures)) +
      ggtitle(paste("Total ", v$value, " per " , input$measures))+
      scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-5))  +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    g + geom_bar(stat = "identity",  fill = "#517693")
  })
  

  output$plotMap <- renderLeaflet({
    dataMap <- dataMap()
    leaflet() %>%
      setView(lng = 31.88, lat = -25.02, zoom=2) %>%
      addTiles()%>%
      addCircleMarkers(data = dataMap, lng= ~lon, lat = ~lat, radius=3)
    
      # addPolygons(data = data6()$CountryName)
  })
    
    # countries3
    # g <- ggplot(data6(), aes_string( y = v$value, x = input$measures))
    # g + geom_bar(stat = "identity")

  
  
  output$plot7 <- renderPlot({
    g <- ggplot(data7(), aes_string(x="CountryName", y=v$value))
    g + geom_bar(stat = "identity")
  })
  

  output$plot9 <- renderPlot({
    dataWins = dataWins()
    dataStakesAmount = dataStakesAmount()
    wins_stakes <- merge(dataWins,dataStakesAmount,by=c("type","CountryName"))
    
    wins_stakes$Profit <-  wins_stakes$Amount_Stakes  - wins_stakes$Amount_Wins
    
    wins_stakes$Profit <- round(wins_stakes$Profit, digits =0)
    
    wins_stakes[is.na(wins_stakes)] <- 0
    
    
    g <-  ggplot(wins_stakes, aes(x=reorder(type, -Profit), y=Profit))+
      
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      ggtitle("Profit per gambling typpe") +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    g + geom_bar(stat = "identity",  fill = "#517693") 
  })

}



View(database)
applicachoice <- unique(unlist(strsplit(as.character(database$ApplicationID), ",")))
countrychoice <- unique(unlist(strsplit(as.character(database$CountryName), ",")))
group_age_choice <- unique(unlist(strsplit(as.character(database$Age_Group), ",")))

ui <- dashboardPage(
  dashboardHeader(title ="DASHBOARD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "overview", icon = icon("th")),
      menuItem("Countries analysis", tabName = "mymap", icon = icon("map")),
      menuItem("Compare contries", tabName = "compare", icon = icon("sliders"))
      # menuItem("Marketing analysis", tabName = "loyalty", icon = icon("th")),
      # menuItem("Timeline", tabName = "timeline", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(
                infoBox("Total Profit", textOutput("totalProfit"),color = "light-blue", icon =icon("money-bill-wave")),
                infoBox("Total Users", textOutput("totalUsers"), color = "light-blue", icon =icon("users")),
              infoBox("Average age", textOutput("avgAge"), color = "light-blue", icon =icon("calendar"))),
                
               h3("Filters"),
            fluidRow(
                  
                  box(pickerInput('countries', 'Select countries', choices = countrychoice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = countrychoice),
                      pickerInput('age_group', 'Select Age range', choices = group_age_choice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = group_age_choice)
                      ),
                  box(pickerInput('applicationIDs', 'ApplicationID', choices = applicachoice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = applicachoice))
                ),
              h3("User demographics"),
              fluidRow(
              box(plotOutput("plot1")),
              box(plotOutput("plot2"))
              ),
            h3("General facts"),
                fluidRow(
                  
                  box(plotOutput("plot4")),
                  box(plotOutput("plot5"))
                ),
              fluidRow(
                box(plotOutput("plot8")),
                box(plotOutput("plot3"))
              ),
            h3("Profit"),
              fluidRow(
                box(plotOutput("plot9"))
              )
              
              
              
  ),
  tabItem(tabName="mymap",
          h3("Filters"),
          fluidRow(
            
            box(selectInput(inputId = "measures",
                            label = "Select the measure",
                            list("Age_Group","Gender")),
                selectInput(inputId = "types",
                            label = "Select the category",
                            list("bets","win","stakes")),
                selectInput(inputId = "categories",
                            label = "Select the category",
                            list("GA","CA","PO", "SP"))),
            box(pickerInput('applicationIDs2', 'ApplicationID', choices = applicachoice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = applicachoice),
                pickerInput('countries2', 'Select countries', choices = countrychoice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = countrychoice),
                pickerInput('age_group2', 'Select Age range', choices = group_age_choice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = group_age_choice)
                )
          ),
          h3("Map of countries that participate in gambling"),
          fluidRow(
            box(width = 6, leafletOutput("plotMap")),
            box(plotOutput("plot6"))
            
          )
  ),
  tabItem(tabName="compare",
          
          fluidRow(
            
            box(pickerInput('countries3', 'Select countries', choices = countrychoice, multiple = TRUE, options = list(`actions-box` = TRUE)),
                selectInput(inputId = "types",
                            label = "Select the category",
                            list("bets","win","stakes")),
                selectInput(inputId = "categories",
                            label = "Select the category",
                            list("GA","CA","PO", "SP")),
                pickerInput('age_group3', 'Select Age range', choices = group_age_choice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = group_age_choice),
                pickerInput('applicationIDs3', 'ApplicationID', choices = applicachoice, multiple = TRUE, options = list(`actions-box` = TRUE), selected = applicachoice)
            ),
            box(plotOutput("datacompare"))
          )
  )
  )
  ))


shinyApp(ui = ui, server = server)
