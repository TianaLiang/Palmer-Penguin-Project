library(shiny)
library(dplyr)
library(ggplot2)

origindata <- readRDS("origindata.RDS")
enrollmentdata <- readRDS("enrollmentdata.RDS")


ui <- fluidPage(
    tags$head(tags$style(
        HTML('
          form.well{ 
              font-family: "Arial";
              background-color: rgb(105, 232, 251);
          }
          
          div {
            background-color: rgb(125, 185, 222); #dark blue
          }
          
          body {
            font-family: "Arial";
            color : black;
          }
          input {
            font-family: "Optima";
            font-weight: bold;
          }
          h1{
            font-family: "Arial";
            color : white;
            text-align: center;
            font-weight: bold;
          } 
          h5{
            text-align: center;
            font-size: 15px;
            font-weight: bold;
          }
          p {
            font-size: 17px;
            
          }
          
           ')
    )),
    
    #Page title
    h1("Dream School Wonderland"),
    h5(em("By Team Random")),
    h5(em("Team Members: Ziyue Jin, Tiana Liang, Stella Wang, Pei-Hsun Hsu, Miaosi Tao, Hongkai Lou")),
    
    
    br(),
    br(),
    p("If you are currently looking for your dream school, you should be able to find useful statistics of geography, enrollment, and research development of those universities in the United States.
      Please first enter a benchmark of a university's 25th perventile SAT score to help narrow down the range of school. Then, you can click on the tabs below to check out different categories of information."),
    
    hr(),
    
    
    # Application title
    titlePanel("US University Statistics"),
    
    
    numericInput("scores", "25th Percentile SAT Score has to be above of: ", value = 1000), #above this score
    numericInput("scores2", "25th Percentile SAT Score has to be below of: ", value = 1300), #below this score
    hr(),
    br(),
    
    tabsetPanel(
        tabPanel("University List",tableOutput("list")),
        tabPanel("Region Distribution", plotOutput("schoolregion")),
        tabPanel("Urbanization", plotOutput("urbanization")),
        tabPanel("Profile classification", plotOutput("profile"), textOutput("one"), textOutput("two"),
                 textOutput("three"),textOutput("four"),textOutput("five"),textOutput("six"),
                 textOutput("seven"),textOutput("eight"),textOutput("nine"),textOutput("ten"),
                 textOutput("eleven"),textOutput("twelve"),textOutput("thirteen"),textOutput("fourteen"),
                 textOutput("fifteen"),textOutput("zero")),
        tabPanel("Enrollment Data", plotOutput("Full_time"),textOutput("Full_title"),tableOutput("Full_low"),plotOutput("transfer"),textOutput("tran_title"),tableOutput("tran_top") ),
        tabPanel("Research Expenditure", plotOutput("research"), tableOutput("top"), textOutput("note"))
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    #Uni List
    output$list <- renderTable({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- origindata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2, ]
        print(selectdata[order(selectdata$SAT, decreasing = TRUE),c(1,4)])
    })
    
    #Profile
    output$profile <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 ||input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- origindata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2, ]
        selectdata$profilef <- factor(selectdata$UGprofile2018, levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
        selectprofile <- selectdata %>%
            group_by(profilef) %>%
            count(profilef)
        x<- barplot(selectprofile$n,names.arg = selectprofile$profilef ,main= "Distribution of Schools based on their profile",xlab = "Profile", ylab = "Number of Schools",  ylim = c(0,max(selectprofile$n)+30),col=2,family="Arial")
        text(x,1.05*selectprofile$n, labels = selectprofile$n)
    })
    output$one <-renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "0: Not classified"})
    output$two <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "1: Two-year, higher part-time"})
    output$three <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "2: Two-year, mixed part/full-time"})
    output$four <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "3: Two-year, medium full-time"})
    output$five <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "4: Two-year, higher full-time"})
    output$six <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "5: Four-year, higher part-time"})
    output$seven <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "6: Four-year, medium full-time, inclusive, higher transfer-in"})
    output$eight <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "7: Four-year, medium full-time, inclusive, lower transfer-in"})
    output$nine <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "8: Four-year, medium full-time, selective, higher transfer-in"})
    output$ten <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "9: Four-year, medium full-time , selective, lower transfer-in"})
    output$eleven <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "10: Four-year, full-time, inclusive, higher transfer-in"})
    output$twelve <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "11: Four-year, full-time, inclusive, lower transfer-in"})
    output$thirteen <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "12: Four-year, full-time, selective, higher transfer-in"})
    output$fourteen <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "13: Four-year, full-time, selective, lower transfer-in"})
    output$fifteen <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "14: Four-year, full-time, more selective, higher transfer-in"})
    output$zero <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "15: Four-year, full-time, more selective, lower transfer-in"})
    
    #SchoolRegion
    output$schoolregion <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- origindata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2, ]
        selectdata$regionf <- factor(selectdata$Region, levels = c("US Service schools", "New England", "Mid East", "Great Lakes", "Plains", "Southeast", "Southwest", "Rocky Mountains", "Far West", "Others"))
        selectregion <- selectdata %>%
            group_by(regionf) %>%
            count(regionf)
        x<- barplot(selectregion$n,names.arg = selectregion$regionf ,main= "Distribution of Schools based on their region codes",xlab = "Region", ylab = "Number of Schools", ylim = c(0,max(selectregion$n)+30), col=5,family="Arial")
        
        text(x, 1.05*selectregion$n, labels = selectregion$n)
    })
    
    
    #Urbanization
    output$urbanization <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- origindata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2, ]
        selectdata$urbanType <- factor(selectdata$Urban, levels = c("Rural Remote","Rural Distant","Rural Fringe","Town Distant","Town Remote","Town Fringe","Suburb Small", "Suburb Midsize" ,"Suburb Large","City Small" ,"City Midsize","City Large", "{Not available}"))
        selectdata <- selectdata[selectdata$urbanType != "{Not available}",]
        selecturban <- selectdata %>%
            group_by(urbanType) %>%
            count(urbanType)
        x<- barplot(selecturban$n,names.arg = selecturban$urbanType ,main= "Distribution of Schools based on their urbanization",xlab = "Urbanization", ylab = "Number of Schools", ylim = c(0,max(selecturban$n)+30), col = 3,family="Arial")
        text(x, 1.05*selecturban$n, labels = selecturban$n)
        
        
    })
    
    #Enrollment
    output$Full_time <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- enrollmentdata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2,]
        selectdata$FTP <- selectdata$FT/selectdata$Total_Enroll
        selectdata <- selectdata[selectdata$FTP!="NaN",]
        plot(selectdata$SAT, selectdata$FTP, main = "Proportion of Full-Time Enrollment", col = "blue", xlab = "SAT score", ylab = "Full-Time Enrollment %", pch = 20, family="Arial") 
        if(nrow(selectdata) != 1) {abline(lm(selectdata$FTP~selectdata$SAT), col = "red", lwd=2)}
    })
    
    output$Full_title <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "Bottom 5 :"})
    output$Full_low <- renderTable({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        selectdata <- enrollmentdata[origindata$SAT >= input$scores & origindata$SAT <= input$scores2,]
        selectdata$FTP <- selectdata$FT/selectdata$Total_Enroll
        selectdata <- selectdata[selectdata$FTP!="NaN",]
        names(selectdata)[8] <- "Full Time Enrollment %"
        print(tail(selectdata[order(selectdata$'Full Time Enrollment %', decreasing = TRUE),c(1,8)],5))
    })
    
    output$transfer <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        selectdata <- enrollmentdata[enrollmentdata$SAT >= input$scores & enrollmentdata$SAT <= input$scores2,]
        selectdata$TRP <- selectdata$Transfer/selectdata$Total_Enroll
        selectdata <- selectdata[selectdata$TRP!="NaN",]
        plot(selectdata$SAT, selectdata$TRP, main = "Proportion of Transfer Student Enrollment", col = "blue", xlab = "SAT score", ylab = "Transfer Enrollment %", pch = 20,family="Arial")
        if(nrow(selectdata) != 1) {abline(lm(selectdata$TRP~selectdata$SAT), col = "red", lwd=2)}
        
    })
    
    output$tran_title <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "Top 5 : "})
    output$tran_top <- renderTable({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        selectdata <- enrollmentdata[enrollmentdata$SAT >= input$scores & enrollmentdata$SAT <= input$scores2,]
        selectdata$TRP <- selectdata$Transfer/selectdata$Total_Enroll
        selectdata <- selectdata[selectdata$TRP!="NaN",]
        names(selectdata)[8] <- "Transfer Enrollment %"
        print(head(selectdata[order(selectdata$'Transfer Enrollment %', decreasing = TRUE),c(1,8)],5))
    })
    
    #Research
    output$research <- renderPlot({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop("You should enter a numeric input!")
        if(input$scores2 < input$scores) stop("The lower bound should be less than the upper bound!")
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop("There is no school satisfying this condition!")
        selectdata <- origindata[(origindata$SAT >= input$scores)&(origindata$SAT <= input$scores2)&(origindata$Research != 0), ]
        selectdata$researchf <- selectdata$Research
        index <- order(selectdata$researchf, decreasing = TRUE)
        r <- head(selectdata[index,], n = 10)
        
        x<- barplot(r$researchf, xaxt = "n",
                    main= "Top 10 Schools Based on Research Expenditure", xlab = "Rank", ylab = "Expenditure in dollars", 
                    col = "salmon", space = 0.08,family="Arial")
        text(x, 1, cex.lab = 0.5)
        
    })
    
    output$top <- renderTable({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        selectdata <- origindata[(origindata$SAT >= input$scores)&(origindata$SAT <= input$scores2)&(origindata$Research != 0), ]
        selectdata$researchf <- selectdata$Research
        indext <- order(selectdata$researchf, decreasing = TRUE)
        t <- head(selectdata[indext,], n = 10)
        print(t[, c(1,10)])
    })
    
    output$note <- renderText({
        if(is.numeric(input$scores) == FALSE |is.numeric(input$scores2) == FALSE) stop()
        if(input$scores2 < input$scores) stop()
        if (input$scores <0 || input$scores > 1530 || input$scores2 < 0) stop()
        "*We would plot all of the schools if there are fewer than ten schools in the input SAT interval."
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

