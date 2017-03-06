library(shiny)
library(shinydashboard)
library(ggplot2)
library(gtools)
library(DT)
library(memoise)
library(tm)
library(wordcloud)
library(stringr)
library(shinyjs)
library(plyr)



Training_data <- read.csv("train_jan_new.csv")
Training_data<-Training_data[c(-1)]
Test_data=read.csv("test_jan_new.csv")  
Test_data=Test_data[c(-1)]
  


shinyServer(function(input, output,session) {

  
  outVar = reactive({
    names(get(input$dataset))
  })
  
  observeEvent(input$dataset, {
    updateSelectInput(session,"", choices = outVar())
  })


  output$table1 <-  DT::renderDataTable(
    get(input$dataset),options = list(scrollX = TRUE)
    
  )  
  
  
  output$plot3 <- renderPlot({
    
    Training_data$X=NULL
    attach(Training_data)
    subset1 <- subset(Training_data, Sex==input$sex1 & PSA_Category==input$PSA_Category1) 
    count(Training_data$PSA_Category1)
    if( (input$sex1 == "ALL") & (input$PSA_Category1 == "ALL"))
    {
      subset1 <- Training_data 
    }
    
    if( (input$sex1 != "ALL") & (input$PSA_Category1 == "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex1)   
    }
    
    if( (input$sex1 == "ALL") & (input$PSA_Category1 != "ALL"))
    {
      subset1 <- subset(Training_data, PSA_Category==input$PSA_Category1) 
    }
    
 
    
    densityPlot(~ Training_data$Exp_In_Lnt, groups =Sex, data=subset1, plot.points=FALSE, auto.key=TRUE, bw=5)
  })
  
  
  
  output$plot4 <- renderPlot({
    subset1 <- subset(Training_data, Emp_Status==input$Emp_Status2 & Sex==input$sex & PSA_Category==input$PSA_Category_Category2) 
    
    if((input$Emp_Status2 == "ALL") & (input$sex2 == "ALL") & (input$PSA_Category2 == "ALL"))
    {
      subset1 <- Training_data 
    }
    
    if((input$Emp_Status2 == "ALL") & (input$sex2 != "ALL") & (input$PSA_Category2 == "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex)   
    }
    
    if((input$Emp_Status2 != "ALL") & (input$sex2 == "ALL") & (input$PSA_Category2 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status) 
    }
    
    if((input$Emp_Status2 == "ALL") & (input$sex2 == "ALL") & (input$PSA_Category2 != "ALL"))
    {
      subset1 <- subset(Training_data, PSA_Category==input$PSA_Category2) 
    }
    
    if((input$Emp_Status2 == "ALL") & (input$sex2 != "ALL") & (input$PSA_Category2 != "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex & PSA_Category==input$PSA_Category) 
    }
    
    if((input$Emp_Status2 != "ALL") & (input$sex2 == "ALL") & (input$PSA_Category2 != "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status & PSA_Category==input$PSA_Category) 
    }
    
    if((input$Emp_Status2 != "ALL") & (input$sex2 != "ALL") & (input$PSA_Category2 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status2 & Sex==input$sex2) 
    }
    
    
    
    
    densityPlot(~ Exp_In_Lnt, groups =Designation_category,data= subset1, plot.points=FALSE, auto.key=TRUE, bw=5)
    
    
  })
  
  output$plot5 <- renderPlot({
    
    subset1 <- subset(Training_data, Emp_Status==input$Emp_Status & Sex==input$sex & PSA_Category==input$PSA_Category) 
    
    if((input$Emp_Status3 == "ALL") & (input$sex3 == "ALL") & (input$PSA_Category3 == "ALL"))
    {
      subset1 <- Training_data 
    }
    
    if((input$Emp_Status3 == "ALL") & (input$sex3 != "ALL") & (input$PSA_Category3 == "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex3)   
    }
    
    if((input$Emp_Status3 != "ALL") & (input$sex3 == "ALL") & (input$PSA_Category3 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status3) 
    }
    
    if((input$Emp_Status3 == "ALL") & (input$sex3 == "ALL") & (input$PSA_Category3 != "ALL"))
    {
      subset1 <- subset(Training_data, PSA_Category==input$PSA_Category3) 
    }
    
    if((input$Emp_Status3 == "ALL") & (input$sex3 != "ALL") & (input$PSA_Category3 != "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex3 & PSA_Category==input$PSA_Category3) 
    }
    
    if((input$Emp_Status3 != "ALL") & (input$sex3 == "ALL") & (input$PSA_Category3 != "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status3 & PSA_Category==input$PSA_Category3) 
    }
    
    if((input$Emp_Status3 != "ALL") & (input$sex3 != "ALL") & (input$PSA_Category3 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status3 & Sex==input$sex3) 
    }
    
    
    
    densityPlot(~Exp_In_Lnt, groups =Training_data$LEO_Current,data=subset1, plot.points=FALSE, auto.key=TRUE, bw=5)
    
  })
  
  output$plot6 <- renderPlot({
    
    subset1 <- subset(Training_data, Emp_Status==input$Emp_Status4 & Sex==input$sex4 & PSA_Category==input$PSA_Category4) 
    
    if((input$Emp_Status4 == "ALL") & (input$sex4 == "ALL") & (input$PSA_Category4 == "ALL"))
    {
      subset1 <- Training_data 
    }
    
    if((input$Emp_Status4 == "ALL") & (input$sex4 != "ALL") & (input$PSA_Category4 == "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$sex4)   
    }
    
    if((input$Emp_Status4 != "ALL") & (input$sex4 == "ALL") & (input$PSA_Category4 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status4) 
    }
    
    if((input$Emp_Status4 == "ALL") & (input$sex4 == "ALL") & (input$PSA_Category4 != "ALL"))
    {
      subset1 <- subset(Training_data, PSA_Category==input$PSA_Category4) 
    }
    
    if((input$Emp_Status4 == "ALL") & (input$sex4 != "ALL") & (input$PSA_Category4 != "ALL"))
    {
      subset1 <- subset(Training_data, Sex==input$se4 & PSA_Category==input$PSA_Category4) 
    }
    
    if((input$Emp_Status4 != "ALL") & (input$sex4 == "ALL") & (input$PSA_Category4 != "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status4 & PSA_Category==input$PSA_Category4) 
    }
    
    if((input$Emp_Status4 != "ALL") & (input$sex4 != "ALL") & (input$PSA_Category4 == "ALL"))
    {
      subset1 <- subset(Training_data, Emp_Status==input$Emp_Status4 & Sex==input$sex4) 
    }
    
    
    
    
    densityPlot(~Exp_In_Lnt, groups =Training_data$Abroad_Status,data=subset1, plot.points=FALSE, auto.key=TRUE, bw=5)
    
  })
  

})
   



