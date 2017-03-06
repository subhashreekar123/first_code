header<- dashboardHeader(title = img(src="logo.jpg", height = 50,width = 210))


body <- dashboardBody(
  useShinyjs(),
  tabsetPanel(
    id = "navbar",
    tabPanel("tab1 title",value=1,
             DT::dataTableOutput("table1")),
    tabPanel("tab2 title",value=2,
             plotOutput("plot3")),
    tabPanel("tab3 title",value=3,
             plotOutput("plot4")),
    tabPanel("tab4 title",value=4,
             plotOutput("plot5")),
    tabPanel("tab5 title",value=5,
             plotOutput("plot6"))
    
  )
)

sidebar <- dashboardSidebar(
  conditionalPanel(
    condition = "input.navbar == 1", 
        selectInput('dataset', 'Choose data set', c('Training_data','Test_data'))
  ), 
  conditionalPanel(
  condition = "input.navbar == 2",
          selectInput('Emp_Status1', 'Emp_Status', c("ALL","Active","Resigned")),
          selectInput('sex1', 'Sex', c("ALL","Female","Male")),
          selectInput('PSA_Category1','PSA', c("ALL","Asia Pacific","Bangalore","Chennai","Europe","Middle East and Africa","Mumbai","North America","Pune"))
    ),
  conditionalPanel(
condition = "input.navbar == 3",
          selectInput('Emp_Status2', 'Emp_Status', c("ALL","Active","Resigned")),
          selectInput('sex2', 'Sex', c("ALL","Female","Male")),
          selectInput('PSA_Category2','PSA', c("ALL","Asia Pacific","Bangalore","Chennai","Europe","Middle East and Africa","Mumbai","North America","Pune"))
    ),
conditionalPanel(
  condition = "input.navbar == 4",
      div(id='tab4_sidebar',
          selectInput('Emp_Status3', 'Emp_Status', c("ALL","Active","Resigned")),
          selectInput('sex3', 'Sex', c("ALL","Female","Male")),
          selectInput('PSA_Category3','PSA', c("ALL","Asia Pacific","Bangalore","Chennai","Europe","Middle East and Africa","Mumbai","North America","Pune"))
      ),
  conditionalPanel(    
  condition = "input.navbar == 5",
            selectInput('Emp_Status4', 'Emp_Status', c("ALL","Active","Resigned")),
            selectInput('sex4', 'Sex', c("ALL","Female","Male")),
            selectInput('PSA_Category4','PSA', c("ALL","Asia Pacific","Bangalore","Chennai","Europe","Middle East and Africa","Mumbai","North America","Pune"))
        )
))

    

dashboardPage (skin="yellow",header,sidebar,body)
