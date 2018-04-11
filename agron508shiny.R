####### script AGRON 508 demo Shiny #######
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readr)
## ui defines the user interface 

ui <- dashboardPage(
  skin = "red", # or blue, green, yellow, ...
  dashboardHeader(title="AGRON 508 tool"), # define title of the app
  dashboardSidebar(
    br(),
    width = 360,
    ## Menu ####
    sidebarMenu(
      menuItem("Introduction",tabName = "Intro", icon = icon("cog", lib = "glyphicon")),
      menuItem("Theo-Kelsie",
               tabName = "groupTK", icon = icon("th"),
               menuItem("Graph 1", tabName = "graph1_TK", icon = icon("angle-right")),
               menuItem("Graph 2", tabName = "graph2_TK", icon = icon("angle-right"))),
       menuItem("Lin-Mitch-Anabelle",
                tabName = "groupLMA", icon = icon("th"),
                menuItem("Graph 1", tabName = "graph1_LMA", icon = icon("angle-right")),
                menuItem("Device data",tabName = "Devicedata", icon = icon("angle-right")))
  )),
  
  ### Body Content 
  dashboardBody(
    tabItems(
      # First tab item : Introduction
      tabItem(tabName = "Intro",
              fluidRow( 
                box(title = "General Information about the tool",width=12,
                           status = "info", solidHeader = TRUE, collapsible = TRUE,
                           "This on-line interactive tool is designed by the students of AGRON 508 spring 2018",
                           br(),
                           "We included interactive graphs based on our assignments"),
                
                box(title = "Other box",width=12,
                    status = "success", solidHeader = TRUE, collapsible = TRUE,
                    "write some extra info")
                )
              ), # end first tabItem
      # Second tab item : Theo & Kelsie 
      tabItem(
        tabName = "groupTK",
        fluidRow(
          column(width = 12))
       
         ), # end tabItem 2
      
      
      # Third tab item: Lin Mitch & Anabelle
      tabItem(
        tabName = "Devicedata",
        h2("Display the data we collected in class"),
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("plot_devicedata", height = "800px"))))
      ) # end third tab Item
      
    )# end tabItems
    
  ) # end dashboardBody
) # end dashboardPage






# server contains the instruction to run the analysis, plot graphs and so on 
server <- function(input, output) { 
  

  # renderPlotly is you want interactive graph, otherwise use renderPlot
  
  output$plot_devicedata <- renderPlotly ({
    tab<-read.csv("https://raw.githubusercontent.com/Agron508/Homework/master/SS-110_1010_12918noon.csv")
    data<-tab[2:482,] # select rows
    colnames(data)[1]<-"waveL"
    data$waveL<-as.numeric(as.vector(data$waveL)) # to get a x_continous scale, data need to be numeric not factor
    time<-data[,84]# here I just pick a column randomly
    as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # function to convert all the level of the factor into numeric without losing information
    time<-as.numeric.factor(time)
  
    p <- plot_ly(data, x = ~waveL, y = ~time, type = 'scatter', mode = 'lines') %>%
      layout(title = "",
             xaxis = list(title = "wavelength (nm)"),
             yaxis = list (title = "Spectral irradiance (W/m2/microm)"))
    p 
  })
  
  } # end server 




# shinyApp links ui and server and allows you to run the app
shinyApp(ui, server)