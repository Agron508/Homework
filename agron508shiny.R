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
      menuItem("Example 1",tabName = "Example1", icon = icon("th")),
      menuItem("Device data",tabName = "Devicedata", icon = icon("check-circle"))
  )),
  
  ### Body Content 
  dashboardBody(
    tabItems(
      # First tab item
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
      # Second tab item
      tabItem(
        tabName = "Example1",
        h2("Example with a R dataset"),
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     radioButtons("feed", # name of the  input = name of the data column
                                              "Product type", # label 
                                              choices=c("horsebean","linseed","soybean","sunflower","meatmeal","casein"), # choice given to the user
                                              selected="horsebean"), # default selected choice,
                     plotOutput("boxplot"), # display the plot defined in server 
                     tableOutput("results") # display the table defined in server 
        )))
       
         ), # end tabItem 2
      
      
      # Third tab item
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
  
  output$boxplot<-renderPlot({ # renderPlot = function to create a reactive plot
    sub <-subset(chickwts,chickwts$feed==input$feed) # sub is the dataset related to 
    ggplot(sub, aes(feed,weight)) +  # ggplot to display the boxplot
      geom_boxplot() + 
      geom_point()
  })
  
  output$results <- renderTable({  # create a reactive table 
    sub <-subset(chickwts,chickwts$feed==input$feed) 
  })
  
  

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