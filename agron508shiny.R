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
               menuItem("Graph 1", tabName = "graph1_TK", icon = icon("angle-right"))),
       menuItem("Lin-Mitch-Anabelle",
                tabName = "groupLMA", icon = icon("th"),
                menuItem("Montieth plot", tabName = "Montieth_plot", icon = icon("angle-right")),
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
        tabName = "graph1_TK",
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("plot_eyes", height = "800px")))),
        br(),
        fluidRow(column(width=12,
                        box(title = "Information", status = "success",
                            width = 12, solidHeader = TRUE,
                            "Write text"
                        )))
       
         ), # end tabItem 2
      
      
      # Third tab item: Lin Mitch & Anabelle
      tabItem(
        tabName = "Devicedata",
        h2("Display the data we collected in class"),
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("plot_devicedata", height = "600px")))),
        br(),
        fluidRow(column(width=12,
                        box(title = "Information", status = "success",
                            width = 12, solidHeader = TRUE,
                            "Write text"
                        )))
      ), # end third tab Item
      tabItem( 
        tabName = "Montieth_plot",
        h2("Interactive 3D plot of biomass (MJ/m2)"),
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("Montieth", height = "600px")))),
        br(),
        fluidRow(column(width=12,
                        box(title = "Yield potential", status = "success",
                            width = 12, solidHeader = TRUE,
                            paste("Yield potential is defined as the yield of a cultivar when grown in environments to which it 
is adapted, with nutrients and water non-limiting and with pests, diseases, weeds, lodging, and other stresses effectively 
controlled.", "We calculated yield potential (MJ/m2)(Yp) by total incident solar radiation during growing season ((MJ/m2))(St), light interception 
efficiency(Ei), energy conversion efficiency(Ec)","Yp=St*Ei*Ec", sep="\n")
                        )))
      ) # end four tab Item
      
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
  
  
  
  
  output$Montieth <- renderPlotly ({
    x<-c(0.5,0.6,0.7,0.75) # x = Ei
    y<-c(0.05,0.1,0.2,0.3) # y = Ec
    St<-2000
    data<-expand.grid(x,y) # gives all the combinations of x and y
    colnames(data)<-c("x","y")
    data$z<-St*0.478*data$x*data$y  # biomass in MJ/m2
    
    p <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~z) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Ei'),
                          yaxis = list(title = 'Ec'),
                          zaxis = list(title = 'Yp')))
    p
  })
  } # end server 




# shinyApp links ui and server and allows you to run the app
shinyApp(ui, server)
