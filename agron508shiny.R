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
                           "We included interactive graphs based on our assignment goals (1) to measure the wavelength of 
                    light that can be observed by a human eye. (2) Calculate potential yield using irradiance
                    measurements derived from a spectroradiometer."),
                
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
  
 
   # coppied from Monteith to make sure a plot shows!!   
  output$plot_eyes <- renderPlotly ({
    h=6.62606896e-34
    c=2.99792458e8 
    I_eye = 4.0e-11
    d = seq(2,8,by = 1)
    d = d*1e-3
    pupil_area = pi*((d/2)^2)
    Power_reach_eye = I_eye*pupil_area
    lam_pupil = array(c(455e-9,492e-9,577e-9,597e-9,622e-9,780e-9))
    e = (h*c)/lam_pupil
    Photons_reach_eye = matrix(0,6,7)
    for(i in 1:6){
      for(j in 1:7){
        Photons_reach_eye[i,j] = Power_reach_eye[j]/e[i] #Units - photons/s
      }
    }
    
    Photons_reach_eye = t(Photons_reach_eye)
    PPF_eye = (Photons_reach_eye /(6.02e23) * 1e6)
    PPF_EYE_1 = PPF_eye[,1]
    PPF_EYE_2 = PPF_eye[,2]
    PPF_EYE_3 = PPF_eye[,3]
    PPF_EYE_4 = PPF_eye[,4]
    PPF_EYE_5 = PPF_eye[,5]
    PPF_EYE_6 = PPF_eye[,6]
    
    data <- data.frame(pupil_area, PPF_EYE_1, PPF_EYE_2, PPF_EYE_3, PPF_EYE_4,PPF_EYE_5,PPF_EYE_6)
    
    p = plot_ly(data, x = ~pupil_area, y = ~PPF_EYE_1, name = '455 nm',type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~PPF_EYE_2, name = '492 nm', mode = 'lines+markers') %>%
    add_trace(y = ~PPF_EYE_3, name = '577 nm', mode = 'lines+markers') %>%
    add_trace(y = ~PPF_EYE_4, name = '597 nm', mode = 'lines+markers') %>%
    add_trace(y = ~PPF_EYE_5, name = '622 nm', mode = 'lines+markers') %>%
    add_trace(y = ~PPF_EYE_6, name = '780 nm', mode = 'lines+markers') %>%
      
      layout(title = "PPF That Reaches The Human Eye at Different Wavelengths with Different Pupil Sizes",
             xaxis = list(title = "Pupil Area (m^2)"),
             yaxis = list (title = "PPF (micromoles/s)"))
    
    p
  })
  } # end server 




# shinyApp links ui and server and allows you to run the app
shinyApp(ui, server)
