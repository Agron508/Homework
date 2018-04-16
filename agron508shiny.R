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
        tabName = "groupTK",
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("plot_eyes", height = "800px"))))
       
         ), # end tabItem 2
      
      
      # Third tab item: Lin Mitch & Anabelle
      tabItem(
        tabName = "Devicedata",
        h2("Display the data we collected in class"),
        fluidRow(
          column(width = 12,
                 box(width = 10, status = "success",
                     plotlyOutput("plot_devicedata", height = "600px"))))
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
                        box(title = "Information", status = "success",
                            width = 12, solidHeader = TRUE,
                            "write text here"
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
  
  output$plot_eyes <- renderPlotly ({
    #Intensity of radiation reaching the eye in order for the human eye to percieve light.
    I_eye = 4.0e-11 # Units - W/m^2
    
    #The following are called in the beginning of the assingment:
    #Plancks Constant: 
    h = 6.63e-34 # Units - J*s
    #Speed of light in a vacuum: 
    c = 3.0e8 # Units - m/s
    #Diameter of a human pupil
    d = seq(2,8,by = 1) # Units - mm
    
    #Convert the diameter to meters
    d = d*1e-3 # Units - m 
    
    #Calculate the pupil area
    pupil_area = pi*((d/2)^2) # Units - m^2
    #Power reaching the eye
    Power_reach_eye = I_eye*pupil_area # Units - W
    #Range of wavelength in the visible spectrum
    #lam_pupil = seq(380e-9,780e-9,by = 1e-9) #Units - m
    lam_pupil = array(c(455e-9,492e-9,577e-9,597e-9,622e-9,780e-9))
    
    #Energy of a photon
    e = (h*c)/lam_pupil #Units - J
    #Create Vector for Calculations
    Photons_reach_eye = matrix(0,6,7)
    
    #Number of photons reaching the eye
    for(i in 1:6){
      for(j in 1:7){
        Photons_reach_eye[i,j] = Power_reach_eye[j]/e[i] #Units - photons/s
      }
    }
    
    Photons_reach_eye = t(Photons_reach_eye)
    col_set = c("Violet","Blue","Green","Yellow","Orange","Red")
    palette(col_set)
    #matplot(pupil_area,Photons_reach_eye,type = "l",main = "Photons That Reach The Human Eye at Different Wavelengths with Different Pupil Sizes",cex.main = .8,xlab = "Pupil Area (m^2)",ylab = "Number of Photons",lty = "solid", lwd = "2",col = col_set)
    #leg.txt <- c("Wavelength = 455 nm","Wavelength = 492 nm","Wavelength = 577 nm","Wavelength = 597 nm","Wavelength = 622 nm","Wavelength = 780 nm")
    cex = .5
    legend(pupil_area[1],Photons_reach_eye[7,6],leg.txt,cex = .5,lwd = 2, col = col_set)
    PPF_eye = (Photons_reach_eye /(6.02e23) * 1e6)
    #Now let's plot the results of this, like we plotted the others
    #Plotting 
    col_set = c("Violet","Blue","Green","Yellow","Orange","Red")
    palette(col_set)
    a = c(1:10)
    b = c(1:10)
    p <- plot_ly(x = a, y = b, type = 'scatter', mode = 'lines') %>%
      layout(title = "",
             xaxis = list(title = "wavelength (nm)"),
             yaxis = list (title = "Spectral irradiance (W/m2/microm)"))
    #p <- matplot(pupil_area,PPF_eye,type = "l",main = "PPF That Reaches The Human Eye at Different Wavelengths with Different Pupil Sizes",cex.main = .8,xlab = "Pupil Area (m^2)",ylab = "PPF (micromoles/s)",lty = "solid", lwd = "2",col = col_set)
    #leg.txt <- c("Wavelength = 455 nm","Wavelength = 492 nm","Wavelength = 577 nm","Wavelength = 597 nm","Wavelength = 622 nm","Wavelength = 780 nm")
    
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
