library(shiny)
library(leaflet)
library(tidyverse)
library(tools)
library(shinythemes)
library(shinyjs)
library(tmap)
library(readr)
library(sp)
library(sf)
library(rgdal)
library(spatstat)
library(maptools)



# Import Data

### Childcare Centres as Example

childcare <- readOGR(dsn="data/Punggol",
                     layer="Punggol_CC",
                     verbose = FALSE)

PDD <- spTransform(childcare, CRS("+init=epsg:3414"))


### Pre Processing

mpsz_sf <- st_read(dsn = "data", 
                   layer = "MP14_SUBZONE_WEB_PL")

pg = mpsz[mpsz@data$PLN_AREA_N == "PUNGGOL",]

pg = st_union(st_as_sf(pg))

icon <- tmap_icons("www/smart-building.png")

# UI
ui <- fluidPage(theme = shinytheme("lumen"),
                useShinyjs(),
                # Navigation Bar
                navbarPage(
                  title = div(img(src = 'SNDGO App Logo.png', style = "margin-top: 0px; padding-right:6px;padding-bottom: 30px", height = 60)),
                  
                  # Homepage Panel
                  tabPanel("Home Page", fluid = TRUE, icon=icon("info-circle"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(img(src = 'SNDGO App Logo.png', height = "80%", width = "80%", 
                                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                                      # White Space below Logo
                                                      tags$br(),
                                                      h4(tags$strong("An initiative by SNDGO Smart City Projects Office, spearheaded by:"), align = "center"),
                                                      tags$ul(
                                                        tags$li(tags$a(href = "https://www.linkedin.com/in/jun-peng-teo/", "Teo Jun Peng"), style = "font-size: 18px;"),
                                                      ),
                                                      tags$br(),
                                                      h4("This project is done as a Proof of Concept 
                                             under the guidance of Smart City Projects Office Senior Manager Anderson Ang.", 
                                                         align = "center"),
                                                      tags$br(),
                                                      img(src = 'SNDGO_Logo.png', height = "80%", width = "80%", 
                                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                                      width = 3),
                                         
                                         mainPanel(h3(tags$strong("Project Motivation")),
                                                   tags$hr(),
                                                   fluidRow(
                                                     column(5,
                                                            h4("Countless data sources exist in the form of spatial data, with geographic elements such as the shape, size or location of the features. 
                                       Such spatial data could be analysed to generate useful insights or drive insightful decisions such as planning locations of facilities and 
                                       understanding more about Ecology."),
                                                            h4("However, not many people are technically trained to do such spatial analysis. Additionally, the only way for them to improve their breadth 
                                       and depth of knowledge pertaining to this area is limited to online resources. 
                                       Without proper foundation, any analysis done could be highly inaccurate as well."),
                                                            h4("Therefore, our main focus is to develop a web-based geospatial analytical tool dedicated to Point Pattern Analysis, with two methods available for use."),
                                                            h4("Through this geospatial application, we hope to give pointers to and allow users to conduct Point Pattern Analysis for their selected data with ease, regardless of their technical background. 
                                       Hence, the name Spatial Pointers is given for our application.")),
                                                     column(7,align = 'center',
                                                            img(src = 'SmartFM.png', height = "60%", width = "60%", 
                                                                style="display: block; margin-left: auto; margin-right: auto;"),
                                                            tags$a(href = "https://www1.bca.gov.sg/buildsg/facilities-management-fm/smart-facilities-management-fm", 
                                                                   "Read more about Smart Facilities Management (FM) from the website of Building and Construction Authority (Singapore)."
                                                            ))),
                                                   tags$br(),
                                                   
                                                   
                                                   h3(tags$strong("What does Point Pattern Analysis do?")),
                                                   tags$hr(),
                                                   h4("Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of
                                          data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations."),
                                                   tags$br(),
                                                   
                                                   h3(tags$strong("About our application: Spatial Pointers")),
                                                   tags$hr(),
                                                   h4("Our application will assist users with two methods of Point Pattern Analysis:"), 
                                                   tags$ul(
                                                     tags$li("Spatial Point Patterns Analysis (SPPA)", style = "font-size: 18px; font-weight: 500;"),
                                                     tags$li("Network-Constrained Point Patterns Analysis (NetSPPA)", style = "font-size: 18px; font-weight: 500;")),
                                                   h4("For each analysis, our application is able to provide users with kernel density maps
                                          of the input spatial point datasets and conduct various hypothesis tests to derive statistical conclusions 
                                          on the distributions of datasets."),
                                                   width = 9)
                           )),
                  
                  # Tracker Panel
                  tabPanel("Tracker", fluid = TRUE, icon=icon("map-marked-alt"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                        tags$strong("Smart FM Tracker Variable Inputs"),
                                                        #helpText("Click the Kernel Density Estimation Tab to see the changes)
                                                        selectInput(inputId = "NetSPPA_main_var",
                                                                    label = "Select Area of Study",
                                                                    choices = c("Punggol Digital District" = "PDD",
                                                                                "Jurong Lake District" = "JLD",
                                                                                "Jurong Innovation District" = "JID"),
                                                                    selected = "PDD"),
                                                        selectInput(inputId = "BuildingSystems",
                                                                    label = "Select Building Systems (Multi-Select)",
                                                                    choices = c("Smart Facility Management" = "FM",
                                                                                "Smart Air-Conditioning" = "AC",
                                                                                "Smart Toilet" = "WC",
                                                                                "Smart Lighting" = "Lighting"),
                                                                    selected = "FM",
                                                                    multiple = TRUE),
                                                        actionButton("NetSPPA_Run_KDE", "Conduct Search")
                                                      ),
                                         mainPanel(width = 9,
                                                     tabsetPanel(id = "NetSPPA_var",
                                                     tabPanel("Smart FM Tracker",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Variable: Childcare Centres, Kernel: Quartic and Method: Simple is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     tmapOutput("NetSPPA_KDE_Map"),
                                                             tabsetPanel(
                                                               id = "NetSPPA_KDE_info",
                                                               tabPanel("About Network-Constrained Kernel Density Estimation",
                                                                        column(12,
                                                                               h2("What is Network-Constrained Kernel Density Estimation?"),
                                                                               h5("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a
                                                                                  two-dimensional space, which is not suitable for analysing density of events occuring on a network.
                                                                                  Therefore, the modified Network-Constrained Kernel Density Estimation is used to calculate density of events
                                                                                  occuring along the edges of a network."),
                                                                               h3("How to interpret the output?"),
                                                                               h5("Essentially, the darker the color of the road, the higher the relative density of the point features as compared 
                                                                                  to road segments with ligher color (meaning lower density)."),
                                                                        )))
                                                              )),
                                                     
                                                   )))
                           )))


server <- function(input, output, session){
  
  
  
  
  # NetSPPA KDE Map
  output$NetSPPA_KDE_Map <- renderTmap({
    
    tmap_mode('view')
    
    NetSPPA_KDE <- isolate(tm_shape(pg) +
      tm_polygons(alpha = 0.1, border.col = "red", lwd = 2) +
      tm_shape(Schools) +
      tm_symbols(shape = icon, size = 0.6) +
      tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                   basemaps.alpha = c(0.8, 0.8, 0.8)) +
      tm_view(set.zoom.limits = c(14,16)))
    
    
  })
  
  # NetSPPA K_Function
  output$NetSPPA_K_Function <- renderPlot({
    
    input$NetSPPA_Run_Kfunc
    
    k_main <- reactive({
      if (input$NetSPPA_K_Main == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_K_Main == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_K_Main == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_K_Main == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    k_func <- isolate(kfunctions(network, 
                                         k_main(),
                                         start = 0, 
                                         end = 1000, 
                                         step = 50, 
                                         width = 50, 
                                         nsim = input$NetSPPA_K_No_Simulations, 
                                         resolution = 50,
                                         verbose = FALSE, 
                                         conf_int = 0.05))
    
    k_func$plotk
    
  })
  
  # NetSPPA Cross_K_Function
  output$NetSPPA_Cross_K_Function <- renderPlot({
    
    input$NetSPPA_Run_Cross_Kfunc
    
    k_main <- reactive({
      if (input$NetSPPA_CrossK_Main == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_CrossK_Main == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_CrossK_Main == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_CrossK_Main == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    kcross_sec <- reactive({
      if (input$NetSPPA_CrossK_Secondary == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_CrossK_Secondary == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_CrossK_Secondary == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_CrossK_Secondary == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    crossk <- isolate(cross_kfunctions(network, 
                                       k_main(),
                                       kcross_sec(),
                                       start = 0, 
                                       end = 1000, 
                                       step = 50, 
                                       width = 50, 
                                       nsim = input$NetSPPA_CrossK_No_Simulations, 
                                       resolution = 50,
                                       verbose = FALSE, 
                                       conf_int = 0.05))
    
    crossk$plotk
    
  })
  
  
}

shinyApp(ui=ui, server=server)




























