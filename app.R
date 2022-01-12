library(shiny)
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
library(leaflet)

# Import Data

### A few PDD Buildings as example

PDD_Buildings <- read_csv("data/Aspatial/PDD_Buildings.csv")

PDD_Buildings <- st_as_sf(PDD_Buildings, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 3414)


# Filter by individual systems (e.g. BMS (Y/N) etc)

# School_BMS <- Schools[Schools$`BUILDING MANAGEMENT SYSTEM` == 'Y', ]
# 
# School_BMS <- as(School_BMS, "Spatial")
# 
# School_ST <- Schools[Schools$`SMART TOILET` == 'Y', ]
# 
# School_ST <- as(School_ST, "Spatial")
# 
# School_SS <- Schools[Schools$`SMART SENSORING` == 'Y', ]
# 
# School_SS <- as(School_SS, "Spatial")

# Filter by SMART FM SYSTEMS

# School_3 <- filter(Schools, grepl('BMS', `SMART FM SYSTEMS`))
# 
# School_3 <- as(School_3, "Spatial")

### Punggol Boundary

MPSZ <- st_read(dsn = "data", 
              layer = "MP14_SUBZONE_WEB_PL")

PG <- MPSZ[MPSZ$`PLN_AREA_N` == 'PUNGGOL', ]

PG <- st_union(PG)

Punggol_Boundary <- as_Spatial(PG)

### Punggol Digital District Boundary

PDD <- st_read(dsn = "data", 
                layer = "PDD_Boundary")

PDD <- st_zm(PDD, drop = TRUE, what = "ZM")

Punggol_Digital_District_Boundary <- as_Spatial(PDD)

### Icons

# FMIcons <- iconList(CBRE = makeIcon("http://globetrotterlife.org/blog/wp-content/uploads/leaflet-maps-marker-icons/ferry-18.png", 18, 18),
#                     JLL = makeIcon("http://globetrotterlife.org/blog/wp-content/uploads/leaflet-maps-marker-icons/danger-24.png", 24, 24))

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
                                         
                                         mainPanel(h3(tags$strong("About Smart Facility Management")),
                                                   tags$hr(),
                                                   fluidRow(
                                                     column(5,
                                                            h4("Smart Facilities Management (FM) refers to the integration of systems, processes, technologies and 
                                                            personnel to enhance the management of a building's facilities. It is about doing better with technology, using it as a means to an end.
                                                            Essentially, technology enables the convergence of siloed systems and processes into an integrated workplace management and operations framework."),
                                                            h4("Smart FM in Singapore is employed to move away from a reactive industry and work towards digitisation of the Built Environment and FM Sectors
                                                               for better coordination and flow of information to improve the productivity, effectiveness and sustainability of the nation's
                                                               buildings to overcome challenges faced today."),
                                                            h4("Therefore, our main focus for this initiative is to develop a web-based geospatial analytical tool dedicated to tracking smart buildings in Singapore,
                                                               with relevant Smart FM systems that we can take reference from."),
                                                            h4("Through this geospatial application, we hope to enable users to quickly locate exemplar buildings in three chosen areas of study, which
                                                               specifically are the smart districts of Singapore; 1) Punggol Digital District, 2) Jurong Lake District, and 3) Jurong Innovation District.")),
                                                     column(7,align = 'center',
                                                            img(src = 'SmartFM.png', height = "60%", width = "60%", 
                                                                style="display: block; margin-left: auto; margin-right: auto;"),
                                                            tags$a(href = "https://www1.bca.gov.sg/buildsg/facilities-management-fm/smart-facilities-management-fm", 
                                                                   "Read more about Smart Facilities Management (FM) from the website of Building and Construction Authority (Singapore)."
                                                            ))),
                                                   tags$br(),
                                                   
                                                   
                                                   h3(tags$strong("What can our application SmartFMTracker be used for?")),
                                                   tags$hr(),
                                                   h4("SmartFMTracker allows for quick locating of Smart Buildings in the three areas of study."),
                                                   h4("Users will be able to identify those buildings
                                                      easily through a map visualisation, with the area of study demarcated in red on the map and the buildings as individual points on the map."),
                                                   width = 9)
                           )),
                  
                  # Tracker Panel
                  tabPanel("Tracker", fluid = TRUE, icon=icon("map-marked-alt"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                        tags$strong("Smart FM Tracker Variable Inputs"),
                                                        #helpText("Click the Kernel Density Estimation Tab to see the changes)
                                                        selectInput(inputId = "Area_Of_Study",
                                                                    label = "Select Area of Study",
                                                                    choices = c("Punggol Digital District" = "PDD",
                                                                                "Jurong Lake District" = "JLD",
                                                                                "Jurong Innovation District" = "JID"),
                                                                    selected = "PDD"),
                                                        selectInput(inputId = "FM_Vendor",
                                                                    label = "Select Facility Management Vendor",
                                                                    choices = c("CBRE" = "CBRE",
                                                                                "JLL" = "JLL",
                                                                                "ISS A/S" = "ISS A/S",
                                                                                "SODEXO" = "SODEXO"),
                                                                    selected = c("CBRE", "JLL", "ISS A/S", "SODEXO"), multiple = T),
                                                        selectInput(inputId = "Building_Systems",
                                                                    label = "Select Building Systems (Multi-Select)",
                                                                    choices = c("Building Management System" = "BMS",
                                                                                "Smart Sensoring" = "SS",
                                                                                "Smart Toilet" = "ST"),
                                                                    selected = "BMS",
                                                                    multiple = TRUE),
                                                      ),
                                         mainPanel(width = 9,
                                                     tabsetPanel(id = "SmartFM_Var",
                                                     tabPanel("Smart FM Tracker",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Area of Study: Punggol Digital District and Building System(s): Building Management System is used to plot the default map,
                                                                        select alternative choices and click on 'Conduct Search' to update the map.")),
                                                                     tmapOutput("SmartFM_Map"),
                                                             tabsetPanel(
                                                               id = "SmartFM_Info",
                                                               tabPanel("About the Map Visualisation",
                                                                        column(12,
                                                                               h2("What can you infer from the Map Visualisation?"),
                                                                               tags$br(),
                                                                               h4("The demarcation in red sets the boundaries of the chosen Area of Study (e.g. Punggol Digital District)."),
                                                                               h4("The numerous points on the map illustrates the georeferenced locations of the smart buildings in the area of study, 
                                                                                  with relevant Smart FM systems that we can take reference from."),
                                                                        )))
                                                              )),
                                                     
                                                   )))
                           )))


server <- function(input, output, session){
  
   FM_Vendor_Var <- reactive({
        PDD_Buildings %>%
          filter(`FACILITY MANAGEMENT VENDOR` %in% input$FM_Vendor)
  })
  
  # Smart FM Map
  output$SmartFM_Map <- renderTmap({
    
    icon <- reactive({
            if (input$Building_Systems == "BMS"){
              icon <- tmap_icons("www/black.png")
            }
            else if (input$Building_Systems == "SS"){
              icon <- tmap_icons("www/blue.png")
            }
            else if (input$Building_Systems == "ST"){
              icon <- tmap_icons("www/green.png")
            }
            return(icon)
          })
          
    tmap_mode('view')

# popup.vars() is to hide away unneeded columns    
        
    Map_Tracker <- tm_shape(Punggol_Boundary) +
      tm_polygons(alpha = 0.1, border.col = "red", lwd = 1.5, lty = "dotted") +
      tm_shape(Punggol_Digital_District_Boundary) +
      tm_polygons(alpha = 0.1, border.col = "red", lwd = 2) +
      tm_shape(FM_Vendor_Var()) +
      tm_symbols(col = "FACILITY MANAGEMENT VENDOR", size = 0.4, popup.vars = c("Name of Building" = "NAME OF BUILDING", "Address" = "ADDRESS", "Typology" = "TYPOLOGY",
                                                            "Age of Building" = "AGE OF BUILDING", "Green Mark Award" = "GREEN MARK AWARD", "Year Of Certification" = "YEAR OF CERTIFICATION",
                                                            "Gross Floor Area" = "GROSS FLOOR AREA", "% of AC Floor Area" = "% OF AC FLOOR AREA", "Avg. Monthly Occupancy Rate" = "AVG. MONTHLY BUILDING OCCUPANCY RATE",
                                                            "Latest Energy Use (EUI)" = "LATEST ENERGY USE INTENSITY (EUI)", "Facility Management Vendor" = "FACILITY MANAGEMENT VENDOR", "Number of Smart FM Systems" = "NUMBER OF SMART FM SYSTEMS",
                                                            "Building Management System" = "BUILDING MANAGEMENT SYSTEM", "Air Conditioning & Mechanical Ventilation System" = "AIR CONDITIONING & MECHANICAL VENTILATION SYSTEM",
                                                            "Electrical & Energy Management System" = "ELECTRICAL & ENERGY MANAGEMENT SYSTEM", "Security & Occupancy System" = "SECURITY & OCCUPANCY SYSTEM",
                                                            "Plumbing Management System" = "PLUMBING MANAGEMENT SYSTEM", "Recency of Data" = "RECENCY OF DATA")) +
      tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                   basemaps.alpha = c(0.8, 0.8, 0.8)) +
      tm_view(set.zoom.limits = c(14,16))
    
  })
  
  # output$testMap <- renderLeaflet({
  #   leaflet(filteredData()) %>%
  #     addTiles(group = 'OSM') %>%
  #     addMarkers(
  #       icon = ~FMIcons[input$FM_Vendor]
  #     )
  # })
  
}

shinyApp(ui=ui, server=server)




























