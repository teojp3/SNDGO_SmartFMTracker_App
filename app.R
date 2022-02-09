# Import required libraries/packages #

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

# BOUNDARIES of Districts #

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

### Jurong Lake District Boundary

JLD <- st_read(dsn = "data/JLD Boundary", 
               layer = "JLD_Boundary")

JLD <- st_zm(JLD, drop = TRUE, what = "ZM")

Jurong_Lake_District_Boundary <- as_Spatial(JLD)

### Jurong Innovation District Boundary

JID <- st_read(dsn = "data/JID Boundary", 
               layer = "JID_Boundary")

JID <- st_zm(JID, drop = TRUE, what = "ZM")

Jurong_Innovation_District_Boundary <- as_Spatial(JID)

# Buildings located in Districts #

### PDD Buildings

Buildings <- read_csv("data/Aspatial/Buildings.csv")

Buildings <- st_as_sf(Buildings, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 3414)



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
                                                               with relevant Smart FM systems and information that we can take reference from."),
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
                                                   fluidRow(
                                                       column(4, align="center",
                                                              img(src = 'JLD.png', height = "60%", width = "60%", 
                                                              style="display: block; margin-left: auto; margin-right: auto;"), 
                                                              tags$a(href = "https://www.jld.gov.sg/", 
                                                                     "Jurong Lake District")),
                                                       column(4, align="center",
                                                              img(src = 'PDD.png', height = "60%", width = "60%", 
                                                              style="display: block; margin-left: auto; margin-right: auto;"),
                                                              tags$a(href = "https://estates.jtc.gov.sg/pdd", 
                                                                     "Punggol Digital District")),
                                                       column(4, align="center",
                                                              img(src = 'JID.png', height = "60%", width = "60%", 
                                                              style="display: block; margin-left: auto; margin-right: auto;"),
                                                              tags$a(href = "https://estates.jtc.gov.sg/jid", 
                                                                     "Jurong Innovation District"))),
                                                   tags$br(),
                                                   h4("SmartFMTracker allows for quick locating of Smart Buildings in the three areas of study."),
                                                   h4("Users will be able to identify those buildings
                                                      easily through a map visualisation, with the area of study demarcated in red on the map and the buildings as individual points on the map."),
                                                   h4("Two kinds of tracking services are available for use: 1) By Facility Management Vendor, and 2) By Number of Facility Management Systems."),
                                                   tags$br(),
                                                   width = 9)
                           )),
                  
                  # Tracker Panel
                  tabPanel("Tracker", fluid = TRUE, icon=icon("map-marked-alt"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                                   
                                                         # If By FM Vendor tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.SmartFM_Var === "By FM Vendor"',
                                                        tags$strong("FM Vendor Tracker Variable Inputs"),
                                                        selectInput(inputId = "Vendor_Area_Of_Study",
                                                                    label = "Select Area of Study",
                                                                    choices = c("Jurong Lake District" = "JLD",
                                                                                "Punggol Digital District" = "PDD",
                                                                                "Jurong Innovation District" = "JID"),
                                                                    selected = "JLD"),
                                                        selectInput(inputId = "FM_Vendor",
                                                                    label = "Select Facility Management Vendor",
                                                                    choices = c("CBRE" = "CBRE",
                                                                                "JLL" = "JLL",
                                                                                "ISS A/S" = "ISS A/S",
                                                                                "SODEXO" = "SODEXO"),
                                                                    selected = c("CBRE", "JLL", "ISS A/S", "SODEXO"), multiple = T),
                                                        img(src = 'Vendors_Legend.png', height = "100%", width = "100%", 
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
                                                      ),
                                                      
                                                      conditionalPanel(
                                                        'input.SmartFM_Var === "By Number of FM Systems"',
                                                        
                                                        tags$strong("FM Systems Tracker Variable Inputs"),
                                                        #helpText("Click the By Number of FM Systems Tab to see the changes)
                                                        selectInput(inputId = "Systems_Area_Of_Study",
                                                                    label = "Select Area of Study",
                                                                    choices = c("Jurong Lake District" = "JLD",
                                                                                "Punggol Digital District" = "PDD",
                                                                                "Jurong Innovation District" = "JID"),
                                                                    selected = "JLD"),
                                                        sliderInput(inputId = "FM_Systems_Count", 
                                                                    label = "Select the Range for Number of Smart FM Systems", 
                                                                    value = c(0, 7), min = 0, max = 7),
                                                        img(src = 'Systems_Legend.png', height = "100%", width = "100%", 
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
                                                      )),
                                                      
                                         mainPanel(width = 9,
                                                     tabsetPanel(
                                                     id = "SmartFM_Var",
                                                     tabPanel("By FM Vendor",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Area of Study: Jurong Lake District and Facility Management Vendor: All is used to plot the default map,
                                                                        select alternative choices to update the map.")),
                                                                     tmapOutput("Vendor_Map"),
                                                                     tags$br(),
                                                             tabsetPanel(
                                                               id = "SmartFM_Vendor_Info",
                                                               tabPanel("About the Vendor Map Visualisation",
                                                                        column(12,
                                                                               h2("What can you infer from the Vendor Map Visualisation?"),
                                                                               tags$br(),
                                                                         fluidRow(
                                                                                column(3,
                                                                                   img(src = 'Red_Boundary.png', height = "60%", width = "60%", 
                                                                                   style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000")),
                                                                                column(9,
                                                                                   tags$br(),
                                                                                   h4("The demarcation in red sets the boundaries of the chosen Area of Study (e.g. Jurong Lake District), while the numerous points on the map 
                                                                                   illustrates the georeferenced locations of the smart buildings in the area of study, 
                                                                                      with relevant information (shown upon clicking) that we can take reference from."))),
                                                                         tags$br(),
                                                                         tags$br(),
                                                                         fluidRow(
                                                                           column(3, 
                                                                               img(src = 'Bubbles.png', height = "60%", width = "60%", 
                                                                                   style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000")),
                                                                           column(9,
                                                                               h4("The primary insight one could get from the map is identifying the main FM Vendor the various buildings has contracted with,
                                                                               whereby the various FM Vendors are color coded for easy identification (refer to Legend on the right)."),
                                                                               h4("A secondary insight that can be derived from the map would be the relative sizing of the buildings in the chosen Area of Study.
                                                                                  The bigger the GFA of a building, the larger their bubble would be on the map."), 
                                                                               h4("This helps us identify which buildings are larger or smaller, as well as infer whether there's a correlation between the size 
                                                                               of a building and its preferred choice of FM Vendor (e.g. Smaller buildings tend to contract CBRE as their vendor of choice)."))),
                                                                         tags$br(),
                                                                         tags$br()
                                                                        )))
                                                              )),
                                                     
                                                     tabPanel("By Number of FM Systems",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Area of Study: Jurong Lake District and Range: 0 to 7 is used to plot the default map,
                                                                        select alternative choices to update the map.")),
                                                                     tmapOutput("Systems_Map"),
                                                                     tags$br(),
                                                                     tabsetPanel(
                                                                       id = "SmartFM_Systems_Info",
                                                                       tabPanel("About the Systems Map Visualisation",
                                                                                column(12,
                                                                                       h2("What can you infer from the Systems Map Visualisation?"),
                                                                                       tags$br(),
                                                                                       fluidRow(
                                                                                         column(3,
                                                                                                img(src = 'Red_Boundary.png', height = "60%", width = "60%", 
                                                                                                    style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000")),
                                                                                         column(9,
                                                                                                tags$br(),
                                                                                                h4("The demarcation in red sets the boundaries of the chosen Area of Study (e.g. Jurong Lake District), while the numerous points on the map 
                                                                                   illustrates the georeferenced locations of the smart buildings in the area of study, 
                                                                                      with relevant information (shown upon clicking) that we can take reference from."))),
                                                                                       tags$br(),
                                                                                       tags$br(),
                                                                                       fluidRow(
                                                                                         column(3, 
                                                                                                img(src = 'Bubbles_Systems.png', height = "60%", width = "60%", 
                                                                                                    style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000")),
                                                                                         column(9,
                                                                                                h4("The primary insight one could get from the map is identifying the Number of Smart FM Systems the various buildings have respectively,
                                                                               whereby the summed amount are color coded for easy identification (refer to Legend on the right)."),
                                                                                                h4("A secondary way of identifying the Number of FM Systems each building have is through the relative sizing of the
                                                                                       bubbles on the map."), 
                                                                                                h4("The higher the number of Smart FM Systems a building has, the larger their bubble would be on the map. This gives planners a more intuitive way
                                                                                  of identifying which buildings have weaker Smart FM capabilities, to allow them to better dedicate future resources for buildings in the Area of Study."))),
                                                                                       tags$br(),
                                                                                       tags$br()
                                                                        )))
                                                              )),
                                                     
                                                   )))
                           )
                  ))


server <- function(input, output, session){
  
  # FM Vendor (Reactive Variable)
  
  FM_Vendor_Var <- reactive({
    
        Buildings %>%
        filter(`DISTRICT` %in% input$Vendor_Area_Of_Study) %>%
          filter(`FACILITY MANAGEMENT VENDOR` %in% input$FM_Vendor)
    
  })
  
  # FM Vendor Map
  
  output$Vendor_Map <- renderTmap({
    
    tmap_mode('view')
    
    Vendor_District <- reactive({
      
                      if (input$Vendor_Area_Of_Study == "JLD"){
                        Vendor_District <- Jurong_Lake_District_Boundary
                      }
                      else if (input$Vendor_Area_Of_Study == "JID"){
                        Vendor_District <- Jurong_Innovation_District_Boundary
                      }
                      else if (input$Vendor_Area_Of_Study == "PDD"){
                        Vendor_District <- Punggol_Digital_District_Boundary
                      }
                      return(Vendor_District)
                    })
    
    Map_Tracker <- tm_shape(Vendor_District()) +
                   tm_polygons(alpha = 0.1, border.col = "red", lwd = 2, popup.vars = F, interactive = F) +
                   tm_shape(FM_Vendor_Var()) +
                   tm_symbols(col = "FACILITY MANAGEMENT VENDOR", size = "GROSS FLOOR AREA", alpha = 0.7, legend.col.show = F, scale = 1.5, id = "NAME OF BUILDING",
                              popup.vars = c("Name of Building" = "NAME OF BUILDING", "Address" = "ADDRESS", "Typology" = "TYPOLOGY",
                                            "Age of Building" = "AGE OF BUILDING", "Green Mark Award" = "GREEN MARK AWARD", "Year Of Certification" = "YEAR OF CERTIFICATION",
                                            "Gross Floor Area" = "GROSS FLOOR AREA", "% of AC Floor Area" = "% OF AC FLOOR AREA", "Avg. Monthly Occupancy Rate" = "AVG. MONTHLY BUILDING OCCUPANCY RATE",
                                            "Latest Energy Use (EUI)" = "LATEST ENERGY USE INTENSITY (EUI)", "Facility Management Vendor" = "FACILITY MANAGEMENT VENDOR", "Number of Smart FM Systems" = "NUMBER OF SMART FM SYSTEMS",
                                            "Building Management System" = "BUILDING MANAGEMENT SYSTEM", "Air Conditioning & Mechanical Ventilation System" = "AIR CONDITIONING & MECHANICAL VENTILATION SYSTEM",
                                            "Electrical & Energy Management System" = "ELECTRICAL & ENERGY MANAGEMENT SYSTEM", "Security & Occupancy System" = "SECURITY & OCCUPANCY SYSTEM",
                                            "Fire Alarm System" = "FIRE ALARM SYSTEM", "Lift Management System" = "LIFT MANAGEMENT SYSTEM", 
                                            "District Centralised Cooling System" = "DISTRICT CENTRALISED COOLING SYSTEM"
                                            , "Recency of Data" = "RECENCY OF DATA")) +    
                   tmap_options(basemaps = c("Esri.WorldGrayCanvas", "OneMapSG.Grey", "OpenStreetMap"),
                                basemaps.alpha = c(0.8, 0.8, 0.8)) +
                   tm_view(set.zoom.limits = c(14,16), symbol.size.fixed = T) +
                   tm_scale_bar(position=c("left", "bottom"), lwd = 50, size = 2)
                  
  })
  
  # Selection of Number of FM Systems (Reactive Variable)

  FM_Systems_Var <- reactive({
    Buildings %>%
      filter(`DISTRICT` %in% input$Systems_Area_Of_Study) %>%  
      filter(`NUMBER OF SMART FM SYSTEMS` %in% (input$FM_Systems_Count[1] : input$FM_Systems_Count[2]))
  })
  
  # FM Systems Map  
    
  output$Systems_Map <- renderTmap({
    
    tmap_mode('view')
    
    Systems_District <- reactive({
      
                        if (input$Systems_Area_Of_Study == "JLD"){
                          Systems_District <- Jurong_Lake_District_Boundary
                        }
                        else if (input$Systems_Area_Of_Study == "JID"){
                          Systems_District <- Jurong_Innovation_District_Boundary
                        }
                        else if (input$Systems_Area_Of_Study == "PDD"){
                          Systems_District <- Punggol_Digital_District_Boundary
                        }
                        return(Systems_District)
                      })
    
    # popup.vars() is to hide away unneeded columns    
    
    Systems_Tracker <- tm_shape(Systems_District()) +
                       tm_polygons(alpha = 0.1, border.col = "red", lwd = 2, popup.vars = F, interactive = F) +
                       tm_shape(FM_Systems_Var()) +
                       tm_symbols(col = "NUMBER OF SMART FM SYSTEMS", size = "BUBBLE SCALING" , as.count = TRUE, palette = "Greens", alpha = 0.7, size.lim = 0, scale = 1, id = "NAME OF BUILDING",
                                  popup.vars = c("Name of Building" = "NAME OF BUILDING", "Address" = "ADDRESS", "Typology" = "TYPOLOGY",
                                                "Age of Building" = "AGE OF BUILDING", "Green Mark Award" = "GREEN MARK AWARD", "Year Of Certification" = "YEAR OF CERTIFICATION",
                                                "Gross Floor Area" = "GROSS FLOOR AREA", "% of AC Floor Area" = "% OF AC FLOOR AREA", "Avg. Monthly Occupancy Rate" = "AVG. MONTHLY BUILDING OCCUPANCY RATE",
                                                "Latest Energy Use (EUI)" = "LATEST ENERGY USE INTENSITY (EUI)", "Facility Management Vendor" = "FACILITY MANAGEMENT VENDOR", "Number of Smart FM Systems" = "NUMBER OF SMART FM SYSTEMS",
                                                "Building Management System" = "BUILDING MANAGEMENT SYSTEM", "Air Conditioning & Mechanical Ventilation System" = "AIR CONDITIONING & MECHANICAL VENTILATION SYSTEM",
                                                "Electrical & Energy Management System" = "ELECTRICAL & ENERGY MANAGEMENT SYSTEM", "Security & Occupancy System" = "SECURITY & OCCUPANCY SYSTEM",
                                                "Fire Alarm System" = "FIRE ALARM SYSTEM", "Lift Management System" = "LIFT MANAGEMENT SYSTEM", "District Centralised Cooling System" = "DISTRICT CENTRALISED COOLING SYSTEM",
                                                "Recency of Data" = "RECENCY OF DATA")) +
                       tmap_options(basemaps = c("Esri.WorldGrayCanvas", "OneMapSG.Grey", "OpenStreetMap"),
                                   basemaps.alpha = c(0.8, 0.8, 0.8)) +
                       tm_view(set.zoom.limits = c(14,16), symbol.size.fixed = T)+
                       tm_scale_bar(position=c("left", "bottom"), lwd = 50, size = 2)
    
  })
  
}

shinyApp(ui=ui, server=server)


























