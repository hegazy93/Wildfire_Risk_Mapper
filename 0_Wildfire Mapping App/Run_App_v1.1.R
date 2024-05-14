#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#Load packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(terra)
library(raster)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(DT)
library(reshape2)
library(tibble)
library(bslib)
library(gdistance)
library(data.table)
library(shinyjs)


#Load Data
ac_lut <- read.csv("ac_lut.csv")

lc <- terra::rast("landcover_ac.tif")
names(lc[[1]]) <- "code"

## Load slope and aspect rasters
slope <- terra::rast("mm_slope_25m_resampled.tif")
aspect <- terra::rast("mm_aspect_25m_resampled.tif")
## Load ari raster data 
ari <- terra::rast("2024_ari_runoff_10.tif")

# Function defintion for fire spread modelling

slope_component <- function(slope_raster, aspect_raster, direction){
  Ddiff <- aspect
  Ddiff <- abs(aspect_raster-direction)
  slope_direction <- slope
  
  slope_direction[Ddiff <= 90] <- slope_raster * cos(Ddiff * pi / 180)
  slope_direction[(Ddiff > 90) & (Ddiff <= 180)] <- -slope_raster * cos((180 - Ddiff) * pi / 180)
  slope_direction[(Ddiff > 180) & (Ddiff <= 270)] <- -slope_raster * cos((Ddiff - 180) * pi / 180)
  slope_direction[Ddiff > 270] <- slope_raster * cos((360 - Ddiff) * pi / 180)
  
  return(slope_direction)
}

slope_N <- slope_component(slope, aspect, 0)
slope_NE <- slope_component(slope, aspect, 45)
slope_E <- slope_component(slope, aspect, 90)
slope_SE <- slope_component(slope, aspect, 135)
slope_S <- slope_component(slope, aspect, 180)
slope_SW <- slope_component(slope, aspect, 225)
slope_W <- slope_component(slope, aspect, 270)
slope_NW <- slope_component(slope, aspect, 315)



# Define UI 

ui <- fixedPage(
  
  theme = bslib::bs_theme(
    bg = "#101010",
    fg = "#FFF",
    primary = "#E69F00",
    secondary = "#FFF",
    success = "#009E73",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")),
  
  titlePanel(title=div(img(src="Header.jpg", height = 66))),
  
  tabsetPanel(
    tabPanel("Propagation Risk Map",
             fixedRow(
               column(3,
           numericInput(
             "wind_speed",
             "Wind speed (MPH)",
             value = 5,
             min = 0,
             max = 100,
             step = 1),
           numericInput(
              "wind_dir",
              "Wind direction (º from North)",
              value = 22.5,
              min = 0,
              max = 360,
              step = 22.5),
           numericInput(
             "temp",
             "Temprature (º celsius)",
             value = 10,
             min = -30,
             max = 60,
             step = 0.1),
           numericInput(
             "rh",
              "Relative humidity (percent)",
              value = 80,
              min = 0,
              max = 100,
              step = 0.1),
           dateInput(
             "Date",
              "Date",
              value = as.Date("2024-03-15"),
              min = as.Date("2024-01-01","%Y-%m-%d"),
              max = as.Date("2024-03-15","%Y-%m-%d"),
              format = "yyyy.mm.dd",
              startview = "month"),
           value_box(
             title = "Wildfire Weather Level",
             value = textOutput("fws"),
             theme = value_box_theme(name = "text-white", bg = "#f5a122"))
           ),
    
    column(3,
           sliderInput(
             "lc_fuel",
              "Landcover fuel weight:",
              min = 0,
              max = 1,
              value=.5,
              step = 0.1),
           sliderInput(
             "lc_priority",
              "Landcover priority weight:",
              min = 0,
              max = 1,
              value=.5,
             step = 0.1),
           sliderInput(
             "w_ari",
              "Soil dryness weight",
              min = 0,
              max = 1,
              value=.5,
             step = 0.1),
           sliderInput(
             "w_slope",
              "Effective slope weight",
              min = 0,
              max = 1,
              value=.5,
              step = 0.1),
           sliderInput(
             "w_fmc",
              "Fuel dryness weight",
              min = 0,
              max = 1,
              value=.5,
              step = 0.1),
                ),
    column(6,
           actionButton("updatePlot", "Update Fire Propagation Risk Map"), #update the risk map based on user input
           leafletOutput("rasterPlot"),
           sliderInput(
                      "threshold",
                      "Risk Threshold:",
                      min = 0,
                      max = 1,
                      value=.5,
                      step = 0.1),
             )
    ),
    
    fixedRow(
      column(6,
           DTOutput("lctable"), #view the reactive lancvover table
           actionButton("Undo", 'Undo'), #undo button for edits in the reactive lancvover table
           actionButton("Confirm", 'Confirm')
           ),
      column(6,
             p(strong("Credits and References", style = "font-size: 14pt")),
             p(strong("Data sources:", style = "font-size: 11pt"),tags$li("Rainfall Data: Environment Agency Rainfall API (EA, 2024)", style = "font-size: 11pt"), 
               tags$li("Landcover: UK Land Cover Map (CEH, 2021)", style = "font-size: 11pt"), tags$li("Lidar Composite DTM: EDINA LIDAR (Digimap, 2023)", style = "font-size: 11pt"), 
               tags$li("Peaty soil: Peaty Soils Location (Natural England 2023)", style = "font-size: 11pt")),
             
             p(strong("Funding:"), "This project is part-funded by West Yorkshire Fire & Rescue Service, and part-funded by the Consumer Data Research Centre (CDRC), an ESRC Data Investment. Funding references ES/L011840/1; ES/L011891/1.", style = "font-size: 11pt"),
             p("For more Information please contact Abdelrahman Ibrahim", tags$a(href="mailto:a.m.a.ibrahim@leeds.ac.uk", "a.m.a.ibrahim@leeds.ac.uk"), style = "font-size: 11pt"),
             

             )
      )
  ),
  
  tabPanel("Spread Map",
           fixedRow(
             column(3,
                   numericInput(
                     "wind_speed_2",
                     "Wind speed (MPH)",
                     value = 20,
                     min = 0,
                     max = 100,
                     step = 1),
                   numericInput(
                     "wind_dir_2",
                     "Wind direction (º from North)",
                     value = 22.5,
                     min = 0,
                     max = 360,
                     step = 22.5),
                   numericInput(
                     "rh_2",
                     "Relative humidity",
                     value = 40,
                     min = 0,
                     max = 100,
                     step = .1),
                   numericInput(
                     "x",
                     "X",
                     value = 405560,
                     min = 399460,
                     max = 407915,
                     step = 25),
                   numericInput(
                     "y",
                     "Y",
                     value = 410010,
                     min = 406460,
                     max = 414165  ,
                     step = 25),
                   sliderInput(
                    "n_ros_bw",
                    "Broadleaf Woodland Nominal ROS (m/s)",
                    min = 0,
                    max = 0.1,
                    value=0.017,
                    step = 0.001),
                   sliderInput(
                     "n_ros_cw",
                     "Coniferous Woodland Nominal ROS (m/s)",
                     min = 0,
                     max = 0.1,
                     value=0.018,
                     step = 0.001)
             ),
             
             column(3,
                  sliderInput(
                    "n_ros_a",
                    "Arable Nominal ROS (m/s)",
                    min = 0,
                    max = 0.1,
                    value=0.006,
                    step = 0.001),
                  sliderInput(
                    "n_ros_ig",
                    "Improved Grassland Nominal ROS (m/s)",
                    min = 0,
                    max = 0.1,
                    value=0.006,
                    step = 0.001),
                  sliderInput(
                    "n_ros_sg",
                    "Semi-natural Grassland Nominal ROS (m/s)",
                    min = 0,
                    max = 0.1,
                    value=0.013,
                    step = 0.001),
                  sliderInput(
                    "n_ros_mhb",
                    "Mountain, Heath and Bog Nominal ROS (m/s)",
                    min = 0,
                    max = 0.1,
                    value=0.017,
                    step = 0.001),
                  ),
           
           column(6,
           actionButton("update_map", "Generate Fire Spread"),
           leafletOutput("spread_map"))
    )
    )
  )
  )


server <- function(input, output, session) {
  # Landcover interactive table ---------------------------------------------
  rv <- reactiveValues(data = ac_lut, orig=ac_lut)
  ## Render the interactive table
  output$lctable <- renderDT({
    datatable(rv$data, editable = TRUE)
  })
  ## Add cell edit functionality
  observeEvent(input$lctable_cell_edit, {
    row  <- input$lctable_cell_edit$row
    clmn <- input$lctable_cell_edit$col
    rv$data[row, clmn] <- input$lctable_cell_edit$value
  })
  ## Add undo edits functionality
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })
  
  #confirm and download the user modified lut table
  observeEvent(input$Confirm, {
    write.csv(rv$data, "ac_lut_user.csv", row.names = F)
  })
 


  # Handle updating the plot based on user input
  observeEvent(input$updatePlot, {
    ## Get the selected date from the slider
    selectedDate <- paste("X", as.character(input$Date), sep = "") 
    selectedDate <- gsub("-", ".", selectedDate)
    
    ### Extract the selected ARI date from the raster
    selectedBand <- ari[[selectedDate]]
    
    ### normalise and multiply by weight
    norm01 <- function(r){
      # get the min max values
      minmax_r = range(values(r), na.rm=TRUE) 
      # rescale 
      return( (r-minmax_r[1]) / (diff(minmax_r)))
    }
    selectedBand <- norm01(selectedBand)
    values(selectedBand) <- 1 - values(selectedBand)
    names(selectedBand) <- "date"
    selectedBand_w <- selectedBand * input$w_ari 
    
    ## Extract user defined landcover fuel and importance from the reactive table
    lc_user <- read.csv("ac_lut_user.csv")
    
    lc$fuel = lc_user$fuel[match(values(lc$code), lc_user$code)]
    lc$priority = lc_user$priority[match(values(lc$code), lc_user$code)]
    
    ### Normalise and multiply by weight
    lc$fuel <- norm01(lc$fuel) * input$lc_fuel
    lc$priority <- norm01(lc$priority) * input$lc_priority
    
    ## Produce a raster for estimated vegetation dryness
    v_dry <- slope
    terra::values(v_dry) <- input$rh / 100
    terra::values(v_dry) <- 1 - terra::values(v_dry)
    v_dry[lc$code %in% c(7,8,10)] <- 0
    
    ## Calculate slope in the direction of wind
    effective_slope <- slope
    names(effective_slope) <- "effective_slope"
    
    wind_direction <- input$wind_dir 
    
    if (wind_direction >= 180){
      wind_head <- wind_direction - 180
    } else{wind_head <- wind_direction + 180}
    
    dir_difference <- abs(aspect - wind_head)

    effective_slope[dir_difference <= 90] <- slope * cos(dir_difference * pi / 180)
    effective_slope[(dir_difference > 90) & (dir_difference <= 180)] <- -slope * cos((180 - dir_difference) * pi / 180)
    effective_slope[(dir_difference > 180) & (dir_difference <= 270)] <- -slope * cos((dir_difference - 180) * pi / 180)
    effective_slope[dir_difference > 270] <- slope * cos((360 - dir_difference) * pi / 180)
    
    ### Normalize between -1 and 1 and multiply by weight
    decimal_scale <- function(x) { 
      max_abs <- max(abs(x))
      power <- ceiling(log10(max_abs))
      x / (10^power)}
    
    effective_slope_w <- decimal_scale(effective_slope) * input$w_slope

    
    #Construct the overall normalised risk score
    risk <- norm01(lc$fuel+lc$priority+selectedBand_w$date+effective_slope_w+v_dry)
    risk[lc$code %in% c(7,8)] <- 0

    
    ##mask values below threshold
    msk <- ifel(risk < input$threshold, NA, 1)
    risk <- mask(risk, msk)
    
    ##Define color Palette
    pal = colorNumeric(palette = "viridis", values(selectedBand),
                       na.color = "transparent")
    
    # Plot the selected band
    output$rasterPlot <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Stadia.AlidadeSmooth) |>
        addRasterImage(x = risk, colors = pal, opacity = 0.7) |>
        addLegend(pal = pal, 
                  values = values(risk),
                  title = "Propagation Risk", position = "bottomleft")
    })
    risk_score <-  0.0272*((1.61*input$wind_speed) ^ 0.16) * (input$temp ^ 0.5) * (100 - input$rh)
    
    risk_level <- "NA"
    
    if(risk_score < 1.5){
      risk_level <- "Very Low"
      }else if(risk_score >= 1.5 & risk_score < 3.5){
        risk_level <- "Low"
      }else if(risk_score >= 3.5 & risk_score < 8.5){
        risk_level <- "Moderate"
      }else if (risk_score >= 8.5 & risk_score < 16){
        risk_level <- "High"
      }else{
        risk_level <- "Very High"
        }
    
    output$fws <- renderText({risk_level})
  })
    
    # Fire Spread
  
  # define map
  output$spread_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Stadia.AlidadeSmooth) |>
      setView(lng = -1.956, lat = 53.589, zoom = 12)})
    
    observeEvent(input$update_map, {
      
      wind_component <- function(wind_speed, wind_direction, direction, raster_temp){
        if (wind_direction >= 180){
          wind_head <- wind_direction - 180
        } else{wind_head <- wind_direction + 180}
        
        Ddiff <- abs(wind_head-direction)
        
        if(Ddiff <= 90){
          wind_component <- wind_speed * cos(Ddiff * (pi / 180))
        } else if ((Ddiff > 90) & (Ddiff <= 180)) {
          wind_component <- -wind_speed * cos((180 - Ddiff) * (pi / 180))
        } else if ((Ddiff > 180) & (Ddiff <= 270)) {
          wind_component <- -wind_speed * cos((Ddiff - 180) * (pi / 180))
        } else { 
          wind_component <- wind_speed * cos((360 - Ddiff) * (pi / 180))
        }
        
        wind_component_r <- raster_temp
        terra::values(wind_component_r) <- wind_component
        
        return(wind_component_r)
      }
      
      wind_speed <- (input$wind_speed_2*1.61*1000)/3600
      
      wind_N <- wind_component(wind_speed, input$wind_dir_2, 0, slope)
      wind_NE <- wind_component(wind_speed, input$wind_dir_2, 45, slope)
      wind_E <- wind_component(wind_speed, input$wind_dir_2, 90, slope)
      wind_SE <- wind_component(wind_speed, input$wind_dir_2, 135, slope)
      wind_S <- wind_component(wind_speed, input$wind_dir_2, 180, slope)
      wind_SW <- wind_component(wind_speed, input$wind_dir_2, 225, slope)
      wind_W <- wind_component(wind_speed, input$wind_dir_2, 270, slope)
      wind_NW <- wind_component(wind_speed, input$wind_dir_2, 315, slope)
      
      slope_factor <- function(slope_component_r) {
        slope_factor <- slope_component_r
        slope_factor <- exp(3.533 * (tan(slope_component_r * (pi / 180))))
        return(slope_factor)
      }
      
      wind_factor <- function(wind_component_r) {
        wind_factor <- wind_component_r
        wind_factor <- exp(0.1783 * wind_component_r)
        return(wind_factor)
      }
      
      ## Fuel Moisture Content Factor
      fmc_factor <- exp(-0.14 *((input$rh_2)/100))
      
      #Nominal rate of spread for landcover types
      n_ros <- lc
      n_ros[lc == 1] <- input$n_ros_bw
      n_ros[lc ==2] <- input$n_ros_cw
      n_ros[lc ==3] <- input$n_ros_a
      n_ros[lc == 4] <- input$n_ros_ig
      n_ros[lc == 5] <- input$n_ros_sg
      n_ros[lc == 6] <- input$n_ros_mhb
      n_ros[lc %in% c(7,8,9,10)] <- 0
      
      #calculate rate of spread (ROS)
      f_rate_N <- (n_ros * fmc_factor * slope_factor(slope_N) * wind_factor(wind_N))
      f_rate_NE <- (n_ros * fmc_factor * slope_factor(slope_NE) * wind_factor(wind_NE))
      f_rate_E <- (n_ros * fmc_factor * slope_factor(slope_E) * wind_factor(wind_E))
      f_rate_SE <- (n_ros * fmc_factor * slope_factor(slope_SE) * wind_factor(wind_SE))
      f_rate_S <- (n_ros * fmc_factor * slope_factor(slope_S) * wind_factor(wind_S))
      f_rate_SW <- (n_ros * fmc_factor * slope_factor(slope_SW) * wind_factor(wind_SW))
      f_rate_W <- (n_ros * fmc_factor * slope_factor(slope_W) * wind_factor(wind_W))
      f_rate_NW <- (n_ros * fmc_factor * slope_factor(slope_NW) * wind_factor(wind_NW))
      
      spread_time_quad <- slope
      
      # Define the threshold x and y coordinates
      threshold_x <- input$x 
      threshold_y <- input$y
      
      
      # Directional ROS averaging 
      ##Identify cells where x and y coordinates are greater than the thresholds
      q1 <- c(xmin = threshold_x, xmax = ext(spread_time_quad)[2], ymin = threshold_y , ymax = ext(spread_time_quad)[4])
      q2 <- c(xmin = threshold_x, xmax = ext(spread_time_quad)[2], ymin = ext(spread_time_quad)[3] , ymax = threshold_y)
      q3 <- c(xmin = ext(spread_time_quad)[1], xmax = threshold_x, ymin = ext(spread_time_quad)[3] , ymax = threshold_y)
      q4 <- c(xmin = ext(spread_time_quad)[1], xmax = threshold_x, ymin = threshold_y , ymax = ext(spread_time_quad)[4])
      
      spread_time_q1 <- crop(spread_time_quad, q1)
      spread_time_q2 <- crop(spread_time_quad, q2)
      spread_time_q3 <- crop(spread_time_quad, q3)
      spread_time_q4 <- crop(spread_time_quad, q4)
      
      ## Function to assign values to cells between N and NE directions
      N_NE <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (i + j <= nrow(raster) + 1) {
              mat[i, j] <- 1
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between NE and E directions
      NE_E <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (i + j >= nrow(raster) + 1) {
              mat[i, j] <- 2
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between E and SE directions
      E_SE <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (i <= j) {
              mat[i, j] <- 3
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between SE and S directions
      SE_S <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (i >= j) {
              mat[i, j] <- 4
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between S and SW directions
      S_SW <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (j >= ncol(raster) - i + 1) {
              mat[i, j] <- 5
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between SW and W directions
      SW_W <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if (j <= ncol(raster) - i + 1) {
              mat[i, j] <- 6
            }
          }
        }
        raster <- setValues(raster, mat)
        return(raster)
      }
      
      ## Function to assign values to cells between W and NW directions
      W_NW <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if ((i + j)<= ncol(raster) + 1) {
              mat[i, j] <- 7
            }
          }
        }
        raster <- setValues(raster, mat)
        raster <- flip(raster, direction = "vertical")
        return(raster)
      }
      
      ## Function to assign values to cells between NW and N directions
      NW_N <- function(raster) {
        mat <- matrix(NA, nrow = nrow(raster), ncol = ncol(raster))
        for (i in 1:nrow(raster)) {
          for (j in 1:ncol(raster)) {
            if ((i + j) >= ncol(raster) + 1) {
              mat[i, j] <- 8
            }
          }
        }
        raster <- setValues(raster, mat)
        raster <- flip(raster, direction = "vertical")
        return(raster)
      }
      
      ## Apply the functions to generate directional spread time raster
      N_NE <- N_NE(spread_time_q1)
      NE_E <- NE_E(spread_time_q1)
      E_SE <- E_SE(spread_time_q2)
      SE_S <- SE_S(spread_time_q2)
      S_SW <- S_SW(spread_time_q3)
      SW_W <- SW_W(spread_time_q3)
      W_NW <- W_NW(spread_time_q4)
      NW_N <- NW_N(spread_time_q4)
      
      directional_spread_time <- mosaic(N_NE, NE_E, E_SE, SE_S, S_SW, SW_W, W_NW, NW_N, fun ="max")
      
      directional_spread_time[directional_spread_time == 1] <- mean(f_rate_N, f_rate_NE)
      directional_spread_time[directional_spread_time == 2] <- mean(f_rate_NE, f_rate_E)
      directional_spread_time[directional_spread_time == 3] <- mean(f_rate_E, f_rate_SE)
      directional_spread_time[directional_spread_time == 4] <- mean(f_rate_SE, f_rate_S)
      directional_spread_time[directional_spread_time == 5] <- mean(f_rate_S, f_rate_SW)
      directional_spread_time[directional_spread_time == 6] <- mean(f_rate_SW, f_rate_W)
      directional_spread_time[directional_spread_time == 7] <- mean(f_rate_W, f_rate_NW)
      directional_spread_time[directional_spread_time == 8] <- mean(f_rate_NW, f_rate_N)
      
      ### Exclude water bodies and builtup areas from the spread map 
      directional_spread_time[lc %in% c(7,8,9,10)] <- NA
      
      
      r <- raster::raster(directional_spread_time)
      raster::crs(r) <- "epsg:27700"
      
      # Calculate the transition matrix
      conduct <- gdistance::transition(r, function(x)mean(x), 8, symm=FALSE)
      conduct <- gdistance::geoCorrection(conduct, scl=FALSE)
      
      A <- c(threshold_x,input$y)
      
      
      cs <- data.table(ID=c("A"),
                       x=c(A[[1]]),
                       y=c(A[[2]]))
      
      
      ps <- st_as_sf(cs, coords = c("x", "y"), crs = 27700, agr = "constant")
      ps <- ps |> mutate(x=st_coordinates(ps)[[1]], y = st_coordinates(ps)[[2]])
      
      csA <- gdistance::accCost(conduct, A)
      raster::crs(csA) <- "epsg:27700"
      csA <- csA/60
      csA_xhour <- csA
      csA_xhour[csA_xhour[] > 360] <- NA
      
      ignition <- st_transform(ps, crs = 4326)
      ignition <- ignition |> mutate(x=st_coordinates(ignition)[[1]], y = st_coordinates(ignition)[[2]])
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'white',
        library = 'ion',
        markerColor = 'orange'
      )
      
      pal = colorBin(palette = "viridis", values(csA_xhour), bins = c(0,60,120,180,240,300,360),
                     na.color = "transparent", pretty = TRUE)
      
      output$spread_map <- renderLeaflet({
        leaflet() |>
          addProviderTiles(providers$Stadia.AlidadeSmooth) |>
          setView(lng = -1.956, lat = 53.589, zoom = 12)})
      
      # change clicked polygone
      leafletProxy("spread_map") |> addRasterImage(x = csA_xhour, colors = pal, opacity = 0.7) |>
        addLegend(pal = pal, 
                  values = values(csA_xhour),
                  title = "Time from ignition (minutes)", position = "bottomleft") |>
        addAwesomeMarkers(ignition$x, ignition$y, icon = icons) |>
        addScaleBar(position = c("bottomright"))
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
