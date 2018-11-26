# ================================================================================
# Build Your Own: Vertical Lift
# Designed and built by Gabriel Coll and Yuanjing Han
# --------------------------------------------------------------------------------
# server
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shiny)
library(tidyverse)
library(extrafont)
library(extrafontdb)
library(magrittr)
library(scales)
source("app_functions.R")
source("module_functions.R")

# --------------------------------------------------------------------------------
# begin server section 

shinyServer(function(input, output, session) {
  
  # --------------------------------------------------------------------------------
  # read data and create reactive current$vl frame
  # note: keep(change) or cancel current$vl based on used input

  load("data/FVL_data_gs.Rda")
  load("data/FVL_stats_gs.Rda")
  
  current <- reactiveValues(vl = FVL_data_gs[["Fleet Plan"]],
                            FVL_data_gs = FVL_data_gs,
                            FVL_stats_gs = FVL_stats_gs)
  
  # --------------------------------------------------------------------------------
  # ultra-lightweight
  
  callModule(
    outofproductionvl,
    "LittleBird",
    current,
    vlname = "LittleBird",
    FullName = "Little Bird",
    NickName = "MH-6",
    Purpose = "A light attack helicopter used for special operations by the Army",
    imagename = '1.LittleBirdMH-6.png'
  )
  
  FS_slider_updated <-
    callModule(
      inproductionvl,
      "FireScout",
      current,
      vlname = "FireScout",
      FullName = "Fire Scout",
      NickName = "MQ-8",
      Purpose = "An unmanned helicopter used for ISR and fire support by the Navy",
      imagename = "space.png"
    )
  
  callModule(update_vl_module,
             "FireScout",
             current,
             vlname = "FireScout",
             FS_slider_updated)
  
  
  # --------------------------------------------------------------------------------
  # lightweight
  
  callModule(
    outofproductionvl,
    "CobraViper",
    current,
    vlname = "CobraViper",
    FullName = "Cobra / Viper",
    NickName = "AH-1",
    Purpose = "An attack helicopter used for air support by the Marine Corps",
    imagename = '3.CobraViperAH-1W.png'
  )
  
  Apache_slider_updated <-
    callModule(
      inproductionvl,
      "Apache",
      current,
      vlname = "Apache",
      FullName = "Apache",
      NickName = "AH-64",
      Purpose = "An attack helicopter used for air support by the Army",
      imagename = "space.png"
    )
  callModule(update_vl_module,
             "Apache",
             current,
             vlname = "Apache",
             Apache_slider_updated)
  
  callModule(
    outofproductionvl,
    "Creek",
    current,
    vlname = "Creek",
    FullName = "Creek",
    NickName = "TH-67",
    Purpose = "A light helicopter used for training by the Army",
    imagename = '5.CreekBell_206A.png'
  )
  
  callModule(
    outofproductionvl,
    "Lakota",
    current,
    vlname = "Lakota",
    FullName = "Lakota",
    NickName = "UH-72",
    Purpose = "A light utility helicopter used for logistics and support by the Army",
    imagename = "space.png"
  )
  
  callModule(
    outofproductionvl,
    "KiowaWarrior",
    current,
    vlname = "KiowaWarrior",
    FullName = "Kiowa (Warrior)",
    NickName = "OH-58",
    Purpose = "A light helicopter used for armed reconnassance by the Army",
    imagename = '7.Kiowa(Warrior)OH-58A.png'
  )
  
  callModule(
    outofproductionvl,
    "IroquoisHueyVenom",
    current,
    vlname = "IroquoisHueyVenom",
    FullName = "Iroquois / Huey / Venom",
    NickName = "UH-1",
    Purpose = "A light, multi-mission helicopter now used primarily by the Marines",
    imagename = '08.IroquoisHueyVenomUH-1N.png'
  )
  
  # --------------------------------------------------------------------------------
  # middleweight
  
  BH_slider_updated <-
    callModule(
      inproductionvl,
      "BlackHawk",
      current,
      vlname = "BlackHawk",
      FullName = "Black Hawk",
      NickName = "UH-60",
      Purpose = "A medium-lift utility helicopter used for a wide range of missions by the Army",
      imagename = '09.BlackHawkUH-60.png'
    )
  callModule(update_vl_module,
             "BlackHawk",
             current,
             vlname = "BlackHawk",
             BH_slider_updated)
  
  callModule(
    outofproductionvl,
    "SeaHawk",
    current,
    vlname = "SeaHawk",
    FullName = "Sea Hawk",
    NickName = "SH-60",
    Purpose = "A medium-lift utility helicopter used for a wide range of missions by the Navy",
    imagename = '10.SeaHawkSH-60.png'
  )
  
  callModule(
    outofproductionvl,
    "PaveHawk",
    current,
    vlname = "PaveHawk",
    FullName = "Pave Hawk",
    NickName = "HH-60",
    Purpose = "A medium-lift utility helicopter used primarily for combat rescue operations by the Air Force",
    imagename = '11.PaveHawkHH-60.png'
  )
  
  CRH_slider_updated <-
    callModule(
      inproductionvl,
      "CombatRescueHelicopter",
      current,
      vlname = "CombatRescueHelicopter",
      FullName = "Combat Rescue Helicopter",
      NickName = "HH-60W",
      Purpose = "A medium-lift utility helicopter to be used by the Air Force to replace the Pave Hawk",
      imagename = "space.png"
    )
  callModule(update_vl_module,
             "CombatRescueHelicopter",
             current,
             vlname = "CombatRescueHelicopter",
             BH_slider_updated)
  
  callModule(
    outofproductionvl,
    "SeaKing",
    current,
    vlname = "SeaKing",
    FullName = "Sea King",
    NickName = "H-3",
    Purpose = "A medium-lift helicopter used primarily for official transport by the Marines",
    imagename = '13.SeaKingSH-3.png'
  )
  
  SH_slider_updated <-
    callModule(
      inproductionvl,
      "Superhawk",
      current,
      vlname = "Superhawk",
      FullName = "Superhawk",
      NickName = "H-92",
      Purpose = "A medium-lift helicopter to by used by the Marines to replace the Sea King",
      imagename = "space.png"
    )
  callModule(update_vl_module,
             "Superhawk",
             current,
             vlname = "Superhawk",
             SH_slider_updated)
  
  # --------------------------------------------------------------------------------
  # heavyweight
  
  Chinook_slider_updated <-
    callModule(
      inproductionvl,
      "Chinook",
      current,
      vlname = "Chinook",
      FullName = "Chinook",
      NickName = "CH-47",
      Purpose = "A heavy-lift helicopter used for cargo and personnel transport by the Army",
      imagename = '15.ChinookCH-4.png'
    )
  callModule(update_vl_module,
             "Chinook",
             current,
             vlname = "Chinook",
             Chinook_slider_updated)
  
  
  SDS_slider_updated <-
    callModule(
      inproductionvl,
      "SeaDragonStallion",
      current,
      vlname = "SeaDragonStallion",
      FullName = "Sea Dragon / Stallion",
      NickName = "CH-53",
      Purpose = "A heavy-lift helicopter used for cargo and personnel transport by the Navy",
      imagename = '16.SeaDragonSeastallionCH-53D.png'
    )
  callModule(update_vl_module,
             "SeaDragonStallion",
             current,
             vlname = "SeaDragonStallion",
             SDS_slider_updated)
  
  Osprey_slider_updated <-
    callModule(
      inproductionvl,
      "Osprey",
      current,
      vlname = "Osprey",
      FullName = "Osprey",
      NickName = "V-22/CV-22",
      Purpose = "A vertical takeoff and landing tiltrotor, multi-mission aircraft used by the Air Force and Marines",
      imagename = '17.OspreyMV-22a.png'
    )
  callModule(update_vl_module,
             "Osprey",
             current,
             vlname = "Osprey",
             SDS_slider_updated)
  
  # --------------------------------------------------------------------------------
  # build your own 

  callModule(
    futurevl,
    "BYOUltraLightweight",
    current,
    vlname = "BYOUltraLightweight",
    FullName = "Build Your Own UltraLightweight",
    NickName = " ",
    Purpose = " ",
    imagename = "space.png"
  )
  
  callModule(
    futurevl,
    "BYOLightweight",
    current,
    vlname = "BYOLightweight",
    FullName = "Build Your Own Lightweight",
    NickName = " ",
    Purpose = " ",
    imagename = "space.png"
  )
  
  callModule(
    futurevl,
    "BYOMiddleweight",
    current,
    vlname = "BYOMiddleweight",
    FullName = "Build Your Own Middleweight",
    NickName = " ",
    Purpose = " ",
    imagename = "space.png"
  )
  
  callModule(
    futurevl,
    "BYOHeavyweight",
    current,
    vlname = "BYOHeavyweight",
    FullName = "Build Your Own Heavyweight",
    NickName = " ",
    Purpose = " ",
    imagename = "space.png"
  )
  
  callModule(
    futurevl,
    "BYOUltraHeavyweight",
    current,
    vlname = "BYOUltraHeavyweight",
    FullName = "Build Your Own UltraHeavyweight",
    NickName = " ",
    Purpose = " ",
    imagename = "space.png"
  )
  
  # --------------------------------------------------------------------------------
  # modal section (cont.)
  # note: update current$FVL_stats_gs using numbers in bsModal window
  # ...having bsModal out of mainPanel is also Ok -- another advantage
  # ...is that we don't need to reset main panel and we'll not reset the charts

  # current vertical lift platforms 

  callModule(OldbsModal, "LittleBird", current, vlname = "LittleBird")
  callModule(OldbsModal, "FireScout", current, vlname = "FireScout")
  
  callModule(OldbsModal, "CobraViper", current, vlname = "CobraViper")
  callModule(OldbsModal, "Apache", current, vlname = "Apache")
  callModule(OldbsModal, "Creek", current, vlname = "Creek")
  callModule(OldbsModal, "Lakota", current, vlname = "Lakota")
  callModule(OldbsModal, "Kiowa", current, vlname = "KiowaWarrior")
  callModule(OldbsModal, "IroquoisHueyVenom", current, vlname = "IroquoisHueyVenom")
  
  callModule(OldbsModal, "BlackHawk", current, vlname = "BlackHawk")
  callModule(OldbsModal, "SeaHawk", current, vlname = "SeaHawk")
  callModule(OldbsModal, "PaveHawk", current, vlname = "PaveHawk")
  callModule(OldbsModal, "CombatRescueHelicopter", current, vlname = "CombatRescueHelicopter")
  callModule(OldbsModal, "SeaKing", current, vlname = "SeaKing")
  callModule(OldbsModal, "Superhawk", current, vlname = "Superhawk")
  
  callModule(OldbsModal, "Chinook", current, vlname = "Chinook")
  callModule(OldbsModal, "SeaDragonStallion", current, vlname = "SeaDragonStallion")
  callModule(OldbsModal, "Osprey", current, vlname = "Osprey")
  
  # future vertical lift platforms 
  
  callModule(newbsModal, "BYOUltraLightweight", current, vlname = "BYOUltraLightweight")
  callModule(newbsModal, "BYOLightweight", current, vlname = "BYOLightweight")
  callModule(newbsModal, "BYOMiddleweight", current, vlname = "BYOMiddleweight")
  callModule(newbsModal, "BYOHeavyweight", current, vlname = "BYOHeavyweight")
  callModule(newbsModal, "BYOUltraHeavyweight", current, vlname = "BYOUltraHeavyweight")
  
  # --------------------------------------------------------------------------------
  # format data 

  vl_dataset <- reactive({
    return(
      get_vl_data(
        current$vl,
        current$FVL_data_gs,
        stats_df = current$FVL_stats_gs,
        input
      )
    )
  })
  
  # --------------------------------------------------------------------------------
  # preset values

  preset_value1235(input, "preset_value1", "preset_phase1", session)
  preset_value1235(input, "preset_value2", "preset_phase2", session)
  preset_value1235(input, "preset_value3", "preset_phase3", session)
  preset_value1235(input, "preset_value5", "preset_phase5", session)
  preset_value4(input, "preset_value4", "preset_phase4", session)
  
  # --------------------------------------------------------------------------------  
  # create format for y-axis ($_B)

  formaty1 <- function(x) {
    x <- gsub("000", "", x)
    x <- gsub("500", ".5", x)
    x <- gsub("250", ".25", x)
    x <- gsub("750", ".75", x)
    paste("$", x, "B", sep = "")
  }
  
  output$vl <- renderPlot({
    vl_plot()
  })
 
  vl_plot <- reactive({
    shown <- vl_dataset()
    
# --------------------------------------------------------------------------------
# find plot limits
    
    ymax <- max(c(max(shown$Total), signif(shown$Total, 2)))
    if (input$top_y == "# of Vertical Lift")
      ymax <- max(ymax, 375)
    ymin <- 0
    
# --------------------------------------------------------------------------------
# create options unique to line chart
    
    if (input$top_chart == "Line") {
      p <- ggplot() +
        
        geom_line(
          data = shown,
          aes(x = FY, y = Total, color = Category),
          position = "identity",
          size = 1
        ) +
        
        scale_color_manual(
          values = c("#C76363", "#788ca8"),
          labels = c("Vertical lift Plan ", "Baseline ")
        ) +
        
        coord_cartesian(ylim = c(min(shown$Total), max(shown$Total)))
      
    } else if ((input$top_chart == "Aircraft Type" |
                input$top_chart == "Production Stage") & input$top_y %in% c(
                  "Speed (mph)",
                  "Range (mi)",
                  "Service ceiling (ft)",
                  "Rate of climb (ft/min)",
                  "Empty weight (lbs)",
                  "Max TOW (lbs)"
                )) {
      shown <- filter(shown, Category == "new plan vertical lifts")
      
      p <-
        ggplot() + geom_area(
          data = shown,
          aes(x = FY, y = Total, color = Category),
          alpha = .80,
          color = "#4D7FA3",
          fill = "#4D7FA3"
        )
      
    } else if (input$top_chart == "Aircraft Type" &
               (
                 !input$top_y %in% c(
                   "Speed (mph)",
                   "Range (mi)",
                   "Service ceiling (ft)",
                   "Rate of climb (ft/min)",
                   "Empty weight (lbs)",
                   "Max TOW (lbs)"
                 )
               )) {
      shown <- shown %>% filter(Category == "new plan vertical lifts") %>%
        select(-Category)
      
      shown <- gather(data = shown,
                      key = "vl_type",
                      value = "Quantity",-FY,
                      -Total)
      
      # --------------------------------------------------------------------------------
      # relevel factors       
      # note: the purpose of this is to place future vertical lifts 
      # ...at the bottom of each chart
      
      shown$vl_type <- factor(
        shown$vl_type,
        c(
          "LittleBird",
          "FireScout",
          "CobraViper",
          "Apache",
          "Creek",
          "Lakota",
          "KiowaWarrior",
          "IroquoisHueyVenom",
          "BlackHawk",
          "SeaHawk",
          "PaveHawk",
          "CombatRescueHelicopter",
          "SeaKing",
          "Superhawk",
          "Chinook",
          "SeaDragonStallion",
          "Osprey",
          
          "BYOUltraLightweight",
          "BYOLightweight",
          "BYOMiddleweight",
          "BYOHeavyweight",
          "BYOUltraHeavyweight"
        )
      )
    }
    
    else if (input$top_chart == "Production Stage" &
             (
               !input$top_y %in% c(
                 "Speed (mph)",
                 "Range (mi)",
                 "Service ceiling (ft)",
                 "Rate of climb (ft/min)",
                 "Empty weight (lbs)",
                 "Max TOW (lbs)"
               )
             )) {
      shown <- shown %>% filter(Category == "new plan vertical lifts")
      shown_by_production <- select(shown, FY)
      
      # --------------------------------------------------------------------------------
      # run functions     
      
      if (input$UltraLightweight != 1 &
          input$Lightweight != 1 &
          input$Middleweight != 1 &
          input$Heavyweight != 1 & input$UltraHeavyweight != 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(shown)[-1][which(FVL_stats_gs$`Out of production` == 1)]])
        shown_by_production$`In Production` <-
          rowSums(shown[names(shown)[-1][which(FVL_stats_gs$`In production` == 1)]])
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(shown)[-1][which(FVL_stats_gs$`Future Vertical Lift` == 1)]])
        
      } else if (input$UltraLightweight == 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(FVL_stats_gs$UltraLightweight)[which(FVL_stats_gs$`Out of production` == 1 &
                                                                     FVL_stats_gs$UltraLightweight == 1)]])
        
        shown_by_production$`In Production` <-
          rowSums(shown[names(FVL_stats_gs$UltraLightweight)[which(FVL_stats_gs$`In production` == 1 &
                                                                     FVL_stats_gs$UltraLightweight == 1)]])
        
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(FVL_stats_gs$UltraLightweight)[which(FVL_stats_gs$`Future Vertical Lift` == 1 &
                                                                     FVL_stats_gs$UltraLightweight == 1)]])
      } else if (input$Lightweight == 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(FVL_stats_gs$Lightweight)[which(FVL_stats_gs$`Out of production` == 1 &
                                                                FVL_stats_gs$Lightweight == 1)]])
        
        shown_by_production$`In Production` <-
          rowSums(shown[names(FVL_stats_gs$Lightweight)[which(FVL_stats_gs$`In production` == 1 &
                                                                FVL_stats_gs$Lightweight == 1)]])
        
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(FVL_stats_gs$Lightweight)[which(FVL_stats_gs$`Future Vertical Lift` == 1 &
                                                                FVL_stats_gs$Lightweight == 1)]])
      } else if (input$Middleweight == 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(FVL_stats_gs$Middleweight)[which(FVL_stats_gs$`Out of production` == 1 &
                                                                 FVL_stats_gs$Middleweight == 1)]])
        
        shown_by_production$`In Production` <-
          rowSums(shown[names(FVL_stats_gs$Middleweight)[which(FVL_stats_gs$`In production` == 1 &
                                                                 FVL_stats_gs$Middleweight == 1)]])
        
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(FVL_stats_gs$Middleweight)[which(FVL_stats_gs$`Future Vertical Lift` == 1 &
                                                                 FVL_stats_gs$Middleweight == 1)]])
      } else if (input$Heavyweight == 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(FVL_stats_gs$Heavyweight)[which(FVL_stats_gs$`Out of production` == 1 &
                                                                FVL_stats_gs$Heavyweight == 1)]])
        
        shown_by_production$`In Production` <-
          rowSums(shown[names(FVL_stats_gs$Heavyweight)[which(FVL_stats_gs$`In production` == 1 &
                                                                FVL_stats_gs$Heavyweight == 1)]])
        
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(FVL_stats_gs$Heavyweight)[which(FVL_stats_gs$`Future Vertical Lift` == 1 &
                                                                FVL_stats_gs$Heavyweight == 1)]])
      } else if (input$UltraHeavyweight == 1) {
        shown_by_production$`Out of Production` <-
          rowSums(shown[names(FVL_stats_gs$UltraHeavyweight)[which(FVL_stats_gs$`Out of production` == 1 &
                                                                     FVL_stats_gs$UltraHeavyweight == 1)]])
        
        shown_by_production$`In Production` <-
          rowSums(shown[names(FVL_stats_gs$UltraHeavyweight)[which(FVL_stats_gs$`In production` == 1 &
                                                                     FVL_stats_gs$UltraHeavyweight == 1)]])
        
        shown_by_production$`Future Vertical Lift` <-
          rowSums(shown[names(FVL_stats_gs$UltraHeavyweight)[which(FVL_stats_gs$`Future Vertical Lift` == 1 &
                                                                     FVL_stats_gs$UltraHeavyweight == 1)]])
      }
      
      shown_by_production$Total <-
        rowSums(shown_by_production[-1])
      shown_by_production <-
        gather(shown_by_production,
               key = "Production Stage",
               value = "Quantity",
               -FY,
               -Total)
      
      shown_by_production$`Production Stage` <-
        factor(
          shown_by_production$`Production Stage`,
          levels =
            c(
              "Out of Production",
              "In Production",
              "Future Vertical Lift"
            )
        )
      
      future_2040 <-
        filter(
          shown_by_production,
          FY == max(shown_by_production$FY) &
            `Production Stage` == "Future Vertical Lift"
        )$Quantity
      percentage_2040 <- percent(future_2040 / (
        future_2040 +
          filter(
            shown_by_production,
            FY == max(shown_by_production$FY) &
              `Production Stage` == "In Production"
          )$Quantity +
          filter(
            shown_by_production,
            FY == max(shown_by_production$FY) &
              `Production Stage` == "Out of Production"
          )$Quantity
      ))
      
      p <- ggplot() +
        geom_area(data = shown_by_production, aes(x = FY, y = Quantity, fill = `Production Stage`)) +
        
        scale_fill_manual(
          values = c(
            "Out of Production" = "#56AB9A",
            "In Production" = "#BDD4DE",
            "Future Vertical Lift" = "#4D7FA3"
          ),
          labels = c(
            "Out of Production" = "Out of production ",
            "In Production" = "In production ",
            "Future Vertical Lift" = "Future Vertical Lift "
          )
        ) +
        
        geom_segment(
          aes(
            x = min(FVL_data_gs$`Build Plan`$FY),
            xend = max(FVL_data_gs$`Build Plan`$FY),
            y = future_2040,
            yend = future_2040
          ),
          color = '#554449',
          size = 1.5,
          linetype = "dotted"
        ) +
        geom_text(
          aes(
            x = max(shown_by_production$FY),
            y = future_2040,
            label = paste0(percentage_2040, " FVL"),
            vjust = -1
          ),
          fontface = "bold",
          colour = '#554449'
        )
    }
    
    if (input$top_chart == "Aircraft Type" & (
      !input$top_y %in% c(
        "Speed (mph)",
        "Range (mi)",
        "Service ceiling (ft)",
        "Rate of climb (ft/min)",
        "Empty weight (lbs)",
        "Max TOW (lbs)"
      )
    )) {
      if (input$UltraLightweight == 1 |
          input$Lightweight == 1 |
          input$Middleweight == 1 |
          input$Heavyweight == 1 | input$UltraHeavyweight) {
        p <-
          
          ggplot() +
          geom_area(data = shown, aes(
            x = FY,
            y = Quantity,
            fill = vl_type
          )) +
          
          scale_fill_manual(
            values = c(
              "LittleBird" = "#4DA396",
              "FireScout" = "#BDD4DE",
              
              "CobraViper" = "#4DA396",
              "Apache" = "#BDD4DE",
              "Creek" = "#C74745",
              "Lakota" = "#4D7FA3",
              "KiowaWarrior" = "#BDD4DE",
              "IroquoisHueyVenom" = "#F2BC57",
              
              "BlackHawk" = "#4DA396",
              "SeaHawk" = "#BDD4DE",
              "PaveHawk" = "#C74745",
              "CombatRescueHelicopter" = "#4D7FA3",
              "SeaKing" = "#F2BC57",
              "Seawolf" = "#4DA396",
              "Superhawk" = "#BDD4DE",
              
              "Chinook" = "#4DA396",
              "SeaDragonStallion" = "#BDD4DE",
              "Osprey" = "#C74745",
              
              "BYOUltraLightweight" = "#C74745",
              "BYOLightweight" = "#4DA396",
              "BYOMiddleweight" = "#C74745",
              "BYOHeavyweight" = "#4D7FA3",
              "BYOUltraHeavyweight" = "#4DA396"
              
            ),
            
            labels = c(
              "LittleBird" = "Little Bird ",
              "FireScout" = "Fire Scout ",
              "CobraViper" = "Cobra / Viper ",
              "Apache" = "Apache ",
              "Creek" = "Creek ",
              "Lakota" = "Lakota ",
              "KiowaWarrior" = "Kiowa (Warrior) ",
              "IroquoisHueyVenom" = "Iroquois / Huey / Venom ",
              "BlackHawk" = "BlackHawk ",
              "SeaHawk" = "Sea Hawk ",
              "PaveHawk" = "Pave Hawk ",
              "CombatRescueHelicopter" = "Combat Rescue Helicopter ",
              "SeaKing" = "Sea King ",
              "Superhawk" = "Superhawk ",
              "Chinook" = "Chinook ",
              "SeaDragonStallion" = "Sea Dragon / Stallion ",
              "Osprey" = "Osprey ",
              
              "BYOUltraLightweight" = "New Ultra-Lightweight ",
              "BYOLightweight" = "New Lightweight ",
              "BYOMiddleweight" = "New Middleweight ",
              "BYOHeavyweight" = "New Heavyweight ",
              "BYOUltraHeavyweight" = "New Ultra-Heavyweight "
            )
          )
      } else{
        shown$Function[shown$vl_type %in% names(current$FVL_stats_gs$UltraLightweight)[which(current$FVL_stats_gs$UltraLightweight == 1)]] <-
          "UltraLightweight"
        shown$Function[shown$vl_type %in% names(current$FVL_stats_gs$Lightweight)[which(current$FVL_stats_gs$Lightweight == 1)]] <-
          "Lightweight"
        shown$Function[shown$vl_type %in% names(current$FVL_stats_gs$Middleweight)[which(current$FVL_stats_gs$Middleweight == 1)]] <-
          "Middleweight"
        shown$Function[shown$vl_type %in% names(current$FVL_stats_gs$Heavyweight)[which(current$FVL_stats_gs$Heavyweight == 1)]] <-
          "Heavyweight"
        shown$Function[shown$vl_type %in% names(current$FVL_stats_gs$UltraHeavyweight)[which(current$FVL_stats_gs$UltraHeavyweight == 1)]] <-
          "UltraHeavyweight"
        
        shown$Function <- factor(
          shown$Function,
          levels =
            c(
              "UltraLightweight",
              "Lightweight",
              "Middleweight",
              "Heavyweight",
              "UltraHeavyweight"
            )
        )
        
        shown <-
          shown %>% group_by(FY, Function) %>% mutate(Quantity = sum(Quantity)) %>% ungroup %>%
          select(FY, Total, Quantity, Function) %>% unique
        
        p <-

          ggplot() +
          geom_area(data = shown,
                    aes(
                      x = FY,
                      y = Quantity,
                      fill = Function
                    ),
                    alpha = .875) +
          
          
          scale_fill_manual(
            values = c(
              "UltraLightweight" = "#115175",
              "Lightweight" = "#0a8672",
              "Middleweight" = "#BDD4DE",
              "Heavyweight" = "#C74745",
              "UltraHeavyweight" = "#F2BC57"
            ),
            
            labels = c(
              "UltraLightweight" = "UltraLightweight ",
              "Lightweight" = "Lightweight ",
              "Middleweight" = "Middleweight ",
              "Heavyweight" = "Heavyweight ",
              "UltraHeavyweight" = "UltraHeavyweight "
            )
          )
      }
    }
    
    # --------------------------------------------------------------------------------
    # add options common to both views (Line/Area)
    
    p <- p + ggtitle(update_title(input)[1]) +
      
      scale_y_continuous(labels = (switch(
        input$top_y,
        "# of Vertical Lift" = format_ylab,
        "Crew" = format_ylab,
        "Passengers" = format_ylab,
        "Speed (mph)" = format_ylab,
        "Range (mi)" = format_ylab,
        "Service ceiling (ft)" = format_ylab,
        "Rate of climb (ft/min)" = format_ylab,
        "Empty weight (lbs)" = format_ylab,
        "Max TOW (lbs)" = format_ylab
      ))) +
      
      
      geom_rect(aes(
        xmin = min(FVL_data_gs$`Build Plan`$FY),
        xmax = 2024,
        ymin = 0,
        ymax = Inf
      ),
      alpha = 0.05,
      fill = "grey") +
      geom_rect(aes(
        xmin = 2024,
        xmax = max(FVL_data_gs$`Build Plan`$FY),
        ymin = 0,
        ymax = Inf
      ),
      alpha = 0.2,
      fill = "grey") +
      
      
      scale_x_continuous(
        breaks = seq(2020, max(shown$FY), 2),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        nrow = 0 + (input$top_chart == "Aircraft Type") + (input$top_chart == "Production Stage"),
        reverse = FALSE
      )) +
      
      xlab("Fiscal Year") +
      ylab(input$top_y)
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  # --------------------------------------------------------------------------------
  # format data for use by bottom chart     
  
  budget_dataset <- reactive({
    if (input$bottom_chart == "Change") {
      return(get_budget_change_data(current, input))
    } else if (input$bottom_chart == "Total") {
      return(get_budget_total_data(current, current$vl, current$FVL_data_gs, input))
    }
  })
  
  # --------------------------------------------------------------------------------
  # create budget plot     
  
  output$budget <- renderPlot({
    budget_plot()
  })
  
  budget_plot <- reactive({
    shown <- budget_dataset()
    
    if (input$checkbox == TRUE) {
      shown %<>% deflate_frame()
    }
  
    # --------------------------------------------------------------------------------
    # (currently hidden: change chart)   
      
    # options specific to change chart
    # if(input$bottom_chart == "Change"){
    #
    #   shown$budget_cat <- factor(
    #     shown$budget_cat,
    #     levels = c("Added O&S", "Added Acq","Added R&D", "Cut O&S", "Cut Acq", "Cut R&D"))
    #
    #   p <- ggplot(data = shown, aes(x = FY, y = Total)) +
    #     geom_area(stat = 'identity', alpha = .50, aes(fill = budget_cat)) +
    #     ggtitle(update_title(input)[2]) +
    #     scale_fill_manual(
    #       values = c(
    #         "Added O&S" = "#115175",
    #         "Added Acq" = "#0a8672",
    #         "Added R&D" = "#F2A85A",
    #         "Cut R&D" = "#F2BC57",
    #         "Cut O&S" = "#788ca8",
    #         "Cut Acq" = "#0faa91"),
    #       labels = c(
    #         "Added O&S" = "Added Operation & Support  ",
    #         "Added Acq" = "Added Acquisition  ",
    #         "Added R&D" = "Added R&D ",
    #         "Cut R&D" = "Decreased R&D",
    #         "Cut O&S" = "Decreased Operation & Support  ",
    #         "Cut Acq" = "Decreased Acquisition  "))
    #
    #   # add net change line and y limits
    #   netdata <- shown %>%
    #     group_by(FY) %>%
    #     summarize(Total = sum(Total))
    #
    #   # y_limits <- find_budget_change_limits(shown)
    #
    #   p <- p +
    #     geom_line(data = netdata, aes(color = "Net Change"), size = 1) +
    #     scale_color_manual(values = "#C76363")
    #   # +
    #   #   coord_cartesian(ylim = c(y_limits[1], y_limits[2]))
    #   #   coord_cartesian(ylim = y_limits[1])
    #
    # } else if(input$bottom_chart == "Total"){
    
    shown <- subset_budget_total(shown, input)
    
    # y_limit <- max(shown$Total[shown$budget_cat == "Acquisition"]) +
    #   max(shown$Total[shown$budget_cat == "O&S"]) +
    #   max(shown$Total[shown$budget_cat == "R&D"])
    
    # if(y_limit > 80e9) y_limit <- y_limit * 1.1
    # else if(y_limit > 50e9) y_limit <- 80e9
    # else y_limit <- 50e9
    
    # y_limits <- find_budget_total_limits(shown)
    
    p <- ggplot() +
      geom_area(
        data = shown,
        aes(x = FY, y = Total, fill = budget_cat),
        stat = 'identity',
        alpha = .80
      ) +
      ggtitle(update_title(input)[2]) +
      scale_fill_manual(
        values = c(
          "O&S" = "#115175",
          "Acquisition" = "#0a8672",
          "R&D" = "#F2BC57",
          "Upgrade" = "#C74745"
        ),
        
        labels = c(
          "O&S" = "Operations & Support  ",
          "Acquisition" = "Acquisition  ",
          "R&D" = "Research & Development ",
          "Upgrade" = "Upgrade "
        )
      ) +
      
      geom_rect(aes(
        xmin = min(FVL_data_gs$`Build Plan`$FY),
        xmax = 2024,
        ymin = 0,
        ymax = Inf
      ),
      alpha = 0.05,
      fill = "grey") +
      geom_rect(aes(
        xmin = 2024,
        xmax = max(FVL_data_gs$`Build Plan`$FY),
        ymin = 0,
        ymax = Inf
      ),
      alpha = 0.2,
      fill = "grey")
    
    # --------------------------------------------------------------------------------
    # options applied to both charts    
    
    p <- p +
      scale_y_continuous(labels = format_ylab2) +
      scale_x_continuous(
        breaks = seq(2020, max(shown$FY), 2),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        nrow = 1 + (input$bottom_chart == "Change"),
        reverse = FALSE
      )) +
      xlab("Fiscal Year") +
      ylab("Constant FY18 Dollars") +
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans"
      )) +
      labs(caption = "Source: USA; USN; USAF; CBO; GAO; CSIS analysis",
           size = 30,
           family = "Open Sans")
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  # --------------------------------------------------------------------------------
  # hover feature for top chart    

  output$hover_info_vl <- renderUI({
    hover <- input$plot_hover_vl
    shown <- vl_dataset()
    
    if (is.null(hover))
      return(NULL)
    
    hover_year <- round(hover$x)
    
    # --------------------------------------------------------------------------------
    # limit hover year 
    # note: limit hover year to between 2019 and 2040 
    # ...(otherwise hover_year can take values 2017 and 2047 even if these two years
    # ...don't exist in the data)
    
    if ((hover_year < min(FVL_data_gs$`Build Plan`$FY)) |
        hover_year > max(FVL_data_gs$`Build Plan`$FY))
      return(NULL)
    
    # --------------------------------------------------------------------------------
  
    hover_year_index <- which(shown$FY == hover_year)[1]
    
    hover_ships_new <- shown %>%
      filter(Category == "new plan vertical lifts") %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    hover_ships_old <- shown %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    # Use HTML/CSS to change style of tooltip panel here
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0.90); ",
      "left:",
      left_px - 275,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    # --------------------------------------------------------------------------------
    # subset function  
    
    new_plan <- shown %>%
      filter(FY == hover_year &
               Category == "new plan vertical lifts") %>%
      select(-FY,-Total,-Category)
    
    subset_by_weight <-
      c(
        "UltraLightweight",
        "Lightweight",
        "Middleweight",
        "Heavyweight",
        "UltraHeavyweight"
      )
    subset_by_production <- c("In production", "Out of production",
                              "Future Vertical Lift")
    
    
    if ((
      input$top_chart == "Line" |
      input$top_chart == "Aircraft Type" |
      input$top_chart == "Production Stage"
    ) &
    input$UltraLightweight != 1 &
    input$Lightweight != 1 &
    input$Middleweight != 1 & input$Heavyweight != 1) {
      if (input$top_chart == "Aircraft Type" | input$top_chart == "Line") {
        for (i in 1:length(subset_by_weight)) {
          new_plan[[subset_by_weight[i]]] <-
            rowSums(new_plan[, names(new_plan) %in%
                               names(FVL_stats_gs[[subset_by_weight[i]]])[which(FVL_stats_gs[[subset_by_weight[i]]] == 1)], drop =
                               FALSE])
        }
        new_plan <- new_plan[, subset_by_weight]
      } else if (input$top_chart == "Production Stage") {
        for (i in 1:length(subset_by_production)) {
          new_plan[[subset_by_production[i]]] <-
            rowSums(new_plan[, names(new_plan) %in%
                               names(FVL_stats_gs[[subset_by_production[i]]])[which(FVL_stats_gs[[subset_by_production[i]]] == 1)], drop =
                               FALSE])
        }
        new_plan <- new_plan[, subset_by_production]
      }
      
    }
    
    
    tooltip_string <- HTML(paste0(sapply(names(new_plan),
                                         function(vlname) {
                                           return(
                                             paste0(
                                               "<b>",
                                               vlname,
                                               "</b>",
                                               ": ",
                                               
                                               as.character(prettyNum(round(new_plan[[vlname]]), big.mark  = ",")),
                                               "&nbsp",
                                               "&nbsp",
                                               "&nbsp",
                                               "(",
                                               percent(round(new_plan[[vlname]] / hover_ships_new, 5)),
                                               ")"
                                             )
                                           )
                                         }),
                                  collapse = "<br>"))
    
    
    hover_ships_new <-
      prettyNum(round(hover_ships_new), big.mark = ",")
    hover_ships_old <- prettyNum(hover_ships_old, big.mark = ",")
    
    topy <- input$top_y
    
    
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<div align = 'left'>",
                  
                  "<b><u>",
                  topy,
                  " in FY",
                  hover_year,
                  ":</b></u><br>",
                  
                  br(),
                  "<b> Baseline: ",
                  "</b>",
                  hover_ships_old,
                  
                  br(),
                  "<b> Vertical Lift Plan: ",
                  "</b>",
                  hover_ships_new,
                  
                  br(),
                  "<br/>",
                  
                  if (input$top_chart == "Line" |
                      input$top_chart == "Aircraft Type") {
                    if (!input$top_y %in% c(
                      "Speed (mph)",
                      "Range (mi)",
                      "Service ceiling (ft)",
                      "Rate of climb (ft/min)",
                      "Empty weight (lbs)",
                      "Max TOW (lbs)"
                    )) {
                      br()
                      br()
                      
                      tooltip_string
                      
                    } else{
                      br()
                      br()
                      
                      if (input$UltraLightweight == 1 |
                          input$Lightweight == 1 |
                          input$Middleweight == 1 |
                          input$Heavyweight == 1 | input$UltraHeavyweight == 1) {
                        HTML(paste0(sapply(names(new_plan),
                                           function(vlname) {
                                             return(paste0(
                                               "<b>",
                                               vlname,
                                               "</b>",
                                               ": ",
                                               as.character(prettyNum(
                                                 current$FVL_stats_gs[[input$top_y]][[vlname]],
                                                 big.mark  = ","
                                               ))
                                             ))
                                           }),
                                    collapse = "<br>"))
                        
                      } else{
                        new_plan_performance <- current$FVL_stats_gs[[input$top_y]]

                        col <- which(new_plan_performance[1, ] == 0)
                        
                        new_plan_ships <-
                          select(filter(current$vl, FY == hover_year), -FY)

                        if (length(col) != 0) {
                          new_plan_ships[, col] = 0
                        }
                        
                        for (i in 1:length(subset_by_weight)) {
                          new_plan_performance[subset_by_weight[i]] <-
                            new_plan[[subset_by_weight[i]]] /
                            rowSums(new_plan_ships[names(new_plan_ships) %in% names(FVL_stats_gs[[subset_by_weight[i]]])[which(FVL_stats_gs[[subset_by_weight[i]]] == 1)], drop =
                                                     FALSE])
                        }
                        
                        new_plan_performance <-
                          new_plan_performance[, subset_by_weight]
                        
                        HTML(paste0(sapply(names(new_plan_performance),
                                           function(vlname) {
                                             return(paste0(
                                               "<b>",
                                               vlname,
                                               "</b>",
                                               ": ",
                                               as.character(prettyNum(
                                                 new_plan_performance[[vlname]],
                                                 big.mark  = ","
                                               ))
                                             ))
                                           }),
                                    collapse = "<br>"))
                      }
                    }
                  } else{
                    if (!input$top_y %in% c(
                      "Speed (mph)",
                      "Range (mi)",
                      "Service ceiling (ft)",
                      "Rate of climb (ft/min)",
                      "Empty weight (lbs)",
                      "Max TOW (lbs)"
                    )) {
                      br()
                      br()
                      
                      tooltip_string
                      
                    } else{
                      br()
                      br()
                      
                      if (input$UltraLightweight == 1 |
                          input$Lightweight == 1 |
                          input$Middleweight == 1 |
                          input$Heavyweight == 1 | input$UltraHeavyweight == 1) {
                        HTML(paste0(sapply(names(new_plan),
                                           function(vlname) {
                                             return(paste0(
                                               "<b>",
                                               vlname,
                                               "</b>",
                                               ": ",
                                               as.character(prettyNum(
                                                 current$FVL_stats_gs[[input$top_y]][[vlname]],
                                                 big.mark  = ","
                                               ))
                                             ))
                                           }),
                                    collapse = "<br>"))
                        
                      } else{
                        new_plan_performance <- current$FVL_stats_gs[[input$top_y]]

                        col <- which(new_plan_performance[1, ] == 0)
                        
                        new_plan_ships <-
                          select(filter(current$vl, FY == hover_year), -FY)

                        if (length(col) != 0) {
                          new_plan_ships[, col] = 0
                        }
                        
                        for (i in 1:length(subset_by_production)) {
                          new_plan_performance[subset_by_production[i]] <-
                            new_plan[[subset_by_production[i]]] /
                            rowSums(new_plan_ships[names(new_plan_ships) %in% names(FVL_stats_gs[[subset_by_production[i]]])[which(FVL_stats_gs[[subset_by_production[i]]] == 1)], drop =
                                                     FALSE])
                        }
                        
                        new_plan_performance <-
                          new_plan_performance[, subset_by_production]
                        
                        HTML(paste0(sapply(names(new_plan_performance),
                                           function(vlname) {
                                             return(paste0(
                                               "<b>",
                                               vlname,
                                               "</b>",
                                               ": ",
                                               as.character(prettyNum(
                                                 new_plan_performance[[vlname]],
                                                 big.mark  = ","
                                               ))
                                             ))
                                           }),
                                    collapse = "<br>"))
                      }
                    }
                    
                  }
                )
              )))
  })
  
  # --------------------------------------------------------------------------------
  # hover feature for bottom chart     

  output$hover_info_budget <- renderUI({
    hover <- input$plot_hover_budget
    
    if (is.null(hover)) {
      return()
    }
    
    hover_year <- round(hover$x)
    
    # --------------------------------------------------------------------------------
    # limit hover year 
    # note: limit hover year to between 2019 and 2040 
    # ...(otherwise hover_year can take values 2017 and 2047 even if these two years
    # ...don't exist in the data)
    
    if ((hover_year < min(FVL_data_gs$`Build Plan`$FY)) |
        hover_year > max(FVL_data_gs$`Build Plan`$FY))
      return(NULL)
    
    # --------------------------------------------------------------------------------
    # hover calculations 
    # note: calculate point position INSIDE the image as percent of total dimensions
    # ...from left (horizontal) and from top (vertical)
    
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    # Use HTML/CSS to change style of tooltip panel here
    
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0.90); border-color: none;",
      "left:",
      left_px - 225,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    if (input$bottom_chart == "Change") {
      wellPanel(style = style,
                p(HTML(
                  create_change_tip(hover_year, budget_dataset())
                )))
    } else if (input$bottom_chart == "Total") {
      wellPanel(style = style,
                p(HTML(
                  create_total_tip(
                    hover_year,
                    subset_budget_total(budget_dataset(), input)
                  )
                )))
    }
  })

  # --------------------------------------------------------------------------------

  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })

  # --------------------------------------------------------------------------------

  output$specs_change <- renderUI({
    performance_specs_change <- performance_change(current$vl,
                                                   current$FVL_data_gs,
                                                   stats_df = current$FVL_stats_gs,
                                                   input)
    
    tagList(helpText(HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> <b> &nbsp Percent Change in Avg. Performance Specs, ",
        min(current$vl$FY),
        "-",
        max(current$vl$FY),
        ": </b> </h5>",
        "<h5> &nbsp Speed (mph)：",
        performance_specs_change[1],
        "</h5>",
        "<h5> &nbsp Range (mi)：",
        performance_specs_change[2],
        "</h5>",
        "<h5> &nbsp Service ceiling (ft)：",
        performance_specs_change[3],
        "</h5>",
        "<h5> &nbsp Rate of climb (ft/min)：",
        performance_specs_change[4],
        "</h5>",
        "<h5> &nbsp Empty weight (lbs)：",
        performance_specs_change[5],
        "</h5>",
        "<h5> &nbsp Max TOW (lbs)：",
        performance_specs_change[6],
        "</h5>",
        "<h5> &nbsp Passengers：",
        performance_specs_change[7],
        "</h5>"
      )
    )))
  })
  
  
  output$info_display <- renderUI({
    tagList(helpText(HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> <b> 2019 - 2023 </b>: The baseline setting for these five years is based on the PB19 Future Years Defense Program and current inventory levels. </h5>",
        "<h5> <b> 2024 - 2040 </b>: For the following time period, there is less certainty.</h5>",
        "<h5> Unlike with the Navy’s 30-year shipbuilding plan, there is no comparable plan for the vertical lift fleet. The baseline settings include some estimates. It incorporate near-term acquisition projections that are available within Selected Acquisition Reports. It also projects retirements of older platforms and estimates continuing O&S and R&D costs based on current funding levels.</h5>",
        "<h5> Due to this lack of certainty, it is up to the user to build their own 20-year vertical lift plan. This app makes it possible to compare a range of plans, while evaluating many of the tradeoffs that policymakers face today.</h5>",
        "</div>"
      )
    )))
  })
  
  
  output$cost_change <- renderUI({
    budget_data <- budget_dataset()
    shown <- subset_budget_total(budget_data, input)
    
    tagList(helpText(HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> <b> &nbsp Average Annual Budget, ",
        min(shown$FY) ,
        "-",
        max(shown$FY),
        ":</b> </h5>",
        
        "<h5> &nbsp Acquisition: $",
        round(sum(shown$Total[which(shown$budget_cat == "Acquisition")]
                  / 1e9) / (max(shown$FY) - min(shown$FY) + 1), 2),
        "B</h5>",
        
        "<h5> &nbsp O&S: $",
        round(sum(shown$Total[which(shown$budget_cat == "O&S")]
                  / 1e9) / (max(shown$FY) - min(shown$FY) + 1), 2),
        "B</h5>",
        
        "<h5> &nbsp R&D: $",
        round(sum(shown$Total[which(shown$budget_cat == "R&D")]
                  / 1e9) / (max(shown$FY) - min(shown$FY) + 1), 2),
        "B</h5>",
        
        "<h5> &nbsp Upgrade: $",
        round(sum(shown$Total[which(shown$budget_cat == "Upgrade")]
                  / 1e9) / (max(shown$FY) - min(shown$FY) + 1), 2),
        "B</h5>",
        
        "<h5> &nbsp Total: $",
        round((summarize(
          shown, sum(Total)
        ) / 1e9) / (max(shown$FY) - min(shown$FY) + 1), 2),
        "B</h5>",
        
        # "<br/>",
        "</div>"
        
      )
    )))
  })
  
  # --------------------------------------------------------------------------------
  # disable feature
  
  observeEvent(input$UltraLightweight, ({
    updateButton(session, "Lightweight", disabled = input$UltraLightweight)
    updateButton(session, "Middleweight", disabled = input$UltraLightweight)
    updateButton(session, "Heavyweight", disabled = input$UltraLightweight)
    updateButton(session, "UltraHeavyweight", disabled = input$UltraLightweight)
  }))
  
  observeEvent(input$Lightweight, ({
    updateButton(session, "UltraLightweight", disabled = input$Lightweight)
    updateButton(session, "Middleweight", disabled = input$Lightweight)
    updateButton(session, "Heavyweight", disabled = input$Lightweight)
    updateButton(session, "UltraHeavyweight", disabled = input$Lightweight)
  }))
  
  observeEvent(input$Middleweight, ({
    updateButton(session, "UltraLightweight", disabled = input$Middleweight)
    updateButton(session, "Lightweight", disabled = input$Middleweight)
    updateButton(session, "Heavyweight", disabled = input$Middleweight)
    updateButton(session, "UltraHeavyweight", disabled = input$Middleweight)
  }))
  
  observeEvent(input$Heavyweight, ({
    updateButton(session, "UltraLightweight", disabled = input$Heavyweight)
    updateButton(session, "Lightweight", disabled = input$Heavyweight)
    updateButton(session, "Middleweight", disabled = input$Heavyweight)
    updateButton(session, "UltraHeavyweight", disabled = input$Heavyweight)
  }))
  
  observeEvent(input$UltraHeavyweight, ({
    updateButton(session, "UltraLightweight", disabled = input$UltraHeavyweight)
    updateButton(session, "Lightweight", disabled = input$UltraHeavyweight)
    updateButton(session, "Middleweight", disabled = input$UltraHeavyweight)
    updateButton(session, "Heavyweight", disabled = input$UltraHeavyweight)
  }))
  
  # --------------------------------------------------------------------------------
  # disable feature - preset buttons 
  
  observeEvent(input$preset_value1, ({
    shinyjs::hide("preset_value2")
    shinyjs::hide("preset_value3")
    shinyjs::hide("preset_value4")
    shinyjs::hide("preset_value5")
  }))
  
  observeEvent(input$preset_value2, ({
    shinyjs::hide("preset_value1")
    shinyjs::hide("preset_value3")
    shinyjs::hide("preset_value4")
    shinyjs::hide("preset_value5")
  }))
  
  observeEvent(input$preset_value3, ({
    shinyjs::hide("preset_value1")
    shinyjs::hide("preset_value2")
    shinyjs::hide("preset_value4")
    shinyjs::hide("preset_value5")
  }))
  
  observeEvent(input$preset_value4, ({
    shinyjs::hide("preset_value1")
    shinyjs::hide("preset_value2")
    shinyjs::hide("preset_value3")
    shinyjs::hide("preset_value5")
  }))
  
  observeEvent(input$preset_value5, ({
    shinyjs::hide("preset_value1")
    shinyjs::hide("preset_value2")
    shinyjs::hide("preset_value3")
    shinyjs::hide("preset_value4")
  }))
  
  # --------------------------------------------------------------------------------
  # (currently hidden) user submission function 
  
  observeEvent(input$submit, {
    email_string <- ifelse(input$text == "", "None", input$text)
    file <- file.path("responses",
                      paste0(email_string,
                             format(Sys.time(), '_%Y%m%d_%H%M%S'),
                             ".csv"))
    output_frame <- current$vl %>%
      filter(FY == max(FY))
    output_frame$email <- email_string
    output_frame$comments <-
      ifelse(input$text2 == "", "None", input$text2)
    
    write_csv(output_frame, file)
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Success!')
  })
  
  # --------------------------------------------------------------------------------

  output$description <- renderUI({
    HTML(
      "<div align = 'center'>",
      "<b> Build your own vertical lift fleet </b>",
      "<hr>",
      "<h5><b> Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey </b></h4>",
      "<hr>",
      "</div>",
      "<div align = 'left'>",
      "<h5> We designed this app to give our audience the ability to build their own vertical lift fleet. In seconds, you can see the impact that this fleet plan could have on a range of factors. </h5>",
      "<h5> You can build more helicopters, retire existing helicopters, develop new platforms, change the build / retire rate, adjust cost and performance assumptions, and much more. The factors that we account for include: acquisition cost, operations and support costs, research and development costs, crew size, passenger capacity, speed, range, and takeoff weight. </h5>",
      "<h5> The U.S. military faces the challenge of how to allocate resources between operating existing fleets, maintaining industrial capacity, and developing new capabilities. These decisions involve many stakeholders, including leaders within Congress, the White House, industry, Army, Navy, Marines, and Air Force. </h5>",
      "<h5> Unlike in many private sector industries, stakeholders in the public sector do not share access to tools that make it easy to understand and plan for investments. While key stakeholders can hold different and sometimes competing perspectives, there are many areas of common ground. But progress requires more than a shared vision. It also relies on developing a better understanding of our current plans and the potential impact of new investments. </h5>",
      "<h5> The impact of potential government investments is generally evaluated in terms of financial costs. And the list of investment options that get evaluated is quite short, limited mainly to legislative proposals or agency recommendations. We rely primarily on the Congressional Budget Office for these types of assessments, whose role is to provide independent analysis of the budgetary and economic impact that public policies may have. For instance, the CBO provides helpful reports that examine the specific costs associated with Department of Defense proposals. But why stop there?</h5>",
      "<h5> First, we should more comprehensively evaluate government investments to include other factors such as personnel requirements, the impact on jobs, and the implications on capacity or capabilities. Second, we should have the ability to evaluate a longer list of investment options and to compare those plans against each other. This app is designed to do just that for the U.S. vertical lift fleet, and is just one small step in that direction.</h5>",
      "</div>"
    )
  })
  
  # --------------------------------------------------------------------------------
  
  output$bottom_line <- renderUI({
    HTML(
      paste0(
        "<br/>",
        "<div align = 'center'>",
        "<iframe width=100% height=800 src=https://app.powerbi.com/view?r=eyJrIjoiZWUxMTc2NzctNjE4YS00MWUxLTk5MDMtNWQ2ZTE0YWFiMzk0IiwidCI6IjE5ZmViNzVjLTcwY2ItNDA1MS04NTBlLWEyNDk1MGE4OWMyMCIsImMiOjF9 frameborder=0 allowFullScreen=true></iframe>",
        "<br/>"
      )
    )
    
  })
  
  observeEvent(input$reset_input, {
    
    shinyjs::reset("side-panel")
    
    shinyjs::show("preset_value1")
    shinyjs::show("preset_value2")
    shinyjs::show("preset_value3")
    shinyjs::show("preset_value4")
    shinyjs::show("preset_value5")
    
    # --------------------------------------------------------------------------------
    # reset double click issue 
    # note: the following platforms have this issue 
    
    updateSliderInput(
      session,
      inputId = "Apache-build_retire_rate",
      label = "Build rate",
      value = max_build_rate[["Apache"]],
      min = 0,
      max = 100,
      step = 0.01
    )
    
    updateSliderInput(
      session,
      inputId = "BlackHawk-build_retire_rate",
      label = "Build rate",
      value = max_build_rate[["BlackHawk"]],
      min = 0,
      max = 100,
      step = 0.01
    )
    
    updateSliderInput(
      session,
      inputId = "Chinook-build_retire_rate",
      label = "Build rate",
      value = max_build_rate[["Chinook"]],
      min = 0,
      max = 100,
      step = 1
    )
    
    updateSliderInput(
      session,
      inputId = "SeaDragonStallion-build_retire_rate",
      label = "Build rate",
      value = max_build_rate[["SeaDragonStallion"]],
      min = 0,
      max = 100,
      step = 0.01
    )
    
    updateSliderInput(
      session,
      inputId = "Osprey-build_retire_rate",
      label = "Build rate",
      value = max_build_rate[["Osprey"]],
      min = 0,
      max = 100,
      step = 0.01
    )
  })
  
  popover.templat.preset(
    session,
    button_id = "preset_value1",
    label = "Replace middleweight helos first in 2030",
    description = FVL_data_gs$preset_phase1$OptionDescriptions[1]
  )
  popover.templat.preset(
    session,
    button_id = "preset_value2",
    label = "Replace lightweight helos first in 2025",
    description = FVL_data_gs$preset_phase2$OptionDescriptions[1]
  )
  popover.templat.preset(
    session,
    button_id = "preset_value3",
    label = "Begin Future Vertical Lift at a low-rate of production",
    description = FVL_data_gs$preset_phase3$OptionDescriptions[1]
  )
  popover.templat.preset(
    session,
    button_id = "preset_value4",
    label = "Delay Future Vertical Lift and upgrade older helos",
    description = FVL_data_gs$preset_phase4$OptionDescriptions[1]
  )
  popover.templat.preset(
    session,
    button_id = "preset_value5",
    label = "Replace all helos beginning in the 2035",
    description = FVL_data_gs$preset_phase5$OptionDescriptions[1]
  )
  
})

# ================================================================================