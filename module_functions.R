# ================================================================================
# Build Your Own: Vertical Lift
# Designed and built by Gabriel Coll and Yuanjing Han
# --------------------------------------------------------------------------------
# module functions 
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(extrafont)
library(extrafontdb)

# --------------------------------------------------------------------------------
# load data

load("data/FVL_data_gs.Rda")
load("data/FVL_stats_gs.Rda")

# --------------------------------------------------------------------------------
# begin functions

# build_start_yr is the start year of the original build plan
build_start_yr <- FVL_stats_gs$Build_start_yr

# max_build_rate is the average build rate of the original build plan
max_build_rate <- FVL_stats_gs$max_build_rate

# the number of helos we'll have by 2040 if we don't build/retire anything
default_vl <- filter(FVL_data_gs[["Fleet Plan"]], FY == max(FY))

# max_vl is the maximum number of vertical lifts we end up with by 2040
# retirement_base is the number of helos we currently have in the fleet
retirement_base <-
  as.data.frame(lapply(FVL_stats_gs$to_retire, function(x)
    as.numeric(x)))
max_vl <- retirement_base
max_vl[which(max_build_rate != 0)] <-
  floor(retirement_base[which(max_build_rate != 0)] +
          (FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - build_start_yr[which(max_build_rate != 0)] + 1) *
          max_build_rate[which(max_build_rate != 0)])

# ================================================================================
# 1st module type: outofproductionvlUI

# note: cannot build more for some existing helicopters,
# ...i.e. Little Bird, Seahawk, Seaking

outofproductionvlUI <- function(id, labelname) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelname),
      style = "basic",
      value = 0,
      type = "toggle",
      size = "extra-small",
      block = TRUE,
      disabled = FALSE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 0"),
      
      br(),
      
      fluidRow(
        column(1),
        
        column(
          10,
          sliderInput(
            inputId = ns("slider"),
            label = NA,
            value = as.numeric(FVL_data_gs$default[1, id]),
            min = 0,
            max = max_vl[[id]],
            step = 1,
            width = '100%',
            ticks = FALSE
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] != 'Nothing'"),
          column(
            1,
            actionLink(
              inputId = ns("rate_yr_upgrade"),
              label = "",
              icon = icon("plus"),
              value = 0,
              type = "toggle"
            ),
            actionLink(
              inputId = ns("c"),
              label = strong(" "),
              icon = icon("bars")
            )
          )
        )
      ),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_yr_upgrade"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("build_retire_rate"),
                     label = h6(em("Retirement Rate")),
                     value = as.numeric(FVL_data_gs$default[3, id]),
                     min = 0,
                     max = retirement_base[[id]],
                     step = 1,
                     width = '100%',
                     ticks = FALSE
                   )
                 )
               ),
               column(1)),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_yr_upgrade"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("build_start_yr"),
                     label = h6(em("Start Year")),
                     value = as.numeric(FVL_data_gs$default[4, id]),
                     min = min(FVL_data_gs$`Build Plan`$FY),
                     max = max(FVL_data_gs$`Build Plan`$FY),
                     step = 1,
                     width = '100%',
                     ticks = FALSE,
                     sep = ""
                   )
                 )
               ),
               column(1)),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_yr_upgrade"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("upgrade_slider"),
                     label = h6(em("Upgrade")),
                     value = 0,
                     min = 0,
                     max = unlist(retirement_base[id]),
                     step = 1,
                     width = '100%',
                     ticks = FALSE
                   )
                 )
               ),
               column(1)),
      
      bsTooltip(
        ns("slider"),
        paste0("Set inventory level for ", max(FVL_data_gs$`Build Plan`$FY)),
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("rate_yr_upgrade"),
        "Expand slider selection",
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("c"),
        "Change the cost assumptions",
        "left",
        options = list(container = "body")
      )
      
    )
  )
}


outofproductionvl <-
  function(input,
           output,
           session,
           mydata,
           vlname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    observeEvent({
      input$slider
      input$build_retire_rate
      input$build_start_yr
    },
    {
      updateSliderInput(session,
                        inputId = "build_retire_rate",
                        min = ceiling((retirement_base[[vlname]] - input$slider) /
                                        (mydata$vl$FY[nrow(mydata$vl)] - input$build_start_yr + 1)
                        ))
      updateSliderInput(session,
                        inputId = "upgrade_slider",
                        max = input$slider)
      
      mydata$vl[vlname] <-
        update_current_vl(vlname, input, mydata)
    })
    
    observeEvent({
      input$upgrade_slider
      input$upgrade_start_yr
    },
    {
      updateSliderInput(session,
                        inputId = "upgrade_rate",
                        min = ceiling(
                          min(input$slider, input$upgrade_start_yr) /
                            (mydata$vl$FY[nrow(mydata$vl)] - input$upgrade_start_yr + 1)
                        ))
      if (input$upgrade_slider > 0) {
        updateSliderInput(
          session,
          inputId = "speed",
          value = as.numeric(FVL_stats_gs$`Upgraded Speed (mph)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "range",
          value = as.numeric(FVL_stats_gs$`Upgraded Range (mi)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "service_ceiling",
          value = as.numeric(FVL_stats_gs$`Upgraded Service ceiling (ft)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "rate_climbing",
          value = as.numeric(FVL_stats_gs$`Upgraded Rate of climb (ft/min)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "empty_weight",
          value = as.numeric(FVL_stats_gs$`Upgraded Empty weight (lbs)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "max_tow",
          value = as.numeric(FVL_stats_gs$`Upgraded Max TOW (lbs)`[[vlname]])
        )
      }
      
      mydata$FVL_stats_gs$`Upgraded Speed (mph)`[vlname] <-
        input$speed
      mydata$FVL_stats_gs$`Upgraded Range (mi)`[vlname] <-
        input$range
      mydata$FVL_stats_gs$`Upgraded Service ceiling (ft)`[vlname] <-
        input$service_ceiling
      mydata$FVL_stats_gs$`Upgraded Rate of climb (ft/min)`[vlname] <-
        input$rate_climbing
      mydata$FVL_stats_gs$`Upgraded Empty weight (lbs)`[vlname] <-
        input$empty_weight
      mydata$FVL_stats_gs$`Upgraded Max TOW (lbs)`[vlname] <-
        input$max_tow
      
    })
    
    # --------------------------------------------------------------------------------
    # upgrade
    
    observeEvent({
      input$slider
      input$upgrade_slider
      input$upgrade_rate
      input$upgrade_start_yr
    }, {
      actual_upgraded_2040 <- min(input$upgrade_slider, input$slider)
      
      if (input$upgrade_rate >= actual_upgraded_2040) {
        mydata$FVL_data_gs$`Upgrade Number`[vlname] <-
          c(
            rep(
              0,
              input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
            ),
            actual_upgraded_2040,
            rep(
              0,
              FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - min(FVL_data_gs$`Build Plan`$FY) -
                length(rep(
                  0,
                  input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
                ))
            )
          )
        
      } else if (input$upgrade_rate < actual_upgraded_2040) {
        max_new_upgrades <-
          cumsum(rep(
            input$upgrade_rate,
            FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$upgrade_start_yr + 1
          ))
        
        achieved <-
          min(which(max_new_upgrades >= actual_upgraded_2040))
        
        mydata$FVL_data_gs$`Upgrade Number`[[vlname]] <-
          c(
            rep(
              0,
              input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
            ),
            rep(
              input$upgrade_rate,
              FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$upgrade_start_yr + 1
            )[1:(achieved - 1)],
            min(
              input$upgrade_rate,
              actual_upgraded_2040 - max_new_upgrades[(achieved - 1)]
            ),
            rep(
              0,
              nrow(FVL_data_gs[[1]]) - (
                input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
              ) -
                achieved
            )
          )
      }
    })
    
    # --------------------------------------------------------------------------------
    
    Popover.template(ns("b"),
                     vlname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

# ================================================================================
# 2nd module type

# note: can build more for some existing helicopters,
# e.g. FireScout, Apache, Blackhawk

inproductionvlUI <- function(id, labelname) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelname),
      style = "basic",
      value = 0,
      type = "toggle",
      size = "extra-small",
      block = TRUE,
      disabled = FALSE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 0"),
      
      br(),
      
      fluidRow(
        column(1),
        
        column(
          10,
          sliderInput(
            inputId = ns("slider"),
            label = " ",
            value = as.numeric(FVL_data_gs$default[1, id]),
            min = 0,
            max = unlist(max_vl[id]),
            step = 1,
            width = '100%',
            ticks = FALSE
          )
        ),
        
        column(
          1,
          actionLink(
            inputId = ns("rate_yr_upgrade"),
            label = "",
            icon = icon("plus"),
            value = 0,
            type = "toggle"
          ),
          
          actionLink(
            inputId = ns("c"),
            label = strong(" "),
            icon = icon("bars")
          )
        )
        
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("rate_yr_upgrade"), "'] % 2 == 1"),
        
        fluidRow(column(1),
                 column(
                   10,
                   sliderInput(
                     inputId = ns("build_retire_rate"),
                     label = h6(em("Build rate")),
                     value = unlist(max_build_rate[id]),
                     min = 0,
                     max = 200,
                     step = 0.01,
                     width = '100%',
                     ticks = FALSE
                   )
                 ),
                 
                 column(1)),
        
        fluidRow(column(1),
                 column(
                   10,
                   sliderInput(
                     inputId = ns("build_start_yr"),
                     label = h6(em("Start year")),
                     value = unlist(build_start_yr[id]),
                     min = min(FVL_data_gs$`Build Plan`$FY),
                     max = max(FVL_data_gs$`Build Plan`$FY),
                     step = 1,
                     width = '100%',
                     ticks = FALSE,
                     sep = ""
                   )
                 ),
                 column(1))
      ),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_yr_upgrade"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("upgrade_slider"),
                     label = h6(em("Upgrade")),
                     value = 0,
                     min = 0,
                     max = retirement_base[[id]],
                     step = 1,
                     width = '100%',
                     ticks = FALSE
                   )
                 )
               ),
               column(1)),
      
      bsTooltip(
        ns("slider"),
        paste0("Set inventory level for ", max(FVL_data_gs$`Build Plan`$FY)),
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("rate_yr_upgrade"),
        "Expand slider selection",
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("c"),
        "Change the cost assumptions",
        "left",
        options = list(container = "body")
      )
      
    )
  )
}


inproductionvl <-
  function(input,
           output,
           session,
           mydata,
           vlname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------------
    # retirement zone
    
    observeEvent({
      input$slider
      input$build_start_yr
    }, {
      if (input$slider < retirement_base[[vlname]]) {
        updateSliderInput(
          session,
          inputId = "build_retire_rate",
          label = "Retirement rate",
          min = ceiling((retirement_base[[vlname]] - input$slider) /
                          (FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$build_start_yr + 1)
          ),
          max = max_vl[[vlname]],
          value = max(
            25,
            (retirement_base[[vlname]] - input$slider) /
              (FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$build_start_yr + 1)
          ),
          step = 1
        )
        
        updateSliderInput(session,
                          inputId = "upgrade_slider",
                          max = input$slider)
      }
    })
    
    # --------------------------------------------------------------------------------
    # build more zone
    
    observeEvent({
      input$slider
      input$build_start_yr
      input$build_retire_rate
    }, {
      if (input$slider >= retirement_base[[vlname]]) {
        # Update build_retire_rate slider
        updateSliderInput(
          session,
          inputId = "build_retire_rate",
          label = "Build rate",
          # value = max_build_rate[[vlname]],
          min = 0,
          max = 200,
          step = 0.01
        )
        
        # Update build_cut_retire slider
        temp <- input$slider
        
        updateSliderInput(
          session,
          inputId = "slider",
          # min = 0,
          max = floor(
            retirement_base[[vlname]] +
              (FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$build_start_yr + 1) * input$build_retire_rate
          ),
          value = min(
            temp,
            floor(
              retirement_base[[vlname]] +
                (FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$build_start_yr + 1) * input$build_retire_rate
            )
          )
        )
        
        updateSliderInput(session,
                          inputId = "upgrade_slider",
                          max = retirement_base[[vlname]])
        
      }
    })
    
    # --------------------------------------------------------------------------------
    
    Popover.template(ns("b"),
                     vlname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

update_vl_module <-
  function(input,
           output,
           session,
           mydata,
           vlname,
           updated_input) {
    ns <- session$ns
    observeEvent({
      input$build_retire_rate
      input$slider
      input$build_start_yr
    }, {
      mydata$vl[vlname] <- update_current_vl(vlname, input, mydata)
      
    })
    
    # --------------------------------------------------------------------------------
    
    observeEvent({
      input$upgrade_slider
      input$upgrade_start_yr
    },
    {
      updateSliderInput(session,
                        inputId = "upgrade_rate",
                        min = ceiling(
                          min(input$slider, input$upgrade_start_yr) /
                            (mydata$vl$FY[nrow(mydata$vl)] - input$upgrade_start_yr + 1)
                        ))
      
      if (input$upgrade_slider > 0) {
        updateSliderInput(
          session,
          inputId = "speed",
          value = as.numeric(FVL_stats_gs$`Upgraded Speed (mph)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "range",
          value = as.numeric(FVL_stats_gs$`Upgraded Range (mi)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "service_ceiling",
          value = as.numeric(FVL_stats_gs$`Upgraded Service ceiling (ft)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "rate_climbing",
          value = as.numeric(FVL_stats_gs$`Upgraded Rate of climb (ft/min)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "empty_weight",
          value = as.numeric(FVL_stats_gs$`Upgraded Empty weight (lbs)`[[vlname]])
        )
        updateSliderInput(
          session,
          inputId = "max_tow",
          value = as.numeric(FVL_stats_gs$`Upgraded Max TOW (lbs)`[[vlname]])
        )
      }
      
      mydata$FVL_stats_gs$`Upgraded Speed (mph)`[vlname] <-
        input$speed
      mydata$FVL_stats_gs$`Upgraded Range (mi)`[vlname] <-
        input$range
      mydata$FVL_stats_gs$`Upgraded Service ceiling (ft)`[vlname] <-
        input$service_ceiling
      mydata$FVL_stats_gs$`Upgraded Rate of climb (ft/min)`[vlname] <-
        input$rate_climbing
      mydata$FVL_stats_gs$`Upgraded Empty weight (lbs)`[vlname] <-
        input$empty_weight
      mydata$FVL_stats_gs$`Upgraded Max TOW (lbs)`[vlname] <-
        input$max_tow
      
    })
    
    # --------------------------------------------------------------------------------
    # upgrade
    
    observeEvent({
      input$slider
      input$upgrade_slider
      input$upgrade_rate
      input$upgrade_start_yr
    }, {
      actual_upgraded_2040 <- min(input$upgrade_slider, input$slider)
      
      if (input$upgrade_rate >= actual_upgraded_2040) {
        mydata$FVL_data_gs$`Upgrade Number`[vlname] <-
          c(
            rep(
              0,
              input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
            ),
            actual_upgraded_2040,
            rep(
              0,
              FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - min(FVL_data_gs$`Build Plan`$FY) -
                length(rep(
                  0,
                  input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
                ))
            )
          )
        
      } else if (input$upgrade_rate < actual_upgraded_2040) {
        max_new_upgrades <-
          cumsum(rep(
            input$upgrade_rate,
            FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$upgrade_start_yr + 1
          ))
        
        achieved <-
          min(which(max_new_upgrades >= actual_upgraded_2040))
        
        mydata$FVL_data_gs$`Upgrade Number`[[vlname]] <-
          c(
            rep(
              0,
              input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
            ),
            rep(
              input$upgrade_rate,
              FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - input$upgrade_start_yr + 1
            )[1:(achieved - 1)],
            min(
              input$upgrade_rate,
              actual_upgraded_2040 - max_new_upgrades[(achieved - 1)]
            ),
            rep(
              0,
              nrow(FVL_data_gs[[1]]) - (
                input$upgrade_start_yr - min(FVL_data_gs$`Build Plan`$FY)
              ) -
                achieved
            )
          )
      }
    })
  }


# ================================================================================
# 3rd module type: future vertical lift

# vertical lift id, i.e. "FW-W";
# label name of ship, i.e. "Build your own feather weight helicopter"
futurevlUI <- function(id,
                       labelName) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelName),
      style = "basic",
      value = 1,
      type = "toggle",
      size = "extra-small",
      block = TRUE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 1"),
      
      br(),
      
      fluidRow(
        column(1),
        
        column(
          10,
          sliderInput(
            inputId = ns("slider"),
            label = NA,
            value = unlist(default_vl[id]),
            min = 0,
            max = 3400,
            step = 1,
            width = '100%',
            ticks = FALSE
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] != 'Nothing'"),
          column(
            1,
            actionLink(
              inputId = ns("rate_rd"),
              label = "",
              icon = icon("plus"),
              value = 0,
              type = "toggle"
            ),
            actionLink(
              inputId = ns("c"),
              label = strong(" "),
              icon = icon("bars")
            )
          )
        )
      ),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_rd"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("build_rate"),
                     label = h6(em("Build rate")),
                     value = as.numeric(FVL_data_gs$default[2, id]),
                     min = 0,
                     max = 250,
                     step = 1,
                     width = '100%',
                     ticks = FALSE
                   )
                 )
               ),
               column(1)),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_rd"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("rd_start_num"),
                     label = h6(em("RDT&E Start Year")),
                     value = as.numeric(FVL_data_gs$default[5, id]),
                     min = min(FVL_data_gs$`Build Plan`$FY),
                     max = max(FVL_data_gs$`Build Plan`$FY),
                     step = 1,
                     width = '100%',
                     ticks = FALSE,
                     sep = ""
                   )
                 )
               ),
               
               column(1)),
      
      fluidRow(column(1),
               column(
                 10,
                 conditionalPanel(
                   condition = paste0("input['", ns("rate_rd"), "'] % 2 == 1"),
                   sliderInput(
                     inputId = ns("rd_duration"),
                     label = h6(em("RDT&E Duration")),
                     value = as.numeric(FVL_data_gs$default[6, id]),
                     min = 0,
                     max = 24,
                     step = 1,
                     width = '100%',
                     ticks = FALSE
                   )
                 )
               ),
               column(1)),
      
      bsTooltip(
        ns("slider"),
        paste0("Set inventory level for ", max(FVL_data_gs$`Build Plan`$FY)),
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("rate_rd"),
        "Expand slider selection",
        "right",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("c"),
        "Change the cost assumptions",
        "left",
        options = list(container = "body")
      )
    )
  )
}


futurevl <-
  function(input,
           output,
           session,
           current,
           vlname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$slider
    }, {
      current$FVL_data_gs %<>% apply_new_vl_rd(
        new_name = vlname,
        stats_df = current$FVL_stats_gs,
        rd_start = input$rd_start_num,
        rd_duration = input$rd_duration,
        inventory_num = input$slider
      )
      
      current$vl[vlname] <- update_future_vl(vlname, input, current)
    })
    
    
    observeEvent(ignoreInit = TRUE,
                 {
                   input$build_rate
                 }, {
                   temp = input$slider
                   
                   if (input$rd_duration > 0 & input$build_rate > 0) {
                     updateSliderInput(
                       session,
                       "slider",
                       value = min(temp, (
                         FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
                       ) * input$build_rate),
                       max = (
                         FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
                       ) * input$build_rate
                     )
                     
                     # current$vl[vlname] <- update_future_vl(vlname, input, current)
                   } else if (input$build_rate == 0) {
                     updateSliderInput(session,
                                       "slider",
                                       value = 0,
                                       max = 0)
                   }
                   current$vl[vlname] <-
                     update_future_vl(vlname, input, current)
                 })
    
    
    observeEvent(ignoreInit = TRUE, input$rd_start_num, {
      temp <- input$rd_duration
      
      updateSliderInput(
        session,
        "rd_duration",
        value = min(temp, current$FVL_data_gs[[1]]$FY[nrow(current$FVL_data_gs[[1]])] - input$rd_start_num),
        max = current$FVL_data_gs[[1]]$FY[nrow(current$FVL_data_gs[[1]])] - input$rd_start_num
      )
      
      updateSliderInput(
        session,
        "slider",
        value = min(input$slider, (
          FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
        ) * input$build_rate),
        max = (
          FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
        ) * input$build_rate
      )
      
      
      current$FVL_data_gs %<>% apply_new_vl_rd(
        new_name = vlname,
        stats_df = current$FVL_stats_gs,
        rd_start = input$rd_start_num,
        rd_duration = input$rd_duration,
        inventory_num = input$slider
      )
      
      current$vl[vlname] <- update_future_vl(vlname, input, current)
    })
    
    # --------------------------------------------------------------------------------
    # update after observing the duration of RD changes
    
    observeEvent(ignoreInit = TRUE, input$rd_duration, {
      temp <- input$slider
      rd_duration_temp <- input$rd_duration
      
      updateSliderInput(
        session,
        inputId = "rd_start_num",
        max = current$FVL_data_gs[[1]]$FY[nrow(current$FVL_data_gs[[1]])] - rd_duration_temp
      )
      
      if (input$rd_duration > 0 & input$build_rate) {
        updateSliderInput(
          session,
          inputId = "slider",
          min = 0,
          max = (
            FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
          ) * input$build_rate,
          value = min(temp,
                      (
                        FVL_data_gs[[1]]$FY[nrow(FVL_data_gs[[1]])] - (input$rd_start_num + input$rd_duration - 1)
                      ) * input$build_rate)
        )
        
      } else if (input$rd_duration == 0) {
        updateSliderInput(session,
                          inputId = "slider",
                          max = 0,
                          value = 0)
      }
      
      current$FVL_data_gs %<>% apply_new_vl_rd(
        new_name = vlname,
        stats_df = current$FVL_stats_gs,
        rd_start = input$rd_start_num,
        rd_duration = input$rd_duration,
        inventory_num = input$slider
      )
      
      temp <- input$slider
      current$vl[vlname] <- update_future_vl(vlname, input, current)
    })
    
    Popover.template(ns("b"),
                     vlname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

# ================================================================================
# 4th module type: change cost assumptions of current platforms (bsModal)

# id, i.e. Nimitz, Ford, ...
# label name of ship, i.e. "Nimitz Class", ...
OldbsModalUI <- function(id,
                         labelName) {
  ns <- NS(id)
  
  bsModal(ns("modalExample"),
          labelName,
          ns("c"),
          size = "small",
          
          tabsetPanel(
            tabPanel(
              "Cost",
              br(),
              
              sliderInput(
                ns("acquisition_cost"),
                "Per Unit Acquisition Cost ($)",
                value = ifelse(
                  is.na(FVL_stats_gs$`Acquisition Cost`[[id]]),
                  0,
                  as.numeric(FVL_stats_gs$`Acquisition Cost`[[id]])
                ),
                min = 0,
                max = 2 * ifelse(
                  is.na(FVL_stats_gs$`Acquisition Cost`[[id]]),
                  0,
                  as.numeric(FVL_stats_gs$`Acquisition Cost`[[id]])
                ),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("os_total"),
                "O&S Cost ($)",
                value = as.numeric(FVL_stats_gs$`Total O&S Cost`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Total O&S Cost`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("upgrade_rate"),
                "Upgrade Rate",
                value = 50,
                min = 0,
                max = 100,
                ticks = FALSE
              ),
              
              sliderInput(
                ns("upgrade_start_yr"),
                "Upgrade Start Year",
                value = 2024,
                min = min(FVL_data_gs$`Build Plan`$FY),
                max = max(FVL_data_gs$`Build Plan`$FY),
                ticks = FALSE,
                sep = " "
              ),
              
              sliderInput(
                ns("crew"),
                "Crew",
                value = as.numeric(FVL_stats_gs$Crew[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$Crew[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("passenger"),
                "Passengers",
                value = ifelse(
                  is.na(FVL_stats_gs$Passengers[[id]]),
                  0,
                  as.numeric(FVL_stats_gs$Passengers[[id]])
                ),
                min = 0,
                max = 2 * ifelse(
                  is.na(FVL_stats_gs$Passengers[[id]]),
                  0,
                  as.numeric(FVL_stats_gs$Passengers[[id]])
                ),
                ticks = FALSE
              )
            ),
            
            tabPanel(
              "Performance (for Upgrades)",
              br(),
              
              conditionalPanel(
                condition = paste0("input['", ns("upgrade_slider"), "']  != 0"),
                
                sliderInput(
                  ns("speed"),
                  "Speed (mph)",
                  value = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  max = 2 * as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("range"),
                  "Range (mi)",
                  value = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  max = 2 * as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("service_ceiling"),
                  "Service ceiling (ft)",
                  value = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  max = 2 * as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("rate_climbing"),
                  "Rate of climb (ft/min)",
                  value = ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  min = ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  max = 2 * ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("empty_weight"),
                  "Empty weight (lbs)",
                  value = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  max = 2 * as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("max_tow"),
                  "Max Takeoff Weight (lbs)",
                  value = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  max = 2 * as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  ticks = FALSE
                )
              ),
              
              conditionalPanel(
                condition = paste0("input['", ns("upgrade_slider"), "'] == 0"),
                
                sliderInput(
                  ns("speed"),
                  "Speed (mph)",
                  value = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  max = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("range"),
                  "Range (mi)",
                  value = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  max = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("service_ceiling"),
                  "Service ceiling (ft)",
                  value = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  max = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("rate_climbing"),
                  "Rate of climb (ft/min)",
                  value = ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  min = ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  max = ifelse(
                    is.na(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                    0,
                    as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]])
                  ),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("empty_weight"),
                  "Empty weight (lbs)",
                  value = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  max = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                  ticks = FALSE
                ),
                
                sliderInput(
                  ns("max_tow"),
                  "Max Takeoff Weight (lbs)",
                  value = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  min = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  max = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                  ticks = FALSE
                )
              )
            )
          ))
}

OldbsModal <- function(input,
                       output,
                       session,
                       current,
                       vlname,
                       ...) {
  ns <- session$ns
  
  observeEvent({
    input$acquisition_cost
    input$os_total
    # input$upgrade_rate
    # input$upgrade_start_year
    input$crew
    input$passenger
    
    input$speed
    input$range
    input$service_ceiling
    input$rate_climbing
    input$empty_weight
    input$max_tow
  }, {
    if (input$c) {
      # update acquisition cost
      current$FVL_stats_gs$`Acquisition Cost`[vlname] <-
        input$acquisition_cost
      current$FVL_data_gs$`Acquisition Yearly`[vlname] <-
        input$acquisition_cost
      
      # update all O&S cost-related stats
      current$FVL_stats_gs$`Total O&S Cost`[vlname] <-
        input$os_total
      current$FVL_data_gs$`O&S Yearly`[vlname] <- input$os_total
      
      # update all personnel-related stats
      current$FVL_stats_gs$Passengers[vlname] <- input$passenger
      current$FVL_stats_gs$Crew[vlname] <- input$crew
      
      # allow user to upgrade performance metrics
      current$FVL_stats_gs$`Upgraded Speed (mph)`[vlname] <-
        input$speed
      current$FVL_stats_gs$`Upgraded Range (mi)`[vlname] <-
        input$range
      current$FVL_stats_gs$`Upgraded Service ceiling (ft)`[vlname] <-
        input$service_ceiling
      current$FVL_stats_gs$`Upgraded Rate of climb (ft/min)`[vlname] <-
        input$rate_climbing
      current$FVL_stats_gs$`Upgraded Empty weight (lbs)`[vlname] <-
        input$empty_weight
      current$FVL_stats_gs$`Upgraded Max TOW (lbs)`[vlname] <-
        input$max_tow
    }
  })
}

# ================================================================================
# 5th module type: change cost assumptions of future platforms (bsModal)

newbsModalUI <- function(id, labelname) {
  ns <- NS(id)
  
  bsModal(ns("modalExample"),
          labelname,
          ns("c"),
          size = "small",
          
          tabsetPanel(
            tabPanel(
              "Cost",
              br(),
              
              sliderInput(
                ns("acquisition_cost"),
                "Per Unit Acquisition Cost ($)",
                value = as.numeric(FVL_stats_gs$`Acquisition Cost`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Acquisition Cost`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("rd"),
                "R&D Cost",
                value = as.numeric(FVL_stats_gs$`R&D Cost`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`R&D Cost`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("os_total"),
                "O&S Cost ($)",
                value = as.numeric(FVL_stats_gs$`Total O&S Cost`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Total O&S Cost`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("crew"),
                "Crew",
                value = as.numeric(FVL_stats_gs$Crew[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$Crew[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("passenger"),
                "Passengers",
                value = as.numeric(FVL_stats_gs$Passengers[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$Passengers[[id]]),
                ticks = FALSE
              )
            ),
            
            tabPanel(
              "Performance",
              br(),
              
              sliderInput(
                ns("speed"),
                "Speed (mph)",
                value = as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Speed (mph)`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("range"),
                "Range (mi)",
                value = as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Range (mi)`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("service_ceiling"),
                "Service ceiling (ft)",
                value = as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Service ceiling (ft)`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("rate_climbing"),
                "Rate of climb (ft/min)",
                value = as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("empty_weight"),
                "Empty weight (lbs)",
                value = as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Empty weight (lbs)`[[id]]),
                ticks = FALSE
              ),
              
              sliderInput(
                ns("max_tow"),
                "Max Takeoff Weight (lbs)",
                value = as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                min = 0,
                max = 2 * as.numeric(FVL_stats_gs$`Max TOW (lbs)`[[id]]),
                ticks = FALSE
              )
            )
          ))
}


newbsModal <- function(input,
                       output,
                       session,
                       current,
                       vlname,
                       ...) {
  ns <- session$ns
  
  observeEvent({
    input$acquisition_cost
    input$rd
    input$os_total
    input$crew
    input$passenger
    
    input$speed
    input$range
    input$service_ceiling
    input$rate_climbing
    input$empty_weight
    input$max_tow
    
  }, {
    if (input$c) {
      # update cost assumptions
      current$FVL_stats_gs$`Acquisition Cost`[vlname] <-
        input$acquisition_cost
      current$FVL_stats_gs$`R&D Cost`[vlname] <- input$rd
      current$FVL_stats_gs$`Total O&S Cost`[vlname] <-
        input$os_total
      
      # update personnel assumptions
      current$FVL_stats_gs$Crew[vlname] <- input$crew
      current$FVL_stats_gs$Passengers[vlname] <- input$passenger
      
      # update performance assumptions
      # note: - user can adjust a platform's performance specs
      # ... but this is not considered as an upgrade
      # ... we don't record this part in "Upgraded #" in FVL_stats_gs
      current$FVL_stats_gs$`Speed (mph)`[vlname] <- input$speed
      current$FVL_stats_gs$`Range (mi)`[vlname] <- input$range
      current$FVL_stats_gs$`Service ceiling (ft)`[vlname] <-
        input$service_ceiling
      current$FVL_stats_gs$`Rate of climb (ft/min)`[vlname] <-
        input$rate_climbing
      current$FVL_stats_gs$`Empty weight (lbs)`[vlname] <-
        input$empty_weight
      current$FVL_stats_gs$`Max TOW (lbs)`[vlname] <- input$max_tow
    }
  })
}

# ================================================================================
