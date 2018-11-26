# ================================================================================
# Build Your Own: Vertical Lift
# Designed and built by Gabriel Coll and Yuanjing Han
# --------------------------------------------------------------------------------
# app functions 
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(tidyverse)
library(forcats)
library(extrafont)
library(extrafontdb)

# ================================================================================
# update_current_vl: update platform count 

# note: returns the updated vector of vertical lift count, 
# ...given the user's choice of a new level for 2046
# ...vl_name: vertical lift name 
# ...input: shiny input opject 
# ...reactive values which store all of the most updated dataframes 

update_current_vl <- function(
  
  vl_name,    
  input,      
  current,    
  session = getDefaultReactiveDomain()
  ){
    

    input_name <- gsub(" ", "_", vl_name)
    
    slider_max_prepared <- retirement_base[[vl_name]]+(max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr+1)*input$build_retire_rate
    
    if(class(input) == "numeric" | class(input) == "integer"){
      new_number <- input
      
      # --------------------------------------------------------------------------------
      # note: the following comparison is to avoid crashing when building at the current
      # ...build rate and delaying the build start year, the actual numeber of helos 
      # ...we will have by 2040 is smaller than the build/retire slider value
      
    } else new_number <- min(input$slider,ifelse(slider_max_prepared - floor(slider_max_prepared) >= 0.99,
                                                 ceiling(slider_max_prepared),floor(slider_max_prepared)))
    
    orig_number <- current$FVL_data_gs[["Baseline"]][
      nrow(current$FVL_data_gs[["Baseline"]]),vl_name]
    
    # the total number of helos we will have finished building by FY 2040
    actual_new_builds_2040 <- new_number - retirement_base[[vl_name]]
    
    
    if(!is.null(new_number)){
      
      if (new_number >= orig_number){
        
        if(actual_new_builds_2040 <= input$build_retire_rate){
            
            new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                       rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                           22 - length(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)))))
            
            current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                                           actual_new_builds_2040,
                                                           rep(0, 22 - length(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - 1))
                
        } else if(actual_new_builds_2040 > input$build_retire_rate){
            
            max_new_builds <- cumsum(rep(input$build_retire_rate, max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr + 1))
            
            yearly_builds <- ifelse(max_new_builds - floor(max_new_builds) >= 0.99,
                                    ceiling(max_new_builds), floor(max_new_builds))
            
            achieved <- min(which(yearly_builds >= actual_new_builds_2040))
            
            new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                       retirement_base[[vl_name]] + yearly_builds[1:(achieved - 1)],
                                       rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                           22 - length(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - 
                                           length(yearly_builds[1:(achieved - 1)])))
            
            current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                                           rep(input$build_retire_rate, max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr + 1)[1:(achieved - 1)],
                                                           min(input$build_retire_rate, actual_new_builds_2040 - yearly_builds[(achieved-1)]),
                                                           rep(0, 22 - length(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - achieved))
        }
        
        return(new_yearly_quantities)  
        
        # --------------------------------------------------------------------------------
        # cut 
        
      } else if(new_number < orig_number & new_number >= retirement_base[vl_name]){
          
          if(actual_new_builds_2040 <= input$build_retire_rate){
              
              new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                         rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                             22 - length(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)))))
              
              current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                                             actual_new_builds_2040,
                                                             rep(0, 22 - length(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - 1))
              
          } else if(actual_new_builds_2040 > input$build_retire_rate){
              
              max_new_builds <- cumsum(rep(input$build_retire_rate, max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr + 1))
              
              yearly_builds <- ifelse(max_new_builds - floor(max_new_builds) >= 0.99,
                                      ceiling(max_new_builds), floor(max_new_builds))
              
              achieved <- min(which(yearly_builds >= actual_new_builds_2040))
              
              new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                         retirement_base[[vl_name]] + yearly_builds[1:(achieved - 1)],
                                         rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                             22 - length(rep(retirement_base[[vl_name]], input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - 
                                             length(yearly_builds[1:(achieved - 1)])))
              
              current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                                             rep(input$build_retire_rate, max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr + 1)[1:(achieved - 1)],
                                                             actual_new_builds_2040 - yearly_builds[(achieved-1)],
                                                             rep(0, 22 - length(rep(0,input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY))) - achieved))
          }
          
          return(new_yearly_quantities)  
        
        # --------------------------------------------------------------------------------
        # retire 
        
      } else if(new_number < orig_number & new_number < retirement_base[[vl_name]]){
        

        retire_num <- as.numeric(current$FVL_stats_gs$to_retire[[vl_name]]) - new_number
        updated_slider_min <- ceiling((retirement_base[[vl_name]] - input$slider)/(max(FVL_data_gs$`Build Plan`$FY) - input$build_start_yr + 1))
        
        # --------------------------------------------------------------------------------
        # note: the following if-else will prevent the app from crashing when the current
        # ...build_retire_rate slider value is lower than the updateSliderInput slider min
        
        if(input$build_retire_rate < updated_slider_min){
            
            new_yearly_quantities <- c(rep(current$FVL_stats_gs$to_retire[[vl_name]],input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                       seq(current$FVL_stats_gs$to_retire[[vl_name]],new_number,-1 * min(retire_num,updated_slider_min))[-1],
                                       rep(new_number,22 - (input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)) - 
                                               length(seq(current$FVL_stats_gs$to_retire[[vl_name]],new_number,-1 * min(retire_num,updated_slider_min))[-1])))
              
        } else if(input$build_retire_rate >= updated_slider_min){
            
            new_yearly_quantities <- c(rep(current$FVL_stats_gs$to_retire[[vl_name]],input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)),
                                       seq(current$FVL_stats_gs$to_retire[[vl_name]],new_number,-1 * min(retire_num,input$build_retire_rate))[-1],
                                       rep(new_number,22 - (input$build_start_yr - min(FVL_data_gs$`Build Plan`$FY)) - 
                                               length(seq(current$FVL_stats_gs$to_retire[[vl_name]],new_number,-1 * min(retire_num,input$build_retire_rate))[-1])))
        }
        
        current$FVL_data_gs$`Actual Builds`[vl_name] <- rep(0,22)
        return(new_yearly_quantities)  
      }
    }
  } 


# ================================================================================
# update_future_vl: update platform count

# note: returns the updated vector of vertical lift counts, 
# ...given the user's choice of a new level for 2040
# ...vl_name: vertical lift name 
# ...input: shiny input object 
# ...current: reactive values which store all of the most updated dataframes 

update_future_vl <- function(
  
  
    vl_name,   
    input,      
    current,    
    session = getDefaultReactiveDomain()){
  
    temp_rd_duration <- min(input$rd_duration, current$FVL_data_gs[[1]]$FY[nrow(current$FVL_data_gs[[1]])] - input$rd_start_num)
        
    input_name <- gsub(" ", "_", vl_name)
    if(class(input) == "numeric" | class(input) == "integer"){
      new_number <- input
    } else new_number <- min(input$slider, (max(FVL_data_gs$`Build Plan`$FY) - (input$rd_start_num + temp_rd_duration - 1)) * input$build_rate)
  

    orig_number <- current$FVL_data_gs[["Baseline"]][
      nrow(current$FVL_data_gs[["Baseline"]]),
      vl_name]
    

    if(!is.null(new_number)){
      if(new_number == orig_number){

        current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0, input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)),
                                                       new_number - orig_number,
                                                       rep(0, 22 - length(rep(0, input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY))) - 1))
        return(current$FVL_data_gs[["Fleet Plan"]][,vl_name])
      }

      if(new_number > orig_number){
        actual_new_builds_2040 <- new_number - orig_number

        if(actual_new_builds_2040 <= input$build_rate){
          
          new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)),
                                     rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                         22 - length(rep(retirement_base[[vl_name]], input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)))))
          
          current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0, input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)),
                                                         actual_new_builds_2040,
                                                         rep(0, 22 - length(rep(0, input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY))) - 1))
          
        } else if(actual_new_builds_2040 > input$build_rate){
          
          
          max_new_builds <- cumsum(rep(input$build_rate, max(FVL_data_gs$`Build Plan`$FY) - (input$rd_start_num + temp_rd_duration - 2)))
          
          yearly_builds <- ifelse(max_new_builds - floor(max_new_builds) >= 0.99,
                                  ceiling(max_new_builds), floor(max_new_builds))
          
          achieved <- min(which(yearly_builds >= actual_new_builds_2040))
          
          new_yearly_quantities <- c(rep(retirement_base[[vl_name]], input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)),
                                     retirement_base[[vl_name]] + yearly_builds[1:(achieved - 1)],
                                     rep(retirement_base[[vl_name]] + actual_new_builds_2040, 
                                         22 - length(rep(retirement_base[[vl_name]], input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY))) - 
                                           (achieved - 1)))
          
          current$FVL_data_gs$`Actual Builds`[vl_name] <- c(rep(0,input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)),
                                                         rep(input$build_rate, max(FVL_data_gs$`Build Plan`$FY) - (input$rd_start_num + temp_rd_duration - 1) + 1)[1:(achieved - 1)],
                                                         actual_new_builds_2040 - yearly_builds[(achieved-1)],
                                                         rep(0, 22 - (input$rd_start_num + temp_rd_duration - min(FVL_data_gs$`Build Plan`$FY)) - 
                                                             achieved))
        }
        return(new_yearly_quantities)
      }
      
}}

# ================================================================================
# add_diigtheme: chart theme  

# note: passed_plot, a ggplot object to add the theme to 

add_diigtheme <- function(
  passed_plot  
){
  themed_plot <- passed_plot +
    theme(
      plot.title = element_text(
        family = "Open Sans",
        color = "#554449",
        size = 18,
        face="bold",
        hjust = .5), 
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#FCFCFC"),
      plot.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      legend.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      panel.grid.major.x = element_line(size=.1, color="grey80"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="grey80"),
      panel.grid.minor.y = element_line(size=.1, color="grey80"),
      legend.text = element_text(
        size = 12,
        family = "Open Sans",
        color="#554449"), 
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(fill="#FCFCFC", color="#FCFCFC"),
      legend.key.width = unit(2,"line"),
      
      axis.text.x = element_text(
        size = 12,
        color="#554449",
        family = "Open Sans",
        margin=margin(0,0,0,0),
        angle = 0),
      
      axis.ticks.length = unit(.00, "cm"),
      
      axis.text.y = element_text(
        size = 12,
        color="#554449",
        family = "Open Sans",
        margin=margin(0,5,0,0)),
      
      axis.title.x = element_text(
        size = 12,
        face = "bold",
        color="#554449",
        family = "Open Sans",
        margin=margin(15,0,0,0)),
      
      axis.title.y = element_text(
        size = 12,
        face = "bold",
        color="#554449",
        family = "Open Sans",
        margin=margin(0,15,0,0)),
      
      legend.direction = "horizontal") 
  
  return(themed_plot)
}

# ================================================================================
# get_vl_data: platform data and subsetting 

# note: current_vl, current vertical lift dataframe 
# ...input: shiny input object

get_vl_data <- function(
  current_vl,   
  FVL_data,   
  stats_df,    
  input,    
  session = getDefaultReactiveDomain()
){
  
  baseline <- FVL_data[["Baseline"]]
  new_plan <- as.data.frame(lapply(current_vl,function(x) as.numeric(x)))
  
  # note: subset by category type first when the sidebarPanel is clicked
  
  if(input$UltraLightweight == 1){
    baseline <- baseline[,c("FY",names(baseline)[-1][which(stats_df$UltraLightweight == 1)])]
    new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$UltraLightweight == 1)])]
    
  } else if(input$Lightweight == 1){
    baseline <- baseline[,c("FY",names(baseline)[-1][which(stats_df$Lightweight == 1)])]
    new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Lightweight == 1)])]
    
  } else if(input$Middleweight == 1){
    baseline <- baseline[,c("FY",names(baseline)[-1][which(stats_df$Middleweight == 1)])]
    new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Middleweight == 1)])]
    
  } else if(input$Heavyweight == 1){
    baseline <- baseline[,c("FY",names(baseline)[-1][which(stats_df$Heavyweight == 1)])]
    new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Heavyweight == 1)])]
    
  } else if(input$UltraHeavyweight == 1){
    baseline <- baseline[,c("FY",names(baseline)[-1][which(stats_df$UltraHeavyweight == 1)])]
    new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$UltraHeavyweight == 1)])]
  }
  
    if(input$top_y != "# of Vertical Lift" & !(input$top_y %in% c(
      "Speed (mph)",
      "Range (mi)",
      "Service ceiling (ft)",
      "Rate of climb (ft/min)",
      "Empty weight (lbs)",
      "Max TOW (lbs)" 
    ))){
      
      for(i in 1:(ncol(baseline)-1)){
        baseline[,i+1] <- round(baseline[,i+1] * as.numeric(stats_df[[input$top_y]][,(names(baseline)[-1])[i]]))
        new_plan[,i+1] <- round(new_plan[,i+1] * as.numeric(stats_df[[input$top_y]][,(names(baseline)[-1])[i]]))
      }
      
      baseline$Total <- rowSums(baseline[,-1,drop=FALSE])
      baseline$Category <- "old plan vertical lifts"
      new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE])
      new_plan$Category <- "new plan vertical lifts"
    } else if(input$top_y %in% c(
      
      "Speed (mph)",
      "Range (mi)",
      "Service ceiling (ft)",
      "Rate of climb (ft/min)",
      "Empty weight (lbs)",
      "Max TOW (lbs)" 
      )){
  
     
      for(i in 1:(ncol(baseline)-1)){
        baseline[,i+1] <- round(baseline[,i+1] * as.numeric(stats_df[[input$top_y]][,(names(baseline)[-1])[i]])) 
        new_plan[,i+1] <- round((new_plan[,i+1] - cumsum(FVL_data$`Upgrade Number`[,(names(baseline)[-1])[i]])) * as.numeric(stats_df[[input$top_y]][,(names(baseline)[-1])[i]]) + 
                                 cumsum(FVL_data$`Upgrade Number`[,(names(baseline)[-1])[i]]) * as.numeric(stats_df[[paste0("Upgraded"," ",input$top_y)]][,(names(baseline)[-1])[i]]))
      }
      
      
      # note: find the column name where the performance value equals to zero 
      # ...and when calculating total number of platforms, exclude them in sum
      
      col <- names(stats_df[[input$top_y]])[which(stats_df[[input$top_y]][1,] == 0)]
      
      if(length(col) != 0){
        baseline$Total <- rowSums(baseline[,-1]) /
          rowSums(FVL_data[["Baseline"]][names(FVL_data[["Baseline"]]) %in% 
                                              names(baseline)[-1] & !names(FVL_data[["Baseline"]]) %in% col,drop=FALSE])
        baseline$Category <- "old plan vertical lifts"
        
        new_plan$Total <- rowSums(new_plan[,-1]) / 
          rowSums(current_vl[names(current_vl) %in% 
                                  names(baseline)[-1] & !names(current_vl) %in% col,drop=FALSE])
        new_plan$Category <- "new plan vertical lifts"
        
      } else{
        baseline$Total <- rowSums(baseline[,-1,drop=FALSE]) / 
          rowSums(FVL_data[["Baseline"]][names(FVL_data[["Baseline"]]) %in% 
                                              names(baseline)[-1],drop=FALSE])
        baseline$Category <- "old plan vertical lifts"
        new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE]) / 
          rowSums(current_vl[names(current_vl) %in%  names(new_plan)[-1],drop=FALSE])
        new_plan$Category <- "new plan vertical lifts"
      }
      
    } else{
        baseline$Total <- rowSums(baseline[,-1,drop=FALSE])
        baseline$Category <- "old plan vertical lifts"
        new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE])
        new_plan$Category <- "new plan vertical lifts"}
  
  return(bind_rows(baseline, new_plan))
}

# ================================================================================
# performance_change

# note: the function below will calculate the percent change in performance specs
# ...and record them in `current`
# ...current_vl: current vertical lift dataframe 
# ...input: shiny input object 

performance_change <- function(
  current_vl, 
  FVL_data,   
  stats_df,    
  input,       
  session = getDefaultReactiveDomain()){
    
    
    performance_specs <- c("Speed (mph)","Range (mi)","Service ceiling (ft)","Rate of climb (ft/min)","Empty weight (lbs)","Max TOW (lbs)","Passengers")

    
    for(k in 1:length(performance_specs)){
      
      new_plan <- as.data.frame(lapply(current_vl,function(x) as.numeric(x)))
      
      if(input$UltraLightweight == 1){
        new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$UltraLightweight == 1)])]

      } else if(input$Lightweight == 1){
        new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Lightweight == 1)])]

      } else if(input$Middleweight == 1){
        new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Middleweight == 1)])]

      } else if(input$Heavyweight == 1){
        new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$Heavyweight == 1)])]

      } else if(input$UltraHeavyweight == 1){
        new_plan <- new_plan[,c("FY",names(new_plan)[-1][which(stats_df$UltraHeavyweight == 1)])]
      }
      
      if(performance_specs[k] == "Passengers"){
        
        # note: generate the percentage change for passengers
        
        for(i in 1:(ncol(new_plan)-1)){
          new_plan[,i+1] <- round(new_plan[,i+1] * as.numeric(stats_df[[performance_specs[k]]][,(names(new_plan)[-1])[i]]))
        }
        
        new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE])
        new_plan$Category <- "new plan vertical lifts"
        
 
      } else{
        
        for(i in 1:(ncol(new_plan)-1)){
          
          new_plan[,i+1] <- round((new_plan[,i+1] - cumsum(FVL_data$`Upgrade Number`[,(names(new_plan)[-1])[i]])) * as.numeric(stats_df[[performance_specs[k]]][,(names(new_plan)[-1])[i]]) + 
                                    cumsum(FVL_data$`Upgrade Number`[,(names(new_plan)[-1])[i]]) * as.numeric(stats_df[[paste0("Upgraded"," ",performance_specs[k])]][,(names(new_plan)[-1])[i]]))
        }
        
        # note: find the column name where the performance value equals to zero and when calculating
        # ...total number of helicopters, exclude them in sum
        
        col <- names(stats_df[[performance_specs[k]]])[which(stats_df[[performance_specs[k]]][1,] == 0)]
        
        if(length(col) != 0){
          
          new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE]) / 
            rowSums(current_vl[names(current_vl) %in% 
                                 names(new_plan)[-1] & !names(current_vl) %in% col,drop=FALSE])
          new_plan$Category <- "new plan vertical lifts"
          
        } else{
          
          new_plan$Total <- rowSums(new_plan[,-1,drop=FALSE]) / 
            rowSums(current_vl[names(current_vl) %in%  names(new_plan)[-1],drop=FALSE])
          
          new_plan$Category <- "new plan vertical lifts"
        }
      }
      
      stats_df[[paste0(performance_specs[k],"_change")]] <- percent((new_plan$Total[new_plan$FY == max(FVL_data_gs$`Build Plan`$FY)] - new_plan$Total[new_plan$FY == min(FVL_data_gs$`Build Plan`$FY)]) / 
                                                                      new_plan$Total[new_plan$FY == min(FVL_data_gs$`Build Plan`$FY)])
      
    }
    
    return(c(stats_df[["Speed (mph)_change"]],stats_df[["Range (mi)_change"]],stats_df[["Service ceiling (ft)_change"]],
             stats_df[["Rate of climb (ft/min)_change"]],stats_df[["Empty weight (lbs)_change"]],stats_df[["Max TOW (lbs)_change"]],
             stats_df[["Passengers_change"]]))
    
}

# ================================================================================
# apply_new_vl_rd 

apply_new_vl_rd <- function(
    FVL_data_gs,
    new_name,
    stats_df,
    rd_start,
    rd_duration,
    inventory_num
){
  
    col <- which(names(FVL_data_gs[[1]]) == new_name)
    stats_col <- which(names(stats_df[[1]]) == new_name)
    
    # calculate R&D cost 
    if(inventory_num > 0){
      FVL_data_gs[["R&D Cost"]][col] <- c(
            rep(0, rd_start - FVL_data_gs[[1]]$FY[1]),
            rep(
                as.numeric(unlist(stats_df[["R&D Cost"]][stats_col])) / rd_duration,
                min(max(FVL_data_gs$`Build Plan`$FY) - rd_start + 1, rd_duration)),
            rep(0, nrow(FVL_data_gs$`Build Plan`) - length(rep(0, rd_start - FVL_data_gs[[1]]$FY[1])) - 
                    length(rep(
                        as.numeric(unlist(stats_df[["R&D Cost"]][stats_col])) / rd_duration,
                        min(max(FVL_data_gs$`Build Plan`$FY) - rd_start + 1, rd_duration))))) 
        
    } else if(inventory_num == 0){
      FVL_data_gs[["R&D Cost"]][col] <- rep(0,nrow(FVL_data_gs[["R&D Cost"]]))
    }
    return(FVL_data_gs)
}

# ================================================================================
# get_budget_change_data

get_budget_change_data <- function(
  
    # returns a tibble in long form for the budget plot
    current,
    input,
    session = getDefaultReactiveDomain()
    
){
    
  # --------------------------------------------------------------------------------
  # O&S variation
    
    # fleet plan 2019 is the baseline
    vl_baseline <- current$FVL_data_gs[["Baseline"]]
    vl_current <- current$vl

    # add O&S 
    os_change <- vl_current
    os_change$FY <- os_change$FY + 1  # start count o&s in the year after ship built/upgrade
    os_change <- bind_rows(filter(vl_current, FY == min(FY)),
                           filter(os_change, FY != max(FY)))
    
    os_original <- vl_baseline
    os_original$FY <- os_original$FY + 1
    os_original <- bind_rows(filter(vl_baseline, FY == min(FY)),
                             filter(os_original, FY != max(FY)))
    
    os_change[-1] <- (os_change[-1] - current$FVL_data_gs$`Upgrade Number`[-1]) * current$FVL_data_gs[["O&S Yearly"]][,-1] + current$FVL_data_gs$`Upgraded Yearly O&S`[-1] - 
                      os_original[-1] * current$FVL_data_gs[["O&S Yearly"]][-1] 
    
    added_os <- os_change
    added_os[-1][added_os[-1] < 0] <- 0
    added_os$Total <- rowSums(added_os[-1])
    added_os$budget_cat <- "Added O&S"
    
    # cut O&S
    cut_os <- os_change
    
    cut_os[-1][cut_os[-1] > 0] <- 0
    cut_os$Total <- rowSums(cut_os[-1])
    cut_os$budget_cat <- "Cut O&S"
    
    # --------------------------------------------------------------------------------
    # acq variation
    
    acq_variation <- vl_current
    
    acq_variation[-1] <- current$FVL_data_gs$`Actual Builds`[-1] * FVL_data[["Acquisition Yearly"]][-1] - 
                         current$FVL_data_gs$`Build Plan`[-1] * FVL_data[["Acquisition Yearly"]][-1]
    
    # add acq
    added_acq <- acq_variation
    added_acq[-1][added_acq[-1] < 0] <- 0
    added_acq$Total <- rowSums(added_acq[-1])
    added_acq$budget_cat <- "Added Acq"
    
    # cut acq
    cut_acq <- acq_variation
    cut_acq[-1][cut_acq[-1] > 0] <- 0
    cut_acq$Total <- rowSums(cut_acq[-1])
    cut_acq$budget_cat <- "Cut Acq"
    
    # -------------------------------------------------------------------------------- 
    # R&D variation
    
    rd_change <- vl_current
    
    rd_change[-1] <- current$FVL_data_gs[["R&D Cost"]][-1]
    
    # add rd
    added_rd <- rd_change
    added_rd[added_rd < 0] <- 0
    added_rd$Total <- rowSums(added_rd[-1])
    added_rd$budget_cat <- "Added R&D"
    
    # cut rd
    cut_rd <- rd_change
    cut_rd[-1][cut_rd[-1] > 0] <- 0
    cut_rd$Total <- rowSums(cut_rd[-1])
    cut_rd$budget_cat <- "Cut R&D"

        
    return(bind_rows(added_acq, added_os, added_rd, cut_rd, cut_acq, cut_os))
}

# ================================================================================
# get_budget_total_data

get_budget_total_data <- function(
  
    # note: returns a tibble in long form for the budget plot
    # ...current_vl: tibble of current platforms 
    # ...FVL_data: list of data frames 
  
    current,
    current_vl,    
    FVL_data,     
    input,
    session = getDefaultReactiveDomain()
    
){
    
    # fleet plan 2019 is the baseline
    vl_total <- as.data.frame(lapply(current_vl,function(x) as.numeric(x)))

    # --------------------------------------------------------------------------------
    # O&S (the original O&S and upgrade O&S)
    
    os <- vl_total
    
    os$FY <- os$FY + 1  # start count o&s in the year after the ship built
    os <- bind_rows(
          filter(vl_total, FY == min(FY)),
          filter(os, FY != max(FY)))
    
    os[-1] <- os[-1] * FVL_data[["O&S Yearly"]][,-1] 
    
    os$Total <- rowSums(os[-1])
    os$budget_cat <- "O&S"

    # --------------------------------------------------------------------------------
    # acquisition
   
    acq <- vl_total
    acq[-1] <- FVL_data$`Actual Builds`[-1] * FVL_data[["Acquisition Yearly"]][-1]

    acq$Total <- rowSums(acq[-1])
    acq$budget_cat <- "Acquisition"
    
    # --------------------------------------------------------------------------------
    # total R&D
    
    rd_current <- vl_total
    rd_current[rd_current != 0] <- 1
    
    rd_future <- current$FVL_data_gs[["R&D Cost"]]
    rd_future[,19:23][rd_future[,19:23] > 0] <- 1
    rd_future[,1:18][rd_future[,1:18] != 0] <- 0
    
    rd_full <- rd_current + rd_future
    rd <- rd_full * current$FVL_data_gs[["R&D Cost"]]
    rd$Total <- rowSums(rd[-1])
    rd$Total <- rowSums(rd[-1]) + c(FVL_stats_gs$preloaded_rd %>% unlist(use.names = FALSE))
    rd$budget_cat <- "R&D"

    # --------------------------------------------------------------------------------
    # total upgrade cost 

    upgrade <- FVL_data$`Upgrade Number`
    upgrade[-1] <- upgrade[-1] * FVL_data$`Upgrade Cost`[-1]
    upgrade$Total <- rowSums(upgrade[-1])
    upgrade$budget_cat <- "Upgrade"
    
    return(bind_rows(acq, os, rd, upgrade))
}  

# ================================================================================
# deflate_frame

# note: frame, data frame to be deflated

deflate_frame <- function(
  frame   
  
){
  
  deflate <- 
    c("2017" = 1.02040053248136,
      "2018" = 1.04233226837061,
      "2019" = 1.065406461,
      "2020" = 1.089368122,
      "2021" = 1.113862265,
      "2022" = 1.138907152,
      "2023" = 1.164515166,
      "2024" = 1.190698969,
      "2025" = 1.217471508,
      "2026" = 1.24484602,
      "2027" = 1.272836041,
      "2028" = 1.301455409,
      "2029" = 1.330718276,
      "2030" = 1.360639111,
      "2031" = 1.391232707,
      "2032" = 1.422514192,
      "2033" = 1.454499032,
      "2034" = 1.487203043,
      "2035" = 1.520642395,
      "2036" = 1.554833621,
      "2037" = 1.589793627,
      "2038" = 1.6255397,
      "2039" = 1.662089513, 
      "2040" = 1.699461139,
      "2041" = 1.737673055,
      "2042" = 1.776744156,
      "2043" = 1.81669376,
      "2044" = 1.85754162,
      "2045" = 1.899307932,
      "2046" = 1.942013349
      # "2047" = ...,
      # "2048" = ...
    )

  deflated_frame <- as_tibble(
    sapply(names(frame), function(var_name) {
      if(var_name == "FY" | var_name == "Category" | var_name == "budget_cat"){
        return(frame[[var_name]])}
      return(round(frame[[var_name]] / deflate[as.character(frame[["FY"]])]))
    })
    ,
    validate = FALSE
  )
  deflated_frame[,
    which(!(names(deflated_frame) %in% c("Category", "budget_cat")))
    ] %<>% sapply(as.numeric)
  
  return(deflated_frame)
}

# ================================================================================
# format_ylab

# note: formats the y-axis labels for the top chart 
# ...x: a vector of axis labels, as numeric 

format_ylab <- function(
  x  
){
  find_one <- function(x){
    if(is.na(x)) return(NULL)
    if(abs(x) < 1) return(as.character(round(x, 3)))
    if(abs(x) < 10) return(as.character(round(x, 2)))
    if(abs(x) < 100) return(as.character(round(x, 1)))
    if(abs(x) < 1000) return(as.character(round(x)))
    if(abs(x) < 1e4) return(
      paste0(as.character(round(x/1e3, 2)), "k"))
    if(abs(x) < 1e5) return(
      paste0(as.character(round(x/1e3, 1)), "k"))
    if(abs(x) < 1e6) return(
      paste0(as.character(round(x/1e3)), "k"))
    if(abs(x) < 1e7) return(
      paste0(as.character(round(x/1e6, 2)), "M"))
    if(abs(x) < 1e8) return(
      paste0(as.character(round(x/1e6, 1)), "M"))
    if(abs(x) < 1e9) return(
      paste0(as.character(round(x/1e6)), "M"))
    if(abs(x) < 1e10) return(
      paste0(as.character(round(x/1e9, 2)), "B"))
    if(abs(x) < 1e11) return(
      paste0(as.character(round(x/1e9, 1)), "B"))
    if(abs(x) < 1e12) return(
      paste0(as.character(round(x/1e9)), "B"))
    if(abs(x) < 1e13) return(
      paste0(as.character(round(x/1e12, 2)), "T"))
    if(abs(x) < 1e14) return(
      paste0(as.character(round(x/1e12, 1)), "T"))
    if(abs(x) < 1e15) return(
      paste0(as.character(round(x/1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# ================================================================================
# format_ylab2

# note: formats the y-axis labels for the top chart 
# ...x: a vector of axis labels, as numeric 

format_ylab2 <- function(
  x  
){
  find_one <- function(x){
    if(is.na(x)) return(NULL)
    if(abs(x) < 1) return(as.character(round(x, 3)))
    if(abs(x) < 10) return(as.character(round(x, 2)))
    if(abs(x) < 100) return(as.character(round(x, 1)))
    if(abs(x) < 1000) return(as.character(round(x)))
    if(abs(x) < 1e4) return(
      paste0("$", as.character(round(x/1e3, 2)), "k"))
    if(abs(x) < 1e5) return(
      paste0("$", as.character(round(x/1e3, 1)), "k"))
    if(abs(x) < 1e6) return(
      paste0("$", as.character(round(x/1e3)), "k"))
    if(abs(x) < 1e7) return(
      paste0("$", as.character(round(x/1e6, 2)), "M"))
    if(abs(x) < 1e8) return(
      paste0("$", as.character(round(x/1e6, 1)), "M"))
    if(abs(x) < 1e9) return(
      paste0("$", as.character(round(x/1e6)), "M"))
    if(abs(x) < 1e10) return(
      paste0("$", as.character(round(x/1e9, 2)), "B"))
    if(abs(x) < 1e11) return(
      paste0("$", as.character(round(x/1e9, 1)), "B"))
    if(abs(x) < 1e12) return(
      paste0("$", as.character(round(x/1e9)), "B"))
    if(abs(x) < 1e13) return(
      paste0("$", as.character(round(x/1e12, 2)), "T"))
    if(abs(x) < 1e14) return(
      paste0("$", as.character(round(x/1e12, 1)), "T"))
    if(abs(x) < 1e15) return(
      paste0("$", as.character(round(x/1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# --------------------------------------------------------------------------------
# (currently hidden) tooltip for change chart 

# create_change_tip <- function(
#   # returns a html-formatted string to use in the budget change chart tooltip
#   hover_year,    # year the mouse is over
#   budget_dataset,  # current budget data
#   session = getDefaultReactiveDomain()
# ){
#   this_year <- filter(budget_dataset, FY == hover_year)
#   until_now <- filter(budget_dataset, FY <= hover_year)
#   first_year <- min(until_now$FY)
#   
#   tip <- paste0(
#     # "<div align = 'center'>",
#     "<div align = 'left'>",
#     "<b>Budget for FY", hover_year,"</b><br>",
#     br(), 
#     "<div align = 'left'>",
#     " Added Acq: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Added Acq")]
#           / 1e9, 2), "B<br/>",
#     " Cut Acq: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Cut Acq")]
#           / 1e9, 2), "B<br/>",
#     " Added O&S: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Added O&S")]
#           / 1e9, 2), "B<br/>",
#     " Cut O&S: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Cut O&S")]
#           / 1e9, 2), "B<br/>",
#     " Added R&D: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Added R&D")]
#           / 1e9, 2), "B<br/>",
#     " Cut R&D: $</b>",
#     round(this_year$Total[which(this_year$budget_cat == "Cut R&D")]
#           / 1e9, 2), "B<br/>",
#     "<u> ", " Net Change: $",
#     round(summarize(this_year, sum(Total)) / 1e9, 2), "B</u><br/>",
#     br(), 
# 
#     # "<div align = 'center'>",
#     "<div align = 'left'>",
#     "<b>Budget for FY", first_year, "-", hover_year, "</b><br>",
#     br(),
#     "<div align = 'left'>",
#     " Added Acq: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Added Acq"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     " Cut Acq: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Cut Acq"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     " Added O&S: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Added O&S"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     " Cut O&S: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Cut O&S"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     " Added R&D: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Added R&D"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     " Cut R&D: $</b>",
#     round(
#       summarize(filter(until_now, budget_cat == "Cut R&D"), sum(Total)) /
#         1e9, 2), "B<br/>",
#     "<u> Net Change: $",
#     round(summarize(until_now, sum(Total)) / 1e9, 2), "B</u><br/>",
#     br(),
#     "</div>",
#     
#     br(), 
#     
#     "<div align = 'left'>",
#     "<b>Annual Avg. Budget for FY", first_year, "-", hover_year, "</b><br>",
#     br(),
#     "<div align = 'left'>",
#     " Added Acq: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Added Acq"), sum(Total)) /
#         1e9)/(hover_year - 2017), 2), "B<br/>",
#     " Cut Acq: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Cut Acq"), sum(Total)) /
#         1e9)/(hover_year - 2017), 2), "B<br/>",
#     " Added O&S: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Added O&S"), sum(Total)) /
#         1e9)/(hover_year = 2017), 2), "B<br/>",
#     " Cut O&S: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Cut O&S"), sum(Total)) /
#         1e9)/(hover_year - 2017), 2), "B<br/>",
#     " Added R&D: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Added R&D"), sum(Total)) /
#         1e9)/(hover_year - 2017), 2), "B<br/>",
#     " Cut R&D: $</b>",
#     round(
#       (summarize(filter(until_now, budget_cat == "Cut R&D"), sum(Total)) /
#         1e9)/(hover_year - 2017), 2), "B<br/>",
#     "<u> Net Change: $",
#     round((summarize(until_now, sum(Total)) / 1e9)/(hover_year - 2017), 2), "B</u><br/>",
#     br(),
#     "</div>"
#   )
#   
#   return(tip)
# }

# ================================================================================
# create_total_tip

# note: returns a html-formatted string to use in the budget total chart tooltip
# ...hover_year: year the mouse is over 
# ...budget_dataset: current budget data

create_total_tip <- function(

  
  hover_year,    
  budget_dataset, 
  session = getDefaultReactiveDomain()
){
  # 
  this_year <- filter(budget_dataset, FY == hover_year)
  until_now <- filter(budget_dataset, FY <= hover_year)
  first_year <- min(until_now$FY)
  
  tip <- paste0(
    # "<div align = 'center'>",
    "<div align = 'left'>",
    "<b>Budget for FY", hover_year,"</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $",
    round(this_year$Total[which(this_year$budget_cat == "Acquisition")]
          / 1e9, 2), "B<br/>",
    " O&S: $",
    round(this_year$Total[which(this_year$budget_cat == "O&S")]
          / 1e9, 2), "B<br/>",
    " R&D: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "R&D")]
          / 1e9, 2), "B<br/>",
    " Upgrade: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Upgrade")]
          / 1e9, 2), "B<br/>",
    "<u> ", " Total: $",
    round(summarize(this_year, sum(Total)) / 1e9, 2), "B<br/></u>",
    br(), 
    "<div align = 'left'>",
    
    
    "<b>Budget for FY", first_year, "-", hover_year, "</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "Acquisition")]
          / 1e9), 2), "B<br/>",
    " O&S: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "O&S")]
          / 1e9), 2), "B<br/>",
    " R&D: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "R&D")]
          / 1e9), 2), "B<br/>",
    " Upgrade: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Upgrade")]
          / 1e9, 2), "B<br/>",
    "<u> ", " Total: $",
    round(summarize(until_now, sum(Total)) / 1e9, 2), "B<br/></u>",
    "</div>",
    br(),
    "<div align = 'left'>",
    
    
    "<b>Annual Avg. Budget for FY", first_year, "-", hover_year, "</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "Acquisition")]
              / 1e9)/(hover_year - 2018), 2), "B<br/>",
    " O&S: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "O&S")]
              / 1e9)/(hover_year - 2018), 2), "B<br/>",
    " R&D: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "R&D")]
              / 1e9)/(hover_year - 2018), 2), "B<br/>",
    " Upgrade: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "Upgrade")]
              / 1e9)/(hover_year - 2018), 2), "B<br/>",
    "<u> ", " Average: $",
    round((summarize(until_now, sum(Total))/1e9)/(hover_year - 2018), 2), "B<br/></u>",
    "</div>"
  )
  
  return(tip)
}

# ================================================================================
# update_title

# note: make ggplot title for both the top chart and the bottom chart reactive
# ...populates the title field with a dynamic title, if appropriate
# ...input: shiny input object 
# ...session = ...: shiny session object 

update_title <- function(
  input,   
  session = getDefaultReactiveDomain()  
){
  
  # 1st section of title
  if(input$UltraLightweight == 1){
    title_part1 <- " Ultra-Lightweight"
  }else if(input$Lightweight == 1){
    title_part1 <- " Lightweight"
  }else if(input$Middleweight == 1){
    title_part1 <- " Middleweight"
  }else if(input$Heavyweight == 1){
    title_part1 <- " Heavyweight"
  }else if(input$UltraHeavyweight == 1){
    title_part1 <- " Ultra-Heavyweight"
  }else{
    title_part1 <- ""
  }

  title_top_part2 <- input$top_y
  
  title <- c()
  
  if(input$top_chart == "Line"){
    if(title_top_part2 == "# of Vertical Lift"){
      title[1] <- paste0(title_part1," Vertical Lift ","Inventory")
    } else if(title_top_part2 %in% c("Speed (mph)",
                                     "Range (mi)",
                                     "Service ceiling (ft)",
                                     "Rate of climb (ft/min)",
                                     "Empty weight (lbs)",
                                     "Max TOW (lbs)"))
    {
      title[1] <- paste0("Avg. ",title_top_part2 ," for",title_part1," ","Vertical Lift")
    } else{ title[1] <- paste0(title_part1," Vertical Lift ",title_top_part2)}
    
    } else if(input$top_chart == "Aircraft Type" | input$top_chart == "Production Stage"){
      
    if(title_top_part2 == "# of Vertical Lift"){
      
      title[1] <- paste0(title_part1," Vertical Lift ","Inventory", " by ", input$top_chart)
      
    } else if(title_top_part2 %in%  c("Speed (mph)",
                                      "Range (mi)",
                                      "Service ceiling (ft)",
                                      "Rate of climb (ft/min)",
                                      "Empty weight (lbs)",
                                      "Max TOW (lbs)")) 
    {
      title[1] <- paste0("Avg. ", title_top_part2 ," for",title_part1, " Vertical Lift")
    } 
      else{ title[1] <- paste0(title_part1,"Vertical Lift"," ",title_top_part2," by ", input$top_chart)}
    } 
  
  # bottom chart
  if(input$bottom_chart == "Total"){
    title[2] <- paste0("Funding for",title_part1," Vertical Lift")
  } else{title[2] <- paste0("Change in Funding from",title_part1," Baseline")}    
  return(title)
  
}

# --------------------------------------------------------------------------------
# (currently hidden) update live data 

# update_live_data <- function(
#     current, vl_name, hex_code,
#     label_string, legend_order = "auto"){
#     # update the live data to include the new ship
#     ship_col <- which(names(current$vl) == vl_name)
#     if(is_empty(ship_col)){
#         ship_col <- length(current$vl) + 1}
#     current$vl[ship_col] <- current$FVL_data_gs[["Fleet Plan"]][[vl_name]]
#     names(current$vl)[ship_col] <- vl_name
# 
#     # define a new area-graph color, label, and legend order for the new ship
#     if(!(vl_name %in% names(current$area_colors))){
#         current$area_colors %<>% append(c("tempname" = hex_code))
#         names(current$area_colors)[which(names(current$area_colors) == "tempname")] <-
#             vl_name}
# 
# 
#     if(!(vl_name %in% names(current$area_labels))){
#         current$area_labels %<>% append(c("tempname" = label_string))
#         names(current$area_labels)[which(names(current$area_labels) == "tempname")] <-
#             vl_name}
# 
#     if(legend_order == "auto") legend_order <- length(current$legend_order)
# 
#     if(!(vl_name %in% current$legend_order)){
#         current$legend_order %<>% append(vl_name, legend_order)}
# 
#     return(current)
# }

# ================================================================================
# subset_budget_total

subset_budget_total <- function(shown, input){
    
  
    if(input$UltraLightweight == 1){
      shown <- select(shown,FY,names(FVL_stats_gs$UltraLightweight)[which(FVL_stats_gs$UltraLightweight == 1)],budget_cat)
      shown$Total <- rowSums(shown[,-c(1,ncol(shown))])
    }
    
    if(input$Lightweight == 1){
      shown <- select(shown,FY,names(FVL_stats_gs$Lightweight)[which(FVL_stats_gs$Lightweight == 1)], budget_cat)
      shown$Total <- rowSums(shown[,-c(1,ncol(shown))])
    }
    
    if(input$Middleweight == 1){
      shown <- select(shown,FY,names(FVL_stats_gs$Middleweight)[which(FVL_stats_gs$Middleweight == 1)],budget_cat)
      shown$Total <- rowSums(shown[,-c(1,ncol(shown))])
    }
    
    if(input$Heavyweight == 1){
      shown <- select(shown,FY,names(FVL_stats_gs$Heavyweight)[which(FVL_stats_gs$Heavyweight == 1)],budget_cat)
      shown$Total <- rowSums(shown[,-c(1,ncol(shown))])
    }
  
    if(input$UltraHeavyweight == 1){
      shown <- select(shown,FY,names(FVL_stats_gs$UltraHeavyweight)[which(FVL_stats_gs$UltraHeavyweight == 1)],budget_cat)
      shown$Total <- rowSums(shown[,-c(1,ncol(shown)),drop=FALSE])
    }
  
  return(shown)
}

# ================================================================================
# Popover.template: general function for the hover function

Popover.template <-function(Abbreviated.Name, Reference.Number, FullName, Nickname, Purpose,imagename,session){
    
    addPopover(
        session,
        Abbreviated.Name,
        title = NA,
        content = HTML(
            paste0(
              "<br/>",
              "<div align = 'center'",
              '<h3><b>',FullName, '</b></h3>', "<br/>", 
              Nickname,
              
              "<img src = " , imagename,
              " alt='', style='width:250px;height:auto;'>",
              
              "<div align = 'left'>", 
              # "<br/>",
              "<b> Function:  </b>", Purpose , "<br/>",
              "<br/>",
              
              "<br/>", 
              "<b> Acquisition cost: </b>", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Acquisition Cost"], "<br/>",
              "<b> Operation and Support Cost: </b>", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Total O&S Cost"], "<br/>",
              "<br/>",
              
              "<b> First Flight Year: </b>", "",FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "First Flight"],"<br/>",
              "<b> Introduction Year: </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Introduction"],"<br/>",
              "<b> Lastest Manufacturer: </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Lastest Manufacturer"],"<br/>",
              
              "<br/>",
              
              "<b> Crew: </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Crew"],"<br/>",
              "<b> Passengers: </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Passengers"],"<br/>",
              "<br/>",
              # "<b> Total Personnel: </b>", "",FVL_data_gs$popover_display[12,Reference.Number], "<br/>",
              
              "<b> Speed (mph): </b>", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Speed (mph)"],"", "<br/>",
              "<b> Range (mi): </b>", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Range (mi)"], "", "<br/>",
              "<b> Service ceiling (ft): </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Service ceiling (ft)"],"<br/>",
              "<b> Rate of climb (ft/min): </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Rate of climb (ft/min)"],"<br/>",
              "<b> Empty weight (lbs): </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Empty weight (lbs)"],"<br/>",
              "<b> Max Take Off Weight (lbs): </b>", "", FVL_data_gs$popover_display[[Reference.Number]][FVL_data_gs$popover_display$Class == "Max TOW (lbs)"],"<br/>",
              "<br/>"
            )
        ),
        placement = "right",
        trigger = 'hover'
    )
}

popover.templat.preset <- function(session,button_id,label,description){
  
  addPopover(
    session,
    id = button_id,
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'",
        '<h3><b>',label,'</b></h3>', "<br/>", 
        
        "<img src = " , "space.png",
        " alt='', style='width:250px;height:auto;'>",
        
        "<div align = 'left'>", 
        "<b> Description:  </b>", description, "<br/>", 
        "<br/>"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
}

# ================================================================================
# preset_value1235: preset value function - for preset_phase1/2/3/5

preset_value1235 <- function(input,which_preset_value,which_preset_phase,session){
  
  observeEvent(ignoreInit = TRUE, input[[which_preset_value]],{
    
    # inventory
    updateSliderInput(session,
                      inputId = "LittleBird-slider",
                      value = FVL_data_gs[[which_preset_phase]]$LittleBird[1])
    updateSliderInput(session,
                      inputId = "FireScout-slider",
                      value = FVL_data_gs[[which_preset_phase]]$FireScout[1])
    updateSliderInput(session,
                      inputId = "CobraViper-slider",
                      value = FVL_data_gs[[which_preset_phase]]$CobraViper[1])
    updateSliderInput(session,
                      inputId = "Apache-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[1])
    updateSliderInput(session,
                      inputId = "Creek-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Creek[1])
    updateSliderInput(session,
                      inputId = "Lakota-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Lakota[1])
    updateSliderInput(session,
                      inputId = "KiowaWarrior-slider",
                      value = FVL_data_gs[[which_preset_phase]]$KiowaWarrior[1])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-slider",
                      value = FVL_data_gs[[which_preset_phase]]$IroquoisHueyVenom[1])
    updateSliderInput(session,
                      inputId = "BlackHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[1])
    updateSliderInput(session,
                      inputId = "SeaHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaHawk[1])
    updateSliderInput(session,
                      inputId = "PaveHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$PaveHawk[1])
    updateSliderInput(session,
                      inputId = "CombatRescueHelicopter-slider",
                      value = FVL_data_gs[[which_preset_phase]]$CombatRescueHelicopter[1])
    updateSliderInput(session,
                      inputId = "SeaKing-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaKing[1])
    updateSliderInput(session,
                      inputId = "Superhawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Superhawk[1])
    updateSliderInput(session,
                      inputId = "Chinook-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[1])
    updateSliderInput(session,
                      inputId = "SeaDragonStallion-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaDragonStallion[1])
    updateSliderInput(session,
                      inputId = "Osprey-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Osprey[1])
    
    # future vertical lift
    # BYOUltraLightweight
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[1])
    # BYOLightweight
    updateSliderInput(session,
                      inputId = "BYOLightweight-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[1])
    # BYOMiddleweight
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[1])
    #BYOHeavyweight
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[1])
    
    # start year
    updateSliderInput(session,
                      inputId = "LittleBird-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$LittleBird[4])
    updateSliderInput(session,
                      inputId = "FireScout-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$FireScout[4])
    updateSliderInput(session,
                      inputId = "CobraViper-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$CobraViper[4])
    updateSliderInput(session,
                      inputId = "Apache-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[4])
    updateSliderInput(session,
                      inputId = "Creek-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Creek[4])
    updateSliderInput(session,
                      inputId = "Lakota-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Lakota[4])
    updateSliderInput(session,
                      inputId = "KiowaWarrior-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$KiowaWarrior[4])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$IroquoisHueyVenom[4])
    updateSliderInput(session,
                      inputId = "BlackHawk-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[4])
    updateSliderInput(session,
                      inputId = "SeaHawk-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$SeaHawk[4])
    updateSliderInput(session,
                      inputId = "PaveHawk-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$PaveHawk[4])
    updateSliderInput(session,
                      inputId = "CombatRescueHelicopter-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$CombatRescueHelicopter[4])
    updateSliderInput(session,
                      inputId = "SeaKing-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$SeaKing[4])
    updateSliderInput(session,
                      inputId = "Superhawk-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Superhawk[4])
    updateSliderInput(session,
                      inputId = "Osprey-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Osprey[4])
    updateSliderInput(session,
                      inputId = "Chinook-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[4])
    updateSliderInput(session,
                      inputId = "SeaDragonStallion-build_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$SeaDragonStallion[4])
    
    # RDT&E start year
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[5])
    updateSliderInput(session,
                      inputId = "BYOLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[5])
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[5])
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[5])
    
    # RDT&E duration
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-rd_duration",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[6])
    updateSliderInput(session,
                      inputId = "BYOLightweight-rd_duration",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[6])
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-rd_duration",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[6])
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-rd_duration",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[6])
    
    # build rate
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-build_retire_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[2])
    updateSliderInput(session,
                      inputId = "BYOLightweight-build_retire_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[2])
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-build_retire_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[2])
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-build_retire_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[2])
  })
}

# ================================================================================
# preset_value4

preset_value4 <- function(input,which_preset_value,which_preset_phase,session){
  
  observeEvent(ignoreInit = TRUE, input[[which_preset_value]],{
    
    # inventory
    updateSliderInput(session,
                      inputId = "LittleBird-slider",
                      value = FVL_data_gs[[which_preset_phase]]$LittleBird[1])
    updateSliderInput(session,
                      inputId = "FireScout-slider",
                      value = FVL_data_gs[[which_preset_phase]]$FireScout[1])
    updateSliderInput(session,
                      inputId = "CobraViper-slider",
                      value = FVL_data_gs[[which_preset_phase]]$CobraViper[1])
    updateSliderInput(session,
                      inputId = "Apache-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[1])
    updateSliderInput(session,
                      inputId = "Creek-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Creek[1])
    updateSliderInput(session,
                      inputId = "Lakota-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Lakota[1])
    updateSliderInput(session,
                      inputId = "KiowaWarrior-slider",
                      value = FVL_data_gs[[which_preset_phase]]$KiowaWarrior[1])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-slider",
                      value = FVL_data_gs[[which_preset_phase]]$IroquoisHueyVenom[1])
    updateSliderInput(session,
                      inputId = "BlackHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[1])
    updateSliderInput(session,
                      inputId = "SeaHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaHawk[1])
    updateSliderInput(session,
                      inputId = "PaveHawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$PaveHawk[1])
    updateSliderInput(session,
                      inputId = "CombatRescueHelicopter-slider",
                      value = FVL_data_gs[[which_preset_phase]]$CombatRescueHelicopter[1])
    updateSliderInput(session,
                      inputId = "SeaKing-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaKing[1])
    updateSliderInput(session,
                      inputId = "Superhawk-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Superhawk[1])
    updateSliderInput(session,
                      inputId = "Chinook-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[1])
    updateSliderInput(session,
                      inputId = "SeaDragonStallion-slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaDragonStallion[1])
    updateSliderInput(session,
                      inputId = "Osprey-slider",
                      value = FVL_data_gs[[which_preset_phase]]$Osprey[1])
    
    # upgrade number
    updateSliderInput(session,
                      inputId = "LittleBird-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$LittleBird[7])
    updateSliderInput(session,
                      inputId = "FireScout-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$FireScout[7])
    updateSliderInput(session,
                      inputId = "CobraViper-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$CobraViper[7])
    updateSliderInput(session,
                      inputId = "Apache-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[7])
    updateSliderInput(session,
                      inputId = "Creek-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Creek[7])
    updateSliderInput(session,
                      inputId = "Lakota-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Lakota[7])
    updateSliderInput(session,
                      inputId = "KiowaWarrior-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$KiowaWarrior[7])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$IroquoisHueyVenom[7])
    updateSliderInput(session,
                      inputId = "BlackHawk-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[7])
    updateSliderInput(session,
                      inputId = "SeaHawk-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaHawk[7])
    updateSliderInput(session,
                      inputId = "PaveHawk-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$PaveHawk[7])
    updateSliderInput(session,
                      inputId = "CombatRescueHelicopter-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$CombatRescueHelicopter[7])
    updateSliderInput(session,
                      inputId = "SeaKing-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaKing[7])
    updateSliderInput(session,
                      inputId = "Superhawk-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Superhawk[7])
    updateSliderInput(session,
                      inputId = "Chinook-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[7])
    updateSliderInput(session,
                      inputId = "SeaDragonStallion-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$SeaDragonStallion[7])
    updateSliderInput(session,
                      inputId = "Osprey-upgrade_slider",
                      value = FVL_data_gs[[which_preset_phase]]$Osprey[7])

    
    # upgrade rate
    updateSliderInput(session,
                      inputId = "Apache-upgrade_rate",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[8])
    updateSliderInput(session,
                      inputId = "CobraViper-upgrade_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[8])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-upgrade_rate",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[8])
    updateSliderInput(session,
                      inputId = "BlackHawk-upgrade_rate",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[8])
    updateSliderInput(session,
                      inputId = "Chinook-upgrade_rate",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[8])
    
    # upgrade start year
    
    updateSliderInput(session,
                      inputId = "Apache-upgrade_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Apache[9])
    updateSliderInput(session,
                      inputId = "CobraViper-upgrade_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[9])
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-upgrade_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[9])
    updateSliderInput(session,
                      inputId = "BlackHawk-upgrade_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$BlackHawk[9])
    updateSliderInput(session,
                      inputId = "Chinook-upgrade_start_yr",
                      value = FVL_data_gs[[which_preset_phase]]$Chinook[9])
    
    # RDT&E start year
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[5])
    updateSliderInput(session,
                      inputId = "BYOLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[5])
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[5])
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[5])
    
    
    # RDT&E start year
    updateSliderInput(session,
                      inputId = "BYOUltraLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOUltraLightweight[6])
    updateSliderInput(session,
                      inputId = "BYOLightweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOLightweight[6])
    updateSliderInput(session,
                      inputId = "BYOMiddleweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOMiddleweight[6])
    updateSliderInput(session,
                      inputId = "BYOHeavyweight-rd_start_num",
                      value = FVL_data_gs[[which_preset_phase]]$BYOHeavyweight[6])
    
    
    # performance specs for Apache
    updateSliderInput(session,
                      inputId = "Apache-speed",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Speed (mph)"] * 
                              as.numeric(FVL_stats_gs$`Speed (mph)`[["Apache"]])) 
    
    updateSliderInput(session,
                      inputId = "Apache-range",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Range (mi)"] * as.numeric(FVL_stats_gs$`Range (mi)`[["Apache"]]))
    
    updateSliderInput(session,
                      inputId = "Apache-service_ceiling",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Service ceiling (ft)"] * 
                              as.numeric(FVL_stats_gs$`Service ceiling (ft)`[["Apache"]]))
    
    updateSliderInput(session,
                      inputId = "Apache-rate_climbing",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Rate of climb (ft/min)"] * 
                              as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[["Apache"]]))
    
    updateSliderInput(session,
                      inputId = "Apache-empty_weight",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Empty weight (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Empty weight (lbs)`[["Apache"]]))
    
    updateSliderInput(session,
                      inputId = "Apache-max_tow",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Max TOW (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Max TOW (lbs)`[["Apache"]]))
    
    
    # performance specs for CobraViper
    updateSliderInput(session,
                      inputId = "CobraViper-speed",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Speed (mph)"] * 
                              as.numeric(FVL_stats_gs$`Speed (mph)`[["CobraViper"]])) 
    
    updateSliderInput(session,
                      inputId = "CobraViper-range",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Range (mi)"] * 
                              as.numeric(FVL_stats_gs$`Range (mi)`[["CobraViper"]]))
    
    updateSliderInput(session,
                      inputId = "CobraViper-service_ceiling",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Service ceiling (ft)"] * 
                              as.numeric(FVL_stats_gs$`Service ceiling (ft)`[["CobraViper"]]))
    
    updateSliderInput(session,
                      inputId = "CobraViper-rate_climbing",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Rate of climb (ft/min)"] * 
                              as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[["CobraViper"]]))
    
    updateSliderInput(session,
                      inputId = "CobraViper-empty_weight",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Empty weight (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Empty weight (lbs)`[["CobraViper"]]))
    
    updateSliderInput(session,
                      inputId = "CobraViper-max_tow",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Max TOW (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Max TOW (lbs)`[["CobraViper"]]))
    
    
    # performance specs for IroquoisHueyVenom
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-speed",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Speed (mph)"] * 
                              as.numeric(FVL_stats_gs$`Speed (mph)`[["IroquoisHueyVenom"]])) 
    
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-range",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Range (mi)"] * 
                              as.numeric(FVL_stats_gs$`Range (mi)`[["IroquoisHueyVenom"]]))
    
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-service_ceiling",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Service ceiling (ft)"] * 
                              as.numeric(FVL_stats_gs$`Service ceiling (ft)`[["IroquoisHueyVenom"]]))
    
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-rate_climbing",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Rate of climb (ft/min)"] * 
                              as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[["IroquoisHueyVenom"]]))
    
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-empty_weight",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Empty weight (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Empty weight (lbs)`[["IroquoisHueyVenom"]]))
    
    updateSliderInput(session,
                      inputId = "IroquoisHueyVenom-max_tow",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Max TOW (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Max TOW (lbs)`[["IroquoisHueyVenom"]]))
    
    
    # performance specs for BlackHawk
    updateSliderInput(session,
                      inputId = "BlackHawk-speed",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Speed (mph)"] * 
                              as.numeric(FVL_stats_gs$`Speed (mph)`[["BlackHawk"]])) 
    
    updateSliderInput(session,
                      inputId = "BlackHawk-range",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Range (mi)"] * 
                              as.numeric(FVL_stats_gs$`Range (mi)`[["BlackHawk"]]))
    
    updateSliderInput(session,
                      inputId = "BlackHawk-service_ceiling",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Service ceiling (ft)"] * 
                              as.numeric(FVL_stats_gs$`Service ceiling (ft)`[["BlackHawk"]]))
    
    updateSliderInput(session,
                      inputId = "BlackHawk-rate_climbing",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Rate of climb (ft/min)"] * 
                              as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[["BlackHawk"]]))
    
    updateSliderInput(session,
                      inputId = "BlackHawk-empty_weight",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Empty weight (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Empty weight (lbs)`[["BlackHawk"]]))
    
    updateSliderInput(session,
                      inputId = "BlackHawk-max_tow",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Max TOW (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Max TOW (lbs)`[["BlackHawk"]]))
    
    
    # performance specs for Chinook
    updateSliderInput(session,
                      inputId = "Chinook-speed",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Speed (mph)"] * 
                              as.numeric(FVL_stats_gs$`Speed (mph)`[["Chinook"]])) 
    
    updateSliderInput(session,
                      inputId = "Chinook-range",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Range (mi)"] * 
                              as.numeric(FVL_stats_gs$`Range (mi)`[["Chinook"]]))
    
    updateSliderInput(session,
                      inputId = "Chinook-service_ceiling",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Service ceiling (ft)"] * 
                              as.numeric(FVL_stats_gs$`Service ceiling (ft)`[["Chinook"]]))
    
    updateSliderInput(session,
                      inputId = "Chinook-rate_climbing",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Rate of climb (ft/min)"] * 
                              as.numeric(FVL_stats_gs$`Rate of climb (ft/min)`[["Chinook"]]))
    
    updateSliderInput(session,
                      inputId = "Chinook-empty_weight",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Empty weight (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Empty weight (lbs)`[["Chinook"]]))
    
    updateSliderInput(session,
                      inputId = "Chinook-max_tow",
                      value = FVL_stats_gs$upgrades_parameter$times[FVL_stats_gs$upgrades_parameter$`Upgrade Cost` == "Max TOW (lbs)"] * 
                              as.numeric(FVL_stats_gs$`Max TOW (lbs)`[["Chinook"]]))
    
  })
}

# ================================================================================