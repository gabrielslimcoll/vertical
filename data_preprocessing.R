# ================================================================================
# Build Your Own: Vertical Lift
# Designed and built by Gabriel Coll and Yuanjing Han
# --------------------------------------------------------------------------------
# data preprocessing: read from Google sheet 
# ================================================================================

# --------------------------------------------------------------------------------
# note: read data from Google Sheet in Google Drive
# ...No intermediate EXCEL sheets to read

# Input: "app data" in Google drive 

# Output:
# (1) FVL_data_gs.Rda includes build/fleet plan per year, acquisition/O&S Cost 
#     per year, blanks sheets which track how many vertical lifts we actually 
#     build/upgrade per year, all preset value settings and a well-formatted 
#     data frame for popover (display purpose)
# (2) FVL_stats_gs.Rda includes all cost/performance assumptions per vertical 
#     lifts as well as build start year and average build rate

# note: for this vertical lift project and any other Build Your Own app that this
# ...app's framework can be adapted to, one goal (of the many goals) is to make 
# sure that all the numbers/strings in the scripts are either from 
# FVL_data_gs.Rda/FVL_stats_gs.Rda or shiny inputId --- any absolute reference 
# including fixed number like starting/ending years(2019,2040), number indexing of 
# dataframes and other assumptions that the scripts made should be eliminated in 
# the script.

# --------------------------------------------------------------------------------
# load packages 

library(googlesheets)
library(tidyverse)

# expect a prompt to authenticate with Google interactively here
my_sheets <- gs_ls()

# take a look at a summary of all google sheets
my_sheets %>% glimpse()

# register a sheet
fvl_app <- gs_title("app data")

# get worksheet names as a character vector                                              
gs_ws_ls(fvl_app)

# read every worksheet into a list
FVL_book <- list()

# note: set Sys.sleep() so that we don't get 
# ..."Too Many Requests (RFC 6585) (HTTP 429)" error message

for(i in 1:length(gs_ws_ls(fvl_app))){
  
  # skip some sheets which the app doesn't use
  if(gs_ws_ls(fvl_app)[i] %in% c("Marine One O&S calculations",
                                 "Vertical Input Data - example",
                                 "Navy Input Data example",
                                 "Cost and Performance scrap",
                                 "O&S Cost Conversion",
                                 "AWS Inventory Breakdown",
                                 "Deflators",
                                 "Color Code for Sources",
                                 "Notes",
                                 "Deflators 2",
                                 "Cost Conversion notes"
  )){
    next
  }
  
  app_data <- fvl_app %>% gs_read(ws = gs_ws_ls(fvl_app)[i])
  FVL_book[[gs_ws_ls(fvl_app)[i]]] <- app_data
  Sys.sleep(6)
}

# --------------------------------------------------------------------------------
# FVL_stats_gs

FVL_stats_gs <- list()

create_stat_frame <- function(var_name,dataset){
  new_frame <- dataset %>%
    filter(Class == var_name) %>%
    select(-Class) 
  new_frame <- as.data.frame(lapply(new_frame,function(x) as.numeric(x)))
  
  return(new_frame)
}

# --------------------------------------------------------------------------------
# the following data frame is only for display

popover_display <- FVL_book$`Cost, Personnel, Performance`

# --------------------------------------------------------------------------------
# the following data frame is for calculating

# note: NAs in "Cost, Personnel, Performance" table means not known/not available
# ...so we'll just exclude them when summing up numbers --- the effect is 
# ...the same as setting them to zeros

FVL_book$`Cost, Personnel, Performance`[is.na(FVL_book$`Cost, Personnel, Performance`)] <- 0
FVL_book$`Cost, Personnel, Performance` <- filter(FVL_book$`Cost, Personnel, Performance`,Class != "Lastest Manufacturer")
FVL_book$`Cost, Personnel, Performance` <- as.data.frame(lapply(FVL_book$`Cost, Personnel, Performance`,function(x) gsub("[$,]","",x)),
                                                         stringsAsFactors=FALSE)

FVL_book$`Cost, Personnel, Performance`$Class <- as.character(FVL_book$`Cost, Personnel, Performance`$Class)

temp_FVL_stats <- FVL_book$`Cost, Personnel, Performance`[1,][rep(seq_len(nrow(FVL_book$`Cost, Personnel, Performance`[1,])), times = 5),]

temp_FVL_stats$Class <- c("UltraLightweight","Lightweight","Middleweight","Heavyweight","UltraHeavyweight")

# note: generate dummy variables for BYOUltraLightweight, BYOLightweight, 
# ...BYOMiddleweight, BYOHeavyweight and BYOUltraHeavyweight

temp_FVL_stats[1,-1] <- as.numeric(temp_FVL_stats[1,-1] == "UltraLight")
temp_FVL_stats[2,-1] <- as.numeric(temp_FVL_stats[2,-1] == "Light")
temp_FVL_stats[3,-1] <- as.numeric(temp_FVL_stats[3,-1] == "Medium")
temp_FVL_stats[4,-1] <- as.numeric(temp_FVL_stats[4,-1] == "Heavy")
temp_FVL_stats[5,-1] <- as.numeric(temp_FVL_stats[5,-1] == "UltraHeavy")

FVL_book$`Cost, Personnel, Performance` <- rbind(FVL_book$`Cost, Personnel, Performance`,temp_FVL_stats)


for(i in 4:length(FVL_book$`Cost, Personnel, Performance`$Class)){
  
  if(FVL_book$`Cost, Personnel, Performance`$Class[i] %in% c("First Flight","Introduction")){
    next
  }
  
  FVL_stats_gs[[FVL_book$`Cost, Personnel, Performance`$Class[i]]] <- create_stat_frame(FVL_book$`Cost, Personnel, Performance`$Class[i],
                                                                                               FVL_book$`Cost, Personnel, Performance`)
}

# R&D cost

FVL_stats_gs$`R&D Cost` <- FVL_stats_gs$`Acquisition Cost`
FVL_stats_gs$`R&D Cost`[1:(length(FVL_stats_gs$`R&D Cost`) - 5)] <- 0
FVL_stats_gs$`R&D Cost`[18:22] <-
                  as.numeric(unlist(lapply(FVL_book$`R&D Cost Assumptions`$`R&D preset (FY18 $)`[1:5],function(x) gsub("[$,]","",x))))

# --------------------------------------------------------------------------------

# store the 2018 level of helicopters in the fleet in FVL_stats_gs
FVL_stats_gs$to_retire <- FVL_stats_gs$`Acquisition Cost`

FVL_stats_gs$to_retire <- FVL_book$`Inventory Plan`[1,-1]
FVL_stats_gs$to_retire$BYOUltraLightweight <- 0
FVL_stats_gs$to_retire$BYOLightweight <- 0
FVL_stats_gs$to_retire$BYOMiddleweight <- 0
FVL_stats_gs$to_retire$BYOHeavyweight <- 0
FVL_stats_gs$to_retire$BYOUltraHeavyweight <- 0

FVL_stats_gs$to_retire <- as.data.frame(lapply(FVL_stats_gs$to_retire, function(x) as.numeric(x)))

# --------------------------------------------------------------------------------

# note: extract information from 'Build Plan' to obtain the default build_start_yr 
# ...and max_build_rate 

# (1) helicopters we decide to build more
FVL_stats_gs$Build_start_yr <- FVL_stats_gs$`Acquisition Cost`
FVL_stats_gs$max_build_rate <- FVL_stats_gs$`Acquisition Cost`

FVL_book$`Build Plan`[is.na(FVL_book$`Build Plan`)] <- 0

FVL_data_build <- FVL_book$`Build Plan`[4:(which(FVL_book$`Build Plan`$`Fiscal Year` == max(FVL_book$`Build Plan`$`Fiscal Year`))),]


names(FVL_data_build) <- gsub(" ","", names(FVL_data_build))
names(FVL_data_build) <- gsub("/","", names(FVL_data_build))
names(FVL_data_build) <- gsub("[()]","", names(FVL_data_build))
names(FVL_data_build)[1] <- "FY"

get_build_start_yr <- function(vlname){
  
  if(any(FVL_data_build[[vlname]] != 0)){
    build_start_yr <- FVL_data_build$FY[min(which(FVL_data_build[vlname] != 0))]
  } else{
    build_start_yr <- 0
  }
  return(build_start_yr)
}

get_build_rate <- function(vlname){
  
  FVL_data_build[[vlname]] <- as.numeric(FVL_data_build[[vlname]])
  
  if(any(FVL_data_build[[vlname]] != 0)){
    mean_build_rate <- round(sum(FVL_data_build[[vlname]][FVL_data_build[[vlname]] != 0]) / 
                               length(FVL_data_build[[vlname]][FVL_data_build[[vlname]] != 0]), 2)
  } else{
    mean_build_rate <- 0
  }
  return(mean_build_rate)
}

build_start_yr <- sapply(names(FVL_data_build)[-1],get_build_start_yr) %>% as.list() %>% as.data.frame()
build_rate <- sapply(names(FVL_data_build)[-1],get_build_rate) %>% as.list() %>% as.data.frame()

FVL_stats_gs$Build_start_yr <- build_start_yr
FVL_stats_gs$Build_start_yr$BYOUltraLightweight <- 0
FVL_stats_gs$Build_start_yr$BYOLightweight <- 0
FVL_stats_gs$Build_start_yr$BYOMiddleweight <- 0
FVL_stats_gs$Build_start_yr$BYOHeavyweight <- 0
FVL_stats_gs$Build_start_yr$BYOUltraHeavyweight <- 0

FVL_stats_gs$max_build_rate <- build_rate
FVL_stats_gs$max_build_rate$BYOUltraLightweight <- 0
FVL_stats_gs$max_build_rate$BYOLightweight <- 0
FVL_stats_gs$max_build_rate$BYOMiddleweight <- 0
FVL_stats_gs$max_build_rate$BYOHeavyweight <- 0
FVL_stats_gs$max_build_rate$BYOUltraHeavyweight <- 0

# --------------------------------------------------------------------------------

# rename all dataframes in FVL_stats_gs
for(i in 1:length(FVL_stats_gs)){
  
  names(FVL_stats_gs[[i]]) <- c("LittleBird","FireScout","CobraViper","Apache","Creek","Lakota","KiowaWarrior","IroquoisHueyVenom",
                                "BlackHawk","SeaHawk","PaveHawk","CombatRescueHelicopter","SeaKing","Superhawk","Chinook","SeaDragonStallion",
                                "Osprey","BYOUltraLightweight","BYOLightweight","BYOMiddleweight","BYOHeavyweight","BYOUltraHeavyweight")
}

# --------------------------------------------------------------------------------
# upgraded performance specs - read from Google Sheet

FVL_stats_gs$`Upgraded Speed (mph)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Speed (mph)"] * FVL_stats_gs$`Speed (mph)`
FVL_stats_gs$`Upgraded Range (mi)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Range (mi)"] * FVL_stats_gs$`Range (mi)`
FVL_stats_gs$`Upgraded Service ceiling (ft)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Service ceiling (ft)"] * FVL_stats_gs$`Service ceiling (ft)`
FVL_stats_gs$`Upgraded Rate of climb (ft/min)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Rate of climb (ft/min)"] * FVL_stats_gs$`Rate of climb (ft/min)`
FVL_stats_gs$`Upgraded Empty weight (lbs)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Empty weight (lbs)"] * FVL_stats_gs$`Empty weight (lbs)`
FVL_stats_gs$`Upgraded Max TOW (lbs)` <- FVL_book$`Upgrade Assumptions`$times[FVL_book$`Upgrade Assumptions`$`Upgrade Cost` == "Max TOW (lbs)"] * FVL_stats_gs$`Max TOW (lbs)`

# --------------------------------------------------------------------------------
# plug-in for R&D numbers for the first five years

FVL_stats_gs$preloaded_rd <- list()

FVL_stats_gs$preloaded_rd[['2019']] <- 2141.622 * 1000000
FVL_stats_gs$preloaded_rd[['2020']] <- 1618.164 * 1000000
FVL_stats_gs$preloaded_rd[['2021']] <- 1061.256 * 1000000
FVL_stats_gs$preloaded_rd[['2022']] <- 1092.836 * 1000000
FVL_stats_gs$preloaded_rd[['2023']] <- 1054.384 * 1000000

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
  )

# deflate all R&D costs after FY 2023
for(i in seq(2024,2040,1)){
  FVL_stats_gs$preloaded_rd[as.character(i)] <- 1054.384 * 1000000 / deflate[[as.character(i)]]
}

# --------------------------------------------------------------------------------
# FVL_data_gs

FVL_data_gs <- list()

# build plan
FVL_data_gs$`Build Plan` <- filter(FVL_book$`Build Plan`, `Fiscal Year` >= 2019 & `Fiscal Year` <= 2040)
FVL_data_gs$`Build Plan`[is.na(FVL_data_gs$`Build Plan`)] <- 0

# lapply outputs a list
FVL_data_gs$`Build Plan` <- as.data.frame(lapply(FVL_data_gs$`Build Plan`,function(x) as.numeric(x)))

# R&D cost
FVL_data_gs$`R&D Cost` <- FVL_data_gs$`Build Plan`
FVL_data_gs$`R&D Cost`[-1] <- 0

# baseline
FVL_data_gs$Baseline <- filter(FVL_book$`Inventory Plan`,`Fiscal Year` >= 2019 & `Fiscal Year` <= 2040)
FVL_data_gs$Baseline[-1] <- FVL_book$`Inventory Plan`[-1] %>% slice(rep(1, times = 22))
FVL_data_gs$Baseline <- as.data.frame(lapply(FVL_data_gs$Baseline,function(x) as.numeric(x)))

# fleet plan
FVL_data_gs[["Fleet Plan"]] <- FVL_data_gs$Baseline 
FVL_data_gs[["Fleet Plan"]][-1] <- FVL_data_gs$Baseline[-1] + 
                                   as.data.frame(lapply(FVL_data_gs[["Build Plan"]][-1], function(x) cumsum(x)))

# O&S yearly
FVL_data_gs[["O&S Yearly"]] <- FVL_data_gs$Baseline 
FVL_data_gs[["O&S Yearly"]][-1] <- FVL_stats_gs$`Total O&S Cost`[1:17][rep(seq_len(nrow(FVL_stats_gs$`Total O&S Cost`[1:17])), times = nrow(FVL_data_gs$Baseline)),]

# acquisition yearly
FVL_data_gs[["Acquisition Yearly"]] <- FVL_data_gs$Baseline 
FVL_data_gs[["Acquisition Yearly"]][-1] <- FVL_stats_gs$`Acquisition Cost`[1:17][rep(seq_len(nrow(FVL_stats_gs$`Acquisition Cost`[1:17])), times = nrow(FVL_data_gs$Baseline)),]

# upgrade cost
FVL_data_gs[["Upgrade Cost"]] <- FVL_data_gs$Baseline 
FVL_data_gs[["Upgrade Cost"]][-1] <- FVL_stats_gs$`Upgrade Cost`[1:17][rep(seq_len(nrow(FVL_stats_gs$`Upgrade Cost`[1:17])), times = nrow(FVL_data_gs$Baseline)),]

# blank sheet
FVL_data_gs[["Blank Sheet"]] <- FVL_data_gs[["Upgrade Cost"]]
FVL_data_gs[["Blank Sheet"]][-1] <- 0

# actual builds
FVL_data_gs[["Actual Builds"]] <- FVL_data_gs[["Blank Sheet"]]

# upgrade number
FVL_data_gs[["Upgrade Number"]] <- FVL_data_gs[["Blank Sheet"]]

# total upgrade cost
FVL_data_gs[["Upgrade"]] <- FVL_data_gs[["Blank Sheet"]]

future_ships <- c("BYOUltraLightweight","BYOLightweight","BYOMiddleweight","BYOHeavyweight","BYOUltraHeavyweight")

for(i in 1:length(future_ships)){
  
  col <- which(names(FVL_data_gs[[1]]) == future_ships[i])
  if(is_empty(col)) col <- length(FVL_data_gs[[1]]) + 1
  years <- nrow(FVL_data_gs[[1]])
  stats_col <- which(names(FVL_stats_gs[[1]]) == future_ships[i])
  
  FVL_data_gs[["Build Plan"]][col] <- rep(0, years)
  FVL_data_gs[["Baseline"]][col] <- rep(0, years)
  FVL_data_gs[["R&D Cost"]][col] <- rep(0, years) 
  FVL_data_gs[["Fleet Plan"]][col] <- rep(0, years)
  FVL_data_gs[["O&S Yearly"]][col] <- 
    as.numeric(rep(unlist(FVL_stats_gs[["Total O&S Cost"]][stats_col]), years))
  FVL_data_gs[["Acquisition Yearly"]][col] <-
    as.numeric(rep(unlist(FVL_stats_gs[["Acquisition Cost"]][stats_col]), years))
  FVL_data_gs[["Upgrade Cost"]][col] <-
    as.numeric(rep(unlist(FVL_stats_gs[["Upgrade Cost"]][stats_col]), years))
  
  FVL_data_gs[["Blank Sheet"]][col] <- rep(0, years)
  FVL_data_gs[["Actual Builds"]][col] <- rep(0,years)
  FVL_data_gs[["Upgrade Number"]][col] <- rep(0,years)
  FVL_data_gs[["Upgrade"]][col] <- rep(0,years)
  
  for(j in 1:length(FVL_data_gs)){
    names(FVL_data_gs[[j]])[col] <- future_ships[i]
  }
}

# --------------------------------------------------------------------------------
# preset options

# find the separator row between preset options
NA_row_index <- which(rowSums(is.na(FVL_book$`Preset Options`)) == ncol(FVL_book$`Preset Options`))

# default option
FVL_data_gs$default <- FVL_book$`Preset Options`[1:(NA_row_index[1]-1),-1]

# preset option 1
FVL_data_gs$preset_phase1 <- FVL_book$`Preset Options`[(NA_row_index[1]+1):(NA_row_index[2]-1),-1]

# preset option 2
FVL_data_gs$preset_phase2 <- FVL_book$`Preset Options`[(NA_row_index[2]+1):(NA_row_index[3]-1),-1]

# preset option 3
FVL_data_gs$preset_phase3 <- FVL_book$`Preset Options`[(NA_row_index[3]+1):(NA_row_index[4]-1),-1]

# preset option 4
FVL_data_gs$preset_phase4 <- FVL_book$`Preset Options`[(NA_row_index[4]+1):(NA_row_index[5]-1),-1]

# preset option 5
FVL_data_gs$preset_phase5 <- FVL_book$`Preset Options`[(NA_row_index[5]+1):nrow(FVL_book$`Preset Options`),-1]

FVL_data_gs$popover_display <- popover_display

for(i in 1:length(FVL_data_gs)){
  
  names(FVL_data_gs[[names(FVL_data_gs)[i]]]) <- gsub("\\.","", names(FVL_data_gs[[names(FVL_data_gs)[i]]]))
  names(FVL_data_gs[[names(FVL_data_gs)[i]]]) <- gsub("/","", names(FVL_data_gs[[names(FVL_data_gs)[i]]]))
  names(FVL_data_gs[[names(FVL_data_gs)[i]]]) <- gsub("[()]","", names(FVL_data_gs[[names(FVL_data_gs)[i]]]))
  names(FVL_data_gs[[names(FVL_data_gs)[i]]]) <- gsub(" ","",names(FVL_data_gs[[names(FVL_data_gs)[i]]]))
  names(FVL_data_gs[[names(FVL_data_gs)[i]]]) <- gsub("-","",names(FVL_data_gs[[names(FVL_data_gs)[i]]]))
  if(!(names(FVL_data_gs)[i] %in% c("default","preset_phase1","preset_phase2","preset_phase3","preset_phase4","preset_phase5","popover_display"))){
    names(FVL_data_gs[[names(FVL_data_gs)[i]]])[1] <- "FY"
    
  }
}

# --------------------------------------------------------------------------------

FVL_stats_gs$upgrades_parameter <- FVL_book$`Upgrade Assumptions` 

# --------------------------------------------------------------------------------

save("FVL_stats_gs", file = "FVL_stats_gs.Rda")
save("FVL_data_gs", file = "FVL_data_gs.Rda")

# --------------------------------------------------------------------------------

# take a look at the data types in two lists: make sure variables used for calculating are "numeric" and variables used for display are "character"

for(i in 1:length(FVL_stats_gs)){
  print("=====================================================================================================================================")
  print(names(FVL_stats_gs)[i])
  print(summary(FVL_stats_gs[[i]]))
}

for(i in 1:length(FVL_data_gs)){
  print("=====================================================================================================================================")
  print(names(FVL_data_gs)[i])
  print(summary(FVL_data_gs[[i]]))
}

# ================================================================================