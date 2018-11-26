# ================================================================================
# Build Your Own: Vertical Lift
# Designed and built by Gabriel Coll and Yuanjing Han
# --------------------------------------------------------------------------------
# user interface
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shinyjs)
library(shinyBS)
library(tidyverse)
library(extrafont)
library(extrafontdb)

# --------------------------------------------------------------------------------
# load functions 

source("module_functions.R")

# --------------------------------------------------------------------------------
# get the min, max, and default settings for sliders

load("data/FVL_data_gs.Rda")
load("data/FVL_stats_gs.Rda")

# --------------------------------------------------------------------------------
# begin ui section

ui <-
  
  fluidPage(
    useShinyjs(),
    
    # --------------------------------------------------------------------------------
    # import Google Font "Open Sans"
    
    tags$style(
      HTML(
        "@import url('//fonts.googleapis.com/css?family=Open+Sans');
        body {
        font-family: 'Open Sans',  sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #554449;
        }"
)
      ),

# --------------------------------------------------------------------------------
# app design style

tags$head(
  tags$style(HTML("body{background-color: #fcfcfc;}"))),

tags$style(HTML(
  ".tooltip {position: {my: 'left top', at: 'left bottom'}"
)),

tags$style(HTML(".fa-bars {
                color: #4D7FA3;
                }")),

tags$style(
  type = "text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
),

# --------------------------------------------------------------------------------
# button style

tags$style(
  HTML(
    ".btn {background-color: #4D7FA3;
    color: white;
    border-color: #FCFCFC}"
  )
),
tags$style(
  HTML(
    ".btn:hover {border-color: #FCFCFC;
    background-color: #2F4D63;
    color: white;
    font-weight: normal}"
  )
  ),
tags$style(HTML(
  ".btn:active:hover {background-color: #6BC6B5}"
)),
tags$style(HTML(
  ".btn:dropdown-toggle {background-color: red}"
)),

tags$style(
  HTML(
    ".btn-basic {background-color: #E5E5E5;
    border-color: #FCFCFC;
    border-color: #FCFCFC;
    color: #554449}"
  )
),
tags$style(
  HTML(".btn-basic:hover {background-color: #E3E3E3;
       color: #554449}")
  ),
tags$style(
  HTML(
    ".btn-basic:active,.open>.dropdown-toggle.btn-basic {background-color: #6BC6B5}"
  )
),
tags$style(
  HTML(".btn-basic:dropdown-toggle {background-color: #BDD4DE}")
),

tags$style(
  HTML(
    ".btn-primary {background-color: #BDD4DE;
    border-color: #FCFCFC;
    border-color: #FCFCFC;
    color: #554449}"
  )
),
tags$style(
  HTML(
    ".btn-primary:hover {background-color: #A5B9C2;
    color: #FFFFFF}"
  )
),

# --------------------------------------------------------------------------------
# slider style

tags$style(
  HTML(
    ".irs-bar {
    background: #788ca8;
    border-top: 1px #687991;
    border-bottom: 1px #687991}"
  )
),

tags$style(
  HTML(
    ".irs-bar-edge {border: 1px #566377;
    border-color: 1px #63c5b8}"
  )
),

tags$style(
  HTML(".irs .irs-bar-edge, .irs .irs-bar {
       background: #788ca8;
       }")
),

tags$style(HTML(".irs-max {color: #554449}")),
tags$style(HTML(".irs-min {color: #554449}")),
tags$style(HTML(
  ".irs-single, .irs-to, .irs-from {background: #687991}"
)),

# --------------------------------------------------------------------------------
# help block style

(
  tags$style(
    ".help-block {color: #554449;
    font-size: 14px;
    font-style: normal;
    background-color: #fcfcfc;
    border-color: #C76363;
    border-style: solid;
    border-width: 4px;
    border-top: 4px #63c5b8;
    border-bottom: 4px #63c5b8;
    border-right: 4px #63c5b8;
    #border-left: 4px #63c5b8;
    border-radius: 1px
    }"
  )
  ),

# --------------------------------------------------------------------------------
# popover style 

tags$style(HTML(".popover {background: #246D90};")),

# --------------------------------------------------------------------------------
# code to prevent early app disconnection 

tags$head(
  HTML(
    "
    <script>
    var socket_timeout_interval
    var n = 0
    $(document).on('shiny:connected', function(event) {
    socket_timeout_interval = setInterval(function(){
    Shiny.onInputChange('count', n++)
    }, 15000)
    });
    $(document).on('shiny:disconnected', function(event) {
    clearInterval(socket_timeout_interval)
    });
    </script>
    "
  )
  ),

# --------------------------------------------------------------------------------
# CSIS header

tags$div(
  HTML(
    "<div class='fusion-secondary-header'>
    <div class='fusion-row'>
    <div class='fusion-alignleft'><div class='fusion-contact-info'>
    <center style=' padding:15px;'>
    <a href='https://defense360.csis.org/content-type/data/' target='_blank'>
    <img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'>
    </a></center><a href='mailto:'></a></div></div>
    </div>
    </div>"
  )
  ),

tags$style(
  HTML(
    ".fusion-secondary-header {border-bottom: 2.5px solid #6F828F}"
  )
),

br(),

# --------------------------------------------------------------------------------
# begin sidebar panel

fluidRow(
  column(
    4,
    shinyjs::useShinyjs(),
    id = "side-panel",
    tags$head(tags$style(
      HTML(".body{background-color: #FCFCFC;}")
    )),
    
    tags$head(tags$style(
      HTML(".well{
           background-color: #FCFCFC;
           border-color: #FCFCFC;
           }")
)),

tags$style(HTML(
  ".popover({delay: {show: 500, hide: 100}})"
)),

# --------------------------------------------------------------------------------
# Google analytics script

tags$script(
  HTML(
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-99363803-1', 'auto');
    ga('send', 'pageview')"
)
),

# --------------------------------------------------------------------------------

hidden(textOutput("keepAlive")),

# --------------------------------------------------------------------------------
# info button

bsButton(
  inputId = "info_btn",
  label = strong("Build your own vertical lift fleet >"),
  style = "default",
  type = "action",
  size = "small",
  block = TRUE
),

br(),

# --------------------------------------------------------------------------------
# preset buttons

bsButton(
  inputId = "preset_value1",
  label = strong("Replace middleweight helos first in 2030"),
  type = "action",
  style = "primary",
  size = "small",
  width = '100%',
  block = TRUE,
  value = 0
),

div(
  style = "font-size: 10px; padding: 0px 0px; margin-top:0em",
  bsButton(
    inputId = "preset_value2",
    label = strong("Replace lightweight helos first in 2025"),
    type = "action",
    style = "primary",
    size = "small",
    width = '100%',
    block = TRUE,
    value = 0
  )
),

bsButton(
  inputId = "preset_value3",
  label = strong("Begin Future Vertical Lift at a low-rate of production"),
  type = "action",
  style = "primary",
  size = "small",
  width = '100%',
  block = TRUE,
  value = 0
),

div(
  style = "font-size: 10px; padding: 0px 0px; margin-top:0em",
  bsButton(
    inputId = "preset_value4",
    label = strong("Delay Future Vertical Lift and upgrade older helos"),
    type = "action",
    style = "primary",
    size = "small",
    width = '100%',
    block = TRUE,
    value = 0
  )
),

bsButton(
  inputId = "preset_value5",
  label = strong("Replace all helos beginning in the 2035"),
  type = "action",
  style = "primary",
  size = "small",
  width = '100%',
  block = TRUE,
  value = 0
),

div(
  style = "font-size: 10px; padding: 0px 0px; margin-top:0em",
  bsButton(
    inputId = "reset_input",
    label = strong("Reset"),
    style = "basic",
    size = "small",
    width = '100%',
    block = TRUE
  )
),

br(),

# --------------------------------------------------------------------------------
# ultra-lightweight panel 

div(
  style = "font-size: 10px; padding: 0px 0px; margin-top:-1.5em",
  bsButton(
    inputId = "UltraLightweight",
    label = strong("Ultra-Lightweight"),
    style = "primary",
    value = 0,
    type = "toggle",
    size = "small",
    block = TRUE
  )
),

conditionalPanel(
  condition = "input.UltraLightweight == 1",
  
  futurevlUI(
    "BYOUltraLightweight",
    "Build your own Ultra-Lightweight (Cap Set 1)"
  ),
  outofproductionvlUI("LittleBird", "Little Bird"),
  inproductionvlUI("FireScout", "Fire Scout")
  
),

# --------------------------------------------------------------------------------
# lightweight panel

bsButton(inputId = "Lightweight",
  label = strong("Lightweight"),
  style = "primary",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.Lightweight == 1",
  
  futurevlUI("BYOLightweight", "Build your own Lightweight (Cap Set 2)"),
  outofproductionvlUI("CobraViper", "Cobra / Viper"),
  inproductionvlUI("Apache", "Apache"),
  outofproductionvlUI("Creek", "Creek"),
  outofproductionvlUI("Lakota", "Lakota"),
  outofproductionvlUI("KiowaWarrior", "Kiowa (Warrior)"),
  outofproductionvlUI("IroquoisHueyVenom", "Iroquois / Huey / Venom")
  
),

# --------------------------------------------------------------------------------
# middleweight panel 

bsButton(
  inputId = "Middleweight",
  label = strong("Middleweight"),
  style = "primary",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.Middleweight == 1",

  futurevlUI("BYOMiddleweight", "Build your own middleweight (Cap Set 3)"),
  inproductionvlUI("BlackHawk", "Black Hawk"),
  outofproductionvlUI("SeaHawk", "Sea Hawk"),
  outofproductionvlUI("PaveHawk", "Pave Hawk"),
  inproductionvlUI("CombatRescueHelicopter", "Combat Rescue Helicopter"),
  outofproductionvlUI("SeaKing", "Sea King"),
  inproductionvlUI("Superhawk", "Superhawk")
  
),

# --------------------------------------------------------------------------------
# heavyweight panel 

bsButton(
  inputId = "Heavyweight",
  label = strong("Heavyweight"),
  style = "primary",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.Heavyweight == 1",
  
  futurevlUI("BYOHeavyweight", "Build your own Heavyweight (Cap Set 4)"),
  inproductionvlUI("Chinook", "Chinook"),
  inproductionvlUI("SeaDragonStallion", "Sea Dragon / Stallion"),
  inproductionvlUI("Osprey", "Osprey")
  
),

# --------------------------------------------------------------------------------
# ultra-heavyweight panel  

bsButton(
  inputId = "UltraHeavyweight",
  label = strong("Ultra-Heavyweight"),
  style = "primary",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.UltraHeavyweight == 1",

  futurevlUI("BYOUltraHeavyweight", "Build your own Ultra-Heavyweight (Cap Set 5)")
  
),

# --------------------------------------------------------------------------------
# help block section 

br(),

tabsetPanel(
  tabPanel("Specs",
           uiOutput("specs_change")),
  tabPanel("Cost",
           uiOutput("cost_change")),
  tabPanel("Info",
           uiOutput("info_display"))
),

# --------------------------------------------------------------------------------
# call modals for current vertical lift platforms 

OldbsModalUI("LittleBird", "Little Bird"),
OldbsModalUI("FireScout", "Fire Scout"),

OldbsModalUI("CobraViper", "Cobra / Viper"),
OldbsModalUI("Apache", "Apache"),
OldbsModalUI("Creek", "Creek"),
OldbsModalUI("Lakota", "Lakota"),
OldbsModalUI("KiowaWarrior", "Kiowa (Warrior)"),
OldbsModalUI("IroquoisHueyVenom", "Iroquois / Huey / Venom"),

OldbsModalUI("BlackHawk", "Black Hawk"),
OldbsModalUI("SeaHawk", "Sea Hawk"),
OldbsModalUI("PaveHawk", "Pave Hawk"),
OldbsModalUI("CombatRescueHelicopter", "Combat Rescue Helicopter"),
OldbsModalUI("SeaKing", "Sea King"),
OldbsModalUI("Superhawk", "Superhawk"),

OldbsModalUI("Chinook", "Chinook"),
OldbsModalUI("SeaDragonStallion", "Sea Dragon / Stallion"),
OldbsModalUI("Osprey", "Osprey"),

# --------------------------------------------------------------------------------
# call modals for future vertical lift platforms 

newbsModalUI(
  "BYOUltraLightweight",
  "Build your own Ultra-Lightweight helicopter"
),
newbsModalUI("BYOLightweight", "Build your own lightweight helicopter"),
newbsModalUI("BYOMiddleweight", "Build your own middleweight helicopter"),
newbsModalUI("BYOHeavyweight", "Build your own heavyweight helicopter"),
newbsModalUI(
  "BYOUltraHeavyweight",
  "Build your own ultra-heavyweight helicopter"
),

# --------------------------------------------------------------------------------
# (currently hidden) user submission function 

br(),

conditionalPanel(
  condition = "input.options_top_bottom == 3",
  textInput("text", label = NULL,
            placeholder = "Email (optional)"),
  textInput("text2", label = NULL,
            placeholder = "Comments (optional)")
),

br(),

conditionalPanel(
  condition = "input.options_top_bottom == 3",
  tags$head(tags$script(src = "message-handler.js")),
  bsButton(
    inputId = "submit",
    label = strong("Submit"),
    style = "default",
    size = "small",
    width = '100%',
    block = TRUE
  ),
  
  bsTooltip(
    "submit",
    "CSIS is collecting data...",
    "right",
    options = list(container = "body")
  ),
  
  bsTooltip(
    "text",
    "CSIS is collecting data...",
    "right",
    options = list(container = "body")
  ),
  
  bsTooltip(
    "text2",
    "CSIS is collecting data...",
    "right",
    options = list(container = "body")
  ),
  
  br(),
  br()
),

conditionalPanel(
  condition = "input.options_input == 3",
  div(
    style = "display:inline-block",
    checkboxInput("checkbox", label = "Dollars", value = FALSE)
  )
  
),

bsModal("modalExample", trigger = "info_btn", htmlOutput("description")),

align = "center"
  ),

# --------------------------------------------------------------------------------
# begin main panel 

mainPanel(tabsetPanel(
  type = "tabs",
  tabPanel(
    "App",
    div(
      style = "position:relative",
      plotOutput(
        "vl",
        height = "300px",
        hover = hoverOpts(id = "plot_hover_vl", delay = 80)
      ),
      uiOutput("hover_info_vl")
    ),

    # --------------------------------------------------------------------------------
    # top chart display 
    
    fluidRow(
      column(
        6,
        radioButtons(
          "top_chart",
          "",
          c("Line", "Aircraft Type", "Production Stage"),
          inline = TRUE,
          selected = "Production Stage"
        )
      ),
      
      # --------------------------------------------------------------------------------
      # top chart inputs  
      
      column(
        6,
        selectizeInput(
          inputId = "top_y",
          label = "",
          choices = list(
            "# of Vertical Lift" = c("# of Vertical Lift"),
            "Personnel (CBO)" = c("Crew",
                                  "Passengers"),
            "Performance / Specifications" = c(
              "Speed (mph)",
              "Range (mi)",
              "Service ceiling (ft)",
              "Rate of climb (ft/min)",
              "Empty weight (lbs)",
              "Max TOW (lbs)"
            )
          ),
          selected = "# of Vertical Lift"
        )
        
      ),
      
      column(4,
             hidden(
               radioButtons("bottom_chart", "",
                            c("Total", "Change"),
                            inline = TRUE)
             )),
      
      align = "center"
    ),
    
    shinyjs::useShinyjs(),
    id = "main-panel",
    
    # --------------------------------------------------------------------------------
    # bottom chart 
    
    div(
      style = "position:relative",
      plotOutput(
        "budget",
        height = "300px",
        hover = hoverOpts(id = "plot_hover_budget", delay = 80)
      ),
      uiOutput("hover_info_budget")
    ),
    align = 'center'
  ),
  
  # --------------------------------------------------------------------------------
  # vertical lift budget data 
  
  tabPanel("Vertical Lift Budget Data", uiOutput("bottom_line"))
)),

# --------------------------------------------------------------------------------
# footer 

hr(),
hr(),
HTML(
  paste0(
    "<div align = 'left'>",
    "<br/>",
    "<h5> &nbsp &nbsp For more on our work, contact 
    <a href='mailto:gcoll@csis.org?subject=Build%20your%20own%20Navy'>
    Gabriel Coll</a>. This is an ongoing project, and we are designing and 
    building a new force structure app, so we welcome any recommendations.</h4>",
    "<h5> &nbsp &nbsp Learn more about our 
    <a href=https://docs.google.com/spreadsheets/d/12ZmNOJ34rEXkNrF7SflJbZZNpVf87Dq_mW2S1TWEJlA/edit#gid=1166404147>
    numbers and sources</a> here, and find our apps on 
    <a href=https://defense360.csis.org>Defense360</a>.</h4>",
    "<br/>"
  )
),

hr(),

fluidRow(
  tags$body(tags$style(HTML(
    ".col-sm-6 {border: 4}"
  ))),
  
  tags$style(
    ".selectize-control {
    color: #554449;
    font-size: 14px;
    font-style: normal;
    background-color: #EDECEB;
    border-color: #C76363;
    border-style: solid;
    padding: 10px;
    border-width: 6px;
    border-top: 6px #63c5b8;
    border-bottom: 6px #63c5b8;
    border-right: 6px #63c5b8;
    #border-left: 6px #63c5b8;
    border-radius: 5px
    }"
    ),
  
  tags$style(
    HTML(
      ".popover-content {color: white; font-family: 'Open Sans',  sans-serif};"
    )
  ),
  
  tags$style(HTML(".arrow {visibility: hidden};")),
  tags$style(HTML(".tooltip-arrow {visibility: hidden};")),
  tags$style(
    HTML(
      ".popover.right > .arrow:after {visibility: hidden;
      border-right-color: white;
      border-color: white;
      };"
    )
    ),
  
  tags$style(HTML(".well {color: #554449}")),
  tags$style(
    HTML(
      "img {
      padding:1px;
      #border:1px solid #021a40;
      #background-color:#ff0;
      height: 'auto';
      max-width: '100%';
      border-radius: 5px
      }"
)
    ),

tags$style(HTML(
  ".img .shiny-image-output {border: 4px green}"
)),

tags$style(HTML(
  ".img .shiny-image-output {border-color: 4px purple}"
))

    ),

br(),
br(),
br()
  )
)

# ================================================================================
