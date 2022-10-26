## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(patchwork)
library(scales)
#library(RCurl)

# Sidebar of the program
sidebar <- dashboardSidebar(
  width=150,
  sidebarMenu(
    # Where main body resides
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # user inputs to change dashboard chart
    checkboxGroupInput(inputId = "RSPFloor",label = "RSP Floors", 
                       choiceNames=c("RSP1","RSP2","RSP3","RSP4"),
                       choiceValues=c(1,2,3,4), selected = c(1,2,3,4)),
    checkboxGroupInput(inputId = "Side", label = "Floor Side",
                       choiceNames = c("North","West","South"),
                       choiceValues = c("North","West","South"),
                       selected = c("North","West","South")),
    checkboxGroupInput(inputId = "UnitSize", label = "Unit Size",
                       choiceNames = c("Small","Medium","Large","Total"),
                       choiceValues = c("Small","Medium","Large","Total"),
                       selected = c("Small","Medium","Large","Total"))
    )
  )

# composes of boxes to showcase datasets
body <- dashboardBody(
  # css if ever want to change the look and feel of the program
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  # stow total count plot
  box(
    title = "Units Stowed", background = "black", solidHeader = TRUE, width = '100%',
    plotOutput("stow_count_plot")
  ),
  # small percentage plot over number of stowers
  box(
    title = "Floor Side Small Distribution / Stower Count", background = "olive", solidHeader = TRUE, 
    plotOutput("percent_plot")
  ),
  # waterspider distribution
  box(
    title = "WaterSpider Small Distribution", background = "blue", solidHeader = TRUE,
    plotOutput("ws_plot")
  )
)

# user interface components
ui <- dashboardPage(
  dashboardHeader(title = "YYZ4 Stow Inbound"),
  sidebar,
  body,
  skin="purple"
)

server <- function(input, output) {
  set.seed(29)
  side <- c("West","South","East","North")
  stp_ib <- read.csv("./STP_Inbound.csv")
  #stp_ib <- read.csv(text = getURL("https://www.google.com"))
  stp_ib <- stp_ib %>%
    drop_na() %>%
    mutate(FloorSide = side[strtoi(substr(Station,start = 2,stop = 2))])
  percent_df <- stp_ib %>%
    group_by(Floor,FloorSide) %>%
    summarize(small_stowed = sum(Units[Size=="Small"]),
              med_stowed = sum(Units[Size=="Medium"]),
              large_stowed = sum(Units[Size=="Large"]),
              stower_count = n_distinct(Login)) %>%
    mutate(total_stowed = small_stowed + med_stowed+large_stowed,
           small_percent = small_stowed/total_stowed,
           med_percent = med_stowed/total_stowed,
           large_percent = large_stowed/total_stowed)
  
  
  
  output$stow_count_plot <- renderPlot({
    stp_ib %>%
      filter(Floor %in% input$RSPFloor,
             Size %in% input$UnitSize,
             FloorSide %in% input$Side) %>%
      ggplot(aes(x=Floor,
                 y=Units,
                 fill=Size)
      ) +
      geom_bar(stat="identity",
               position = "dodge") +
      scale_fill_brewer(palette="Spectral",
                        name="Size Stowed"
      ) +
      theme_excel() +
      made_theme +
      labs(title = "Inbound Stow Distribution",
           subtitle = "YYZ4 RSP Stow Distribution",
           y = "Units Stowed"
      )
  })
  
  
  output$percent_plot <- renderPlot({
    percent_df %>%
    filter(Floor %in% input$RSPFloor,
           FloorSide %in% input$Side) %>%
    ggplot(aes(x = Floor, 
               y = small_percent,
                label = paste(stower_count," Stowers"))) +
    geom_bar(stat="identity",
             aes(fill=small_percent)) +
    geom_text(position = position_dodge(width = 0.5)) +
    scale_fill_gradientn(
      colours = c("darkred","red","green","darkgreen"),
      limits=c(0,1),
      name="Percentage") +
    labs(title = "Small Units Percentage Stowed Only",
         y=NULL,
         subtitle="Make everyone happy green") +
    theme_excel() +
    facet_wrap(~FloorSide)
  })
  
  
  output$ws_plot <- renderPlot({
    stp_ib %>%
      filter(Size=="Small",
             Floor %in% input$RSPFloor,
             FloorSide %in% input$Side) %>%
      ggplot(aes(x=FloorSide,
                 y=Units,
                 fill=Size)
      ) +
      geom_boxplot(outlier.colour="red",
                   outlier.shape=NA,
                   outlier.size=2,
                   notch=FALSE
      ) +
      geom_jitter(shape=16, position=position_jitter(0)) +
      stat_summary(aes(label = round(stat(y), 1)),
                   geom = "text", 
                   fun.y = function(y) { o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o },
                   hjust = -1
      ) +
      stat_summary(fun=mean,
                   geom="point",
                   shape=23,
                   size=4) +
      scale_fill_brewer(palette="Paired",
                        name="Small Stowed Mean"
      ) +
      theme_excel() +
      made_theme +
      labs(title = "Floor Side WS Distribution",
           y = "Small STP Distribution"
      ) +
      facet_wrap(~Floor)
    })
  }

shinyApp(ui, server)