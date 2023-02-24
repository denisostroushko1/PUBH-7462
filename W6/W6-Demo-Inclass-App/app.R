#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(kableExtra)
library(eply)
library(data.table)
library(plotly)
library(data.table)
library(DT)
library(processx)
library(shiny)

#################################################################
# read in the data 
#################################################################
source("/Users/denisostroushko/Desktop/UofM MS/MS 2023 - 1 Spring/PUBH 7462/Cloned Assignements Repos/Assignment3/A3 Only Functions.R")
atus_raw <- read_csv("/Users/denisostroushko/Desktop/UofM MS/MS 2023 - 1 Spring/PUBH 7462/Cloned Assignements Repos/Assignment3/data/atus_raw.csv", 
                     col_types = list(CASEID = col_character()))
atus_raw <- atus_raw %>% arrange()
atus_raw$ID <- as.numeric(rownames(atus_raw))
# length(unique(atus_raw$CASEID)) - again, we have 1 unique ID for all rows, so we will create a custon ID for each row
dict <- read.csv("/Users/denisostroushko/Desktop/UofM MS/MS 2023 - 1 Spring/PUBH 7462/Cloned Assignements Repos/Assignment3/data/atus_dictionary.csv")
  
atus <- 
  label_all_from_dict(
    data = atus_raw, 
    dict = dict
    )

#################################################################
# functions for my shiny app 

makePlot <- function(data, act_var, by_var) {
  data %>% ggplot(aes(x = !!sym(act_var) , fill = !!sym(by_var), color = !!sym(by_var))) +
    geom_density(alpha = 0.3) +
    xlab(paste("Minutes of", act_var) ) +
    ylab(NULL) + 
    theme_minimal()
}


#################################################################


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
    titlePanel("ATUS Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "act_var",
                label = "Activity variable",
                choices = atus %>% select(starts_with("ACT_")) %>% names()),
            
            selectInput(inputId = "by_var",
                label = "Grouping variable",
                choices = dict %>% filter(type == "categorical") %>% distinct(variable))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  makePlot <- function(data, act_var, by_var) {
    data %>% ggplot(aes(x = !!sym(act_var) , 
                        fill = !!sym(by_var), 
                        color = !!sym(by_var))) +
      geom_density(alpha = 0.3) +
      xlab(paste("Minutes of", act_var) ) +
      ylab(NULL) +
      theme_minimal()
  }
  
  output$distPlot <- renderPlot({
    atus %>% makePlot(input$act_var, input$by_var)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
