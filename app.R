#
# Where do people end up working after their qualification

library(shiny)
library(tidyverse)
library(data.table)
library(grattantheme)

# Get data --------------------------------------------------------------------

dat <- read_rds("data/dat.rds")

foes <- dat %>% 
        filter(foe_census != "not_used", 
               foe_census != "Not applicable") %>% 
        pull(foe_census) %>% 
        unique()

source("R/plot_dat.R")
source("R/get_title.R")


# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme = "cosmo.css",
   
   # Application title
   titlePanel("Where do people end up working after their qualification?"),
   
   p(paste("The chart below uses ABS Census data to show what jobs 18-40 year-olds",
           "are working in for a given qualification, field and gender.")),
   
   p(paste("Note that small numbers (less than 5) have been removed from",
           "the analysis because they are unreliable.")),
   
   # Select
   fluidRow(
     column(2,
        selectInput("foe",
                    "Field of education",
                    choices = foes, 
                    selected = "Commerce")),
      column(2,
        selectInput("gender",
                    "Gender",
                    choices = c("Female", 
                                "Male",
                                "Compare genders"), 
                    selected = "Female")),
     column(2,  
        selectInput("qual",
                    "Qualification level",
                    choices = c("Postgrad", 
                                "Bachelor",
                                "Cert III/IV",
                                "Compare qualifications"), 
                    selected = "Bachelor")),
      column(2,        
        selectInput("occ_level",
                    "Occupation detail",
                    choices = list("Broad occupations" = 2, 
                                "Detailed occupations" = 3,
                                "More detailed" = 4), 
                    selected = 3)),
      column(2,  
        sliderInput("occ_number", 
                    "Number of additional occupations to show:",
                    min = 1, max = 40, value = 15))
        
      ),
   
   h2(textOutput("theTitle")),
   plotOutput("plotted", height = "500px")
      
   
  )


# Server -----------------------------------------------------------------------
server <- function(input, output) {
  
  output$plotted <- renderPlot({ 
  
    plot_dat(foe = input$foe,
             gender = input$gender,
             qual = input$qual,
             occ_level = input$occ_level,
             topn = input$occ_number,
             getData = TRUE,
             useTitle = FALSE)
  })
  
  output$theTitle <- renderText({ 
    get_title(foe = input$foe,
              gender = input$gender,
              qual = input$qual,
              occ_level = input$occ_level,
              topn = input$occ_number)
  })
  
  
}

# Run --------------------------------------------------
shinyApp(ui = ui, server = server)

