## Lauren practice dashboard for shiny!
## visualizes Denmark Cancer Data 

## how could you PCA on this data? 

## how could you connect a ML or LR 
## model that predicts survival time on this data?


library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

source("bar_treatment.R")

cancer_data_denmark <- read_csv("data/cancer_data_denmark.csv")

type_theme <- c("Lung" = "darkred", 
                "Leukemia" = "yellow", 
                "Prostate" = "violet", 
                "Skin" = "orange", 
                "Breast" = "pink", 
                "Colon" = "blue")

treatment_theme <- c("Radiation" = "maroon", 
                     "Hormone Therapy" = "lightpink", 
                     "Immunotherapy" = "lightblue",
                     "Chemotherapy" = "darkgreen", 
                     "Surgery" = "lightgreen")

ui <- page_fluid(
  titlePanel("Denmark Cancer Data Dashboard"), 
  data = cancer_data_denmark,
  title = "Denmark Cancer Data Dashboard", 
  
  card_body(
    layout_sidebar(
      sidebar = sidebar(
        title = "Cohort Quick Facts", 
        value_box(
          title = "Patients (n)",
          #value = 3000,
          value = nrow(cancer_data_denmark) - 1, 
          showcase = bsicons::bs_icon("capsule-pill"),
          color = "navy"
        ), 
        value_box(
          title = "Average Age in Years",
          #value = 65.6,
          value = mean(cancer_data_denmark$Age, trim=1),
          showcase = bsicons::bs_icon("clock"),
          color = "lightblue"
        ), 
        value_box(
          title = "Average Tumor Size in cm", 
          #value = 9.3,
          value = mean(cancer_data_denmark$Tumor_Size_cm,trim=3), 
          showcase = bsicons::bs_icon("circle"), 
          color = "darkgreen"
        ),
        value_box(
          title = "Average Survival in Months", 
          #value = 9.3,
          value = mean(cancer_data_denmark$Survival_Months,trim=4), 
          showcase = bsicons::bs_icon("boombox"), 
          color = "darkgreen"
        ),
        value_box(
          title = "Average BMI", 
          #value = 9.3,
          value = mean(cancer_data_denmark$BMI,trim=4), 
          showcase = bsicons::bs_icon("repeat"), 
          color = "darkgreen"
        )
      ),
    card(
      selectInput("type", 
                  label = "Select cancer type of interest:",
                  choices = c("Lung", 
                              "Leukemia", 
                              "Prostate", 
                              "Skin", 
                              "Breast", 
                              "Colon"),
                  selected = "Lung"
      ),
      card(
        title = "Treatments Given",
        plotOutput("bar_treatment")
      ),
      
      card(
        title = "Cancer Stage of Selected Cancer Type", 
        plotOutput("bar_stage")
      )
    )
  ),

  nav_panel("Survival Calculator", #how can you make this a navigation panel?
      title = "Survival Calculator"
    )
  )
)

server <- function(input, output) {
  
  output$bar_stage <- renderPlot({
    summary <- cancer_data_denmark %>% filter(Cancer_Type == input$type) %>% count(Stage) 
    ggplot(summary, aes(x = reorder(Stage, n), y = n), fill = Stage) + 
      geom_col() +
      labs(
        title = "# of Cancer Stage Diagnoses for Selected Cancer Type", 
        x = "Cancer Stage", 
        y = "Count")
  })
  
  
  output$bar_treatment <- renderPlot({
    summary <- cancer_data_denmark %>% filter(Cancer_Type == input$type) %>% count(Treatment_Type) 
    ggplot(summary, aes(x = reorder(Treatment_Type, n), y = n), fill = Treatment_Type) + 
      geom_col() +
      coord_flip() + 
      labs(
        title = "Treatments for Selected Cancer Type", 
        x = "Treatment", 
        y = "Count") #how can you add your color theme to this barplot?
  })
  
}
  
shinyApp(ui = ui, server = server)