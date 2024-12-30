library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

allData <- read.csv("behaviordata.csv")
# Remove missing data
allData <- allData |> filter(Data_Value_Footnote == "")

states <- unique(allData$LocationDesc)
qids <- c("Q036", "Q037", "Q044", "Q046", "Q043", "Q045", "Q047", "Q018", "Q019")

choices_questions <- c("Have obesity" = "Q036",
                       "Have an overweight classification" = "Q037",
                       "Do at least 150 minutes of moderate intensity cardio or equivalent and 2 or more days of strength training per week" = "Q044",
                       "Strength train 2 or more days a week" = "Q046",
                       "Do at least 150 minutes of moderate intesity cardio or equivalent per week" = "Q043",
                       "Do at least 300 minutes of moderate intesity cardio or equivalent per week" = "Q045",
                       "Do not exercise" = "Q047",
                       "Report consuming fruit less than one time daily" = "Q018",
                       "Report consuming vegetables less than one time daily" = "Q019"
)

questions_short <- c("Obese" = "Q036", 
                     "Overweight" = "Q037", 
                     "Cardio + strength" = "Q044", 
                     "Strength" = "Q046", 
                     ">150 cardio" = "Q043", 
                     ">300 cardio" = "Q045", 
                     "No exercise" = "Q047", 
                     "<1 Fruit" = "Q018", 
                     "<1 Vegetable" = "Q019")

graph_params <- tabsetPanel(
  id = "graphs",
  type = "hidden",
  # Comparing variables against time
  tabPanel(
    title = "rawdata",
    "Select any number of variables to view over time",
    checkboxGroupInput(
      inputId = "questionRaw",
      label = "Percent of adults age 18 or older who:",
      choices = choices_questions
    ),
    hr(style = "border-top: 1px solid #000000;"),
    selectInput(
      inputId = "state",
      label = "State(s) (or 'All'):",
      choices = c(states, 'All'),
      multiple = TRUE,
      selected = "All"
    )
  ),
  # Comparing means of variables
  tabPanel(
    title = "means",
    "Select two variables and a grouping variable to view correlation",
    selectizeInput(
      inputId = "meansX",
      label = "X - Percent of adults age 18 or older who:",
      choices = choices_questions
    ),
    selectInput(
      inputId = "meansY",
      label = "Y - Percent of adults age 18 or older who:",
      choices = choices_questions
    ),
    selectInput(
      inputId = "group",
      label = "Select a variable to group by:",
      choices = c("State" = "LocationDesc",
                  "Age" = "Age.years.",
                  "Education",
                  "Gender",
                  "Income",
                  "Ethnicity" = "Race.Ethnicity"
                  )
    )
  )
)

ui <- page_sidebar(
  h1("Trends of Obesity and General Health, USA from 2011 to 2023"),
  sidebar = sidebar(
    em("This page allows you to compare trends of different health statistics in the US, 
    as sampled by the CDC across hundreds of thousands of surveys ranging from 2011 to 2023."),
    # Input questions of interest
    radioButtons(
      inputId = "graphType",
      label = "Choose what type of graph to view:",
      choices = c("Multiple variables over time" = "rawdata", 
                  "Correlate two variables" = "means")
    ),
    hr(style = "border-top: 1px solid #000000; margin-top: 0em; margin-bottom: 0em;"),
    graph_params,
    checkboxInput(
      inputId = "regression",
      label = "Show linear regression?",
      value = FALSE
    ), 
    conditionalPanel(
      condition = "input.regression == true && input.graphType == 'means'",
      checkboxInput(
        inputId = "regressionStats",
        label = "Show regression stats?",
        value = FALSE
      )
    ),
    p("Data provided by U.S. Department of Health and Human Services under Open Database License (ODbL) at", 
    a(href="https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system",
      "https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system"), style="font-size:10px")
  ),
  card(plotOutput(outputId = "graph"))
)

server <- function(input, output) {
  # Update which graph type according to user selection
  observeEvent(input$graphType, {
    updateTabsetPanel(inputId = "graphs", selected=input$graphType)
  })
  
  selectedData <- reactive({
    if(input$graphType == "rawdata" & length(input$questionRaw) == 0) {
      allData # Default case, may be better choice for this
    } else {
      # If rawdata: get variables of interest
      if(input$graphType == "rawdata") {
        if(input$state[1] == "All") {
          allData |> filter(QuestionID %in% input$questionRaw)
        } else {
          allData |> 
            filter(QuestionID %in% input$questionRaw) |> 
            filter(LocationDesc == input$state)
        }
      } # If means: get variables of interest, group, then average
        else if(input$graphType == "means") {
          allData |> 
            filter(QuestionID %in% c(input$meansX, input$meansY)) |> 
            group_by_at(c("QuestionID", "YearStart", input$group)) |>
            summarize(
              Data_Value = mean(Data_Value)
            ) |>
            pivot_wider(
              names_from = QuestionID,
              values_from = Data_Value
            )
        }
      }
  })
  
  aesMap <- reactive({
    if(input$graphType == 'rawdata') {
      aes(x=YearStart, y=Data_Value, color=QuestionID)
    } else if(input$graphType == 'means') {
      aes(x=.data[[input$meansX]], y=.data[[input$meansY]])
    }
  })
  
  output$graph <-renderPlot({
    graph <- ggplot(data=selectedData(), aesMap()) + 
      geom_point()
    if(input$graphType == "rawdata") {
      graph <- graph +
        ylab("Percentage") + 
        scale_color_discrete(name = "Question",
                             breaks = qids,
                             labels = c("Obese", "Overweight", "Cardio + strength", 
                                        "Strength", ">150 cardio", ">300 cardio", 
                                        "No exercise", "<1 Fruit", "<1 Vegetable"))
      if(input$regression) {
        graph <- graph + geom_smooth(method="lm")
      }
    }
    if(input$graphType == "means") {
      graph <- graph + 
        xlab(names(questions_short)[which(qids == input$meansX)]) +
        ylab(names(questions_short)[which(qids == input$meansY)])
      if(input$regression) {
        graph <- graph + geom_smooth(method="lm")
        if(input$regressionStats) {
          graph <- graph + stat_regline_equation(
            aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
            label.y.npc = 1, label.x.npc = .6)
        }
      }
    }
    graph
  })
}

shinyApp(ui = ui, server = server)
