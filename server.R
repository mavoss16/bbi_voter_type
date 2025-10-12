
library(shiny)

library(readr)
library(dplyr)
library(stringr)

library(tidygeocoder)
library(sf)


house_boundaries <- st_read("data/district_boundaries/Plan2_House.shp")
district_data <- read_rds("data/voter_district_data.rds")
voter_type_text <- read_csv("data/voter_type_text.csv")

legislators <- read_csv("data/house_legislators.csv") |>
  arrange(district_num) |>
  mutate(name_num = paste0(name, " (", district_num, ")"))


percent <- function(x){
  (x * 100) |> round(digits = 1) |> paste0("%")
}

# Define server logic required to draw a histogram
function(input, output, session) {

  search_district <- reactive({
    if(input$address_input != ""){
      district_num <- geo(address = input$address_input, method = "osm") |>
        st_as_sf(coords = c("long", "lat"), crs = 4269, na.fail = FALSE) |>
        st_join(house_boundaries) |>
        pull(DISTRICT_N)
      # print(district_num)
      return(district_num)
      
    } else{
      return(NA)
    }
    
  }) |>
    bindEvent(input$address_input_search)
  
  observe({
    updateSearchInput(
      session = getDefaultReactiveDomain(),
      inputId = "address_input", value = ""
    )
  }) |>
    bindEvent(input$legislator_input)
  
  observe({
    if(is.na(search_district())){
      show(id = "search_warning")
    } else{
      hide(id = "search_warning")
      new_legislator <- legislators |>
        filter(district_num == search_district()) |>
        pull(name_num)
      updatePickerInput(inputId = "legislator_input", selected = new_legislator)
    }
  }) |>
    bindEvent(search_district())
  
  selected_district <- reactive({
    str_extract(input$legislator_input, "\\d+")
  })
  
  # voter_type <- reactive({
  #   house_voters |>
  #     filter(
  #       district_num == selected_district(),
  #       party == input$party_input
  #     )
  # })
  # 
  # election <- reactive({
  #   house_results |>
  #     filter(district_num == selected_district())
  # })
  
  selected_data <- reactive({
    district_data |>
      filter(
        district_num == selected_district(),
        party == input$party_input
      )
  })
  
  output$voter_class <- renderText({
    selected_data()$voter_classification
  })
  
  output$voter_class_ui <- renderUI({
    
    page_fluid(
      tags$h3(paste0("Your voter type is ", selected_data()$voter_classification)),
      tags$div(paste0("In your Iowa House district's November election, ", selected_data()$first_vote_cand, " won with a margin of ", percent(selected_data()$margin), ".")),
      br(),
      tags$div(
        voter_type_text |> filter(voter_classification == selected_data()$voter_classification) |> pull(explanation_text)
      )
    )
  })
  
  
  output$grade <- renderText({
    selected_data()$grade
  })
  
  output$general_margin <- renderText({
    percent(selected_data()$gen_margin)
  })
  
  output$primary_margin <- renderText({
    selected_data()$prim_margin_description
  })

  session$onSessionEnded(function() { stopApp() }) 
}
