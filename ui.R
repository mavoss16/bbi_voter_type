

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

library(readr)
library(dplyr)

legislators <- read_csv("data/house_legislators.csv") |>
  arrange(district_num) |>
  mutate(name_num = paste0(name, " (", district_num, ")"))

page_fluid(
  includeCSS("www/styles.css"),
  shinyjs::useShinyjs(),
  tags$script(src = "loading.js"),

  # Fixed-height frame so the app keeps a constant size when embedded in an
  # iframe: the inputs and the results take turns filling it, and anything
  # taller than the frame scrolls inside it
  div(
    id = "app_frame",
    div(
      id = "inputs",
      class = "input-steps",

      # Covers the inputs while an address is being geocoded, shown by www/loading.js
      div(
        id = "inputs_loading",
        div(class = "spinner"),
        tags$p("Looking up your address...")
      ),

      # Step 1: find the voter's district, by address (most people) or by name
      div(
        class = "input-step",
        h4(span(class = "step-num", "1"), "Find your district"),
        searchInput(
          inputId = "address_input",
          label = "Search for your address",
          placeholder = "e.g. 1007 E Grand Ave, Des Moines",
          btnSearch = icon("magnifying-glass"),
          width = "100%"
        ),
        tags$small(class = "input-hint", "Click the search button."),
        tags$p("Address could not be found.", id = "search_warning", class = "search-warning") |> shinyjs::hidden(),
        uiOutput("search_result"),
        div(class = "or-divider", span("or choose from the list")),
        pickerInput(
          inputId = "legislator_input",
          label = "Select your Iowa House representative",
          choices = legislators$name_num,
          width = "100%",
          options = pickerOptions(liveSearch = TRUE, title = "Choose a representative")
        )
      ),

      # Step 2: party registration
      div(
        class = "input-step",
        h4(span(class = "step-num", "2"), "Your party registration"),
        radioGroupButtons(
          inputId = "party_input",
          label = NULL,
          choices = c(
            "No Party", "Republican" = "Rep", "Democratic" = "Dem", "Libertarian" = "Lib", "Other"
          ),
          selected = character(0),
          status = "outline-primary"
        )
      ),

      # Enabled by the server once both a representative and a party are chosen
      div(
        class = "submit-row",
        actionButton("submit", "See my results", class = "btn-primary btn-lg") |> shinyjs::disabled(),
        tags$small(
          id = "submit_hint", class = "input-hint",
          "Choose a representative and a party to see your results."
        )
      )
    ),

    # Loading circle, shown by www/loading.js while results are being looked up
    div(id = "loading", div(class = "spinner")),

    # Results stay hidden until Submit is pressed with a district and a party
    div(
      id = "results",

      actionLink("change_answers", "Change my answers", icon = icon("arrow-left"), class = "change-answers"),

      # textOutput("district_info"),
      # textOutput("voter_class"),
      uiOutput("voter_class_ui"),
      layout_columns(
        col_widths = c(5, 7),
        # Primary, then general: whether each gave the voter a meaningful vote
        div(class = "vote-path", uiOutput("vote_path")),
        card(
          class = "dial-card",
          div(class = "dial-title", "How close was the general election?"),
          uiOutput("margin_dial")
        )
      )# ,
      # br(),
      # layout_columns(
      #   col_widths = c(6.5, 5.5),
      #   max_height = "300px",
      #   fill = FALSE,
      #   tags$img(
      #     src = "competitive_house_races.png", width = "100%",# height = "440px", width = "572px", 
      #     style="display: block; margin-left: auto; margin-right: auto;"),
      #   tags$img(
      #     src = "district_grades.png", width = "100%",# height = "400px", width = "520px", 
      #     style="display: block; margin-left: auto; margin-right: auto;")
      # )
    ) |> shinyjs::hidden()
  )
)
