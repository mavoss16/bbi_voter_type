
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

# General election margin dial, from "Close race" to "Foregone conclusion".
# The four zones match the margin categories rather than a linear 0-100%
# scale, since half of all margins are over 35% and would pile up at the far
# end. Uncontested races pin the needle at the very end.
dial_zones <- tibble(
  label = c("Close race", "Competitive", "Leaning", "Foregone conclusion"),
  from = c(0, 0.05, 0.10, 0.20),
  to = c(0.05, 0.10, 0.20, 0.60),
  # Purple ("purple district") for competitive races, draining to gray for
  # foregone ones; avoids the red and blue party colors
  color = c("#3C3489", "#7F77DD", "#B4B2A9", "#5F5E5A")
)

# Position (0 = far left, 1 = far right) of a margin on the dial
dial_position <- function(margin, uncontested){
  if(uncontested) return(1)
  zone <- findInterval(margin, dial_zones$from)
  within <- (margin - dial_zones$from[zone]) / (dial_zones$to[zone] - dial_zones$from[zone])
  # Contested races stop just short of the end, which is kept for uncontested ones
  min((zone - 1 + min(within, 1)) / nrow(dial_zones), 0.97)
}

margin_dial <- function(margin, uncontested){
  cx <- 150; cy <- 160; r <- 110
  point <- function(position, radius = r){
    angle <- pi * (1 - position)
    c(x = cx + radius * cos(angle), y = cy - radius * sin(angle))
  }
  arc <- function(from, to, color){
    a <- point(from); b <- point(to)
    sprintf(
      '<path d="M%.1f %.1f A%d %d 0 0 1 %.1f %.1f" stroke="%s" stroke-width="22" fill="none"/>',
      a["x"], a["y"], r, r, b["x"], b["y"], color
    )
  }
  n <- nrow(dial_zones)
  arcs <- mapply(arc, (seq_len(n) - 1) / n, seq_len(n) / n, dial_zones$color) |>
    paste(collapse = "")
  needle <- point(dial_position(margin, uncontested), radius = 88)

  zone_label <- if(uncontested) "Uncontested" else
    dial_zones$label[findInterval(margin, dial_zones$from)]
  value <- if(uncontested) "No opponent" else percent(margin)

  div(
    class = "margin-dial",
    HTML(sprintf(
      paste0(
        '<svg viewBox="0 0 300 200" role="img" aria-label="General election margin: %s, %s">',
        '%s',
        # The 10% line that separates competitive races from the rest
        '<line x1="150" y1="28" x2="150" y2="74" class="dial-threshold"/>',
        '<text x="150" y="20" text-anchor="middle" class="dial-threshold-label">10%% competitive line</text>',
        '<line x1="%d" y1="%d" x2="%.1f" y2="%.1f" class="dial-needle"/>',
        '<circle cx="%d" cy="%d" r="7" class="dial-hub"/>',
        '<text x="20" y="194" class="dial-end-label">Close race</text>',
        '<text x="280" y="194" text-anchor="end" class="dial-end-label">Foregone conclusion</text>',
        '</svg>'
      ),
      value, zone_label, arcs, cx, cy, needle["x"], needle["y"], cx, cy
    )),
    div(class = "dial-value", value),
    div(class = "dial-zone", zone_label)
  )
}

# Geocode an address and return its Iowa House district number (NA if not found).
# geo() can't combine a single-line address with state = "IA", so add
# Iowa to the query instead. Nominatim handles a duplicated state fine
# (e.g. "..., IA 50319, Iowa"), so it's appended unconditionally.
lookup_district <- function(address){
  geo(address = paste0(address, ", Iowa"), method = "osm") |>
    st_as_sf(coords = c("long", "lat"), crs = 4269, na.fail = FALSE) |>
    st_join(house_boundaries) |>
    pull(DISTRICT_N)
}

# Map friendly URL values for ?party= onto the party picker's choice values
party_lookup <- c(
  "rep" = "Rep", "republican" = "Rep",
  "dem" = "Dem", "democrat" = "Dem", "democratic" = "Dem",
  "lib" = "Lib", "libertarian" = "Lib",
  "other" = "Other",
  "none" = "No Party", "np" = "No Party", "no party" = "No Party", "noparty" = "No Party"
)

# Display names for the party values, for the results headline
party_names <- c(
  "Rep" = "Republican", "Dem" = "Democrat", "Lib" = "Libertarian",
  "No Party" = "No party", "Other" = "Other party"
)

party_primary_names <- c("Rep" = "Republican", "Dem" = "Democratic")
voter_group_names <- c(
  "Lib" = "Libertarian voters", "No Party" = "no-party voters", "Other" = "other-party voters"
)

# The voter's two chances to make a difference, the primary and then the
# general election, each with whether it gave them a meaningful vote. Together
# they explain how the voter type was reached.
primary_step <- function(data){
  winner_primary <- party_primary_names[data$gen_winning_party]
  contested <- !data$prim_margin_description %in% c("No Primary", "Uncontested")

  switch(data$voter_classification,
    "Meaningful (General)" = ,
    "Meaningful (Primary)" = if(contested){
      list(pass = TRUE, title = "You had more than one choice",
           detail = paste0("Your party's primary was contested, decided by ", data$prim_margin_description, "."))
    } else if(data$prim_margin_description == "No Primary"){
      list(pass = FALSE, title = "No primary choice",
           detail = "No one ran in your party's primary.")
    } else{
      list(pass = FALSE, title = "No primary choice",
           detail = "No one ran against your party's candidate.")
    },
    "Unneeded" = list(pass = FALSE, title = "No primary choice",
                      detail = "No one ran against your party's candidate."),
    "Outnumbered" = list(pass = FALSE, title = "The deciding primary was closed to you",
                         detail = paste0("The race was decided in the ", winner_primary, " primary, which you can't vote in.")),
    "Excluded" = list(pass = FALSE, title = "The deciding primary was closed to you",
                      detail = paste0("The race was decided in the ", winner_primary, " primary, which is closed to ",
                                      voter_group_names[data$party], ".")),
    "None-of-the-Above" = list(pass = FALSE, title = "Primaries closed to you",
                               detail = paste0("Iowa's two major party primaries are closed to ",
                                               voter_group_names[data$party], "."))
  )
}

general_step <- function(data){
  if(data$gen_margin_category == "Uncontested"){
    list(pass = FALSE, title = "No race", detail = paste0(data$first_vote_cand, " ran unopposed."))
  } else if(data$gen_competitive){
    list(pass = TRUE, title = "Competitive",
         detail = paste0("Decided by ", percent(data$gen_margin), ", under the 10% line."))
  } else{
    list(pass = FALSE, title = "Not competitive",
         detail = paste0("Decided by ", percent(data$gen_margin), "."))
  }
}

path_card <- function(label, step){
  div(
    class = paste("path-card", if(step$pass) "path-pass" else "path-fail"),
    div(
      class = "path-icon",
      if(step$pass) icon("check") else icon("xmark"),
      span(class = "visually-hidden", if(step$pass) "Meaningful vote" else "No meaningful vote")
    ),
    div(
      div(class = "path-label", label),
      div(class = "path-title", step$title),
      div(class = "path-detail", step$detail)
    )
  )
}

# Define server logic required to draw a histogram
function(input, output, session) {

  # The representative and party the results are shown for. Only changes when
  # Submit is pressed (or when a URL fills in both), so editing the inputs
  # doesn't change the results until then.
  submitted <- reactiveVal(NULL)

  observe({
    submitted(list(legislator = input$legislator_input, party = input$party_input))
  }) |>
    bindEvent(input$submit)

  # Submit is only clickable once both a representative and a party are chosen;
  # until then a hint under the button says what's missing
  observe({
    ready <- isTruthy(input$legislator_input) && isTruthy(input$party_input)
    shinyjs::toggleState(id = "submit", condition = ready)
    shinyjs::toggle(id = "submit_hint", condition = !ready)
  })

  search_district <- reactive({
    if(input$address_input != ""){
      return(lookup_district(input$address_input))
    } else{
      return(NA)
    }
    
  }) |>
    bindEvent(input$address_input_search)
  
  # Set when the representative is chosen from an address search (typed or from
  # the URL), so the address box keeps showing that address instead of being
  # cleared below
  keep_address <- FALSE

  # The representative found by the last successful address search, shown as a
  # confirmation under the address box
  search_found <- reactiveVal(NULL)

  output$search_result <- renderUI({
    req(search_found())
    district <- str_extract(search_found(), "\\d+")
    name <- str_remove(search_found(), " \\(\\d+\\)$")
    tags$p(
      class = "search-found",
      icon("circle-check"), paste0("Found District ", district, ": ", name)
    )
  })

  # Choosing a representative from the list replaces any address search
  observe({
    if(keep_address){
      keep_address <<- FALSE
      return()
    }
    search_found(NULL)
    updateSearchInput(
      session = getDefaultReactiveDomain(),
      inputId = "address_input", value = ""
    )
  }) |>
    bindEvent(input$legislator_input)

  # Pre-populate inputs from URL parameters, e.g. ?district=45&party=Dem or
  # ?address=1007 E Grand Ave Des Moines&party=Rep. Without parameters the
  # page loads as normal.
  observe({
    query <- parseQueryString(session$clientData$url_search)

    district <- query$district
    new_legislator <- NULL
    party <- NA
    if(is.null(district) && isTruthy(query$address)){
      district <- lookup_district(query$address)
      updateSearchInput(session, "address_input", value = query$address)
      if(is.na(district)){
        show(id = "search_warning")
      } else{
        keep_address <<- TRUE
      }
    }
    if(!is.null(district) && !is.na(district)){
      new_legislator <- legislators |>
        filter(district_num == suppressWarnings(as.integer(district))) |>
        pull(name_num)
      if(length(new_legislator) == 1){
        updatePickerInput(session, "legislator_input", selected = new_legislator)
        if(keep_address) search_found(new_legislator)
      } else{
        keep_address <<- FALSE
      }
    }

    if(!is.null(query$party)){
      party <- party_lookup[tolower(query$party)]
      if(!is.na(party)){
        updateRadioGroupButtons(session, "party_input", selected = unname(party))
      }
    }

    # Show results right away when the URL gives both a district and a party
    if(length(new_legislator) == 1 && !is.na(party)){
      submitted(list(legislator = new_legislator, party = unname(party)))
    }
  }) |>
    bindEvent(session$clientData$url_search, once = TRUE)
  
  observe({
    if(is.na(search_district())){
      search_found(NULL)
      show(id = "search_warning")
    } else{
      hide(id = "search_warning")
      new_legislator <- legislators |>
        filter(district_num == search_district()) |>
        pull(name_num)
      # Only a changed selection triggers the clearing observer above
      keep_address <<- !identical(new_legislator, isolate(input$legislator_input))
      search_found(new_legislator)
      updatePickerInput(inputId = "legislator_input", selected = new_legislator)
    }
  }) |>
    bindEvent(search_district())
  
  selected_district <- reactive({
    req(submitted()$legislator)
    str_extract(submitted()$legislator, "\\d+")
  })

  # The inputs and the results take turns filling the app's fixed-size frame.
  # Submit (or a URL with a district and a party) swaps in the results;
  # "Change my answers" swaps the inputs back.
  showing_results <- reactiveVal(FALSE)

  observe(showing_results(TRUE)) |> bindEvent(input$submit)
  observe(showing_results(TRUE)) |> bindEvent(submitted())
  observe(showing_results(FALSE)) |> bindEvent(input$change_answers)

  observe({
    show_results <- showing_results() &&
      isTruthy(submitted()$legislator) && isTruthy(submitted()$party)
    shinyjs::toggle(id = "results", condition = show_results)
    shinyjs::toggle(id = "inputs", condition = !show_results)
    shinyjs::runjs("document.getElementById('app_frame').scrollTop = 0;")
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
    req(submitted()$party)
    district_data |>
      filter(
        district_num == selected_district(),
        party == submitted()$party
      )
  })
  
  output$voter_class <- renderText({
    selected_data()$voter_classification
  })
  
  # Headline: the grade badge next to the voter type and a one-line summary,
  # followed by what happened in the district and the full explanation
  output$voter_class_ui <- renderUI({
    data <- selected_data()
    type_text <- voter_type_text |> filter(voter_classification == data$voter_classification)
    grade_class <- paste0("grade-", tolower(sub("+", "-plus", data$grade, fixed = TRUE)))

    election_text <- if(data$gen_margin_category == "Uncontested"){
      paste0("In November 2024, ", data$first_vote_cand, " ran unopposed for your Iowa House seat.")
    } else{
      paste0(
        "In your Iowa House district's November 2024 election, ", data$first_vote_cand,
        " won with a margin of ", percent(data$margin), "."
      )
    }

    div(
      class = "results-summary",
      div(
        class = "results-headline",
        div(
          class = paste("grade-badge", grade_class),
          role = "img", `aria-label` = paste("Voter power grade:", data$grade),
          data$grade
        ),
        div(
          div(
            class = "results-context",
            paste0("House District ", data$district_num, " · ", party_names[data$party])
          ),
          tags$h3(
            "Your voter type is ",
            span(class = paste("voter-type", grade_class), data$voter_classification)
          ),
          div(class = "results-tagline", type_text$summary_text)
        )
      ),
      tags$p(election_text),
      tags$p(class = "results-explanation", type_text$explanation_text)
    )
  })

  output$margin_dial <- renderUI({
    margin_dial(
      selected_data()$gen_margin,
      uncontested = selected_data()$gen_margin_category == "Uncontested"
    )
  })
  
  output$vote_path <- renderUI({
    tagList(
      div(class = "path-heading", "Your two chances to make a difference"),
      path_card("Primary · June 2024", primary_step(selected_data())),
      div(class = "path-arrow", icon("arrow-down")),
      path_card("General · November 2024", general_step(selected_data()))
    )
  })

  # Render the results while they're still hidden, so they arrive in the same
  # server round trip that swaps them in. Otherwise Shiny waits for them to be
  # shown first and fills them in on a second trip, briefly showing them empty.
  for(id in c("voter_class_ui", "margin_dial", "vote_path")){
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

}
