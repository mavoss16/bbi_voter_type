
library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Builds data/voter_district_data.rds for the app from the better_ballot_iowa
# pipeline (src/00_config.R through src/07_hd_elections.R), which should be
# checked out next to this repository. Run from this repository's root.

upstream <- "../better_ballot_iowa"
source(file.path(upstream, "src", "00_config.R"), chdir = FALSE)

# One row per year x chamber x district x party
voters <- read_rds(file.path(upstream, "data", "voter_classification.rds"))

# General election winner and runner-up for each race, every year and chamber
results <- unique(voters$year) |>
  map(\(election_year){
    read_rds(file.path(upstream, leg_results_path("general", election_year, "wide"))) |>
      ungroup() |>
      filter(race_level %in% chambers) |>
      mutate(
        year = election_year,
        chamber = names(chambers)[match(race_level, chambers)]
      )
  }) |>
  bind_rows()


percent <- scales::label_percent(accuracy = 0.1)


combined_data <- voters |>
  left_join(results, by = c("year", "chamber", "district_num"))

grades <- c("A+", "A", "B", "C", "D", "F")

combined_data <- combined_data |>
  mutate(
    grade_index = case_when(
      gen_margin <= 0.05 ~ 2,
      gen_margin <= 0.1 ~ 3,
      gen_margin <= 0.15 ~ 4,
      gen_margin <= 0.2 ~ 5,
      gen_margin > 0.2 ~ 6
    ),
    grade_index = ifelse(
        (prim_margin_category %in% c("Uncontested", "No Candidates")) | is.na(prim_margin_category),
        yes = grade_index,
        no = grade_index - 1
      ),
    grade = grades[grade_index]
  ) |>
  mutate(
    prim_margin_description = case_when(
      is.na(prim_margin) ~ "No Primary",
      prim_margin_category %in% c("Uncontested", "No Candidates") ~ prim_margin_category,
      TRUE ~ prim_margin |> percent() |> as.character()
    )
  )



write_rds(combined_data, "data/voter_district_data.rds")




library(ggplot2)

combined_data |>
  filter(party == "No Party", year == max(year), chamber == "House") |>
  count(grade) |>
  ggplot(aes(x = grade, y = n, label = n)) +
  geom_col(fill = "#e1b6fa") +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(title = "Iowa House District Competitiveness Grades", y = "Count", x = "Grade") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 18, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )
