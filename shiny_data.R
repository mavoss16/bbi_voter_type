
library(readr)
library(dplyr)
library(stringr)



house_voters <- read_rds("data/house_district_voter_classification.rds")
house_results <- read_rds("data/election_results/ia_2024_leg_general_wide.rds") |> 
  filter(race_level == "State Representative")
house_primaries <- read_rds("data/election_results/ia_2024_leg_primary_wide.rds") |> 
  filter(race_level == "State Representative") |> 
  mutate(party = str_remove(party, ".$"))


percent <- scales::label_percent(accuracy = 0.1)


combined_data <- house_voters |>
  left_join(house_results) |>
  left_join(house_primaries)

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



write_rds(combined_data, "voter-type/data/voter_district_data.rds")




library(ggplot2)

combined_data |>
  filter(party == "No Party") |>
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
