# Export the Shiny app's data as JSON for the static Quarto prototype
# (static/index.qmd). Run from the repository root:
#   Rscript static/prep_static_data.R
# Re-run whenever data/ changes.

library(readr)
library(dplyr)
library(sf)
library(jsonlite)

out_dir <- "static/data"
dir.create(out_dir, showWarnings = FALSE)

# District results: only the columns the page uses, for the 2024 House like
# the Shiny app. Margins are kept unrounded so the page rounds them the same way.
read_rds("data/voter_district_data.rds") |>
  filter(year == 2024, chamber == "House") |>
  transmute(
    district_num, party, voter_classification, grade,
    gen_margin_category = as.character(gen_margin_category),
    gen_margin, gen_competitive, gen_winning_party,
    margin, first_vote_cand, prim_margin_description
  ) |>
  write_json(file.path(out_dir, "districts.json"), digits = NA, na = "null")

read_csv("data/house_legislators.csv", show_col_types = FALSE) |>
  arrange(district_num) |>
  select(district_num, name) |>
  write_json(file.path(out_dir, "legislators.json"))

read_csv("data/voter_type_text.csv", show_col_types = FALSE) |>
  write_json(file.path(out_dir, "voter_type_text.json"))

# District boundaries, simplified to ~0.5 MB (4 MB unsimplified). Shapes are
# simplified one at a time, in meters (UTM 15N), so neighbors can open slivers
# of up to ~10 m between them; the page assigns a point in a sliver to the
# nearest district. WGS84 matches the geocoders' coordinates.
st_read("data/district_boundaries/Plan2_House.shp", quiet = TRUE) |>
  select(district_num = DISTRICT_N) |>
  st_transform(26915) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 10) |>
  st_transform(4326) |>
  st_write(
    file.path(out_dir, "house_districts.geojson"),
    delete_dsn = TRUE, quiet = TRUE,
    layer_options = "COORDINATE_PRECISION=5"
  )
