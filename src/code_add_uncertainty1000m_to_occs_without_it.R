library(tidyverse)      # To do datascience
library(here)           # To find files
library(rgbif)          # To use GBIF services
library(sf)             # To work with GIS data
library(leaflet)        # To make maps using Openstreet Maps


key <- "0011257-200127171203522"
zip_filename <- paste0(key, ".zip")
if (!file.exists(here::here("data", "raw", zip_filename))) {
  occ <- occ_download_get(
    key = key,
    path = here::here("data", "raw")
  )
}

occ_file <- paste(key, "occurrence.txt", sep = "_")
occ_path <- here::here("data", "raw", occ_file)
if (!file.exists(here::here("data", "raw", occ_file))) {
  unzip(zipfile = occ,
        files = "occurrence.txt",
        exdir = here::here("data", "raw"))
  file.rename(from = here::here("data", "raw", "occurrence.txt"),
              to = occ_path
  )
}
cols_occ_file <- read_delim(
  occ_path, "\t", n_max = 1,
  quote = ""
)
cols_occ_file <- names(cols_occ_file)

length(cols_occ_file)


occ_reynoutria <- read_tsv(
  here::here("data", "raw", occ_file),
  na = "",
  quote = "",
  guess_max = 50000)
nrow(occ_reynoutria)

issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE",
  "COORDINATE_INVALID"
)
names(issues_to_discard) <- issues_to_discard
issues_to_discard

issues <-
  occ_reynoutria %>%
  distinct(issue) %>%
  separate(issue, into = "issues", sep = ";") %>%
  distinct() %>%
  arrange()
issues

if (any(issues_to_discard %in% issues$issues)) {
  occ_reynoutria <-
    map_dfc(issues_to_discard,
            function(x) {
              str_detect(occ_reynoutria$issue, x)
            }) %>%
    mutate_all(funs(replace(., is.na(.), FALSE))) %>%
    bind_cols(occ_reynoutria) %>%
    filter_at(issues_to_discard,
              all_vars(. == FALSE)) %>%
    select(-one_of(issues_to_discard))
}

occurrenceStatus_to_discard <- c(
  "absent",
  "excluded"
)

occurrenceStatus <-
  occ_reynoutria %>%
  distinct(occurrenceStatus) %>%
  distinct()
occurrenceStatus

if (any(
  occurrenceStatus_to_discard %in% occurrenceStatus$occurrenceStatus)) {
  occ_reynoutria <-
    occ_reynoutria %>%
    filter(!occurrenceStatus %in% occurrenceStatus_to_discard)
}
nrow(occ_reynoutria)

# Make table 1
occ_reynoutria %>%
  distinct(scientificName,
           taxonRank,
           species,
           taxonomicStatus,
           acceptedScientificName) %>%
  arrange(species, taxonomicStatus)

# Show how to add uncertainty if not present
occs_no_uncertainty <-
  occ_reynoutria %>%
  filter(is.na(coordinateUncertaintyInMeters))

occs_no_uncertainty %>%
  distinct(datasetKey)

# Code for Fig.2 Add 1000m uncertainty to occurrences without it and plot them
sample_occs_no_uncertainty <-
  occs_no_uncertainty
occs_no_uncertainty_map <-
  occs_no_uncertainty %>%
  head() %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(3035) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = sample_occs_no_uncertainty$decimalLongitude,
             lat = sample_occs_no_uncertainty$decimalLatitude, weight = 1,
             radius = 1000, color = "red") %>%
  setView(lng = 3.683700, lat = 50.900596, zoom = 9)
occs_no_uncertainty_map
