library(tidyverse)      # To do datascience
library(here)           # To find files
library(rgbif)          # To use GBIF services

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

occ_reynoutria <- read_tsv(
  here::here("data", "raw", occ_file),
  na = "",
  quote = "",
  guess_max = 50000)

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

if (any(
  occurrenceStatus_to_discard %in% occurrenceStatus$occurrenceStatus)) {
  occ_reynoutria <-
    occ_reynoutria %>%
    filter(!occurrenceStatus %in% occurrenceStatus_to_discard)
}

# Source of table 1
occ_reynoutria %>%
  distinct(scientificName,
           taxonRank,
           species,
           taxonomicStatus,
           acceptedScientificName) %>%
  arrange(species, taxonomicStatus)
