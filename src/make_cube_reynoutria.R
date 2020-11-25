library(sf)
library(sp)
library(tidyverse)
library(here)
library(rgbif)
library(ggplot2)
library(leaflet)
library(tidylog)


#read EEA grid of Belgium
be_grid <- st_read("../pipeline/data/external/utm1_bel")

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

# Assign 1000 meters to occurrences without uncertainty or occurrences with zero
# uncertainty

occ_reynoutria <-
  occ_reynoutria %>%
  mutate(
    coordinateUncertaintyInMeters =
      if_else(is.na(coordinateUncertaintyInMeters) |
                coordinateUncertaintyInMeters == 0.0,
              1000.0,
              coordinateUncertaintyInMeters)
  )

# Save decimalLatitude` and `decimalLongitude` and coordinate uncertainty,
# `coordinateUncertaintyInMeters` as `geodata_df`
geodata_df <-
  occ_reynoutria %>%
  select(decimalLatitude,
         decimalLongitude,
         coordinateUncertaintyInMeters)
nrow_geodata_df <- nrow(geodata_df)

## Project geographic coordinates
coordinates(geodata_df) <- ~decimalLongitude+decimalLatitude
proj4string(geodata_df) <- CRS("+init=epsg:4326")
geodata_df <- spTransform(geodata_df, CRS("+init=epsg:3035"))
colnames(geodata_df@coords) <- c("x", "y")

## Assign occurrence within uncertainty circle
geodata_df@data <-
  geodata_df@data %>%
  mutate(random_angle = runif(nrow_geodata_df, 0, 2*pi))
geodata_df@data <-
  geodata_df@data %>%
  mutate(random_r = sqrt(runif(
    nrow_geodata_df, 0, 1)) * coordinateUncertaintyInMeters)
geodata_df@data <-
  geodata_df@data %>%
  mutate(x = geodata_df@coords[, "x"],
         y = geodata_df@coords[, "y"])
geodata_df@data <-
  geodata_df@data %>%
  mutate(x = x + random_r * cos(random_angle),
         y = y + random_r * sin(random_angle))
# x` and `y` are the new coordinates while in `@coords` we keep track of the original coordinates:
geodata_df@data <-
  geodata_df@data %>%
  select(-c(random_angle, random_r)) %>%
  select(x, y, coordinateUncertaintyInMeters)

# We assign each occurrence to a grid cell.
geodata_df@data <-
  geodata_df@data %>%
  mutate(eea_cell_code = paste0("1km",
                                "E", floor(x/1000),
                                "N", floor(y/1000)))
# Add cell code to occ_reynoutria
occ_reynoutria$eea_cell_code <- geodata_df@data$eea_cell_code
# example
occ_reynoutria %>% head()

occ_cube_reynoutria <-
  occ_reynoutria %>%
  group_by(year, eea_cell_code, speciesKey, species) %>%
  summarize(n = n(),
            min_coord_uncertainty = min(coordinateUncertaintyInMeters)) %>%
  ungroup()

# get all taxa
occ_cube_reynoutria_species <-
  occ_reynoutria %>%
  distinct(speciesKey, taxonKey, scientificName, species)

taxa_species_reynoutria <-
  occ_cube_reynoutria_species %>%

  # get unique 'speciesKey'
  distinct(speciesKey) %>%

  # extract speciesKey
  pull(speciesKey) %>%

  # GBIF query via name_usage
  map(~name_usage(key = .x)) %>%

  # Select data
  map(~.x[["data"]]) %>%

  # Merge all taxa in a data.frame
  reduce(full_join) %>%

  # select columns of interest
  select(speciesKey, scientificName, rank, taxonomicStatus) %>%

  # rename 'scientificName' to 'species_scientificName'
  rename(species_scientificName = scientificName) %>%

  # add these columns to original df
  right_join(occ_cube_reynoutria_species, by = "speciesKey") %>%

  # group by 'speciesKey'
  group_by(speciesKey,
           species_scientificName,
           species,
           rank,
           taxonomicStatus) %>%

  # create 'includes' column
  summarize(includes = paste(
    taxonKey,
    scientificName,
    sep = ": ",
    collapse = " | ")) %>%

  # rename 'species_scientificName' to 'scientificName'
  rename(scientificName = species_scientificName)

taxa_species_reynoutria

write_csv(occ_cube_reynoutria, path = "./data/processed/cube_reynoutria_BE.csv", na = "")
write_csv(taxa_species_reynoutria, path = "./data/processed/taxa_reynoutria_BE.csv", na = "")
