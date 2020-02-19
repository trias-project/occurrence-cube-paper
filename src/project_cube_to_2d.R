library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(tidylog)
library(egg)
library(ggpubr)


# read occurrence cube
occ_cube_reynoutria <- read_csv(file = here("data",
                                            "processed",
                                            "cube_reynoutria_BE.csv"),
                                na = "")
# read taxonomic compendium of occurrence cube
taxa_species_reynoutria <- read_csv(file = here("data",
                                            "processed",
                                            "taxa_reynoutria_BE.csv"),
                                na = "")

#read EEA grid of Belgium
be_grid <- st_read("../pipeline/data/external/utm1_bel")


## Projct cube on taxonomic - temporal dimension

# Number of occurrences
n_occs_per_species_year <-
  occ_cube_reynoutria %>%
  group_by(speciesKey, species, year) %>%
  summarize(occurrences = sum(n)) %>%
  ungroup()

n_occs_per_species_year_plot <-
  n_occs_per_species_year %>%
  ggplot() +
  geom_line(aes(x = year, y = occurrences, color = as.factor(species))) +
  scale_colour_manual(values = c("red", "green", "blue"), guide = FALSE) +
  theme(axis.title = element_text(size = 14))

# Number of cells (area of occuapncy)
n_cells_per_species_year <-
  occ_cube_reynoutria %>%
  group_by(speciesKey, species, year) %>%
  summarize(aoo = n()) %>%
  ungroup()

n_cells_per_species_year_plot <-
  n_cells_per_species_year %>%
  ggplot() +
  geom_line(aes(x = year, y = aoo, color = as.factor(species))) +
  scale_colour_manual(labels = c("R. bohemica", "R. japonica", "R. sachaliensis"),
                      values = c("red", "green", "blue")) +
  ylab("area of occupancy (km2)") +
  theme(legend.title = element_blank(),
        legend.justification = c(1,0), legend.position = c(0.25,0.77),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14))

n_occs_cells_per_species_year_plot <-
  grid.arrange(n_occs_per_species_year_plot,
               n_cells_per_species_year_plot, nrow = 1)
# Save png
ggsave(filename = here::here("figures", "n_occs_cells_per_species_year.png"),
       plot = n_occs_cells_per_species_year_plot,
       dpi = 300,
       width = 15)


## Project cube on taxonomic - spatial dimension
n_occs_per_species_cell <-
  occ_cube_reynoutria %>%
  group_by(speciesKey, species, eea_cell_code) %>%
  summarize(occurrences = sum(n)) %>%
  ungroup()

# map (just a sample of all the cells)
x_y_wgs84 <- list(x = 4.398913, y = 51.203547)
x_y <- tibble(x = x_y_wgs84$x, y = x_y_wgs84$y)
x_y <- st_as_sf(x_y, coords = c("x", "y"), crs = 4326) %>%
  st_transform(3035) %>%
  st_coordinates()

x_y_surroundings <- crossing(
  X = seq(floor(x_y[1]/1000) - 20, floor(x_y[1]/1000) + 20),
  Y = seq(floor(x_y[2]/1000) - 20, floor(x_y[2]/1000) + 20)
)
x_y_cells_surroundings <- paste0(
  "1km", "E", x_y_surroundings$X, "N", x_y_surroundings$Y
)

basic_grid <-
  be_grid %>%
  filter(CELLCODE %in% x_y_cells_surroundings)

# make sf objects (grid + data from aggregated n_occs_per_species_cell)
n_occs_per_species_cell_sf <-
  basic_grid %>%
  left_join(n_occs_per_species_cell,
            by = c("CELLCODE" = "eea_cell_code")) %>%
  filter(!is.na(species)) %>%
  mutate(occurrences = if_else(is.na(occurrences), 0.,
                               as.double(occurrences)))

# shorten species names for shorter facet labels
species_labels <- str_replace(unique(taxa_species_reynoutria$species), pattern = "eynoutria", replacement = ".")
names(species_labels) <- unique(taxa_species_reynoutria$species)

# plot via facet_wrap and geom_sf
max_occs <- max(n_occs_per_species_cell_sf$occurrences)
n_occs_per_species_cell_map <-
  ggplot(n_occs_per_species_cell_sf %>%
           st_transform(4326)) +
  geom_sf(aes(fill = occurrences)) +
  facet_wrap(~ species, nrow = 1,
             labeller = labeller(species = species_labels)) +
  scale_fill_gradient(limits = c(1, max_occs),
                      trans = "log",
                      breaks = c(1, 10, 100, 1000, 2000)) +
  coord_sf(xlim = c(4.2, 4.6),
           ylim = c(51.05, 51.3),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12))
n_occs_per_species_cell_map

ggsave(filename = here::here("figures", "n_occs_per_species_cell.png"),
       plot = n_occs_per_species_cell_map,
       dpi = 300,
       width = 15)


## Project on temporal - spatial dimension
n_occs_per_year_cell <-
  occ_cube_reynoutria %>%
  group_by(year, eea_cell_code) %>%
  summarize(occurrences = sum(n)) %>%
  ungroup()

# make sf objects (grid + data from aggregated n_occs_per_year_cell)
n_occs_per_year_cell_sf <-
  basic_grid %>%
  left_join(n_occs_per_year_cell,
            by = c("CELLCODE" = "eea_cell_code")) %>%
  filter(!is.na(year)) %>%
  mutate(occurrences = if_else(is.na(occurrences), 0.,
                               as.double(occurrences)))

# plot via facet_wrap and geom_sf
max_occs <- max(n_occs_per_year_cell_sf$occurrences)
min_year <- max(n_occs_per_year_cell_sf$year) - 2
max_year <- max(n_occs_per_year_cell_sf$year)
n_occs_per_year_cell_map <-
  ggplot(n_occs_per_year_cell_sf %>%
           st_transform(4326) %>%
           filter(year >= min_year & year <= max_year)) +
  geom_sf(aes(fill = occurrences)) +
  facet_wrap(~ year) +
  scale_fill_gradient(limits = c(1, max_occs),
                      trans = "log",
                      breaks = c(1, 10, 100, 1000, 2000)) +
  coord_sf(xlim = c(4.2, 4.6),
           ylim = c(51.05, 51.3),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12))
n_occs_per_year_cell_map

ggsave(filename = here::here("figures", "n_occs_per_year_cell.png"),
       plot = n_occs_per_year_cell_map,
       dpi = 300,
       width = 15)

# Put the three pngs together with labels B, C, D

