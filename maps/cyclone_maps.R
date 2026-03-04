# ===============================
# CYCLONE MAPS SCRIPT
# ===============================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(gganimate)
library(gifski)
library(transformr)
library(htmlwidgets)

# -------------------------------
# Read data from project root
# -------------------------------

data_path <- file.path(getwd(), "data", "final_dataset.csv")

if (!file.exists(data_path)) {
  stop("final_dataset.csv not found inside data folder")
}

cyclone_data <- read.csv(data_path)

# -------------------------------
# Convert time column (X1)
# -------------------------------

cyclone_data$X1 <- as.POSIXct(
  cyclone_data$X1,
  format = "%Y-%m-%d %H_%M_%S"
)

# Remove missing coordinates
cyclone_data <- cyclone_data %>%
  filter(!is.na(X2), !is.na(X3), !is.na(X1))

# -------------------------------
# Create maps folder if not exists
# -------------------------------

if (!dir.exists("maps")) {
  dir.create("maps")
}

# ===============================
# 1️⃣ STATIC MAP
# ===============================

world <- ne_countries(scale = "medium", returnclass = "sf")

static_map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_path(
    data = cyclone_data,
    aes(x = X3, y = X2, group = X0),
    color = "red",
    linewidth = 0.7
  ) +
  theme_minimal() +
  labs(title = "Global Cyclone Tracks (Kaggle Dataset)")

ggsave(
  filename = file.path(getwd(), "maps", "static_cyclone_map.png"),
  plot = static_map,
  width = 10,
  height = 6
)

# ===============================
# 2️⃣ INTERACTIVE MAP
# ===============================

interactive_map <- leaflet(cyclone_data) %>%
  addTiles() %>%
  addPolylines(
    lng = ~X3,
    lat = ~X2,
    group = ~X0,
    color = "blue",
    weight = 2
  )

saveWidget(
  interactive_map,
  file = file.path(getwd(), "maps", "interactive_cyclone_map.html"),
  selfcontained = FALSE
)

# ===============================
# 3️⃣ ANIMATED MAP (GIF)
# ===============================

animated_map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_path(
    data = cyclone_data,
    aes(x = X3, y = X2, group = X0),
    color = "red",
    linewidth = 0.8
  ) +
  transition_reveal(X1) +
  labs(title = "Cyclone Movement Over Time") +
  theme_minimal()

animate(
  animated_map,
  renderer = gifski_renderer(
    file.path(getwd(), "maps", "animated_paths.gif")
  ),
  width = 800,
  height = 500,
  fps = 10,
  duration = 15
)

cat("All cyclone maps generated successfully.\n")