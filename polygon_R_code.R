library(dplyr)
library(sf)
library(purrr)
library(ggplot2)
library(ggmap)
library(tidyverse)

# Function to calculate Euclidean distance between two points
get_longest_segment <- function(line_sf, test_point, full_segment) {
  
  # Calculate the nearest point on the line for the test point
  nearest_points <- st_nearest_points(test_point, line_sf)
  nearest_point_coords <- st_coordinates(nearest_points)[2, , drop = FALSE]  # Second row gives the nearest point on the line
  
  # Find the index of the nearest point in df001
  distances_to_nearest <- sqrt((full_segment$E - nearest_point_coords[1, 1])^2 + (full_segment$N - nearest_point_coords[1, 2])^2)
  nearest_index <- which.min(distances_to_nearest)
  
  # Split df001 into left and right segments
  left_segment <- df001[1:nearest_index, ]
  right_segment <- df001[nearest_index:nrow(df001), ]
  
  # Function to calculate the total length of a segment
  calculate_segment_length <- function(segment) {
    if (nrow(segment) < 2) return(0)  # No length for segments with fewer than 2 points
    sum(sqrt(diff(segment$E)^2 + diff(segment$N)^2))
  }
  
  # Calculate lengths of left and right segments
  left_length <- calculate_segment_length(left_segment)
  right_length <- calculate_segment_length(right_segment)
  
  
  # Determine which segment is larger and update df001
  if (left_length > right_length) {
    longest_segment <- left_segment
  } else {
    longest_segment <- right_segment
  }
  
  return(longest_segment)
  
}

# # Define a function to calculate zoom level based on distance
calculate_zoom <- function(distance) {
  # Estimate the zoom level based on the distance
  # You may need to adjust the factor based on your specific use case
  zoom <- ceiling(log2(40075016.68 / (distance * 156543.03392)))
  
  # Add a buffer to the zoom level
  #zoom_buffered <- zoom - 1
  zoom_buffered <- zoom
  
  # Ensure the zoom level is not less than 1
  zoom_final <- max(zoom_buffered, 8)
  
  return(zoom_final)
}


coordinates <- data.frame(
  E = c(
    194141.570, 197550.000, 197406.000, 196990.000, 196720.000,
    195442.292, 195218.3481, 195445.7218, 194612.000, 193939.870,
    193836.322, 193786.460, 193797.1171, 193889.695, 194125.6905,
    194269.0147, 194353.314, 194573.250, 193952.7152, 194052.4530,
    192543.0974, 191918.306, 191276.8454, 192230.000, 191945.000,
    191590.000, 191918.538, 191065.834, 191284.892, 191831.002,
    194141.570  # Closing the loop
  ),
  N = c(
    8235010.609, 8243090.000, 8243130.000, 8244475.000, 8244600.000,
    8243833.371, 8244206.6089, 8244342.0338, 8244825.000, 8244939.621,
    8245013.726, 8244968.760, 8244904.5025, 8244833.2555, 8244449.4247,
    8244045.3144, 8244008.8377, 8243546.383, 8243255.7516, 8243042.7992,
    8242248.3369, 8242921.906, 8243859.2756, 8244540.000, 8244750.000,
    8244220.000, 8242225.723, 8239942.942, 8238537.468, 8237949.283,
    8235010.609  # Closing the loop
  )
)

rodovia <- read_sf('/home/marcelo/Downloads/rodovia/rodovia.shp')
filtered_rodovia <- rodovia %>%
  filter(rod_cod_di %in% c("001EDF0130", "001EDF0150", "001EDF0170", "001EDF0190"))

# Extract coordinates and convert to a dataframe
road_points_df <- filtered_rodovia %>%
  mutate(points = map(geometry, ~ st_coordinates(.))) %>%  # Extract coordinates for each geometry
  select(rod_cod_di, points) %>%  # Keep only rod_cod_di and the extracted points
  unnest_longer(points) %>%       # Expand the points into separate rows
  arrange(desc(points[, 2])) %>% 
  transmute(rod_cod_di,           # Retain the rod_cod_di column
            X = points[, 1],      # Extract the X-coordinate (1st column)
            Y = points[, 2])      # Extract the Y-coordinate (2nd column)

# Convert the tibble to a regular data frame
road_points_df <- as.data.frame(road_points_df)

df001 <- road_points_df[, -1]
colnames(df001) <- c("E", "N")

# Convert df001 to an sf object (line)
line_coords <- cbind(df001$E, df001$N)
line_sf <- st_sfc(st_linestring(line_coords), crs = 32643)

test_point <- st_sfc(st_point(c(194141.6, 8235011)), crs = 32643)
longest_segment1 <- get_longest_segment(line_sf, test_point, df001)

test_point <- st_sfc(st_point(c(197550.0, 8243090)), crs = 32643)
longest_segment2 <- get_longest_segment(line_sf, test_point, df001)

#intersection_df <- inner_join(longest_segment1, longest_segment2, by = c("E", "N"))
intersection_df <- merge(longest_segment1, longest_segment2, by = c("E", "N"))

sorted_df <- intersection_df %>%
  arrange(N)

# updated_line_sf <- st_sfc(st_linestring(cbind(sorted_df$E, sorted_df$N)), crs = 32643)
# 
# ggplot() +
#   geom_sf(data = test_point, color = "red", size = 3) +  # Test point (red)
#   geom_sf(data = updated_line_sf, color = "blue", size = 1) +  # Updated road segment (blue)
#   theme_minimal() +
#   ggtitle("Updated Road Segment After Splitting by Nearest Point")



# Specify the position after which to insert rows
position <- 1  # Insert after the 2nd row

# Combine the dataframes
combined_df <- rbind(
  coordinates[1:position, ],  # Rows from the first dataframe up to the position
  sorted_df,                # Rows from the second dataframe
  coordinates[(position + 1):nrow(coordinates), ]  # Remaining rows from the first dataframe
)

multipolygon_sf <- st_sfc(st_multipolygon(list(list(as.matrix(combined_df)))), crs = 31983)

shapefile_fi <- read_sf('/home/marcelo/Downloads/shapefile_full_info.shp')
register_google(key = 'AIzaSyAIj92FSAapbppBXT_2_-7Liby6dOep1kY')

sf_object <- st_sf(geometry = st_sfc(multipolygon_sf), crs = 31983)

# Transform the coordinates to WGS 84
sf_object_wgs84 <- st_transform(sf_object, 4326)

bbox <- st_bbox(sf_object_wgs84)

# Calculate the distance between corners of the bounding box
bbox_distance <- max(c(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin))

# Calculate zoom level
zoom_level <- calculate_zoom(bbox_distance)

names(bbox) <- c("left", "bottom", "right", "top")

map <- get_map(location = bbox, zoom = zoom_level, scale=4, language="pt-BR", maptype = "satellite", source = "google")

Sys.sleep(2)

result <- ggmap(map) +
  geom_sf(data = sf_object_wgs84, alpha = 0.4, linewidth = 0.8, color = "darkred")

ggsave(paste("/home/marcelo/Downloads/temp/", "635_test3", ".png", sep = ""), result, width = 10, height = 10, units = "in")

st_write(sf_object_wgs84, "/home/marcelo/Downloads/temp/output_file.kml", driver = "KML")



