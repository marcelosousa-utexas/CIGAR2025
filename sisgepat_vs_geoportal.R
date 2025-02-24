library(tidyverse)
library(jsonlite)
library(sf)
library(rgdal)
library(ggmap)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)

# Define the change_strings function
map_ra_names <- function(address) {
  
  # Define the mappings using a named list
  mappings <- list(
    "PLANO PILOTO" = c("Vila Planalto", "Asa Sul", "Asa Norte", "Setor Noroeste", 
                       "Noroeste", "Brasília", "Eixo Monumental"),
    "JARDIM BOTÂNICO" = c("Jardim Botânico", "Jerdim Botânico"),
    "GUARÁ" = c("Guará", "Guara II", "Guará II"),
    "SCIA" = c("Vila Estrutural", "Estrutural"), 
    "SÃO SEBASTIÃO" = c("São Bartolomeu"),
    "RIACHO FUNDO" = c("Riacho Fundo I")
  )
  
  # Perform the replacements
  for (new_string in names(mappings)) {
    for (old_string in mappings[[new_string]]) {
      # Use word boundaries to match whole words
      address <- gsub(paste0("\\b", old_string, "\\b"), new_string, address, ignore.case = TRUE)
    }
  }
  
  # Convert any remaining strings to uppercase
  address <- toupper(address) 
  
  return(address)
}

extract_coords <- function(ra, precision = 6) {
  # Extract the geometry data for the given RA
  geometry_data <- data[[ra]]$geometry[[1]]
  
  # Check if the geometry data is a numeric array
  if (is.array(geometry_data)) {
    coords <- geometry_data[1, , ]
    coords <- round(coords, precision)  # Round to the specified precision
    
  } else if (is.list(geometry_data)) {
    coords <- do.call(rbind, geometry_data)
    coords <- round(coords, precision)  # Round to the specified precision
    
  } else {
    stop("Unknown geometry data structure")
  }
  
  return(coords)
}


dms_to_decimal <- function(dms) {
  degrees <- as.numeric(sub("º.*", "", dms))
  minutes <- as.numeric(sub("^.*º", "", sub("’.*", "", dms)))
  seconds <- as.numeric(sub("^.*’", "", sub("”.*", "", dms)))
  direction <- ifelse(grepl("S|W", dms), -1, 1)
  
  return (direction * (degrees + minutes / 60 + seconds / 3600))
}


# Function to split coordinates and check for NA or non-numeric values
split_coordinates <- function(coordinate_string) {
  if (is.na(coordinate_string)) {
    return(list(lat = NA, lon = NA))
    
  } else if (grepl("º", coordinate_string)) {
    split_coordinate <- strsplit(coordinate_string, ",")[[1]]
    lat <- as.numeric(dms_to_decimal(split_coordinate[1]))
    lon <- as.numeric(dms_to_decimal(split_coordinate[2]))
    
    if (lat > 0) {
      lat <- lat *-1
    }
    if (lon > 0) {
      lon <- lon *-1
    }
    if (lon > lat) {
      temp <- lat
      lat <- lon
      lon <- temp
    }
    
    # Return a list with lat and lon
    return(list(lat = lat, lon = lon))
    
  } else {
    coordinate_string <- sub(" ", "", coordinate_string)
    coordinate_string <- gsub("(?<=\\d),(?=\\d)", ".", coordinate_string, perl = TRUE)    
    pattern <- "(-?\\d+\\.\\d+)"
    matches <- stringr::str_extract_all(coordinate_string, pattern)
    coordinates <- as.numeric(unlist(matches))

    # Check if either lat or lon is NA or not numeric
    if (any(is.na(coordinates)) || any(!sapply(coordinates, is.numeric))) {
      return(list(lat = NA, lon = NA))
    } else {
      
      lat = coordinates[1]
      lon = coordinates[2]
      
      if (lat > 0) {
        lat <- lat *-1
      }
      if (lon > 0) {
        lon <- lon *-1
      }      
      if (lon > lat) {
        temp <- lat
        lat <- lon
        lon <- temp
      }
      
      
      # Return a list with lat and lon
      return(list(lat = lat, lon = lon))
    }
  }
}


create_ra_shape_file <- function(ra_shape_path, precision = 6) {

  # Initialize empty lists to store geometries and attributes

  all_lote_coords <- list()
  result <- list()
  geometries <- list()
  attributes <- list()

  # Loop through each key in the data
  for (i in seq_along(data)) {
    key <- names(data)[i]


    coords <- extract_coords(key, precision)
    
    objectid <- data[[key]]$objectid
    ra_geo_num <- data[[key]]$ra_cira
    ra_geo <- data[[key]]$ra_nome
    
    ra_codigo <- data[[key]]$ra_codigo
    ra_path <- data[[key]]$ra_path    
    
    st_area <- round(data[[key]]$`st_area(shape)`, precision)
    st_length <- round(data[[key]]$`st_length(shape)`, precision)
    # last_edited_date <- data[[key]]$last_edited_date

    attributes[[key]] <- c(objectid, ra_geo_num, ra_geo, ra_codigo, ra_path, st_area, st_length)

    # Ensure the first and last coordinates are the same (closed polygon)
    if (!identical(coords[1, ], coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }

    # Create a polygon from the coordinates
    polygon <- st_polygon(list(coords))
    geometries[[key]] <- polygon
  }


  # Create a simple feature collection from the list of Multipolygons
  sfc_multipolygon <- st_sfc(geometries, crs = 31983)

  # Combine attributes into a data frame
  df_attributes <- do.call(rbind, attributes)

  # Convert df_attributes to a data frame with proper column names
  df_attributes <- as.data.frame(df_attributes, stringsAsFactors = FALSE)
  colnames(df_attributes) <- c(
    "objectid", "ra_geo_num", "ra_geo", "ra_codigo", 
    "ra_path", "st_area", "st_length"
  )
  
  # Combine the data frame and the simple feature collection
  sf_multipolygon <- st_sf(df_attributes, geometry = sfc_multipolygon)  
  
  # Write to a shapefile
  st_write(sf_multipolygon, ra_shape_path, append = FALSE)

}

# Verify if the point (lat and lon in 31983 system) is inside the Administrative Region
ra_coords <- function(df_row) {
  
  ra <- df_row$RA_cidade
  Coordenadas <- df_row$IM_COORDENADAS
  number_of_results <- NA
  
  # Split the string into a list using the comma as a separator
  result <- split_coordinates(Coordenadas)
  
  lat <- result$lat
  lon <- result$lon
  
  # Check if lat or lon is NA
  if (is.na(lat) || is.na(lon)) {
    return(data.frame(ra_geo = NA, ra_geo_num = NA, ra_cidade_vs_reo_geo = NA))
  }
  
  # Point Coordinate -----------------
  # Latitude and Longitude coordinates in the standard WGS84 system (epsg code = 4326)
  point_4326 <- SpatialPoints(cbind(lon, lat), proj4string = CRS("+init=epsg:4326"))
  # Convert the point to GeoPortal reference system : IRGAS 2000 / UTM zone 23S (epsg code = 31983)
  point_31983 <- spTransform(point_4326, CRS("+init=epsg:31983"))
  
  x_convert = point_31983$lon
  y_convert = point_31983$lat
  
  point <- st_as_sf(point_31983)
  # Point Coordinate -----------------
  
  result <- st_within(point, shapefile_ra)[[1]]
  #result <- st_contains(shapefile_ra, point)
  number_of_results = length(result)
  
  if (is.na(number_of_results)) {
    #ra_find <- NA
    return(data.frame(ra_geo = NA, ra_geo_num = NA, ra_cidade_vs_reo_geo = NA))
  } else if (number_of_results > 0) {
    ra_information <- shapefile_ra[result, ]
    #print(ra_information)
    if (ra_information$ra_geo == ra){
      ra_cidade_vs_reo_geo <- TRUE
    } else {
      ra_cidade_vs_reo_geo <- FALSE
    }
    new_columns <- ra_information %>% 
      select(ra_geo, ra_geo_num) %>%
      mutate(
        ra_cidade_vs_reo_geo = ra_cidade_vs_reo_geo
      )
    
    return(new_columns)    
  } else {
    print(number_of_results)
    print(df_row)
    ra_find <- FALSE
    return(data.frame(ra_geo = NA, ra_geo_num = NA, ra_cidade_vs_reo_geo = NA))
  }
  
}
process_coords <- function(df_row, shapefile, mode) {
  # Mapping columns based on mode
  columns_mapping <- list(
    lote = c("pu_ra", "pu_ciu", "pu_tipo", "pn_uso", "pu_end_car", "pu_end_usu", 
             "pu_cep", "qd_setor", "qd_quadra", "qd_conjunt", "qd_lote", 
             "qd_area", "st_area_sh", "st_length_", "x", "y"),
    ocup = c("lt_ra", "ct_ciu", "lt_enderec", "lt_cep", "lt_setor", "lt_quadra", 
             "lt_conjunt", "lt_lote", "ac_area_ce", "st_area_sh", "geometry", 
             "x", "y")
  )
  
  # Extract row-specific data
  ra_geo_num <- df_row$ra_geo_num
  Coordenadas <- df_row$IM_COORDENADAS
  
  # Split coordinates
  result <- split_coordinates(Coordenadas)
  lat <- result$lat
  lon <- result$lon
  
  # Handle missing coordinates
  if (is.na(lat) || is.na(lon)) {
    empty_df <- data.frame(matrix(ncol = length(columns_mapping[[mode]]) + 5, nrow = 1))
    colnames(empty_df) <- c(columns_mapping[[mode]], "busca", "lat", "lon", "x_convert", "y_convert")
    return(empty_df)
  }
  
  # Point Coordinate -----------------
  # Latitude and Longitude coordinates in WGS84 system (EPSG: 4326)
  point_4326 <- SpatialPoints(cbind(lon, lat), proj4string = CRS("+init=epsg:4326"))
  # Convert the point to the GeoPortal reference system: IRGAS 2000 / UTM zone 23S (EPSG: 31983)
  point_31983 <- spTransform(point_4326, CRS("+init=epsg:31983"))
  x_convert <- coordinates(point_31983)[, 1]
  y_convert <- coordinates(point_31983)[, 2]
  point <- st_as_sf(point_31983)
  # Point Coordinate -----------------
  
  # Check if the point is within the shapefile
  result <- st_within(point, shapefile)[[1]]
  number_of_results <- length(result)
  
  if (number_of_results == 0) {
    busca <- "APROXIMADA"
    nearest_polygon_index <- st_nearest_feature(point, shapefile)
    closest_polygon <- shapefile[nearest_polygon_index, ]
  } else {
    busca <- "EXATA"
    closest_polygon <- shapefile[result, ]
  }
  
  # Dynamically select columns based on the mode
  selected_columns <- columns_mapping[[mode]]
  
  new_columns <- closest_polygon %>%
    select(all_of(selected_columns)) %>%
    mutate(
      busca = busca,
      lat = lat,
      lon = lon,
      x_convert = x_convert,
      y_convert = y_convert
    )
  
  return(new_columns)
}

# Define the change_strings function
find_lotes_coords <- function(shapefile, df_terrenos_temp, mode) {
  
  # Initialize an empty list to store results
  all_lote_coords <- list()
  
  # Loop through unique values of pu_ra
  
  unique_pu_ra <- na.omit(unique(df_terrenos_temp$ra_geo_num))
  #unique_pu_ra <- c("15")
  for (pu_ra_value in unique_pu_ra) {
    # Filter shapefile_ep_pub
    if (mode == "ocup") {
      filtered_shapefile <- shapefile %>%
        filter(lt_ra == pu_ra_value)
    } else {
      filtered_shapefile <- shapefile %>%
        filter(pu_ra == pu_ra_value)
    }
    
    print(pu_ra_value)
    
    coords <- df_terrenos_temp %>%
      filter(ra_geo_num == pu_ra_value) %>%
      mutate(new_columns = pmap(
        list(ra_geo_num = ra_geo_num, IM_COORDENADAS = IM_COORDENADAS),
        ~ process_coords(tibble(ra_geo_num = ..1, IM_COORDENADAS = ..2), filtered_shapefile, mode)
      )) %>%
      unnest(cols = new_columns) %>%
      select(-geometry)
    
    all_lote_coords[[as.character(pu_ra_value)]] <- coords
    
  }
  
  # Combine all data frames in the list into a single data frame
  result <- bind_rows(all_lote_coords)
  return(result)
}

# Function to process and clean a table
process_and_clean_table <- function(file_path, suffix, exclude_range = NULL) {
  table <- read.csv(file_path)
  
  
  # Remove duplicates based on TOMBAMENTO and select relevant columns
  table <- table %>%
    distinct(TOMBAMENTO, .keep_all = TRUE)
  
  # Dynamically exclude a range of columns
  if (!is.null(exclude_range)) {
    table <- table %>%
      select(-all_of(names(table)[exclude_range]))
  }
  
  # Keep TOMBAMENTO and remaining columns
  table <- table %>%
    select(TOMBAMENTO, everything())
  
  # Optionally filter columns starting with "X32" if needed
  table <- table %>%
    select(TOMBAMENTO, matches("^X32"), everything())
  
  # Rename columns
  colnames(table)[-1] <- paste0(colnames(table)[-1], ".", suffix)
  
  return(table)
}

lote_path <- "/home/marcelo/Downloads/temp/Sisgepat_lotes.csv"
ocup_path <- "/home/marcelo/Downloads/temp/Sisgepat_ocup.csv"
lote_pub_path <- "/home/marcelo/Downloads/temp/Sisgepat_lotes_public.csv"
#see python file "api_arcgis_get_ra.py" to generate this ra.json file
ra_json_path <- "/home/marcelo/Downloads/temp/ra.json" 
ra_shape_path <- "/home/marcelo/Downloads/temp/shapefile_ra.shp"
#dataset with public properties from District Federal Government
sisgepat_data_path <- "/home/marcelo/Downloads/temp/PTXX - Relatório Sisgepat - Incorporados.xlsx"
sisgepat_data_table_name <- "RELATORIO_SISGEPAT_TABELA"

data <- fromJSON(txt = ra_json_path)
create_ra_shape_file(ra_shape_path, precision= 6)
shapefile_ra <- read_sf(ra_shape_path)

sisgepat_data <- read_excel(sisgepat_data_path, sheet = sisgepat_data_table_name, col_names = TRUE)

df_terrenos <- sisgepat_data %>%
  filter(TIPO != "MOBILIÁRIO URBANO" & PAT_TERRENOS.SR_CODIGO == 0) %>%
  mutate(RA_cidade = ifelse(CIDADE == "Sudoeste", "SUDOESTE/OCTOGONAL", sapply(CIDADE, change_strings)))

df_terrenos_temp <- merge(df_terrenos, shapefile_ra[, c("ra_geo", "ra_geo_num")], 
                          by.x = "RA_cidade", by.y = "ra_geo", 
                          all.x = TRUE)

df_terrenos_temp <- df_terrenos_temp %>%
  rename(RA_cidade_num = ra_geo_num) %>%
  select(-geometry)

df_terrenos_temp <- df_terrenos_temp %>%
  rowwise() %>%
  #mutate(new_columns = list(ra_coords(cur_data()))) %>%
  mutate(new_columns = list(ra_coords(pick(c("RA_cidade", "IM_COORDENADAS"))))) %>%
  ungroup() %>% # Don't forget to ungroup after using rowwise
  unnest(cols = new_columns)  %>% # Unnest the RA_COORDS column %>%
  select(-geometry)

table_lotes <- find_lotes_coords(shapefile, df_terrenos_temp, mode="lote")
write.csv(as.data.frame(table_lotes), lote_path, row.names = FALSE)

# pu_tipo: 0 - null; 1 - Lote; 2 - Setor Viário; 3 - Equipamento Público
shapefile_lotes_ep_pub <- shapefile %>%
  filter(pu_tipo == 3)
table_lotes_pub <- find_lotes_coords(shapefile_lotes_ep_pub, df_terrenos_temp, mode="lote")
write.csv(table_lotes_pub, lote_pub_path, row.names = FALSE)

table_ocup <- find_lotes_coords(shapefile_oc, df_terrenos_temp, mode="ocup")
write.csv(table_ocup, ocup_path, row.names = FALSE)

# Process tables
lote_table <- process_and_clean_table(lote_path, "lotes", 2:27)
ocup_table <- process_and_clean_table(ocup_path, "ocup", 2:27)
lote_pub_table <- process_and_clean_table(lote_pub_path, "lotes_pub", 2:27)

# List of tables to merge
tables_list <- list(df_terrenos, lote_table, ocup_table, lote_pub_table)

# Merge all tables by "TOMBAMENTO"
df_sisgepat_geoportal <- reduce(tables_list, ~ merge(.x, .y, by = "TOMBAMENTO", all = TRUE))

df_sisgepat_geoportal_filtered <- df_sisgepat_geoportal %>%
  filter(
    !grepl("VILA", CIDADE, ignore.case = TRUE) & 
      (is.na(SITUACAO) | SITUACAO == "") & 
      (!is.na(IM_COORDENADAS) | IM_COORDENADAS != "") & 
      PAT_TERRENOS.METRAGEM_NUM > 100
  )

