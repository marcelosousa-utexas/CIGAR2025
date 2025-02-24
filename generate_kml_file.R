library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)
library(sf)
library(purrr)
library(stringr)

#API not working anymore!
api_connection_valor_venal <- function(iptu_value) {
  # Create a list to store variables
  dict_iptu <- list()
  
  # Define the variable name and value
  name <- "InscricaoImovel"
  
  # Associate the variable name with its value in the list
  dict_iptu[[name]] <- iptu_value
  url <- 'https://ww1.receita.fazenda.df.gov.br/WPI/psv/Consulta/Imovel/DadosIptuTlp'
  
  # Convert the list to a JSON string and then to a plain text string
  data <- toJSON(dict_iptu, auto_unbox = TRUE)
  
  # Define headers
  headers <- c(
    "Accept" = "application/json, */*",
    "Accept-Language" = "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7,es;q=0.6",
    "Authority" = "ww1.receita.fazenda.df.gov.br",
    "Content-Type" = "application/json",
    "Origin" = "https://ww1.receita.fazenda.df.gov.br",
    "Referer" = "https://ww1.receita.fazenda.df.gov.br/cidadao/consulta/imoveis/iptu-tlp/PautaImovel",
    "Sec-Ch-Ua" = "\"Edge\";v=\"114\", \"Chromium\";v=\"114\", \"Not=A?Brand\";v=\"24\"",
    "Sec-Ch-Ua-Mobile" = "?0",
    "Sec-Ch-Ua-Platform" = "\"Windows\"",
    "Sec-Fetch-Dest" = "empty",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Site" = "same-origin",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1788.0"
  )
  
  # Make the POST request
  response <- POST(url, body = data, add_headers(headers))
  
  json_data <- NA
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Parse the JSON content
    json_data <- content(response, as = "text") %>%
      fromJSON(flatten = TRUE)  # Convert to a flat list or data frame
  } else {
    cat("Error fetching data from the server.\n")
  }
  
  return(json_data)
}

#API not working anymore!
api_connection_ficha_cadastral <- function(iptu_value) {
  # Create a list to store variables
  dict_iptu <- list()
  
  # Define the variable name and value
  name <- "InscricaoImovel"
  
  # Associate the variable name with its value in the list
  dict_iptu[[name]] <- iptu_value
  url <- 'https://ww1.receita.fazenda.df.gov.br/WPI/psv/Consulta/Imovel/FichaCadastral'
  
  # Convert the list to a JSON string and then to a plain text string
  data <- toJSON(dict_iptu, auto_unbox = TRUE)
  
  # Define headers
  headers <- c(
    "Accept" = "application/json, */*",
    "Accept-Language" = "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7,es;q=0.6",
    "Authority" = "ww1.receita.fazenda.df.gov.br",
    "Content-Type" = "application/json",
    "Origin" = "https://ww1.receita.fazenda.df.gov.br",
    "Referer" = "https://ww1.receita.fazenda.df.gov.br/cidadao/consulta/imoveis/iptu-tlp/PautaImovel",
    "Sec-Ch-Ua" = "\"Edge\";v=\"114\", \"Chromium\";v=\"114\", \"Not=A?Brand\";v=\"24\"",
    "Sec-Ch-Ua-Mobile" = "?0",
    "Sec-Ch-Ua-Platform" = "\"Windows\"",
    "Sec-Fetch-Dest" = "empty",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Site" = "same-origin",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1788.0"
  )
  
  # Make the POST request
  response <- POST(url, body = data, add_headers(headers))
  
  json_data <- NA
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Parse the JSON content
    json_data <- content(response, as = "text") %>%
      fromJSON(flatten = TRUE)  # Convert to a flat list or data frame
  } else {
    cat("Error fetching data from the server.\n")
  }
  
  return(json_data)
}

#API not working anymore!
get_iptu_info <- function(iptu_value) {
  
  if (is.na(iptu_value) | is.nan(iptu_value)  | iptu_value == "Não localizado") {
    return(list(BC_IPTU = NA, NOME_PROPRIETARIO = NA, AREA_TERRENO = NA))
  }
  
  result <- api_connection_valor_venal(iptu_value)
  
  if (!any(is.na(result))){
    BC_IPTU <- str_trim(result$DadosImovel$BCIPTU[[1]])
    #BC_IPTU <- as.numeric(gsub(",", ".", BC_IPTU))
    BC_IPTU <- as.numeric(gsub(",", ".", gsub("\\.", "", BC_IPTU)))
  } else {
    BC_IPTU <- NA
    
  }
  
  
  result <- api_connection_ficha_cadastral(iptu_value)
  
  if (!any(is.na(result))){
    AREA_TERRENO <- str_trim(result$AreaTerreno)
    AREA_TERRENO <- as.numeric(gsub(",", ".", gsub("\\.", "", AREA_TERRENO)))
    NOME_PROPRIETARIO <- str_trim(result$Proprietarios$NomeProprietario)
    
  } else {
    AREA_TERRENO <- NA
    NOME_PROPRIETARIO <- NA
  }
  
  return(list(BC_IPTU = BC_IPTU, NOME_PROPRIETARIO = NOME_PROPRIETARIO, AREA_TERRENO = AREA_TERRENO))
}

# Define the function to get the 'centroid' latitude from the polygon
get_latitude <- function(x) {
  sf_object <- st_sf(geometry = st_sfc(x), crs = 31983)
  sf_object_wgs84 <- st_transform(sf_object, 4326)
  st_coordinates(st_centroid(sf_object_wgs84))[1, "Y"]
}

# Define the function to get the 'centroid' latitude from the polygon
get_longitude <- function(x) {
  sf_object <- st_sf(geometry = st_sfc(x), crs = 31983)
  sf_object_wgs84 <- st_transform(sf_object, 4326)
  st_coordinates(st_centroid(sf_object_wgs84))[1, "X"]
}

# Custom function to convert text after the period to lowercase
change_after_dot <- function(name) {
  if (str_detect(name, "\\.")) {
    parts <- str_split(name, "\\.", n = 2, simplify = TRUE)
    return(paste0(parts[1], ".", tolower(parts[2])))
  } else {
    return(name)
  }
}

ra_shape_path <- "/home/marcelo/Downloads/temp/shapefile_ra.shp"
amostra_kml_path <- "/home/marcelo/Downloads/temp/amostra.kml"
amostra_shape_path <- "/home/marcelo/Downloads/temp/amostra.shp"

shapefile_ra <- read_sf(ra_shape_path)

# SAMPLE
# Our 94 public lands samples was selected using stratified sampling
# The sample was generated by our own developed package ('marcelosousa-utexas/AuditSampling')
# The column cipu (Geoportal id) and pu_end_car and st_area_sh was automatically generated
# from "sisgepat_vs_geoporta.R" and was manually checked from the auditors and validated with Seduh 
# Secretariat for Urban Development and Housing (Seduh)
# The column "Área Polígono (st_area_sh)" is the total area sum of all cipu in a line
# For instance, the TOMBAMENTO id 1913/93 has two polygons cipu ids 223034;227520 associated with it
# and "Área Polígono (st_area_sh)" is the sum of both polygons 
df_sample <- read_excel("/home/marcelo/Downloads/PT - Analise Amostra Imoveis_Seduh.xls", sheet = "AMOSTRA - AUDITORIA", col_names = TRUE)

#SISGEPAT (public properties dataset from Federal District Government)
df_sisgepat <- read_excel("/home/marcelo/Downloads/PTXX - Relatório Sisgepat - Incorporados.xlsx", sheet = "RELATORIO_SISGEPAT_TABELA", col_names = TRUE)
df_sisgepat <- df_sisgepat %>%
  filter(TIPO != "MOBILIÁRIO URBANO" & PAT_TERRENOS.SR_CODIGO == 0)

#IPTU (tax dataset of public properties from the Federal District Government)
df_iptu <- read_excel("/home/marcelo/Downloads/tabela_tcdf_v5 (1).xlsx", sheet = "Sheet1", col_names = TRUE)
df_iptu$Insc_IPTU_a_Verificar <- df_iptu$INSCRICAO_IMOVEL

#GEOPORTAL
sf_geoportal <- read_sf('/home/marcelo/Downloads/lotes_registrados/lotes_registrados.shp')
sf_geoportal$cipu <- as.character(sf_geoportal$pu_cipu)

# 1:n relantionship: 1 public property id ('TOMBAMENTO') in SisGepat could has more than one polygonal id (cipu) associated
# 1:1 relantionship: we add a line for each cipu separed by a comma, keeping all the remaing information in SisGepat in each added line
sf_sample_geoportal <- df_sample %>%
  separate_rows(cipu, pu_end_car, sep = ";") %>%
  mutate(cipu = str_trim(cipu), pu_end_car = str_trim(pu_end_car))

# Adding Geoportal features to SisGepat sample
sf_sample_geoportal_merged <- sf_sample_geoportal %>%
  left_join(sf_geoportal, by = "cipu") %>%
  select(cipu, TOMBAMENTO, everything())

# Adding RA name to the shapefile
sf_sample_geoportal_merged <- sf_sample_geoportal_merged %>%
  mutate(ra_geo_num = as.character(pu_ra)) %>%
  left_join(as.data.frame(shapefile_ra) %>%
              select(ra_geo_num, ra_geo), 
            by = "ra_geo_num")

# Assuming 'df' is your data frame
# 
sf_sample_geoportal_united <- sf_sample_geoportal_merged %>%
  group_by(TOMBAMENTO) %>%
  summarize(geometry = st_union(geometry),
            pu_cipu = paste(unique(pu_cipu), collapse = ";"),
            ra_name = paste(unique(ra_geo), collapse = ";"),
            pu_end_car = paste(unique(pu_end_car.x), collapse = ";"),
            pu_end_usu = paste(unique(pu_end_usu), collapse = ";"),
            qd_area = sum(qd_area),
            area_total_poligono = sum(st_area_sh)) %>%
  ungroup() %>%
  mutate(pu_cipu = ifelse(pu_cipu == "NA",NA, pu_cipu),
         ra_name = ifelse(ra_name == "NA",NA, ra_name),
         pu_end_car = ifelse(pu_end_car == "NA",NA, pu_end_car),
         pu_end_usu = ifelse(pu_end_usu == "NA",NA, pu_end_usu)
  )

# identify the column features from GeoPortal adding a suffix .GEOPORTAL
sf_sample_geoportal_united <- sf_sample_geoportal_united %>%
  rename_with(~ifelse(.x != names(sf_sample_geoportal_united)[1:2], paste0("GEOPORTAL.", .x), .x))


# 1:n relantionship: 1 public property id ('TOMBAMENTO') in SisGepat could has more than one IPTU (fiscal id)
# 1:1 relantionship: we add a line for each IPTU id separed by a comma, keeping all the remaining information in SisGepat in each added line
df_sample_iptu <- df_sample %>%
  separate_rows(Insc_IPTU_a_Verificar, sep = ";") %>%
  mutate(Insc_IPTU_a_Verificar = str_trim(Insc_IPTU_a_Verificar))

# Adding IPTU features to SisGepat sample
df_sample_iptu_merged <- df_sample_iptu %>%
  left_join(df_iptu, by = "Insc_IPTU_a_Verificar") %>%
  select(Insc_IPTU_a_Verificar, TOMBAMENTO, TERRENO_ITBI)
#select(Insc_IPTU_a_Verificar, TOMBAMENTO, everything())

#API is restricted now, not working to public access
df_sample_iptu_merged <- df_sample_iptu_merged %>%
  rowwise() %>%
  mutate(
    BC_IPTU = NA,
    NOME_PROPRIETARIO = NA,
    AREA_TERRENO = NA
    #BC_IPTU = get_iptu_info(Insc_IPTU_a_Verificar)$BC_IPTU,
    #NOME_PROPRIETARIO = get_iptu_info(Insc_IPTU_a_Verificar)$NOME_PROPRIETARIO,
    #AREA_TERRENO = get_iptu_info(Insc_IPTU_a_Verificar)$AREA_TERRENO    
  ) %>%
  ungroup()

# Agregate all BC_IPTU and TERRENO_ITBI sum
df_sample_iptu_united <- df_sample_iptu_merged %>%
  group_by(TOMBAMENTO) %>%
  summarize(AREA_TERRENO = sum(AREA_TERRENO),
            BC_IPTU = sum(round(sum(BC_IPTU) , 2)),
            #BC_IPTU = sum(BC_IPTU, na.rm = TRUE),
            TERRENO_ITBI = sum(round(TERRENO_ITBI), 2),
            #NOME_PROPRIETARIO = as.character(NOME_PROPRIETARIO)
            INSCRICAO_IPTU = paste(unique(Insc_IPTU_a_Verificar), collapse = ";"),
            NOME_PROPRIETARIO = paste(unique(NOME_PROPRIETARIO), collapse = ";")) %>%
  ungroup() %>%
  mutate(NOME_PROPRIETARIO = ifelse(NOME_PROPRIETARIO == "NA",NA, NOME_PROPRIETARIO))

# identify the column features from IPTU adding a suffix .SUREC
df_sample_iptu_united <- df_sample_iptu_united %>%
  rename_with(~ifelse(.x != names(df_sample_iptu_united)[1], paste0("SUREC.", .x), .x))


#names(df_sisgepat)
#paste(names(df_sisgepat), collapse = ", ")

df_sisgepat <- df_sisgepat %>%
  select(TOMBAMENTO, ENDERECO, CIDADE, SI_DESCRICAO, LOCALIZACAO, PAT_TERRENOS.MATRICULA_NUM, PAT_TERRENOS.OFICIO_NUM, PAT_TERRENOS.OCUPACAO, PAT_TERRENOS.METRAGEM_NUM, TIPO, Total)

names(df_sisgepat) <- c("TOMBAMENTO", "ENDERECO", "CIDADE", "SI_DESCRICAO", "LOCALIZACAO", "NRO_MATRICULA", "NRO_OFICIO", "OCUPACAO", "METRAGEM_TERRENO", "TIPO", "VALOR")

# identify the column features from SisGepat adding a suffix .SISGEPAT
df_sisgepat <- df_sisgepat %>%
  rename_with(~ifelse(.x != names(df_sisgepat)[1], paste0("SISGEPAT.", .x), .x))

# Full dataset after adding information from the 3 datasets together: SisGepat, Geoportal and IPTU dataset
# Initiating with the shapefile to not lose geometry information
full_df <- sf_sample_geoportal_united %>%  # Start with the sf object
  left_join(df_sample %>% select(TOMBAMENTO), by = "TOMBAMENTO") %>%
  left_join(df_sisgepat, by = "TOMBAMENTO") %>%
  left_join(df_sample_iptu_united, by = "TOMBAMENTO")


# Adding latitude and longitude from GeoPortal as the 'centroid' of the polygon
full_df <- full_df %>%
  mutate(
    GEOPORTAL.latitude = map_dbl(geometry, ~{
      lat <- get_latitude(.)
      if (is.nan(lat)) NA else lat
    }),
    GEOPORTAL.longitude = map_dbl(geometry, ~{
      lon <- get_longitude(.)
      if (is.nan(lon)) NA else lon
    })
  )

full_df$name <- full_df$TOMBAMENTO
full_df$description <- full_df$SISGEPAT.OCUPACAO
colnames(full_df)[1] <- "SISGEPAT.TOMBAMENTO"

# Apply the custom function to each column name
full_df <- full_df %>%
  rename_with(~ sapply(.x, change_after_dot))


st_write(full_df, "/home/marcelo/Downloads/temp/amostra.kml", driver = "KML")

# Need to reduce the column header names to fit in a shapefile
full_df$geometry <- st_cast(full_df$geometry, "MULTIPOLYGON")
colnames(full_df) <- gsub("^SISGEPAT.|^GEOPORTAL.|^SUREC.", "", colnames(full_df))
colnames(full_df) <- substr(colnames(full_df), 1, 10)
full_df$terreno_it <- as.character(full_df$terreno_it)  # Keep 2 decimal places

st_write(full_df, '/home/marcelo/Downloads/temp/amostra.shp', append = FALSE)

