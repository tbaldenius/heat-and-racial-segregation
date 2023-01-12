# Prepare some tables

# MSA table with
# - county code
# - state that has 
#     highest msa population
#     share for multi-state
#     MSAs
cbsa <- readxl::read_excel("Data/Census_Data/census_msa.xls", skip = 2)
race <- fread(
  "Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
  colClasses = c("census_block_group" = "character")
)
race <- race[, .(pop = B02001e1,
                 county = substr(census_block_group, 1, 5))
][, .(pop = sum(pop, na.rm = TRUE)), 
  by = county
]
msa <- cbsa %>%
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" &
           !(`FIPS State Code` %in% c("15","02","72")) &
           !is.na(`State Name`)) %>%
  mutate(county = paste0(`FIPS State Code`, `FIPS County Code`)) %>%
  left_join(race, by = "county") %>%
  group_by(`CBSA Code`, `State Name`) %>%
  mutate(statepop = sum(pop)) %>%
  group_by(`CBSA Code`) %>%
  filter(statepop == max(statepop)) %>%
  filter(!duplicated(`CBSA Code`))

# VI index table
indices_vi_msa <- read_fst("Data/Processed_Data/indices_vi.fst") %>%
  group_by(msa) %>%
  # Take maximum population, 
  #   population size varies 
  #   because sometimes CBGs 
  #   are missing in Patterns 
  #   when there are no visits 
  #   from them
  mutate(pop = max(pop)) %>%
  # March 11 Covid Declared Pandemic#
  #   March 13 Travel Ban on Non-US Citizens
  filter(date_range_start < as.IDate("2020-03-09")) %>%  
  inner_join(
    dplyr::select(msa, `CBSA Title`, `CBSA Code`, `State Name`),
    by = c("msa" = "CBSA Title")
  )

# VI index table with
#   variables for fixed
#   effects and excluding
#   MSAs without full
#   temporal coverage
#   in terms of allowing
#   computing VI
indices_vi <- indices_vi_msa  %>%
  # Week s.t. it is a sequence
  #   from 1 to 114
  mutate(week = (as.numeric(date_range_start)+3)%/%7-2504,
         fips = `CBSA Code`,
         date = date_range_start,
         # Calendar week
         cweek = week(date_range_start),
         state = `State Name`,
         quarter = quarter(date_range_start),
         month = month(date_range_start),
         year = year(date_range_start)) %>%
  group_by(msa) %>%
  filter(!any(is.na(VI))) %>%
  ungroup()

# Use MSA shapefile to get lat/lon
#   for Conley variance
#   Source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
msa_sf <- rgdal::readOGR(
  "Data/Census_Data/tl_2020_us_cbsa/tl_2020_us_cbsa.shp", 
  "tl_2020_us_cbsa"
)

indices_vi <- indices_vi %>%
  left_join(
    dplyr::select(msa_sf@data, NAME, INTPTLAT, INTPTLON),
    by = c("msa" = "NAME")
  ) %>%
  mutate(
    INTPTLAT = as.numeric(INTPTLAT),
    INTPTLON = as.numeric(INTPTLON)
  ) %>%
  rename(
    latitude = INTPTLAT,
    longitude = INTPTLON
  )

# Prepare NAICS subset data
indices_naics <- read_fst("Data/Processed_Data/indices_vi_naics.fst")
indices_naics <- indices_naics %>%
  filter(date_range_start < as.IDate("2020-03-08")) %>%
  inner_join(
    dplyr::select(msa, `CBSA Title`, `CBSA Code`, `State Name`),
    by = c("msa" = "CBSA Title")
  ) %>%
  mutate(week = (as.numeric(date_range_start)+3)%/%7-2504,
         cweek = week(date_range_start),
         fips = `CBSA Code`,
         date = date_range_start,
         state = `State Name`,
         quarter = quarter(date_range_start),
         month = month(date_range_start),
         year = year(date_range_start)) %>%
  dplyr::select(!c(date_range_start, `CBSA Code`, `State Name`)) %>%
  rename(naics = group) %>%
  group_by(msa) %>%
  mutate(pop = max(pop)) %>%
  filter(!any(is.na(VI))) %>%
  ungroup() %>%
  left_join(
    dplyr::select(msa_sf@data, NAME, INTPTLAT, INTPTLON), 
    by = c("msa" = "NAME")
  ) %>%
  mutate(
    INTPTLAT = as.numeric(INTPTLAT),
    INTPTLON = as.numeric(INTPTLON)
  ) %>%
  rename(
    latitude = INTPTLAT,
    longitude = INTPTLON
  )
indices_outdoor <- filter(indices_naics, naics == "outdoor")
indices_indoor <- filter(indices_naics, naics == "leisure")
indices_grocery <- filter(indices_naics, naics == "grocery")

# UA-level data
indices_vi_ua <- read_fst("Data/Processed_Data/indices_vi_ua.fst")

ua <- readRDS("Data/Processed_Data/ua_cbg.RDS") %>% 
  select(-CBG10) %>%
  unique()

uac_sf <- rgdal::readOGR(
  "Data/Census_Data/tl_2021_us_uac10/tl_2021_us_uac10.shp", 
  "tl_2021_us_uac10"
)

indices_vi_ua <- indices_vi_ua %>%
  filter(date_range_start < as.IDate("2020-03-08")) %>%
  inner_join(ua, by = c(ua = "UACE10")) %>%
  mutate(
    week = (as.numeric(date_range_start)+3)%/%7-2504,
    fips = ua,
    date = date_range_start,
    state = state,
    quarter = quarter(date_range_start),
    month = month(date_range_start),
    year = year(date_range_start),
    cweek = week(date_range_start)
  ) %>%
  group_by(ua) %>%
  filter(
    !any(is.na(VI)) &
      !any(V_n-V_A_n < 100)
  ) %>%
  left_join(
    dplyr::select(uac_sf@data, UACE10, INTPTLAT10, INTPTLON10, NAME10),
    by = c("ua" = "UACE10")
  ) %>%
  mutate(
    latitude = as.numeric(INTPTLAT10),
    longitude = as.numeric(INTPTLON10)
  ) %>%
  select(-INTPTLAT10, -INTPTLON10)

# Binned MSA-level weather data
TMAXBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tmaxbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)
TAVGBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tavgbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)
PPTBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_pptbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504,
         pptbin_1 = NULL)
HIBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_hibin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)
HWI_BINARYBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_hwi_binarybin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)

# Mean MSA-level weather data
TMAX <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tmax.fst")
TMIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tmin.fst")
PPT <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_ppt.fst")
HI <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_hi.fst")
HWI_BINARY <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_hwi_binary.fst")

# Fixed effects specs
main_fe <-  append(append(list(c("fips", "week")), cross2(list(c("fips", "week")), c("state^quarter", "state^month", "state^month^year", "state^cweek"))), list(c("fips", "state^week^year")))
interact_fe <- append(cross2(list(c("fips^month")), c("week", "state^week")), list(c("fips^cweek", "week"), c("fips^cweek", "state^week")))

# Prepare shapefiles
state_sf <- rgdal::readOGR(
  "Data/Census_Data/tl_2021_us_state/tl_2021_us_state.shp", 
  "tl_2021_us_state"
)
state_sf <- state_sf[!(state_sf@data$STATEFP %in% 
                         c("15","02","60", "66", "69", "72", "78")), ] 
msa_sf <- msa_sf[msa_sf@data$LSAD == "M1",]
ua_sf <- uac_sf[!is.na(uac_sf@data$UATYP10),]
ua_sf <- ua_sf[ua_sf@data$UATYP10 == "U",]