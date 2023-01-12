### Create two datasets sg_select and sg_long   ###
###   1. Choose only POI in MSA counties        ###
###   2. Create sg_select as weekly dataset     ###
###       with all columns but relevant ones    ###
###       excluded                              ###
###   3. Create sg_long, where the CBG JSON     ###
###       is read                               ###

cbsa <- readxl::read_excel("Data/Census_Data/census_msa.xls", skip = 2)

msa <- cbsa %>%
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") %>%
  transmute(
    msa = `CBSA Title`,
    county = paste0(`FIPS State Code`,`FIPS County Code`)
  ) %>%
  as.data.table()

# To be able to later compare to UAs
#   create a table with columns
#     - CBG
#     - UA
#     - State that majority of UA 
#         population lives in (UAs
#         may span multiple states)

# Statewise UA shapefile names from U.S. Census
#  https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
flist <- grep("2020", list.files("Data/Census_Data/ua_shp"), value = TRUE)
# Read data from state shapefile  from U.S. Census 
#  https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
state_sf <- foreign::read.dbf("Data/Census_Data/tl_2021_us_state/tl_2021_us_state.dbf")

ualist <- lapply(flist, function(f) { 
  # Read UA shapefile
  sf <- foreign::read.dbf(paste0("Data/Census_Data/ua_shp/", f, "/", f, ".dbf"))
  statename <- as.character(state_sf[state_sf$STATEFP == levels(sf$STATEFP10),]$NAME)
  cat(statename, "\n")
  if(levels(sf$STATEFP10) %in% c("15","02","60", "66", "69", "72", "78"))
    warning(paste0(statename, ": State not in CONUS"))
  # Only Urbanized Areas, no other urban area entities
  sf <- sf[!is.na(sf$UATYPE), ]
  sf <- sf[sf$UATYPE == "U", ]
  sf$CBG10 <- substr(sf$GEOID10, 1,12)
  sf <- sf[ , c("CBG10", "UACE10")]
  # Return CBG and its enclosing or intersecting UA
  #  (UA are block-level and may not fully cover CBGs)
  sf %>%
    group_by(UACE10) %>%
    summarise(CBG10 = unique(CBG10))
})

ua_cbg <- do.call(rbind, ualist)

race <- fread(
  "Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
  colClasses = c("census_block_group" = "character")
)
race <- race[, .(cbg = census_block_group,
                 wht = B02001e2/B02001e1,
                 pop = B02001e1)
             ]


ua_state <- merge(ua_cbg, race, by.x = "CBG10", by.y = "cbg")
ua_state$state <- substr(ua_state$CBG10,1,2)
ua_state <- ua_state %>%
  group_by(UACE10, state) %>%
  summarise(pop = sum(pop, na.rm = TRUE)) %>%
  arrange(desc(pop), .by_group = TRUE) %>%
  distinct(UACE10, .keep_all = TRUE) %>%
  mutate(pop = NULL)

ua_cbg <- merge(ua_cbg, ua_state)

saveRDS(ua_cbg, file = "/Users/Till/Documents/MCC/MA - Safegraph/R/Data/ua_cbg.RDS")

rm(ua_cbg)

# Now we can create the weekly data
ua <- readRDS("Data/Processed_Data/ua_cbg.RDS")

### Expand raw weekly patterns files to have one row by CBG by POI

sg_date <- read.fst("Data/Processed_Data/date_table.fst") %>% as.data.table()
all_dates <- sort(unique(sg_date[, date]))
all_dates <- all_dates[all_dates < as.IDate("2020-03-09")]

for(select_date in all_dates) {
  
  parts <- sg_date[date == select_date, part]
  
  sg_select_long <- lapply(
    parts,
    function(p) {
      
      # Keep only relevant columns of
      #  POI in MSA counties and remove 
      #  all enclosed POI
      sg_new <- fread(
        file = paste0("Data/SafeGraph_Data/Patterns/part", p, ".csv.gz"),
        colClasses = list(character = c("sg_wp__poi_cbg")),
        select = c(
          "date_range_start",
          "placekey",
          "sg_wp__parent_placekey",
          "sg_wp__poi_cbg",
          "sg_wp__raw_visit_counts",
          "sg_wp__visitor_home_cbgs"
        ),
        showProgress = FALSE
      )[  
        date_range_start == select_date &
          substr(sg_wp__poi_cbg, start = 1, stop = 5) %in% msa$county & # 1)
          sg_wp__parent_placekey == ""
      ]
      
      sg_new[ua_cbg, on = c("sg_wp__poi_cbg" = "CBG10"), ua := TRUE]
      
      # Expand JSON format column to table
      sg_expand <- lapply(
        gsub("\\\"\\\"", "\\\"", sg_new$sg_wp__visitor_home_cbgs),
        rjson::fromJSON
      )
      
      # Create a table that relates POI
      #  placekey to home CBGs and
      #  number of visitors from
      #  each CBG
      sg_long <- data.table(
        placekey = sg_new$placekey[
          rep(
            seq_along(sg_expand),
            lapply(sg_expand, length)
          )
        ],
        ua = sg_new$ua[
          rep(
            seq_along(sg_expand),
            lapply(sg_expand, length)
          )
        ],
        cbg = names(unlist(sg_expand)),
        visitor_count = unlist(sg_expand)
      )

      cat("\r", "Read", match(p, parts), "out of", length(parts), "parts")
      
      list(
        sg_new[, -"ua"],
        sg_long[, -"ua"],
        sg_new[ua == TRUE, -"ua"],
        sg_long[ua == TRUE, -"ua"])
    }
  )
  
  # 2)
  write_fst(
    rbindlist(
      lapply(sg_select_long, function(l) l[[1]])
    ),
    paste0("Data/Processed_Data/Weekly_Patterns/select_", select_date, ".fst")
  )
  
  write_fst(
    rbindlist(
      lapply(sg_select_long, function(l) l[[3]])
    ),
    paste0("Data/Processed_Data/Weekly_Patterns/select_ua_", select_date, ".fst")
  )
  
  # 3)
  write_fst(
    rbindlist(
      lapply(sg_select_long, function(l) l[[2]])
    ),
    paste0("Data/Processed_Data/Weekly_Patterns/long_", select_date, ".fst")
  )
  
  write_fst(
    rbindlist(
      lapply(sg_select_long, function(l) l[[4]])
    ),
    paste0("Data/Processed_Data/Weekly_Patterns/long_ua_", select_date, ".fst")
  )
  
  cat("\r", "Saved table for week starting", as.character(select_date))
}
