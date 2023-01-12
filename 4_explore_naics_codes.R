# List Places data file names
flist <- list.files(
  "Data/SafeGraph_Data/Places", 
  recursive = TRUE, 
  full.names = TRUE
)
flist <- grep("your", flist, value = TRUE)

# Create one table with all places
places <- lapply(
  flist,
  function(f) {
    fread(
      f, 
      select = c(
        "placekey",
        "parent_placekey",
        "location_name",
        "top_category",
        "sub_category",
        "naics_code",
        "category_tags"
      )
    )[parent_placekey == ""]
  }
)

places <- rbindlist(places)

write_fst(places, "Data/Processed_Data/places.fst")

places <- read_fst("Data/Processed_Data/places.fst") %>%
  mutate(naics_code = str_pad(naics_code, 6, "right", "0")) %>%
  filter(!is.na(naics_code))

tags <- readxl::read_excel(
  # Downloaded from SG website
  "Data/SafeGraph_Data/sg_category_tags.xlsx" 
) 

### Determine outdoor leisure POI ###

# 2017 NAICS can be found here
#   https://www.census.gov/naics/
#
#   71: Arts, Entertainment, and Recreation
naics_71 <- filter(places, str_starts(naics_code, "71"))
tags_71 <- filter(tags, str_starts(naics_code, "71")) 

# All tags related to outdoor activities
#   Tags commented out relate to indoor
#   POI more often than not upon closer
#   inspection
outdoor_tags <- c(
  #"Baseball Fields","Beaches", "Bike Rentals", "Boat Charters",
  "Boat Tours",
  "Bocce Ball",
  "Bungee Jumping",
  "Campgrounds",
  "Community Garden",
  "Country Club",
  #"Day Camp",
  "Disc Golf",
  "Diving",
  "Farm",
  "Fishing",
  "Fly Boarding",
  #"Football",
  "Freediving",
  "Golf",
  "Hang Gliding",
  #"Historical Tours",
  "Horse Boarding",
  "Horse Racing",
  "Horseback Riding",
  "Hunting",
  "Jet Skis",
  "Kite Boarding",
  "Lakes",
  "Mini Golf",
  "Mountain Biking",
  "Paddle Boarding",
  "Paragliding",
  "Parasailing",
  "Petting Zoo",
  "Playground",
  "Rafting",
  "Ranch",
  #"Rock Climbing",
  "Scuba",
  "Skate Parks",
  "Sky Diving",
  "Snorkeling",
  "Summer Camp",
  "Surfing",
  "Surfing Classes",
  #"Swimming Lessons", "Swimming Pools",
  "Tubing",
  #"Walking Tours",
  "Water Park",
  "Waterparks",
  "Zipline"
)

# Multiple outdoor tags are
#   separated by comma,
#   create a logical vector
#   that indicates whether
#   a place has an outdoor 
#   tag
naics_71_out_logical <- lapply(
  naics_71$category_tags, 
  function(x) { 
    any(
      str_split(x, ",", simplify = TRUE) %in%
        outdoor_tags
    )
  }
)

naics_71_outdoor <- naics_71[unlist(naics_71_out_logical),]

# Outdoor tags from 6-digit NAICS
subcat_outdoor_tags <- c(
  "Nature Parks and Other Similar Institutions",
  "Zoos and Botanical Gardens",
  "Marinas",
  "Amusement and Theme Parks",
  "Golf Courses and Country Clubs",
  "Racetracks"
)

naics_71_outdoor <- naics_71_outdoor %>%
  filter(
    # Include all POI with Safegraph 
    #  outdoor tags as listed by `outdoor_tags`
    #  AND from the following NAICS code categories
    sub_category %in%
      c(
        "All Other Amusement and Recreation Industries",
        "Casinos (except Casino Hotels)", 
        "Historical Sites", 
        "Skiing Facilities"
      ) 
  ) %>%
  bind_rows(
    # Add to these all POI with NAICS Codes
    #   as listed by subcat_outdoor_tags
    filter(
      naics_71,
      sub_category %in% 
        subcat_outdoor_tags
    )
  ) %>%
  filter(!duplicated(placekey)) %>%
  arrange(naics_code)
                                
write_fst(naics_71_outdoor, "Data/Processed_Data/outdoor_places.fst")

### Determine indoor leisure POI  ###

naics_out <- read_fst("Data/Processed_Data/outdoor_places.fst")

# All NAICS codes starting with these
#   digits relate to leisure POI
naics_leis <- c(
  "448",
  "451",
  "4522",
  "4531",
  "45322",
  "45392",
  "7111",
  "7112",
  "712",
  "713",
  "7211",
  "7212",
  "7224",
  "7225"
)

# Subset data for efficiency
places_leisure <- places %>% filter(
  str_starts(naics_code, "44") | 
    str_starts(naics_code, "45") | 
    str_starts(naics_code, "71") | 
    str_starts(naics_code, "72")
)
    
# Determine matching indices
lind <- sapply(
  places_leisure$naics_code, 
  function(n) {
    for(l in naics_leis) {
      if(str_starts(n, l)) return(TRUE)
    }
    FALSE
  }
)

# Exclude Outdoor POI
places_leisure <- places_leisure[lind,] %>%
  filter(!(placekey %in% naics_out$placekey))

write_fst(places_leisure, "Data/Processed_Data/indoor_places.fst")

# Subset to grocery stores
places_grocery <- places %>% filter(str_starts(naics_code, "4451")) 

write_fst(places_grocery, "Data/Processed_Data/grocery_places.fst")
