write_vi_data <- function(ind) {
  
  date <- sort(
    unique(
      read.fst("Data/Processed_Data/date_table.fst", as.data.table = TRUE)[,date]
    )
  )[ind]
  
  
  if(file.exists(paste0("Data/Processed_Data/Weekly_Patterns/select_", date, ".fst"))) {
    sg <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/select_", date, ".fst"),
      as.data.table = TRUE
    )
    sg_exp <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/long_", date, ".fst"),
      as.data.table = TRUE
    )
  } else {
    stop("No file for week starting ", date, " found.")
  }
  
  cbsa <- read_excel("Data/Census_Data/census_msa.xls", skip = 2)
  
  msa <- cbsa %>%
    filter(
      `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" &
        `FIPS State Code` != 72 &
        !is.na(`State Name`)
    ) %>%
    transmute(
      msa = `CBSA Title`,
      county = paste0(`FIPS State Code`,`FIPS County Code`)
    ) %>%
    as.data.table()
  
  sg <- sg[str_sub(sg_wp__poi_cbg,1,5) %in% msa$county &
             sg_wp__parent_placekey == ""
           ]
  
  ### Merge expanded SG data with census data
  
  race <- fread(
    "Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
    colClasses = c("census_block_group" = "character")
  )
  
  race <- race[, .(cbg = census_block_group,
                   wht = B02001e2/B02001e1,
                   pop = B02001e1)
               ]
  
  race_msa <- race[, county := substr(cbg, 1, 5) 
                   ][msa, on = "county"
                     ]
  
  sg_exp <- sg_exp[race, on = 'cbg', wht := i.wht
                   ][, ":="(wht_maj = wht>0.5,
                            county = substr(cbg, 1, 5))
                     ][sg, on = 'placekey', poi_cbg := i.sg_wp__poi_cbg
                        ][!is.na(poi_cbg) & !is.na(wht_maj)
                          ][, poi_county := substr(poi_cbg, 1, 5)
                            ]
  
  ### Create POI-level data that counts number of (white) visitors/visits
  
  sg_poi <- sg_exp[, .(wht_sum = sum(visitor_count*wht_maj),
                       visitors = sum(visitor_count),
                       poi_county = unique(poi_county)),
                   by = placekey
                   ][sg, on = 'placekey', visits := i.sg_wp__raw_visit_counts
                     ][, wht_frac := wht_sum / visitors
                       ]
  
  sg_poi_inmsa <- sg_exp[msa, on = "county", ":="(vis_msa = i.msa)
                         ][msa, on = c("poi_county" = "county"), poi_msa := i.msa 
                            ][vis_msa == poi_msa 
                              ][, .(wht_sum_inmsa = sum(visitor_count*wht_maj),
                                    visitors_inmsa = sum(visitor_count)), 
                                by = placekey
                                ]
  
  sg_poi[sg_poi_inmsa,
         on = "placekey",
         ":="(wht_sum_inmsa = i.wht_sum_inmsa,
              visitors_inmsa = i.visitors_inmsa)
         ]
  
  ### Merge Data on Number of Devices and Population Size
  
  sg_home <- read_fst(
    paste0("Data/Processed_Data/Weekly_Home/home_", date, ".fst"),
    as.data.table = TRUE
  )
  
  sg_home <- sg_home[substr(census_block_group, 1, 5) %in% msa$county,
                     .(cbg = census_block_group,
                       tract = str_sub(census_block_group,1,11),
                       cbg_devices = number_devices_residing)
                     ][race, on = 'cbg', pop := i.pop
                       ][, ':='(tract_devices = sum(cbg_devices),
                                tract_pop = sum(pop)),
                         by = tract
                         ]
  
  sg_home_county <- sg_home[, .(county = str_sub(tract, 1, 5),
                                pop = pop)
                            ][, .(pop = sum(pop)),
                              by = county
                              ]

  msa[sg_home_county, on = 'county', pop := i.pop]
  msa[, pop := sum(pop), by = msa]
  
  sg_vi_msa <- sg_poi[msa, 
                      on = .(poi_county = county), 
                      ':='(msa = i.msa,
                           pop = i.pop)
                      ][!is.na(visitors_inmsa)
                        ][, .(P_bb = weighted.mean(
                                wht_frac,
                                w = wht_sum_inmsa*visits/visitors
                                ),
                              P_ab = weighted.mean(
                                wht_frac,
                                w = (visitors_inmsa-wht_sum_inmsa)*visits/visitors
                              ),
                              V_A = sum(wht_sum_inmsa*visits/visitors),
                              V = sum(visitors_inmsa*visits/visitors),
                              P_bb_n = weighted.mean(
                                wht_frac, 
                                w = wht_sum_inmsa
                              ),
                              P_ab_n = weighted.mean(
                                wht_frac, 
                                w = visitors_inmsa-wht_sum_inmsa
                              ),
                              V_A_n = sum(wht_sum_inmsa),
                              V_n = sum(visitors_inmsa)
                              ),
                          by = msa
                          ][,':='(VI = P_bb-P_ab,
                                  VI_n = P_bb_n-P_ab_n)
                            ]
  
  ### Save results in one table
  
  sg_vi_msa[msa,
            on = "msa", 
            ":="(date_range_start = date,
                 pop = i.pop)
            ]
  
  sg_vi_msa <- rbindlist(
    list(
      sg_vi_msa,
      read_fst("Data/Processed_Data/indices_vi.fst", as.data.table = TRUE)
    )
  )
  
  write_fst(sg_vi_msa, "Data/Processed_Data/indices_vi.fst")
  
  cat("\r", as.character(date), sep = "")
}

write_vi_data_ua <- function(ind) {
  
  date <- sort(
    unique(
      read.fst("Data/Processed_Data/date_table.fst", as.data.table = TRUE)[,date]
    )
  )[ind]
  
  
  if(file.exists(paste0("Data/Processed_Data/Weekly_Patterns/select_", date, ".fst"))) {
    sg <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/select_ua_", date, ".fst"),
      as.data.table = TRUE
    )
    sg_exp <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/long_ua_", date, ".fst"),
      as.data.table = TRUE
    )
  } else {
    stop("No file for week starting ", date, " found.")
  }
  
  ua <- as.data.table(readRDS("Data/Processed_Data/ua_cbg.RDS"))
  
  sg <- sg[sg_wp__poi_cbg %in% ua$CBG10 &
             sg_wp__parent_placekey == ""
           ]
  
  ### Merge expanded SG data with census data
  
  race <- fread(
    "Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
    colClasses = c("census_block_group" = "character")
  )
  
  race <- race[, .(cbg = census_block_group,
                   wht = B02001e2/B02001e1,
                   pop = B02001e1)
               ]
  
  sg_exp <- sg_exp[race, on = 'cbg', wht := i.wht
                   ][, ":="(wht_maj = wht>0.5)
                     ][sg, on = 'placekey', poi_cbg := i.sg_wp__poi_cbg
                        ][!is.na(poi_cbg) & !is.na(wht_maj)
                          ]
  
  ### Create POI-level data that counts number of (white) visitors/visits
  
  sg_poi <- sg_exp[, .(wht_sum = sum(visitor_count*wht_maj),
                       visitors = sum(visitor_count),
                       poi_cbg = unique(poi_cbg)),
                   by = placekey
                   ][sg, on = 'placekey', visits := i.sg_wp__raw_visit_counts
                     ][, wht_frac := wht_sum / visitors
                       ]
  
  sg_poi_inua <- sg_exp[ua, on = c("cbg" = "CBG10"), vis_ua := i.UACE10
                         ][ua, on = c("poi_cbg" = "CBG10"), poi_ua := i.UACE10
                           ][vis_ua == poi_ua
                             ][, .(wht_sum_inua = sum(visitor_count*wht_maj),
                                   visitors_inua = sum(visitor_count)), by = placekey
                               ]
                         
                         
  sg_poi <- sg_poi[sg_poi_inua,
                   on = "placekey",
                   ":="(wht_sum_inua = i.wht_sum_inua,
                        visitors_inua = i.visitors_inua)
                   ]
                         
  sg_home <- read_fst(paste0("Data/Processed_Data/Weekly_Patterns/home_", date, ".fst"), as.data.table = TRUE)
                         
  sg_home <- sg_home[census_block_group %in% ua$CBG10,
                     .(cbg = census_block_group,
                       tract = str_sub(census_block_group,1,11),
                       cbg_devices = number_devices_residing)
                     ][race, on = 'cbg', pop := i.pop
                       ][, ':='(tract_devices = sum(cbg_devices),
                                tract_pop = sum(pop)),
                         by = tract
                         ]
                         
                         
  ua[race, on = c("CBG10" = 'cbg'), pop := i.pop]
  ua[, pop := sum(pop, na.rm = TRUE), by = UACE10]
                         
                         
  sg_vi_ua <- sg_poi[ua, 
                     on = .(poi_cbg = CBG10), 
                     ':='(ua = i.UACE10,
                          pop = i.pop)
                      ][!is.na(visitors_inua)
                        ][, .(P_bb = weighted.mean(
                                wht_frac, 
                                w = wht_sum_inua*visits/visitors
                              ),
                              P_ab = weighted.mean(
                                wht_frac,
                                w = (visitors_inua-wht_sum_inua)*visits/visitors
                              ),
                              V_A = sum(wht_sum_inua*visits/visitors),
                              V = sum(visitors_inua*visits/visitors),
                              P_bb_n = weighted.mean(
                                wht_frac, 
                                w = wht_sum_inua
                              ),
                              P_ab_n = weighted.mean(
                                wht_frac, 
                                w = visitors_inua-wht_sum_inua
                              ),
                              V_A_n = sum(wht_sum_inua),
                              V_n = sum(visitors_inua)),
                          by = ua
                          ][, ':='(VI = P_bb-P_ab,
                                   VI_n = P_bb_n-P_ab_n)
                            ]
                                           
                         
                         
                         
  ### Save results in one table
                         
  sg_vi_ua[ua, 
           on = c("ua" = "UACE10"),
           ":="(date_range_start = date,
                pop = i.pop)
           ]
                         
  sg_vi_ua <- rbindlist(
    list(
      sg_vi_ua,
      read_fst("Data/Processed_Data/indices_vi_ua.fst",
               as.data.table = TRUE)
    )
  )
  
  write_fst(sg_vi_ua, "Data//Processed_Data/indices_vi_ua.fst") 
  
  cat("\r", as.character(date), sep = "")  
}

write_vi_data_naics <- function(ind) {
  
  date <- sort(
    unique(
      read.fst("Data/Processed_Data/date_table.fst", as.data.table = TRUE)[,date]
    )
  )[ind]
  
  
  if(file.exists(paste0("Data/Processed_Data/Weekly_Patterns/select_", date, ".fst"))) {
    sg <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/select_", date, ".fst"),
      as.data.table = TRUE
    )
    sg_exp <- read_fst(
      paste0("Data/Processed_Data/Weekly_Patterns/long_", date, ".fst"),
      as.data.table = TRUE
    )
  } else {
    stop("No file for week starting ", date, " found.")
  }
  
  cbsa <- readxl::read_excel("Data/Census_Data/census_msa.xls", skip = 2)
  
  msa <- cbsa %>%
    filter(
      `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" &
        `FIPS State Code` != 72 &
        !is.na(`State Name`)
    ) %>%
    transmute(
      msa = `CBSA Title`,
      county = paste0(`FIPS State Code`,`FIPS County Code`)
    ) %>%
    as.data.table()
  
  sg <- sg[str_sub(sg_wp__poi_cbg,1,5) %in% msa$county &
             sg_wp__parent_placekey == ""
           ]
  
  ### Merge Places data
  
  places_o <- read_fst("Data/Processed_Data/outdoor_places.fst") %>%
    mutate(group = "outdoor")
  places_i <- read_fst("Data/Processed_Data/indoor_places.fst") %>%
    mutate(group = "leisure")
  places_g <- read_fst("Data/Processed_Data/grocery_places.fst") %>%
    mutate(group = "grocery")
  
  places <- rbindlist(
    list(
      places_o,
      places_i,
      places_g
    )
  )[ , .(placekey,
         group)
     ]
  
  sg <- merge(places, sg, by = "placekey")
  sg_exp <- merge(places, sg_exp, by = "placekey") 
  
  rm(
    list = grep("places", ls(), value = TRUE)
  )
  
  ### Merge expanded SG data with census data
  
  race <- fread(
    "Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
    colClasses = c("census_block_group" = "character")
  )
  
  race <- race[, .(cbg = census_block_group,
                   wht = B02001e2/B02001e1,
                   pop = B02001e1)
               ]
  
  race_msa <- race[, county := substr(cbg, 1, 5) 
                   ][msa, on = "county"
                     ]
  
  sg_exp <- sg_exp[race, on = 'cbg', wht := i.wht
                   ][, ":="(wht_maj = wht>0.5,
                            county = substr(cbg, 1, 5))
                     ][sg, on = 'placekey', poi_cbg := i.sg_wp__poi_cbg
                        ][!is.na(poi_cbg) & !is.na(wht_maj)
                          ][, poi_county := substr(poi_cbg, 1, 5)
                            ]
  
  ### Create POI-level data that counts number of (white) visitors/visits
  
  sg_poi <- sg_exp[, .(wht_sum = sum(visitor_count*wht_maj),
                       visitors = sum(visitor_count),
                       poi_county = unique(poi_county),
                       group = unique(group)),
                   by = placekey
                   ][sg, on = 'placekey', visits := i.sg_wp__raw_visit_counts
                     ][, wht_frac := wht_sum / visitors
                       ]
  
  sg_poi_inmsa <- sg_exp[msa, on = "county", ":="(vis_msa = i.msa)
                         ][msa, on = c("poi_county" = "county"), poi_msa := i.msa 
                            ][vis_msa == poi_msa 
                              ][, .(wht_sum_inmsa = sum(visitor_count*wht_maj),
                                    visitors_inmsa = sum(visitor_count)), 
                                by = placekey
                                ]
  
  sg_poi[sg_poi_inmsa,
         on = "placekey",
         ":="(wht_sum_inmsa = i.wht_sum_inmsa,
              visitors_inmsa = i.visitors_inmsa)
         ]
  
  ### Merge Data on Number of Devices and Population Size
  
  sg_home <- read_fst(
    paste0("Data/Processed_Data/Weekly_Home/home_", date, ".fst"),
    as.data.table = TRUE
  )
  
  sg_home <- sg_home[substr(census_block_group, 1, 5) %in% msa$county,
                     .(cbg = census_block_group,
                       tract = str_sub(census_block_group,1,11),
                       cbg_devices = number_devices_residing)
                     ][race, on = 'cbg', pop := i.pop
                       ][, ':='(tract_devices = sum(cbg_devices),
                                tract_pop = sum(pop)),
                         by = tract
                         ]
  
  sg_home_county <- sg_home[, .(county = str_sub(tract, 1, 5),
                                pop = pop)
                            ][, .(pop = sum(pop)),
                              by = county
                              ]

  msa[sg_home_county, on = 'county', pop := i.pop]
  msa[, pop := sum(pop), by = msa]
  
  sg_vi_msa <- sg_poi[msa, 
                      on = .(poi_county = county), 
                      ':='(msa = i.msa,
                           pop = i.pop)
                      ][!is.na(visitors_inmsa)
                        ][, .(P_bb = weighted.mean(
                                wht_frac,
                                w = wht_sum_inmsa*visits/visitors
                                ),
                              P_ab = weighted.mean(
                                wht_frac,
                                w = (visitors_inmsa-wht_sum_inmsa)*visits/visitors
                              ),
                              V_A = sum(wht_sum_inmsa*visits/visitors),
                              V = sum(visitors_inmsa*visits/visitors),
                              P_bb_n = weighted.mean(
                                wht_frac, 
                                w = wht_sum_inmsa
                              ),
                              P_ab_n = weighted.mean(
                                wht_frac, 
                                w = visitors_inmsa-wht_sum_inmsa
                              ),
                              V_A_n = sum(wht_sum_inmsa),
                              V_n = sum(visitors_inmsa)
                              ),
                          by = list(msa, group)
                          ][,':='(VI = P_bb-P_ab,
                                  VI_n = P_bb_n-P_ab_n)
                            ]
  
  ### Save results in one table
  
  sg_vi_msa[msa,
            on = "msa", 
            ":="(date_range_start = date,
                 pop = i.pop)
            ]
  
  sg_vi_msa <- rbindlist(
    list(
      sg_vi_msa,
      read_fst("Data/Processed_Data/indices_vi_naics.fst", as.data.table = TRUE)
    )
  )
  
  write_fst(sg_vi_msa, "Data/Processed_Data/indices_vi_naics.fst")
  
  cat("\r", as.character(date), sep = "")
}