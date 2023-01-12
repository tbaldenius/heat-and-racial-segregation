source("Code/5a_VI_funs.R")

# Reading in the weekly data for
#   each of the isolation index 
#   tables is very inefficient,
#   this could all be done in one
#   go, but the present implementation
#   corresponds to the order of how
#   I tackled these derivations and I 
#   kept the functions this way for now

write_fst(data.table(), "Data/Processed_Data/indices_vi.fst")

for(ind in 1:114) {
  write_vi_data(ind)
}

write_fst(data.table(), "Data/Processed_Data/indices_vi_ua.fst")

for(ind in 1:114) {
  write_vi_data_ua(ind)
}

write_fst(data.table(), "Data/Processed_Data/indices_vi_naics.fst")

for(ind in 1:114) {
  write_vi_data_naics(ind)
}

# Store some additional 
#   characteristics
#   of MSAs and UAs

# Race
race <- fread("Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b02.csv",
              colClasses = c("census_block_group" = "character"))
race <- race[, .(cbg = census_block_group,
                 wht = B02001e2/B02001e1,
                 pop = B02001e1)]
# Household income
b19 <- fread("Data/Census_Data/safegraph_open_census_data_2019/data/cbg_b19.csv",
             colClasses = c("census_block_group" = "character"))
inc <- b19[, .(cbg = census_block_group,
               inc = B19025e1)]

# MSAs

cbsa <- readxl::read_excel("Data/Census_Data/census_msa.xls", skip = 2)
msa <- cbsa %>%
  filter(
    `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" &
      `FIPS State Code` != 72 &
      !is.na(`State Name`)
  ) %>%
  transmute(
    fips = `CBSA Code`,
    county = paste0(`FIPS State Code`,`FIPS County Code`)
  ) %>%
  as.data.table()

race[, ':='(county = str_sub(cbg, 1, 5))]
msa <- merge(msa, race, by = "county")
msa <- merge(msa, inc, by = "cbg")
msa[, ':='(wht_maj = wht > 0.5)]

# Columns:
#   Number of cbgs that
#     are nw
#   Corresponding group
#     population sizes
#   HH income per capita
#   NW CBG and pop shares
any_nw <- msa[!is.na(wht), 
              .(cbgs_nw = sum(1-wht_maj),
                cbgs = .N,
                pop_nw = sum((1-wht_maj)*pop),
                pop = sum(pop),
                inc = sum(inc, na.rm = TRUE)/(sum(pop))), 
              by = fips
              ][, ':='(cbg_nw_share = cbgs_nw/cbgs,
                       pop_nw_share = pop_nw/pop)
                ][order(pop_nw_share)
                  ]
write_fst(any_nw, "Data/Processed_Data/msa_cbg_nw.fst")

# UAs

ua <- readRDS("Data/Processed_Data/ua_cbg.RDS") %>% 
  as.data.table()
ua[race, on = c(CBG10 = 'cbg'), ':='(wht = i.wht,
                                     pop = i.pop)
   ][, ':='(wht_maj = wht > 0.5)
     ][inc, on = c(CBG10 = 'cbg'), ':='(inc = i.inc)
       ]

# Columns:
#   Number of cbgs that
#     are nw
#   Corresponding group
#     population sizes
#   HH income per capita
#   NW CBG and pop shares
any_nw <- ua[!is.na(wht), .(cbgs_nw = sum(1-wht_maj),
                 cbgs = .N,
                 pop_nw = sum((1-wht_maj)*pop),
                 pop = sum(pop),
                 inc = sum(inc, na.rm = TRUE)/(sum(pop))), by = UACE10
             ][, ':='(cbg_nw_share = cbgs_nw/cbgs,
                      pop_nw_share = pop_nw/pop)][order(pop_nw_share)]
write_fst(any_nw, "Data/Processed_Data/ua_cbg_nw.fst")

