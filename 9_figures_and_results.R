#### All figures and results     ####
####   in order of appearance    ####
#### Please run chronologically  ####

source("Code/9a_prepare_tables.R")
source("Code/9b_results_functions.R")

####                             ####
#### Figure       1              ####

# Across all CONUS MSAs
visits_ts <- indices_vi_msa %>% 
  group_by(date_range_start) %>%
  summarise(V = sum(V))
ggplot(visits_ts) +
  geom_line(aes(x=date_range_start, y=V/1000000)) +
  labs(
    y = "Visits (in millions)",
    x = "Week (Starting Date)"
  )
ggsave(
  file = "Figures/fig_1.pdf",
  width = 8,
  height = 3.5, 
  device = "pdf"
)

####                             ####
#### Figure       2              ####

# MSA/UA comparison
tm_all <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf)  +
  tm_fill(col = "MAP_COLORS") +
  tm_shape(ua_sf) +
  tm_fill(col = "black")
tmap_save(
  tm_all, 
  filename = paste0("Figures/fig_2.pdf"),
  height = 80, 
  width = 146.6, 
  units = "mm"
)

####                             ####
#### Figure       3              ####

# Mean VI
indices_vi_msa_mean <- indices_vi_msa  %>%
  group_by(msa, `CBSA Code`) %>%
  summarise(VI = mean(VI),
            pop = unique(pop))

msa_sf_vi_m <- msa_sf
msa_sf_vi_m <- msa_sf_vi_m[msa_sf@data$CBSAFP %in% 
                             indices_vi_msa_mean$`CBSA Code`,]
msa_sf_vi_m@data <- merge(
  msa_sf@data,
  indices_vi_msa_mean, 
  by.x = "GEOID", 
  by.y = "CBSA Code",
  sort = FALSE
)

tm_msa_vi <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf_vi_m) +
  tm_fill(
    col = "VI",
    title = "", 
    style = "cont",
    textNA = "Insufficient NW visits", 
    palette = "-magma"
  )

tmap_save(
  tm_msa_vi,
  filename = paste0("Figures/fig_3.pdf"),
  height = 80,
  width = 146.6,
  units = "mm"
)

####                             ####
#### Figure       4              ####

ggplot(
  filter(indices_vi_msa, pop > 8000000),
  aes(x = date_range_start, y = VI, color = msa)
) + 
  geom_line(
    stat = "smooth", 
    alpha = .5, 
    se  = FALSE, 
    size = 2, 
    span = 0.6
  ) + 
  geom_line() +
  labs(
    x = "Week (Starting Date)",
    color = element_blank()
  ) +
  scale_color_discrete(
    labels =  c("Chicago", "Los Angeles", "New York")
  )

ggsave(
  file = "Figures/fig_4.pdf",
  width = 8,
  height = 3.5,
  device = "pdf"
)

####                             ####
#### Figure       5              ####

indices_vi_mean_18 <- indices_vi_msa  %>%
  filter(date_range_start < as.Date("2018-05-01")) %>%
  group_by(msa) %>%
  summarise(VI  = mean(VI),
            pop = unique(pop))

# Scraped from Athey et al. (2021)
ath <- readxl::read_xlsx("Data/Processed_Data/athey_exp.xlsx")

ath_bal <- ath %>% 
  left_join(indices_vi_mean_18, by = c("msa")) %>%
  filter(!is.na(Exp) & !is.na(VI))

a <- min(ath_bal$Exp)
b <- min(ath_bal$VI)
coef_w <- summary(lm(
  VI ~ Exp, 
  ath_bal, 
  weights = pop
))$coefficients
ci <- c(
  coef_w[2, 1] - 1.96 * coef_w[2, 2],
  coef_w[2, 1] + 1.96 * coef_w[2, 2]
)

ggplot(ath_bal, aes(x=Exp, y=VI, size = pop/1000000)) +
  geom_point(alpha = .5) +
  geom_abline(
    slope = coef_w[2,1],
    intercept = coef_w[1,1], 
    linetype= "dashed"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Experienced isolation (Athey et al., 2021)",
       y = "Isolation in visits (own calculation)",
       size = "Population (in m)") +
  scale_size_continuous(range = c(0.5,10)) +
  coord_cartesian(xlim = c(0,0.6), ylim = c(0,0.6))

ggsave(
  file = "Figures/fig_5.pdf",
  width = 8,
  height = 3.5,
  device = "pdf"
)

####                             ####
#### Figure       6              ####

tmax_msa <- TMAX %>%
  filter(year < 2020) %>%
  group_by(fips) %>%
  summarise(tmax = mean(tmax))

msa_sf_tmax <- msa_sf
msa_sf_tmax <- msa_sf_tmax[msa_sf@data$CBSAFP %in% 
                             tmax_msa$fips,]
msa_sf_tmax@data <- merge(
  msa_sf_tmax@data,
  tmax_msa,
  by.x = "GEOID",
  by.y = "fips",
  sort = FALSE
)

tm_msa_tmax <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf_tmax) +
  tm_fill(
    col = "tmax",
    title = "",
    style = "cont",
    palette = "-Spectral",
    breaks = seq(5,35, by = 5)
  )

tmap_save(
  tm_msa_tmax,
  filename = paste0("Figures/fig_6.pdf"),
  height = 80,
  width = 146.6,
  units = "mm"
)


####                             ####
#### Figure       7              ####

ppt_msa <- PPT %>%
  filter(year < 2020) %>%
  group_by(fips) %>%
  summarise(ppt = mean(ppt))

msa_sf_ppt <- msa_sf
msa_sf_ppt <- msa_sf_ppt[msa_sf@data$CBSAFP %in% ppt_msa$fips,]
msa_sf_ppt@data <- merge(
  msa_sf_ppt@data,
  ppt_msa,
  by.x = "GEOID",
  by.y = "fips",
  sort = FALSE
)

tm_msa_ppt <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf_ppt) +
  tm_fill(
    col = "ppt",
    title = "",
    style = "cont",
    palette = "Blues"
  )

tmap_save(tm_msa_ppt, filename = paste0("Figures/fig_7.pdf"), height = 80, width = 146.6, units = "mm")

####                             ####
#### Figure       8              ####

hi_msa <- HI %>%
  filter(year < 2020) %>%
  group_by(fips) %>%
  summarise(hi = mean(hi))

msa_sf_hi <- msa_sf
msa_sf_hi <- msa_sf_hi[msa_sf@data$CBSAFP %in% 
                         hi_msa$fips,]
msa_sf_hi@data <-merge(
  msa_sf_hi@data,
  hi_msa,
  by.x = "GEOID",
  by.y = "fips",
  sort = FALSE
)

tm_msa_hi <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf_hi) +
  tm_fill(
    col = "hi",
    title = "",
    style = "cont",
    palette = "-Spectral",
    breaks = seq(5,35,by=5)
  )


tmap_save(
  tm_msa_hi,
  filename = paste0("Figures/fig_8.pdf"),
  height = 80,
  width = 146.6,
  units = "mm"
)

####                             ####
#### Figure       9              ####


hwi_msa <- HWI_BINARY %>%
  filter(year < 2020) %>%
  group_by(fips) %>%
  summarise(hwi = sum(hwi_binary>0.5)/2)

msa_sf_hwi <- msa_sf
msa_sf_hwi <- msa_sf_hwi[msa_sf@data$CBSAFP %in%
                           hwi_msa$fips,]
msa_sf_hwi@data <- merge(
  msa_sf_hwi@data,
  hwi_msa,
  by.x = "GEOID",
  by.y = "fips",
  sort = FALSE
)
  

tm_msa_hwi <- tm_shape(state_sf) +
  tm_borders() +
  tm_shape(msa_sf_hwi) +
  tm_fill(
    col = "hwi",
    title = "",
    style = "cont",
    palette = "Reds"
  )

tmap_save(
  tm_msa_hwi,
  filename = paste0("Figures/fig_9.pdf"),
  height = 80,
  width = 146.6,
  units = "mm"
)


####                             ####
#### Table        2              ####

temp_sum <- TMAX %>%
  full_join(TMIN, by = c("fips", "year", "month", "day", "date")) %>%
  full_join(PPT, by = c("fips", "year", "month", "day", "date")) %>%
  full_join(HI, by = c("fips", "year", "month", "day", "date")) %>%
  full_join(HWI_BINARY, by = c("fips", "year", "month", "day", "date")) %>%
  inner_join(unique(select(indices_vi, `CBSA Code`, pop, msa)), by = c("fips" = "CBSA Code")) %>%
  ungroup() %>%
  mutate(tavg = (tmax+tmin)/2) %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504) %>%
  filter(week <= 114) %>%
  group_by(week, msa) %>%
  summarise(pop = unique(pop),
            ppt = sum(ppt),
            tmax = mean(tmax),
            tavg = mean(tavg),
            hi = mean(hi),
            hwi = mean(hwi_binary)) %>%
  inner_join(select(indices_vi, msa, week, VI), by = c("msa", "week"))

weighted_sd <- function(x, w = 1) {
  if(w[1] == 1) w <- x*0+1
  m <- weighted.mean(x, w)
  n <- length(x)
  sqrt(n/(n-1)*weighted.mean((x-m)^2, w))
}

mean_sd <- function(.data, x, w = "pop") {
  x <- pull(.data, x)
  w <- pull(.data, w)
  c(
    "Mean" = mean(x),
    "Wtd. Mean" = weighted.mean(x, w = w),
    "SD" = sd(x),
    "Wtd. SD" = weighted_sd(x, w = w),
    "Max" = max(x),
    "min" = min(x)
  )
}

# Demean for SD Within
tp <- temp_sum %>%
  group_by(msa) %>%
  mutate(tmax = tmax-mean(tmax),
         tavg = tavg-mean(tavg),
         hi = hi-mean(hi),
         hwi = hwi-mean(hwi),
         ppt = ppt-mean(ppt)) %>%
  ungroup() 

# Copied numbers from here by hand
lapply(
  c("tmax", "tavg", "hi", "hwi", "ppt"), 
  function(x) temp_sum %>% mean_sd(x)
)
lapply(
  c("tmax", "tavg", "hi", "hwi", "ppt"),
  function(x) tp %>% mean_sd(x)
)
mean_sd(indices_vi, "VI")
indices_vi %>%
  group_by(msa) %>%
  mutate(VI = VI-mean(VI)) %>%
  ungroup() %>%
  summarise(sd(VI), weighted_sd(VI, w = indices_vi$pop))

weighted_sd(tp$VI, tp$pop)

length(unique(indices_vi$msa))
nrow(indices_vi)
length(unique(temp_sum$msa))

####                             ####
#### Figure      10              ####

tmax_bin_data <- get_bin_data()

# Main result
tmax_bin_coefs <- get_bin_coefs(tmax_bin_data)

plot_bin_bins(
  model = tmax_bin_coefs,
  save = TRUE,
  fname = "fig_10"
)

####                             ####
#### Figure      11              ####

# Average temperature
tavg_bin_data <- get_bin_data(TAVGBIN)

tavg_bin_coefs <- get_bin_coefs(tavg_bin_data)

plot_bin_bins(
  model = tavg_bin_coefs,
  save = TRUE,
  fname = "fig_11a",
  coords = c(-0.0006, 0.0025),
  width = 4,
  height = 3.5
)

# Heat index
hi_bin_data <- get_bin_data(HIBIN)

hi_bin_coefs <- get_bin_coefs(hi_bin_data)

plot_bin_bins(
  model = hi_bin_coefs,
  save = TRUE,
  fname = "fig_11b",
  coords = c(-0.0006, 0.0025),
  width = 4,
  height = 3.5
)

####                             ####
#### Figure      12              ####

# Heat wave index
hwi_bin_data <- get_bin_data(HWI_BINARYBIN)

hwi_bin_coefs <- get_bin_coefs(
  hwi_bin_data,
  refbin = 1
)

plot_bin_bins(
  model = hwi_bin_coefs,
  save = TRUE,
  fname = "fig_12"
)

####                             ####
#### Figure      13              ####

# Fixed effects robustness
tmax_bin_coefs_fx <- get_bin_coefs(
  tmax_bin_data,
  fix_ef = main_fe
)

plot_bin_bins(
  model = tmax_bin_coefs_fx, 
  save = TRUE, 
  fname = "fig_13"
)

####                             ####
#### Figure      14              ####

# Yearly comparison fixed effects robustness
tmax_bin_coefs_fxy <- get_bin_coefs(
  tmax_bin_data,
  fix_ef = interact_fe
)

plot_bin_bins(
  model = tmax_bin_coefs_fxy,
  save = TRUE, 
  fname = "fig_14"
)

####                             ####
#### Figure      15              ####

# Variance estimator robustness
tmax_bin_coefs_comp <- get_bin_coefs(
  tmax_bin_data,
  vcov = "twoway"
)

tmax_bin_coefs_comp$coefs <- rbind(
  rbind(
    tmax_bin_coefs_comp$coefs,
    tmax_bin_coefs$coefs
  ),
  get_bin_coefs(
    tmax_bin_data,
    vcov = "hetero"
  )$coefs
)

plot_bin_bins(
  tmax_bin_coefs_comp, 
  save = TRUE, 
  fname = "fig_15"
)

####                             ####
#### Figure      16              ####

# Load UA weather data
#   variable name required for 
#   results functions to work
# Reload MSA weather data afterwards!
TMAXBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_ua_popw_tmaxbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)
PPTBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_ua_popw_pptbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504,
         pptbin_1 = NULL)

tmax_bin_data_ua <- get_bin_data(.indices = indices_vi_ua)

tmax_bin_coefs_ua <- get_bin_coefs(tmax_bin_data_ua)

plot_bin_bins(
  model = tmax_bin_coefs_ua,
  save = TRUE,
  fname = "fig_16"
)

# Need to set these back to MSA level data
TMAXBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tmaxbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504)
PPTBIN <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_pptbin.fst") %>%
  mutate(week = (as.numeric(date)+3)%/%7-2504,
         pptbin_1 = NULL)

####                             ####
#### Figures     17 & A1         ####

tmax_bin_temp_groups <- get_bin_data(split = c("temperature" = 2))

tmax_bin_temp_groups_coefs <- get_bin_coefs(tmax_bin_temp_groups)

plot_bin_bins(
  model = tmax_bin_temp_groups_coefs, 
  save = TRUE, 
  fname = "fig_17"
)

tmax_bin_temp_groups_coefs_more <- get_bin_coefs(
  tmax_bin_temp_groups,
  fix_ef = main_fe
)

plot_bin_bins(
  model = tmax_bin_temp_groups_coefs_more,
  save = TRUE, 
  fname = "fig_A1"
)

####                             ####
#### Figures     18 & A2         ####

tmax_bin_inc_groups <- get_bin_data(split = c("income" = 2))

tmax_bin_inc_groups_coefs <- get_bin_coefs(tmax_bin_inc_groups)

plot_bin_bins(
  model = tmax_bin_inc_groups_coefs, 
  save = TRUE, 
  fname = "fig_18")

tmax_bin_inc_groups_coefs_more <- get_bin_coefs(
  tmax_bin_inc_groups,
  fix_ef = main_fe
)

plot_bin_bins(
  model = tmax_bin_inc_groups_coefs_more,
  save = TRUE, 
  fname = "fig_A2")

####                             ####
#### Figure      19              ####

# Determine ranks of MSA 
#   population sizes
ord_ind <- order(
  unique(indices_vi$pop), 
  decreasing = TRUE
)

# Data for population size
#   quartiles
tmax_bin_pop_q4 <- get_bin_data(
  .indices = filter(
    indices_vi,
    pop >= unique(indices_vi$pop)[ord_ind[79]]
  )
)
tmax_bin_pop_q3 <- get_bin_data(
  .indices = filter(
    indices_vi,
    pop >= unique(indices_vi$pop)[ord_ind[158]] & 
      pop < unique(indices_vi$pop)[ord_ind[79]]
  )
)
tmax_bin_pop_q2 <- get_bin_data(
  .indices = filter(
    indices_vi,
    pop >= unique(indices_vi$pop)[ord_ind[237]] &
      pop < unique(indices_vi$pop)[ord_ind[158]]
  )
)
tmax_bin_pop_q1 <- get_bin_data(
  .indices = filter(
    indices_vi,
    pop < unique(indices_vi$pop)[ord_ind[237]]
  )
)

# Get coefficients
tmax_bin_coefs_q4 <- get_bin_coefs(
  tmax_bin_pop_q4,
  weighted = FALSE
)
tmax_bin_coefs_q3 <- get_bin_coefs(
  tmax_bin_pop_q3,
  weighted = FALSE
)
tmax_bin_coefs_q2 <- get_bin_coefs(
  tmax_bin_pop_q2,
  weighted = FALSE
)
tmax_bin_coefs_q1 <- get_bin_coefs(
  tmax_bin_pop_q1,
  weighted = FALSE
)
# Unweighted
tmax_bin_coefs_uw <- get_bin_coefs(
  tmax_bin_data,
  weighted = FALSE
)

names <- c(
  "tmax_bin_coefs",
  "tmax_bin_coefs_q4",
  "tmax_bin_coefs_q3",
  "tmax_bin_coefs_q2",
  "tmax_bin_coefs_q1",
  "tmax_bin_coefs_uw"
)

titles <- list(
  "Full Weighted",
  "Rank 1-79 Unweighted",
  "Rank 80-158 Unweighted",
  "Rank 159-237 Unweighted",
  "Rank 238-315 Unweighted",
  "Full Unweighted"
)

# Copy tmax_bin_coefs as "mask"
#   and stack the coefficient
#   tables
tmax_bin_coefs_pop <- tmax_bin_coefs
tmax_bin_coefs_pop$coefs <- do.call(
  rbind, 
  lapply(
    seq_along(names),
    function(i) {
      l <- get(names[i])
      l$coefs$ec <- titles[[i]]
      l$coefs
    }
  )
)

tmax_bin_coefs_pop$fix_ef <- titles

plot_bin_bins(
  model = tmax_bin_coefs_pop,
  save = TRUE,
  fname = "fig_19"
)

####                             ####
#### Figure      20              ####

tmax_bin_data_o <- get_bin_data(.indices = indices_outdoor)

tmax_bin_coefs_o <- get_bin_coefs(tmax_bin_data_o)

plot_bin_bins(
  model = tmax_bin_coefs_o, 
  save = TRUE, 
  fname = "fig_20a", 
  coords = c(-0.002, 0.006), 
  width = 2.8, 
  height = 3.5
)

tmax_bin_data_i <- get_bin_data(.indices = indices_indoor)

tmax_bin_coefs_i <- get_bin_coefs(tmax_bin_data_i)

plot_bin_bins(
  model = tmax_bin_coefs_i, 
  save = TRUE, 
  fname = "fig_20b", 
  coords = c(-0.002, 0.006),
  width = 2.8,
  height = 3.5
)

tmax_bin_data_g <- get_bin_data(.indices = indices_grocery)

tmax_bin_coefs_g <- get_bin_coefs(tmax_bin_data_g)

plot_bin_bins(
  model = tmax_bin_coefs_g, 
  save = TRUE, 
  fname = "fig_20c",
  coords = c(-0.002, 0.006), 
  width = 2.8, 
  height = 3.5
)

####                             ####