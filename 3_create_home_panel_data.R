### Create weekly datasets                     ###

sg_date_home <- read_fst("Data/Processed_Data/date_table_home.fst", as.data.table = TRUE)
all_dates <- sort(unique(sg_date_home[, date]))
all_dates <- all_dates[all_dates < as.IDate("2020-03-09")]

# Get unique dates from Patterns data,
#  because dates are "fuzzy" in home_panel_summary data
sg_date <- unique(
  read.fst(
    "Data/Processed_Data/date_table.fst",
    as.data.table = TRUE,
    columns = "date"
  )
)[, date]

for(select_date in all_dates) {
  
  sg_home <- data.table()
  
  select_date <- as.IDate(select_date)
  parts <- sg_date_home[date == select_date, part]
  
  sg_home <- lapply(
    parts,
    function(p) {
      cat("\r", "Reading part", match(p, parts), "out of", length(parts), "parts...")
      
      sg_home_new <- fread(
        file = paste0("Data/SafeGraph_Data/Patterns/safegraph_home_panel_summary-part", p, ".csv.gz"),
        select = c(
          "date_range_start",
          "census_block_group",
          "number_devices_residing"
        ),
        showProgress = FALSE
      )[, date_range_start := sg_date[
        sapply(
          as.IDate(date_range_start), 
          function(x) which.min(abs(sg_date - x))
        )
        ]
        ][date_range_start == select_date
          ]
    }
  )

  write_fst(
    rbindlist(sg_home),
    paste0("Data/Processed_Data/Weekly_Home/home_", select_date, ".fst")
  )
  
  cat("\r", "Saved table for week starting", as.character(select_date), "\n")
}
