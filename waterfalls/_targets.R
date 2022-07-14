# Script to run the waterfalls viz pipeline
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'sbtools', 'geojsonR', 'sp','sf', 'waterfall', 
                            'ggthemes', 'readr', 
                            'geojsonio', 'USAboundaries', 'purrr', 'jsonlite',
                            'cowplot', 'showtext', 'ggimage'))

source("src/functions.R")

## Global options

## Fetch data
fetch_targets <- list(
  # Read in data from Science Base
  tar_target(
    usgs_sb_geojson,
    read_data_sb(sb_id = "5e8d2b5982cee42d13466001",
                 names = "conterminousUS_falls_nhd.geojson",
                 destinations = "out/conterminousUS_falls_nhd.geojson"),
    format = "file"
  ),
  
  # Read in the geojson from Science Base
  tar_target(
    usgs_sb_spdf,
    geojson_read(usgs_sb_geojson, 
                 what = "sp")
  ),
  
  # Read in state shapefile, 
  tar_target(
    usa_states_sp,
    us_states(resolution = "high") %>% 
      filter(!state_abbr %in% c("PR", "AK", "HI"))
  ),
  
  # Read in web-scraped data (optional, if not using Cee's code)
  ### Note: This includes ONLY the "confirmed" waterfalls 
  tar_target(
    wwdb_scraped_df,
    read_data_wwdb(file = "in/us_waterfalls.csv")
  )
)

## Process data
process_targets <- list(
  # Clean up SB dataset to remove unwanted states and couple trouble data points
  tar_target(
    usgs_sb_df,
    clean_sb_data(usgs_sb_spdf)
  ),
  
  # Convert SB df to sf class
  tar_target(
    usgs_sb_sf,
    usgs_sb_df %>% 
      st_as_sf(coords = c("verifiedx", "verifiedy"), crs = usgs_sb_spdf@proj4string)
  ),
  
  # Spatially match each state to the SB data for each waterfall record
  tar_target(
    usgs_sb_join_sf,
    spatially_join_waterfalls(usgs_sb_sf, usa_states_sp)
  ),
  
  # Extract metadata for SB data and clean up for graphing
  tar_target(
    usgs_metadata_sf,
    metadata_json_export(usgs_sb_join_sf, usgs_sb_df)
  ),
  
  # Clean up web scraped data
  tar_target(
    wwdb_metadata_df,
    metadata_scraped_export(wwdb_scraped_df)
  ),
  
  # Merge two data sources SB and WWDB
  tar_target(
    waterfalls_df,
    bind_rows(wwdb_metadata_df, usgs_metadata_sf)
  ),
  
  # Calculate statistics by state
  tar_target(
    state_stats_df,
    waterfalls_df %>%
      group_by(state) %>%
      summarise(mean = mean(drop_ft, na.rm = T),
                median = median(drop_ft, na.rm = T),
                max = max(drop_ft, na.rm = T),
                sum = sum(drop_ft, na.rm = T),
                count = length(drop_ft))
  ),
  
  # Remove missing values for graphing and create cumulative drop height for graphing
  tar_target(
    waterfalls_graphing_df,
    prep_for_graphing(waterfalls_df)
  )
)

## Visualize data
viz_targets <- list(
  tar_target(
    waterfall_plot_png,
    plot_waterfalls(waterfalls_graphing_df,
                    out_file = "out/main_waterfalls_plot.png"),
    format = "file"
  )
)


list(fetch_targets, process_targets, viz_targets)