
##### Data pull and process functions #####

#' @title Parse the `party_nm` field into individual site visitor initials
#' @description this function separates the `party_nm` field into individual
#' initials and duplicates the field visit record for each initial contained
#' within the `party_nm` string.
#' @details The `party_nm` field appears to be a free text field, so while there
#' is some structure to how people enter their initials, it varies
#' widely. This function should not represent a comprehensive approach
#' to parsing this field into individual hydrotech initials, but does
#' do a good job for the most commond formats.
#' @param field_visit_data a data.frame returned by `dataRetrieval::readNWISmeas()`
#' which has at least a `party_nm` column.
#'

parse_visitor_initials <- function(field_visit_data) {

  possible_separators <- c(
    "/",
    "/[[:space:]]", # TODO: THIS IS NOT WORKING, spaces remain
    ",",
    ",[[:space:]]",  # TODO: THIS IS NOT WORKING, spaces remain
    "[[:space:]]//[[:space:]]",
    "\\+",
    "[[:space:]]\\+[[:space:]]"
  )

  field_visit_data %>%
    mutate(initials_list = str_split(party_nm, paste(possible_separators, collapse="|"))) %>%
    unnest(cols = c(initials_list)) %>%
    # Remove empty strings
    filter(!stringi::stri_isempty(initials_list)) %>%
    # The same hydrotech might write capital letters one day, but lowercase
    # the next. Let's consider them the same person for this workflow.
    mutate(hydrotech = toupper(initials_list)) %>%
    select(-initials_list)

}

#' @title For each site, identify the individual who has the most visits.
#' @description Assuming each site + initials group can represent one unique
#' site visitor, this function tallies the number of visits and returns the
#' initials that appear the most for each site. We are calling the hydrotech
#' behind the initials the site's "best friend".
#'
#' @param field_visit_data_hydrotechs a data.frame returned by `parse_visitor_initials()`
#'
#' @return data.frame with `site_no`, `best_friend`, and `n_bff_visits` columns
#'

identify_site_bff <- function(field_visit_data_hydrotechs) {

  field_visit_data_hydrotechs %>%
    # Tally how many visits each hydrotech has made to per site
    group_by(site_no, hydrotech) %>%
    tally() %>%
    ungroup() %>%
    # Now keep only the hydrotech who made the most visits for each site
    group_by(site_no) %>%
    slice(which.max(n)) %>%
    ungroup() %>%
    select(site_no, best_friend = hydrotech, n_bff_visits = n)

}

#' @title Return the sites with the most visits
#' @description This function tallies the number of visits per site and
#' returns `top_n` site numbers representing the most visited.
#'
#' @param field_visit_data a data.frame returned by `dataRetrieval::readNWISmeas()`
#' @param top_n a number representing how many sites should be returned,
#' defaults to 5
#'
#' @return data.frame with `site_no` and `n_total_visits` columns
#'

identify_most_visited_sites <- function(field_visit_data, top_n = 5) {

  field_visit_data %>%
    group_by(site_no) %>%
    tally() %>%
    arrange(desc(n)) %>%
    head(top_n) %>%
    select(site_no, n_total_visits = n)
}

#' @title For the top sites, identify the individual visited them the most.
#' @description This function combines the most visited sites and adds the
#' initials that appear the most frequently for each site. In addition, this
#' adds the full station name and adds an ordered factor column to keep the
#' highest sites in order from most total visits to least.
#'
#' @param field_visit_bff a data.frame returned by `parse_visitor_initials()`
#' @param field_visit_highest a data.frame returned by `identify_most_visited_sites()`
#' @param sites_sf an `sf` object created using NWIS site metadata
#'
#' @return data.frame with `site_no`, `best_friend`, `n_bff_visits`, `n_total_visits`,
#' `station_nm`, and `station_nm_f` (ordered factor) columns.
#'

link_most_visited_sites_bff <- function(field_visit_bff, field_visit_highest, sites_sf) {
  site_names <- sites_sf %>%
    st_drop_geometry() %>%
    select(site_no, station_nm)

  top5_bffs <- field_visit_bff %>%
    right_join(field_visit_highest, by="site_no") %>%
    arrange(n_total_visits) %>%
    left_join(site_names, by = "site_no") %>%
    mutate(station_nm_f = factor(station_nm, levels = station_nm, ordered = TRUE))

  return(top5_bffs)
}

#' @title Download population data from the U.S. Census Bureau
#' @description This function uses the `tidycensus` package to download
#' resident population data using the 'place' geographies from the Census
#' Bureau. Note that the API call used here was not returning data for
#' Puerto Rico, so we excluded it in the subsequent functions that use the
#' population data.
#'
#' @details To use this, you will need to have a local API key setup. Follow the
#' instructions at https://walker-data.com/tidycensus/articles/basic-usage.html
#' for requesting a key. You will receive the email fast, but note that you may
#' need to wait a few minutes before successfully clicking on the activate key
#' URL. Once the key is activated, run `tidycensus::census_api_key("YOUR KEY", install = TRUE)`
#' and then you should be able to use this function.
#'
#' @param state_cd vector of character abbreviations for states to pull place data
#'
#' @return an `sf` object with polygons for the places, plus `GEOID` (US Census
#' Bureau defined place code), `NAME` (plain text place name), `variable` (will
#' only say "POP" for population), and `value` (the resident population count) columns.
#'

pull_population_data <- function(state_cd) {
  get_estimates(
    'place',
    year = 2019, # Can't get anything newer than 2019 to work
    state = state_cd,
    variable = "POP",
    geometry = TRUE)
}

#' @title Clean the population data to use in later steps
#' @description This function applies a filter to keep only places of a certain
#' resident size and converts the polygons into centroids.
#'
#' @param all_population_data the `sf` object returned from `pull_population_data()`
#' @param min_value single numeric value indicating the minimum population a
#' place should have to remain in the dataset. Note that the default is 50k
#' residents, which means that some states (such as Vermont, where the most
#' populated city is Burlington with just under 43k) will not have any places
#' included in the final dataset.
#'
#' @return an `sf` object with points for the places, plus `GEOID` (US Census
#' Bureau defined place code), `NAME` (plain text place name), and `population`
#' (the resident population count) columns.
#'

prepare_population_data <- function(all_population_data, min_value = 50*10^3) {
  all_population_data %>%
    select(-variable, population = value) %>%
    filter(population > min_value) %>%
    st_centroid() %>%
    st_transform(crs = 4326)
}

#' @title For each site, identify the closest populated place
#' @description This function identifies the nearest place for each gage and
#' how far that place is in kilometers and miles. It also filters out sites
#' in Puerto Rico due to the limitation noted in `pull_population_data()` with
#' pulling place population data for Puerto Rico.
#'
#' @param sites_sf an `sf` object created using NWIS site metadata
#' @param places_sf the `sf` object returned from `prepare_population_data()`
#'
#' @return an `sf` object with points for gages, all site metadata columns
#' returned by `dataRetrieval::readNWISsite()`, plus `nearest_place` (US Census
#' Bureau plain text name for a location), `nearest_place_distance_km` (distance
#' in kilometers from the gage to the `nearest_place`), and `nearest_place_distance_mi`
#' (distance in miles from the gage to the `nearest_place`).
#'

identify_nearest_place <- function(sites_sf, places_sf) {
  distances_mat <- st_distance(sites_sf, places_sf)
  min_distance_per_site <- apply(distances_mat, 1, min)
  min_distance_per_site_i <- apply(distances_mat, 1, which.min)
  nearest_info <- sites_sf %>%
    mutate(nearest_place = places_sf$NAME[min_distance_per_site_i],
           nearest_place_distance_km = min_distance_per_site / 1000,
           nearest_place_distance_mi = nearest_place_distance_km / 1.609) %>%
    # REMOVE Puerto Rico for now since we can't get population data from tidycensus right now.
    filter(state_cd != '72')
  return(nearest_info)
}

#' @title For each site, tally and then average annual visits
#' @description This function calculates an average annual number
#' of visits for each site. Note that visits where `measurement_dt`
#' is `NA` are dropped entirely here. For 2012-06-30 to 2022-06-30,
#' 1% of the field visits (~ 8,500 visits) had `NA` in the
#' `measurement_dt` field and were dropped.
#'
#' @param field_visit_data the data.frame object returned by `dataRetrieval::readNWISmeas()`
#'
#' @return a data.frame with two columns, `site_no` and `avg_annual_visits`
#'

calculate_avg_annual_visits <- function(field_visit_data) {
  field_visit_data %>%
    mutate(year = lubridate::year(measurement_dt)) %>%
    group_by(site_no, year) %>%
    tally() %>%
    group_by(site_no) %>%
    summarize(avg_annual_visits = mean(n, na.rm=TRUE))
}


#' @title Categorize each site into a distance bin based on proximity to populated places
#' @description This function categorizes each site into a bin based on the distance to
#' its `nearest_place` using the `nearest_place_distance_mi` column. It retains the
#' annual visit rates so that they can be used in plotting later.
#'
#' @param site_visits_yearly the data.frame object returned by `calculate_avg_annual_visits()`
#' @param site_nearest_city the `sf` object returned by `identify_nearest_place()`
#'
#' @return an `sf` object with the same columns as `site_visits_yearly` plus `nearest_place_distance_mi`,
#' and `travel_cat` representing which distance category the site falls into.
#'

categorize_site_distance <- function(site_visits_yearly, site_nearest_city) {
  site_visits_yearly %>%
    inner_join(select(site_nearest_city, site_no, nearest_place_distance_mi)) %>%
    mutate(travel_cat = cut(nearest_place_distance_mi,
                            breaks = c(0,2,10,50, 100, Inf),
                            labels = c('walk (< 2 mi)', 'bike (< 10 mi)',
                                       'around-town drive (< 50 mi)',
                                       'day drive (< 100 mi)',
                                       'multi-day trip (100+ mi)')))
}

#' @title Categorize each field visit record into one of the 24 hours in a day
#' @description This function takes the `measurement_tm` column from the field
#' visit measurement records and categorizes into a single hour of the day between
#' 1 and 24 by rounding up (e.g "09:12" is the 10th hour of the day, while "09:00"
#' is the last value of the 9th hour of the day).
#'
#' @param field_visit_data the data.frame object returned by `dataRetrieval::readNWISmeas()`
#'
#' @return a data.frame with the same columns as `field_visit_data` plus `time_of_day`,
#' representing an integer hour of the day between 1 and 24
#'

categorize_visit_into_time_of_day <- function(field_visit_data) {
  field_visit_data %>%
    separate(measurement_tm, c('hours', 'minutes'), sep = ':') %>%
    mutate(time_of_day = ceiling(as.numeric(hours) + as.numeric(minutes)/60)) %>%
    dplyr::select(-hours, -minutes) %>%
    filter(!is.na(time_of_day) & time_of_day != 0)
}

##### Plotting functions #####

#' @title Plot the top sites and their visitor initials on a bar chart
#' @description This function makes a basic horizontal bar chart with
#' the top sites and how many total visits they've had for the set time
#' period. Includes a secondary bar showing visits by the topmost individual.
#'
#' @param highest_sites_bff the data.frame returned by `link_most_visited_sites_bff()`
#'
#' @return a ggplot object of a horizontal bar plot
#'

plot_best_friend_bars <- function(highest_sites_bff) {
  ggplot(highest_sites_bff, aes(x = station_nm_f)) +
    geom_bar(aes(y = n_bff_visits), stat="identity", position = "identity", fill = '#F6CCB0') +
    geom_bar(aes(y = n_total_visits), stat="identity", position = "identity", fill = NA,
             color = '#3774A3', size = 2) +
    geom_label(aes(y = n_total_visits, label = sprintf("%s total visits", n_total_visits)), hjust = "left", nudge_y = 10) +
    geom_label(aes(y = n_bff_visits, label = sprintf("%s visited %s times", best_friend, n_bff_visits)), hjust = "left", nudge_y = 5) +
    ylim(c(0, max(highest_sites_bff$n_total_visits)*1.25)) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_flip()
}

#' @title Plot the distribution of visit frequency and travel times for each site
#' @description This function makes a violin chart with every site plotted showing
#' their frequency of visits through an annual visit average and their distance from
#' populated location.
#'
#' @param travel_distance_data the data.frame returned by `categorize_site_distance()`
#'
#' @return a ggplot object of a violin plot for each site with their average
#' annual visits on the y-axis and travel categories on the x-axis
#'

plot_travel_distance_violins <- function(travel_distance_data) {
  ggplot(travel_distance_data,
         aes(x = travel_cat, y = avg_annual_visits,
             color = travel_cat, fill = travel_cat)) +
    scale_y_continuous(breaks = seq(0, 150, by=25)) +
    geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 2,color=NA) +
    ggbeeswarm::geom_quasirandom(shape = 21, size = 2, dodge.width = 0.75, color="white", alpha=0.5) +
    scico::scale_color_scico_d(palette = 'lapaz', begin = 0.3, direction = -1) +
    scico::scale_fill_scico_d(palette = 'lapaz', end = 0.8, direction = -1) +
    theme_minimal() +
    theme(
      legend.position="none",
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      text = element_text(color = "darkgrey"),
      axis.text = element_text(color = "darkgrey"),
      plot.background =  element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "transparent"),
      panel.grid.major.y = element_line(color = "lightgrey")
    )
}

#' @title Plot the field visit time of days
#' @description This function makes a radial plot of every field visit and their
#' time of day for the visit.
#'
#' @param field_visit_data_with_hour the data.frame returned by `categorize_visit_into_time_of_day()`
#'
#' @return a ggplot object of a radial chart with a dot for every visit and the time of day
#' plotted along the "x-axis". The y-axis does not have specific meaning, but the points
#' are jittered to be able to show the density.
#'

plot_time_of_day_radial <- function(field_visit_data_with_hour) {

  # Simple wrapper around `cut()` to choose a color for each hour (1:24)
  choose_sunset_color <- function(tod) {
    cut(tod,
        breaks = c(0,2,4,6,8,10,14,16,18,20,22,24),
        labels = c("#08183A", "#152852", "#4B3D60",
                   "#FD5E53", "#FC9C54", "#FFE373",
                   "#FC9C54", "#FD5E53", "#4B3D60",
                   "#152852", "#08183A"),
        include.lowest = TRUE) %>%
      as.character()
  }

  field_visit_data_with_hour %>%
    mutate(dot_color = choose_sunset_color(time_of_day)) %>%
    select(time_of_day, dot_color) %>%
    ggplot() +
    geom_jitter(aes(x = time_of_day, y = 0.5, color = dot_color), width=0.49, alpha=0.7, shape = 1) +
    scale_color_identity() +
    coord_polar(start = pi) + theme_void() +
    theme(
      # Make the background transparent
      panel.background = element_rect(fill='transparent', color=NA),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA)
    )
}
