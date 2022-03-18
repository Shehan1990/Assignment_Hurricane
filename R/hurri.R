#' read in data
#'
#' This function is to import the data set from where it is saved.
#'
#' @param file Filename or raw data
#' @param widths Vector of columns widths
#'
#' @import readr
#'
#' @export
read_ext_tracks<-function(file="ebtrk_atlc_1988_2015.txt",...,widths=NULL,
                          colnames=NULL,degW=TRUE){
  ext_tracks_file <- paste0("H:/Building Data Visualization Tools/Assignment/hurricane/data/ebtrk_atlc_1988_2015.txt")

  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")

  ext_tracks <- readr::read_fwf(ext_tracks_file,
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")

  return(ext_tracks)
}


#' tidying data
#'
#' The data set is tidying as per instructed in the question.
#'
#' @param ext_tracks Output from the read_ext_tracks function.
#'
#' @import dplyr
#'
#' @export
ext_tracks_tidy <- function(ext_tracks){

  # add a column for storm_id that combines storm name and year
  ext_tracks <- ext_tracks %>%
    dplyr::mutate(storm_id = paste(storm_name, year, sep = "_")) %>%
    # Format longitude to numeric and has negative values for locations in western hemisphere
    dplyr::mutate(longitude = -longitude) %>%
    # format and combine columns describing the date and time to create a single variable with the date-time of each observation;
    dplyr::mutate(date = paste(year, month, day, sep = "-")) %>%
    # convert the data to a “long” format
    dplyr::mutate(date = ymd_hms(paste0(date, " ", hour, "00:00")))


  # Tidy data into long format - separate rows for each of the three wind speeds for wind radii (34 knots, 50 knots, and 64 knots)

  # Gather columns for 34 knots winds and rename columns
  data_34 <-
    ext_tracks %>%
    dplyr::select(storm_id, date, latitude, longitude, starts_with("radius_34")) %>%
    dplyr::mutate(wind_speed = "34") %>%
    dplyr::rename(ne=radius_34_ne, se=radius_34_se, nw=radius_34_nw, sw=radius_34_sw)

  # Gather columns for 50 knots winds and rename columns
  data_50 <-
    ext_tracks %>%
    dplyr::select(storm_id, date, latitude, longitude, starts_with("radius_50")) %>%
    dplyr::mutate(wind_speed = "50") %>%
    dplyr::rename(ne=radius_50_ne, se=radius_50_se, nw=radius_50_nw, sw=radius_50_sw)

  # Gather columns for 64 knots winds and rename columns
  data_64 <-
    ext_tracks %>%
    dplyr::select(storm_id, date, latitude, longitude, starts_with("radius_64")) %>%
    dplyr::mutate(wind_speed = "64") %>%
    dplyr::rename(ne=radius_64_ne, se=radius_64_se, nw=radius_64_nw, sw=radius_64_sw)

  # Combine data frames for each wind speed for long format
  ext_tracks <- rbind(data_34, data_50, data_64)

}


#' Constructing geom_hurricane - stat_hurricane
#'
#' prepare the stat for the tool being built
#'
#' @param ext_tracks Output from the ext_tracks_tidy function.
#'
#' @import ggplot2 ggproto Stat layer
#' @import dplyr rename select bind_rows
#' @import tidyr gather
#' @import purrr pmap
#'
#' @export
hurri_stat <- function(ext_tracks){

  StatHurricane <-
    ggplot2::ggproto("StatHurricane", ggplot2::Stat,
                     compute_group = function(data, scales, scale_radii) {
                       lon <- data$x[1]
                       lat <- data$y[1]
                       new_arc <- function(direction, radius){
                         my_arc <- geosphere::destPoint(c(lon, lat),
                                                        dplyr::case_when(direction == "r_ne" ~ 0:90,
                                                                         direction == "r_se" ~ 90:180,
                                                                         direction == "r_sw" ~ 180:270,
                                                                         direction == "r_nw" ~ 270:360),
                                                        radius*1852*scale_radii) %>%
                           rbind(data.frame(lon=lon, lat=lat)) %>%
                           dplyr::rename(x=lon, y=lat)
                       }

                       df <-
                         data %>%
                         dplyr::select(r_ne, r_nw, r_se, r_sw) %>%
                         tidyr::gather(direction, radius)

                       grid <-
                         purrr::pmap(list(df$direction, df$radius), new_arc) %>%
                         dplyr::bind_rows()

                       return(grid)
                     },
                     required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw")
    )

  stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, scale_radii = 1, ...) {
    ggplot2::layer(
      stat = StatHurricane, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
    )
  }

}


#' Constructing geom_hurricane - geom_hurricane
#'
#' prepare the geom for the visualization tool being built
#'
#' @param StatHurricane Output from the hurri_stat function.
#'
#' @import ggplot2 ggproto GeomPolygon aes layer
#'
#' @export
hurri_geom <- function(StatHurricane){

  geomHurricane <-
    ggplot2::ggproto("GeomPolygon", ggplot2::GeomPolygon,
                     default_aes = ggplot2::aes(colour = "green",
                                                fill = NA, linetype = 1,
                                                size = 1, alpha = 0.6)
    )

  geom_hurricane <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE,
                             scale_radii = 1.0, ...) {

    ggplot2::layer(stat = StatHurricane, geom = geomHurricane,
                   data = data, mapping = mapping, position = position,
                   show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
    )
  }
}

