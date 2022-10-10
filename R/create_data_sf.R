################################################################################
#
#'
#' Create sf version of data
#'
#
################################################################################

create_data_sf <- function(.data) {
  .data_sf <- .data |>
    dplyr::mutate(
      latitude = geolocation |> (\(x) do.call(rbind, x)[, 1])(),
      longitude = geolocation |> (\(x) do.call(rbind, x)[, 2])()
    )
  
  sf::st_as_sf(
    x = .data_sf,
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  )
}