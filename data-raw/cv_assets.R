# topo and steps
topo_arranged <- sf::st_read("data-raw/penbal_topo_500.gpkg")
steps <- sort(unique(topo_arranged$partition))

boundaries_arranged <- purrr::map(
  steps,
  .f = \(i_step) {
    # ini <- (i_step - 1) * stepcell + 1
    topo_bbox <- topo_arranged |>
      # sf::st_as_sf() |>
      # dplyr::slice(ini:(ini + stepcell)) |>
      dplyr::filter(partition == i_step) |>
      sf::st_bbox() |>
      sf::st_as_sfc()

    topo_nrow <- topo_arranged |>
      dplyr::filter(partition == i_step) |>
      nrow()

    # topo_sliced |>
    #   dplyr::mutate(bbox = topo_bbox, i_step = i_step)
    dplyr::tibble(i_step = i_step, topo_bbox = topo_bbox, topo_nrow = topo_nrow)
  }
) |>
  purrr::list_rbind() |>
  sf::st_as_sf(sf_column_name = "topo_bbox")

interpolators_geojson <- geojsonio::geojson_list(boundaries_arranged)
