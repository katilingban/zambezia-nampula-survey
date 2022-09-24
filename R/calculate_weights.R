################################################################################
#
#'
#' Calculate sampling weights
#'
#
################################################################################

calculate_weights <- function(.data, survey_sampling_list, 
                              type = c("baseline", "endline")) {
  survey_clusters <- .data |>
    dplyr::mutate(
      hh_id = sbjnum,
      ch_id = paste0(sbjnum, cid),
      province = haven::as_factor(prov),
      district = haven::as_factor(distrito),
      ea_code = paste0(
        prov, 
        stringr::str_pad(distrito, width = 2, side = "left", pad = 0), 
        stringr::str_pad(post, width = 3, side = "left", pad = 0), 
        stringr::str_pad(enum1, width = 3, side = "left", pad = 0)
      ),
      ea_id = enum1
    ) |>
    subset(
      select = c(
        hh_id, ch_id, province, district, 
        ea_code, ea_id, strata, longitude, latitude
      )
    ) |>
    dplyr::group_by(ea_id) |>
    summarise(
      province = unique(province),
      district = unique(district),
      strata = unique(strata),
      ea_code = unique(ea_code),
      ea_id = unique(ea_id),
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, mean = TRUE),
      cluster_size = dplyr::n()
    ) |>
    (\(x)
     dplyr::left_join(
       x, 
       x |>
         dplyr::group_by(province, strata) |>
         dplyr::summarise(
           n_cluster = dplyr::n()
         ),
       by = c("province", "strata")
     )
    )()

  sample_clusters <- survey_sampling_list |>
    subset(
      select = c(
        UNIQUE_ID, 
        Paridade,
        `População.Total.-.preliminar`,
        `Homem.-.preliminar`,
        `Mulher.-.preliminar`,
        `Sample.Size.of.EA`, 
        `Sampling.Rate`,
        `Sample.Weight`
      )
    ) |>
    dplyr::rename(
      ea_id = UNIQUE_ID,
      study_group = Paridade,
      cluster_pop = `População.Total.-.preliminar`,
      cluster_pop_men = `Homem.-.preliminar`,
      cluster_pop_women = `Mulher.-.preliminar`,
      cluster_size_exp = `Sample.Size.of.EA`, 
      sample_prob_exp = `Sampling.Rate`,
      sample_weight_exp = `Sample.Weight`
    )
  
  dplyr::left_join(
    survey_clusters, sample_clusters, by = "ea_id"
  ) |>
    dplyr::group_by(province, strata) |>
    dplyr::mutate(
      study_group = study_group,
      total_pop = sum(cluster_pop),
      cluster_sample_prob_obs = (cluster_pop * n_cluster) / total_pop,
      #cluster_sample_prob_obs = cluster_pop / total_pop,
      ind_sample_prob_obs = cluster_size / cluster_pop,
      sample_prob_obs = cluster_sample_prob_obs * ind_sample_prob_obs,
      sample_weight_obs = 1 / sample_prob_obs
    )
}

