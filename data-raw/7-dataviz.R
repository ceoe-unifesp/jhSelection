devtools::load_all()

usethis::ui_info("Data visualization...")

source_colors <- c(
  "SHR" = "#0072B2", # Azul
  "Fatal Encounters" = "#D55E00", # Laranja Queimado
  "Mapping Police Violence" = "#009E73" # Verde
)

dist_race <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, race) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::ungroup()

p_race <- dist_race |>
  ggplot2::ggplot(ggplot2::aes(
    x = prop,
    y = forcats::fct_rev(race),
    fill = source
  )) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = source_colors) +
  ggplot2::labs(
    title = "Racial Distribution of Victims by Data Source",
    subtitle = paste(
      "Comparison of victim proportions by race across SHR,",
      "Fatal Encounters (FE), and Mapping Police Violence (MPV).",
      sep = "\n"
    ),
    x = "Proportion of Total Victims",
    y = NULL,
    fill = "Data Source:",
    caption = "Note: Data periods vary by source."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_race.png",
  p_race,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 6
)

dist_race_unk <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, race) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  # remove Unknown/Others to focus on main racial groups
  dplyr::filter(race != "Unknown/Others") |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::ungroup()

p_race_unk <- dist_race_unk |>
  ggplot2::ggplot(ggplot2::aes(
    x = prop,
    y = forcats::fct_rev(race),
    fill = source
  )) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = source_colors) +
  ggplot2::labs(
    title = "Racial Distribution of Victims by Data Source",
    subtitle = "Comparison of victim proportions by race across SHR,\nFatal Encounters (FE), and Mapping Police Violence (MPV).",
    x = "Proportion of Total Victims",
    y = NULL,
    fill = "Data Source:",
    caption = "Note: 'Unknown/Others' categories have been excluded. Data periods vary by source."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_race_unk.png",
  p_race_unk,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 6
)

dist_sex <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, sex) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::filter(sex != "Unknown/Others") |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::ungroup()

plot_sex_dist <- dist_sex |>
  ggplot2::ggplot(ggplot2::aes(x = prop, y = sex, fill = source)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = source_colors) +
  ggplot2::labs(
    title = "SHR Proportionally Underreports Female Victims",
    subtitle = "Comparison of victim proportions by sex across the three data sources.",
    x = "Proportion of Total Victims",
    y = "Victim Sex",
    fill = "Data Source:",
    caption = "Note: 'Unknown/Others' categories have been excluded."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_sex.png",
  plot_sex_dist,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 6
)

dist_race_year <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, year, race) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::filter(race != "Unknown/Others") |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::filter(!is.nan(prop)) |>
  dplyr::ungroup()

# Generate the plot
plot_race_trends <- dist_race_year |>
  dplyr::filter(year >= 1980) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, color = source)) +
  ggplot2::geom_smooth(alpha = .2, method = "loess", formula = y ~ x) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::facet_wrap(~race, ncol = 1, scales = "free_y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::scale_color_manual(values = source_colors) +
  ggplot2::labs(
    title = "Racial Trends in SHR Diverge from Open-Source Data Over Time",
    subtitle = "Annual proportion of victims by race. Trends in SHR shift, while those in FE and MPV are more stable.",
    x = "Year",
    y = "Annual Proportion of Victims",
    color = "Data Source:",
    caption = "Data from 1980 onwards. Full time periods vary: SHR (since 1980), FE (since 2000), MPV (since 2013)."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_race_year.png",
  plot_race_trends,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 9
)

da_model_fips |>
  dplyr::filter(!is.na(shr)) |>
  with(sum(shr))

dist_sex_year <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, year, sex) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::filter(sex != "Unknown/Others") |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::filter(!is.nan(prop), sex != "Male") |>
  dplyr::ungroup()

plot_female_trends <- dist_sex_year |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, color = source)) +
  ggplot2::geom_point(size = 1.2, alpha = 0.8) +
  ggplot2::geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1,
    alpha = .2,
    formula = y ~ x
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_color_manual(values = source_colors) +
  ggplot2::labs(
    title = "Reporting of Female Victims Shows an Upward Trend in SHR",
    subtitle = "The proportion of female victims in SHR increases linearly over time.",
    x = "Year",
    y = "Proportion of Female Victims",
    color = "Data Source:",
    caption = "Dashed lines represent smoothed trend (LOESS)."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_female_year.png",
  plot_female_trends,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 5
)

dist_race_sex <- da_model_fips |>
  tidyr::pivot_longer(
    cols = dplyr::matches("shr|fe|mpv"),
    names_to = "source",
    values_to = "total"
  ) |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    ),
    source = forcats::fct_inorder(source)
  ) |>
  dplyr::group_by(source, race, sex) |>
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(sex != "Unknown/Others", race != "Unknown/Others") |>
  dplyr::group_by(source, race) |>
  dplyr::mutate(
    prop = formattable::percent(total / sum(total), 1)
  ) |>
  dplyr::filter(!is.nan(prop)) |>
  dplyr::ungroup()

p_female <- dist_race_sex |>
  dplyr::filter(sex == "Female") |>
  ggplot2::ggplot(ggplot2::aes(
    x = prop,
    y = forcats::fct_rev(race),
    fill = source
  )) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = source_colors) +
  ggplot2::labs(
    title = "SHR Reports a Lower Proportion of Females Across All Racial Groups",
    subtitle = "Proportion of female victims within each racial group, by data source.",
    x = "Proportion of Female Victims",
    y = NULL,
    fill = "Data Source:",
    caption = "Note: 'Unknown/Others' categories have been excluded."
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_female_race.png",
  p_female,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 6
)

# MAPS ------------------------

da_model_fips

counties_sf <- tigris::counties(cb = TRUE, year = 2023) |>
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP"))

states_sf <- tigris::states(cb = TRUE, year = 2023) |>
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP"))

da_prop_black_compare <- da_model_fips |>
  dplyr::group_by(fips, race) |>
  dplyr::summarise(
    shr = sum(shr, na.rm = TRUE),
    fe = sum(fe, na.rm = TRUE),
    mpv = sum(mpv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(fips) |>
  dplyr::summarise(
    prop_shr = sum(shr[race == "Hispanic"], na.rm = TRUE) /
      sum(shr, na.rm = TRUE),
    n_shr = sum(shr, na.rm = TRUE),
    prop_fe = sum(fe[race == "Hispanic"], na.rm = TRUE) / sum(fe, na.rm = TRUE),
    n_fe = sum(fe, na.rm = TRUE),
    prop_mpv = sum(mpv[race == "Hispanic"], na.rm = TRUE) /
      sum(mpv, na.rm = TRUE),
    n_mpv = sum(mpv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    diff_fe_shr = prop_shr - prop_fe,
    diff_mpv_shr = prop_shr - prop_mpv
  ) |>
  tidyr::pivot_longer(
    c(diff_fe_shr, diff_mpv_shr),
    names_to = "comparison",
    values_to = "prop_diff"
  ) |>
  dplyr::filter(!is.nan(prop_diff))

counties_sf |>
  dplyr::inner_join(da_prop_black_compare, by = c("GEOID" = "fips")) |>
  dplyr::mutate(
    comparison = dplyr::case_match(
      comparison,
      "diff_fe_shr" ~ "SHR vs. Fatal Encounters",
      "diff_mpv_shr" ~ "SHR vs. Mapping Police Violence"
    )
  ) |>
  dplyr::mutate(
    diff_cat = cut(
      prop_diff,
      c(-1.1, -0.5, -0.05, 0.05, 0.5, 1.1),
      labels = c(
        "Large Underreporting (< -50%)",
        "Underreporting (-50% to -5%)",
        "Near parity (-5% to 5%)",
        "Overreporting (5%  to 50%)",
        "Large Overreporting (> 50%)"
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(fill = diff_cat)) +
  # only mainland US counties
  ggplot2::geom_sf(
    data = counties_sf,
    colour = 'transparent',
    fill = 'white'
  ) +
  ggplot2::geom_sf(
    data = states_sf,
    colour = 'black',
    fill = 'transparent'
  ) +
  ggplot2::geom_sf(color = NA) +
  ggplot2::scale_fill_brewer(type = "div", palette = 5) +
  ggplot2::facet_wrap(~comparison, ncol = 1) +
  ggplot2::labs(
    title = "Geographic Disparities in Reporting of Hispanic Victims",
    fill = "Difference in Proportion",
    caption = "Data sources: SHR, Fatal Encounters, Mapping Police Violence."
  ) +
  theme_pub() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggplot2::ggsave(
  "data-raw/png/map_hispanic_reporting_disparities.png",
  dpi = 300,
  bg = "white",
  width = 8,
  height = 8
)


da_prop_female_compare <- da_model_fips |>
  dplyr::group_by(fips, sex) |>
  dplyr::summarise(
    shr = sum(shr, na.rm = TRUE),
    fe = sum(fe, na.rm = TRUE),
    mpv = sum(mpv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(fips) |>
  dplyr::summarise(
    prop_shr = sum(shr[sex == "Female"], na.rm = TRUE) /
      sum(shr, na.rm = TRUE),
    n_shr = sum(shr, na.rm = TRUE),
    prop_fe = sum(fe[sex == "Female"], na.rm = TRUE) / sum(fe, na.rm = TRUE),
    n_fe = sum(fe, na.rm = TRUE),
    prop_mpv = sum(mpv[sex == "Female"], na.rm = TRUE) /
      sum(mpv, na.rm = TRUE),
    n_mpv = sum(mpv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    diff_fe_shr = prop_shr - prop_fe,
    diff_mpv_shr = prop_shr - prop_mpv
  ) |>
  tidyr::pivot_longer(
    c(diff_fe_shr, diff_mpv_shr),
    names_to = "comparison",
    values_to = "prop_diff"
  ) |>
  dplyr::filter(!is.nan(prop_diff))

counties_sf |>
  dplyr::inner_join(da_prop_female_compare, by = c("GEOID" = "fips")) |>
  dplyr::mutate(
    comparison = dplyr::case_match(
      comparison,
      "diff_fe_shr" ~ "SHR vs. Fatal Encounters",
      "diff_mpv_shr" ~ "SHR vs. Mapping Police Violence"
    )
  ) |>
  dplyr::mutate(
    diff_cat = cut(
      prop_diff,
      c(-1.1, -0.5, -0.05, 0.05, 0.5, 1.1),
      labels = c(
        "Large Underreporting (< -50%)",
        "Underreporting (-50% to -5%)",
        "Near parity (-5% to 5%)",
        "Overreporting (5%  to 50%)",
        "Large Overreporting (> 50%)"
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(fill = diff_cat)) +
  # only mainland US counties
  ggplot2::geom_sf(
    data = counties_sf,
    colour = 'transparent',
    fill = 'white'
  ) +
  ggplot2::geom_sf(
    data = states_sf,
    colour = 'black',
    fill = 'transparent'
  ) +
  ggplot2::geom_sf(color = 'transparent') +
  ggplot2::scale_fill_brewer(type = "div", palette = 4) +
  ggplot2::facet_wrap(~comparison, ncol = 1) +
  ggplot2::labs(
    title = "Geographic Disparities in Reporting of Female Victims",
    fill = "Difference in Proportion",
    caption = "Data sources: SHR, Fatal Encounters, Mapping Police Violence."
  ) +
  theme_pub() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggplot2::ggsave(
  "data-raw/png/map_female_reporting_disparities.png",
  dpi = 300,
  bg = "white",
  width = 8,
  height = 8
)
