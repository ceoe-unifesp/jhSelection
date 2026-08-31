devtools::load_all()

usethis::ui_info("Table 1 stats...")

da_model_fips |>
  dplyr::filter(is.na(fips))

# fips and state counts
length(unique(na.omit(da_model_fips$fips)))
length(unique(na.omit(da_model_fips$state)))

# white male proportion
sum(da_model_fips$race == "White")
mean(da_model_fips$race == "White")
sum(da_model_fips$sex == "Male")
mean(da_model_fips$sex == "Male")

# tange
range(da_model_fips$year[!is.na(da_model_fips$fe)])
range(da_model_fips$year[!is.na(da_model_fips$mpv)])
range(da_model_fips$year[!is.na(da_model_fips$shr)])

# victim counts
length(da_model_fips$fe[!is.na(da_model_fips$fe)])
length(da_model_fips$mpv[!is.na(da_model_fips$mpv)])
length(da_model_fips$shr[
  !is.na(da_model_fips$shr) & da_model_fips$year <= 2023
])

# victim mean (inside the range)
mean(da_model_fips$fe[da_model_fips$fe > 0], na.rm = TRUE)
mean(da_model_fips$mpv[da_model_fips$mpv > 0], na.rm = TRUE)
mean(
  da_model_fips$shr[
    da_model_fips$shr > 0 & da_model_fips$year <= 2023
  ],
  na.rm = TRUE
)

# victim range
range(da_model_fips$fe[da_model_fips$fe > 0], na.rm = TRUE)
range(da_model_fips$mpv[da_model_fips$mpv > 0], na.rm = TRUE)
range(
  da_model_fips$shr[
    da_model_fips$shr > 0 & da_model_fips$year <= 2023
  ],
  na.rm = TRUE
)

usethis::ui_info("Data visualization...")

source_colors <- c(
  "SHR" = "#0072B2", # Blue
  "Fatal Encounters" = "#D55E00", # Orange
  "Mapping Police Violence" = "#009E73" # Green
)

## VIZ BY YEAR

aux_agg <- da_model_state |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    shr = sum(shr, na.rm = TRUE),
    fe = sum(fe, na.rm = TRUE),
    mpv = sum(mpv, na.rm = TRUE)
  ) |>
  tidyr::pivot_longer(
    c(shr:mpv),
    names_to = "source",
    values_to = "count"
  )

yr_labels <- years_available() |>
  purrr::map(tibble::enframe) |>
  purrr::list_rbind(names_to = "source") |>
  dplyr::rename(year = value) |>
  dplyr::mutate(
    year = dplyr::if_else(year == 2025, 2024, year),
    hj = dplyr::if_else(name == 1, 1.1, -0.1)
  ) |>
  dplyr::inner_join(aux_agg, c("source", "year"))

p_year_count <- aux_agg |>
  dplyr::mutate(
    source = dplyr::case_match(
      source,
      "shr" ~ "SHR",
      "fe" ~ "Fatal Encounters",
      "mpv" ~ "Mapping Police Violence"
    )
  ) |>
  dplyr::filter(
    year <= 2024,
    count > 0
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = year, y = count, colour = source, fill = source) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(alpha = .1) +
  ggplot2::geom_text(ggplot2::aes(label = year, hjust = hj), data = yr_labels) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::scale_fill_manual(values = source_colors) +
  ggplot2::scale_colour_manual(values = source_colors) +
  # REVISION JLEA-26-0003 (Task 13 / R1-2): start the y-axis at 0 with a 0 break
  # so the SHR series (~350-480) no longer appears to sit below an implied zero
  # line. The editor confirmed the SHR line is around 500, never negative.
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 1500, 250),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  ) +
  ggplot2::labs(
    title = "Victim Count by Data Source",
    subtitle = paste(
      "Comparison of victim counts by year across SHR,",
      "Fatal Encounters (FE), and Mapping Police Violence (MPV)",
      sep = "\n"
    ),
    x = "Year",
    y = NULL,
    fill = "Data Source:",
    colour = "Data Source:"
  ) +
  theme_pub()

ggplot2::ggsave(
  "data-raw/png/p_year_count.png",
  p_year_count,
  dpi = 300,
  bg = "white",
  width = 9,
  height = 6
)

# DISTRIBUTION BY RACE AND SEX
dist_race <- da_model_fips |>
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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
      "Comparison of victim proportions by race across SHR, FE and MPV"
    ),
    x = "Proportion of Total Victims",
    y = NULL,
    fill = "Data Source:",
    caption = "Note: Data from 2013 to 2021."
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
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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
    subtitle = "Comparison of victim proportions by race across SHR, FE and MPV.",
    x = "Proportion of Total Victims",
    y = NULL,
    fill = "Data Source:",
    caption = "Note: 'Unknown/Others' categories have been excluded. Data from 2013 to 2021."
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
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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
    caption = "Note: 'Unknown/Others' categories have been excluded. Data from 2013 to 2021."
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
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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
    caption = "Note: 'Unknown/Others' categories have been excluded. Data from 2013 to 2021."
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

counties_sf <- tigris::counties(cb = TRUE, year = 2023) |>
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP"))

states_sf <- tigris::states(cb = TRUE, year = 2023) |>
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP"))

da_prop_black_compare <- da_model_fips |>
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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

p_map_race <- counties_sf |>
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
      c(-1.1, -0.1, -0.01, 0.01, 0.1, 1.1),
      labels = c(
        "Large Underreporting (< -10%)",
        "Mild Underreporting (-10% to -1%)",
        "Near parity (-1% to 1%)",
        "Mild Overreporting (1%  to 10%)",
        "Large Overreporting (> 10%)"
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
  ggplot2::scale_fill_manual(
    values = c(
      "Large Underreporting (< -10%)" = "#d73027",
      "Mild Underreporting (-10% to -1%)" = "#fc8d59",
      "Near parity (-1% to 1%)" = "#dddddd",
      "Mild Overreporting (1%  to 10%)" = "#a4bfdb",
      "Large Overreporting (> 10%)" = "#7575b4"
    )
  ) +
  ggplot2::facet_wrap(~comparison, ncol = 1) +
  ggplot2::labs(
    title = "Geographic Disparities in Reporting of Hispanic Victims",
    fill = "Difference in Proportion",
    caption = "Data sources: SHR, Fatal Encounters, Mapping Police Violence. Data from 2013 to 2021."
  ) +
  theme_pub() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.direction = "vertical",
    legend.position = "right"
  )

p_map_race

ggplot2::ggsave(
  "data-raw/png/map_hispanic_reporting_disparities.png",
  p_map_race,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 8
)


da_prop_female_compare <- da_model_fips |>
  dplyr::filter(dplyr::between(year, 2013, 2021)) |>
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
  dplyr::filter(
    !is.nan(prop_diff),
    !is.na(prop_diff),
    !is.infinite(prop_diff)
  )

p_map_sex <- counties_sf |>
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
      c(-1.1, -0.1, -0.01, 0.01, 0.1, 1.1),
      labels = c(
        "Large Underreporting (< -10%)",
        "Mild Underreporting (-10% to -1%)",
        "Near parity (-1% to 1%)",
        "Mild Overreporting (1%  to 10%)",
        "Large Overreporting (> 10%)"
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
  ggplot2::scale_fill_manual(
    values = c(
      "Large Underreporting (< -10%)" = "#d73027",
      "Mild Underreporting (-10% to -1%)" = "#fc8d59",
      "Near parity (-1% to 1%)" = "#dddddd",
      "Mild Overreporting (1%  to 10%)" = "#a4bfdb",
      "Large Overreporting (> 10%)" = "#7575b4"
    )
  ) +
  ggplot2::facet_wrap(~comparison, ncol = 1) +
  ggplot2::labs(
    title = "Geographic Disparities in Reporting of Female Victims",
    fill = "Difference in Proportion",
    caption = "Data sources: SHR, Fatal Encounters, Mapping Police Violence. Data from 2013 to 2021."
  ) +
  theme_pub() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.direction = "vertical",
    legend.position = "right"
  )

p_map_sex

ggplot2::ggsave(
  "data-raw/png/map_female_reporting_disparities.png",
  p_map_sex,
  dpi = 300,
  bg = "white",
  width = 8,
  height = 8
)

# PER-CELL COUNT HISTOGRAMS --------------------------------------------------
# REVISION JLEA-26-0003 (R1/reviewer, descriptive request): before the models,
# show the per-cell victim count distribution for each source (2013-2021, known
# race & sex), INCLUDING the zeros. One facet per source; because the SHR sample
# depends on the benchmark, its facet shows the two estimation samples side by
# side (FE sample, MPV sample). Colour encodes the sample. The benchmarks put
# their mass at 1+; the SHR piles at 0 (its zero share, 71% FE / 66% MPV,
# matches Table 2 Panel B), a direct picture of the underreporting. Absolute
# frequency, x truncated at 10 (counts run to 26-32).

count_base_colors <- c("FE sample" = "#D55E00", "MPV sample" = "#009E73")

count_cells <- da_model_fips |>
  dplyr::filter(
    dplyr::between(year, 2013, 2021),
    race %in% c("Black", "Hispanic", "White"),
    sex %in% c("Male", "Female")
  ) |>
  dplyr::mutate(
    shr0 = dplyr::coalesce(as.integer(shr), 0L),
    fe0 = dplyr::coalesce(as.integer(fe), 0L),
    mpv0 = dplyr::coalesce(as.integer(mpv), 0L)
  )

count_fe_sample <- dplyr::filter(count_cells, shr0 > 0 | fe0 > 0)
count_mpv_sample <- dplyr::filter(count_cells, shr0 > 0 | mpv0 > 0)

count_mk <- function(df, count_col, panel, base) {
  df |>
    dplyr::transmute(count = .data[[count_col]], panel = panel, base = base)
}

count_hist <- dplyr::bind_rows(
  count_mk(count_fe_sample, "fe0", "FE", "FE sample"),
  count_mk(count_mpv_sample, "mpv0", "MPV", "MPV sample"),
  count_mk(count_fe_sample, "shr0", "SHR", "FE sample"),
  count_mk(count_mpv_sample, "shr0", "SHR", "MPV sample")
) |>
  dplyr::mutate(
    panel = factor(panel, levels = c("FE", "MPV", "SHR")),
    base = factor(base, levels = c("FE sample", "MPV sample"))
  ) |>
  dplyr::count(panel, base, count, name = "n")

p_count_dist <- count_hist |>
  ggplot2::ggplot(ggplot2::aes(x = count, y = n, fill = base)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.9), width = 0.85
  ) +
  ggplot2::facet_wrap(~panel, nrow = 1) +
  ggplot2::coord_cartesian(xlim = c(-0.5, 10)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 10, 2)) +
  ggplot2::scale_fill_manual(values = count_base_colors) +
  ggplot2::labs(
    title = "Victim Counts per Observation, by Data Source",
    subtitle = paste(
      "Number of observations with each victim count (2013-2021, known race and sex).",
      "The SHR is zero in most observations; the benchmarks are not.",
      sep = "\n"
    ),
    x = "Victims recorded",
    y = "Number of observations",
    fill = "Estimation sample",
    caption = "Observations where the SHR or a benchmark records a victim. Counts run to 26-32; axis truncated at 10."
  ) +
  theme_pub() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "data-raw/png/p_count_dist.png",
  p_count_dist,
  dpi = 300,
  bg = "white",
  width = 11,
  height = 4.6
)
