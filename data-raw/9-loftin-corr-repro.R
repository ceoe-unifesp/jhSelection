# Create data from Loftin et al. Figure 2 with correct values
state_data <- tibble::tribble(
  ~state                 , ~shr_count , ~nvss_count ,
  "Minnesota"            ,         48 ,          23 ,
  "California"           ,       2295 ,        1180 ,
  "Missouri"             ,        169 ,          90 ,
  "Illinois"             ,        347 ,         188 ,
  "Tennessee"            ,        173 ,          97 ,
  "Pennsylvania"         ,        283 ,         168 ,
  "Hawaii"               ,         23 ,          14 ,
  "Maryland"             ,        270 ,         166 ,
  "Texas"                ,        770 ,         502 ,
  "New York"             ,        670 ,         444 ,
  "New Jersey"           ,        218 ,         149 ,
  "Louisiana"            ,        186 ,         137 ,
  "Michigan"             ,        368 ,         272 ,
  "Florida"              ,        355 ,         271 ,
  "South Carolina"       ,         64 ,          49 ,
  "Nebraska"             ,         20 ,          17 ,
  "Colorado"             ,        132 ,         115 ,
  "Wisconsin"            ,         89 ,          78 ,
  "Alabama"              ,         75 ,          67 ,
  "Washington"           ,        108 ,          97 ,
  "District of Columbia" ,        118 ,         111 ,
  "Alaska"               ,         11 ,          11 ,
  "Virginia"             ,        169 ,         175 ,
  "Ohio"                 ,        264 ,         275 ,
  "Arizona"              ,        221 ,         244 ,
  "Idaho"                ,         23 ,          26 ,
  "Oklahoma"             ,        201 ,         228 ,
  "Oregon"               ,        104 ,         123 ,
  "New Hampshire"        ,         10 ,          12 ,
  "Georgia"              ,        147 ,         181 ,
  "Arkansas"             ,         63 ,          78 ,
  "Nevada"               ,         86 ,         109 ,
  "Delaware"             ,         10 ,          13 ,
  "Connecticut"          ,         41 ,          55 ,
  "Wyoming"              ,         10 ,          14 ,
  "North Carolina"       ,         94 ,         135 ,
  "Kansas"               ,         44 ,          70 ,
  "New Mexico"           ,         68 ,         112 ,
  "Mississippi"          ,         53 ,          88 ,
  "Vermont"              ,          3 ,           5 ,
  "Indiana"              ,         82 ,         139 ,
  "Massachusetts"        ,         37 ,          63 ,
  "Utah"                 ,         24 ,          41 ,
  "West Virginia"        ,         25 ,          43 ,
  "Kentucky"             ,         43 ,          84 ,
  "Iowa"                 ,         20 ,          45 ,
  "Maine"                ,          6 ,          14 ,
  "Rhode Island"         ,          9 ,          24 ,
  "South Dakota"         ,          3 ,           9 ,
  "Montana"              ,          4 ,          23 ,
  "North Dakota"         ,          2 ,          12
)

# Calculate the ratio (SHR/NVSS) and the difference for plotting
large_states <- c(
  "California",
  "Texas",
  "New York",
  "Florida",
  "Illinois",
  "Pennsylvania",
  "Ohio",
  "Michigan",
  "Georgia",
  "North Carolina"
)
state_data <- state_data |>
  dplyr::mutate(
    ratio = shr_count / nvss_count,
    ratio_minus_1 = ratio - 1,
    state_label = paste0(state, "\n", shr_count, ":", nvss_count),
    # Identify large vs small states for coloring
    population_category = dplyr::case_when(
      state %in% large_states ~ "Large State",
      TRUE ~ "Small/Medium State"
    )
  ) |>
  dplyr::arrange(ratio_minus_1) |>
  # Reorder states by ratio for plotting
  dplyr::mutate(
    state = factor(state, levels = state)
  )

theme_publication <- function(base_size = 14) {
  `%+replace%` <- ggplot2::`%+replace%`
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.4),
        face = "bold",
        margin = ggplot2::margin(b = 15)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = ggplot2::rel(1.0),
        color = "grey40",
        margin = ggplot2::margin(b = 15)
      ),
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(1.0),
        face = "bold"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12, b = 5)
      ),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(r = 12, l = 5)
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = "grey30"
      ),
      axis.text.y = ggplot2::element_text(
        size = ggplot2::rel(1.1),
        margin = ggplot2::margin(r = 8)
      ), # Increased from 0.95 to 1.1
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.line = ggplot2::element_line(color = "grey40", size = 0.2),
      panel.grid.major.x = ggplot2::element_line(color = "grey92"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.margin = ggplot2::margin(t = 15),
      plot.margin = ggplot2::margin(t = 15, r = 25, b = 15, l = 25),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# Create the horizontal bar chart
p_loftin <- ggplot2::ggplot(
  state_data,
  ggplot2::aes(x = ratio_minus_1, y = state, fill = population_category)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "solid",
    color = "grey60",
    size = 0.5
  ) +
  ggplot2::geom_col(alpha = 0.8, width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = ifelse(
        ratio_minus_1 >= 0,
        ratio_minus_1 + 0.05,
        ratio_minus_1 - 0.05
      ),
      label = paste0(shr_count, ":", nvss_count)
    ),
    hjust = ifelse(state_data$ratio_minus_1 >= 0, 0, 1),
    size = 3.5, # Increased from 2.8 to 3.5
    color = "grey20"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Large State" = "#2E86AB",
      "Small/Medium State" = "#A23B72"
    ),
    labels = c(
      "Large State" = "Large States (Top 10 by population)",
      "Small/Medium State" = "Small/Medium States"
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(-1.5, 1.5, 0.5),
    labels = seq(-1.5, 1.5, 0.5),
    limits = c(-1.6, 1.6),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::labs(
    title = "Police Justifiable Homicides: State-by-State Reporting Disparities",
    subtitle = "**Large states typically report more cases to FBI (SHR) than appear in National Vital Statistics (NVSS)**<br>
    Ratio shown as (SHR count / NVSS count) - 1, with individual state counts displayed",
    x = "SHR to NVSS Ratio (minus 1)",
    y = "State",
    caption = "Reproduction of Loftin et al. (2003) Figure 2\nPositive values: more SHR cases; Negative values: more NVSS cases\nData: FBI Supplementary Homicide Reports vs National Vital Statistics System (1976-1998)"
  ) +
  theme_publication()

# Save the plot with optimized settings for font visibility
ggplot2::ggsave(
  "data-raw/pdf/loftin_figure2_reproduction.pdf",
  plot = p_loftin,
  width = 10, # Reduced width for better text scaling
  height = 14, # Reduced height
  units = "in",
  dpi = 300,
  device = cairo_pdf
)

ggplot2::ggsave(
  "data-raw/png/loftin_figure2_reproduction.png",
  plot = p_loftin,
  width = 10, # Reduced width for better text scaling
  height = 14, # Reduced height
  units = "in",
  dpi = 300,
  bg = "white"
)

# Correlation analysis --------------------------------------------------
shr_nvss <- state_data |>
  dplyr::transmute(
    state = state,
    shr = shr_count,
    nvss = nvss_count,
    ratio = ratio,
    ratio_idx = ratio_minus_1
  )

# Census: Population & Density
# Pull total population for each state (2023 ACS 1yr)
pop_data <- tidycensus::get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2023,
  survey = "acs1"
) |>
  dplyr::select(state = NAME, population = estimate)

# Pull land area and compute density (people per sq mile)
land <- tidycensus::get_acs(
  geography = "state",
  variables = "B01003_001",
  geometry = TRUE,
  year = 2023
) |>
  sf::st_transform(9311) |>
  dplyr::mutate(area_sq_mi = as.numeric(sf::st_area(geometry)) / 2.59e+6) |>
  sf::st_drop_geometry() |>
  dplyr::select(state = NAME, area_sq_mi)

pop_density <- pop_data |>
  dplyr::left_join(land, by = "state") |>
  dplyr::mutate(density = population / area_sq_mi)

# Count of Cities >= 100K
places <- tidycensus::get_acs(
  geography = "place",
  variables = "B01003_001",
  year = 2023,
  survey = "acs1"
)

big_cities <- places |>
  dplyr::filter(estimate >= 100000) |>
  dplyr::mutate(state = stringr::str_extract(NAME, "(?<=,\\s)([A-Za-z ]+)$")) |>
  dplyr::group_by(state) |>
  dplyr::summarise(cities_100k = dplyr::n())

# Political Ideology Variables
# 2024 election vote share by state
# Source: https://en.wikipedia.org/wiki/2024_United_States_presidential_election
# Source: https://apnews.com/projects/election-results-2024/?office=P
vote24 <- tibble::tribble(
  ~state                 , ~year , ~rep_votes , ~rep_share , ~dem_votes , ~dem_share ,
  "Alabama"              , 2024L , 1462616L   , 64.57      ,  772412L   , 34.10      ,
  "Alaska"               , 2024L ,  184458L   , 54.54      ,  140026L   , 41.41      ,
  "Arizona"              , 2024L , 1770242L   , 52.22      , 1582860L   , 46.69      ,
  "Arkansas"             , 2024L ,  759241L   , 64.20      ,  396905L   , 33.56      ,
  "California"           , 2024L , 6081697L   , 38.33      , 9276179L   , 58.47      ,
  "Colorado"             , 2024L , 1377441L   , 43.14      , 1728159L   , 54.13      ,
  "Connecticut"          , 2024L ,  736918L   , 41.89      ,  992053L   , 56.40      ,
  "Delaware"             , 2024L ,  214351L   , 41.79      ,  289758L   , 56.49      ,
  "District of Columbia" , 2024L ,   21076L   ,  6.47      ,  294185L   , 90.28      ,
  "Florida"              , 2024L , 6110125L   , 56.09      , 4683038L   , 42.99      ,
  "Georgia"              , 2024L , 2668700L   , 50.72      , 2554416L   , 48.53      ,
  "Hawaii"               , 2024L ,  221693L   , 37.48      ,  358593L   , 60.59      ,
  "Idaho"                , 2024L ,  554119L   , 66.85      ,  254765L   , 30.72      ,
  "Illinois"             , 2024L , 2552905L   , 43.22      , 3283641L   , 55.60      ,
  "Indiana"              , 2024L , 1601130L   , 57.25      , 1142700L   , 40.85      ,
  "Iowa"                 , 2024L ,  873449L   , 50.42      ,  840351L   , 48.51      ,
  "Kansas"               , 2024L ,  726266L   , 56.20      ,  548950L   , 42.48      ,
  "Kentucky"             , 2024L , 1350097L   , 64.60      ,  714623L   , 34.20      ,
  "Louisiana"            , 2024L , 1250005L   , 58.04      ,  878239L   , 40.80      ,
  "Maine"                , 2024L ,  377977L   , 45.46      ,  435652L   , 52.40      ,
  "Maryland"             , 2024L ,  957873L   , 36.59      , 1607471L   , 61.39      ,
  "Massachusetts"        , 2024L , 1126053L   , 33.80      , 2132035L   , 63.98      ,
  "Michigan"             , 2024L , 2815700L   , 48.77      , 2803113L   , 48.56      ,
  "Minnesota"            , 2024L , 1476069L   , 45.54      , 1726193L   , 53.26      ,
  "Mississippi"          , 2024L ,  709667L   , 65.74      ,  357211L   , 33.08      ,
  "Missouri"             , 2024L , 1719851L   , 58.44      , 1163860L   , 39.52      ,
  "Montana"              , 2024L ,  343602L   , 59.22      ,  226153L   , 38.99      ,
  "Nebraska"             , 2024L ,  510456L   , 59.26      ,  340266L   , 39.52      ,
  "Nevada"               , 2024L ,  751205L   , 50.60      ,  717749L   , 48.35      ,
  "New Hampshire"        , 2024L ,  344940L   , 45.47      ,  403874L   , 53.24      ,
  "New Jersey"           , 2024L , 1944327L   , 41.96      , 2627469L   , 56.69      ,
  "New Mexico"           , 2024L ,  401286L   , 44.34      ,  489416L   , 54.08      ,
  "New York"             , 2024L , 3536770L   , 37.34      , 5705921L   , 60.23      ,
  "North Carolina"       , 2024L , 2793381L   , 50.76      , 2638836L   , 47.95      ,
  "North Dakota"         , 2024L ,  236603L   , 64.49      ,  125093L   , 34.11      ,
  "Ohio"                 , 2024L , 2964388L   , 55.11      , 2363442L   , 43.96      ,
  "Oklahoma"             , 2024L , 1106420L   , 66.39      ,  533601L   , 32.03      ,
  "Oregon"               , 2024L , 1073499L   , 40.20      , 1538819L   , 57.64      ,
  "Pennsylvania"         , 2024L , 3469659L   , 50.61      , 3310966L   , 48.30      ,
  "Rhode Island"         , 2024L ,  206223L   , 40.21      ,  300476L   , 58.62      ,
  "South Carolina"       , 2024L , 1381102L   , 57.74      ,  983171L   , 41.11      ,
  "South Dakota"         , 2024L ,  278654L   , 62.11      ,  165457L   , 36.90      ,
  "Tennessee"            , 2024L , 1960815L   , 64.29      , 1065643L   , 34.94      ,
  "Texas"                , 2024L , 6135485L   , 55.76      , 4705793L   , 42.79      ,
  "Utah"                 , 2024L ,  776754L   , 59.54      ,  494843L   , 37.92      ,
  "Vermont"              , 2024L ,  119857L   , 31.27      ,  252922L   , 65.95      ,
  "Virginia"             , 2024L , 2075085L   , 46.05      , 2335395L   , 51.83      ,
  "Washington"           , 2024L , 1530923L   , 39.01      , 2245849L   , 57.23      ,
  "West Virginia"        , 2024L ,  533556L   , 69.97      ,  214309L   , 28.10      ,
  "Wisconsin"            , 2024L , 1697626L   , 49.60      , 1668229L   , 48.74      ,
  "Wyoming"              , 2024L ,  192633L   , 71.60      ,   69527L   , 25.84
) |>
  dplyr::mutate(vote_margin = rep_share - dem_share) |>
  dplyr::select(state, vote_margin)

# Merge everything
df_corr <- shr_nvss |>
  dplyr::left_join(pop_density, by = "state") |>
  dplyr::left_join(big_cities, by = "state") |>
  dplyr::left_join(vote24, by = "state") |>
  tidyr::replace_na(list(cities_100k = 0))

# Correlations
vars <- c("population", "density", "cities_100k", "vote_margin") |>
  purrr::set_names()

cor_res <- purrr::map(vars, function(v) {
  test <- cor.test(df_corr$ratio_idx, df_corr[[v]], method = "spearman")
  broom::tidy(test)
}) |>
  purrr::list_rbind(names_to = "variable")

cor_res
