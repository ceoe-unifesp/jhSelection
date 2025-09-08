#' A ggplot2 theme for publication-quality graphics
#'
#' `theme_pub()` is a custom ggplot2 theme designed for creating publication-quality
#' graphics with a clean and professional appearance. It builds upon the
#' `theme_minimal()` base theme and customizes various elements to enhance
#' readability and aesthetics.
#'
#' @param base_size Numeric. Base font size for the theme. Default is 12.
#'
#' @return A ggplot2 theme object.
#'
#' @export
theme_pub <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.3),
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05),
        hjust = 0,
        color = "gray40",
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        hjust = 1,
        color = "gray50"
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(r = 10)
      ),
      panel.grid.major.y = ggplot2::element_line(
        color = "gray85",
        linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.1),
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}
