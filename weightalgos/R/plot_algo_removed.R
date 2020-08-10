#' visualize algorithm output
#'
#' function to plot a 2D jittered plot juxtaposing algorithm-removed data on
#' "top" or in "front" of input data.
#'
#' @param df \code{data.frame} holding raw (input) measurements and algorithm-
#'   processed (output) measurements.
#' @param input string corresponding to the name of the original weight data in
#'   \code{df}.
#' @param output string corresponding to the name of the algorithm processed
#'   weight data in \code{df}.
#' @param title character string, desired plot title
#' @return ggplot object
plot_algo_removed <- function(df, input, output, title) {
  input  <- rlang::sym(input)
  output <- rlang::sym(output)

  df %>%
    filter(!is.na(!!output)) %>%
    ggplot(aes(x = "", y = !!output)) +
    geom_jitter(alpha = 0.5, color = "black") +
    geom_jitter(
      data = df %>% filter(!is.na(!!input)),
      aes(x = "", y = !!input),
      color = "red",
      alpha = 0.5
    ) +
    theme(
      axis.title = element_text(),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(x = "", y = "", title = title)
}

