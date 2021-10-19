#' Title
#'
#' @param bd
#' @param variable
#' @param color_min
#' @param color_max
#'
#' @return
#' @export
#' @importFrom grDevices colorRamp
#'
#' @examples
#' Ejemplo
#' library(dplyr)
#' library(ggplot2)
#' library(grDevices)
#'
#' bd <- tibble(a = letters, b= rnorm(length(letters)))
#'
#'bd %>% rampa(b, "white","red") %>%
#'ggplot(aes(x = b, y = fct_reorder(a,b), fill = color)) + geom_col( ) +
#'scale_fill_identity()

rampa <- function(bd, variable, color_min, color_max){
  bd %>% arrange(desc({{variable}})) %>%
    mutate(color = colorRampPalette(c(color_min, color_max))(nrow(bd)))
}


