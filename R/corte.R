

#' Title
#'
#' @param bd tibble. Es la tabla a la que le agregaremos la columna 'color'
#' @param variable nse. Es la variable en formato 'non standard evaluation' a la que
#' se va a cortar por cuantil
#' @param corte vector int. Vector de cortes base para hacer el corte de colores
#' @param cuantil  lgl. Indica si el vector corte debará traducirse en cuantiles de
#' 'variable'
#' @param paleta vector chr. Vector de colores. La cantidad de colores depende
#' de la cantidad de la longitud del parámetro 'corte' (length(corte)-1)
#'
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
#' Ejemplo
#' bd <- tibble(a = rnorm(50))
#' bd %>% corte(a, corte = c(0,.5,1), cuantil = T, paleta = c("red","green"))
#'
#'
corte <- function(bd, variable, corte, cuantil = FALSE, paleta){
  corte <- if(cuantil) quantile(bd %>% pull({{variable}}), cuantil) else corte
  bd %>% mutate(
    color = as.character(
      cut({{variable}},
          corte,
          include.lowest = T, labels = paleta)
    )
  )
}



