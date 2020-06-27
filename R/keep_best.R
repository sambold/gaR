#' keep_best
#' @description Gibt das beste Individuum (niedrigster Score-Wert/kuerzeste Strecke)
#'     der aktuellen Generation aus. Zudem besteht die Option auch die bisherigen, 
#'     besten Individuen vergangener Generationen mit zu uebergeben und in ein
#'     Tibble zu ueberfuehren, das die besten Individuen jeder Generation enthaelt.
#' @param population tibble mit allen gescorten Individuen einer Generation
#' @param best tibble, Standard NULL, wenn nicht NULL ein tibble mit den 
#'     besten Individuen vergangener Generationen.
keep_best <- function(population,
                      best=NULL){
    best <- population %>%
        dplyr::filter(RANK==1) %>%
        dplyr::bind_rows(best)
    return(best)
}
