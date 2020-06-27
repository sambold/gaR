#' select_parents
#' @description Waehlt eine Pool an n Eltern aus, die im Weiteren die Kinder 
#'     der neuen Generation erzeugen. Neben der Groesse des Elternpools kann auch
#'     der Anteil an Eliten-Eltern gewaelt werden. Eliten sind jeweils jene 
#'     Individuen mit dem niedrigsten Rang. Eliten gehen direkt in die naechste
#'     Generation ueber. Es gibt also kein Crossover und keine Mutation.
#'     In Zukunft werden mitunter noch weitere Selektionsmethoden eingefuehrt. 
#'     Aktuell kommt bei der Selektion die Roulette-Selektion zum Zug
#' @param individuals tibble, mit allen Individuen der Population
#' @param n Numeric, Standard NULL, Groesse des Elternpools. Wenn NULL, entspricht
#'     die Groesse des Elternpools der Haelfte aller Individuen.
#' @param elite_frac Numeric, Standard NULL, Wert zwischen 0 und 1. Gibt den Anteil
#'     des Elternpools wider, der fuer Eliteneltern reserviert ist. Wenn NULL,
#'     gibt es keine Eliten.
#' @param seed Numeric, Standard NULL, die Moeglichkeit den Output der Funktion
#'     durch setzen eines seeds reproduzierbar zu machen. 
select_parents <- function(individuals,
                           n=NULL,
                           elite_frac=NULL,
                           seed=NULL){
    
    if (!is.null(seed)) set.seed(seed)
    if (is.null(n)) n <- floor(nrow(individuals)/2)
    if (is.null(elite_frac)){
        elite_n <- 0
    } else {
        elite_n <- max(1,floor(n*elite_frac))
    }
    
    parents <- individuals %>%
        dplyr::sample_n(size=n-elite_n,
                        weight=individuals$FITNESS,
                        replace=T) %>%
        dplyr::mutate(ELITE=FALSE) %>%
        # Eliten hinzufuegen
        dplyr::bind_rows(individuals %>%
                             dplyr::filter(RANK<=elite_n) %>%
                             dplyr::mutate(ELITE=TRUE))
    return(parents)
}