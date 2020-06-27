#' est_pop_size:
#' @description berechnet Groesse der Population basierend auf Anzahl der 
#'     Dimensionen/Gene und einer Wahrscheinlichkeit(?) 
#'@param n numeric, Anzahl der Dimensionen/Gene
#'@param p numeric, Standard 0.99, Wahrscheinlichkeit(?)
#'
est_pop_size <- function(n,
                         p=0.99){
    pop_size <- ceiling(log(1-p^(1/n))/log((n-3)/(n-1)))
    return(pop_size)
}