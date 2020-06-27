#' calc_score
#' @description berechnet Laenge einer gegebenen Tour/Chromosom
#' @param chromosome Numeric, die Reihenfolge, in der die Gene durchlaufen
#'     werden sollen
#' @param genes tibble mit Informationen zu Genen (init_genes)
calc_score <- function(chromosome,
                       genes){
    `%>%` <- magrittr::`%>%`
    chromosome <- rep(chromosome,length.out=length(chromosome)+1)
    score <- genes$X[chromosome] %>%
        diff()  %>%
        magrittr::raise_to_power(2) %>%
        magrittr::add(genes$Y[chromosome] %>%
                          diff() %>%
                          magrittr::raise_to_power(2)) %>%
        sqrt() %>%
        sum()
    return(score)
}