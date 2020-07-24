#' calc_score
#' @description berechnet Laenge einer gegebenen Tour/Chromosom
#' @param chromosome Numeric, die Reihenfolge, in der die Gene durchlaufen
#'     werden sollen
#' @param genes tibble mit Informationen zu Genen (init_genes)
calc_score <- function(chromosome,
                       genes=NULL,
                       dist_matrix=NULL){
    `%>%` <- magrittr::`%>%`
    chromosome <- rep(chromosome,length.out=length(chromosome)+1)
    
    if (is.null(dist_matrix)){
        score <- genes$X[chromosome] %>%
            diff()  %>%
            magrittr::raise_to_power(2) %>%
            magrittr::add(genes$Y[chromosome] %>%
                              diff() %>%
                              magrittr::raise_to_power(2)) %>%
            sqrt() %>%
            sum()
    } else {
        tour <- embed(chromosome,2)
        score <- sum(usedist::dist_get(dist_matrix,idx1=tour[,2],idx2=tour[,1]))
    }
    return(score)
}