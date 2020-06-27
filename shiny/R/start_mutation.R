#' start_mutation
#' @description Kinder einer Generation werden mit einer Wahrscheinlichkeit p
#'     mutiert (reversed mutation). Dabei werden im Chromosome Start- und 
#'     Endpunkte gewaehlt, deren Reihenfolge umgedreht und wieder in das 
#'     urspruengliche Chromosome eingefuegt. 
#' @param children tibble mit den Kindern der aktuellen Generation
#' @param mutation_prob Numeric, Standard 0.2, Werte zwischen 0 und 1. 
#'     Wahrscheinlichkeit mit der das Chromosome eines Kindes der aktuellen
#'     Generation einer Mutation unterzogen wird.
#' @param seed Numeric, Standard NULL, die Moeglichkeit den Output der Funktion
#'     durch setzen eines seeds reproduzierbar zu machen. 
start_mutation <- function(children,
                           mutation_prob=0.2,
                           seed=NULL){
    if (!is.null(seed)) set.seed(seed)
    
    children <- lapply(children$IID,function(iid){
        child <- children %>%
            dplyr::filter(IID==iid)
        if (runif(1)<=mutation_prob & !child$ELITE){
            # reversed mutation
            chromosome <- child$CHROMOSOME[[1]]
            chromosome_len <- length(chromosome)
            pos <- sample(chromosome_len,size=2)
            pos <- seq(min(pos),max(pos))
            chromosome[pos] <- chromosome[rev(pos)]
            child$CHROMOSOME <- list(chromosome)
        }
        return(child)
    }) %>%
        dplyr::bind_rows() %>% 
        dplyr::mutate(ELITE=NA)
    return(children)
}