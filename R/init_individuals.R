#' init_individuals
#' @description Uebernimmt ein Set an Genen und erzeugt daraus n zufaellige
#'     Chromosome. Ergaenzt um eien ID (IID), Informationen zur jeweiligen
#'     Generation sowie Score, Fitness und Rank entstehen Individuen der 
#'     Population.
#' @param genes tibble mit Informationen zu Genen (init_genes)
#' @param n Numeric, Standard NULL, Populationsgroesse/Anzahl der zu 
#'     erzeugenden Individuen. Wenn n NULL, wird - in Abhaengigkeit der Anzahl
#'     der Gene - automatisch eine Groesse durch est_pop_size() berechnet.
#' @param seed Numeric, Standard NULL, die Moeglichkeit den Output der Funktion
#'     durch setzen eines seeds reproduzierbar zu machen. 
init_individuals <- function(genes,
                             n=NULL,
                             seed=NULL){
    
    if (!is.null(seed)) set.seed(seed)
    if (is.null(n)) n <- est_pop_size(nrow(genes))
    
    individuals <- dplyr::tibble(
        IID=seq(n),
        GENERATION=1,
        CHROMOSOME=lapply(seq(n),
                          function(iid) sample(genes$GID)),
        SCORE=NA,
        FITNESS=NA,
        RANK=NA,
        ELITE=NA)
    return(individuals)
}