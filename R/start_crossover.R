#' start_crossover
#' @description erzeugt Kinder der naechsten Generation. Kinder der naechsten 
#'     Generation umfassen einerseits potentielle Eliten, andererseits werden
#'     die Chromosome zweier Eltern mit einer Wahrscheinlichkeit p miteinander
#'     gekreuzt. Als Kreuzung wird ein 'ordered crossover' durchgefuehrt.
#' @param parents tibble mit Informationen zu den Eltern (Teilmenge der Population)
#' @param children_n Numeric, Anzahl der zu erzeugenden Kinder. Sollte der Anzahl
#'     der Individuen innerhalb einer Generation entsprechen.
#' @param crossover_prob Numeric, Standard 0.9, Wert zwischen 0 und 1. 
#'     Wahrscheinlichkeit mit der es zu einer Kreuzung der Chromosome kommt. Wenn
#'     es zu keiner Kreuzung kommt, entspricht das Chromosome des Kindes jenem 
#'     von Elternteil A.
#' @param seed Numeric, Standard NULL, die Moeglichkeit den Output der Funktion
#'     durch setzen eines seeds reproduzierbar zu machen. #'
start_crossover <- function(parents,
                            children_n,
                            crossover_prob=0.9,
                            seed=NULL){
    
    # Eliten gehen direkt in naechste Generation
    children_elite <- parents %>%
        dplyr::filter(ELITE) %>%
        dplyr::mutate(SCORE=NA,
                      FITNESS=NA,
                      RANK=NA)
    # Mating-Pool fuer alle anderen; jedes Paar erzeugt 1 Kind
    id_pool <- rep(1:nrow(parents),length.out=children_n-nrow(children_elite))
    parents_A <- parents$CHROMOSOME[id_pool]
    parents_B <- parents$CHROMOSOME[sample(id_pool,size=length(id_pool),replace=T)]
    
    chromosomes <- lapply(1:length(parents_A),function(id){
        chromosome_A <- parents_A[[id]]
        if (runif(1)<=crossover_prob){
            # Partner-Chromosome fuer crossover waehlen
            chromosome_B <- parents_B[[id]]
            chromosome_len <- length(chromosome_A)
            # Punkte fuer Crossover waehlen
            pos <- sample(chromosome_len,size=2)
            pos <- seq(min(pos),max(pos))
            # neues Chromosome erzeugen: ordered crossover
            chromosome_C <- rep(NA,chromosome_len)
            chromosome_C[pos] <- chromosome_A[pos]
            chromosome_C[is.na(chromosome_C)] <- chromosome_B[!chromosome_B %in% chromosome_A[pos]]
        } else {
            chromosome_C <- chromosome_A
        }
        return(chromosome_C)
    })
    
    # Children erzeugen: Population muss konstant bleiben (children_n)
    generation <- parents$GENERATION[1]+1
    children <- dplyr::tibble(
        IID=NA,
        CHROMOSOME=chromosomes,
        SCORE=NA,
        FITNESS=NA,
        RANK=NA,
        ELITE=FALSE) %>%
        dplyr::bind_rows(children_elite) %>%
        dplyr::mutate(IID=seq(children_n),
                      GENERATION=generation)
    return(children)
}