#' score_individuals
#' @description berechnet verschiedene Merkmale, um Individuen reihen zu koennen.
#'     Berechnet werden Score (Laenge eines Chromosomes/einer Tour), Rank
#'     (Reihnung der Individuen anhand des Scores) und Fitness. Fitness kann eine
#'     benutzerdefinierte Funktion sein. Wird keine Funktion angegeben 
#'     (.fitness_fun NULL), wird der jeweilige Score-Wert zwischen 0.01 und 100
#'     skaliert. Die Fitness dient im Weiteren zur Auswahl der Eltern.
#' @param individuals tibble, mit allen Individuen der Population
#' @param genes tibble, mit allen Genen 
#' @param .fitness_fun Funktion, Standard NULL, Funktion zur Berechnung der 
#'     Fitness
#' @param do_par Boolean, Standard TRUE, gibt an, ob die Berechnung der Score 
#'     parallelisiert oder nicht parallelisiert durchgefuehrt werden soll. Wenn
#'     do_par TRUE muss die Parallelisierung zuvor initialisiert werden (
#'     library(foreach); doParallel::registerDoParallel(num_cores)
score_individuals <- function(individuals,
                              genes,
                              .fitness_fun=NULL,
                              do_par=T){
    
    .fitness_fun <- rlang::enquo(.fitness_fun)
    if (rlang::quo_is_null(.fitness_fun)) {
        # scale score zwischen 0.01 und 100 - dabei score umdrehen; kuerzer=besser
        .fitness_fun <- rlang::quo(
            (-SCORE-min(-SCORE))/(max(-SCORE)-min(-SCORE))*(100-0.01)+0.01
        )
    }
    
    if (do_par){
        score <- foreach::foreach(i=1:nrow(individuals),.combine=c) %dopar% {
            calc_score(chromosome=individuals$CHROMOSOME[[i]],genes=genes)
        }
        individuals <- individuals %>%
            dplyr::mutate(SCORE=score,
            RANK=rank(SCORE,na.last="keep",ties.method="random"),
            FITNESS=!!.fitness_fun)
    } else {
        individuals <- individuals %>%
            dplyr::mutate(SCORE=sapply(individuals$CHROMOSOME, calc_score, genes),
                          RANK=rank(SCORE,na.last="keep",ties.method="random"),
                          FITNESS=!!.fitness_fun)
    }
    return(individuals)
}