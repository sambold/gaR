#' start_ga
#' @description: Wrapper fuer einzelne GA-Funktionen
#' @param fname Standard NULL, wenn fname NULL ist, wird ein zufaelliger Genpool
#'     mit n Genen initialisiert. Handelt es sich bei fname um einen 
#'     Character-String, wird dieser als Pfadangabe zu einem Input-File interpretiert
#'     und es wird - je nach Endung - versucht, die Daten einzulesen und in die
#'     geforderte Struktur zu bringen. Ebenso kann ein fertiger Dataframe 
#'     uebergeben werden. In diesem Fall wird versucht ueber ein Mapping den 
#'     dataframe in die geforderte Struktur zu bringen.
#' @param gene_n numeric, Standard 10, wird nur verwendet, wenn fname NULL. Gibt
#'     die Anzahl der zu initialisierenden Gene an.
#' @param pop_n Numeric, Standard NULL, Populationsgroesse/Anzahl der zu 
#'     erzeugenden Individuen. Wenn n NULL, wird - in Abhaengigkeit der Anzahl
#'     der Gene - automatisch eine Groesse durch est_pop_size() berechnet.
#' @param crossover_prob Numeric, Standard 0.9, Wert zwischen 0 und 1. 
#'     Wahrscheinlichkeit mit der es zu einer Kreuzung der Chromosome kommt. Wenn
#'     es zu keiner Kreuzung kommt, entspricht das Chromosome des Kindes jenem 
#'     von Elternteil A.
#' @param mutation_prob Numeric, Standard 0.2, Werte zwischen 0 und 1. 
#'     Wahrscheinlichkeit mit der das Chromosome eines Kindes der aktuellen
#'     Generation einer Mutation unterzogen wird.
#' @param elite_frac Numeric, Standard NULL, Wert zwischen 0 und 1. Gibt den Anteil
#'     des Elternpools wider, der fuer Eliteneltern reserviert ist. Wenn NULL,
#'     gibt es keine Eliten.
#' @param max_generations Numeric, Standardwert 2000, maximale Anzahl der 
#'     Generationen, die erzeugt werden, um eine Loesung zu finden
#' @param local_min_thld Numeric; Standardwert floor(max_generations*0.1), 
#'     Anzahl der Gnerationen nach denen die Suche nach einer Loesung abgebrochen 
#'     wird, wenn sie immer die gleiche Tour als Loesung hervorbringen 
#' @param seed Numeric, Standard NULL, wenn nicht NULL wird seed verwendet, um
#'     Ergebnisse von init_genes reproduzierbar zu machen. 
#' @param do_par Boolean, Standardwert TRUE, gibt an, ob Berechnungen (Berechnung
#'     von Entfernungen) parallelisiert werden sollen oder nicht.
#' @param num_cores    Numeric, Standardwert NULL, falls do_par TRUE, gibt der
#'     Wert an, wieviele Kerne zur Parallelisierung verwendet werden sollen. Wenn
#'     NULL, werden alle vorhandenen Kerne - 1 verwendet
#' @param print_info Boolean, Standardwert TRUE, gibt an, ob Informationen zur
#'     aktuellen Generation/zur besten Runde und zum Local-Minimum-Counter 
#'     ausgegeben werden sollen
#' @param print_plot Boolean, Standardwert TRUE, gibt an, ob beste Runde einer
#'     Generation als Plot ausgegeben werden soll
#' @return Liste mit den besten Chromosomen jeder Generation sowie mit den Genen
start_ga <- function(fname=NULL,
                     gene_n=10,
                     pop_n=NULL,
                     crossover_prob=0.9,
                     mutation_prob=0.2,
                     elite_frac=0.1,
                     max_generations=2000,
                     local_min_thld=floor(max_generations*0.1),
                     seed=NULL,
                     do_par=TRUE,
                     num_cores=NULL,
                     dist_matrix_thld=90000,
                     print_info=T,
                     print_plot=T){
    # Init
    `%>%` <- magrittr::`%>%`
    if (!is.null(seed)) set.seed(NULL)
    
    if (do_par){
        library(foreach)
        if (is.null(num_cores)) num_cores <- parallel::detectCores()-1
        cl <- parallel::makeCluster(num_cores,type="FORK")
        doParallel::registerDoParallel(cl)    
    }
    local_min_counter <- 1
    is_local_min <- FALSE
    start <- Sys.time()
    
    # Genetic Algorithm starten
    genes <- init_genes(fname=fname,n=gene_n) 
    # wenn nicht zu gross fuer memory: Berechnung mit Distmatrix
    if (gene_n <= dist_matrix_thld) {
        dist_matrix <- dist(genes %>% dplyr::select(X,Y))
    } else {
        dist_matrix <- NULL
    }
    population <- genes %>%
        init_individuals(n=pop_n) %>%
        score_individuals(genes=genes,
                          dist_matrix=dist_matrix,
                          do_par=do_par)
    best_individuals <- population %>%
        keep_best()
    if (print_info){
        print(glue::glue("Generation {generation} - \\
                     Score: {score} - \\
                     Local-Minium-Counter: {local_min_counter}",
                         generation=best_individuals$GENERATION[1],
                         score=best_individuals$SCORE[1] %>%
                             format(big.mark='.', decimal.mark=',', nsmal=2)))
    }
    if (print_plot){
        name <- ifelse(is.null(fname),"Random Genes",fname)
        plot_genes(genes=genes, 
                   best_individuals$CHROMOSOME[[1]],
                   title=glue::glue("Genetic Algorithm: {name}"),
                   subtitle=glue::glue("Generation: {generation} - \\
                                           Score: {score}",
                                       generation=best_individuals$GENERATION[1],
                                       score=best_individuals$SCORE[1] %>%
                                           format(big.mark='.', decimal.mark=',', nsmal=2))) %>%
            print()
    }
    
    for (generation in 2:max_generations){
        children <- population %>%
            select_parents(elite_frac=elite_frac) %>%
            start_crossover(crossover_prob=crossover_prob,
                            children_n=nrow(population)) %>%
            start_mutation(mutation_prob=mutation_prob)
        population <- children %>%
            score_individuals(genes=genes,
                              do_par=do_par)
        best_individuals <- population %>%
            keep_best(best_individuals)
        
        if (print_info){
            print(glue::glue("Generation {generation} - \\
                     Score: {score} - \\
                     Local-Minium-Counter: {local_min_counter}",
                             generation=best_individuals$GENERATION[1],
                             score=best_individuals$SCORE[1] %>%
                                 format(big.mark='.', decimal.mark=',', nsmal=2)))
        }
        is_new_tour <- best_individuals$CHROMOSOME[c(1,2)] %>%
            unique() %>%
            length() %>%
            magrittr::is_greater_than(1)
        if (is_new_tour){
            if (print_plot){
                name <- ifelse(is.null(fname),"Random Genes",fname)
                plot_genes(genes=genes, 
                           best_individuals$CHROMOSOME[[1]],
                           title=glue::glue("Genetic Algorithm: {name}"),
                           subtitle=glue::glue("Generation: {generation} - \\
                                           Score: {score}",
                                               generation=best_individuals$GENERATION[1],
                                               score=best_individuals$SCORE[1] %>%
                                                   format(big.mark='.', decimal.mark=',', nsmal=2))) %>%
                    print()
            }
            local_min_counter <- 1
        } else {
            local_min_counter <- local_min_counter+1
            if (local_min_counter>=local_min_thld) {
                is_local_min <- TRUE
                break
            }
        }
    }
    if (do_par) parallel::stopCluster(cl)
    end <- Sys.time()
    if (is_local_min) {
        print(glue::glue("Genetic Algorithm nach {generation} Generationen \\
                         (Abbruch durch lokales Minimum) abgeschlossen. \n \\
                         Dauer: {duration} \\
                         Score zu Beginn: {score_begin} \n \\
                         Score am Ende: {score_end}",
                         duration=end-start,
                         score_begin=best_individuals %>% 
                             tail(1) %>%
                             .$SCROE %>%
                             format(big.mark=".",decimal.mark=",",nsmall=2),
                         score_end=best_individuals %>%
                             head(1) %>%
                             .$SCORE %>%
                             format(big.mark=".",decimal.mark=",",nsmall=2)))
    } else {
        print(glue::glue("Genetic Algorithm nach {generation} Generationen //
                         (Maximale Anzahl an Iterationen erreicht) abgeschlossen. \n //
                         Dauer: {duration} \\
                         Score zu Beginn: {score_begin} \n \\
                         Score am Ende: {score_end}",
                         duration=end-start,
                         score_begin=best_individuals %>% 
                             tail(1) %>%
                             .$SCROE %>%
                             format(big.mark=".",decimal.mark=",",nsmall=2),
                         score_end=best_individuals %>%
                             head(1) %>%
                             .$SCORE %>%
                             format(big.mark=".",decimal.mark=",",nsmall=2)))
    }
    return(list(genes=genes,
                best_individuals=best_individuals))
}
