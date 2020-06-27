#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. INIT ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`%>%` <- magrittr::`%>%`

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# B. FUNKTIONEN ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files <- list.files("R",full.names = T)
for (f in files){
    source(f)
}

# Loop mit Verzoegerung, dem weitere Konstanten mit uebergeben werden koennen
map_later <- function(max_iter,       # Anzahl zu durchlaufender Iterationen
                      f,              # Auszuführende Funktion
                      ...,            # Parameter für auszuführende Funktion
                      interval=0){    # Verzögerung bei Update
    i <- 0
    max_iter <- 1:max_iter
    shiny::observe({
        i <<- i + 1
        if (i < length(max_iter)) shiny::invalidateLater(interval)
        f(max_iter[i],...)
    })
}

# Ausgabeformat fuer difftime 00:00:00:00
Fmt <- function(x) UseMethod("Fmt")

Fmt.difftime <- function(x) {
    units(x) <- "secs"
    x <- unclass(x)
    NextMethod()
}
Fmt.default <- function(x) {
    y <- abs(x)
    sprintf("%s%02d:%02d:%02d:%02d", 
            ifelse(x < 0, "-", ""), # sign
            y %/% 86400,  # days
            y %% 86400 %/% 3600,  # hours 
            y %% 3600 %/% 60,  # minutes
            y %% 60 %/% 1) # seconds
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C. UI ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- shinydashboard::dashboardHeader(
    title="Genetic Algorithm: Traveling Salesman Problem")

sidebar <- shinydashboard::dashboardSidebar(
    shiny::selectInput(
        inputId="select_map",
        label="Select Map",
        choices=c("Djibouti38"="dj38.tsp",
                  "Berlin52"="berlin52.tsp",
                  "Qatar194"="qa194.tsp",
                  "Luxembourg980"="lu980.tsp",
                  "Rwanda1621"="rw1621.tsp",
                  "Nicaragua3496"="nu3496.tsp",
                  "Argentinia9152"="ar9152.tsp")),
    shiny::sliderInput(
        inputId="set_crossover_prob",
        label="Crossover Probability",
        min=0,
        max=1,
        value=0.99,
        step=0.01),
    shiny::sliderInput(
        inputId="set_mutation_prob",
        label="Mutation Probability",
        min=0,
        max=1,
        value=0.25,
        step=0.01),
    shiny::sliderInput(
        inputId="set_elite_frac",
        label="Eliten-Anteil",
        min=0,
        max=1,
        value=0.2,
        step=0.01),
    shiny::numericInput(
        inputId="set_max_generations",
        label="Maximum Number of Generations",
        value=2000,
        min=1,
        max=20000),
    shiny::numericInput(
        inputId="set_local_minimum_thld",
        label="Local Minimum: number of identical tours",
        value=100,
        min=1,
        max=1000),
    shiny::actionButton(
        inputId="start_evolution",
        label="Start Evolution"))

body <- shinydashboard::dashboardBody(
    shiny::fluidRow(
        shinydashboard::box(
            title="Best Tour: Plot",
            solidHeader=TRUE,
            collapsible=TRUE,
            shiny::plotOutput("plot_tsp")
        ),
        shinydashboard::box(
            title="Convergence",
            solidHeader=TRUE,
            collapsible=TRUE,
            shiny::plotOutput("plot_score")
        )
    ),
    shiny::fluidRow(
        shinydashboard::box(
            title="Best Tour: Genes",
            solidHeader=TRUE,
            collapsible=TRUE,
            shiny::tableOutput("text_best_tour")
        ),
        shinydashboard::box(
            title="Status",
            solidHeader=TRUE,
            collapsible=TRUE,
            shiny::textOutput("text_gen_info")
        )
    )
)

ui <- shinydashboard::dashboardPage(header,sidebar,body)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# D. SERVER ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output) {
    is_local_min <- FALSE
    select_map_lu <- c("Djibouti38"="dj38.tsp",
                       "Berlin52"="berlin52.tsp",
                       "Qatar194"="qa194.tsp",
                       "Luxembourg980"="lu980.tsp",
                       "Rwanda1621"="rw1621.tsp",
                       "Nicaragua3496"="nu3496.tsp",
                       "Argentinia9152"="ar9152.tsp")
    start <- Sys.time()
    shiny::observeEvent(input$start_evolution,{
        map_later(max_iter=input$set_max_generations,function(gen_counter){
            if (!is_local_min){
                if (gen_counter==1){
                    fname <- file.path("data",input$select_map)
                    genes <<- init_genes(fname=fname)
                    population <<- genes %>%
                        init_individuals() %>%
                        score_individuals(genes=genes,
                                          do_par=FALSE)
                    best_individuals <<- population %>%
                        keep_best()
                    local_min_counter <<- 1
                } else {
                    children <- population %>%
                        select_parents(elite_frac=input$set_elite_frac) %>%
                        start_crossover(crossover_prob=input$set_crossover_prob,
                                        children_n=nrow(population)) %>%
                        start_mutation(mutation_prob=input$set_mutation_prob)
                    population <<- children %>%
                        score_individuals(genes=genes,
                                          do_par=FALSE)
                    best_individuals <<- population %>%
                        keep_best(best_individuals)
                }
                
                # Info Text
                info <- glue::glue(
                    "Generation {generation} - \\
                    Score: {score} - \\
                    Local-Minium-Counter: {local_min_counter} - \\
                    Duration: {duration}",
                    generation=best_individuals$GENERATION[1],
                    score=best_individuals$SCORE[1] %>%
                        format(big.mark='.', decimal.mark=',', nsmal=2),
                    duration=Fmt(Sys.time()-start))
                output$text_gen_info <- shiny::renderText(info)
                
                # Plot Tour und Tour Genes
                if (gen_counter==1){
                    is_new_tour <- TRUE
                } else {
                    is_new_tour <- best_individuals$CHROMOSOME[c(1,2)] %>%
                        unique() %>%
                        length() %>%
                        magrittr::is_greater_than(1)
                }
                if (is_new_tour){
                    # Plot Tour
                    output$plot_tsp <- shiny::renderPlot({
                        plot_genes(genes=genes,
                                   chromosome=best_individuals$CHROMOSOME[[1]],
                                   title=glue::glue("Genetic Algorithm: {tsp_name}",
                                                    tsp_name=names(which(select_map_lu == input$select_map))),
                                   subtitle=glue::glue("Generation: {generation} - \\
                                               Score: {score}",
                                                       generation=best_individuals$GENERATION[1],
                                                       score=best_individuals$SCORE[1] %>%
                                                           format(big.mark=".",
                                                                  decimal.mark=",",
                                                                  nsmall=2)))}) 
                    # Output Best Tour Genes
                    output$text_best_tour <- shiny::renderText({
                        glue::glue("{genes}",
                                   genes=best_individuals$CHROMOSOME[[1]] %>%
                                       paste(collapse="-"))
                    })
                    local_min_counter <<- 1
                } else {
                    local_min_counter <<- local_min_counter+1
                    if (local_min_counter >= input$set_local_minimum_thld){
                        is_local_min <<- TRUE
                        # Output Best Tour Genes
                        library(crayon)
                        output$text_best_tour <- shiny::renderText({
                            glue::glue_col("Lösung gefunden \n {genes}",
                                       genes=best_individuals$CHROMOSOME[[1]] %>%
                                           paste(collapse="-"))
                        })
                    }
                }
                
                # Output Convergence
                output$plot_score <- shiny::renderPlot({
                    plot_convergence(best_individuals,
                                     title=glue::glue("Genetic Algorithm: {tsp_name}",
                                                      tsp_name=names(which(select_map_lu == input$select_map))),
                                     subtitle=glue::glue("Generation: {generation} - \\
                                               Score: {score}",
                                                         generation=best_individuals$GENERATION[1],
                                                         score=best_individuals$SCORE[1] %>%
                                                             format(big.mark=".",
                                                                    decimal.mark=",",
                                                                    nsmall=2)))})
                
            }
        })
        is_local_min <<- FALSE
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
