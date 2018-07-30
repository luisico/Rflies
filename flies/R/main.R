#' Main process function
#'
#' @param infile Path to the input file
#' @param worksheet Worksheet to parse from Excel
#' @param experiment_finished Count unaccounted flies in events
#' @return A list with tally, events and fit objects
#' @export
process = function(inputfile, worksheet = 'Data', experiment_finished = 'auto') {
    ## Import and transform data
    input = readxl::read_excel(inputfile, sheet = worksheet)
    data = transform(input)

    ## Guess if the experiment is finished
    ## Any value other than 'yes/no' maps to 'auto'
    if (experiment_finished == 'yes'){
        experiment_finished = TRUE
    } else if (experiment_finished == 'no') {
        experiment_finished = FALSE
    } else {
        ## Experiment is finished if any cell representing flies in the worksheet has
        ## been marked with a colon (:)
        experiment_finished = !all(is.na(data$marked))
    }

    ## Create datasets
    initial = get_initial(data)
    final =   get_final(data)
    journal = get_journal(data)

    ## Create tally and events
    tally = tally(initial, final, journal)
    events = generate_events(journal, tally, experiment_finished)

    ## The total number of events should be:
    ##expected_events = tally %>% dplyr::mutate(events = (dead + escaped + unaccounted)) %>% dplyr::group_by() %>% dplyr::summarise_if(is.numeric, sum) %>% dplyr::select(events)

    ## Export to prism
    prismfile = paste(paste(tools::file_path_sans_ext(inputfile), worksheet, "prism", sep="_"), ".csv", sep="")
    export_prism(events, prismfile)

    ## Generate survfit
    fit = generate_fit(events)

    ## Return a list with the tally and the events datasets
    list(tally = tally,
         events = events,
         fit = fit,
         experiment_finished = experiment_finished)
}
