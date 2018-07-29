#' Main process function
#'
#' @param infile Path to the input file
#' @param worksheet Worksheet to parse from Excel
#' @param skip_unaccounted Do not count unaccounted flies in events
#' @return A list with tally, events and fit objects
#' @export
process = function(inputfile, worksheet = 'Data', skip_unaccounted = FALSE) {
    ## Import and transform data
    input = readxl::read_excel(inputfile, sheet = worksheet)
    data = transform(input)

    ## Create datasets
    initial = get_initial(data)
    final =   get_final(data)
    journal = get_journal(data)

    ## Create tally and events
    tally = tally(initial, final, journal)
    events = generate_events(journal, tally, skip_unaccounted)

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
         fit = fit)
}
