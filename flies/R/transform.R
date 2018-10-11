#' Transform data (reshape and clean)
#'
#' - Reshape the entry column into rows (removing blanks).
#' - Identify final number of flies marked with ":".
#' - Split dead and escaped events marked with "+".
#' - Set correct formats for group, tube, dead and escaped columns,
#' - Calculating days or hours passed per group/tube.
#'
#' TODO:
#' - Remove flies0
#' - Manage initial dates marked with ":"
#'
#' @importFrom magrittr %>%
#' @param data Raw data from excel file
#' @return Transform data
transform = function(data) {
    data %>%
        tidyr::gather(entry, flies0, -tube, -group, na.rm=T) %>%
        tidyr::separate(flies0, c("marked", "flies"), ":", remove=T, fill="left") %>%
        tidyr::separate(flies, c("dead", "escaped"), "[+]", fill="right") %>%
        dplyr::mutate(group = as.factor(group),
               tube = as.factor(tube),
               dead = as.numeric(dead),
               escaped = as.numeric(escaped),
               timeseries = entry_to_timeseries(entry)) %>%
        dplyr::group_by(group, tube)
}

#' Generate dataset of initial flies per group/tube
#'
#' @importFrom magrittr %>%
#' @param data Transformed data
#' @return Dataset with initial flies
get_initial =  function(data) {
    data %>%
        dplyr::filter(timeseries == 0) %>%
        dplyr::select(-marked, -escaped) %>%
        dplyr::rename(initial = dead)
}

#' Generate dataset of final flies per group/tube
#'
#' @importFrom magrittr %>%
#' @param data Transformed data
#' @return Dataset with final flies
get_final = function(data) {
    data %>%
        dplyr::filter(!is.na(marked)) %>%
        dplyr::select(-marked, -escaped) %>%
        dplyr::rename(final = dead)
}


#' Generate journal of flies per group/tube
#'
#' @importFrom magrittr %>%
#' @param data Transformed data
#' @return Dataset with journal of flies
get_journal = function(data) {
    data %>%
        dplyr::filter(timeseries != 0) %>%
        dplyr::filter(is.na(marked)) %>%
        dplyr::select(-marked)
}

#' Create summary tally
#'
#' By group/tube for calculating unaccounted flies per tube.
#' By group for inspection.
#'
#' @importFrom magrittr %>%
#' @param initial Dataset with initial flies
#' @param final Dataset with final flies
#' @param journal Dataset with journal of flies
#' @return Tally dataset
tally = function(initial, final, journal) {
    tally = initial %>% dplyr::select(-entry, -timeseries) %>%
        dplyr::full_join(
            journal %>% dplyr::summarise(dead = sum(dead, na.rm=T), escaped = sum(escaped, na.rm=T)),
            by = c("group", "tube")
        ) %>%
        dplyr::full_join(
            final %>% dplyr::select(-entry, -timeseries),
            by = c("group", "tube")
        ) %>%
        dplyr::mutate_all(~ dplyr::if_else(is.na(as.numeric(.x)), 0, as.numeric(.x))) %>%
        dplyr::mutate(unaccounted = abs((final + dead + escaped) - initial))

    dplyr::bind_rows(
        tally %>% dplyr::group_by(group) %>% dplyr::summarise_if(is.numeric, sum),
        tally %>% dplyr::group_by() %>% dplyr::summarise_if(is.numeric, sum)
    )
}

#' Construct events dataset for use in survival analysis packages
#'
#' Expand rows with dead and escaped (censored) events.
#' Add censored events for flies unaccounted for.
#'
#' @param journal Dataset with journal of flies
#' @param tally Tally dataset
#' @param experiment_finished Add unaccounted flies to events for finished experiments
#' @return Events dataset
generate_events = function(journal, tally, experiment_finished) {
    events = dplyr::bind_rows(
        journal %>%                                       # dead flies: 1
        dplyr::select(-escaped) %>%
        dplyr::filter(dead > 0) %>%
        tidyr::uncount(dead, .id="fly") %>%
        dplyr::mutate(event=as.integer(1)),

        journal %>%                                       # escape flies: 0
        dplyr::select(-dead) %>%
        dplyr::filter(!is.na(escaped)) %>%
        tidyr::uncount(escaped, .id="fly") %>%
        dplyr::mutate(event=as.integer(0)),

        tally %>%                                         # unaccounted flies: 0  (-1 if experiment_finished is TRUE)
        dplyr::select(group, unaccounted) %>%
        dplyr::filter(unaccounted > 0) %>%
        tidyr::uncount(unaccounted, .id="fly") %>%
        dplyr::mutate(timeseries=max(journal$timeseries), event=ifelse(experiment_finished, -1, 0), entry=as.Date(NA))
    )

    events %>% dplyr::filter(event == 0 | event == 1)
}
