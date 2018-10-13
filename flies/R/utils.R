#' Guess if entries are in dates or hours
#'
#' @param values List of time series values
#' @return string "days" or "hours"
guess_timeseries = function(values) {
    if(all(grepl("h", values))) {
        "hours"
    } else {
        if(any(grep("h", values))) {
            stop("Error: timeseries looks like a mix of dates and hours")
        } else {
            "days"
        }
    }
}

#' Correct format of entry to date or hours
#'
#' @param values List of entries
#' @return Formated entry in date or hours
entry_format = function(values) {
    format = guess_timeseries(values)
    if (format == "days") {
        values = entry_to_days(values)
    }
    values
}

#' Convert entry from excel to days or hours timeseries
#'
#' @param values List of entries
#' @return Integer timeseries
entry_to_timeseries = function(values) {
    format = guess_timeseries(values)
    timeseries = as.integer(do.call(paste("entry", "to", format, sep="_"), list(values)))
    timeseries - min(timeseries, na.rm=T)
}

#' Convert entry from excel to date
#'
#' @param values List of entries
#' @return Integer timeseries in days
entry_to_days = function(values) {
    values = janitor::excel_numeric_to_date(as.numeric(values))
}

#' Convert entry from excel to hours
#'
#' @param values List of entries
#' @return Integer timeseries in hours
entry_to_hours = function(values) {
    values = as.integer(sub("h$", "", values))
}
