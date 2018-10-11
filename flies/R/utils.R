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

#' Convert entry from excel to days or hours timeseries
#'
#' @param values List of entries
#' @return Integer timeseries
entry_to_timeseries = function(values) {
    format = guess_timeseries(values)
    as.integer(do.call(paste("timeseries", "to", format, sep="_"), list(values)))
}

#' Convert entry from excel to days timeseries
#'
#' @param values List of entries
#' @return Integer timeseries in days
timeseries_to_days = function(values) {
    values = janitor::excel_numeric_to_date(as.numeric(values))
    values - min(values, na.rm=T)
}

#' Convert entry from excel to hour timeseries
#'
#' @param values List of entries
#' @return Integer timeseries in hours
timeseries_to_hours = function(values) {
    values = as.integer(sub("h$", "", values))
    values - min(values, na.rm=T)
}
