#' importFrom magrittr %>%
#'
#' Export to Prism
#'
#' @param events Events dataset
#' @param outfile Path to the prims file
#' @return Events dataset
#' @export
export_prism = function(events, format, outfile) {
    prism = events %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(id = 1:n()) %>%
        data.table::dcast(group+tube+timeseries+fly+event+id ~ group, value.var="event", fill="") %>%
        dplyr::select(-group, -tube, -fly, -event, -id)

    colnames(prism) = c(format, "A", "B")
    write.csv(prism, file=outfile)
}
