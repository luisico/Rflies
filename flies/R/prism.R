#' importFrom magrittr %>%
#'
#' Export to Prism
#'
#' @param events Events dataset
#' @param outfile Path to the prims file
#' @return Events dataset
export_prism = function(events, outfile) {
    prism = events %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(id = 1:n()) %>%
        data.table::dcast(group+tube+days+fly+event+id ~ group, value.var="event", fill="") %>%
        dplyr::select(-group, -tube, -fly, -event, -id)

    write.csv(prism, outfile)
}
