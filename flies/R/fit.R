#' Generate survival fit
#'
#' @param events Events dataset
#' @return Events dataset
generate_fit = function(events) {
    survival::survfit(survival::Surv(days, event) ~ group, data=events)
}

#' Plot graph
#'
#' @param data Data object from the process function
#' @return ggsurvplot object
#' @export
plot = function(data) {
    survminer::ggsurvplot(data$fit, data=data$events, xlab="Days", palette="jco")
}
