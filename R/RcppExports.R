# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

.correlation_event <- function(x, y) {
    .Call(`_eseis_correlation_event`, x, y)
}

.kurtosis_event <- function(x, k) {
    .Call(`_eseis_kurtosis_event`, x, k)
}

.run_cor <- function(x, y, k) {
    .Call(`_eseis_run_cor`, x, y, k)
}

.stalta_event_freeze <- function(event_length, data_sta, data_lta, on, off) {
    .Call(`_eseis_stalta_event_freeze`, event_length, data_sta, data_lta, on, off)
}

.stalta_event_nofreeze <- function(event_length, ratio, on, off) {
    .Call(`_eseis_stalta_event_nofreeze`, event_length, ratio, on, off)
}

