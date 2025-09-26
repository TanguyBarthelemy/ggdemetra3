#' Initialise 'ggplot2' with 'SA' model
#' 
#' @param x A seasonal adjusted model.
#' @param context the context used to estimate the model.
#' @param ... Other parameters passes to [ggplot2::ggplot()]
#' @examples
#' mod <- rjd3x13::x13(window(rjd3toolkit::ABS$X0.2.20.10.M, start = 2000))
#' init_ggplot(mod) +
#'     geom_line(color =  "#F0B400") +
#'     geom_sa(component = "sa", color = "#155692")
#' @export
init_ggplot <- function(x, ...) {
    UseMethod("init_ggplot", x)
}
#' @export
init_ggplot.JD3_X13_OUTPUT<- function(x, context = NULL, ...) {
    spec <- x$result_spec
    y <- raw(x)
    d_y <- ts2dataframe(y)
    seasonal_adjustment(
        data = d_y,
        method = "x13",
        spec = spec,
        context = context,
        frequency = frequency(y),
        message = FALSE,
        new_data = TRUE)
    ggplot2::ggplot(data = d_y, ggplot2::aes(x = x, y = y), 
                    ...)
}
#' @export
init_ggplot.JD3_TRAMOSEATS_OUTPUT <- function(x, context = NULL, ...) {
    spec <- x$result_spec
    y <- raw(x)
    d_y <- ts2dataframe(y)
    seasonal_adjustment(
        data = d_y,
        method = "tramoseats",
        spec = spec,
        context = context,
        frequency = frequency(y),
        message = FALSE,
        new_data = TRUE)
    ggplot2::ggplot(data = d_y, ggplot2::aes(x = x, y = y), 
                    ...)
}
#' @export
init_ggplot.JD3_Object <- function(x, ...){
    y <- raw(x)
    d_y <- data <- ts2dataframe(y)
    frequency <- frequency(y)
    message <- FALSE
    spec <- NULL
    if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
        method <- "x13"
    } else  if (rJava::.jinstanceof(x$internal, "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults")) {
        method <- "tramoseats"
    }
    .demetra$data_ts <-
        dataframe2ts(data = data, frequency = frequency, message = message)
    
    .demetra$sa <- sa
    .demetra$spec <- spec
    .demetra$method <- method
    .demetra$data_y <- data$y
    ggplot2::ggplot(data = d_y, ggplot2::aes(x = x, y = y), 
                    ...)
}

