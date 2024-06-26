#' Table of diagnostics
#'
#' Adds a table of diagnostics to the plot
#'
#' @inheritParams geom_sa
#' @param diagnostics vector of character containing the name of the diagnostics to plot.
#'    See [rjd3x13::userdefined_variables_x13()] or [rjd3tramoseats::userdefined_variables_tramoseats()] for the available
#'    parameters.
#' @param digits integer indicating the number of decimal places to be used for numeric diagnostics. By default `digits = 2`.
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster.
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster.
#' @param table_theme list of theme parameters for the table of diagnostics (see [ttheme_default()][gridExtra::ttheme_default()]).
#'
#'
#' @examples
#' p_sa_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
#'     geom_line() +
#'     labs(title = "Seasonal adjustment of the French industrial production index",
#'          x = "time", y = NULL) +
#'     geom_sa(color = "red", message = FALSE)
#'
#' # To add of diagnostics with result of the X-11 combined test and the p-values
#' # of the residual seasonality qs and f tests:
#' diagnostics <- c("diagnostics.seas-sa-combined", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-f")
#' p_sa_ipi_fr +
#'     geom_diagnostics(diagnostics = diagnostics,
#'                      ymin = 58, ymax = 72, xmin = 2010,
#'                      table_theme = gridExtra::ttheme_default(base_size = 8),
#'                      message = FALSE)
#'
#' # To customize the names of the diagnostics in the plot:

#' diagnostics <- c(`Combined test` = "diagnostics.seas-sa-combined",
#'                  `Residual qs-test (p-value)` = "diagnostics.seas-sa-qs",
#'                  `Residual f-test (p-value)` = "diagnostics.seas-sa-f")
#' p_sa_ipi_fr +
#'     geom_diagnostics(diagnostics = diagnostics,
#'                      ymin = 58, ymax = 72, xmin = 2010,
#'                      table_theme = gridExtra::ttheme_default(base_size = 8),
#'                      message = FALSE)
#'
#' # To add the table below the plot:
#'
#' p_diag <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
#'     geom_diagnostics(diagnostics = diagnostics,
#'                      table_theme = gridExtra::ttheme_default(base_size = 8),
#'                      message = FALSE) +
#'     theme_void()
#'
#' gridExtra::grid.arrange(p_sa_ipi_fr, p_diag,
#'                         nrow = 2, heights  = c(4, 1))
#'
#' @importFrom gridExtra tableGrob ttheme_default
#' @export
geom_diagnostics <- function(mapping = NULL, data = NULL,
                             position = "identity", ...,
                             method = c("x13", "tramoseats"),
                             spec = NULL,
                             frequency = NULL,
                             message = TRUE,
                             diagnostics = NULL,
                             digits = 2,
                             xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                             table_theme = ttheme_default(),
                             inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = StatDiagnostics,
                   geom = GeomDiagnostics,
                   position = position, inherit.aes = inherit.aes,
                   params = list(method = method, spec = spec,
                                 frequency = frequency, message = message,
                                 digits = digits, diagnostics = diagnostics,
                                 xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                 table_theme = table_theme,
                                 new_data = !missing(data) || !is.null(data),
                                 ...))
}
# Code largely inspired by GeomCustomAnn of ggplot2
GeomDiagnostics <- ggproto("GeomDiagnostics", Geom,
                         extra_params = "",
                         handle_na = function(data, params) {
                             data
                         },
                         draw_panel = function(data, panel_params, coord,
                                               xmin = -Inf, xmax = Inf,
                                               ymin = -Inf, ymax = Inf,
                                               table_theme = ttheme_default()) {
                             if (is.null(data))
                                 return(NULL)
                             if (!inherits(coord, "CoordCartesian")) {
                                 stop("geom_diagnostics only works with Cartesian coordinates",
                                      call. = FALSE)
                             }
                             corners <- data.frame(x = c(xmin, xmax),
                                                   y = c(ymin, ymax))
                             datatemp <- coord$transform(corners, panel_params)
                             x_rng <- range(datatemp$x, na.rm = TRUE)
                             y_rng <- range(datatemp$y, na.rm = TRUE)
                             vp <- grid::viewport(x = mean(x_rng), y = mean(y_rng),
                                            width = diff(x_rng), height = diff(y_rng),
                                            just = c("center","center"))

                             ## computation data

                             grob <- gridExtra::tableGrob(data[, c("Diagnostic", "Value")],
                                                          theme = table_theme,
                                                          rows = NULL)
                             ##
                             grid::editGrob(grob, vp = vp)
                         },
                         default_aes = aes_(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, x = NULL, y = NULL)
)

StatDiagnostics <- ggproto("StatDiagnostics", Stat,
                       required_aes = c("x", "y"),
                       compute_group = function(data, scales,
                                                method = c("x13", "tramoseats"),
                                                spec = NULL,
                                                frequency = NULL,
                                                message = TRUE,
                                                diagnostics = NULL,
                                                digits = 2,
                                                first_date = NULL,
                                                last_date = NULL,
                                                new_data = TRUE){
                           if (is.null(diagnostics))
                               return(NULL)
                           result <- seasonal_adjustment(data = data,
                                                         method = method,
                                                         spec = spec,
                                                         frequency = frequency,
                                                         message = message,
                                                         new_data = new_data)
                           data <- result[["data"]]
                           sa <- result[["sa"]]
                           frequency <- result[["frequency"]]

                           if (is.null(names(diagnostics))) {
                               names(diagnostics) <- diagnostics
                           } else {
                               names_supplied <- names(diagnostics) != ""
                               names(diagnostics)[names_supplied] <- diagnostics[names_supplied]
                           }

                           diag_table <- rjd3toolkit::user_defined(sa, diagnostics)
                           diag_table <- lapply(diag_table, function(x){
                               if (is.null(x) || is.ts(x))
                                   return(NULL)
                               if (length(x) > 1) {
                                   if (is.list(x)) {
                                       x <- x[[2]]
                                   } else {
                                       x <- x[2]
                                   }
                               }
                               if (is.numeric(x))
                                   x <- round(x, digits)
                               x
                           })
                           diag_table <- unlist(diag_table) #do.call(c, diag_table)

                           diag_names <- diagnostics[names(diagnostics) %in% names(diag_table)]

                           diag_table <- data.frame(Diagnostic =
                                                        diag_names,
                                                    Value = diag_table)
                           diag_table
                       }
)
