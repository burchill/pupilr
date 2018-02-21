#' Left-join and overwrite values in "left" dataframe
#'
#' If you want to recalculate Z-scores, for example, without worrying about columns in the
#' "right" dataframe having the same names as columns in the left data frame
#' (which would cause their names to become something like "colname.x" and "colname.y")
#' this function replaces all the columns in the first data frame with the ones in the second.
#' The arguments are just like \code{\link[dplyr]{left_join}}, and builds off that code.
#'
#' @param x,y the "left" and "right" data frames, respectively
#' @param by the columns you want to join by would appear by default.
#' @param copy whatever this argument means in the regular \code{\link[dplyr]{left_join}}
#' @param \dots Optional arguments
#' @return a data frame or tibble or whatever, just like \code{\link[dplyr]{left_join}} but with columns overwritten
#'
#' @export
left_join_and_overwrite <- function(x, y, by = NULL, copy = FALSE, warnifnoprevious = FALSE, ...) {
  by <- dplyr::common_by(by,x,y)
  incoming_cols <- dplyr::setdiff(names(y),by$y)
  tryCatch({
    dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...)
  }, warning = function(war) {
    if (!(warnifnoprevious) &
        paste0(war)==paste0("simpleWarning in one_of(incoming_cols): Unknown variables: `",
                   paste0(incoming_cols, collapse = "`,`"),
                   "`\n")) {
      suppressWarnings(dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...))
    } else {
      dplyr::left_join(dplyr::select(x, -one_of(incoming_cols)), y, by, copy, ...)
    }

  })
  # dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...)
}
