#' rmse
#'
#' Utility function to compute Root Mean Squared Error (RMSE)
#'
#' @inheritParams se
#'
#' @export
rmse <- function(y, y_hat) sqrt(mse(y, y_hat))


#' se
#'
#' Utility function to compute pointwise squared error (SE)
#'
#' @param y A numeric vector representing the actual values.
#' @param y_hat A numeric vector representing the forecasted values.
#'
#' @return squared error of forecasted values.
#'
#' @export
se <- function(y, y_hat) {
  stopifnot(length(y) == length(y_hat),
            is.numeric(y),
            is.numeric(y_hat))

  (y - y_hat) ^ 2
}

#' mse
#'
#' Utility function to compute mean squared error (MSE)
#'
#' @inheritParams se
#'
#' @export
mse <- function(y, y_hat) mean(se(y, y_hat), na.rm = TRUE)
