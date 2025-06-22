#' Get local 2D density
#'
#' @description Compute a 2D density estimate and return the estimate evaluated
#'   at each pair of points
#' @param x a vector of x-values
#' @param y a vector of y-values
#' @param group a group
#' @param ... arguments to pass to ks::kde for the density estimation

#' @details This is useful for computing color-density scatterplots.
#' @examples
#' overlapping = data.frame(x = rt(3e3, df = 3), y = rt(3e3, df = 3))
#' get_local_density(overlapping$x, overlapping$y)
#' @export
get_local_density = function(x, y, group = NULL, ...){
  rlang::check_installed("ks")

  if (!missing(group)){stop('Groupwise coloring is not supported yet')}

  ks::kde(cbind(x, y),
          eval.points = cbind(x, y),
          ...)$estimate
}

#' Add local density
#'
#' @description Add a column of local density to a data frame
#' @param .data input data frame
#' @param x unquoted column name of variable to use for the x-variable
#' @param y unquoted column name of variable to use for the y-variable
#' @param var name to use for the output column
#' @param sort_output if TRUE, return the output sorted by local density
#' @details The \code{sort_output} parameter sometimes helps the output plot
#'   look a little less speckled.
#' @export
add_local_density = function(.data, x, y, var = 'local_density', sort_output = TRUE){
  vn = (substitute(var))
  eq_x = .data[[deparse(substitute(x))]]
  eq_y = .data[[deparse(substitute(y))]]

  if (sort_output){

    nc = ncol(.data)

    mtt(.data, .eq_var = get_local_density(eq_x, eq_y)) |>
      frename(.eq_var = vn, .nse = FALSE) |>
      roworderv(cols = nc+1)
  } else{
    mtt(.data, .eq_var = get_local_density(eq_x, eq_y)) |>
      frename(.eq_var = vn, .nse = FALSE)
  }
}

#'
# color_density_scatterplot = function(.data, x, y, var = 'local_density'){
#   eq_x = enquo(x)
#   eq_y = enquo(y)
#   eq_var = enquo(var)
#
#   .data |>
#     add_local_density(!!eq_x, !!eq_y, var = var) |>
#     ggplot(aes(!!eq_x, !!eq_y)) +
#     geom_point(aes(color = !!eq_var))
#
# }
