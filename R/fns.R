#' @title  See the memory usage of objects in your workspace
#'
#' It's easy to see the memory usage of a single object with
#' \code{object.size()} but doing that in a sorted, pretty way for everything in
#' the workspace is a bit more involved. This function has taken care of that.
#'
#' @return A tibble of objects in the global environment and columns stating
#'   how much memory the objects are using (both with pretty units and the raw
#'   integer number of bytes).
#' @import dplyr
#' @import ggplot2
#' @import ks
#' @import tibble
#' @import purrr
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
ws_size = function(){
  # Use to get a data frame of objects in the global environment and columns
  # stating how much memory the objects are using (both with pretty units and
  # the raw integer number of bytes).

  tibble(obj = ls(name = .GlobalEnv)) %>%
    mutate(obj_size = map(.data$obj, ~object.size(get(.x)))) %>%
    mutate(size_string = map_chr(.data$obj_size, format, units = 'auto'),
           n_bytes = as.numeric(.data$obj_size)) %>%
    arrange(desc(.data$n_bytes)) %>%
    select(-.data$obj_size) %>%
    rename(obj_size = .data$size_string)
}

#' Pull out a single element from a (list-) variable
#'
#' \code{dplyr::pull} is a convenient way to pull out the values of a column
#' from a data frame, but if you've got a long list column and just want to
#' check one to make sure everything's working, printing the whole list can
#' clutter up the console. \code{pull1} just pulls out one.
#'
#' @param .data a data frame
#' @param my_var the unquoted name of the variable you want to pull out an element from
#' @param one_to_pull if you don't want to pull the first element, set this to the row number of the one you want
#' @return The \code{one_to_pull}-th element of the input variable
#' @export
pull1 = function(.data, my_var, one_to_pull = 1){
  # Use to pull out the first element of a list column in a tibble
  # Optionally you can instead pull the one_to_pull-th element

  eq_var = enquo(my_var)

  dplyr::pull(.data, !!eq_var)[[one_to_pull]]
}

# TODO fix this one day
# convert_vcf_genome = function(vcf_path, start_genome, goal_genome){
#   library(VariantAnnotation)
#   library(rtracklayer)
#   library(Biostrings)
#
#   vcf = VariantAnnotation::readVcf(vcf_path, genome = start_genome)
#   if(start_genome == 'hg19' & goal_genome == 'hg38'){
#     path = '/mnt/labhome/andrew/plateletMPRA/data/hg19ToHg38.over.chain'
#     ch = rtracklayer::import.chain(path)
#   } else if(start_genome == 'hg38' & goal_genome == 'hg19'){
#     path = '/mnt/labhome/andrew/plateletMPRA/data/hg38ToHg19.over.chain'
#     ch = rtracklayer::import.chain(path)
#   } else {
#     stop('Unable to use this pair of genomes')
#   }
#   seqlevelsStyle(vcf) = 'UCSC'
#
#   liftOver(rowRanges(vcf), ch) %>%
#     unlist %>%
#     as.data.frame %>%
#     rownames_to_column() %>%
#     as_tibble %>% # fucking shitty data formats
#     mutate(seqnames = gsub('chr', '', as.character(seqnames)),
#            ALT = ALT %>% map_chr(toString))
# }

#' A theme for presentations
#'
#' Ever seen oblivious researchers presenting slides with ggplot2's
#' characteristic tiny axis-titles that even Legolas's elf eyes couldn't see?
#' This theme makes text larger so that the output plots look nice on slides.
#' Otherwise it's mostly identical to \code{ggplot2::theme_gray()}.
#' @param base_size The baseline size of plot elements
#' @param ... arguments to pass to ggplot2::theme()
#' @export
theme_pres = function(base_size = 22, ...) {
  base_family = ""
  base_line_size = base_size/22
  base_rect_size = base_size/22

  theme_light(base_size = base_size,
              base_family = base_family,
              base_line_size = base_line_size,
              base_rect_size = base_rect_size) %+replace%
    theme(strip.text = element_text(color = 'grey10'),
          ...)
}

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
  eq_var = enquo(var)
  eq_x = enquo(x)
  eq_y = enquo(y)

  if (sort_output){
    mutate(.data, !!eq_var := get_local_density(!!eq_x, !!eq_y)) %>%
      arrange(!!sym(var))
  } else{
    mutate(.data, !!eq_var := get_local_density(!!eq_x, !!eq_y))
  }
}

#'
color_density_scatterplot = function(.data, x, y, var = 'local_density'){
  eq_x = enquo(x)
  eq_y = enquo(y)
  eq_var = enquo(var)

  .data %>%
    add_local_density(!!eq_x, !!eq_y, var = var) %>%
    ggplot(aes(!!eq_x, !!eq_y)) +
    geom_point(aes(color = !!eq_var))

}
