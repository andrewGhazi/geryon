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
#' @importFrom purrr map map_chr map_dfr
#' @import tidyr
#' @import rlang
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
#' Ever seen researchers presenting slides with ggplot2's characteristic tiny
#' axis-titles that even Legolas's elf eyes couldn't see? This theme makes text
#' larger so that the output plots look nice on slides. Otherwise it's mostly
#' identical to \code{ggplot2::theme_gray()}.
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

#' Show the top-left corner
#'
#' This function pulls out the top-left corner of a data frame or matrix. This
#' is useful if the input is very tall and/or wide, such that printing the
#' entire object fills up the entire console, even with the nice printing
#' features of tibbles.
#' @param x the input data frame or matrix
#' @param nrow number of rows to pull out
#' @param ncol number of columns to pull out
#' @export
corner = function(x, nrow = 5, ncol = 5){
  x[1:nrow, 1:ncol]
}

#' Show the classes of a data frame
#'
#' Return a data frame giving the class of each column of the input
#' @param x input data frame
#'
#' @export
show_classes = function(x) {
  x %>%
    map_dfr(function(.x) paste(class(.x[[1]]), collapse = ', ')) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = 'column',
                        values_to = 'class')
}

#' Filter to rows matching a pattern
#' @param x input data frame
#' @param pattern pattern to search for
#' @param col_name column to search
#' @export
fpat = function(x, pattern, col_name) {
  x %>%
    dplyr::filter(grepl(pattern, x = {{ col_name }}))
}
