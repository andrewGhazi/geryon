#' See the memory usage of objects in your workspace
#'
#' @description It's easy to see the memory usage of a single object with
#' \code{lobstr::obj_size()} but doing that in a sorted, pretty way for everything in
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

  set_obj_class = function(x){
    class(x) = "object_size"
    x
  }

  tibble(obj = ls(name = .GlobalEnv)) %>%
    mutate(obj_size = map(.data$obj, \(.x) {lobstr::obj_size(get(.x))})) %>%
    mutate(size_string = map_chr(.data$obj_size,
                                 \(.x) {format(set_obj_class(.x), units = 'auto')}),
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
#' @details
#' use negative nr or nc to select bottom or right corners, respectively.
#'
#' @param x the input data frame or matrix
#' @param nr number of rows to pull out
#' @param nc number of columns to pull out
#' @export
corner = function(x, nr = 5, nc = 5) {

  nrx = nrow(x)
  ncx = ncol(x)

  nr_s = min(nrx, abs(nr))
  nc_s = min(ncx, abs(nc))

  if (nr < 0) {
    row_range = (nrx - nr_s + 1):nrx
  } else {
    row_range = 1:nr_s
  }

  if (nc < 0) {
    col_range = (ncx - nc_s + 1):ncx
  } else {
    col_range = 1:nc_s
  }

  x[row_range, col_range]
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

#' @export
broad_pal = function(type = "seq"){
  function(n){
    if (n>7) stop("The Broad palette only has 7 colors")
    broad_colors[1:n]
  }
}

#' @export
scale_color_broad = function(..., type = "seq", aesthetics = "color") {
  discrete_scale(aesthetics, "broad", broad_pal(type), ...)
}

#' @export
scale_fill_broad = function(..., type = "seq", aesthetics = "fill") {
  discrete_scale(aesthetics, "broad", broad_pal(type), ...)
}

#' Assign the default argument values of a function in the global environment
#'
#' @param f a function
#' @export
args2ge = function(f) {

  forms = formals(f)

  for (i in seq_along(forms)) {
    if (class(forms[[i]]) == "name") next

    if (class(forms[[i]]) == "call") {
      assign(x     = names(forms)[i],
             value = eval(forms[[i]]),
             envir = globalenv())
      next
    }

    assign(x     = names(forms)[i],
           value = forms[[i]],
           envir = globalenv())
  }

  return(invisible(NULL))
}

#' Set a timer.
#'
#' @description
#' "wilhelm" is the best sound, you know it.
#'
#' @param m time in minutes
#' @param sound beepr::beep() sound number
#' @examples
#' timer(2/60)
#' @export
timer = function(m = 5, sound = 9) {

  spinna = cli::make_spinner(which = "dots4",
                             template = paste0("{spin} Timer set for ",
                                               round(m, digits = 2),
                                               " minutes."))

  lapply(1:(10*60*m), \(x) {spinna$spin(); Sys.sleep(.1)})

  spinna$finish
  cat("\n")

  beepr::beep(sound = sound)
}

#' Find duplicates
#'
#' @description Filter to rows where the specified variable has at least one dulicate.
#' Useful for identifying problematic cases when 1:1 joins come out at 1:many.
#'
#' @param data a data frame
#' @param var a variable in that data frame
#' @param order_by if TRUE, order the result by the specified variable
#' @examples
#' mtcars |> find_dups(mpg)
#' @export
find_dups = function(data, var, order_by = TRUE) {
  res = data |>
    dplyr::filter({{var}} %in% {{var}}[duplicated({{var}})])

  if (order_by) {
    res = res |> dplyr::arrange({{var}})
  }

  res
}

#' @export
insert_img_link = function() {
  cur_source = rstudioapi::getSourceEditorContext()

  img_path = cur_source$path |>
    dirname() |>
    fs::path(img_dir); img_path

  if (!dir.exists(img_path)) cli::cli_abort("There's no images/ directory in the directory of the current source document!")

  img_file = fs::path(img_path,
                      format(Sys.time(), "%Y-%m-%e_%H:%M:%S"),
                      ext = "png")

  # TODO: check xclip is installed
  # system2("which", "xclip")
  system2("xclip",
          args = c("-selection", "clipboard", "-t", "image/png",
                   "-o", ">", img_file))

  rel_path = img_file |> fs::path_split() |> getElement(1) |> tail(2) |> fs::path_join()
  rstudioapi::insertText(text = paste0("![](", rel_path, ")"),
                         location = cur_source$selection[[1]]$range,
                         id = cur_source$id)
}
