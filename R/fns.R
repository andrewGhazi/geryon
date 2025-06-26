#' See the memory usage of objects in your workspace
#'
#' @description It's easy to see the memory usage of a single object with
#' \code{lobstr::obj_size()} but doing that in a sorted, pretty way for everything in
#' the workspace is a bit more involved. This function has taken care of that.
#'
#' @return A data.table of objects in the global environment and columns stating
#'   how much memory the objects are using (both with pretty units and the raw
#'   integer number of bytes).
#' @import collapse
#' @import rlang
#' @export
ws_size = function(){
  # Use to get a data frame of objects in the global environment and columns
  # stating how much memory the objects are using (both with pretty units and
  # the raw integer number of bytes).

  rlang::check_installed("lobstr")

  data.table::data.table(obj = ls(name = .GlobalEnv)) |>
    mtt(obj_size = fs::fs_bytes(sapply(obj, .get_obj_size))) |>
    roworder(-obj_size)
}

.get_obj_size = function(.x) {
  get(.x, envir = .GlobalEnv) |>
    lobstr::obj_size()
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
  # Use to pull out the first element of a list column in a data frame
  # Optionally you can instead pull the one_to_pull-th element

  .data[[deparse(substitute(my_var))]][[one_to_pull]]
}

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

  rlang::check_installed("ggplot2")

  base_family = ""
  base_line_size = base_size/22
  base_rect_size = base_size/22

  t1 = ggplot2::theme_light(base_size = base_size,
                            base_family = base_family,
                            base_line_size = base_line_size,
                            base_rect_size = base_rect_size)

  t2 = ggplot2::theme(strip.text = ggplot2::element_text(color = 'grey10'),
                      ...)

  ggplot2::`%+replace%`(t1, t2)

}

#' Show the top-left corner
#'
#' This function pulls out the top-left corner of a data frame or matrix. This
#' is useful if the input is very tall and/or wide, such that printing the
#' entire object fills up the entire console, even with the nice printing
#' features of e.g. tibbles.
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

#' Filter to rows matching a pattern
#' @param x input data frame
#' @param pattern pattern to search for
#' @param col_name column to search (unquoted)
#' @export
fpat = function(x, pattern, col_name) {

  v = x[[deparse(substitute(col_name))]]

  x |> sbt(data.table::like(v,
                            pattern))
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
  rlang::check_installed("ggplot2")
  ggplot2::discrete_scale(aesthetics, "broad", broad_pal(type), ...)
}

#' @export
scale_fill_broad = function(..., type = "seq", aesthetics = "fill") {
  rlang::check_installed("ggplot2")
  ggplot2::discrete_scale(aesthetics, "broad", broad_pal(type), ...)
}

#' Assign the default argument values of a function in the global environment
#'
#' @param f a function
#' @export
args2ge = function(f) {

  forms = formals(f)

  for (i in seq_along(forms)) {
    if (inherits(forms[[i]], "name")) next

    if (inherits(forms[[i]], "call")) {
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

  rlang::check_installed("beepr")

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
#' @param var a variable in that data frame (unquoted)
#' @param order_by if TRUE, order the result by the specified variable
#' @examples
#' mtcars |> find_dups(mpg)
#' @seealso [has_dups()]
#' @export
find_dups = function(data, var, order_by = TRUE) {

  vn = deparse(substitute(var))
  v = data[[vn]]

  res = data |>
    sbt(has_dups(v))

  if (order_by) {
    res = res |> roworderv(vn)
  }

  res
}

#' Has duplicates
#'
#' @description
#' Return TRUE on all elements of a vector that has any duplicates (i.e. including the first occurrence)
#'
#' @param x a vector
#' @returns a logical vector the same length as the input
#' @seealso [find_dups()]
#' @seealso [vctrs::vec_duplicate_detect()] (will be faster for large x)
#' @export
has_dups = function(x) {
  x %in% x[duplicated(x)]
}

#' Insert an image link to an image on your clipboard
#'
#' @description This function let's you "paste" an image from your clipboard
#'   into your Quarto document. It's available as an RStudio addin so you can
#'   bind it to a keyboard shortcut. I use ctrl+shift+H.
#' @details This function requires xclip to be installed on the user's system
#'   (non-Linux users, you're on your own). It copies the image from the
#'   clipboard to the specified image directory into a time-stamped file, then
#'   inserts the link to the image in the current source pane document at the
#'   cursor. It also includes an empty alternate text tag to encourage authors
#'   to make their work more accessible.
#'
#' @param img_dir directory where the image will be pasted (relative to the
#'   working directory of the current document in the source panel).
#'
#' @export
insert_img_link = function(img_dir = "images/") {
  cur_source = rstudioapi::getSourceEditorContext()

  img_path = cur_source$path |>
    dirname() |>
    fs::path(img_dir); img_path

  if (!dir.exists(img_path)) cli::cli_abort("There's no images/ directory in the directory of the current source document!")

  img_file = fs::path(img_path,
                      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                      ext = "png")

  # TODO: check xclip is installed
  # system2("which", "xclip")
  system2("xclip",
          args = c("-selection", "clipboard", "-t", "image/png",
                   "-o", ">", img_file))

  rel_path <- img_file |>
    fs::path_split() |>
    getElement(1) |>
    tail(2) |>
    fs::path_join()

  rstudioapi::insertText(text = paste0("![](", rel_path, "){fig-alt=\"\" style=\"filter: drop-shadow(0 0 0.75rem grey);\"}"),
                         location = cur_source$selection[[1]]$range,
                         id = cur_source$id)
}

grid_pts = function(dat, bw=.01) {


  sq_dat = dat |>
    slt(x, y) |>
    mtt(y = as.numeric(y) + 1) |>
    gby(x, y) |>
    mtt(sq_n = length(x),
        side_n = ceiling(sqrt(sq_n)),
        gi = 1:length(x),
        x = x - bw*(side_n-1)/2 + bw*((gi-1) %% side_n),
        y = y - bw*(side_n-1)/2 + 2*bw*floor((gi-1)/side_n)) |>
    fungroup()

  sq_dat |>
    ggplot(aes(x,y)) +
    geom_point(size = .5)
}

