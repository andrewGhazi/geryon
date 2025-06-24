#' Plot dependency graph
#' @description Plot a package's dependency graph, coloring each node by the
#'   number of packages it depends on.
#' @param pkg package string passed to \code{\link[pak:pkg_deps]{pak::pkg_dep}}
#' @param dep_type type of dependencies to look up. Valid values are
#'   \code{c("depends", "imports", "suggests")}
#' @details If you include \code{"suggests"} among the dependency types to look
#'   up, be aware that suggests can be circular / cyclic. If this is detected,
#'   the node coloring will be turned off.
#'
#' @returns a ggplot
#' @examples
#' # Using pkgcache in examples is not allowed, uncomment to run these interactively:
#' # plot_deps_graph("ggplot2") # ggplot2 has many downstream dependencies
#' # plot_deps_graph("rlang") # rlang has only one
#'
#' @export
plot_deps_graph = function(pkg,
                           dep_type = c("depends", "imports", "linkingto")) {

  req_pkgs = c("ggplot2", "igraph", "ggraph", "grid", "pals", "pak")
  rlang::check_installed(req_pkgs)

  dep_type = tolower(dep_type)

  rlang::arg_match(dep_type,
                   values = c("depends", "imports", "suggests", "linkingto"),
                   multiple = TRUE)

  # TODO check if it's empty

  nested_pkg_list = pak::pkg_deps(pkg) |>
    slt(package, deps) |>
    frename(from = package)

  names(nested_pkg_list$deps) = nested_pkg_list$from

  unnested = rowbind(nested_pkg_list$deps,
                     idcol = "from") |>
    frename(to = "package") |>
    qDT() |>
    mtt(from = as.character(from))

  edge_list = unnested |>
    sbt(tolower(type) %in% dep_type) |>
    slt(from:to) |>
    funique() |>
    sbt(to != "R")

  edge_vec = mapply(\(x,y) c(x,y),
                    edge_list$from, edge_list$to) |>
    unlist()

  if (length(edge_vec) == 0) {
    gr = igraph::make_empty_graph(1) |>
      igraph::set_vertex_attr("label", value = pkg) |>
      igraph::set_vertex_attr("name", value = pkg)
  } else {
    gr = igraph::make_directed_graph(edge_vec)
  }

  ec = igraph::ecount(gr)

  if (ec > 0 ) {
    edges_geom = ggraph::geom_edge_link(arrow = grid::arrow(length = grid::unit(1.5, "mm"),
                                                            type = "closed"),
                                        ggplot2::aes(end_cap = ggraph::label_rect(node2.name)),
                                        color = "#222222")
  } else {
    edges_geom = NULL
  }

  if ("suggests" %in% dep_type && !igraph::is_acyclic(gr)) {
    cli::cli_alert_warning("Cycle detected among suggested packages. Can't color nodes by number of dependencies.")
    fill_aes = ggplot2::aes(label = name)
    fill_scale = NULL
  } else {
    fill_scale = ggplot2::scale_fill_gradientn(colors = pals::parula(100)[12:97])
    fill_aes = if (ec == 0) {
      ggplot2::aes(label = pkg)
    } else {
      ggplot2::aes(label = name,
                   fill = igraph::neighborhood_size(gr,
                                                    mode = "out",
                                                    order = ec,
                                                    mindist = 1))
    }

  }

  gr |>
    ggraph::ggraph(ifelse(ec > 1, "stress", "tree")) +
    edges_geom +
    ggraph::geom_node_label(fill_aes) +
    fill_scale +
    ggplot2::theme_dark() +
    ggplot2::theme(axis.title        = ggplot2::element_blank(),
                   axis.text         = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   plot.background   = ggplot2::element_rect(fill = "#444444"),
                   legend.background = ggplot2::element_rect(fill = "#666666"),
                   legend.ticks      = ggplot2::element_line(colour = "#333333")) +
    ggplot2::labs(fill = "n_deps")
}
