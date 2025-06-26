get_pkg_graph = function(pkg, dep_type) {

  pak_res = pak::pkg_deps(pkg)

  # V This prints an empty message...
  nested_pkg_list = pak_res |>
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

  return(list(pak_res, gr))
}

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

  prgc = get_pkg_graph(pkg, dep_type)

  pak_res = prgc[[1]]

  gr = prgc[[2]]

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

#' Count unique dependencies
#' @description A function for package developers to count which direct
#'   dependencies add the most unique dependencies (direct plus downstream) in
#'   total.
#'
#' @param pkg a package to check
#' @param order Consider combinations of this many packages. Be careful going
#'   beyond 3.
#' @details Use this function to identify which packages have the most unique
#'   downstream dependencies. Set the order argument to check which pairs,
#'   triplets, etc have the most unique dependencies.
#' @returns A data.table listing the packages and the number of unique
#'   dependencies.
#' @export
uniq_pkg_deps = function(pkg, order = 1) {

  rlang::check_installed(c("pak", "igraph"))

  prgc = get_pkg_graph(pkg, c("depends", "imports", "linkingto"))

  pak_res = prgc[[1]]

  gr = prgc[[2]]

  pkg = pak_res |> sbt(direct) |> getElement("package")
  # ^ In case the user specified a directory "." or something

  dir_deps = pak_res |>
    sbt(direct) |>
    pull1(deps) |>
    sbt(ref != "R" &
          !grepl("[Ss]uggests|[Ee]nhances", type) &
          !fduplicated(package))

  if (nrow(dir_deps) == 0) {
    cli::cli_alert_success("No dependencies")
    return(invisible(NULL))
  }

  all_deps = pak_res$deps |>
    rowbind() |>
    mtt(type = tolower(type)) |>
    sbt(type %in% c("depends", "imports", "linkingto") & package != "R") |>
    getElement("package") |>
    funique()

  pkg_combn = combn(dir_deps$package, order)

  n_dir = nrow(dir_deps) # number of direct dependencies
  n_tot = length(all_deps)
  n_com = ncol(pkg_combn)

  dep_mat = matrix(0,
                   ncol = n_dir + 1,
                   nrow = n_tot,
                   dimnames = list(all_deps,
                                   c(dir_deps$package, pkg)))


  for (i in seq_len(n_dir)) {
    # dep_deps = dep_deps |>
    #   pull1(deps) |>
    #   sbt(type %in% c("depends", "imports", "linkingto") & package != "R") |>
    #   get_elem("package")
    # ^ This doesn't work. pak doesn't list indirect dependencies in `package`.
    # Need to form the graph then look up children from there.

    all_ds = igraph::neighborhood(gr,
                                  order = n_tot,
                                  nodes = dir_deps$package[i],
                                  mode = "out",
                                  mindist = 1) |>
      lapply(names) |>
      unlist() |>
      funique()

    iv = all_deps %in% all_ds

    dep_mat[iv,i] = 1
  }
  dep_mat[,n_dir+1] = 1

  n_uniq = vector("numeric", n_com)
  udv = vector("list", n_com)

  for (i in seq_len(n_com)) {

    compl = dep_mat[,-which(dir_deps$package %in% pkg_combn[,i]),
                    drop=FALSE]

    od = rowSums(compl) == 1

    ud = od & !(names(od) %in% dir_deps$package)

    n_uniq[i] = sum(ud)
    udv[[i]] = names(which(ud))
  }

  pkg_combn |>
    t() |>
    qDT() |>
    setColnames(paste0("pkg", 1:order)) |>
    mtt(n_uniq_deps = n_uniq + order,
        uniq_deps = udv) |> # add order because things depend on themselves at least
    roworder(-n_uniq_deps)

}
