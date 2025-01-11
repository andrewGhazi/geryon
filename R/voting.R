#' Ranked pairs voting
#'
#' @description
#' This function performs ranked pairs voting.
#'
#' @param vote_df a data frame with three columns: voter_id, candidate, and vote_rank
#' @param plot if TRUE, plot the pair graph
#' @examples
#' # Simulate data. Candidate 3 should be ranked first most often.
#' vote_df = data.frame(voter_id = rep(1:100, each = 4),
#'                      candidate = rep(1:4, times = 100),
#'                      vote_rank = replicate(100,
#'                                            order(sample(1:4,
#'                                                         replace = FALSE,
#'                                                         prob = c(.1, .45, .5, .1))),
#'                                            simplify = FALSE) |> unlist())
#' ranked_pairs(vote_df)
#'
#' @export
ranked_pairs = function(vote_df,
                        plot = TRUE) {

  # Checks ----
  rlang::check_installed('igraph', reason = "for ranked pairs.")

  right_cols = all(names(vote_df) == c("voter_id", "candidate", "vote_rank"))

  if (!right_cols) cli::cli_abort('vote_df must have these columns: c("voter_id", "candidate", "vote_rank")')

  n_cand = dplyr::n_distinct(vote_df$candidate)

  candidate_as_ints = is.integer(vote_df$candidate) && all(sort(unique(vote_df$candidate)) == seq_len(n_cand))

  if (!candidate_as_ints) cli::cli_abort("Candidates must be input as integers from 1:n_candidates")

  cli::cli_alert_warning("This function is still experimental. It probably doesn't handle ties nor ballots with equal ranks properly.")

  # Count prefs by pair ----
  .count_pair = function(c1, c2, vote_df) {
    res = vote_df |>
      filter(candidate %in% c(c1, c2)) |>
      summarise(pref_c1 = sum(vote_rank[candidate == c1] <  vote_rank[candidate == c2]),
                pref_c2 = sum(vote_rank[candidate == c1] >  vote_rank[candidate == c2]),
                eq_rank = sum(vote_rank[candidate == c1] == vote_rank[candidate == c2]))

    list(res = res,
         selection = names(res)[which.max(unlist(res))])

  }

  pair_df = combn(1:n_cand, 2) |> t() |>
    `colnames<-`(c("V1", "V2")) |>
    as_tibble() |>
    mutate(pair_res = mapply(.count_pair,
                             V1, V2,
                             MoreArgs = list(vote_df = vote_df),
                             SIMPLIFY = FALSE)) |>
    mutate(sel = sapply(pair_res,
                        \(x) x$selection),
           marg = sapply(pair_res,
                         \(x) abs(x$res$pref_c1 - x$res$pref_c2)),
           elim = FALSE) |>
    arrange(-marg)

  # Compute pair dominance graph ----

  g = igraph::make_empty_graph(n = n_cand, directed = TRUE)

  for (i in seq_len(nrow(pair_df))) {
    nodes = c(pair_df$V1[i], pair_df$V2[i])

    if (pair_df$sel[i] == "pref_c2") nodes = rev(nodes)

    g_prop = g + igraph::edges(nodes)

    cycle_check = !igraph::is_acyclic(g_prop)

    if (cycle_check) {
      pair_df$elim[i] = TRUE
      next
    } else {
      g = g_prop
    }
  }

  if (plot) igraph::plot.igraph(g)

  list(pair_df = pair_df,
       pair_graph = g,
       winner = as.numeric(igraph::topo_sort(g))[1])
}
