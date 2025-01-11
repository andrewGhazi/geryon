check_vote_df = function(vote_df, third) {
  right_cols = all(names(vote_df) == c("voter_id", "candidate", third))

  if (!right_cols) cli::cli_abort('vote_df must have these columns: c("voter_id", "candidate", \"{third}\")')

  n_cand = dplyr::n_distinct(vote_df$candidate)

  candidate_as_ints = is.integer(vote_df$candidate) && all(sort(unique(vote_df$candidate)) == seq_len(n_cand))

  if (!candidate_as_ints) cli::cli_abort("Candidates must be input as integers from 1:n_candidates")
}

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

  check_vote_df(vote_df, "vote_rank")

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

#' STAR voting
#'
#' @description Score Then Automatic Runoff (STAR) voting. See
#' \link{https://en.wikipedia.org/wiki/STAR_voting} for more detail on
#' properties.
#' @inheritParams ranked_pairs
#' @param verbose print score and automatic runoff tables
#' @returns invisibly returns the automatic runoff table
#' @examples
#' sim_vote = function(n_cand) {
#'   # Simulate one voter's scores from 0-5.
#'   # Higher candidates more likely to get higher scores.
#'   # Last candidate will be most likely to win.
#'
#'   floor(6*rbeta(n_cand, 1:n_cand, 1))
#' }
#'
#' n_voter = 100
#' n_cand = 4
#'
#' vote_df = data.frame(voter_id  = rep(1:n_voter, each = n_cand),
#'                      candidate = rep(1:n_cand, times = n_voter),
#'                      score = as.vector(replicate(n_voter, sim_vote(n_cand))))
#'
#' star(vote_df)
#' @export
star = function(vote_df, verbose = TRUE) {

  if (verbose) cli::cli_alert_warning("This function is still experimental. It probably shouldn't be used for anything serious.")

  check_vote_df(vote_df, "score")

  # Score ----

  top_scores = vote_df |>
    summarise(.by = "candidate",
              sum_score = sum(score)) |>
    arrange(-sum_score)

  if (verbose) {
    cli::cli_inform("{.strong Score sums:}")
    print(top_scores)
  }

  top_scorers = top_scores |>
    head(n = 2) |>
    pull(candidate) |>
    sort()

  # Then Automatic Runoff ----

  .get_preference = function(scores, cands) {
    if (length(scores) != 2) cli::cli_abort("wrong number of scores")
    if (length(cands) != 2)  cli::cli_abort("wrong number of candidates")

    if (scores[2] > scores[1]) return(cands[2])
    if (scores[1] > scores[2]) return(cands[1])
    if (scores[1] == scores[2]) return(NA)

    cli::cli_abort("Preference function shouldn't get here, wtf?")
  }

  res_df = vote_df |>
    dplyr::filter(candidate %in% top_scorers) |>
    arrange(voter_id, candidate) |>
    group_by(voter_id) |>
    summarise(preference = .get_preference(score, top_scorers)) |>
    count(preference) |>
    arrange(-n)

  if (verbose) {
    if (verbose) {
      cli::cli_inform("{.strong Automatic runoff results:}")
      print(res_df)
    }
  }

  if (verbose) cli::cli_alert_success(paste0("The winning candidate is: ", dplyr::filter(res_df, !is.na(preference))$preference[1]))

  invisible(res_df)
}
