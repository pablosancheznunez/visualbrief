#' Compare content, audiences or channels on a rate
#'
#' Compares every level of `by` with a reference level on one rate, within
#' each level of `within` if given (for example, each piece of content within
#' each audience). Reports the rate with its Wilson interval, the difference
#' from the reference with Newcombe's hybrid score interval, and a two-sample
#' test of proportions with Holm's adjustment for the number of comparisons.
#'
#' Campaign data are observational unless they come from a randomised test:
#' delivery algorithms, timing and audience overlap all affect the numbers. A
#' difference here describes what happened in this campaign; it does not prove
#' that one piece causes better results.
#'
#' @inheritParams vb_summarise
#' @param metric `"ctr"`, `"engagement_rate"` or `"conversion_rate"`.
#' @param by Dimension whose levels are compared, e.g. `"content"`.
#' @param within Optional dimension to compare inside, e.g. `"audience"`.
#' @param reference Level of `by` used as reference. By default, within each
#'   group, the level with the largest denominator (the most delivered one).
#' @param min_events Rows with fewer events or non-events than this are
#'   flagged in `small_sample`; their intervals are wide and tests unreliable.
#' @return A data frame of class `vb_comparison`, one row per compared level.
#' @export
#' @examples
#' cmp <- vb_compare(campaign_demo, metric = "ctr", by = "content", within = "audience")
#' cols <- c("content", "rate", "diff", "diff_low", "diff_high", "p_adjusted")
#' cmp[cmp$audience == "Researchers", cols]
vb_compare <- function(x, metric = c("ctr", "engagement_rate", "conversion_rate"),
                       by = "content", within = NULL, reference = NULL,
                       level = 0.95, min_events = 10) {
  metric <- match.arg(metric)
  if (length(by) != 1) stop("`by` takes a single dimension.", call. = FALSE)
  if (identical(by, within)) stop("`by` and `within` must differ.", call. = FALSE)
  s <- vb_summarise(x, by = c(within, by), level = level)
  num <- vb_rates[[metric]][1]
  den <- vb_rates[[metric]][2]
  if (all(is.na(s[[num]])) || all(is.na(s[[den]]))) {
    stop("The data have no `", num, "` or `", den, "`, needed for ", metric, ".", call. = FALSE)
  }
  groups <- if (is.null(within)) list(all = s) else split(s, s[[within]])
  rows <- lapply(groups, function(g) {
    g <- g[!is.na(g[[den]]) & g[[den]] > 0, , drop = FALSE]
    if (nrow(g) < 2) return(NULL)
    ref <- if (is.null(reference)) g[[by]][which.max(g[[den]])] else reference
    if (!ref %in% g[[by]]) stop("Reference \"", ref, "\" is not a level of `", by, "`.", call. = FALSE)
    r <- g[g[[by]] == ref, ]
    p_ref <- r[[metric]]
    l_ref <- r[[paste0(metric, "_low")]]
    u_ref <- r[[paste0(metric, "_high")]]
    out <- data.frame(
      level = g[[by]], events = g[[num]], trials = g[[den]], rate = g[[metric]],
      rate_low = g[[paste0(metric, "_low")]], rate_high = g[[paste0(metric, "_high")]],
      reference = ref, stringsAsFactors = FALSE
    )
    out$diff <- out$rate - p_ref
    # Newcombe (1998), method 10: hybrid score interval for a difference of proportions.
    out$diff_low <- out$diff - sqrt((out$rate - out$rate_low)^2 + (u_ref - p_ref)^2)
    out$diff_high <- out$diff + sqrt((out$rate_high - out$rate)^2 + (p_ref - l_ref)^2)
    out$p_value <- vapply(seq_len(nrow(out)), function(i) {
      if (out$level[i] == ref) return(NA_real_)
      suppressWarnings(stats::prop.test(c(out$events[i], r[[num]]), c(out$trials[i], r[[den]]),
                                        correct = FALSE)$p.value)
    }, numeric(1))
    out$p_adjusted <- stats::p.adjust(out$p_value, method = "holm")
    out$small_sample <- out$events < min_events | (out$trials - out$events) < min_events
    out[out$level == ref, c("diff", "diff_low", "diff_high")] <- NA
    if (!is.null(within)) out <- cbind(stats::setNames(data.frame(g[[within]][1], stringsAsFactors = FALSE), within), out)
    out
  })
  res <- do.call(rbind, rows)
  if (is.null(res)) stop("Nothing to compare: each group needs at least two levels with data.", call. = FALSE)
  names(res)[names(res) == "level"] <- by
  rownames(res) <- NULL
  structure(res, class = c("vb_comparison", "data.frame"), metric = metric, by = by,
            within = within, level = level)
}

#' @export
print.vb_comparison <- function(x, digits = 3, ...) {
  m <- attr(x, "metric")
  # Subsets keep the class but lose the attributes: print them as data frames.
  if (is.null(m)) return(print(as.data.frame(unclass(x)), ...))
  cat("Comparison of ", m, " by ", attr(x, "by"),
      if (!is.null(attr(x, "within"))) paste0(" within ", attr(x, "within")), "\n", sep = "")
  shown <- as.data.frame(unclass(x))
  num <- vapply(shown, is.numeric, TRUE) & !names(shown) %in% c("events", "trials")
  shown[num] <- lapply(shown[num], signif, digits)
  print(shown, row.names = FALSE)
  cat("Differences are relative to the reference level; observational data show association, not causation.\n")
  invisible(x)
}
