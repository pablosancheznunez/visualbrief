#' Wilson score interval for a proportion
#'
#' @param x Successes.
#' @param n Trials.
#' @param level Confidence level.
#' @return A two-column matrix with the lower and upper bounds.
#' @noRd
wilson <- function(x, n, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  out <- cbind(centre - half, centre + half)
  out[!(n > 0) | is.na(x) | is.na(n), ] <- NA
  pmin(pmax(out, 0), 1)
}

vb_rates <- list(
  ctr = c("clicks", "impressions"),
  engagement_rate = c("engagements", "impressions"),
  conversion_rate = c("conversions", "clicks")
)

safe_div <- function(a, b) ifelse(is.na(a) | is.na(b) | b == 0, NA_real_, a / b)

add_period <- function(date, unit) {
  switch(unit,
    day = date,
    week = date - (as.integer(format(date, "%u")) - 1L),
    month = as.Date(format(date, "%Y-%m-01")),
    stop("`unit` must be \"day\", \"week\" or \"month\".", call. = FALSE)
  )
}

#' Summarise campaign metrics
#'
#' Adds up the counts for each group and computes the usual rates and costs,
#' with confidence intervals for the rates.
#'
#' Rates (with Wilson score intervals):
#' * `ctr` = clicks / impressions
#' * `engagement_rate` = engagements / impressions
#' * `conversion_rate` = conversions / clicks
#'
#' Costs: `cpm` = spend per 1,000 impressions, `cpc` = spend per click, `cpa` =
#' spend per conversion.
#'
#' **Reach is not additive**: the same person can be reached on several days
#' or channels, so summed reach is an upper bound of unique reach.
#'
#' @param x A `vb_campaign`.
#' @param by Dimensions to group by: any of `"channel"`, `"campaign"`,
#'   `"content"`, `"audience"`, plus `"period"`. `NULL` gives one total row.
#' @param unit Time unit for `"period"`: `"day"`, `"week"` (starting on
#'   Monday) or `"month"`.
#' @param level Confidence level for the intervals.
#' @return A data frame with one row per group.
#' @export
#' @examples
#' vb_summarise(campaign_demo, by = "channel")
#' s <- vb_summarise(campaign_demo, by = c("content", "audience"))
#' s[, c("content", "audience", "ctr", "ctr_low", "ctr_high")]
vb_summarise <- function(x, by = "content", unit = "week", level = 0.95) {
  check_campaign(x)
  by <- check_by(by)
  if ("period" %in% by) x$period <- add_period(x$date, unit)
  if (length(by)) {
    keys <- lapply(by, function(b) { v <- x[[b]]; if (inherits(v, "Date")) format(v) else ifelse(is.na(v), "(none)", v) })
    groups <- interaction(keys, drop = TRUE, lex.order = TRUE, sep = "\r")
  } else {
    groups <- factor(rep("all", nrow(x)))
  }
  sums <- lapply(vb_counts, function(f) {
    v <- x[[f]]
    if (all(is.na(v))) return(rep(NA_real_, nlevels(groups)))
    as.numeric(tapply(v, groups, sum, na.rm = TRUE))
  })
  names(sums) <- vb_counts
  out <- if (length(by)) {
    parts <- do.call(rbind, strsplit(levels(groups), "\r", fixed = TRUE))
    df <- as.data.frame(matrix(parts, ncol = length(by)), stringsAsFactors = FALSE)
    names(df) <- by
    if ("period" %in% by) df$period <- as.Date(df$period)
    df
  } else {
    data.frame(row.names = 1)
  }
  for (f in vb_counts) out[[f]] <- sums[[f]]
  for (r in names(vb_rates)) {
    num <- out[[vb_rates[[r]][1]]]
    den <- out[[vb_rates[[r]][2]]]
    out[[r]] <- safe_div(num, den)
    ci <- wilson(num, den, level)
    out[[paste0(r, "_low")]] <- ci[, 1]
    out[[paste0(r, "_high")]] <- ci[, 2]
  }
  out$cpm <- safe_div(out$spend, out$impressions) * 1000
  out$cpc <- safe_div(out$spend, out$clicks)
  out$cpa <- safe_div(out$spend, out$conversions)
  if (length(by)) out <- out[do.call(order, unname(as.list(out[by]))), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("vb_summary", "data.frame"), by = by, unit = unit, level = level)
}

check_campaign <- function(x) {
  if (!inherits(x, "vb_campaign")) stop("`x` must be a vb_campaign; see vb_campaign().", call. = FALSE)
}

check_by <- function(by) {
  if (is.null(by)) return(character())
  ok <- c(vb_dimensions, "period")
  bad <- setdiff(by, ok)
  if (length(bad)) stop("Unknown grouping: ", paste(bad, collapse = ", "), ". Use ", paste(ok, collapse = ", "), ".", call. = FALSE)
  by
}

#' Campaign metrics over time
#'
#' @inheritParams vb_summarise
#' @param by Optional dimension to split the series, e.g. `"content"`.
#' @return A `vb_summary` with a `period` column, ordered by group and time.
#' @export
#' @examples
#' tr <- vb_trend(campaign_demo, by = "channel", unit = "week")
#' head(tr[, c("channel", "period", "impressions", "ctr")])
vb_trend <- function(x, by = NULL, unit = "week", level = 0.95) {
  if (length(by) > 1) stop("`by` takes a single dimension.", call. = FALSE)
  vb_summarise(x, by = c(by, "period"), unit = unit, level = level)
}
