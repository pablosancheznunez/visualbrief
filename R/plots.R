metric_label <- c(
  ctr = "Click-through rate", engagement_rate = "Engagement rate", conversion_rate = "Conversion rate",
  impressions = "Impressions", reach = "Reach (summed)", clicks = "Clicks", engagements = "Engagements",
  conversions = "Conversions", spend = "Spend", cpm = "Cost per 1,000 impressions", cpc = "Cost per click",
  cpa = "Cost per conversion"
)
is_rate <- function(m) m %in% names(vb_rates)

fmt_value <- function(v, metric) {
  if (is_rate(metric)) return(paste0(formatC(100 * v, format = "f", digits = 2), "%"))
  if (metric %in% c("cpm", "cpc", "cpa", "spend")) return(formatC(v, format = "f", digits = 2, big.mark = ","))
  formatC(v, format = "d", big.mark = ",")
}

with_brand <- function(brand, code) {
  op <- graphics::par(family = brand$font, fg = brand$text, col.axis = brand$text,
                      col.lab = brand$text, col.main = brand$text, bg = brand$background,
                      las = 1, bty = "n", cex.axis = 0.85)
  on.exit(graphics::par(op), add = TRUE)
  force(code)
}

#' Plot a rate with confidence intervals
#'
#' Dot-and-interval chart of a rate for each level of `by`, with one panel per
#' level of `within`. Intervals make visible which differences are within
#' the noise of the data.
#'
#' @inheritParams vb_compare
#' @param brand A `vb_brand`.
#' @param main Chart title; by default, the metric name.
#' @return The summary used for the chart, invisibly.
#' @export
#' @examples
#' vb_plot_rates(campaign_demo, metric = "ctr", by = "content", within = "audience")
vb_plot_rates <- function(x, metric = c("ctr", "engagement_rate", "conversion_rate"),
                          by = "content", within = NULL, brand = vb_brand(), level = 0.95,
                          main = NULL) {
  metric <- match.arg(metric)
  s <- vb_summarise(x, by = c(within, by), level = level)
  s <- s[!is.na(s[[metric]]), , drop = FALSE]
  if (!nrow(s)) stop("No data for ", metric, ".", call. = FALSE)
  panels <- if (is.null(within)) list(s) else split(s, s[[within]])
  levels_by <- sort(unique(s[[by]]), decreasing = TRUE)
  xlim <- range(c(0, s[[paste0(metric, "_low")]], s[[paste0(metric, "_high")]]), na.rm = TRUE)
  with_brand(brand, {
    margin <- max(graphics::strwidth(levels_by, units = "inches", cex = 0.85), 0.6) + 0.45
    np <- length(panels)
    graphics::par(mfrow = c(np, 1), mai = c(0.45, margin, 0.35, 0.2),
                  oma = c(1.2, 0, if (is.null(main) && np == 1) 0 else 2, 0))
    for (i in seq_along(panels)) {
      p <- panels[[i]]
      y <- match(p[[by]], levels_by)
      graphics::plot(NA, xlim = xlim, ylim = c(0.5, length(levels_by) + 0.5), yaxt = "n", xaxt = "n",
                     xlab = "", ylab = "", main = if (is.null(within)) "" else names(panels)[i],
                     font.main = 1, cex.main = 0.95)
      ticks <- pretty(xlim)
      graphics::abline(v = ticks, col = grDevices::adjustcolor(brand$text, 0.12))
      graphics::axis(1, at = ticks, labels = fmt_value(ticks, metric), lwd = 0, lwd.ticks = 0)
      graphics::axis(2, at = seq_along(levels_by), labels = levels_by, lwd = 0, lwd.ticks = 0)
      graphics::segments(p[[paste0(metric, "_low")]], y, p[[paste0(metric, "_high")]], y,
                         col = brand$primary, lwd = 2.5)
      graphics::points(p[[metric]], y, pch = 21, bg = brand$background, col = brand$primary, cex = 1.3, lwd = 2)
    }
    graphics::mtext(main %||% metric_label[[metric]], side = 3, outer = TRUE, line = 0.4, font = 2)
    graphics::mtext(sprintf("Points: observed rate. Lines: %d%% confidence interval.", round(100 * level)),
                    side = 1, outer = TRUE, line = 0, cex = 0.75, col = grDevices::adjustcolor(brand$text, 0.7))
  })
  invisible(s)
}

#' Plot a metric over time
#'
#' Line chart of a count, cost or rate by period, optionally one line per level
#' of `by`, coloured with the brand palette.
#'
#' @inheritParams vb_trend
#' @param metric A count (`"impressions"`, `"clicks"`...), cost (`"cpc"`...)
#'   or rate (`"ctr"`...).
#' @param brand A `vb_brand`.
#' @param main Chart title; by default, the metric name.
#' @return The trend data used for the chart, invisibly.
#' @export
#' @examples
#' vb_plot_trend(campaign_demo, metric = "ctr", by = "channel")
vb_plot_trend <- function(x, metric = "impressions", by = NULL, unit = "week",
                          brand = vb_brand(), main = NULL) {
  tr <- vb_trend(x, by = by, unit = unit)
  if (!metric %in% names(tr)) stop("Unknown metric: ", metric, call. = FALSE)
  tr <- tr[!is.na(tr[[metric]]), , drop = FALSE]
  if (!nrow(tr)) stop("No data for ", metric, ".", call. = FALSE)
  series <- if (is.null(by)) list(all = tr) else split(tr, tr[[by]])
  cols <- rep(brand$palette, length.out = length(series))
  with_brand(brand, {
    graphics::par(mai = c(0.6, 0.9, 0.5, if (is.null(by)) 0.2 else 1.6))
    ylim <- range(c(0, tr[[metric]]), na.rm = TRUE)
    graphics::plot(NA, xlim = range(tr$period), ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "",
                   main = main %||% paste(metric_label[[metric]], "by", unit), font.main = 2, cex.main = 1)
    ticks <- pretty(ylim)
    graphics::abline(h = ticks, col = grDevices::adjustcolor(brand$text, 0.12))
    graphics::axis(2, at = ticks, labels = fmt_value(ticks, metric), lwd = 0, lwd.ticks = 0)
    # ISO dates: readable in any language and locale.
    graphics::axis.Date(1, at = pretty(tr$period), format = "%Y-%m-%d", lwd = 0, lwd.ticks = 0)
    for (i in seq_along(series)) {
      s <- series[[i]][order(series[[i]]$period), ]
      graphics::lines(s$period, s[[metric]], col = cols[i], lwd = 2.5)
      graphics::points(s$period, s[[metric]], col = cols[i], pch = 16, cex = 0.7)
    }
    if (!is.null(by)) {
      last <- vapply(series, function(s) s[[metric]][which.max(s$period)], numeric(1))
      ord <- order(last)
      graphics::par(xpd = NA)
      graphics::text(graphics::par("usr")[2], seq(ylim[1], ylim[2], length.out = length(series) + 2)[-c(1, length(series) + 2)],
                     labels = names(series)[ord], col = cols[ord], adj = c(-0.1, 0.5), font = 2, cex = 0.85)
    }
  })
  invisible(tr)
}
