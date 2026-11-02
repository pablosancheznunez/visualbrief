base64 <- function(raw) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(raw)
  pad <- (3 - n %% 3) %% 3
  b <- as.integer(c(raw, as.raw(rep(0, pad))))
  m <- matrix(b, nrow = 3)
  idx <- rbind(
    bitwShiftR(m[1, ], 2),
    bitwOr(bitwShiftL(bitwAnd(m[1, ], 3L), 4), bitwShiftR(m[2, ], 4)),
    bitwOr(bitwShiftL(bitwAnd(m[2, ], 15L), 2), bitwShiftR(m[3, ], 6)),
    bitwAnd(m[3, ], 63L)
  )
  out <- chars[as.vector(idx) + 1]
  if (pad) out[(length(out) - pad + 1):length(out)] <- "="
  paste(out, collapse = "")
}

data_uri <- function(path) {
  type <- switch(tolower(tools::file_ext(path)), svg = "image/svg+xml", jpg = , jpeg = "image/jpeg", "image/png")
  paste0("data:", type, ";base64,", base64(readBin(path, "raw", file.size(path))))
}

plot_uri <- function(width, height, code) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  type <- if (isTRUE(capabilities("cairo"))) "cairo" else getOption("bitmapType")
  grDevices::png(f, width = width, height = height, units = "in", res = 150, type = type)
  ok <- tryCatch({ force(code); TRUE }, finally = grDevices::dev.off())
  data_uri(f)
}

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

html_table <- function(df, caption = NULL) {
  head <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) paste0("<td>", esc(r), "</td>", collapse = ""))
  paste0("<div class=\"scroll\"><table>", if (!is.null(caption)) paste0("<caption>", esc(caption), "</caption>"),
         "<thead><tr>", head, "</tr></thead><tbody>",
         paste0("<tr>", rows, "</tr>", collapse = ""), "</tbody></table></div>")
}

css_font <- function(font) {
  switch(font,
    sans = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
    serif = "Georgia, 'Times New Roman', serif",
    mono = "ui-monospace, Consolas, monospace",
    paste0("'", gsub("'", "", font), "', system-ui, sans-serif")
  )
}

pct <- function(v) ifelse(is.na(v), "", paste0(formatC(100 * v, format = "f", digits = 2), "%"))
num <- function(v) ifelse(is.na(v), "", formatC(v, format = "d", big.mark = ","))
money <- function(v) ifelse(is.na(v), "", formatC(v, format = "f", digits = 2, big.mark = ","))

#' Write a campaign report
#'
#' Creates a self-contained HTML report (one file, no internet needed to open
#' it) with headline figures, results by channel, a comparison of content
#' across audiences with confidence intervals, the evolution over time, an
#' accessibility check of the brand colours and notes on method. It is meant
#' to be shared as is with a client, a team or a supervisor.
#'
#' @param x A `vb_campaign`.
#' @param file Output HTML file.
#' @param title Report title.
#' @param brand A `vb_brand`.
#' @param metric Rate used to compare content: `"ctr"`, `"engagement_rate"`
#'   or `"conversion_rate"`.
#' @param by,within Dimensions for the comparison, e.g. content within
#'   audience. `within = NULL` compares across the whole campaign.
#' @param unit Time unit for the trend: `"day"`, `"week"` or `"month"`.
#' @param currency Currency label for costs.
#' @return `file`, invisibly.
#' @seealso [vb_quarto_template()] for an editable Quarto version.
#' @export
#' @examples
#' f <- file.path(tempdir(), "campaign-report.html")
#' vb_report(campaign_demo, f, title = "Spring awareness campaign")
#' file.exists(f)
#' unlink(f)
vb_report <- function(x, file = "campaign-report.html", title = "Campaign report",
                      brand = vb_brand(), metric = "ctr", by = "content", within = "audience",
                      unit = "week", currency = "") {
  check_campaign(x)
  total <- vb_summarise(x, by = NULL)
  channels <- vb_summarise(x, by = "channel")
  has <- function(col) any(!is.na(x[[col]]))
  rng <- range(x$date, na.rm = TRUE)
  period <- if (all(is.finite(rng))) paste(format(rng[1]), "to", format(rng[2])) else ""

  kpi <- function(label, value) sprintf("<div class=\"kpi\"><span>%s</span><strong>%s</strong></div>", esc(label), esc(value))
  kpis <- c(kpi("Impressions", num(total$impressions)),
            if (has("reach")) kpi("Reach (summed)", num(total$reach)),
            if (has("clicks")) kpi("Click-through rate", pct(total$ctr)),
            if (has("engagements")) kpi("Engagement rate", pct(total$engagement_rate)),
            if (has("conversions")) kpi("Conversions", num(total$conversions)),
            if (has("spend")) kpi(paste("Spend", currency), money(total$spend)),
            if (has("spend") && has("conversions")) kpi(paste("Cost per conversion", currency), money(total$cpa)))

  ch <- data.frame(Channel = channels$channel, Impressions = num(channels$impressions),
                   Clicks = num(channels$clicks), CTR = pct(channels$ctr),
                   `Engagement rate` = pct(channels$engagement_rate),
                   Conversions = num(channels$conversions), `Conversion rate` = pct(channels$conversion_rate),
                   check.names = FALSE)
  if (has("spend")) ch[[paste("CPA", currency)]] <- money(channels$cpa)
  ch <- ch[, vapply(ch, function(v) any(nzchar(v)), TRUE), drop = FALSE]

  sections <- character()
  if (has(vb_rates[[metric]][1]) && has(by)) {
    n_panels <- if (is.null(within) || !has(within)) 1 else length(unique(stats::na.omit(x[[within]])))
    w <- if (n_panels == 1) NULL else within
    n_levels <- length(unique(stats::na.omit(x[[by]])))
    img <- plot_uri(7, 0.6 + n_panels * (0.55 + 0.32 * n_levels), vb_plot_rates(x, metric, by = by, within = w, brand = brand))
    cmp <- vb_compare(x, metric = metric, by = by, within = w)
    tab <- data.frame(check.names = FALSE,
                      Group = if (is.null(w)) "All" else cmp[[w]], Level = cmp[[by]],
                      Rate = pct(cmp$rate), `95% CI` = paste(pct(cmp$rate_low), "to", pct(cmp$rate_high)),
                      `vs reference` = ifelse(is.na(cmp$diff), "reference",
                                              paste0(ifelse(cmp$diff >= 0, "+", ""), formatC(100 * cmp$diff, format = "f", digits = 2), " pp")),
                      `Adjusted p` = ifelse(is.na(cmp$p_adjusted), "", formatC(cmp$p_adjusted, format = "g", digits = 2)),
                      Note = ifelse(cmp$small_sample, "few events", ""))
    sections <- c(sections, sprintf(
      "<section><h2>%s by %s%s</h2><p class=\"lead\">Which %s performs better%s? Points are observed rates; lines are 95%% confidence intervals. Overlapping intervals mean the data cannot separate the options.</p><img src=\"%s\" alt=\"%s of each %s%s with confidence intervals\">%s</section>",
      esc(metric_label[[metric]]), esc(by), if (is.null(w)) "" else paste(" and", esc(w)),
      esc(by), if (is.null(w)) "" else paste(" with each", esc(w)), img,
      esc(metric_label[[metric]]), esc(by), if (is.null(w)) "" else paste(" by", esc(w)),
      html_table(tab, "Each level compared with the most delivered level of its group (reference); p adjusted with Holm's method.")))
  }
  img_trend <- plot_uri(7, 3.2, vb_plot_trend(x, metric = if (has(vb_rates[[metric]][1])) metric else "impressions",
                                               by = if (has("channel")) "channel" else NULL, unit = unit, brand = brand))
  sections <- c(sections, sprintf("<section><h2>Over time</h2><img src=\"%s\" alt=\"Evolution by %s\"></section>", img_trend, esc(unit)))

  acc <- vb_check_contrast(brand)
  swatch <- sprintf("<span class=\"swatch\" style=\"background:%s\"></span>%s", acc$colour, acc$colour)
  acc_tab <- paste0("<div class=\"scroll\"><table><thead><tr><th>Role</th><th>Colour</th><th>Contrast</th><th>Required</th><th>Result</th></tr></thead><tbody>",
                    paste0("<tr><td>", esc(acc$role), "</td><td>", swatch, "</td><td>", acc$ratio, ":1</td><td>",
                           acc$required, ":1</td><td>", ifelse(acc$passes, "passes", "<strong>fails</strong>"), "</td></tr>", collapse = ""),
                    "</tbody></table></div>")
  sections <- c(sections, paste0("<section><h2>Accessibility of the colours</h2><p class=\"lead\">WCAG 2.2 asks for 4.5:1 for text and 3:1 for chart elements against the background.</p>", acc_tab, "</section>"))

  logo <- if (!is.null(brand$logo)) sprintf("<img class=\"logo\" src=\"%s\" alt=\"%s\">", data_uri(brand$logo), esc(brand$name)) else ""
  css <- sprintf(paste(
    ":root{--p:%s;--bg:%s;--t:%s}",
    "body{margin:0;background:var(--bg);color:var(--t);font-family:%s;line-height:1.5}",
    "main{max-width:880px;margin:0 auto;padding:32px 20px 56px}",
    "header{display:flex;justify-content:space-between;align-items:center;gap:16px;border-bottom:4px solid var(--p);padding-bottom:12px}",
    "h1{margin:0;font-size:1.7rem}h2{color:var(--p);font-size:1.2rem;margin-top:36px}",
    ".brand{color:var(--p);font-weight:700}.meta,.lead,caption{color:color-mix(in srgb,var(--t) 70%%,var(--bg))}",
    ".logo{max-height:56px}.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin-top:20px}",
    ".kpi{border:1px solid color-mix(in srgb,var(--t) 15%%,var(--bg));border-radius:8px;padding:10px 12px}",
    ".kpi span{display:block;font-size:.8rem}.kpi strong{font-size:1.35rem}",
    "img{max-width:100%%;height:auto}.scroll{overflow-x:auto}table{border-collapse:collapse;width:100%%;font-size:.88rem}",
    "caption{text-align:left;font-size:.8rem;padding:4px 0}",
    "th,td{text-align:left;padding:5px 8px;border-bottom:1px solid color-mix(in srgb,var(--t) 15%%,var(--bg))}",
    ".swatch{display:inline-block;width:12px;height:12px;border-radius:2px;margin-right:6px;vertical-align:-1px}",
    ".notes{font-size:.85rem}footer{margin-top:40px;font-size:.8rem}"),
    brand$primary, brand$background, brand$text, css_font(brand$font))
  page <- paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", esc(title), "</title><style>", css, "</style></head><body><main>",
    "<header><div><div class=\"brand\">", esc(brand$name), "</div><h1>", esc(title), "</h1><div class=\"meta\">",
    esc(period), " &middot; ", nrow(x), " rows</div></div>", logo, "</header>",
    "<div class=\"kpis\">", paste(kpis, collapse = ""), "</div>",
    "<section><h2>By channel</h2>", html_table(ch), "</section>",
    paste(sections, collapse = ""),
    "<section class=\"notes\"><h2>How to read this report</h2><ul>",
    "<li>Rates: click-through = clicks / impressions; engagement = engagements / impressions; conversion = conversions / clicks. Intervals are Wilson 95% intervals.</li>",
    "<li>Reach is summed over rows; people reached on several days or channels are counted more than once.</li>",
    "<li>Campaign data are observational unless they come from a randomised test: delivery algorithms, timing and audience overlap shape the numbers. Differences describe this campaign; they do not prove that one piece causes better results.</li>",
    "</ul></section><footer>Made with visualbrief ", as.character(utils::packageVersion("visualbrief")), " on ", format(Sys.Date()), ".</footer>",
    "</main></body></html>\n")
  writeLines(page, file, useBytes = TRUE)
  invisible(file)
}

#' Copy the Quarto report template
#'
#' Copies an editable Quarto document that builds a campaign report with
#' visualbrief, for teams that want to add their own text and sections.
#' Rendering it needs Quarto (<https://quarto.org>); [vb_report()] does not.
#'
#' @param dir Folder where the template is copied.
#' @param overwrite Overwrite an existing file.
#' @return The path of the copied `.qmd` file, invisibly.
#' @export
#' @examples
#' path <- vb_quarto_template(tempdir(), overwrite = TRUE)
#' basename(path)
#' unlink(path)
vb_quarto_template <- function(dir = ".", overwrite = FALSE) {
  src <- system.file("templates", "campaign-report.qmd", package = "visualbrief", mustWork = TRUE)
  dest <- file.path(dir, "campaign-report.qmd")
  if (file.exists(dest) && !overwrite) stop(dest, " exists; use overwrite = TRUE.", call. = FALSE)
  file.copy(src, dest, overwrite = TRUE)
  invisible(dest)
}
