vb_fields <- c("date", "channel", "campaign", "content", "audience",
               "impressions", "reach", "clicks", "engagements", "conversions", "spend")
vb_dimensions <- c("channel", "campaign", "content", "audience")
vb_counts <- c("impressions", "reach", "clicks", "engagements", "conversions", "spend")

#' Standardise campaign data
#'
#' Maps the columns of any campaign export (advertising platforms, social
#' media tools, web analytics or a spreadsheet) to the standard columns used
#' by visualbrief, checks them and returns a `vb_campaign` data frame.
#'
#' Each argument names the column of `data` that holds that field. Only
#' `impressions` is required; fields you do not have are filled with `NA` and
#' the metrics that need them are reported as `NA`.
#'
#' | Field | Meaning |
#' |---|---|
#' | `date` | Day of the observation |
#' | `channel` | Platform or medium, e.g. "Instagram", "Search", "Email" |
#' | `campaign` | Campaign name |
#' | `content` | Piece, message or creative variant |
#' | `audience` | Audience or segment |
#' | `impressions` | Times the content was shown |
#' | `reach` | Unique people reached |
#' | `clicks` | Clicks or link visits |
#' | `engagements` | Interactions (likes, comments, shares, saves...) |
#' | `conversions` | Goal completions |
#' | `spend` | Cost, in one currency |
#'
#' @param data A data frame, one row per date and combination of dimensions.
#' @param date,channel,campaign,content,audience,impressions,reach,clicks,engagements,conversions,spend
#'   Names of the columns of `data` holding each field, or `NULL` if absent.
#' @param date_format Format of `date` when it is text, see [strptime()].
#' @return A data frame of class `vb_campaign` with the eleven standard
#'   columns.
#' @seealso [vb_read_csv()] to read and standardise a file in one step.
#' @export
#' @examples
#' raw <- data.frame(
#'   Day = c("2026-03-02", "2026-03-02", "2026-03-03"),
#'   Ad = c("Video", "Carousel", "Video"),
#'   Impr = c(1200, 950, 1300),
#'   Clicks = c(36, 19, 41)
#' )
#' camp <- vb_campaign(raw, date = "Day", content = "Ad",
#'                     impressions = "Impr", clicks = "Clicks")
#' camp
vb_campaign <- function(data, date = "date", channel = "channel", campaign = "campaign",
                        content = "content", audience = "audience",
                        impressions = "impressions", reach = "reach", clicks = "clicks",
                        engagements = "engagements", conversions = "conversions",
                        spend = "spend", date_format = "%Y-%m-%d") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  map <- list(date = date, channel = channel, campaign = campaign, content = content,
              audience = audience, impressions = impressions, reach = reach, clicks = clicks,
              engagements = engagements, conversions = conversions, spend = spend)
  if (is.null(map$impressions) || !map$impressions %in% names(data)) {
    stop("`impressions` must name a column of `data`; it is the one required field.", call. = FALSE)
  }
  out <- data.frame(row.names = seq_len(nrow(data)))
  for (f in vb_fields) {
    col <- map[[f]]
    present <- !is.null(col) && col %in% names(data)
    explicit <- !is.null(col) && !identical(col, f)
    if (explicit && !present) stop("Column `", col, "` (for `", f, "`) is not in `data`.", call. = FALSE)
    out[[f]] <- if (present) data[[col]] else NA
  }
  out$date <- as_date(out$date, date_format)
  for (f in vb_dimensions) out[[f]] <- ifelse(is.na(out[[f]]), NA_character_, as.character(out[[f]]))
  for (f in vb_counts) {
    v <- out[[f]]
    if (is.character(v)) v <- as.numeric(gsub("[^0-9.eE+-]", "", v))
    out[[f]] <- as.numeric(v)
    if (any(out[[f]] < 0, na.rm = TRUE)) stop("`", f, "` has negative values.", call. = FALSE)
  }
  check_ratio <- function(num, den) {
    bad <- sum(out[[num]] > out[[den]], na.rm = TRUE)
    if (bad) warning(bad, " rows have more ", num, " than ", den, "; check the column mapping.", call. = FALSE)
  }
  check_ratio("clicks", "impressions")
  check_ratio("reach", "impressions")
  structure(out, class = c("vb_campaign", "data.frame"))
}

as_date <- function(x, format) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (all(is.na(x))) return(as.Date(rep(NA_character_, length(x))))
  d <- as.Date(as.character(x), format = format)
  if (any(is.na(d) & !is.na(x))) stop("Some dates do not match `date_format` (\"", format, "\").", call. = FALSE)
  d
}

#' Read and standardise a campaign export
#'
#' Reads a CSV export and passes it to [vb_campaign()]. Handles both
#' comma-separated files with decimal points and the semicolon-separated files
#' with decimal commas produced by spreadsheets in many European locales.
#'
#' @param file Path to a CSV file.
#' @param ... Column mapping passed to [vb_campaign()].
#' @param sep,dec Field separator and decimal mark. With `sep = NULL` the
#'   separator is guessed from the first line.
#' @param encoding File encoding.
#' @return A `vb_campaign` data frame.
#' @export
#' @examples
#' f <- tempfile(fileext = ".csv")
#' write.csv(campaign_demo[1:20, ], f, row.names = FALSE)
#' camp <- vb_read_csv(f)
#' summary(camp)
#' unlink(f)
vb_read_csv <- function(file, ..., sep = NULL, dec = NULL, encoding = "UTF-8") {
  first <- readLines(file, n = 1, warn = FALSE, encoding = encoding)
  if (is.null(sep)) sep <- if (lengths(regmatches(first, gregexpr(";", first))) > lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  if (is.null(dec)) dec <- if (sep == ";") "," else "."
  data <- utils::read.csv(file, sep = sep, dec = dec, stringsAsFactors = FALSE,
                          check.names = FALSE, encoding = encoding)
  vb_campaign(data, ...)
}

#' @export
print.vb_campaign <- function(x, ...) {
  rng <- range(x$date, na.rm = TRUE)
  cat("<vb_campaign> ", nrow(x), " rows", sep = "")
  if (all(is.finite(rng))) cat(", ", format(rng[1]), " to ", format(rng[2]), sep = "")
  cat("\n")
  for (f in vb_dimensions) {
    lv <- unique(stats::na.omit(x[[f]]))
    if (length(lv)) {
      shown <- paste(utils::head(lv, 6), collapse = ", ")
      if (length(lv) > 6) shown <- paste0(shown, ", ...")
      cat(sprintf("  %-12s %d: %s\n", f, length(lv), shown))
    }
  }
  have <- vb_counts[vapply(vb_counts, function(f) any(!is.na(x[[f]])), TRUE)]
  cat("  metrics      ", paste(have, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
summary.vb_campaign <- function(object, ...) {
  vb_summarise(object, by = NULL)
}
