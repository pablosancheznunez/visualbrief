#' A synthetic campaign
#'
#' Twelve weeks of daily results of a simulated awareness campaign: four pieces
#' of content shown to three audiences on three channels. **All values are
#' synthetic**; they do not describe real people or platforms. Because the
#' generating effects are known, the data are suited to teaching and to checking
#' that an analysis recovers them.
#'
#' Known effects on click-through rate (multipliers of a 1.2% base rate):
#'
#' | Audience | Video | Carousel | Infographic | Testimonial |
#' |---|---|---|---|---|
#' | Students | 1.6 | 1.1 | 0.9 | 0.8 |
#' | Professionals | 1.0 | 1.0 | 1.1 | 1.5 |
#' | Researchers | 0.8 | 0.9 | 1.7 | 1.0 |
#'
#' Search multiplies click-through by 2.2, LinkedIn by 1.0 and Instagram by
#' 0.8; response declines 3% a week (creative fatigue); 8% of clicks convert.
#'
#' @format A `vb_campaign` data frame with 3,024 rows and 11 columns: `date`,
#'   `channel`, `campaign`, `content`, `audience`, `impressions`, `reach`,
#'   `clicks`, `engagements`, `conversions` and `spend` (in euros).
#' @source Simulated with `data-raw/campaign_demo.R` in the package sources.
#' @examples
#' campaign_demo
#' vb_summarise(campaign_demo, by = "audience")
"campaign_demo"
