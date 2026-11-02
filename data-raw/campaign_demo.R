# Synthetic campaign used in examples, tests and the vignette.
# All values are simulated; the known effects are documented in R/data.R.
# Run from the package root after devtools::load_all().

set.seed(20260302)
dates <- seq(as.Date("2026-03-02"), by = "day", length.out = 84)
channels <- c(Instagram = 900, LinkedIn = 500, Search = 400)        # mean daily impressions
cpm <- c(Instagram = 6, LinkedIn = 14, Search = 12)                  # cost per 1,000 impressions
audiences <- c(Students = 1.2, Professionals = 1, Researchers = 0.7) # delivery multiplier
contents <- c("Video", "Carousel", "Infographic", "Testimonial")

base_ctr <- 0.012
# Known effects on click-through, by audience (multipliers of base_ctr).
effect <- rbind(
  Students      = c(Video = 1.6, Carousel = 1.1, Infographic = 0.9, Testimonial = 0.8),
  Professionals = c(Video = 1.0, Carousel = 1.0, Infographic = 1.1, Testimonial = 1.5),
  Researchers   = c(Video = 0.8, Carousel = 0.9, Infographic = 1.7, Testimonial = 1.0)
)
channel_ctr <- c(Instagram = 0.8, LinkedIn = 1.0, Search = 2.2)
eng_rate <- c(Instagram = 0.045, LinkedIn = 0.025, Search = 0.002)

grid <- expand.grid(date = dates, channel = names(channels), audience = names(audiences),
                    content = contents, stringsAsFactors = FALSE)
week <- as.integer(grid$date - min(dates)) %/% 7
fatigue <- 0.97^week                                                 # 3% weekly decline in response

grid$campaign <- "Spring awareness"
grid$impressions <- stats::rpois(nrow(grid), channels[grid$channel] * audiences[grid$audience] / length(contents))
grid$reach <- round(grid$impressions * stats::runif(nrow(grid), 0.6, 0.85))
p_click <- base_ctr * effect[cbind(grid$audience, grid$content)] * channel_ctr[grid$channel] * fatigue
grid$clicks <- stats::rbinom(nrow(grid), grid$impressions, pmin(p_click, 1))
grid$engagements <- stats::rbinom(nrow(grid), grid$impressions, eng_rate[grid$channel] * fatigue)
grid$conversions <- stats::rbinom(nrow(grid), grid$clicks, 0.08)
grid$spend <- round(grid$impressions / 1000 * cpm[grid$channel], 2)

campaign_demo <- vb_campaign(grid[order(grid$date, grid$channel, grid$audience, grid$content),
                                               c("date", "channel", "campaign", "content", "audience", "impressions",
                                                 "reach", "clicks", "engagements", "conversions", "spend")])
rownames(campaign_demo) <- NULL
save(campaign_demo, file = "data/campaign_demo.rda", compress = "xz")
