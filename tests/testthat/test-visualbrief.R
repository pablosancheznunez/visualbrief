small <- function() {
  vb_campaign(data.frame(
    Day = c("2026-03-02", "2026-03-02", "2026-03-09", "2026-03-09"),
    Ad = c("A", "B", "A", "B"), Seg = c("x", "x", "y", "y"),
    Impr = c(1000, 1000, 2000, 2000), Clk = c(50, 20, 80, 40), Cost = c(10, 10, 20, 20)
  ), date = "Day", content = "Ad", audience = "Seg", impressions = "Impr", clicks = "Clk", spend = "Cost")
}

test_that("vb_campaign maps, fills and validates columns", {
  x <- small()
  expect_s3_class(x, "vb_campaign")
  expect_named(x, c("date", "channel", "campaign", "content", "audience", "impressions",
                    "reach", "clicks", "engagements", "conversions", "spend"))
  expect_s3_class(x$date, "Date")
  expect_true(all(is.na(x$reach)))
  expect_error(vb_campaign(data.frame(a = 1)), "required")
  expect_error(vb_campaign(data.frame(impressions = 1), clicks = "missing"), "not in")
  expect_error(vb_campaign(data.frame(impressions = -1)), "negative")
  expect_warning(vb_campaign(data.frame(impressions = 10, clicks = 20)), "more clicks")
  expect_error(vb_campaign(data.frame(impressions = 1, date = "02/03/2026")), "date_format")
  expect_output(print(x), "vb_campaign")
})

test_that("vb_read_csv reads comma and semicolon files", {
  f1 <- tempfile(fileext = ".csv")
  f2 <- tempfile(fileext = ".csv")
  on.exit(unlink(c(f1, f2)))
  writeLines(c("date,impressions,clicks,spend", "2026-03-02,1000,30,12.5"), f1)
  writeLines(c("date;impressions;clicks;spend", "2026-03-02;1000;30;12,5"), f2)
  a <- vb_read_csv(f1)
  b <- vb_read_csv(f2)
  expect_equal(a$spend, 12.5)
  expect_equal(b$spend, 12.5)
})

test_that("Wilson intervals match a reference value", {
  # 50 successes out of 1000: Wilson 95% interval 0.0381 to 0.0653 (Newcombe 1998, method 3).
  ci <- wilson(50, 1000)
  expect_equal(round(as.numeric(ci), 4), c(0.0381, 0.0653))
  expect_true(all(is.na(wilson(0, 0))))
})

test_that("vb_summarise adds counts and computes rates and costs", {
  s <- vb_summarise(small(), by = "content")
  expect_s3_class(s, "vb_summary")
  a <- s[s$content == "A", ]
  expect_equal(a$impressions, 3000)
  expect_equal(a$clicks, 130)
  expect_equal(a$ctr, 130 / 3000)
  expect_equal(a$cpc, 30 / 130)
  expect_equal(a$cpm, 30 / 3000 * 1000)
  expect_true(a$ctr_low < a$ctr && a$ctr < a$ctr_high)
  expect_true(is.na(a$engagement_rate))
  total <- vb_summarise(small(), by = NULL)
  expect_equal(nrow(total), 1)
  expect_error(vb_summarise(small(), by = "colour"), "Unknown grouping")
})

test_that("weekly and monthly periods start on Monday and the first of the month", {
  tr <- vb_trend(campaign_demo, unit = "week")
  expect_true(all(format(tr$period, "%u") == "1"))
  mo <- vb_trend(campaign_demo, unit = "month")
  expect_true(all(format(mo$period, "%d") == "01"))
})

test_that("vb_compare recovers the known effects of the demo campaign", {
  cmp <- vb_compare(campaign_demo, metric = "ctr", by = "content", within = "audience")
  expect_s3_class(cmp, "vb_comparison")
  best <- vapply(split(cmp, cmp$audience), function(g) g$content[which.max(g$rate)], "")
  expect_equal(unname(best[c("Professionals", "Researchers", "Students")]), c("Testimonial", "Infographic", "Video"))
  ref_rows <- cmp[cmp$content == cmp$reference, ]
  expect_true(all(is.na(ref_rows$diff)))
  others <- cmp[cmp$content != cmp$reference, ]
  expect_true(all(others$diff_low <= others$diff & others$diff <= others$diff_high))
  expect_true(all(others$p_adjusted >= others$p_value))
})

test_that("vb_compare handles references and errors", {
  cmp <- vb_compare(small(), metric = "ctr", by = "content", reference = "B")
  expect_equal(unique(cmp$reference), "B")
  expect_gt(cmp$diff[cmp$content == "A"], 0)
  expect_true(all(cmp$small_sample == FALSE | cmp$events < 10 | (cmp$trials - cmp$events) < 10))
  expect_error(vb_compare(small(), metric = "engagement_rate"), "engagements")
  expect_error(vb_compare(small(), by = "content", reference = "Z"), "not a level")
  expect_error(vb_compare(small(), by = "content", within = "content"), "must differ")
  expect_output(print(cmp), "association, not causation")
})

test_that("brands validate colours and report contrast", {
  expect_error(vb_brand(palette = c("#000000", "notacolour")), "Not valid")
  expect_error(vb_brand(palette = "#000000"), "at least two")
  expect_equal(round(vb_contrast("black", "white"), 1), 21)
  chk <- vb_check_contrast(vb_brand())
  expect_true(all(chk$passes))
  weak <- vb_check_contrast(vb_brand(palette = c("#F0E442", "#0072B2")))
  expect_false(any(weak$passes[weak$colour == "#F0E442"]))
  expect_output(print(vb_brand()), "vb_brand")
})

test_that("brand.yml files are read", {
  skip_if_not_installed("yaml")
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("meta:", "  name: Acme", "color:", "  palette:", "    navy: \"#1F4E5F\"", "    rust: \"#B35900\"",
               "  primary: navy", "typography:", "  base: Georgia"), f)
  b <- vb_brand_yml(f)
  expect_equal(b$name, "Acme")
  expect_equal(b$primary, "#1F4E5F")
  expect_equal(b$font, "Georgia")
  expect_true(all(c("#1F4E5F", "#B35900") %in% b$palette))
})

test_that("plots draw without error", {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  grDevices::png(f)
  expect_invisible(vb_plot_rates(campaign_demo, "ctr", by = "content", within = "audience"))
  expect_invisible(vb_plot_trend(campaign_demo, "impressions", by = "channel"))
  expect_invisible(vb_plot_trend(small(), "ctr"))
  grDevices::dev.off()
})

test_that("reports are self-contained HTML", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  vb_report(campaign_demo, f, title = "Test <report>")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "Test &lt;report&gt;", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_match(html, "not prove", fixed = TRUE)
  expect_false(grepl("http", gsub("https://[^\"]*", "", html)))
  vb_report(small(), f)
  expect_true(file.exists(f))
})

test_that("base64 matches a known encoding", {
  expect_equal(base64(charToRaw("Man")), "TWFu")
  expect_equal(base64(charToRaw("Ma")), "TWE=")
  expect_equal(base64(charToRaw("M")), "TQ==")
})

test_that("the Quarto template is copied", {
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  p <- vb_quarto_template(d)
  expect_true(file.exists(p))
  expect_error(vb_quarto_template(d), "exists")
})
