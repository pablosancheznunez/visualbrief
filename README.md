# visualbrief <img src="man/figures/logo.png" align="right" height="139" alt="visualbrief logo" />

[![R-CMD-check](https://github.com/pablosancheznunez/visualbrief/actions/workflows/ci.yml/badge.svg)](https://github.com/pablosancheznunez/visualbrief/actions/workflows/ci.yml)

**Turn campaign data into clear, reusable visual reports.**

visualbrief takes the results of a digital campaign (exports from advertising platforms, social media tools, web analytics or a spreadsheet) and turns them into summaries, comparisons of content across audiences with confidence intervals, branded and accessible charts, and a self-contained report you can send to a client, a team or a supervisor.

It helps answer:

- Which pieces or messages work best with each audience?
- What patterns appear in a campaign over time?
- How do I present the results so that anyone can understand them?

It describes what happened in a campaign; it does not claim to predict which design or ad will work.

## Installation

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/visualbrief")
```

## Example

```r
library(visualbrief)

# Any export: map its columns once
camp <- vb_read_csv("campaign.csv", date = "Day", content = "Ad name",
                    audience = "Audience", impressions = "Impressions",
                    clicks = "Link clicks", spend = "Amount spent")

vb_summarise(camp, by = "channel")                                   # totals, rates, costs
vb_compare(camp, metric = "ctr", by = "content", within = "audience") # differences with intervals
vb_plot_rates(camp, metric = "ctr", by = "content", within = "audience")
vb_report(camp, "report.html", title = "Spring campaign",
          brand = vb_brand("My organisation", primary = "#1F4E5F"))
```

Try it with the built-in synthetic campaign: `vb_report(campaign_demo, "report.html")`.

## What makes it different

- **The whole workflow**, from a raw export to a report, rather than one isolated step.
- **Honest uncertainty**: Wilson intervals for rates, Newcombe intervals for differences, Holm-adjusted tests and small-sample flags, with plain-language notes that campaign data are observational.
- **Accessible by default**: a colour-blind-friendly palette that meets WCAG 2.2 contrast, and `vb_check_contrast()` for your own colours.
- **One identity everywhere**: reads `_brand.yml`, the format used by Quarto and Shiny.
- **No heavy dependencies**: base R graphics and a self-contained HTML report; Quarto is optional.

## Learn more

- [Get started](https://pablosancheznunez.com/visualbrief/articles/visualbrief.html)
- [Reference](https://pablosancheznunez.com/visualbrief/reference/)

## Citation

`citation("visualbrief")`. MIT License.
