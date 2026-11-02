# Contributing

Thank you for helping improve visualbrief. Bug reports with a small example, column mappings for new platforms and ideas for the report are welcome.

## Running the tests

- `R CMD build . && R CMD check --as-cran visualbrief_*.tar.gz`
- or `devtools::check()`.

## Small changes (welcome as pull requests)

- Documentation, examples and the report layout.
- Compatibility fixes, as long as every test still passes unchanged.

## Changes to discuss in an issue first

- The statistical methods in `R/metrics.R` and `R/compare.R`, and the synthetic data in `data-raw/`.
- New dependencies: visualbrief deliberately depends only on base R.

Tests are never weakened or deleted to make them pass.
