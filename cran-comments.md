## Submission

First submission of visualbrief.

## Test environments

* GitHub Actions: Ubuntu (R devel, release, oldrel-1), macOS (release),
  Windows (release).
* Local: Windows 11, R 4.6.2.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for CRAN

* The package depends only on base R packages; 'yaml' is suggested for
  reading brand files.
* Examples, tests and the vignette use a synthetic dataset and write only to
  `tempdir()`. No internet access is used.
