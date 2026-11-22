## Submission

This is a new submission. It has not been submitted yet: the version is still
0.0.0.9000 and must be set to a release number first.

## R CMD check results

Local, macOS 26.6.1, R 4.4.2, `R CMD check --as-cran`: 0 errors, 0 warnings,
4 notes.

* New submission. Version contains large components (0.0.0.9000).
  Expected for an unreleased development version.
* Installed size is 9.9 MB, of which inst/extdata is 8.9 MB. These are the 16
  CNN models, the ordinal regression model, the PCA reference data and the
  example images. Still to be resolved before submission.
* Problems with news in 'NEWS.md': No news entries found. The top heading is
  "(development version)", which R does not parse as a version.
* Unable to verify current time. A local network issue, not from the package.

The Keras tests, and the scoring step of the vignette, need a Python Keras
3.15.0+ backend. They are skipped on CRAN and run in CI (NOT_CRAN=true).

## Test environments

* GitHub Actions (R CMD check workflow): macOS, Windows and Ubuntu on R
  release; Ubuntu on R devel and R oldrel-1. All passed.
* R-hub, win-builder and macOS builder: not run yet.
