## Submission

This is a new submission.

## R CMD check results

Local, macOS 26.6.1, R 4.4.2, `R CMD check --as-cran`: 0 errors, 0 warnings,
3 notes.

* New submission.
* Unable to verify current time. A local network issue, not from the package.
* HTML version of manual: "<main> is not recognized". The local HTML Tidy is
  Apple's 2006 build, which predates HTML5. R-hub reported no notes on any
  platform.

The CNN models are not in the package. Users download them from Hugging Face
with download_models(), into tools::R_user_dir("AutoCDAScorer", "cache"). The
Keras tests, and the scoring step of the vignette, need a Python Keras 3.15.0+
backend and that download. They are skipped on CRAN and run in CI
(NOT_CRAN=true).

## Test environments

* GitHub Actions (R CMD check workflow): macOS, Windows and Ubuntu on R
  release; Ubuntu on R devel and R oldrel-1. All passed.
* R-hub, R-devel on linux, windows, macos and macos-arm64 (run 2026-11-23):
  Status OK on all four.
* win-builder, R-devel and R-release: submitted 2026-11-23, results emailed to
  the maintainer.
