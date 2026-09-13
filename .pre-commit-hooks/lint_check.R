#!/usr/bin/env Rscript
# Lints the given R/Rmd files with lintr (using .lintr config). Reports and does not
# fail: the package predates the linter and carries a backlog of style lints. Change
# the final line to `quit(status = 1)` once that backlog is cleared.
args <- commandArgs(trailingOnly = TRUE)
n <- 0L
for (path in args) {
  lints <- lintr::lint(path)
  if (length(lints) > 0) {
    print(lints)
    n <- n + length(lints)
  }
}
if (n > 0) message(sprintf("lintr: %d lint(s), not blocking the commit", n))
quit(status = 0)
