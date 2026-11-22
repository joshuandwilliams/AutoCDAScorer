# A uniform leaf-coloured image has a leaf but no CDA, which exercises the branches
# taken when there is nothing to compare.
uniform_leaf <- function() {
  img <- array(0, c(20, 20, 3))
  img[, , 1] <- 0.8
  img[, , 2] <- 0.4
  img[, , 3] <- 0.3
  img
}

test_that("an image with no CDA gives zero gap and spread features", {
  f <- cda_features(uniform_leaf())
  expect_true(all(is.finite(f)))
  zero <- c("ent_gap", "green_gap", "cda_green", "cda_green_sd", "cda_bright", "frac_g_gt_r")
  expect_equal(unname(f[zero]), rep(0, 6))
})

test_that("otsu_sep handles a constant input", {
  expect_equal(otsu_sep(rep(0.5, 10)), c(thr = 0.5, sep = 0, frac_hi = 0))
})

test_that("region_entropy of an empty region is NA", {
  expect_identical(region_entropy(uniform_leaf(), matrix(FALSE, 20, 20)), NA_real_)
})
