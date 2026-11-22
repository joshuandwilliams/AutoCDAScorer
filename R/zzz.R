# keras3's own .onLoad declares `py_require("keras")` with no version, so a fresh
# ephemeral environment resolves to whatever pip picks. State the floor the packaged
# models need, rather than finding out at load_model() time.
# nocov start
# covr cannot count .onLoad: R runs a package's .onLoad before the "onLoad" user hook
# in which covr starts tracing (see ?userhooks).
.onLoad <- function(libname, pkgname) {
  reticulate::py_require(paste0("keras>=", KERAS_MINIMUM))
}
# nocov end
