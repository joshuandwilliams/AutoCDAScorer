# keras3's own .onLoad declares `py_require("keras")` with no version, so a fresh
# ephemeral environment resolves to whatever pip picks. State the floor the packaged
# models need, rather than finding out at load_model() time.
.onLoad <- function(libname, pkgname) {
  reticulate::py_require(paste0("keras>=", KERAS_MINIMUM))
}
