# AutoCDAScorer (development version)

* The 16 CNN models behind `"geom_cnn"` and `"ensemble"` are no longer included
  in the package, which keeps it under CRAN's 5 MB guideline. Download them once
  from Hugging Face with `download_models()`. Each file is checked against a
  stored MD5 checksum. `predict_score()` offers the download if they are missing.
  The `"ordinal"` model and the diagnostic plots work without it.
* The example source image is stored with lossless compression (1.6 MB to
  0.6 MB, identical pixels).
* AutoCDAScorer now declares that it needs R 4.1.0 or later, which its
  dependencies already required.
* Every exported function's help page has an example. Examples that need the
  Python Keras backend are not run automatically.
* Tests that need the Keras backend are skipped when it is not available.
* The maintainer's email address is now joshuandwilliams@outlook.com.
* `citation("AutoCDAScorer")` also gives the CNN models' DOI,
  10.57967/hf/11810.
