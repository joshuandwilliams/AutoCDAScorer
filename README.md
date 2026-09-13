---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# AutoCDAScorer

<!-- badges: start -->

[![codecov](https://codecov.io/gh/joshuandwilliams/AutoCDAScorer/graph/badge.svg?token=DVSFFFKKQ4)](https://app.codecov.io/gh/joshuandwilliams/AutoCDAScorer) [![R CMD check](https://github.com/joshuandwilliams/AutoCDAScorer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshuandwilliams/AutoCDAScorer/actions/workflows/R-CMD-check.yaml) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) ![R](https://img.shields.io/badge/R-%3E=3.5.0-1e90ff?logo=r) [![keras version](https://img.shields.io/badge/keras-%3E=3.15.0-orange)](https://keras.io/)

<!-- badges: end -->

The goal of AutoCDAScorer is to make scoring cell death areas on UV spectra agroinfiltration images faster, more consistent, and less subjective.

AutoCDAScorer uses deep learning models trained on UV spectra agroinfiltration images to automatically predict cell death severity between 0 (no cell death) and 6 (strong cell death).

## Installation

You can install the development version of AutoCDAScorer from [GitHub](https://github.com/) with:


``` r
install.packages("devtools")
devtools::install_github("joshuandwilliams/AutoCDAScorer")
```

AutoCDAScorer's image segmentation uses [EBImage](https://bioconductor.org/packages/EBImage/), which is a Bioconductor package rather than a CRAN one. `devtools::install_github()` handles this for you.

The package also uses TensorFlow, through Keras 3.15.0 or newer. Older versions of Keras cannot read the packaged models. If you already have a suitable environment you can point R at it with `reticulate::use_condaenv()`. Otherwise install one from within R:

``` r
library(AutoCDAScorer)
check_and_install_tensorflow()
```

This installation only needs to be done once. After that, TensorFlow will remain installed unless you actively remove it.

## Models available

Each model is scored once on the same held-out 308 CDAs, which no model saw during
training or selection. Human raters agree with the consensus score on 68.92% of those
CDAs, and to within one score on 92.80%.

| Model Name | Description | Exact | Within 1 |
|----|----|----|----|
| "ensemble" | The seventeen CNNs below, averaged, then averaged with the ordinal regression. The most accurate option. | 69.16% | 98.38% |
| "geom_cnn" | The single best CNN, trained on geometrically augmented crops. | 66.88% | 99.03% |
| "ordinal" | Ordinal regression on twelve hand-built colour, brightness and lesion-shape features. No neural network. | 63.31% | 96.43% |

`"ensemble"` and `"geom_cnn"` share a training set, so they share a diagnostic plot.
`"ordinal"` was fitted on a smaller set of real crops only, so it has its own.

## Usage



This is an example workflow to show loading image datasets, checking whether they are appropriate for AutoCDAScorer's models using diagnostic plots, and finally making score predictions.


``` r
library(AutoCDAScorer)
```



## Load CDA image dataset

AutoCDAScorer provides two ways of loading image datasets

Option 1: You can load an existing CDAScorer dataframe, which includes raw images and the coordinates of CDAs to be cropped.


``` r
cdascorer_output_path <- file.path(tempdir(), "cropped_images") # Where to save the crops

your_dataset <- crop_and_load_images(input_path = tmp_cdascorer_csv, output_path = cdascorer_output_path)

show_test_image(your_dataset)
```

<div class="figure">
<img src="man/figures/README-load_cdascorer-1.png" alt="plot of chunk load_cdascorer" width="20%" />
<p class="caption">plot of chunk load_cdascorer</p>
</div>

Option 2: You can load already-cropped CDA TIFF images from a directory.


``` r
image_directory <- system.file("extdata", "example_dataset", "cropped_images", package = "AutoCDAScorer", mustWork = TRUE)
your_dataset <- load_images(input_path = image_directory)

show_test_image(your_dataset)
```

<div class="figure">
<img src="man/figures/README-load_images-1.png" alt="plot of chunk load_images" width="20%" />
<p class="caption">plot of chunk load_images</p>
</div>

## Diagnostic plots

AutoCDAScorer provides three types of diagnostic plot to help you gauge whether your images fall within the variation of those seen by the model training. This is important, since models only make accurate predictions on images similar to those they have been trained on.

#### How to interpret these plots?

These diagnostic plots are based on principal components analysis (PCA). The grey cloud represents the image dataset the models were trained on. Your images are more likely to receive accurate score predictions if they fall within this region across all subplots.


``` r
rds_path <- system.file("extdata/example_dataset/training_subset/good_dataset.rds", package = "AutoCDAScorer", mustWork = TRUE)
good_dataset <- readRDS(rds_path)

convexhull_plot <- diagnostic_pca(
  "ensemble",
  good_dataset,
  num_pcs = 5,
  plot_type = "convexhull",
  output_path = NULL
)
convexhull_plot
```

<div class="figure">
<img src="man/figures/README-positive_diagnostic-1.png" alt="plot of chunk positive_diagnostic" width="100%" />
<p class="caption">plot of chunk positive_diagnostic</p>
</div>

If your images appear outside of the grey cloud (i.e. are different to those used in training), we do not recommend using AutoCDAScorer.


``` r
bad_dataset <- good_dataset
bad_dataset$images <- bad_dataset$images[,,,c(3,1,2)] # Mix up the colour channels to make a "bad" set of images

convexhull_plot <- diagnostic_pca(
  "ensemble",
  bad_dataset,
  num_pcs = 5,
  plot_type = "convexhull",
  output_path = NULL
)
convexhull_plot
```

<div class="figure">
<img src="man/figures/README-negative_diagnostic-1.png" alt="plot of chunk negative_diagnostic" width="100%" />
<p class="caption">plot of chunk negative_diagnostic</p>
</div>

Sometimes it will be unclear if your images are appropriate for use with AutoCDAScorer. Perhaps some of your images are consistently within the grey cloud whilst others aren't. You should try diagnostic plots using different models to find one whose training images best match your images.

To help you decide, each scatter plot in the top left (coordinates A, B) is paired with an importance plot in the bottom right (coordinates B, A). You should pay more attention to plots with higher importance percentages, since the points on these plots are more reflective of the true similarity between your images and the training images.


``` r
target_plot <- diagnostic_pca(
  model = "ensemble",
  your_data = your_dataset,
  num_pcs = 5,
  plot_type = "target",
  num_ellipses = 3
)
target_plot
```

<div class="figure">
<img src="man/figures/README-real_diagnostic-1.png" alt="plot of chunk real_diagnostic" width="100%" />
<p class="caption">plot of chunk real_diagnostic</p>
</div>

``` r
contour_plot <- diagnostic_pca(
  model = "ensemble",
  your_data = your_dataset,
  num_pcs = 5,
  plot_type = "density",
  num_bins = 5
)
contour_plot
```

<div class="figure">
<img src="man/figures/README-real_diagnostic-2.png" alt="plot of chunk real_diagnostic" width="100%" />
<p class="caption">plot of chunk real_diagnostic</p>
</div>

``` r
convexhull_plot <- diagnostic_pca(
  model = "ensemble",
  your_data = your_dataset,
  num_pcs = 5,
  plot_type = "convexhull"
)
convexhull_plot
```

<div class="figure">
<img src="man/figures/README-real_diagnostic-3.png" alt="plot of chunk real_diagnostic" width="100%" />
<p class="caption">plot of chunk real_diagnostic</p>
</div>

## Make score predictions

Once you've identified a model whose training images are similar to your own images, you can have that model make predictions on your images.

You can choose the results to be returned as scores between 0-6 (0 = no cell death, 6 = strong cell death) or softmax probabilities (with one column for each score 0-6).


``` r
your_predictions <- predict_score(model = "ensemble", data = your_dataset, softmax = FALSE)
```

```
#> Your images are more red than blue, so they are in RGB. Converting to BGR for the model.
```

```
#> [1] "TensorFlow and Keras 3.15.0 backend are already available."
```

``` r
table(your_predictions)
```

```
#> your_predictions
#>  0  1  3  4  5  6 
#> 52  2  3  1  2  1
```
