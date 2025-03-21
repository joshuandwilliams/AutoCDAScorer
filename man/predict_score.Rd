% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keras_functions.R
\name{predict_score}
\alias{predict_score}
\title{Predict the score for a batch of images using a CDAScorer Keras model}
\usage{
predict_score(model = "base_cnn", data, softmax = FALSE)
}
\arguments{
\item{model}{A Keras model used for making predictions.}

\item{data}{A data list containing a 4D array of images (height, width, channels, num_images).}

\item{softmax}{A boolean to return raw softmax values (TRUE) or the predicted class index (FALSE). Default is FALSE.}
}
\value{
A vector or matrix containing either the raw softmax values or the predicted class indices.
}
\description{
This function returns either the raw softmax probabilities or the predicted class for a set of images.
}
