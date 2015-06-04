\name{confusionMatrix}
\alias{confusionMatrix}
\title{
Confusion Matrix
}
\description{
Given the actual and predicted labels, this function computes the confusion matrix.
}
\usage{
confusionMatrix(actual, predicted)
}
\arguments{
\item{actual}{
This is a list of values corresponding to the true class of the data.
}
\item{predicted}{
This is a list of values corresponding to the predicted class of the data
using the EM algorithm.
}
}
\value{
\item{t}{This is the table representing the confusion matrix.}
}

\examples{
\dontrun{
confusionMatrix(c("A","B","A","B","B","A"),c(1,2,2,1,2,1))
}
}
