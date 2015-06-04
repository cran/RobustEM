\name{errorRate}
\alias{errorRate}

\title{
This function calculates the error rates for the different EM algorithms
}
\description{
This function uses different criteria to measure the accuracy of predicted labels with respect to
the true labels.
}
\usage{
errorRate(actual, predicted, beta=1)
}

\arguments{
  \item{actual}{
This is a vector that represents the actual true class labels.
}
  \item{predicted}{
This is the predicted cluster returned by the EM algorithm
}
\item{beta}{
This is a parameter of beta used for the F-measure. The default is 1 which correpondes to the balanced F-measure.
}
}

\value{

\item{FPR}{Represents the computed False Positive Rate}
\item{FNR }{Represents the computed False Negative Rate}
\item{RI}{Represents the computed Rand Index}
\item{F}{Represents the computed F-Measure}
}
\section{Warning}{This function is dependent on the true and predicted clusters having the same levels.}

\seealso{
\code{\link{matchCluster}}
}

\examples{
\dontrun{
  true<-c("a","b","a","a","a")
   predicted<-c("a","b","b","a","a")
   errorRate(true,predicted)
}
}
