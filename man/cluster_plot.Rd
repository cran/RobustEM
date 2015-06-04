\name{cluster_plot}
\alias{cluster_plot}

\title{
Clustering Plot
}
\description{
A graph in 2D used to visualize the results of the EM clustering algorithm.
}
\usage{
cluster_plot(x, means, covariance, clusters)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
This is the data matrix.
}
  \item{means}{
This is a matrix with rows representing the means of the mixture model. 
}
  \item{covariance}{
This is a list where each element represents the covariance of the components of the distribution.
}
  \item{clusters}{
This is a cluster label factor indicating each obaservation belonging to.
}

}
\details{
This function shows the clustering results of the EM algorithm.    Ellipses in the plot represent the estimated 95% 
probability density contours of each of the component. When the dimension is higher than 2, a pairs plot is produced. Presently, the function only works for dimensions being between 2 and 5. 
}
\value{
The value returned is the model-based clustering plot(s).
}

\seealso{
\code{\link{cluster_em}}
}
\examples{
\dontrun{
 x1 <- matrix(rnorm(2*200), ncol=2)
 x2 <- matrix(rnorm(2*200,2,1),ncol=2)
 x <- rbind(x1,x2)
 k <- 2
 cl <- cluster_em(x,k,"rcm")
 mean <- cl$mean
 covariance <- cl$sigma
 cluster_plot(x,mean,covariance)
}
}

