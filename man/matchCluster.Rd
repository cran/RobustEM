\name{matchCluster}
\alias{matchCluster}
\title{
Correspondence between clusters
}
\description{
This function returns the best correspondence between two label vectors.
}
\usage{
matchCluster(actual, predicted)
}

\arguments{
  \item{actual}{
This is a vector that represents the actual cluster of the data
}
  \item{predicted}{
This is the predicted cluster returned by the EM algorithm
}
}

\value{

  \item{component}{This returns the corresponding cluster label with respect to the true cluster label provided}

}

\section{Warning}{To use this function, you must have a vector returned by one of the EM algorithms provided} 

\seealso{
 \code{\link{cluster_em}}
}
\examples{
  \dontrun{
   actual<-c("a","b","a","a","a")
   predicted<-c(1,2,2,1,1)
   purity(actual,predicted)
}
}

