\name{normalizedMI}
\alias{normalizedMI}

\title{
Computes the normalized mutual information
}
\description{
This uses the normalized mutual information to evaluate how well the EM algorithm performs with respect to the data provided.  
}
\usage{
normalizedMI(trueLabel, predictedLabel)
}

\arguments{
  \item{trueLabel}{
This is a vector that represents the actual cluster labels of the data
}
  \item{predictedLabel}{
This is the predicted cluster labels returned by the EM algorithm
}
}

\value{
\item{normalizedMI}{This value represents the efficiency of the algorithm. The higher the value, the greater the efficiency of the algorithm}

}


\section{Warning}{This function is dependent on the true and predicted clusters having the same levels.} 

\seealso{
\code{\link{matchCluster}}
}

\references{ 
Manning, C., Raghavan, P., Schutze, H. (2008). \emph{An Introduction to Information Retrieval}. Cambridge University Press. ISBN 0-521-86571-9
}
\examples{
\dontrun{
  true<-c("a","b","a","a","a")
   predicted<-c("a","b","b","a","a")
   normalizedMI(true,predicted)
}
}
