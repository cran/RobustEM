\name{cluster_em}
\alias{cluster_em}
\title{Clustering Algorithm}
\description{
This function estimates the mixture model by one of three EM
algorithms and clusters observations to the component in which their density is the highest. 
}
\usage{
cluster_em(x, k,method=c("reg","rcm","kotz"),iter_max=100)
}
\arguments{
\item{x}{
This is a matrix or data frame of observations, where rows correspond to n observations and columns 
correspond to d variables. Categorical variables are not allowed.  
}
\item{k}{
This is an integer specifying the number of mixture components (clusters).
}

\item{method}{
This specifies which of the algorithms is to be used. Presently there are three algorithms
that can be used. These are reg(regular-EM), rcm(spatial-EM) and kotz. Regular EM uses sample mean
and sample covariance in each M step and hence is sensitive to outliers. Spatial EM utilizes median-based
location and rank-based scatter estimators, hence enhancing stability and robustness. It is robust to 
outliers and initial values. Robustness of Kotz EM is between that of other two. 
}
\item{iter_max}{
This is a parameter maxiter. It is the maximum number of iterations of the EM algorithm.
The default value is 100. If the EM algorithm has not converged at this iteration, the 
estimates for the 100th iteration are returned and a warning message is presented. 
}
}
\details{
The cluster_em function provides mixture model estimation by one of EM algorithms used and clustering of data.
Observations clustered to the component in which their density is highest. 
}
\value{The function returns a list with the following components: 
\item{clusters}{A factor consisting of cluster labels of observations.}
\item{mean}{The mean of each component. If there is more than one component, it forms a matrix 
whose kth row is the mean of kth component of the mixture model.}
\item{sigma}{A list of variance estimator for each component of the model.}
\item{taul}{A vector of the mixing proportion for each of the components.}
}
\references{
Yu, K., Dang, X., Bart Jr, H. and Chen, Y. (2015). Robust Model-based Learning via Spatial-EM Algorithm. 
\emph{IEEE Transactions on Knowledge and Data Engineering}, 27(6), 1670-1682. 
}

\examples{
x1<-matrix(rnorm(2*200),ncol=2)
x2<-matrix(rnorm(2*200,2,1),ncol=2)
x <- rbind(x1,x2)
k<-2
cluster_em(x,k,"rcm")
}
