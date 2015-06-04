\name{cluster_em_outlier}
\alias{cluster_em_outlier}

\title{
Clustering and Outlier Detection Algorithm
}
\description{
This function estimates parameters of mixture model, provides robust clustering and identifies outliers.  
}
\usage{
cluster_em_outlier(x, k, method=c("reg","rcm","kotz"),eps = 0.01,iter_max=100)
}
\arguments{
  \item{x}{
This is a matrix or data frame of observations, where rows correspond to observations and columns 
correspond to variables. Categorical variables are not allowed.  
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

\item{eps}{
This is a value used to determine the threshold for outlier identification. (1-eps)-quantile of Chi-square
distribution with degree freedom d is used as the threshold, where d is the dimension. 
}
\item{iter_max}{
This is the parameter maxiter. It is the maximum number of iterations of the EM algorithm.
The default value is 100. If the EM algorithm has not converged at this iteration, the 
estimates for the 100th iteration are returned and a warning message is presented.  
}
}

\details{
The cluster_em_outlier function uses the mean and covariance of each component returned by one of
the algorithms specified using the method, and computes squared Mahalanobis distances (MD)
of each observation with respect to each component. If its lowest MD is greater than the threshold, 
then the observation is identified as an outlier and grouped into the outlier cluster, otherwise, 
it is clustered to the component in which its MD is the lowest.

It is essential to use robust Spatial EM for outlier detection, otherwise model estimation 
is distorted with presence of outliers and hence the outlier detection easily suffers masking and swamping effects 
(false negative and false positive errors).  }

\value{The function returns a list with the following components: 

\item{clusters}{A factor consisting of cluster labels of observations. The cluster label 
includes a "outlier" class.}
\item{mean}{The mean of each component. If there is more than one component, it forms a matrix 
whose kth row is the mean of kth component of the mixture model.}
\item{sigma}{A list of variance estimator for each component of the model.}
\item{taul}{A vector of the mixing proportion for each of the components. The last value of the value is a estimated proportion of outliers.}
}
\references{
Yu, K., Dang, X., Bart Jr, H. and Chen, Y. (2015). Robust Model-based Learning via Spatial-EM Algorithm. 
\emph{IEEE Transactions on Knowledge and Data Engineering}, 27(6), 1670-1682. 
}

\examples{
\dontrun{
x1 <- matrix(rnorm(2*200),ncol=2)
x2 <- matrix(rnorm(2*200,2,1),ncol=2)
x3 <- matrix(c(rnorm(20,3,1),rnorm(20,-3,1)),ncol=2, byrow=FALSE)
x <- rbind(x1,x2,x3)
k<-2
cluster_em_outlier(x,k,"rcm")
}

}