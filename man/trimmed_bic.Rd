\name{trimmed_bic}
\alias{trimmed_bic}

\title{
Determines the number of components in the distribution.
}
\description{
The function selects the number of components in the distribution using a variation of the Bayesian Information Criterion (BIC).
The trimmed BIC uses a trimmed likelihood and a complexity penalty term to optimally determine the number of
mixture components. It uses a range of values as number of components and returns the value that gives the maximum trimmed BIC.
}
\usage{
trimmed_bic(data, alpha, end, method=c("reg","rcm","kotz"),iter_max=100)
}

\arguments{
  \item{data}{
This is a matrix or data frame of observations, where rows correspond to n observations and columns 
correspond to d variables. Categorical variables are not allowed.
}
  \item{alpha}{
This is the trimming percentage in the calculation of the trimmed BIC. The alpha value ranges from 0 to 0.5. alpha = 0 corresponds
the conventional BIC. 
}
  \item{end}{
This is an integer value that represents the maximum number of components in the mixture models considered. 
The minimum number compoents is always set to be 2.  
}
  \item{method}{
This specifies which of the algorithms is to be used. Presently there are three algorithms
that can be used. These are reg(regular-EM),rcm(spatial-EM), kotz(kotz-EM).
}
\item{iter_max}{
This is a parameter maxiter. It is the maximum number of iterations of the EM algorithm.
The default value is 100. If the EM algorithm has not converged at this iteration, the 
parameters for the 100th iteration is returned.
}
}

\value{
  \item{bic}{A list containing the BIC computed in the range.}
  \item{k}{The optimal number of components selected.}
}

\references{
Schwarz, G. (1978). Estimating the dimension of a model. \emph{Annals of Statistics}, 6, 461--464. 	
}
\examples{
\dontrun{
x1 <- matrix(rnorm(2*200),ncol=2)
x2 <- matrix(rnorm(2*200,2,1),ncol=2)
data <- rbind(x1,x2)
epsilon <- 0.5
end <- 3
iter_max <- 50
trimmed_bic(data,epsilon,end,"rcm",50)
}
}
