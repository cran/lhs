% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile_transforms.R
\name{qfactor}
\alias{qfactor}
\alias{qinteger}
\alias{qdirichlet}
\title{Quantile Transformations}
\usage{
qfactor(p, fact)

qinteger(p, a, b)

qdirichlet(X, alpha)
}
\arguments{
\item{p}{a vector of LHS samples on (0,1)}

\item{fact}{a factor or categorical variable.  Ordered and un-ordered variables are allowed.}

\item{a}{a minimum integer}

\item{b}{a maximum integer}

\item{X}{multiple columns of an LHS sample on (0,1)}

\item{alpha}{Dirichlet distribution parameters.  All \code{alpha >= 1} The marginal
mean probability of the Dirichlet distribution is given by \code{alpha[i] / sum(alpha)}}
}
\value{
the transformed column or columns
}
\description{
A collection of functions that transform the margins of a Latin hypercube
sample in multiple ways
}
\details{
\code{qdirichlet} is not an exact quantile function since the quantile of a
multivariate distribution is not unique.  \code{qdirichlet} is also not the
independent quantiles of the marginal distributions since
those quantiles do not sum to one.  \code{qdirichlet} is the quantile of the
underlying gamma functions, normalized.  This is the same procedure that
is used to generate random deviates from the Dirichlet distribution therefore
it will produce transformed Latin hypercube samples with the intended distribution.

\code{q_factor} divides the [0,1] interval into \code{nlevel(fact)} equal sections
and assigns values in those sections to the factor level.
}
\examples{
X <- randomLHS(20, 7)
Y <- as.data.frame(X)
Y[,1] <- qnorm(X[,1], 2, 0.5)
Y[,2] <- qfactor(X[,2], factor(LETTERS[c(1,3,5,7,8)]))
Y[,3] <- qinteger(X[,3], 5, 17)
Y[,4:6] <- qdirichlet(X[,4:6], c(2,3,4))
Y[,7] <- qfactor(X[,7], ordered(LETTERS[c(1,3,5,7,8)]))
}
