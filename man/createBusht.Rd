% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createOA.R
\name{createBusht}
\alias{createBusht}
\title{Create an orthogonal array using the Bush algorithm with alternate strength}
\usage{
createBusht(q, ncol, strength, bRandom = TRUE)
}
\arguments{
\item{q}{the number of symbols in the array}

\item{ncol}{number of parameters or columns}

\item{strength}{the strength of the array to be created}

\item{bRandom}{should the array be randomized}
}
\value{
an orthogonal array
}
\description{
Create an orthogonal array using the Bush algorithm with alternate strength
}
\examples{
A <- createBusht(3, 4, 2, TRUE)
B <- createBusht(3, 4, 3, FALSE)
G <- createBusht(3, 4, 4, TRUE)
}
\seealso{
Other methods to create orthogonal arrays [createBoseBush()],
[createBose()], [createAddelKemp()], [createAddelKemp3()],
[createAddelKempN()], [createBoseBushl()]
}