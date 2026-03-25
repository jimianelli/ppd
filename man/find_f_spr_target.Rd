\name{find_f_spr_target}
\alias{find_f_spr_target}
\title{Solve for a target SPR fishing mortality}
\usage{
find_f_spr_target(
  target,
  selectivity,
  natural_mortality,
  weight_at_age,
  maturity_at_age,
  interval = c(0, 5),
  tol = 1e-08,
  plus_group = TRUE
)
}
\arguments{
  \item{target}{Target spawning potential ratio between 0 and 1.}

  \item{selectivity}{Numeric vector of selectivity at age.}

  \item{natural_mortality}{Numeric vector of natural mortality at age.}

  \item{weight_at_age}{Numeric vector of weight at age.}

  \item{maturity_at_age}{Numeric vector of maturity at age.}

  \item{interval}{Search interval for fishing mortality.}

  \item{tol}{Root finding tolerance passed to \code{\link[stats:uniroot]{uniroot()}}.}

  \item{plus_group}{Logical; if \code{TRUE}, treats the final age as a plus group.}
}
\value{
A numeric scalar containing the fishing mortality that matches the target SPR.
}
\description{
Finds the fishing mortality that yields a specified spawning potential ratio.
}
\examples{
sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
m <- rep(0.2, length(sel))
waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

find_f_spr_target(0.35, sel, m, waa, mat)
}
