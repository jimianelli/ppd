\name{calc_spr}
\alias{calc_spr}
\title{Calculate a spawning potential ratio}
\usage{
calc_spr(
  f,
  selectivity,
  natural_mortality,
  weight_at_age,
  maturity_at_age,
  plus_group = TRUE
)
}
\arguments{
  \item{f}{Fishing mortality scalar.}

  \item{selectivity}{Numeric vector of selectivity at age.}

  \item{natural_mortality}{Numeric vector of natural mortality at age.}

  \item{weight_at_age}{Numeric vector of weight at age.}

  \item{maturity_at_age}{Numeric vector of maturity at age.}

  \item{plus_group}{Logical; if \code{TRUE}, treats the final age as a plus group.}
}
\value{
A numeric scalar containing \code{SPR(F)}.
}
\description{
Computes spawning biomass per recruit at a candidate fishing mortality and
returns the ratio relative to the unfished state.
}
\examples{
sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
m <- rep(0.2, length(sel))
waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

calc_spr(0.2, sel, m, waa, mat)
}

