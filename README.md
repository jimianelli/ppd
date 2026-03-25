# ppd

`ppd` is a minimal R package scaffold for posterior predictive reference point
estimation workflows. The repository is set up so age-structured model
components can be added incrementally without reworking the package basics.

## Current scope

The initial package includes:

- package metadata for installation and release
- generic spawning potential ratio helpers
- `testthat` scaffolding
- a GitHub Actions workflow for `R CMD check`

## Install

For local development:

```r
install.packages("pak")
pak::pkg_install(".")
```

Once the repository exists on GitHub, install from the remote with:

```r
install.packages("pak")
pak::pkg_install("your-github-user/ppd")
```

## Example

```r
library(ppd)

sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
m <- rep(0.2, length(sel))
waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

calc_spr(0.2, sel, m, waa, mat)
find_f_spr_target(0.35, sel, m, waa, mat)
```

## Next steps

- bring over the age-structured model interface
- add simulation and refit wrappers
- define package vignettes around the full workflow

