#' Calculate a spawning potential ratio
#'
#' Computes spawning biomass per recruit at a candidate fishing mortality and
#' returns the ratio relative to the unfished state.
#'
#' @param f Fishing mortality scalar.
#' @param selectivity Numeric vector of selectivity at age.
#' @param natural_mortality Numeric vector of natural mortality at age.
#' @param weight_at_age Numeric vector of weight at age.
#' @param maturity_at_age Numeric vector of maturity at age.
#' @param plus_group Logical; if `TRUE`, treats the final age as a plus group.
#'
#' @return A numeric scalar containing `SPR(F)`.
#' @export
calc_spr <- function(f,
                     selectivity,
                     natural_mortality,
                     weight_at_age,
                     maturity_at_age,
                     plus_group = TRUE) {
  validate_life_history_inputs(
    selectivity = selectivity,
    natural_mortality = natural_mortality,
    weight_at_age = weight_at_age,
    maturity_at_age = maturity_at_age
  )

  if (!is.numeric(f) || length(f) != 1L || !is.finite(f) || f < 0) {
    stop("`f` must be a single non-negative finite numeric value.", call. = FALSE)
  }

  ssbpr_fished <- calc_ssbpr(
    f = f,
    selectivity = selectivity,
    natural_mortality = natural_mortality,
    weight_at_age = weight_at_age,
    maturity_at_age = maturity_at_age,
    plus_group = plus_group
  )
  ssbpr_unfished <- calc_ssbpr(
    f = 0,
    selectivity = selectivity,
    natural_mortality = natural_mortality,
    weight_at_age = weight_at_age,
    maturity_at_age = maturity_at_age,
    plus_group = plus_group
  )

  ssbpr_fished / ssbpr_unfished
}

#' Solve for a target SPR fishing mortality
#'
#' Finds the fishing mortality that yields a specified spawning potential ratio.
#'
#' @param target Target spawning potential ratio between 0 and 1.
#' @param selectivity Numeric vector of selectivity at age.
#' @param natural_mortality Numeric vector of natural mortality at age.
#' @param weight_at_age Numeric vector of weight at age.
#' @param maturity_at_age Numeric vector of maturity at age.
#' @param interval Search interval for fishing mortality.
#' @param tol Root finding tolerance passed to [uniroot()].
#' @param plus_group Logical; if `TRUE`, treats the final age as a plus group.
#'
#' @return A numeric scalar containing the fishing mortality that matches the
#'   target SPR.
#' @export
find_f_spr_target <- function(target,
                              selectivity,
                              natural_mortality,
                              weight_at_age,
                              maturity_at_age,
                              interval = c(0, 5),
                              tol = 1e-8,
                              plus_group = TRUE) {
  validate_life_history_inputs(
    selectivity = selectivity,
    natural_mortality = natural_mortality,
    weight_at_age = weight_at_age,
    maturity_at_age = maturity_at_age
  )

  if (!is.numeric(target) || length(target) != 1L || !is.finite(target)) {
    stop("`target` must be a single finite numeric value.", call. = FALSE)
  }
  if (target <= 0 || target >= 1) {
    stop("`target` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(interval) || length(interval) != 2L || any(!is.finite(interval))) {
    stop("`interval` must contain two finite numeric values.", call. = FALSE)
  }
  if (interval[1] < 0 || interval[2] <= interval[1]) {
    stop("`interval` must be increasing and start at a non-negative value.", call. = FALSE)
  }

  objective <- function(f) {
    calc_spr(
      f = f,
      selectivity = selectivity,
      natural_mortality = natural_mortality,
      weight_at_age = weight_at_age,
      maturity_at_age = maturity_at_age,
      plus_group = plus_group
    ) - target
  }

  lower <- interval[1]
  upper <- interval[2]
  lower_value <- objective(lower)
  upper_value <- objective(upper)

  while (upper_value > 0 && upper < 1e4) {
    upper <- upper * 2
    upper_value <- objective(upper)
  }

  if (lower_value * upper_value > 0) {
    stop(
      "Could not bracket the requested target SPR. Try a wider `interval`.",
      call. = FALSE
    )
  }

  stats::uniroot(
    objective,
    interval = c(lower, upper),
    tol = tol
  )$root
}

calc_ssbpr <- function(f,
                       selectivity,
                       natural_mortality,
                       weight_at_age,
                       maturity_at_age,
                       plus_group) {
  z <- natural_mortality + f * selectivity
  n_ages <- length(z)
  survivorship <- numeric(n_ages)
  survivorship[1] <- 1

  if (n_ages > 1L) {
    for (age in 2:n_ages) {
      survivorship[age] <- survivorship[age - 1L] * exp(-z[age - 1L])
    }
  }

  if (plus_group) {
    survivorship[n_ages] <- survivorship[n_ages] / (1 - exp(-z[n_ages]))
  }

  sum(survivorship * weight_at_age * maturity_at_age)
}

validate_life_history_inputs <- function(selectivity,
                                         natural_mortality,
                                         weight_at_age,
                                         maturity_at_age) {
  inputs <- list(
    selectivity = selectivity,
    natural_mortality = natural_mortality,
    weight_at_age = weight_at_age,
    maturity_at_age = maturity_at_age
  )

  lengths <- vapply(inputs, length, integer(1))
  if (length(unique(lengths)) != 1L) {
    stop("Life-history inputs must all have the same length.", call. = FALSE)
  }

  invalid <- vapply(
    inputs,
    function(x) !is.numeric(x) || any(!is.finite(x)),
    logical(1)
  )
  if (any(invalid)) {
    stop("Life-history inputs must be finite numeric vectors.", call. = FALSE)
  }

  if (any(selectivity < 0) || any(natural_mortality < 0)) {
    stop("Selectivity and natural mortality must be non-negative.", call. = FALSE)
  }
  if (any(weight_at_age < 0) || any(maturity_at_age < 0) || any(maturity_at_age > 1)) {
    stop(
      "`weight_at_age` must be non-negative and `maturity_at_age` must be in [0, 1].",
      call. = FALSE
    )
  }

  invisible(NULL)
}
