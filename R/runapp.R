utils::globalVariables(c("X_mean", "X_sd", "X_distro", "X_lower", "X_mean", "X_sd", "X_upper"))

#' Run Flamingo app
#'
#' This function is a wrapup to run the Flamingo shinyapp.
#'    It takes no arguments.
#'
#' @importFrom shiny runApp
#' @importFrom utils browseURL
#'
#' @examples
#' \dontrun{
#' Flamingo::run()
#' }
#' @export
run <- function() {
  browseURL("blank")
  path <- path.package("Flamingo")
  full_path <- paste0(path, "/shiny")
  shiny::runApp(full_path)
  }


#' Generate Random Distribution
#'
#' @param n number of realizations
#' @param mean mean
#' @param sd standard deviation
#' @param method sampling method, either "RND" or "LHS". Default = "RND"
#' @param lhs vector of quatiles (obtained from LH sampling functions). Not required
#'     is \code{method = "RND"}. Default = NULL
#' @param distro distribution
#' @param lower lower limit of the truncated distribution
#' @param upper upper limit of the truncated distribution
#' @param seed integer serving as seed number
#'
#' @importFrom truncdist rtrunc qtrunc
#' @importFrom stats rlnorm rnorm runif qnorm qlnorm qunif
#' @importFrom mc2d rtriang qtriang
#'
#' @return an array with n random values for the given parameters
#'
#' @examples
#' \dontrun{
#' distribution_1 <- run_variable(100, 10, 4)
#' hist(distribution_1)
#'
#' distribution_2 <- run_variable(100, 10, 4, "truncated normal", 7, 13, seed = 123)
#' hist(distribution_2)
#' }
#'
#' @export
run_variable <- function(n, mean, sd , method = "RND", lhs = NULL,
                         distro = "normal", lower = -Inf, upper = Inf,
                         seed = 1234) {
  set.seed <- seed
  X <- seq(mean, mean, length.out = n) # constant value
  if (method == "RND") {
    if (distro == "normal") X <- rnorm(n, mean, sd)
    else if (distro == "truncated normal") X <- truncdist::rtrunc(spec = "norm", n, linf = lower, lsup = upper, mean = mean, sd = sd)
    else if (distro == "lognormal") X <- rlnorm(n, log(mean), sd)
    else if (distro == "truncated lognormal") X <- truncdist::rtrunc(spec = "lnorm", n, linf = lower, lsup = upper, mean = log(mean), sd = sd)
    else if (distro == "uniform") X <- runif(n, min = lower, max = upper)
    else if (distro == "triangular") X <- mc2d::rtriang(n, min = lower, mode = mean, max = upper)
  }
  else if (method == "LHS") {
    if (distro == "normal") X <- qnorm(lhs, mean, sd)
    else if (distro == "truncated normal") X <- truncdist::qtrunc(lhs, spec = "norm", mean = mean, sd = sd, a = lower, b = upper)
    else if (distro == "lognormal") X <- qlnorm(lhs, log(mean), sd)
    else if (distro == "truncated lognormal") X <- truncdist::qtrunc(lhs, spec = "lnorm", mean = mean, sd = sd, a = lower, b = upper)
    else if (distro == "uniform") X <- qunif(lhs, min = lower, max = upper)
    else if (distro == "triangular") X <- mc2d::qtriang(lhs, min = lower, mode = mean, max = upper)
  }

  return(X)
}

