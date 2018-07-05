#' Sample dataset
#'
#' @source Simulated
#' @format A data frame with columns:
#' \describe{
#'  \item{ID}{Patient ID}
#'  \item{x}{Risk score as an example of a marker}
#'  \item{c1}{First covariate. Binary variable.}
#'  \item{c2}{Second covariate. Continuous variable.}
#'  \item{c3}{Third covariate. Continuous variable.}
#'  \item{tx}{Binary varianble indicating treatment assignment.}
#'  \item{ln_time}{Log of follow-up}
#'  \item{events}{Number of events}
#' }
#' @examples
#' \dontrun{
#'  reg_data
#' }
"reg_data"
