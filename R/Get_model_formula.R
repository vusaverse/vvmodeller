#' Get model formula
#'
#' Creates a formula to use in a model.
#'
#' @param dependent Specify dependent value.
#' @param independent Specify independent value.
#'
#' @return Formula for the model.
#' @export
get_model_formula <- function(dependent, independent){

  return_var <- stats::as.formula(paste(dependent," ~ ", paste(independent, collapse = " + ")))

  return(return_var)
}