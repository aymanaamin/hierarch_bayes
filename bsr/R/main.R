
#' Run Hierarchichal Reconciliation with Bayesian Shrinkage
#'
#' BSR description
#' @param object gts or hts object.
#' @param h integer, forecasting horizon.
#' @param fmethod character, forecasting method to use ("arima", "ets" or "rw").
#' to shrink towards its base forecast.
#' @param burn_in integer, number of initial iterations to be discarded.
#' @param exogen named list of h-dimension vectors to replace automatic forecasts
#' @param length_sample integer, sample size from joint posterior.
#' @param shrinkage character or numeric, indicates type shrinkage prior. Defaults to null.
#' @return A list containing parameters for each horizon and gts output
#' @import hts forecast Matrix
#' @export
bsr <- function(object,
                h,
                fmethod = "arima",
                burn_in = 10,
                exogen = NULL,
                length_sample = 100,
                shrinkage = NULL){

  # Step 1: Define Summation Matrix & Parameters
  S <- hts:::SmatrixM(object$groups)
  pars <- list("length_sample" = length_sample,
               "burn_in" = burn_in,
               "fmethod" = fmethod,
               "S" = S,
               "h" = h,
               "n" = 1000,
               "m" = nrow(S),
               "q" = ncol(S),
               "shrinkage" = shrinkage)


  # Step 2: Prior Weights
  pars$weights <- define_weights(pars)


  # Step 3: Forecasting Models
  print.noquote("Forecasting...")
  forecasts_list <- create_predictions(object, fmethod, pars)

  # Step 3.1: Replace Exogenous Forecasts
  if(!is.null(exogen)){

    for(ix in names(exogen)) forecasts_list[[ix]] <- forecasts_list[[ix]] -
        matrix(rep(colMeans(forecasts_list[[ix]]),pars$n),pars$n,pars$h,byrow = T) +
        matrix(rep(exogen[[ix]],pars$n),pars$n,pars$h,byrow = T)

  }

  # Step 4: Reconciliation
  print.noquote("Reconciling...")
  results <- run_reconciliation(forecasts_list, pars)


  # Step 5: Collect Output and Parameters
  bfcasts <- ts(as.matrix(results),
                start = as.numeric(tail(time(object$bts),1)) + 1/frequency(object$bts),
                frequency = frequency(object$bts))
  colnames(bfcasts) <- colnames(object$bts)
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")
  out <- list(bts = bfcasts,
              histy = object$bts,
              labels = object$labels,
              fmethod = pars$fmethod,
              base_forecasts = forecasts_list)
  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }


  return(structure(out, class = class(object)))

}
