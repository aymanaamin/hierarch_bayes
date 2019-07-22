
#' Run Bayesian Hierarchichal Reconciliation
#'
#' Creates predictions and reconciles them using
#' @param object gts or hts object.
#' @param h integer, forecasting horizon.
#' @param fmethod character, forecasting method to use ("arima", "ets" or "rw").
#' @param series_to_be_shrunk vector of integers, indicating which reconciled
#' to shrink towards its base forecast.
#' @param nser_shr numerical, shrinks top level towards base forecasts.
#' @param xser_shr numerical, shrinks selected forecasts towards base forecasts.
#' @return A list containing parameters for each horizon and gts output
#' @import hts Matrix forecast coda MCMCpack MASS
#' @export
#' @examples out <- RunBSR(tradegts_red, h = 3, fmethod = "arima")
RunBSR <- function(object, h, fmethod = "arima",
                   series_to_be_shrunk = NULL,
                   shrinkage = "none", sparse = FALSE){

  start.time = Sys.time() # Tic

  # Step 1: Define Summation Matrix & Parameters
  S <- hts:::SmatrixM(object$groups)
  pars <- list("sparse" = sparse,
               "length_sample" = 1000,
               "length_max" = 1e+5,
               "fmethod" = fmethod,
               "h" = h,
               "n" = 1000,
               "m" = nrow(S),
               "q" = ncol(S),
               "shrinkage" = shrinkage,
               "xser_shr" = 1e+5,
               "series_to_be_shrunk" = series_to_be_shrunk)

  if(!is.null(series_to_be_shrunk)) if(max(series_to_be_shrunk) > pars$m){
    stop("Series to be shrunk doesn't exist.",
         call. = FALSE)
  }


  # Step 2: Run Forecasting Model
  forecasts.list <- CreatePredictions(object, fmethod, pars)

  # Step 3: Create Weighting Matrix
  pars$lambda <- DefineWeights(S,pars)

  # Step 4: Run Reconciliation
  results.list <- RunReconciliation(S, forecasts.list, pars)

  # Step 5: Collect Output and Parameters
  out <- CollectOutput(object, forecasts.list, results.list, pars)

  print(Sys.time() - start.time) # Toc

  return(out)

}
