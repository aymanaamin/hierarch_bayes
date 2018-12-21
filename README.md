# Hierarchical Reconciliation with Bayesian Shrinkage
A method to identify and adjust reconciliation errors.

#### Scripts
- hts_bsr: This is a generic file that runs the BSR method on a hierarchy of your choice
and compares it to OLS, WLS and MinT.
- 0_data_import.R: This script processes the raw data files and stores various *gts* 
and *hts* objects in 'dat/'.
- 1_descriptive_plots.R: This script generates plots to describe the export data.
- 2_backtest.R: This script runs pseudo-out-of-sample forecasts for various periods,
horizons, reconciliation methods and forecasting models. Requires parallelization, is
currently optimized up to run on a cluster at ETH (https://scicomp.ethz.ch/wiki/Euler).
Output is a large tibble in wide format.
- 3_evaluation.R: This script takes the backtest results and creates plots to compare
the accuracy of different reconciliation methods.

#### Folders
dat/: contains raw data and hts/gts files.

lib/: contains functions 

- functions_import.R contains functions to process raw data.
- functions_model.R contains the functions used by the Bayesian shrinkage model.
- functions_eval.R contains functions that help with the backtest results.

tex/: LaTeX files