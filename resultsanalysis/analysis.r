load("results_tsdl_nf10.rdata")

source("resultsanalysis/src.r")
source("general/utils.r")
source("general/stationarity-src.r")

len <- sapply(results, function(x) length(x$data))
methods <- colnames(results[[1]]$errhat)


is_st_df <- sapply(results, function(x) stationarity_test_df(x$data))
is_st_method <- unname(is_st_df)

is_stationary_method <- character(length(results))
is_stationary_method[is_st_method] <- "b_x_val"
is_stationary_method[!is_st_method] <- "rephout"

finalresults <- results#[is_st_method]
stationatiry_final <- is_st_method#[is_st_method]
is_stationary_method_f <- is_stationary_method#[is_st_method]

guides <-
  eval_oracle_iter(results = finalresults,
                   estimator_list = is_stationary_method_f)

eval_oracle <-
  lapply(methods,
         function(x) {
           xx <- rep(x, times = length(finalresults))
           eval_oracle_iter(results = finalresults,
                            estimator_list = xx)
         })

names(eval_oracle) <- methods

fresults  <-
  c(eval_oracle,
    list(guides = guides))

res <- lapply(fresults, function(x) x$results)
res <- do.call(rbind, res)

rn <- rownames(res) <- 
  c("CV","CV-Bl",
    "CV-Mod","CV-hvBl",
    "Preq-Bls","Preq-Bls-Trim",
    "Preq-Sld-Bls",
    "Preq-Bls-Gap",
    "Holdout",
    "Rep-Holdout",
    "Guidelines")

colnames(res) <- c("Accuracy", "Avg. Loss", "Ovr. Avg. Loss",
                   "SD Loss", "Ovr. SD Loss",
                   "Median Loss", "Ovr. Median Loss",
                   "IQR Loss", "Ovr. IQR Loss",
                   "Median W. Rank", "Mean W. Rank",
                   "SD W. Rank"
                   )
