rm(list=ls())
load("data/datasets/tsl_uni_90_mix.rdata")

library(tsensembler)

form <- target~.

IDS <- 1:174

nfolds <- 10
outer_split <- .7

source("performanceestimation/workflows.r")

results <- vector("list", length(ts_list))
for (i in IDS) {
  cat(i,"\n")
  ds <- ts_list[[i]]
  
  est <-
    workflow.get_estimations(
      ds = ds,
      form = form,
      nfolds = nfolds,
      outer_split = outer_split
    )
  
  est$data <- ds
  
  results[[i]] <- est
  
  save(results, file = "results_tsdl_nf10.rdata")
}
