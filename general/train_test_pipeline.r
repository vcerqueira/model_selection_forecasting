source("general/model-specs.r")
source("resultsanalysis/metrics.r")

train_test_pipe <- 
  function(train, test, form, num_cores=1) {
    specs <- MODELSPECS
    
    M <- build_base_ensemble(form, train, specs, num_cores = 1)
    
    Y_hat <- predict(M, test)
        
    Y <- get_y(test, form)
    
    err <- sapply(Y_hat, function(o) rmse(Y, o))
    
    err
  }
