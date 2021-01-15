source("general/train_test_pipeline.r")
source("general/utils.r")
source("performanceestimation/estimation-procedures.r")

workflow.get_estimations <-
  function(ds, form, nfolds, outer_split) {
    require(forecast)
    
    nd <- ndiffs(ds)
    if (nd > 0) {
      ds <- diff(ds, differences = nd)
    }
    
    #
    khat <-
      estimate_k(ds[1:(length(ds) * outer_split)], m.max = 30, tol = .01)
    if (khat < 8) {
      khat <- 8
    }
    
    x <- my_embedd(as.numeric(ds), khat + 1)
    
    xp <- partition(x, outer_split)
    
    train <- xp$train
    test <- xp$test
    
    true_loss <-
      train_test_pipe(
        train = train,
        test = test,
        form = form,
        num_cores = 1
      )
    
    estimated_loss <-
      performance_estimation(
        train = train,
        form = form,
        pred_model = train_test_pipe,
        nfolds = nfolds
      )
    
    time_exec <- estimated_loss$time_execution
    estimated_loss <- estimated_loss$loss_estimations
    
    estimated_loss <- do.call(rbind, estimated_loss)
    estimated_loss <- as.data.frame(t(estimated_loss))
    
    err_estimation <- sapply(estimated_loss,
                             function(u) {
                               ((u - true_loss) / true_loss) * 100
                             })
    
    list(errhat = estimated_loss,
         err = true_loss,
         time_exec = time_exec)
  }

performance_estimation <-
  function(train, form, pred_model, nfolds) {
    cat("Estimating loss using ...\n")
    cat("... std. x val ...\n")
    t0 <- Sys.time()
    std_x_val <-
      kf_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        shuffle.rows = TRUE,
        form = form,
        average_results = TRUE
      )
    std_x_val_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... blocked x val ...\n")
    t0 <- Sys.time()
    blocked_x_val <-
      blocked_kf_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        form = form
      )
    blocked_x_val_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... modified x val ...\n")
    t0 <- Sys.time()
    mod_x_val <-
      modified_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    mod_x_val_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... hv blocked x val ...\n")
    t0 <- Sys.time()
    hvb_x_val <-
      hv.block_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    hvb_x_val_t <- difftime(Sys.time(), t0, units = 'secs')
    
    ##
    cat("... preq blocks ...\n")
    t0 <- Sys.time()
    preq_b <-
      prequential_in_blocks(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    preq_b_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... preq blocks late ...\n")
    t0 <- Sys.time()
    preq_b2 <-
      prequential_in_blocks_trim(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    preq_b2_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... sliding preq blocks ...\n")
    t0 <- Sys.time()
    sl_preq_b <-
      sliding_prequential_in_blocks(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    sl_preq_b_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... preq blocks w gap ...\n")
    t0 <- Sys.time()
    preq_b_gap <-
      prequential_in_blocks_gap(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    preq_b_gap_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... holdout ...\n")
    t0 <- Sys.time()
    hout <-
      holdout(x = train,
              FUN = pred_model,
              form = form)
    hout_t <- difftime(Sys.time(), t0, units = 'secs')
    
    cat("... repeated holdout ...\n")
    t0 <- Sys.time()
    rephout <-
      repeated_holdout(
        x = train,
        nreps = nfolds,
        train_size = .6,
        test_size = .1,
        average_results = TRUE,
        FUN = pred_model,
        form = form
      )
    rephout_t <- difftime(Sys.time(), t0, units = 'secs')
    
    
    loss_estimations <-
      list(
        x_val = std_x_val,
        b_x_val = blocked_x_val,
        m_x_val = mod_x_val,
        hvb_x_val = hvb_x_val,
        preq_b = preq_b,
        preq_b2 = preq_b2,
        sl_preq_b = sl_preq_b,
        preq_b_gap = preq_b_gap,
        hout = hout,
        rephout = rephout
      )
    
    time_execution <-
      list(
        x_val = std_x_val_t,
        b_x_val = blocked_x_val_t,
        m_x_val = mod_x_val_t,
        hvb_x_val = hvb_x_val_t,
        preq_b = preq_b_t,
        preq_b2 = preq_b2_t,
        sl_preq_b = sl_preq_b_t,
        preq_b_gap = preq_b_gap_t,
        hout = hout_t,
        rephout = rephout_t
      )
    
    list(time_execution = time_execution,
         loss_estimations = loss_estimations)
  }
