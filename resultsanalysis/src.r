source('general/utils.r')

eval_oracle_single <- 
  function(results, estimator) {
    analysis_results <-
      lapply(results,
             function(x) {
               #x<-results[[1]]
               err2oracle(errhat = x$errhat,
                          err = x$err,
                          estimator = estimator)
             })
    
    analysis_results <- do.call(rbind, analysis_results)
    analysis_results <- round(analysis_results, 2)
    
    accuracy <- mean(analysis_results[,1])
    avg_loss <- mean(analysis_results[,2][analysis_results[,1] == 0])
    ovr_avg_loss <- mean(analysis_results[,2])
  
    
    results <-
      c(accuracy = accuracy,
        avg_loss = avg_loss,
        ovr_avg_loss = ovr_avg_loss)
    
    list(results_by_ds=analysis_results,
         results = results)
  }

err2oracle <- 
  function(errhat, err, estimator) {
    decision <- model_selection_process(errhat,estimator)
    actual_oracle <- oracle_selection(err)
    est_err <- get_info_on_model(err, decision$best_model)
    
    decision_acc <- decision$best_model == actual_oracle$best_model
    
    if (!decision_acc) {
      oracle_err <- actual_oracle$best_model_err
      
      p_incr <- percentual_difference(est_err,oracle_err)
    } else {
      p_incr <- 0
    }
    
    ord <- order(err)
    pred_named <- sort(t(errhat[,estimator,drop=F])[1,])
    gt_rank <- order(err[ord])
    pred_rank <- match(names(pred_named), names(err[ord]))
    
    wr=weighted.rank(pred_rank, gt_rank)
    
    
    c(
      accurate = as.integer(decision_acc),
      err_p_incr = p_incr,
      wr = wr
    )
  }


eval_oracle_iter <- 
  function(results, estimator_list) {
    
    analysis_results <- vector("list", length(results))
    for (i in 1:length(analysis_results)) {
      print(i)
      x <- results[[i]]
      
      analysis_results[[i]] <-
        err2oracle(errhat = x$errhat,
                   err = x$err,
                   estimator = estimator_list[i])
    }
    
    analysis_results <- do.call(rbind, analysis_results)
    analysis_results <- round(analysis_results, 2)
    
    analysis_results <- as.data.frame(analysis_results)
    rownames(analysis_results) <- names(results)
    
    accuracy <- mean(analysis_results[,1])
    
    avg_loss <- mean(analysis_results[,2][analysis_results[,1] == 0])
    ovr_avg_loss <- mean(analysis_results[,2])
    sdev_loss <- sd(analysis_results[,2][analysis_results[,1] == 0])
    ovr_sdev_loss <- sd(analysis_results[,2])
    
    mdn_loss <- median(analysis_results[,2][analysis_results[,1] == 0])
    ovr_mdn_loss <- median(analysis_results[,2])
    iqr_loss <- IQR(analysis_results[,2][analysis_results[,1] == 0])
    ovr_iqr_loss <- IQR(analysis_results[,2])
    
    mdn_rank <- median(analysis_results[,'wr'])
    mean_rank <- mean(analysis_results[,'wr'])
    sdev_rank <- sd(analysis_results[,'wr'])
    
    results <-
      c(accuracy = accuracy,
        avg_loss = avg_loss,
        ovr_avg_loss = ovr_avg_loss,
        sdev_loss=sdev_loss,
        ovr_sdev_loss=ovr_sdev_loss,
        mdn_loss=mdn_loss,
        ovr_mdn_loss=ovr_mdn_loss,
        iqr_loss=iqr_loss,
        ovr_iqr_loss=ovr_iqr_loss,
        mdn_rank=mdn_rank,
        mean_rank=mean_rank,
        sdev_rank=sdev_rank)
    
    list(results_by_ds=analysis_results,
         results = results)
  }


#### utils

oracle_selection <- 
  function(test_err) {
    best_model <- names(which.min(test_err))
    
    best_model_err <- test_err[[best_model]]
    
    list(
      best_model=best_model,
      best_model_err=best_model_err)
  }

model_selection_process <- 
  function(errhat, estimator) {
    id <- which.min(errhat[,estimator])
    est_best_model <- rownames(errhat)[id]
    
    est_best_model_err <- errhat[est_best_model,estimator]
    
    list(
      best_model=est_best_model,
      best_model_err=est_best_model_err)
  }

get_info_on_model <- 
  function(err, estimator) {
    err[[estimator]]
  }

guides_model_selection <- 
  function(errhat, err, stationarity) {
    best_est_model <- 
      if(stationarity) {
        "b_x_val"
      } else {
        "rephout"
      }
    
    decision <- model_selection_process(errhat,best_est_model)
    
    best_model_err <- err[[decision$best_model]]
    
    list(
      best_model=decision$best_model,
      best_model_err=best_model_err)
  }

log_trans <- function(x) sign(x) * log(abs(x) + 1)

build_boxplot <- 
  function(x, log_vals=F) {
    library(reshape2)
    library(ggplot2)
    
    xmelt <- melt(x)
    
    ord <- names(sort(apply(x,2,median)))
    
    xmelt$variable <- factor(xmelt$variable, levels = ord)
    
    if (log_vals) {
      xmelt$value <- log_trans(xmelt$value)
    }
    
    p <- ggplot(xmelt, aes(variable, value))
    
    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Percentage Loss to Oracle") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=30)) +
      theme(axis.text.y  = element_text(size = 10),
            axis.title.y = element_text(size = 10)) 
    
    p
  }
