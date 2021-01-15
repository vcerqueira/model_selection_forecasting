#' k-fold cross validation
#' The standard cross validation procedure.
#'
#' @param x data: embedded time series
#' @param nfolds no of folds
#' @param FUN function to apply to each iteration's train and test. Typically
#' \strong{FUN} is a workflow where a predictive model is applied in a training set
#' and the model is evaluated in the test set.
#' @param ... further parameters to \code{FUN}
#'
#' @export
kf_xval <- function(x, nfolds, FUN, shuffle.rows = TRUE, average_results = TRUE, ...) {
  if (shuffle.rows) x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list() 
  for (i in seq_len(nfolds)) {
    cat("X VAL iter no. ", i, "\n")
    ts.id <- which(f == i)

    train <- x[-ts.id, ]
    #print(dim(train))
    test  <- x[ ts.id, ]
    #print(dim(test))
    cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
  }
  
  if(average_results) {
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}

#' Blocked k-fold cross validation
#' Standard k-fold cross validation without reshuffling rows
#'
#' @inheritParams kf_xval
#'
#' @export
blocked_kf_xval <- function(x, nfolds, FUN, ...) {
  kf_xval(x = x, nfolds = nfolds, FUN = FUN, shuffle.rows = FALSE, ...)
}

#' Modified k-fold Cross Validation
#' Standard CV, but removes rows from training that are
#' dependent with the test set.
#'
#' @inheritParams kf_xval
#'
#' @export
modified_xval <- function(x, nfolds, FUN, average_results = TRUE, ...) {
  K <- floor(sqrt(ncol(x)))
  x$aux <- seq_len(nrow(x))
  x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  for (i in seq_len(nfolds)) {
    ts.id <- which(f == i)
    test  <- x[ts.id, ]

    depRows <- unique(unlist(lapply(test$aux, function(z) (z-K-1):(z+K-1L))))
    depRows <- depRows[depRows > 0]

    train <- x[-c(ts.id, depRows), ]
    if (nrow(train) < 1) {
      cv.res[[i]] <- NA#rep(NA_real_, times = 3)
    } else {
      test$aux <- NULL
      train$aux <- NULL

      cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
    }
  }
  
  if(average_results) {
    #cv.res <- mean(unlist(cv.res), na.rm = TRUE)
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}

#' hv - block Cross Validation
#' Cross without reshuffling rows and removing dependent rows.
#' Since there is no reshuffling, the dependent rows are just
#' the \code{K}-adjacent ones in the adjacent folds. \code{K} denotes
#' the embedding dimension.
#'
#' @inheritParams kf_xval
#'
#' @export
hv.block_xval <- function(x, nfolds, FUN, average_results, ...) {
  K <- ncol(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds)
  lseq <- seq.[length(seq.)]
  for (i in seq.) {
    ts.id <- which(f == i)
    # upper cut
    kcuts <- integer(0L)
    if (i + 1L <= lseq) {
      upper.fold <- which(f == i + 1L)
      upper.cut <- upper.fold[1:K]
      if (any(is.na(upper.cut))) {
        upper.cut <- upper.cut[!is.na(upper.cut)]
      }
      
      kcuts <- c(kcuts, upper.cut)
    }
    # lower cut
    if (i - 1L >= 1L) {
      lower.fold <- which(f == i - 1L)
      len.lf <- length(lower.fold)
      idx <- (len.lf - K + 1L):len.lf
      idx <- idx[idx > 0]
      lower.cut <- lower.fold[idx]
      kcuts <- c(kcuts, lower.cut)
    }

    train <- x[-c(ts.id, kcuts), ]
    test  <- x[ts.id, ]

    cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
  }
  
  if (average_results) {
    #cv.res <- mean(unlist(cv.res), na.rm = TRUE)
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}

#' Sequential Block Evaluation
#' Trains in the the up-to-i folds and tests on i+1
#' 
#' Prequential in blocks in a growing window fashion.
#'
#' @inheritParams kf_xval
#'
#' @export
prequential_in_blocks <- function(x, nfolds, FUN, average_results, ...) {
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds)
  
  for (i in seq.[-length(seq.)]) {
    tr.id <- which(f %in% seq_len(i))
    ts.id <- which(f == i + 1L)

    train <- x[tr.id, ]
    test  <- x[ts.id, ]
    cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
  }
  
  if (average_results) {
    #cv.res <- mean(unlist(cv.res), na.rm = TRUE) 
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}

#' Sequential Block Evaluation Trimmed
#' Trains in the the up-to-i folds and tests on i+1
#' 
#' Prequential in blocks in a growing window fashion.
#'
#' @inheritParams kf_xval
#'
#' @export
prequential_in_blocks_trim <- function(x, nfolds, FUN, average_results, ...) {
  f <- cv.folds(x, nfolds)
  
  cv.res <- list()
  seq. <- seq_len(nfolds)
  
  seq. <- tail(seq., ceiling(0.6 * nfolds))
  
  for (i in seq.[-length(seq.)]) {
    tr.id <- which(f %in% seq_len(i))
    ts.id <- which(f == i + 1L)
    
    train <- x[tr.id, ]
    test  <- x[ts.id, ]
    cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
  }
  
  if (average_results) {
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}

sliding_prequential_in_blocks <-
  function(x, nfolds, FUN, average_results, ...) {
    nfolds0<-10
    f <- cv.folds(x, nfolds0)
    
    cv.res <- list()
    seq. <- seq_len(nfolds0)
    for (i in seq.[-length(seq.)]) {
      tr.id <- which(f == i)#which(f %in% seq_len(i))
      ts.id <- which(f == i + 1L)
      
      train <- x[tr.id,]
      test  <- x[ts.id,]
      cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
    }
    
    if (average_results) {
      #cv.res <- mean(unlist(cv.res), na.rm = TRUE)
      cv.res <- colMeans(do.call(rbind, cv.res))
    }
    
    cv.res
  }

#' Sequential Block Evaluation
#' Trains in the the up-to-i folds and tests on i+2
#' Doesn't test on i+1 to further increase independence
#'
#' @inheritParams kf_xval
#'
#' @export
prequential_in_blocks_gap <- function(x, nfolds, FUN, average_results, ...) {
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds); len <- length(seq.)

  seq. <- seq.[-c(len:(len - 1L))]
  for (i in seq.) {
    tr.id <- which(f %in% seq_len(i))
    ts.id <- which(f == i + 2L)

    train <- x[tr.id, ]
    test  <- x[ts.id, ]
    cv.res[[i]] <- FUN(train=train, test=test, form=target ~.)
  }
  
  if (average_results) {
    #cv.res <- mean(unlist(cv.res), na.rm = TRUE)
    cv.res <- colMeans(do.call(rbind, cv.res))
  }
  
  cv.res
}


holdout <- 
  function(x, FUN, ...) {
    xp <- partition(x, .8)
    
    train <- xp$train
    test <- xp$test
    
    FUN(train=train, test=test, form=target ~.)
  }

repeated_holdout <- 
  function(x, nreps, train_size, test_size, FUN, average_results, ...) {
    n <- nrow(x)
    tr_size <- as.integer(n * train_size)
    ts_size <- as.integer(n * test_size)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, nreps)
    
    mcapprox <-
      lapply(origins, function(o) {
        train <- x[(o - tr_size):(o - 1),]
        test <- x[o:(o + ts_size - 1),]
        
        FUN(train = train, test = test, form=target~.)
      })
    
    if (average_results) {
      #mcapprox<-mean(unlist(mcapprox), na.rm = TRUE)
      mcapprox <- colMeans(do.call(rbind, mcapprox))
    }
    
    mcapprox
  }

