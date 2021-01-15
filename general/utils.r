cv.shuffle <- function(x)
  x[sample(NROW(x)),]

resample_timeseries <-
  function (x,
            resamples,
            size_estimation,
            size_validation) {
    n <- nrow(x)
    tr_size <- as.integer(n * size_estimation)
    ts_size <- as.integer(n * size_validation)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, resamples)
    
    lapply(origins, function(o) {
      list(train = x[(o - tr_size):(o - 1),], test = x[o:(o + ts_size - 1),])
    })
  }

percentual_difference <-
  function(x, y) {
    ((x - y) / abs(y)) * 100
  }

cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

partition <- function(x, hat.ratio) {
  len <- NROW(x)
  
  if (class(x)[1] == "data.frame") {
    train <- x[seq_len(hat.ratio * len), ]
    test <-  x[-seq_len(hat.ratio * len), ]
  } else {
    train <- x[seq_len(hat.ratio * len)]
    test <-  x[-seq_len(hat.ratio * len)]
  }
  list(train = train, test = test)
}


rbind_l <- function(x)
  do.call(rbind, x)

estimate_k <-
  function(x, m.max = 20, tol = .15) {
    require(tseriesChaos)
    
    fn.out <- false.nearest(x, m.max, d = 1, t = 1)
    fn.out <- round(fn.out, 4)
    fn.out[is.na(fn.out)] <- 0
    
    fnp.tol <- fn.out["fraction", ] > tol
    fnp.tol.sum <- sum(fnp.tol)
    
    m <- ifelse(fnp.tol.sum < m.max, fnp.tol.sum + 1, m.max)
    
    m
  }


get_y <-
  function(test, form)
    model.response(model.frame(form, test, na.action = NULL))

my_embedd <-
  function(x, m, d = 1, h = 1) {
    require(tseriesChaos)
    xk <- embedd(x, m = m, d = d)
    n <- ncol(xk)
    
    if (h < 2) {
      colnames(xk) <- paste0("tm", (n - 1):0)
      colnames(xk)[n] <- "target"
    } else {
      colnames(xk)[(n - h + 1):n] <- paste0("target", 1:h)
      colnames(xk)[-c((n - h + 1):n)] <- paste0("tm", (n - h):1)
    }
    
    as.data.frame(xk)
  }


unembed_ts <-
  function(train, test, freq = 1) {
    x_nm <- "tm"
    y_nm <- "target"
    
    X_tr <- train[, grep(x_nm, colnames(train)), drop = F]
    X_ts <- test[, grep(x_nm, colnames(test)), drop = F]
    y_ts <- test[, grep(y_nm, colnames(test)), drop = F]
    
    train_ts <- c(X_tr[, 1],
                  unlist(X_ts[1, ], use.names = FALSE))
    
    test_ts <- c(unlist(y_ts[1, ], use.names = FALSE),
                 y_ts[-1, NCOL(y_ts)])
    
    
    train_ts <- ts(train_ts, frequency = freq)
    test_ts <- ts(test_ts, frequency = freq)
    
    list(train = train_ts, test = test_ts)
  }

weighted.rank <-
  function(r1, r2) {
    n <- length(r1)
    
    num <- numeric(n)
    for (i in 1:n) {
      num[i] <- ((r1[i] - r2[i]) ^ 2) * (2 * n  + 2 - r1[i] - r2[i])
    }
    
    wr <- 1 - ((6 * sum(num)) / (n ^ 4 + n ^ 3 - n ^ 2 - n))
    
    return(wr)
  }
