library(forecast)

stationarity_test_df <-
  function(x) {
    library(forecast)
    library(urca)
    
    nd <- ndiffs(x)
    if (nd > 0) {
      x <- diff(x, differences = nd)
    }
    
    x <- as.vector(x)
    
    xtest <- ur.df(x,
                   type = "none",
                   selectlags = "AIC",
                   lags = 10)
    
    tr <- unname(xtest@teststat < min(xtest@cval))[1, 1]
    
    st <- ifelse(tr, F, T)
    
    st
  }
