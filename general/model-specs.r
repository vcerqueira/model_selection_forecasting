require(tsensembler)

nall_kernels <- c("rbfdot","polydot","vanilladot","laplacedot")

pars_predictors <- list(
  bm_gaussianprocess = list(kernel = nall_kernels, tol = c(.001)),
  bm_svr = list(kernel = nall_kernels, 
                C=c(1, 5), 
                epsilon=c(.1,0.01)),
  bm_ppr = list(nterms = c(2, 4),
                sm.method = c("supsmu","gcvspline")),
  bm_mars = list(degree = c(1, 3), nk = c(5, 10,20),
                 thresh=c(0.001),
                 pmethod=c("forward")),
  bm_glm = list(alpha = c(0,.25,.5,.75,1),
                family = c("gaussian")),
  bm_randomforest = list(num.trees = c(250,500),
                         mtry = c(5,10)),
  bm_ffnn = list(hidden1=c(10, 15), hidden2=c(0,5)),
  # bm_gbm = list(n.trees = c(100, 200), 
  #               interaction.depth=c(1,2), 
  #               shrinkage = c(0.001, 0.01)),
  bm_pls_pcr = list(method = c("simpls","kernelpls","svdpc")),
  bm_cubist  = list(committees= c(1,5, 10, 25)),
  bm_xgb = list()
)


base_predictors <- names(pars_predictors)

MODELSPECS <- model_specs(base_predictors,pars_predictors)
MODELSPECS


