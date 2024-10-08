# Functions ####

# Non-stantardized Student's-t
dt_ls <- function(x, df=1, mu=0, sigma=1) 1/sigma * dt((x - mu)/sigma, df)
pt_ls <- function(q, df=1, mu=0, sigma=1)  pt((q - mu)/sigma, df)
qt_ls <- function(p, df=1, mu=0, sigma=1)  qt(p, df)*sigma + mu
rt_ls <- function(n, df=1, mu=0, sigma=1)  rt(n,df)*sigma + mu

# Unit-variance Student's-t
rT4 = function(N, df = 4)
  rt(N, df = df) / sqrt(df/(df-2))

# Scaled F
df_s <- function(x, df=1, sigma=1) 1/sigma * df(x/sigma, 1, df)
pf_s <- function(q, df=1, sigma=1)  pf(q/sigma, 1, df)
qf_s <- function(p, df=1, sigma=1)  qf(p, 1, df)*sigma
rf_s <- function(n, df=1, sigma=1)  rf(n, 1, df)*sigma

calScoresBS = function(x, data) {
  # Statistics to be bootstrapped
  E = data[x,1]; uE = data[x,2]
  Z = E / uE
  RMV  = sqrt(mean(uE^2))
  RMSE = sqrt(mean(E^2))
  c(
    ZMS = mean(Z^2),
    RCE = (RMV - RMSE) / RMV
  )
}

fPredBS = function(X, statistic, cl = NULL, nBoot = 5000,
                   method = 'bca', ...) {
  cl0 = cl
  if (is.null(cl0))
    cl <- makeCluster(detectCores())
  bs = nptest::np.boot(
    x=1:NROW(X), data = X,
    statistic = statistic, R = nBoot,
    level = 0.95, method = method,
    parallel = TRUE, cl = cl,
    boot.dist = TRUE, ...)
  if(is.null(cl0))
    stopCluster(cl)

  return(bs)
}

fZetaBS = function(bs, target, Utarget=0, method = 'bca') {
  # Estimate zeta-scores
  score = bs$t0
  delta = score - target
  ## Pick half-CI closest to the target
  ## (to deal with non-symmetrical CIs)
  lim  = (delta >= 0) * bs[[method]][1,] +
    (delta <  0) * bs[[method]][2,]
  # Estimate length of half-CI, including target expanded uncertainty
  width = sqrt((score - lim)^2 + Utarget^2)
  return(
    delta/width
  )
}

RCEfun  = function(X) {
  E  = X[,1]
  uE = X[,2]
  (sqrt(mean(uE^2)) - sqrt(mean(E^2))) / sqrt(mean(uE^2))
}
ZMSfun  = function(X) {
  E  = X[,1]
  uE = X[,2]
  Z  = E / uE
  mean(Z^2)
}
Sconf0 = function( X, stat, pcVec = 0:99) {
  # Returns vector of stat for datasets ablated
  # from their firts p% elements
  M = NROW(X)
  S0 = stat(X)
  vstat = rep(0.0, length(pcVec))
  vstat[1] = S0
  for (i in 2:length(pcVec)) {
    k = pcVec[i]
    sel = 1:floor(k * M / 100)
    if (length(sel) == 0) {
      vstat[i] = NA
    } else {
      vstat[i] = stat(X[-sel,])
    }
  }
  return(vstat)
}
