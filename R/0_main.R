figDir = '../Figs'
tabDir = '../Tabs'

doCalc = TRUE # set to TRUE for first run

library(nptest)
library(parallel)
library(ErrViewLib)
gPars = ErrViewLib::setgPars(type = 'publish')

# Load functions ####
source('functions.R')

# Load datasets ####
source('getDatasets.R')

# Model - Fig. 1 ####
M = 10000
nuT = exp(seq(log(2.1), log(100), by = 0.01))
bgmZ2 = picp67 = picp95 = lo95 = up95 = c()
for(j in seq_along(nuT)) {
  Z = rT4(M, df = nuT[j])
  bgmZ2[j]  = ErrViewLib::skewgm(Z^2)
  S = sum(abs(Z) <= 1.96)
  ci = DescTools::BinomCI(S, M, method = "wilsoncc")
  picp95[j] = ci[1]
  lo95[j] = ci[2]
  up95[j] = ci[3]
}

png(
  file = file.path(figDir, 'fig_1.png'),
  width  = 2 * gPars$reso,
  height = 1 * gPars$reso
)
par(
  mfrow = c(1,2),
  mar = gPars$mar,
  mgp = gPars$mgp,
  pty = 'm',
  tcl = gPars$tcl,
  cex = gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)
x = nuT
val = lo95 <= 0.955 & 0.945 <= up95
cols = ifelse( val, 'blue', 'red')
plot(x, picp95, pch = 16, log = 'x', type = 'p',
     col = cols,
     ylim = c(0.94,0.98),
     xlab = expression(nu[T]),
     ylab = "PICP",
     panel.first = {grid();polygon(
       c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
       c(0.945, 0.945, 0.955, 0.955),
       border = NA, col = gPars$cols_tr[1])}
)
abline(h = 0.950, lty = 2, col = gPars$cols[5])
abline(v=3, lty = 2, col = gPars$cols[5])
mtext(3, side=1, line = 0.5, at = 3, col = gPars$cols[5])
segments(x, lo95, x, up95, col = cols)
q = 1.96
sigm1 = sqrt((x-2)/x)
ptheo = pt_ls(q,x,0,sigm1) - pt_ls(-q,x,0,sigm1)
lines(x, ptheo, lty = 1, col = 5, lwd = 2*gPars$lwd)

x = bgmZ2
io = order(x)
plot(x[io], picp95[io], pch = 16, log = '', type = 'p',
     col = cols[io],
     ylim = c(0.94,0.98),
     xlab = expression(beta[GM](Z^2)),
     ylab = "PICP",
     panel.first = {grid();polygon(
       c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
       c(0.945, 0.945, 0.955, 0.955),
       border = NA, col = gPars$cols_tr[1])}
)
abline(h = 0.950, lty = 2, col = gPars$cols[5])
segments(x[io], lo95[io], x[io], up95[io], col = cols[io])
# lines(x[io], ptheo[io], lty = 1, col = 5, lwd = 3)
dev.off()

# Fig. 1a ####
x = exp(seq(log(2.1), log(100), by = 0.01))
p = 0.975
sigm1 = sqrt((x-2)/x)
qtheo = qt_ls(p,x,0,sigm1)
png(
  file = file.path(figDir, 'fig_1a.png'),
  width  = 1 * gPars$reso,
  height = 1 * gPars$reso
)
par(
  mfrow = c(1,1),
  mar = gPars$mar,
  mgp = gPars$mgp,
  pty = 'm',
  tcl = gPars$tcl,
  cex = gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)
plot(x, qtheo, log = 'x', type = 'l', lwd = 2*gPars$lwd,
     col = gPars$cols[5],
     ylim = c(1.8,2.0), xlim = c(2,110),
     xlab = expression(nu), xaxs = 'i',
     ylab = expression(k[95]),
     panel.first = grid(equilogs = FALSE)
)
abline(v = 3, lty = 3)
mtext(3, side = 1, at = 3, line = 0.25, cex = 0.75 * gPars$cex )
abline(h = 1.96, lty = 2, col = gPars$cols[1])
box()
mtext(
  text = paste0('(a)'),
  side = 3,
  adj = 1,
  cex = gPars$cex,
  line = 0.3)
dev.off()

# Fig. 1b ####
x = exp(seq(log(2.1), log(100), by = 0.01))
q = 1.96
sigm1 = sqrt((x-2)/x)
ptheo = pt_ls(q,x,0,sigm1) - pt_ls(-q,x,0,sigm1)
png(
  file = file.path(figDir, 'fig_1b.png'),
  width  = 1 * gPars$reso,
  height = 1 * gPars$reso
)
par(
  mfrow = c(1,1),
  mar = gPars$mar,
  mgp = gPars$mgp,
  pty = 'm',
  tcl = gPars$tcl,
  cex = gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)
plot(x, ptheo, log = 'x', type = 'l', lwd = 2*gPars$lwd,
     col = gPars$cols[5],
     ylim = c(0.94,0.965), xlim = c(2,110),
     xlab = expression(nu), xaxs = 'i',
     ylab = "PICP",
     panel.first = grid(equilogs = FALSE)
)
polygon(
  c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
  c(0.945, 0.945, 0.955, 0.955),
  border = NA, col = gPars$cols_tr[1])
abline(v = 3, lty = 3)
mtext(3, side = 1, at = 3, line = 0.25, cex = 0.75 * gPars$cex )
abline(h = 0.95, lty = 2, col = gPars$cols[1])
box()
mtext(
  text = paste0('(b)'),
  side = 3,
  adj = 1,
  cex = gPars$cex,
  line = 0.3)
dev.off()

# Fig. 1c ####
x = exp(seq(log(2.1), log(100), by = 0.01))

png(
  file = file.path(figDir, 'fig_1c.png'),
  width  = 1 * gPars$reso,
  height = 1 * gPars$reso
)
par(
  mfrow = c(1,1),
  mar = gPars$mar,
  mgp = gPars$mgp,
  pty = 'm',
  tcl = gPars$tcl,
  cex = gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)
q = 1; pt = 0.683
sigm1 = sqrt((x-2)/x)
ptheo = pt_ls(q,x,0,sigm1) - pt_ls(-q,x,0,sigm1)
plot(x, ptheo, log = 'x', type = 'l', lwd = 2*gPars$lwd,
     col = gPars$cols[5],
     ylim = c(0.65,1), xlim = c(2,110),
     xlab = expression(nu), xaxs = 'i',
     ylab = "PICP",
     panel.first = grid(equilogs = FALSE)
)
polygon(
  c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
  c(pt-0.005, pt-0.005, pt+0.005, pt+0.005),
  border = NA, col = gPars$cols_tr[1])
abline(v = 3, lty = 3)
mtext(3, side = 1, at = 3, line = 0.25, cex = 0.75 * gPars$cex )
abline(h = pt, lty = 2, col = gPars$cols[1])

q = 1.96; pt = 0.95
sigm1 = sqrt((x-2)/x)
ptheo = pt_ls(q,x,0,sigm1) - pt_ls(-q,x,0,sigm1)
lines(x, ptheo, lwd = 2*gPars$lwd, col = gPars$cols[5])
polygon(
  c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
  c(pt-0.005, pt-0.005, pt+0.005, pt+0.005),
  border = NA, col = gPars$cols_tr[1])
mtext(3, side = 1, at = 3, line = 0.25, cex = 0.75 * gPars$cex )
abline(h = pt, lty = 2, col = gPars$cols[1])

q = 2.58; pt = 0.99
sigm1 = sqrt((x-2)/x)
ptheo = pt_ls(q,x,0,sigm1) - pt_ls(-q,x,0,sigm1)
lines(x, ptheo, lwd = 2*gPars$lwd, col = gPars$cols[5])
polygon(
  c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
  c(pt-0.005, pt-0.005, pt+0.005, pt+0.005),
  border = NA, col = gPars$cols_tr[1])
mtext(3, side = 1, at = 3, line = 0.25, cex = 0.75 * gPars$cex )
abline(h = pt, lty = 2, col = gPars$cols[1])
box()
mtext(
  text = paste0('(c)'),
  side = 3,
  adj = 1,
  cex = gPars$cex,
  line = 0.3)
dev.off()


# Stats ####
cases = sort(
  c('debyeT_aflow','dielectric','RPV_TTS','perovskite_conductivity',
    'bandgap_expt','perovskite_Opband','piezoelectric','heusler',
    'concrete','phonon_freq','semiconductor_lvls','double_perovskite_gap',
    'perovskite_workfunction','metallicglass_Rc','metallicglass_Dmax',
    'perovskite_stability','thermal_conductivity','perovskite_Habs',
    'exfoliation_E','steel_yield','perovskite_tec',
    'diffusion','Li_conductivity','metallicglass_Rc_LLM',
    'hea_hardness','oxide_vacancy','perovskite_ASR','Mg_alloy',
    'superconductivity','thermalcond_aflow','thermalexp_aflow',
    'perovskite_formationE','elastic_tensor')
)


df = data.frame(
  Name = NA, M = NA,
  bgmuE2 = NA, bgmE2 = NA, bgmZ2 = NA,
  picp67 = NA, ci67lo = NA, ci67up = NA,
  picp95 = NA, ci95lo = NA, ci95up = NA)
for(i in seq_along(cases)) {
  case = cases[i]
  D = getCase(case); print(case)

  uE = D$uE
  E  = D$E
  Z  = E / uE
  M = length(Z)

  picp67 = mean(abs(Z) <= 1   )
  S = sum(abs(Z) <= 1)
  ci0 = DescTools::BinomCI(S, M, method = "wilsoncc")
  ci67lo = ci0[2]
  ci67up = ci0[3]
  picp95 = mean(abs(Z) <= 1.96)
  S = sum(abs(Z) <= 1.96)
  ci0 = DescTools::BinomCI(S, M, method = "wilsoncc")
  ci95lo = ci0[2]
  ci95up = ci0[3]

  df = rbind(
    df,
    c(
      Name   = case,
      M      = M,
      bgmuE2 = round(ErrViewLib::skewgm(uE^2), 3),
      bgmE2  = round(ErrViewLib::skewgm(E^2), 3),
      bgmZ2  = round(ErrViewLib::skewgm(Z^2), 3),
      picp67 = round(picp67, 3),
      ci67lo = round(ci67lo, 3),
      ci67up = round(ci67up, 3),
      picp95 = round(picp95, 3),
      ci95lo = round(ci95lo, 3),
      ci95up = round(ci95up, 3)
    )
  )
}
df = df[-1,]
rownames(df) = 1:length(cases)

sink(file = file.path(tabDir,'JAC2024ScoresPICP.tex'))
print(knitr::kable(df, 'latex'))
sink()

# Fig dist ####
parstabZ2 = matrix(NA, nrow = length(cases), ncol = 2)
colnames(parstabZ2) = c("shape","scale")
ifig = 0
nclass = 55
for(i in seq_along(cases)) {
  case = cases[i]
  D = getCase(case)

  if(i%%6 == 1) {
    try(dev.off(), silent = TRUE)
    ifig = ifig + 1; print(ifig)
    png(
      file = file.path(figDir, paste0('fig_dist_',ifig,'.png')),
      width  = 2 * gPars$reso,
      height = 3 * gPars$reso
    )
    par(
      mfrow = c(3,2),
      mar = c(gPars$mar[1:3],3),
      mgp = gPars$mgp,
      pty = 'm',
      tcl = gPars$tcl,
      cex = gPars$cex,
      cex.main = 1.5,
      lwd = gPars$lwd
    )
  }
  uE = D$uE
  E  = D$E
  Z  = E / uE
  Z2 = Z^2

  fit.t<-fitdistrplus::fitdist(
    Z2, "f_s", method = "mge",
    start = list(df = 10, sigma = 1),
    keepdata = FALSE, gof = "KS")
  bic1   = summary(fit.t)$bic
  pars   = summary(fit.t)$estimate
  shape1 = unname(pars["df"])
  scale1 = unname(pars["sigma"])
  parstabZ2[i,]=c(shape1, scale1)

  X   = log(Z2[Z2>0])
  h1  = hist(X, nclass = nclass, plot = FALSE)
  ylim = c(0,1.2*max(h1$density))
  hist(
    X, freq = FALSE, col = NULL, nclass = nclass,
    border = gPars$cols[1], yaxs = 'i',
    main = paste0('Set ',i,'; nu = ',round(shape1)),
    xlab = 'log(Z^2)',
    xlim = quantile(X, probs = c(0.01,0.99)),
    ylim = ylim
  )

  curve(
    df_s(exp(x),df = shape1,sigma = scale1)*exp(x),
    from = min(X), to = 2*max(X), lwd = 2.5*gPars$lwd,
    n= 1000, col = gPars$cols[6], add=TRUE)

  if(i==1)
    legend(
      'topleft', bty = 'n', cex=0.75,
      legend = c('Data','F fit'),
      col = gPars$cols[c(1,5)],
      lty = c(0,1), lwd = 2 * gPars$lwd,
      pch = c(22,NA), pt.bg = 'white',
      pt.lwd = 2, pt.cex = 1.5
    )
  box()
}
dev.off()

# Fig. 2 ####
png(
  file = file.path(figDir, 'fig_2.png'),
  width  = 2 * gPars$reso,
  height = 1 * gPars$reso
)
par(
  mfrow = c(1,2),
  mar = gPars$mar,
  mgp = gPars$mgp,
  pty = 'm',
  tcl = gPars$tcl,
  cex = gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)

x = as.numeric(df$bgmuE2)
setNum = 1:length(cases)
bgmZ2 = as.numeric(df$bgmZ2)
inval = bgmZ2 > 0.85


y = as.numeric(df$picp67)
ylo = as.numeric(df$ci67lo)
yup = as.numeric(df$ci67up)
val = ylo <= 0.675 & 0.665 <= yup
cols = ifelse(val, 'blue', 'red')
cols[inval] = 'gray'
ylim = c(0.6,1.0)
plot(x, y,
     pch = 16, col = cols,
     # xlab = 'Set #',
     xlab = expression(beta[GM]({u[E]}^2)),
     ylim = ylim, ylab = "PICP (67%)",
     panel.first = {grid(); abline(h = 0.67, lty = 2)},
     panel.last = box()
)
segments(x, ylo, x, yup, col = cols)
text(x, y, labels = setNum, pos = 1, cex = 0.5)

y = as.numeric(df$picp95)
ylo = as.numeric(df$ci95lo)
yup = as.numeric(df$ci95up)
val = ylo <= 0.955 & 0.945 <= yup
cols = ifelse(val, 'blue', 'red')
cols[inval] = 'gray'
ylim = c(0.9,1.0)
plot(x, y,
     pch = 16, col = cols,
     xlab = expression(beta[GM]({u[E]}^2)),
     ylim = ylim, ylab = "PICP (95%)",
     panel.first = {grid(); abline(h = 0.95, lty = 2)},
     panel.last = box()
)
segments(x, ylo, x, yup, col = cols)
text(x, y, labels = setNum, pos = 1, cex = 0.5)

dev.off()

# Compare ZMS & PICP ####
load('JAC2024Scores.Rda') # Get scores generated by 2024_RCE

bgmZ2 = as.numeric(df$bgmZ2)

x = scores[,1]
xlo = cilo[,"ZMS"]
xup = ciup[,"ZMS"]
val = xlo <= 1 & xup >= 1
cols2 = ifelse(val, 'blue', 'red')
cols2[bgmZ2 >= 0.8] = 'gray'

y = as.numeric(df$picp95)
ylo = as.numeric(df$ci95lo)
yup = as.numeric(df$ci95up)
val = ylo <= 0.955 & 0.945 <= yup
cols = ifelse(val, 'blue', 'red')
cols[bgmZ2 >= 0.85] = 'gray'


sink(file = file.path(tabDir,'contingency.tex'))
print(knitr::kable(addmargins(table(cols,cols2)), 'latex'))
sink()

# LCP; Figs 3- ####
bgmZ2 = as.numeric(df$bgmZ2)

ylo = as.numeric(df$ci95lo)
yup = as.numeric(df$ci95up)
valid = ylo <= 0.955 & 0.945 <= yup & bgmZ2 < 0.85

nBin = 20
ifig = 2
ival = 0
for(i in seq_along(cases)) {
  if(!valid[i])
    next
  case = cases[i]
  D = getCase(case); print(c(i,case))
  ival = ival +1

  if(ival%%6 == 1) {
    try(dev.off(), silent = TRUE)
    ifig = ifig + 1; print(ifig)
    png(
      file = file.path(figDir, paste0('fig_',ifig,'.png')),
      width  = 2 * gPars$reso,
      height = 3 * gPars$reso
    )
    par(
      mfrow = c(3,2),
      mar = c(gPars$mar[1:3],3),
      mgp = gPars$mgp,
      pty = 'm',
      tcl = gPars$tcl,
      cex = gPars$cex,
      cex.main = 1,
      lwd = gPars$lwd
    )
  }
  uE = D$uE
  E  = D$E
  Z  = E / uE
  M = length(Z)

  intrv = ErrViewLib::genIntervals(1:M, nBin = nBin)
  bgm = c()
  for(j in 1:intrv$nbr) {
    sel    = intrv$lwindx[j]:intrv$upindx[j]
    bgm[j] = ErrViewLib::skewgm(Z[sel]^2)
  }

  res = ErrViewLib::plotLCP(
    E, 1.96*uE,
    ordX = uE,
    intrv = intrv,
    slide = FALSE,
    logX = TRUE,
    plot = FALSE
  )

  x = res$mint
  xlim = quantile(uE, probs=c(0.001,0.999))
  lo = res$pcl; up = res$pcu
  val = lo <= 0.955 & 0.945 <= up
  cols = ifelse(val, 'blue', 'red')
  cols[bgm >= 0.85] = 'gray'
  ylim = range(c(lo,up))
  plot(x, res$pc, log = 'x',
       pch = 19,
       col = cols,
       xlim = xlim, xaxs = 'i',
       ylim = ylim,
       xlab = 'Uncertainty',
       ylab = 'PICP',
       main = paste0('Set ', i), #,'; bgmZ2 = ',bgmZ2[i]),
       panel.first = {
         grid();
         abline(h=0.95, lty=2);
         polygon(
           c(min(uE)/2, max(uE)*2, max(uE)*2, min(uE)/2),
           c(0.945, 0.945, 0.955, 0.955),
           border = NA, col = gPars$cols_tr[1])
       }
  )
  segments(
    x, res$pcl, x, res$pcu,
    col = cols
  )
  S = sum(abs(Z) <= 1.96)
  ci0 = DescTools::BinomCI(S, M, method = "wilsoncc")
  val = ci0[2] <= 0.955 & 0.945 <= ci0[3]
  segments(
    xlim[2],ci0[2],
    xlim[2],ci0[3],
    col  = ifelse(bgmZ2[i] < 0.85,
                  ifelse(val, 'blue', 'red'),
                  'gray'),
    lwd  = 6*gPars$lwd,
    lend = 1
  )
  mtext(text = c(' Mean',paste0('- ',round(ci0[1],3))),
        side = 4,
        at = c(par("usr")[4],ci0[1]),
        col  = c(1,ifelse(bgmZ2[i] <= 0.85,
                          ifelse(val, 'blue', 'red'),
                          'gray')),
        cex = 0.75* gPars$cex,
        las = 1,
        font = 2)
}
dev.off()

