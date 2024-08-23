parstabZ2 = matrix(NA, nrow = length(setList), ncol = 2)
colnames(parstabZ2) = c("shape","scale")

png(
  file = file.path(figDir, paste0('fig_fitZ2.png')),
  width  = 2*gPars$reso,
  height = 2*gPars$reso
)
par(
  mfrow = c(3, 3),
  mar = c(3,2,2,0), #gPars$mar,
  mgp = gPars$mgp,
  pty = 's',
  tcl = gPars$tcl,
  cex = 1 * gPars$cex,
  cex.main = 1,
  lwd = gPars$lwd
)
for(i in seq_along(setList)) {
  D2 = dataList[[paste0(setList[i],'_cal')]]
  E  = D2$E
  uE = D2$uE
  Z  = E/uE
  Z2 = Z^2

  X = log(Z2)
  h1 = hist(X, nclass = 25, plot = FALSE)
  ylim = c(0,1.2*max(h1$density))
  hist(
    X, freq = FALSE, col = NULL, nclass=25,
    border = gPars$cols[1], yaxs = 'i',
    main = paste0('Set ',i),
    xlab = paste0('log(Z^2) ',D2$unit), ylim = ylim
  )

  fit.t<-fitdistrplus::fitdist(
    Z2, "f_s", method = "mge",
    start = list(df = 10, sigma = 1),
    keepdata = FALSE, gof = "KS")
  bic1   = summary(fit.t)$bic
  pars   = summary(fit.t)$estimate
  shape1 = unname(pars["df"])
  scale1 = unname(pars["sigma"])
  parstabZ2[i,]=c(shape1, scale1)

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

sink(file =  file.path(tabDir,'tabParsFitZ2.tex'))
print(knitr::kable(signif(parstabZ2,3), 'latex'))
sink()

