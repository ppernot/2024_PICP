x = exp(seq(log(2.1), log(100), by = 0.01))
sigm1 = sqrt((x-2)/x)

p  = 0.975
q  = qt_ls(p,x,0,sigm1)
q0 = qt_ls(p,Inf,0,1)
plot(x, q/q0, log = 'x',
     type = 'l', col = 2,
     ylim = c(0.9,1.1),
     panel.first = {
       grid(equilogs = FALSE);
       polygon(
         c(min(x)/2, max(x)*2, max(x)*2, min(x)/2),
         c(0.98, 0.98, 1.02, 1.02),
         border = NA, col = gPars$cols_tr[1]);
       abline(h=1,lty=2)}
)

for(p in seq(0.9,0.99, by=0.001)) {
  q  = qt_ls(p,x,0,sigm1)
  q0 = qt_ls(p,Inf,0,1)
  lines(x, q/q0)
}

