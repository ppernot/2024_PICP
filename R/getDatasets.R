getCase <- function(case, filterZerE = FALSE) {
  uE = unlist(
    read.csv(
      file.path(
        '..','Data','JAC2024',
        case,
        'model_errors_leaveout_calibrated.csv')
    ),
    use.names = FALSE)
  E  = unlist(
    read.csv(
      file.path(
        '..','Data','JAC2024',
        case,
        'residuals_leaveout.csv')
    ),
    use.names = FALSE)

  # Clean up
  zeruE = uE <= 0
  zerE  = E == 0
  zer = zeruE
  if(filterZerE)
    zer = zer | zerE
  uE = uE[!zer]
  E  = E[!zer]

  return(
    list(
      E = E,
      uE = uE,
      zeruE = zeruE,
      zerE = zerE
    )
  )
}

# # stats ####
# cases = sort(
#   c('debyeT_aflow','dielectric','RPV_TTS','perovskite_conductivity',
#           'bandgap_expt','perovskite_Opband','piezoelectric','heusler',
#           'concrete','phonon_freq','semiconductor_lvls','double_perovskite_gap',
#           'perovskite_workfunction','metallicglass_Rc','metallicglass_Dmax',
#           'perovskite_stability','thermal_conductivity','perovskite_Habs',
#           'exfoliation_E','steel_yield','perovskite_tec',
#           'diffusion','Li_conductivity','metallicglass_Rc_LLM',
#           'hea_hardness','oxide_vacancy','perovskite_ASR','Mg_alloy',
#           'superconductivity','thermalcond_aflow','thermalexp_aflow',
#           'perovskite_formationE','elastic_tensor')
# )
# 
# 
# if(doCalc) {
#   # scores ####
#   nBoot = 5000
#   cl <- makeCluster(detectCores())
#   stats   = names(calScoresBS(2,cbind(1:2,1:2)))
# 
#   scores = bias = cilo = ciup = ciString = zmat =
#     matrix(NA, nrow = length(cases), ncol = length(stats))
# 
#   targets = c(1,0)
# 
#   for(i in seq_along(cases)) {
#     D2 = getCase(cases[i]); print(cases[i])
#     uE = D2$uE
#     E  = D2$E
#     M  = length(uE)
# 
#     # BS scores and CIs
#     bs = fPredBS(cbind(E,uE), calScoresBS,
#                  nBoot = nBoot, cl = cl)
#     scores[i,]   = bs$t0
#     bias[i,]     = bs$bias
#     cilo[i,]     = bs$bca[1,]
#     ciup[i,]     = bs$bca[2,]
#     ciString[i,] = paste0('[', signif(bs$bca[1,],3),', ',
#                           signif(bs$bca[2,],3), ']')
#     zmat[i,]     = fZetaBS(bs, targets)
#   }
#   stopCluster(cl)
#   colnames(cilo) = colnames(ciup) = colnames(scores) = stats
#   save(
#     stats, scores, bias,
#     cilo, ciup, ciString, zmat,
#     file = 'JAC2024Scores.Rda'
#   )
# } else {
#   load(file = 'JAC2024Scores.Rda')
# }
# 
# df = data.frame(
#   Name = NA,
#   M = NA, nzeruE = NA, nzerE = NA,
#   bgmuE2 = NA, bgmE2 = NA, bgmZ2 = NA,
#   zms = NA, rce = NA)
# for(i in seq_along(cases)) {
#   case = cases[i]
#   D = getCase(case); print(case)
# 
#   uE = D$uE
#   E  = D$E
#   Z  = E / uE
#   M = length(Z)
# 
#   df = rbind(
#     df,
#     c(
#       Name   = case,
#       M      = M,
#       nzeruE = sum(D$zeruE),
#       nzerE  = sum(D$zerE),
#       bgmuE2 = round(ErrViewLib::skewgm(uE^2),3),
#       bgmE2  = round(ErrViewLib::skewgm(E^2),3),
#       bgmZ2  = round(ErrViewLib::skewgm(Z^2),3),
#       zms    = round(scores[i,1],2),
#       rce    = round(scores[i,2],2)
#     )
#   )
# }
# df = df[-1,]
# rownames(df) = 1:length(cases)
# 
# sink(file = file.path(tabDir,'JAC2024Scores.tex'))
# print(knitr::kable(df, 'latex'))
# sink()
# 
