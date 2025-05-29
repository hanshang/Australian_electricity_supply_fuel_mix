##############
# clr summary
##############

## EVR

# ARIMA

clr_states_MASE_arima_EVR <- cbind(NSW_clr_MASE_arima, QLD_clr_MASE_arima, SA_clr_MASE_arima, TAS_clr_MASE_arima, 
                               VIC_clr_MASE_arima)

# ETS

clr_states_MASE_ets_EVR <- cbind(NSW_clr_MASE_ets, QLD_clr_MASE_ets, SA_clr_MASE_ets, TAS_clr_MASE_ets, 
                                   VIC_clr_MASE_ets)

## K = 6

# ARIMA

clr_states_MASE_arima_K6 <- cbind(NSW_clr_MASE_arima_K6, QLD_clr_MASE_arima_K6, SA_clr_MASE_arima_K6, 
                                  TAS_clr_MASE_arima_K6, VIC_clr_MASE_arima_K6)

# ETS

clr_states_MASE_ets_K6 <- cbind(NSW_clr_MASE_ets_K6, QLD_clr_MASE_ets_K6, SA_clr_MASE_ets_K6, 
                                TAS_clr_MASE_ets_K6, VIC_clr_MASE_ets_K6)


colnames(clr_states_MASE_arima_EVR) = colnames(clr_states_MASE_ets_EVR) = 
colnames(clr_states_MASE_arima_K6) = colnames(clr_states_MASE_ets_K6) = c("NSW", "QLD", "SA", "TAS", "VIC")

rownames(clr_states_MASE_arima_EVR) = rownames(clr_states_MASE_ets_EVR) = 
rownames(clr_states_MASE_arima_K6) = rownames(clr_states_MASE_ets_K6) = 1:n_test

# save figures

savefig("clr_MASE_ets_EVR", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(clr_states_MASE_ets_EVR, ylab = "MASE", ylim = ylim_range, main = "CLR")
dev.off()

round(colMeans(clr_states_MASE_arima_EVR), 4) # 0.1317 0.0909 0.3333 0.1849 0.1325 
round(colMeans(clr_states_MASE_ets_EVR), 4)   # 0.1327 0.0923 0.3402 0.1920 0.1330
round(colMeans(clr_states_MASE_arima_K6), 4)  # 0.1314 0.0851 0.3371 0.1956 0.1332
round(colMeans(clr_states_MASE_ets_K6), 4)    # 0.1323 0.0864 0.3439 0.2036 0.1336

##############
# CDF summary
##############

# EVR

cdf_states_MASE_arima_EVR <- cbind(NSW_cdf_MASE_arima_EVR, QLD_cdf_MASE_arima_EVR, 
                                   SA_cdf_MASE_arima_EVR, TAS_cdf_MASE_arima_EVR, 
                                   VIC_cdf_MASE_arima_EVR)

cdf_states_MASE_ets_EVR <- cbind(NSW_cdf_MASE_ets_EVR, QLD_cdf_MASE_ets_EVR, 
                                   SA_cdf_MASE_ets_EVR, TAS_cdf_MASE_ets_EVR, 
                                   VIC_cdf_MASE_ets_EVR)

colnames(cdf_states_MASE_arima_EVR) = colnames(cdf_states_MASE_ets_EVR) = c("NSW", "QLD", "SA", "TAS", "VIC")
rownames(cdf_states_MASE_arima_EVR) = rownames(cdf_states_MASE_ets_EVR) = 1:n_test

# K = 6

cdf_states_MASE_arima_K6 <- cbind(NSW_cdf_MASE_arima_K6, QLD_cdf_MASE_arima_K6, 
                                   SA_cdf_MASE_arima_K6, TAS_cdf_MASE_arima_K6, 
                                   VIC_cdf_MASE_arima_K6)

cdf_states_MASE_ets_K6 <- cbind(NSW_cdf_MASE_ets_K6, QLD_cdf_MASE_ets_K6, 
                                  SA_cdf_MASE_ets_K6, TAS_cdf_MASE_ets_K6, 
                                  VIC_cdf_MASE_ets_K6)

colnames(cdf_states_MASE_arima_K6) = colnames(cdf_states_MASE_ets_K6) = c("NSW", "QLD", "SA", "TAS", "VIC")
rownames(cdf_states_MASE_arima_K6) = rownames(cdf_states_MASE_ets_K6) = 1:n_test

# save figure

savefig("cdf_MASE_ets_EVR", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(cdf_states_MASE_ets_EVR, ylab = "", ylim = ylim_range, main = "CDF")
dev.off()

NSW_MASE_overall = cbind(NSW_hts_err_MASE, NSW_clr_MASE_ets, NSW_cdf_MASE_ets_EVR)
QLD_MASE_overall = cbind(QLD_hts_err_MASE, QLD_clr_MASE_ets, QLD_cdf_MASE_ets_EVR)
SA_MASE_overall = cbind(SA_hts_err_MASE,  SA_clr_MASE_ets, SA_cdf_MASE_ets_EVR)
TAS_MASE_overall = cbind(TAS_hts_err_MASE[,1:4], TAS_clr_MASE_ets, TAS_cdf_MASE_ets_EVR)
VIC_MASE_overall = cbind(VIC_hts_err_MASE, VIC_clr_MASE_ets, VIC_cdf_MASE_ets_EVR)

colnames(NSW_MASE_overall) = colnames(QLD_MASE_overall) = 
colnames(SA_MASE_overall) = colnames(TAS_MASE_overall) = 
colnames(VIC_MASE_overall) = c("BU", "TDGSA", "TDGSF", "TDFP", "CLR", "CDF")

state_MASE_overall = rbind(colMeans(NSW_MASE_overall),
                           colMeans(QLD_MASE_overall),
                           colMeans(SA_MASE_overall),
                           colMeans(TAS_MASE_overall),
                           colMeans(VIC_MASE_overall))
rownames(state_MASE_overall) = c("NSW", "QLD", "SA", "TAS", "VIC")

xtable(state_MASE_overall, digits = 4)

# tabulate the number of times one method performs the best

table(apply(NSW_MASE_overall, 1, which.min)) # 174 31 25 168 1 58
table(apply(QLD_MASE_overall, 1, which.min)) # 204 13 7 203 0 30
table(apply(SA_MASE_overall,  1, which.min)) # 127 30 4 126 55 115
table(apply(TAS_MASE_overall, 1, which.min)) # 112 55 58 119 31 82
table(apply(VIC_MASE_overall, 1, which.min)) # 157 41 33 134 54 38

xtable(rbind(table(apply(NSW_MASE_overall, 1, which.min)),
             table(apply(QLD_MASE_overall, 1, which.min)),
             table(apply(SA_MASE_overall,  1, which.min)),
             table(apply(TAS_MASE_overall, 1, which.min)),
             table(apply(VIC_MASE_overall, 1, which.min))), digits = 4)

