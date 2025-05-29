##############
# hts summary
##############

hts_states_MASE_arima <- cbind(NSW_hts_MASE_arima, QLD_hts_MASE_arima, SA_hts_MASE_arima, TAS_hts_MASE_arima, 
                               VIC_hts_MASE_arima)
colnames(hts_states_MASE_arima) = c("NSW", "QLD", "SA", "TAS", "VIC")
rownames(hts_states_MASE_arima) = 1:n_test

# save figures

savepdf("hts_MASE", width = 12, height = 10, toplines = 0.5)
boxplot(hts_states_MASE_arima, ylab = "MASE")
dev.off()

# ETS

states_hts_bu_MASE = cbind(NSW_hts_bu_MASE, QLD_hts_bu_MASE, SA_hts_bu_MASE, TAS_hts_bu_MASE, VIC_hts_bu_MASE)
states_hts_tdgsa_MASE = cbind(NSW_hts_tdgsa_MASE, QLD_hts_tdgsa_MASE, SA_hts_tdgsa_MASE, TAS_hts_tdgsa_MASE, VIC_hts_tdgsa_MASE)
states_hts_tdgsf_MASE = cbind(NSW_hts_tdgsf_MASE, QLD_hts_tdgsf_MASE, SA_hts_tdgsf_MASE, TAS_hts_tdgsf_MASE, VIC_hts_tdgsf_MASE)
states_hts_tdfp_MASE = cbind(NSW_hts_tdfp_MASE, QLD_hts_tdfp_MASE, SA_hts_tdfp_MASE, TAS_hts_tdfp_MASE, VIC_hts_tdfp_MASE)

colnames(states_hts_bu_MASE) = colnames(states_hts_tdgsa_MASE) = colnames(states_hts_tdgsf_MASE) = colnames(states_hts_tdfp_MASE) = c("NSW", "QLD", "SA", "TAS", "VIC")
rownames(states_hts_bu_MASE) = rownames(states_hts_tdgsa_MASE) = rownames(states_hts_tdgsf_MASE) = rownames(states_hts_tdfp_MASE) = 1:n_test

ylim_range = range(range(states_hts_bu_MASE),
                   range(states_hts_tdgsa_MASE),
                   range(states_hts_tdgsf_MASE),
                   range(states_hts_tdfp_MASE),
                   range(clr_states_MASE_ets_EVR),
                   range(cdf_states_MASE_ets_EVR))

###########
# boxplots
###########

savefig("states_hts_bu", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(states_hts_bu_MASE, ylab = "MASE", ylim = ylim_range, main = "BU")
dev.off()

savefig("states_hts_tdgsa", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(states_hts_tdgsa_MASE, ylab = "", ylim = ylim_range, main = "TDGSA")
dev.off()

savefig("states_hts_tdgsf", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(states_hts_tdgsf_MASE, ylab = "MASE", ylim = ylim_range, main = "TDGSF")
dev.off()

savefig("states_hts_tdfp", width = 12, height = 10, toplines = 0.5, type = "png")
boxplot(states_hts_tdfp_MASE, ylab = "", ylim = ylim_range, main = "TDFP")
dev.off()

