source("~/Dropbox/Todos/Electricity_fuel_mix/code/cdf_transformation.R")
source("~/Dropbox/Todos/Electricity_fuel_mix/code/load_packages.R")

# EVR

QLD_fuel_cdf_EVR = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_cdf_EVR[,iwk] = cdf_transformation(data = QLD_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                                fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk)
}
rownames(QLD_fuel_cdf_EVR) = fuel_type
colnames(QLD_fuel_cdf_EVR) = 1:n_test

# K = 6

QLD_fuel_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_cdf_K6[,iwk] = cdf_transformation(data = QLD_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets")
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(QLD_fuel_cdf_fore_K6)
}
rownames(QLD_fuel_cdf_K6) = fuel_type
colnames(QLD_fuel_cdf_K6) = 1:n_test

####################
# MASE (proportion)
####################

# EVR

QLD_fuel_cdf_MASE_prop_EVR = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_cdf_MASE_prop_EVR[iwk] = ftsa:::mase(forecast = QLD_fuel_cdf_EVR[,iwk], 
                                                 outsampletrue = QLD_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                                 insampletrue = QLD_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

# K = 6

QLD_fuel_cdf_MASE_prop_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_cdf_MASE_prop_K6[iwk] = ftsa:::mase(forecast = QLD_fuel_cdf_K6[,iwk], 
                                          outsampletrue = QLD_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = QLD_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

round(mean(QLD_fuel_cdf_MASE_prop_EVR), 4) # 0.0654
round(mean(QLD_fuel_cdf_MASE_prop_K6), 4)  # 0.1065

################
# MASE (amount)
################

# distribute total to fuel type mix

QLD_fuel_type_fore_mat_arima_EVR = QLD_fuel_type_fore_mat_ets_EVR = 
QLD_fuel_type_fore_mat_arima_K6 = QLD_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_type_fore_mat_arima_EVR[,iwk] = QLD_fuel_cdf_EVR[,iwk] * QLD_fuel_sum_fore_arima[iwk]
    QLD_fuel_type_fore_mat_ets_EVR[,iwk]   = QLD_fuel_cdf_EVR[,iwk] * QLD_fuel_sum_fore_ets[iwk]
    
    QLD_fuel_type_fore_mat_arima_K6[,iwk] = QLD_fuel_cdf_K6[,iwk] * QLD_fuel_sum_fore_arima[iwk]
    QLD_fuel_type_fore_mat_ets_K6[,iwk]   = QLD_fuel_cdf_K6[,iwk] * QLD_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(QLD_fuel_type_fore_mat_arima_EVR) = rownames(QLD_fuel_type_fore_mat_ets_EVR) = 
rownames(QLD_fuel_type_fore_mat_arima_K6) = rownames(QLD_fuel_type_fore_mat_ets_K6) = fuel_type

colnames(QLD_fuel_type_fore_mat_arima_EVR) = colnames(QLD_fuel_type_fore_mat_ets_EVR) = 
colnames(QLD_fuel_type_fore_mat_arima_K6) = colnames(QLD_fuel_type_fore_mat_ets_K6) = 1:n_test

# MASE calculation

QLD_cdf_MASE_arima_EVR = QLD_cdf_MASE_ets_EVR = 
QLD_cdf_MASE_arima_K6 = QLD_cdf_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_cdf_MASE_arima_EVR[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_arima_EVR[,iwk], 
                                          outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_cdf_MASE_ets_EVR[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_ets_EVR[,iwk], 
                                        outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    QLD_cdf_MASE_arima_K6[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_arima_K6[,iwk], 
                                              outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                              insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
  
    QLD_cdf_MASE_ets_K6[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_ets_K6[,iwk], 
                                            outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                            insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(QLD_cdf_MASE_arima_EVR), 4)  # 0.0672
round(mean(QLD_cdf_MASE_ets_EVR), 4)    # 0.0684

round(mean(QLD_cdf_MASE_arima_K6), 4)  # 0.1087
round(mean(QLD_cdf_MASE_ets_K6), 4)    # 0.1097

