source("~/Dropbox/Todos/Electricity_fuel_mix/code/cdf_transformation.R")
source("~/Dropbox/Todos/Electricity_fuel_mix/code/load_packages.R")

# EVR

SA_fuel_cdf_EVR = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_cdf_EVR[,iwk] = cdf_transformation(data = SA_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk)
}
rownames(SA_fuel_cdf_EVR) = fuel_type
colnames(SA_fuel_cdf_EVR) = 1:n_test

# K = 6

SA_fuel_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_cdf_K6[,iwk] = cdf_transformation(data = SA_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(SA_fuel_clr_fore_K6)
}
rownames(SA_fuel_cdf_K6) = fuel_type
colnames(SA_fuel_cdf_K6) = 1:n_test


# forecast total supply

SA_fuel_sum_fore_arima = SA_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    SA_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(SA_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    SA_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(SA_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

# distribute total to fuel type mix

SA_fuel_type_fore_mat_arima_cdf_EVR = SA_fuel_type_fore_mat_ets_cdf_EVR = 
SA_fuel_type_fore_mat_arima_cdf_K6 = SA_fuel_type_fore_mat_ets_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_type_fore_mat_arima_cdf_EVR[,iwk] = SA_fuel_cdf_EVR[,iwk] * SA_fuel_sum_fore_arima[iwk]
    SA_fuel_type_fore_mat_ets_cdf_EVR[,iwk]   = SA_fuel_cdf_EVR[,iwk] * SA_fuel_sum_fore_ets[iwk]
  
    SA_fuel_type_fore_mat_arima_cdf_K6[,iwk] = SA_fuel_cdf_K6[,iwk] * SA_fuel_sum_fore_arima[iwk]
    SA_fuel_type_fore_mat_ets_cdf_K6[,iwk]   = SA_fuel_cdf_K6[,iwk] * SA_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(SA_fuel_type_fore_mat_arima_cdf_EVR) = rownames(SA_fuel_type_fore_mat_ets_cdf_EVR) = 
rownames(SA_fuel_type_fore_mat_arima_cdf_K6) = rownames(SA_fuel_type_fore_mat_ets_cdf_K6) = fuel_type

colnames(SA_fuel_type_fore_mat_arima_cdf_EVR) = colnames(SA_fuel_type_fore_mat_ets_cdf_EVR) = 
colnames(SA_fuel_type_fore_mat_arima_cdf_K6) = colnames(SA_fuel_type_fore_mat_ets_cdf_K6) = 1:n_test

# MASE calculation

SA_cdf_MASE_arima_EVR = SA_cdf_MASE_ets_EVR = 
SA_cdf_MASE_arima_K6 = SA_cdf_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    SA_cdf_MASE_arima_EVR[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_arima_cdf_EVR[,iwk], 
                                         outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                         insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_cdf_MASE_ets_EVR[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_ets_cdf_EVR[,iwk], 
                                       outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    SA_cdf_MASE_arima_K6[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_arima_cdf_K6[,iwk], 
                                             outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                             insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_cdf_MASE_ets_K6[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_ets_cdf_K6[,iwk], 
                                           outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                           insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(SA_cdf_MASE_arima_EVR), 4)  # 0.3919
round(mean(SA_cdf_MASE_ets_EVR), 4)    # 0.3975

round(mean(SA_cdf_MASE_arima_K6), 4)  # 0.4489
round(mean(SA_cdf_MASE_ets_K6), 4)    # 0.4591

