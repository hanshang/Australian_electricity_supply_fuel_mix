source("~/Dropbox/Todos/Electricity_fuel_mix/code/cdf_transformation.R")
source("~/Dropbox/Todos/Electricity_fuel_mix/code/load_packages.R")

# EVR

TAS_fuel_cdf_EVR = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_cdf_EVR[,iwk] = cdf_transformation(data = TAS_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk)
}
rownames(TAS_fuel_cdf_EVR) = fuel_type
colnames(TAS_fuel_cdf_EVR) = 1:n_test

# K = 6

TAS_fuel_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_cdf_K6[,iwk] = cdf_transformation(data = TAS_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                              fh = 1, fmethod = "ets", ncomp_method = "fixed")
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(TAS_fuel_clr_fore_K6)
}
rownames(TAS_fuel_cdf_K6) = fuel_type
colnames(TAS_fuel_cdf_K6) = 1:n_test


# distribute total to fuel type mix

TAS_fuel_type_fore_mat_arima_cdf_EVR = TAS_fuel_type_fore_mat_ets_cdf_EVR = 
TAS_fuel_type_fore_mat_arima_cdf_K6 = TAS_fuel_type_fore_mat_ets_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_type_fore_mat_arima_cdf_EVR[,iwk] = TAS_fuel_cdf_EVR[,iwk] * TAS_fuel_sum_fore_arima[iwk]
    TAS_fuel_type_fore_mat_ets_cdf_EVR[,iwk]   = TAS_fuel_cdf_EVR[,iwk] * TAS_fuel_sum_fore_ets[iwk]
    
    TAS_fuel_type_fore_mat_arima_cdf_K6[,iwk] = TAS_fuel_cdf_K6[,iwk] * TAS_fuel_sum_fore_arima[iwk]
    TAS_fuel_type_fore_mat_ets_cdf_K6[,iwk]   = TAS_fuel_cdf_K6[,iwk] * TAS_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(TAS_fuel_type_fore_mat_arima_cdf_EVR) = rownames(TAS_fuel_type_fore_mat_ets_cdf_EVR) = 
rownames(TAS_fuel_type_fore_mat_arima_cdf_K6) = rownames(TAS_fuel_type_fore_mat_ets_cdf_K6) = fuel_type

colnames(TAS_fuel_type_fore_mat_arima_cdf_EVR) = colnames(TAS_fuel_type_fore_mat_ets_cdf_EVR) = 
colnames(TAS_fuel_type_fore_mat_arima_cdf_K6) = colnames(TAS_fuel_type_fore_mat_ets_cdf_K6) = 1:n_test

# MASE calculation

TAS_cdf_MASE_arima_EVR = TAS_cdf_MASE_ets_EVR = 
TAS_cdf_MASE_arima_K6 = TAS_cdf_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    TAS_cdf_MASE_arima_EVR[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_arima_cdf_EVR[,iwk], 
                                                  outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                                  insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_cdf_MASE_ets_EVR[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_ets_cdf_EVR[,iwk], 
                                                outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                                insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    TAS_cdf_MASE_arima_K6[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_arima_cdf_K6[,iwk], 
                                                 outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                                 insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_cdf_MASE_ets_K6[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_ets_cdf_K6[,iwk], 
                                                outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                                insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(TAS_cdf_MASE_arima_EVR), 4)  # 0.1602
round(mean(TAS_cdf_MASE_ets_EVR), 4)    # 0.1661

round(mean(TAS_cdf_MASE_arima_K6), 4)  # 0.3079
round(mean(TAS_cdf_MASE_ets_K6), 4)    # 0.3086

