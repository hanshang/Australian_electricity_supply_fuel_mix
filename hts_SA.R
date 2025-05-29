################
# read data set
################

SA_fuel = read.csv("daily_fuel_mix_SA.csv")
SA_fuel_amount = SA_fuel[,2:7]
SA_fuel_amount_zero_fill = matrix(NA, nrow(SA_fuel_amount), ncol(SA_fuel_amount))
for(iw in 1:ncol(SA_fuel_amount))
{
    SA_fuel_amount_zero_fill[,iw] = replace(SA_fuel_amount[,iw], which(is.na(SA_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
colnames(SA_fuel_amount_zero_fill) = colnames(SA_fuel)[2:7]

fuel_type = c("Battery.Storage", "Diesel.oil", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(apply(SA_fuel_amount_zero_fill[,1:5], 1, sum), 2) == round(SA_fuel_amount_zero_fill[,6], 2)) # TRUE

# set up training and testing data

n_days = nrow(SA_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

# hierarchical time series forecasting (top-down or bottom-up)

SA_fuel_type_bu_fore = SA_fuel_type_tdgsa_fore = SA_fuel_type_tdgsf_fore = SA_fuel_type_tdfp_fore = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    hts_obj = SA_fuel_amount_zero_fill[1:(n_train + iwk - 1),1:n_fuel_type]
    
    # BU
    
    dum = forecast(hts(hts_obj), h = 1, method = "bu", fmethod = "ets")
    SA_fuel_type_bu_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSA
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsa", fmethod = "ets")
    SA_fuel_type_tdgsa_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSF
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsf", fmethod = "ets")
    SA_fuel_type_tdgsf_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDFP
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdfp", fmethod = "ets")
    SA_fuel_type_tdfp_fore[,iwk] = dum$bts
    print(iwk); rm(iwk); rm(dum); rm(hts_obj)
}
rownames(SA_fuel_type_bu_fore) = rownames(SA_fuel_type_tdgsa_fore) =
rownames(SA_fuel_type_tdgsf_fore) = rownames(SA_fuel_type_tdfp_fore) = fuel_type

# holdout data samples

fuel_type_holdout = t(QLD_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE

mase = ftsa:::mase

SA_hts_bu_MASE = SA_hts_tdgsa_MASE = SA_hts_tdgsf_MASE = SA_hts_tdfp_MASE = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    SA_hts_bu_MASE[iwk] = mase(forecast = SA_fuel_type_bu_fore[,iwk], 
                                outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_hts_tdgsa_MASE[iwk] = mase(forecast = SA_fuel_type_tdgsa_fore[,iwk], 
                                   outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_hts_tdgsf_MASE[iwk] = mase(forecast = SA_fuel_type_tdgsf_fore[,iwk], 
                                   outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_hts_tdfp_MASE[iwk] = mase(forecast = SA_fuel_type_tdfp_fore[,iwk], 
                                  outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                  insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

SA_hts_err_MASE = cbind(SA_hts_bu_MASE, SA_hts_tdgsa_MASE, SA_hts_tdgsf_MASE, SA_hts_tdfp_MASE)
colnames(SA_hts_err_MASE) = c("BU", "TDGSA", "TDGSF", "TDFP")
rownames(SA_hts_err_MASE) = 1:n_test

table(apply(SA_hts_err_MASE, 1, which.min)) # 158 105 26 168

round(mean(SA_hts_bu_MASE), 4)    # 0.3126
round(mean(SA_hts_tdgsa_MASE), 4) # 0.4209
round(mean(SA_hts_tdgsf_MASE), 4) # 0.421
round(mean(SA_hts_tdfp_MASE), 4)  # 0.3049

boxplot(SA_hts_err_MASE, ylab = "MASE")

