################
# read data set
################

TAS_fuel = read.csv("daily_fuel_mix_TAS.csv")
TAS_fuel_amount = TAS_fuel[,2:5]
TAS_fuel_amount_zero_fill = matrix(NA, nrow(TAS_fuel_amount), ncol(TAS_fuel_amount))
for(iw in 1:ncol(TAS_fuel_amount))
{
    TAS_fuel_amount_zero_fill[,iw] = replace(TAS_fuel_amount[,iw], which(is.na(TAS_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
colnames(TAS_fuel_amount_zero_fill) = c(fuel_type, "Sum")

fuel_type = c("Hydro", "Natural Gas", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(apply(TAS_fuel_amount_zero_fill[,1:n_fuel_type], 1, sum), 2) == round(TAS_fuel_amount_zero_fill[,(n_fuel_type+1)], 2)) # TRUE

# set up training and testing data

n_days = nrow(TAS_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

# hierarchical time series forecasting (top-down or bottom-up)

TAS_fuel_type_bu_fore = TAS_fuel_type_tdgsa_fore = TAS_fuel_type_tdgsf_fore = TAS_fuel_type_tdfp_fore = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    hts_obj = TAS_fuel_amount_zero_fill[1:(n_train + iwk - 1),1:n_fuel_type]
    
    # BU
    
    dum = forecast(hts(hts_obj), h = 1, method = "bu", fmethod = "ets")
    TAS_fuel_type_bu_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSA
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsa", fmethod = "ets")
    TAS_fuel_type_tdgsa_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSF
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsf", fmethod = "ets")
    TAS_fuel_type_tdgsf_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDFP
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdfp", fmethod = "ets")
    TAS_fuel_type_tdfp_fore[,iwk] = dum$bts
    print(iwk); rm(iwk); rm(dum); rm(hts_obj)
}

rownames(TAS_fuel_type_bu_fore) = rownames(TAS_fuel_type_tdgsa_fore) =
rownames(TAS_fuel_type_tdgsf_fore) = rownames(TAS_fuel_type_tdfp_fore) = fuel_type

equal_weight_bu_tdfp = 0.5 * (TAS_fuel_type_bu_fore + TAS_fuel_type_tdfp_fore)

# holdout data samples

fuel_type_holdout = t(TAS_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE

mase = ftsa:::mase

TAS_hts_bu_MASE = TAS_hts_tdgsa_MASE = TAS_hts_tdgsf_MASE = TAS_hts_tdfp_MASE = equal_weight_bu_tdfp_MASE = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    TAS_hts_bu_MASE[iwk] = mase(forecast = TAS_fuel_type_bu_fore[,iwk], 
                                outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_hts_tdgsa_MASE[iwk] = mase(forecast = TAS_fuel_type_tdgsa_fore[,iwk], 
                                   outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_hts_tdgsf_MASE[iwk] = mase(forecast = TAS_fuel_type_tdgsf_fore[,iwk], 
                                   outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_hts_tdfp_MASE[iwk] = mase(forecast = TAS_fuel_type_tdfp_fore[,iwk], 
                                  outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                  insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    equal_weight_bu_tdfp_MASE[iwk] = mase(forecast = equal_weight_bu_tdfp[,iwk], 
                                          outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

TAS_hts_err_MASE = cbind(TAS_hts_bu_MASE, TAS_hts_tdgsa_MASE, TAS_hts_tdgsf_MASE, TAS_hts_tdfp_MASE, equal_weight_bu_tdfp_MASE)
colnames(TAS_hts_err_MASE) = c("BU", "TDGSA", "TDGSF", "TDFP", "equal_weight")
rownames(TAS_hts_err_MASE) = 1:n_test

table(apply(TAS_hts_err_MASE, 1, which.min)) # 141 87 104 125

round(mean(TAS_hts_bu_MASE), 4)    # 0.1657
round(mean(TAS_hts_tdgsa_MASE), 4) # 0.1657
round(mean(TAS_hts_tdgsf_MASE), 4) # 0.1659
round(mean(TAS_hts_tdfp_MASE), 4)  # 0.159
round(mean(equal_weight_bu_tdfp_MASE), 4) # 0.161

boxplot(TAS_hts_err_MASE, ylab = "MASE")
