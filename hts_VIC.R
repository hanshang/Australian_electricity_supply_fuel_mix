################
# read data set
################

VIC_fuel = read.csv("daily_fuel_mix_VIC.csv")
VIC_fuel_amount = VIC_fuel[,2:8]
VIC_fuel_amount_zero_fill = matrix(NA, nrow(VIC_fuel_amount), ncol(VIC_fuel_amount))
for(iw in 1:ncol(VIC_fuel_amount))
{
    VIC_fuel_amount_zero_fill[,iw] = replace(VIC_fuel_amount[,iw], which(is.na(VIC_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
colnames(VIC_fuel_amount_zero_fill) = colnames(VIC_fuel)[2:(n_fuel_type + 2)]

fuel_type = c("Battery.Storage", "Brown.coal", "Hydro", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(apply(VIC_fuel_amount_zero_fill[,1:n_fuel_type], 1, sum), 2) == round(VIC_fuel_amount_zero_fill[,(n_fuel_type+1)], 2)) # TRUE

# set up training and testing data

n_days = nrow(VIC_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

# hierarchical time series forecasting (top-down or bottom-up)

VIC_fuel_type_bu_fore = VIC_fuel_type_tdgsa_fore = VIC_fuel_type_tdgsf_fore = VIC_fuel_type_tdfp_fore = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    hts_obj = VIC_fuel_amount_zero_fill[1:(n_train + iwk - 1),1:n_fuel_type]
    
    # BU
    
    dum = forecast(hts(hts_obj), h = 1, method = "bu", fmethod = "ets")
    VIC_fuel_type_bu_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSA
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsa", fmethod = "ets")
    VIC_fuel_type_tdgsa_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSF
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsf", fmethod = "ets")
    VIC_fuel_type_tdgsf_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDFP
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdfp", fmethod = "ets")
    VIC_fuel_type_tdfp_fore[,iwk] = dum$bts
    print(iwk); rm(iwk); rm(dum); rm(hts_obj)
}
rownames(VIC_fuel_type_bu_fore) = rownames(VIC_fuel_type_tdgsa_fore) =
rownames(VIC_fuel_type_tdgsf_fore) = rownames(VIC_fuel_type_tdfp_fore) = fuel_type

# holdout data samples

fuel_type_holdout = t(VIC_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE

mase = ftsa:::mase

VIC_hts_bu_MASE = VIC_hts_tdgsa_MASE = VIC_hts_tdgsf_MASE = VIC_hts_tdfp_MASE = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_hts_bu_MASE[iwk] = mase(forecast = VIC_fuel_type_bu_fore[,iwk], 
                                outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_hts_tdgsa_MASE[iwk] = mase(forecast = VIC_fuel_type_tdgsa_fore[,iwk], 
                                   outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_hts_tdgsf_MASE[iwk] = mase(forecast = VIC_fuel_type_tdgsf_fore[,iwk], 
                                   outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_hts_tdfp_MASE[iwk] = mase(forecast = VIC_fuel_type_tdfp_fore[,iwk], 
                                  outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                  insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

VIC_hts_err_MASE = cbind(VIC_hts_bu_MASE, VIC_hts_tdgsa_MASE, VIC_hts_tdgsf_MASE, VIC_hts_tdfp_MASE)
colnames(VIC_hts_err_MASE) = c("BU", "TDGSA", "TDGSF", "TDFP")
rownames(VIC_hts_err_MASE) = 1:n_test

table(apply(VIC_hts_err_MASE, 1, which.min)) # 175 74 61 147

round(mean(VIC_hts_bu_MASE), 4)    # 0.1061
round(mean(VIC_hts_tdgsa_MASE), 4) # 0.1378
round(mean(VIC_hts_tdgsf_MASE), 4) # 0.1369
round(mean(VIC_hts_tdfp_MASE), 4)  # 0.109   

boxplot(VIC_hts_err_MASE, ylab = "MASE")
