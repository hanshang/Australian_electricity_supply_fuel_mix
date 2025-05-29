######
# CDF
######

# data: n by p data matrix
# fh: forecast horizon
# fmethod: univariate time series method
# ncomp_method: EVR or K = 6

cdf_transformation <- function(data, fh, fmethod, ncomp_method)
{
    n_age = ncol(data)
    data_cumsum_dum = matrix(NA, nrow(data), ncol(data))
    for(ij in 1:nrow(data))
    {
        data_cumsum_dum[ij,] = cumsum(data[ij,])
        rm(ij)
    }
    
    # check if any cumsum values equal to 0
    
    if(any(data_cumsum_dum == 0))
    {
        data_cumsum = replace(data_cumsum_dum, which(data_cumsum_dum == 0), 10^-5)
    }
    else
    {
        data_cumsum = data_cumsum_dum
    }
    rm(data_cumsum_dum)
    
    # logit transformation
    
    data_cumsum_logit = matrix(NA, nrow(data), (ncol(data) - 1))
    for(ij in 1:nrow(data))
    {
        data_cumsum_logit[ij,] = logit(data_cumsum[ij, 1:(ncol(data) - 1)])
        rm(ij)
    }
    rm(data_cumsum)
    
    # fitting a functional time series forecasting method
    
    if(ncomp_method == "EVR")
    {
        ncomp = select_K(tau = 10^-3, eigenvalue = (svd(data_cumsum_logit)$d)^2)
    }
    else if(ncomp_method == "provide")
    {
        ncomp = 6
    }
    data_cumsum_logit_fore = forecast(ftsm(fts(1:(n_age - 1), t(data_cumsum_logit)), order = ncomp), 
                                      h = fh, method = fmethod)
    
    data_cumsum_logit_fore_add = c(invlogit(data_cumsum_logit_fore$mean$y[,fh]), 1)
    data_cumsum_logit_fore_add_diff = c(data_cumsum_logit_fore_add[1], diff(data_cumsum_logit_fore_add))
    return(data_cumsum_logit_fore_add_diff)
}

