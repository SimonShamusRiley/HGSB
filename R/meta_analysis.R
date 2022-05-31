#' #' @title Meta analysis
#' #'
#' #' @description Calculates, among others, estimates and standard errors for different
#' #'   types of meta analysis. The code is an adaptation of the mainverse and marandom SAS
#' #'   macros by Jim Weir & Stephen Senn.
#' #'
#' #' @param effect_size A numeric vector of effect sizes. Must be the same length
#' #'   as `std_err` and may not contain any missing values.
#' #' @param std_err 	A numeric vector of standard errors, Must be the same length
#' #'   as `effect_size`
#' #' @param method The method to for calculating estimates and standard errors. Defaults to
#' #'   "FE" (fixed effects meta analysis). Random effects meta analysis can be undertaken
#' #'   using methods "DSL" (DerSimonian & Laird 1986) and "HT" (Hardy & Thompson 1996).
#' #' @param alpha Numeric. Confidence intervals are of width 1-alpha.
#' #' @param HT_iter Numeric. Number of iterations to employ in HT algorithm. Ignored if
#' #'   method is not "HT".
#' #'
#' #' @return A list estimates.
#' #'
#' #' @references DerSimonian, R., Laird, N. (1986). Meta-analysis in clinical trials.
#' #'   Controlled Clinical Trials, 7(3), 177–188.
#' #'   https://doi.org/10.1016/0197-2456(86)90046-2
#' #' @references Hardy, R. J., Thompson, S. G. (1996). A Likelihood Approach to
#' #'   Meta-Analysis with Random Effects. Statistics in Medicine, 15(6), 619–629.
#' #'   https://doi.org/10.1002/(SICI)1097-0258(19960330)15:6<619::AID-SIM188>3.0.CO;2-A
#' #'
#' #' @export
#' meta_analysis <- function(effect_size, std_err,
#'                         method = 'FE', alpha = 0.05,
#'                         HT_iter = 100) {
#'
#' check0 <- toupper(method) %in% c('FE', 'DSL', 'HT')
#' if (isFALSE(check0)){
#'   stop(simpleError('method must be one of "FE", "DSL" or "HT"'))
#' }
#' check1 <- length(effect_size) == length(std_err)
#' if (isFALSE(check1)){
#'   stop(simpleError('effect_size and std_err must be the same length'))
#' }
#' check2 <- all(complete.cases(c(effect_size, std_err)))
#' if (isFALSE(check2)){
#'   stop(simpleError('Missing values dected in effect_size and/or std_err'))
#' }
#'   # calculate confidence intervals
#'   lower <- effect_size - qnorm(1-alpha/2)*std_err
#'   upper <- effect_size + qnorm(1-alpha/2)*std_err
#'
#'   # calculate weights, Q & Q's p-value, and degrees of freedom
#'   w <- 1/(std_err^2)
#'   Q <- sum(w*(effect_size^2)) - ((sum(w*effect_size))^2/sum(w))
#'   df <- length(effect_size) - 1
#'   Qp <- 1- pchisq(Q, df)
#'
#'   # calcualte I-squared
#'   Isq <- max(0, 100*((Q-df)/Q))
#'
#'   comb_eff <- sum(w*effect_size)/sum(w)
#'   comb_std_err <- sqrt(1/sum(w))
#'   comb_lcl <- comb_eff - (qnorm(1-alpha/2)*comb_std_err)
#'   comb_ucl <- comb_eff + (qnorm(1-alpha/2)*comb_std_err)
#'
#'   z <- comb_eff/comb_std_err
#'   zp <- 2*(1-pnorm(abs(z)))
#'
#'   # this needs to be moved below/combined with shrinkage estimates
#'   if (toupper(method) == 'FE'){
#'     name <- c('Estimate', 'Std. Err.', 'Wald LCL', 'Wald UCL', 'Z', 'Pr(<|Z|)',
#'               'Q', 'Pr(<|Q|)', 'DF', 'I^2')
#'     out <- list(comb_eff, comb_std_err, comb_lcl, comb_ucl, z, zp, Q, Qp, df, Isq)
#'     names(out) <- name
#'     return(out)
#'   }
#'
#'   # calculate random effects variance component
#'   var <- max(0, (Q - df)/(sum(w) - (sum(w^2)/sum(w))))
#'   check3 <- var == 0
#'   if (isTRUE(check3)){
#'     warning(simpleWarning('Random effects variance component estimated to be negative has been set to zero'))
#'   }
#'
#'   # update weights with variance
#'   new_w <- 1/(std_err^2 + var)
#'   rel_w <- 100*new_w/sum(new_w)
#'
#'   est <- (sum(new_w*effect_size)/sum(new_w))
#'   comb_std_err <- sqrt(1/sum(new_w))
#'   wald_lower <- est - ((qnorm(1-(alpha/2)))*comb_std_err)
#'   wald_upper <- est + ((qnorm(1-(alpha/2)))*comb_std_err)
#'   z <- est/comb_std_err
#'   zp <- 2*(1-pnorm(abs(z)))
#'
#'   shr <- var/(var+std_err^2)
#'   shrink <- est + (shr*(effect_size-est))
#'
#'   pred_mean <- est
#'   pred_var <- (1/sum(new_w))+var
#'
#'   if (pred_var > 0) {
#'     pred_se <- sqrt(pred_var)
#'     pred_low <- pred_mean - qnorm(1-alpha/2)*pred_se
#'     pred_high <- pred_mean + qnorm(1-alpha/2)*pred_se
#'
#'   } else {
#'     pred_se <- NA
#'     pred_low <- NA
#'     pred_high <- NA
#'   }
#'   if (toupper(method) == 'DSL'){
#'     name <- c("Estimate", "Std. Err.", "Wald LCL", "Wald UCL", "Mean", "Pred Std. Err.", "Pred LCL", "Pred UCL", "RE Variance", "Z", "Pr(<|Z|)", "Q", "Pr(<|Q|)", "DF", "I^2")
#'     out <- list(est, comb_std_err, wald_lower, wald_upper, pred_mean, pred_se, pred_low, pred_high, var, z, zp, Q, Qp, df, Isq)
#'     names(out) <- name
#'     attr(out, 'method') <- 'DerSimonian & Laird'
#'     return(out)
#'
#'   } else {
#'     theta <- vector('numeric', iter)
#'     sigma <- vector('numeric', iter)
#'     theta[1] <- est
#'     sigma[1] <- var
#'     for (i in 2:iter){
#'       numer <- sum(effect_size/(std_err^2 + sigma[i-1]))
#'       demon <- sum(1/(std_err^2 + sigma[i-1]))
#'       new_theta <- numer/demon
#'       numer2 <- sum((((effect_size - theta[i-1])^2) - std_err^2)/((std_err^2 + sigma[i-1])^2))
#'       denom2 <- sum(1/((std_err^2 + sigma[i-1])^2))
#'       new_sigma <- numer2/denom2
#'       theta[i] <- new_theta
#'       sigma[i] <- max(0, new_sigma)
#'     }
#'     theta_pct_diff <- 100*(theta[iter] - theta[iter-1])/theta[iter-1]
#'     if (sigma[iter] == 0){
#'       sigma_pct_diff <- 0
#'     } else {
#'       sigma_pct_diff <- 100*(sigma[iter] - sigma[iter-1])/sigma[iter-1]
#'     }
#'     if (theta_pct_diff < -1e-6 | sigma_pct_diff > 1e-6){
#'       stop(simpleWarning('Convergence not reached; try increasing the number of iterations'))
#'     }
#'     a11 <- sum(1/std_err^2 + sigma[iter])
#'     a21 <- sum((effect_size - theta[iter])/((std_err^2 + sigma[iter])^2))
#'     a22 <- sum(((effect_size - theta[iter])^2)/((std_err^2+sigma[iter])^3)) - sum(1/(2*((std_err^2+sigma[iter])^2)))
#'
#'     inv11 <- (1/((a11*a22) - (a21*a21)))*a22
#'     inv22 <- (1/((a11*a22) - (a21*a21)))*a11
#'
#'     ht_std_err <- sqrt(inv11)
#'     ht_lower <- theta[iter] - ((qnorm(1-alpha/2))*ht_std_err)
#'     ht_upper <- theta[iter] + ((qnorm(1-alpha/2))*ht_std_err)
#'
#'     if (inv22 > 0){
#'       ht_var_std_err <- sqrt(inv22)
#'       var_lower <- sigma[iter] - ((qnorm(1-alpha/2)*ht_var_std_err))
#'       var_upper <- sigma[iter] + ((qnorm(1-alpha/2)*ht_var_std_err))
#'     } else {
#'       ht_var_std_err <- NA
#'       var_lower <- NA
#'       var_upper <- NA
#'     }
#'     z <- (theta[iter]/ht_std_err)
#'     zp <- 2*(1-pnorm(abs(z)))
#'
#'     shr <- sigma[iter]/(sigma[iter]+std_err^2)
#'     shrink <- theta[iter] + (shr*(effect_size - theta[iter]))
#'
#'     w <- 1/(std_err^2 + sigma[iter])
#'     rel_w <- 100*w/sum(w)
#'     pred_mean <- theta[iter]
#'     pred_var <- (1/sum(w))+sigma[iter]
#'
#'     if (pred_var >= 0){
#'       pred_se <- sqrt(pred_var)
#'       pred_low <- theta[iter] - (qnorm(1-alpha/2)*pred_se)
#'       pred_high <- theta[iter] + (qnorm(1-alpha/2)*pred_se)
#'     } else {
#'       pred_se <- NA
#'       pred_low <- NA
#'       pred_high <- NA
#'     }
#'
#'     # Profile Likelihood
#'     lmax <- -sum(0.5*(log(2*pi*(std_err^2 + sigma[iter])))) - sum(((effect_size - theta[iter])^2)/(2*(std_err^2 + sigma[iter])))
#'     x <- lmax-((qchisq((1-alpha), 1))/2)
#'     y <- x*1.0001
#'     z <- x*0.9999
#'     if (y > z){
#'       y <- x*0.9999
#'       z <- x*1.0001
#'     }
#'     # upper
#'     sigma_ml <- sigma[iter]
#'     prof_upper <- wald_upper
#'     for (i in 1:20){
#'       numer <- sum((((effect_size - prof_upper)^2) - std_err^2)/((std_err^2 + sigma_ml)^2))
#'       denom <- sum(1/((std_err^2 + sigma_ml)^2))
#'       sigma_ml <- numer/denom
#'     }
#'     lupp <- -sum(0.5*(log(2*pi*(std_err^2+sigma_ml)))) - sum(((effect_size - prof_upper)^2)/(2*(std_err^2+sigma_ml)))
#'
#'     if (lupp != x){
#'     while (lupp > z | lupp < y){
#'         prof_upper <- prof_upper + sign(lupp - x)*(ht_std_err*0.0001)
#'         for (i in 1:20){
#'           numer <- sum((((effect_size - prof_upper)^2) - std_err^2)/((std_err^2 + sigma_ml)^2))
#'           denom <- sum(1/((std_err^2 + sigma_ml)^2))
#'           sigma_ml <- numer/denom
#'         }
#'         lupp <- -sum(0.5*(log(2*pi*(std_err^2+sigma_ml)))) - sum(((effect_size - prof_upper)^2)/(2*(std_err^2+sigma_ml)))
#'       }
#'     }
#'     # lower
#'     sigma_ml <- sigma[iter]
#'     prof_lower <- wald_lower
#'     for (i in 1:20){
#'       numer <- sum((((effect_size - prof_lower)^2) - std_err^2)/((std_err^2 + sigma_ml)^2))
#'       denom <- sum(1/((std_err^2 + sigma_ml)^2))
#'       sigma_ml <- numer/denom
#'     }
#'     llow <- -sum(0.5*(log(2*pi*(se^2+sigma_ml)))) - sum(((effect_size - prof_lower)^2)/(2*(std_err^2+sigma_ml)))
#'
#'     if (llow != x){
#'       while (llow > z | llow < y){
#'         prof_lower <- prof_lower + sign(x-lupp)*(ht_std_err*0.0001)
#'         for (i in 1:20){
#'           numer <- sum((((effect_size - prof_lower)^2) - std_err^2)/((std_err^2 + sigma_ml)^2))
#'           denom <- sum(1/((std_err^2 + sigma_ml)^2))
#'           sigma_ml <- numer/denom
#'         }
#'         llow <- -sum(0.5*(log(2*pi*(std_err^2+sigma_ml)))) - sum(((effect_size - prof_lower)^2)/(2*(std_err^2+sigma_ml)))
#'       }
#'     }
#'
#'     # Profile likelihood of Random effects variance
#'     prof_upper_var <- sigma[iter] + ht_var_std_err
#'     numer <- sum(effect_size/(std_err^2 + prof_upper_var))
#'     denom <- sum(1/(std_err^2 + prof_upper_var))
#'     theta_ml <- numer/denom
#'
#'     lupp <- -sum(0.5*(log(2*pi*(std_err^2 + prof_upper_var)))) - sum(((effect_size - theta_ml)^2)/(2*(std_err^2 + prof_upper_var)))
#'
#'     if (lupp != x){
#'       while (lupp > z | lupp < y){
#'         prof_upper_var <- prof_upper_var + sign(lupp - x)*(ht_var_std_err*0.0001)
#'         for (i in 1:20){
#'           numer <- sum(effect_size/(std_err^2 + prof_upper_var))
#'           denom <- sum(1/((std_err^2 + prof_upper_var)))
#'           theta_ml <- numer/denom
#'         }
#'         lupp <- -sum(0.5*(log(2*pi*(std_err^2 + prof_upper_var)))) - sum(((effect_size - theta_ml)^2)/(2*(std_err^2 + prof_upper_var)))
#'       }
#'     }
#'
#'     lower_flag <- 0
#'     prof_lower_var <- sigma[iter] - ht_var_std_err
#'
#'     if (prof_lower_var <= 0) {
#'       prof_lower_var <- 0
#'       lower_flag <- 1
#'     }
#'
#'     if (prof_lower_var > 0){
#'       numer <- sum(effect_size/(std_err^2 + prof_lower_var))
#'       denom <- sum(1/(std_err^2 + prof_lower_var))
#'       theta_ml <- numer/denom
#'
#'       llow <- -sum(0.5*(log(2*pi*(std_err^2 + prof_lower_var)))) - sum(((effect_size - theta_ml)^2)/(2*(std_err^2+prof_lower_var)))
#'
#'       if (llow != x){
#'         while (llow > z | llow < y){
#'           prof_lower_var <- prof_lower_var + sign(x-llow)*(ht_var_std_err*0.0001)
#'           if (prof_lower_var < 0){
#'             prof_lower_var <- 0
#'             llow <- ((z-y)/2)+y
#'             lower_flag <- 1
#'           } else {
#'             numer <- sum(effect_size/(std_err^2 + prof_lower_var))
#'             denom <- sum(1/(std_err^2 + prof_lower_var))
#'             theta_ml <- numer/denom
#'             llow <- -sum(0.5*(log(2*pi*(std_err^2 + prof_lower_var)))) - sum(((effect_size - theta_ml)^2)/(2*(std_err^2+prof_lower_var)))
#'           }
#'         }
#'       }
#'     }
#'     if (sigma[iter] == 0){
#'       warning(simpleWarning('Random effects variance component estimated to be negative has been set to zero'))
#'     }
#'     if (lower_flag == 1){
#'       warning(simpleWarning('Random effects variance component estimated to be negative; profile likelihood lower limit has been set to zero'))
#'     }
#'     name <- c('Estimate', 'Est. Std. Err.', 'Est. Wald LCL', 'Est. Wald UCL', 'Est. Prof LCL', 'Est. Prof UCL',
#'               'RE Variance', 'Var Std. Err.', 'Var Wald LCL', 'Var Wald UCL', 'Var. Prof LCL', 'Var Prof UCL',
#'               'Z', 'Pr(<|Z|)', 'Q', 'Pr(<|Q|)', 'DF', 'I^2')
#'     out <- list(theta[iter], ht_std_err, wald_lower, wald_upper, prof_lower, prof_upper,
#'              sigma[iter], ht_var_std_err, var_lower, var_upper, prof_lower_var, prof_upper_var,
#'              z, zp, Q, Qp, df, Isq)
#'     names(out) <- name
#'     return(out)
#'     }
#' }
#'
