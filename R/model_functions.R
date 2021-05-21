# - - 
# Helper functions

#' get quantile of x with vector names removed
#' @param x a numeric vector; the values to quantile
#' @param qs numeric vector; target quantiles; should be on (0, 1)
c.nume <- function(x, qs = c(0.5, 0.025, 0.975)) {
  unname(quantile(x, probs = qs))
}

#' wrapper for c.nume to include 50% quantile as well
c.nume.50 <- function(x) c.nume(x, qs=c(0.5, 0.025, 0.25, 0.75, 0.975))

#' estimate doubling time in x
#' @param x a numeric vector; each index assumed to represent one time unit,
#'  n.b. will tolerate NAs as long not present in first or last element
doubling_est2 <- function(x) {
  log(2)*length(x)/log(tail(x,1)/head(x,1))
}

#' creates CI text on x
#' @param x a numeric vector; the values to CI
#' @param sigF an integer; the number of significant figures to display
#' @param fmt a string; format string for `sprintf`;
#'   n.b. '%' must be escaped w/ `sprintf`, so to get '%' in output, use '%%' 
#' 
#' TODO eliminate 95 as a magic number
c.text <- function(
  x,
  sigF = 2,
  fmt="%s (95%% CrI: %s-%s)"
) do.call(sprintf, c(list(fmt), as.list(signif(c.nume(x), sigF))))

#' generate binomial est + CIs
binest_CI <- function(xx, nn, CI=0.95) {
  cis <- mapply(function(x, n, conf.level) binom.test(x, n)$conf.int, x=xx, n=nn, conf.level = CI)
  data.table(x=xx, n=nn, med=x/n, lo=cis[1,], hi=cis[2,])
}

# Plot data and binom CI
plot_CI <- function(dates, xx, nn, colA="black") {
  ci.dt <- binest_CI(xx, nn)
  points(dates, ci.dt$med, col=colA,pch=19)
  lines(dates, ci.dt$lo, col=colA)
  lines(dates, ci.dt$hi, col=colA)
}

#' TODO: any of t_max, total_days determined by length of cases_fit?
decline_f <- function(
  t_max,
  daily_decline,
  dt_decline,
  cases_fit
){
  total_days <- length(cases_fit)
  last_non_NA_off <- which.max(!is.na(rev(cases_fit)))-1
  tms <- (1:(t_max - total_days + last_non_NA_off))
  daily_declineX <- daily_decline + (dt_decline-1)*tms/10
  declining_cases <- cases_fit[total_days-last_non_NA_off]*(1-daily_declineX)^tms # Add Poisson noise?
  
  return(c(cases_fit[1:(total_days-last_non_NA_off)], declining_cases))
  
  # Alternative version with sequence numbers
  # prop_non_b16172 <- (1-data_fit$B.1.617.2/data_fit$N)
  # prop_non_b16172 <- ma(prop_non_b16172,7); prop_non_b16172[1:3] <- 1 # Fix early points
  # 
  # na_remove <- prop_non_b16172[!is.na(prop_non_b16172)]
  # tail_val <- tail(na_remove,1)
  # prop_non_b16172 <- c(prop_non_b16172[1:length(na_remove)],rep(tail_val,t_max-length(na_remove)))
  # #ma_UK_cases_2 <- ma_UK_cases*prop_non_b16172 # Includes overall UK case data
  # 
  # na_remove_cases <- ma_UK_cases_fit[1:(total_days_uk-4)]
  # ma_UK_cases0 <- c(na_remove_cases,rep(tail(na_remove_cases,1),t_max-length(na_remove_cases)))
  # ma_UK_cases_2 <- ma_UK_cases0*prop_non_b16172 # Includes overall UK case data
  
}



# Simulation deterministic  ---------------------------------------------------------

fit_R_deterministic <- function(
  theta, run_n, add_days = 25,
  downweight_imports,
  daily_seq,
  daily_imports,
  ma_variant,
  ma_local_cases,
  data_fit
) with(theta, {
  
  # DEBUG r_pick=1.6; run_n=15; add_days=0; import_f = 0.2; daily_decline=0.02
  # theta <- c(rr=1.6,r_scale=1,r_scale_2=1,decline=0.02,dt_decline=1,imp=2,rep_vol=1); add_days=0
  
  # rr <- theta[["rr"]]
  # r_scale <- theta[["r_scale"]]
  # r_scale_2 <- theta[["r_scale_2"]]
  # imp <- theta[["imp"]]
  # decline <- theta[["decline"]]
  dt_decline <- 1 #theta[["dt_decline"]] # Scale rate of change
  ## overwrite dt_decline
  # rep_vol <- theta[["rep_vol"]]
  
<<<<<<< HEAD
  daily <- c(
    daily_seq + imp*downweight_imports*daily_imports*ma_variant,
    rep(0,add_days)
  )
=======
  #daily_india <- daily_india_seq + import_f*downweight_imports*all_india$daily_imports*ma_India_variant
>>>>>>> adamkucharski-main
  
  t_max <- total_days + add_days
  if( add_days>0 ){
    # Set up matrix
    serial_mat <- gen_serial_matrix(t_max)
  } else {
    serial_mat <- serial_mat0
  }

  # Extract UK fit data
  ma_local_cases_2 <- decline_f(t_max, decline, dt_decline, ma_local_cases)
  
  # Store_values
  store_vals <- rep(0, t_max)

  # Add onwards transmission from travellers
  serial_mat2 <- serial_mat*daily*rr
  store_vals1 <- store_vals + colSums(serial_mat2)
  
  # Add onwards transmission from first genertion - exclude travellers for now to avoid double counting
  serial_mat2 <- serial_mat*store_vals1*rr*r_scale
  store_vals <- store_vals + colSums(serial_mat2)
  
  
  # Add onwards transmission from subsequent generations - reduces twice
  for(ii in 1:t_max){
    store_vals <- store_vals + store_vals[ii]*serial_mat[ii,]*rr*r_scale*r_scale_2
  }
  
  store_vals <- daily + store_vals1 + store_vals # Add imports + first generation to avoid double counting
  
  # Calculate estimation interval
  pred_interval <- rbind(store_vals,store_vals,store_vals)
  mean_val <-  store_vals
  
  # Observation model
  data_fit1 <- data_fit[!is.na(data_fit$N),]
  actual_617 <- data_fit1$B.1.617.2
  actual_prop <- data_fit1$B.1.617.2/data_fit1$N
  actual_non_6172 <- data_fit1$N - data_fit1$B.1.617.2
  
  expected_prop <- mean_val/(mean_val+ma_local_cases_2);
  expected_prop <- expected_prop[1:length(actual_prop)]
  
  expected_cases <- (ma_local_cases);
  expected_cases <- expected_cases[1:length(actual_prop)]
  
  expected_6172 <- mean_val;
  expected_6172 <- expected_6172[1:length(actual_prop)]
  
  
  # DEBUG
  #plot(data_fit1$B.1.617.2/data_fit1$N);lines(expected_prop)
  #expected_prop <- mean_val/(ma_UK_cases_2); expected_prop <- expected_prop[1:length(actual_prop)]
  
  #
  # - - 
  # Calculate likelihood on 617.2
  log_L_b <- dnbinom(actual_617,mu=(expected_6172*data_fit1$N/expected_cases),size=1/rep_vol,log=T)

  log_L_b_sum <- log_L_b[!is.na(log_L_b)] %>% sum()
  
  # Calculate likelihood of overall cases
  pick_fit <- all_uk$date>as.Date("2021-04-01") # Only from April 1st 2021
  
  log_L_p <- dnbinom(all_uk$cases_new,mu=(mean_val+ma_UK_cases_2),size=1/rep_vol,log=T)

  log_L_p <- log_L_p[pick_fit]
  
  log_L_p_sum <- log_L_p[!is.na(log_L_p)] %>% sum()
  
  # Total likelihood
  log_L_sum <- log_L_b_sum + log_L_p_sum 
  
  # Output fits
  list(lik = log_L_sum, traj = pred_interval,long_dates = long_dates, mov_average = ma_UK_cases_2, daily_india = daily)
  
})

# End loop


