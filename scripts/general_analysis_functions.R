resultsfnc <- function(data, baseline_covariates, time_varying_covariates_transplant, 
                       time_varying_covariates_censoring, 
                       increased_risk_usage_factor, standard_risk_usage_factor, 
                       summary_only = T){
  
  data_wts <- data %>% mutate(Wts = estimate_weights(
    data = data, 
    baseline_covariates = baseline_covariates, 
    time_varying_covariates_transplant = time_varying_covariates_transplant, 
    time_varying_covariates_censoring = time_varying_covariates_censoring,
    increased_risk_usage_factor = increased_risk_usage_factor, 
    standard_risk_usage_factor = standard_risk_usage_factor
    ))
  
  standard_data <- data.frame(days_since_start = seq(from=0, to=3630, by=30))
  
  outcome_model <- glm(death_event ~ 
                         ns(days_since_start, knots = c(60,120,360,720,1620), Boundary.knots = c(30,3600)), 
    data = data_wts,
    family = binomial(),
    weights = Wts
    )
  
  predicted <- standard_data %>%
    mutate(cond_surv = 1-predict(outcome_model, ., type="response")) %>% mutate(CI = 1-cumprod(cond_surv)) 
  
  if(summary_only == F){
    return(data.frame(CI=c(0,predicted[["CI"]])) %>%
             mutate(days_since_start = seq(from=0, to=3660, by=30)))
  } else {
    return(CI = predicted[nrow(predicted),"CI"])
  }
}


resultsfnctransplant <- function(data, baseline_covariates, time_varying_covariates_transplant, 
                                time_varying_covariates_censoring, transplant_var, 
                                increased_risk_usage_factor, standard_risk_usage_factor,
                                summary_only = T){
  
  data_wts <- data %>% mutate(Wts = estimate_weights(
    data = data, 
    baseline_covariates = baseline_covariates, 
    time_varying_covariates_transplant = time_varying_covariates_transplant, 
    time_varying_covariates_censoring = time_varying_covariates_censoring,
    increased_risk_usage_factor = increased_risk_usage_factor, 
    standard_risk_usage_factor = standard_risk_usage_factor
  ))
  
  num_grafts_used <- data_wts %>% 
    group_by(days_since_start) %>% summarise(sum(!!as.symbol(transplant_var)*Wts)) %>% as.data.frame() %>% 
    {.[1:length(seq(from=0, to=3630, by=30)),2]}
  
  if(summary_only == F){
    return(data.frame(num_grafts_used = num_grafts_used) %>%
             mutate(days_since_start = seq(from=0, to=3630, by=30)))
  } else {
    return(num_grafts_used = num_grafts_used[length(num_grafts_used)])
  }
}


estimate_weights <- function(data, baseline_covariates,
                             time_varying_covariates_transplant, time_varying_covariates_censoring, 
                             increased_risk_usage_factor, standard_risk_usage_factor){
  
  transplant_var_list <- list("ns(days_since_start, knots = c(60,120,360,720,1620), Boundary.knots = c(30,3600))",
                              paste(baseline_covariates, collapse=" + "),
                              paste(time_varying_covariates_transplant, collapse=" + "))
  censoring_var_list <- list("ns(days_since_start, knots = c(60,120,360,720,1620), Boundary.knots = c(30,3600))",
                             paste(baseline_covariates, collapse=" + "),
                             paste(time_varying_covariates_censoring, collapse=" + "))
    
  transplant_formula_RHS <- paste(
    transplant_var_list[transplant_var_list != ""], 
    collapse=" + "
    )
  censoring_formula_RHS <- paste(
    censoring_var_list[censoring_var_list != ""], 
    collapse=" + "
  )
  
  prob_censored <- 
    predict(glm(formula=as.formula(paste("censoring_event", censoring_formula_RHS, sep=" ~ ")), 
                data=data, 
                family=binomial()), 
            newdata=data, type="response")
  
  prob_censored_at_transplant <- 
    predict(glm(formula=as.formula(paste("transplant_and_censored_at_tx", transplant_formula_RHS, sep=" ~ ")), 
                data=data %>% filter(censoring_event==0 & post_transplant==0), 
                family=binomial()), 
            newdata=data, type="response")
  
  prob_standard_risk_transplant <- 
    predict(glm(formula=as.formula(paste("transplant_and_standard_risk", transplant_formula_RHS, sep=" ~ ")), 
                data=data %>% filter(censoring_event==0 & post_transplant==0 & transplant_and_censored_at_tx == 0), 
                family=binomial()), 
            newdata=data, type="response")
  
  prob_increased_risk_transplant <- 
    predict(glm(formula=as.formula(paste("transplant_and_increased_risk", transplant_formula_RHS, sep=" ~ ")), 
                data=data %>% filter(censoring_event==0 & post_transplant==0 & transplant_and_censored_at_tx == 0 & transplant_and_standard_risk==0), 
                family=binomial()), 
            newdata=data, type="response")
  
  
  data_with_weights <- data.frame(patient_id = data$patient_id,
                                  post_transplant = data$post_transplant,
                                  transplant_event = data$transplant_event,
                                  censoring_event = data$censoring_event,
                                  transplant_and_standard_risk = data$transplant_and_standard_risk,
                                  transplant_and_increased_risk = data$transplant_and_increased_risk,
                                  transplant_and_censored_at_tx = data$transplant_and_censored_at_tx,
                                  prob_censored = prob_censored,
                                  prob_censored_at_transplant = prob_censored_at_transplant,
                                  prob_increased_risk_transplant = prob_increased_risk_transplant,
                                  prob_standard_risk_transplant = prob_standard_risk_transplant) %>%
    mutate(
           Wt_censoring = case_when(censoring_event == 1 ~ 0,
                                    TRUE ~ 1/(1-prob_censored)),
           Wt_censored_at_transplant = case_when(post_transplant == 1 ~ 1,
                                                 transplant_and_censored_at_tx == 1 ~ 0,
                                                 TRUE ~ 1/(1-prob_censored_at_transplant)),
           Wt_d = 1,
           Wt_dd = 1,
           Wt_no_int = Wt_censoring*Wt_censored_at_transplant
           ) %>%
    group_by(patient_id) %>%
    mutate(timegroup = row_number(),
           Wt_no_int_cumprod = cumprod(Wt_no_int)) %>%
    ungroup()
  
  data_with_weights <- adjust_wts(
    data_with_weights = data_with_weights, 
    increased_risk_usage_factor = increased_risk_usage_factor, 
    standard_risk_usage_factor = standard_risk_usage_factor
    ) %>%
    mutate(Wt_g = Wt_censoring*Wt_d*Wt_dd*Wt_censored_at_transplant) %>%
    group_by(patient_id) %>% 
    mutate(Wt_g_cumprod = cumprod(Wt_g)) %>%
    ungroup()
  
  print(data_with_weights$Wt_g_cumprod %>% quantile(seq(0,1,0.1)))
  
  return(data_with_weights$Wt_g_cumprod)
}


adjust_wts <- function(data_with_weights, increased_risk_usage_factor, standard_risk_usage_factor) {
  
  alpha <- rep(1,length=max(data_with_weights$timegroup)); beta <- rep(1,length=max(data_with_weights$timegroup))
  alef_B <- rep(1,length=max(data_with_weights$timegroup)); alef_H <- rep(1,length=max(data_with_weights$timegroup))
  bet_B <- rep(1,length=max(data_with_weights$timegroup)); bet_H <- rep(1,length=max(data_with_weights$timegroup))
  
  for (j in 1:length(alpha)) {
    
    data_with_weights <- data_with_weights %>% mutate(Wt_g = Wt_censoring*Wt_censored_at_transplant*Wt_d*Wt_dd,
                                                      Wt_g_natural_B = case_when(timegroup < j ~ Wt_g,
                                                                            timegroup >= j ~ Wt_no_int)) %>%
      group_by(patient_id) %>%
      mutate(Wt_g_natural_B_cumprod = cumprod(Wt_g_natural_B)) %>%
      ungroup()
      
    E_B_k <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum(.$transplant_and_standard_risk*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*.$Wt_no_int_cumprod)}
    
    E_B_k_g_natural_B <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum(.$transplant_and_standard_risk*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*.$Wt_g_natural_B_cumprod)}
    
    E_Y_k_g_natural_B <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum((1-.$post_transplant)*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*.$Wt_g_natural_B_cumprod)}
    
    alef_B[j] <- (standard_risk_usage_factor*E_B_k <= E_Y_k_g_natural_B)
    
    bet_B[j] <- (E_B_k_g_natural_B > standard_risk_usage_factor*E_B_k)
    
    if (alef_B[j] == 0) {
      alpha[j] <- 0
    } else {
      if (bet_B[j] == 1) {
        alpha[j] <- standard_risk_usage_factor*E_B_k/E_B_k_g_natural_B
      } else {
        alpha[j] <- (1-standard_risk_usage_factor*(E_B_k/E_Y_k_g_natural_B))/(1-(E_B_k_g_natural_B/E_Y_k_g_natural_B))
      }
    }
    
    alpha[which(is.na(alpha) == T)] <- 1
    
    data_with_weights <- data_with_weights %>% mutate(
      Wt_d = case_when(post_transplant == 1 ~ 1,
                       transplant_and_standard_risk == 1 ~ (alpha[timegroup]^bet_B[timegroup])*
                         ((1-alpha[timegroup]*(1-prob_standard_risk_transplant))/prob_standard_risk_transplant)^(1-bet_B[timegroup]),
                       TRUE ~ ((1-alpha[timegroup]*prob_standard_risk_transplant) / (1-prob_standard_risk_transplant))^(bet_B[timegroup])* 
                         (alpha[timegroup]^(1-bet_B[timegroup]))),
      Wt_g = Wt_censoring*Wt_censored_at_transplant*Wt_d*Wt_dd,
      Wt_g_natural_H = case_when(timegroup < j ~ Wt_g,
                            timegroup >= j ~ Wt_no_int*Wt_d)
    ) %>%
      group_by(patient_id) %>%
      mutate(Wt_g_natural_H_cumprod = cumprod(Wt_g_natural_H)) %>%
      ungroup()
    

    E_H_k <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum(.$transplant_and_increased_risk*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*(1-.$transplant_and_standard_risk)*.$Wt_no_int_cumprod)}
    
    E_H_k_g_natural_H <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum(.$transplant_and_increased_risk*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*(1-.$transplant_and_standard_risk)*.$Wt_g_natural_H_cumprod)}
    
    E_Z_k_g_natural_H <- data_with_weights %>% filter(timegroup==j) %>% 
    {sum((1-.$post_transplant)*(1-.$censoring_event)*(1-.$transplant_and_censored_at_tx)*(1-.$transplant_and_standard_risk)*.$Wt_g_natural_H_cumprod)}
    
    alef_H[j] <- (increased_risk_usage_factor*E_H_k <= E_Z_k_g_natural_H)
    
    bet_H[j] <- (E_H_k_g_natural_H > increased_risk_usage_factor*E_H_k)
    
    if (alef_H[j] == 0) {
      beta[j] <- 0
    } else {
      if (bet_H[j] == 1) {
        beta[j] <- increased_risk_usage_factor*E_H_k/E_H_k_g_natural_H
      } else {
        beta[j] <- (1-increased_risk_usage_factor*(E_H_k/E_Z_k_g_natural_H))/(1-(E_H_k_g_natural_H/E_Z_k_g_natural_H))
      }
    }
    
    beta[which(is.na(beta) == T)] <- 1
    
    data_with_weights <- data_with_weights %>% mutate(
      Wt_dd = case_when(post_transplant == 1 | transplant_and_standard_risk == 1 ~ 1,
                        transplant_and_increased_risk == 1 ~ (beta[timegroup]^bet_H[timegroup])*
                          ((1-beta[timegroup]*(1-prob_increased_risk_transplant))/prob_increased_risk_transplant)^(1-bet_H[timegroup]),
                        TRUE ~ ((1-beta[timegroup]*prob_increased_risk_transplant)/(1-prob_increased_risk_transplant))^(bet_H[timegroup])* 
                          (beta[timegroup]^(1-bet_H[timegroup])))
      )
  }
  
  return(data_with_weights)
}


bootstrapfnc <- function(data, baseline_covariates, 
                         time_varying_covariates_transplant, time_varying_covariates_censoring, 
                         increased_risk_usage_factor, standard_risk_usage_factor,
                         summary_only = T){
  
  IDs <- sample(unique(data$patient_id), length(unique(data$patient_id)), replace = TRUE)
  datatemp <- as.data.table(data)
  setkey(datatemp, "patient_id")
  data_resample <- datatemp[J(IDs), allow.cartesian = TRUE]
  data_resample <- data_resample %>% mutate(patient_id = case_when(days_since_start==0 ~ 1, TRUE ~ 0)) %>% mutate(patient_id = cumsum(patient_id))
  
  return(
    resultsfnc(
      data = data_resample,
      baseline_covariates = baseline_covariates,
      time_varying_covariates_transplant = time_varying_covariates_transplant,
      time_varying_covariates_censoring = time_varying_covariates_censoring,
      increased_risk_usage_factor = increased_risk_usage_factor,
      standard_risk_usage_factor = standard_risk_usage_factor,
      summary_only = summary_only
    )
  )
}
