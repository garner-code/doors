## K. Garner, 2025 ######################################
#########################################################
# apply second level analysis to the beta values estimated
# at the first level
remove_outliers <- function(betas, dv){
  # this function takes the betas dataframe, and replaces
  # any outliers with NA values
  p <- boxplot(betas[, dv])
  nu_dv = paste(dv, "flt", sep='_')
  betas_flt <- betas %>% select(sub, dv) %>%
                 mutate( !!nu_dv := if_else(.data[[dv]] > min(p$stats) &
                                         .data[[dv]] < max(p$stats),
                                         .data[[dv]], NA))
  betas_flt %>% select(nu_dv)
}


apply_outlier_filter_to_all_vars <- function(betas){
  betas <- read.csv(paste(data_path, 'betas_', exp_str, '_first-level.csv', 
                          sep=''))
  vars <- names(betas)[names(betas) != 'sub' & names(betas) != 'train_type']
  betas <- cbind(betas, 
                 do.call(cbind, lapply(vars, remove_outliers, betas=betas)))
  # note that I manually plotted histograms of the distributions at this 
  # point and they all looked reasonably normal
  betas
}

apply_t_tests_to_all_vars <- function(betas){
  
  # first, get the variables of interest
  vars_2_test <- names(betas)[grepl('_flt', names(betas))]
  
  # now apply the t test across variables
  
  #################################################
  # compare the beta co-efficients against zero
  with(betas, t.test(mu_flt, mu=0)) 
  with(betas, t.test(sw_flt, mu=0))
  with(betas, t.test(scs_flt, mu=0))
  with(betas, t.test(cntx_flt, mu=0))
  
  # compare the groups
  with(betas, t.test(mu_flt ~ train_type))
  with(betas, t.test(sw_flt ~ train_type))
  with(betas, t.test(scs_flt ~ train_type))
  with(betas, t.test(cntx_flt ~ train_type))
  
}

#################################################
# now, correlate the regressors with each other
# and with the routine score
# first get the routine score
R <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
                            "rscore-full.csv", sep="_"))
# first get an average routine score
R <- R %>% group_by(sub) %>% summarise(mur = mean(r))
flt <- inner_join(flt, R, by="sub")
# remove rows with nas
flt <- flt %>% na.omit()

# first, check for multivariate outliers
mhl.mat <- as.matrix(flt[,c("mu_flt", "scs_flt", "cntx_flt", "mur")]) 
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20)
sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))

#### learning transfer - we therefore remove the participants with a mahalanobis distance
#### greater than 56.47
rm_sbs <- as.numeric(names(mhl.dist)[which(mhl.dist > qchisq(.001, df=length(mhl.dist)-1))])
flt <- flt %>% filter(!sub %in% rm_sbs)
flt <- flt %>% mutate(mur_l = log(mur))
ggpairs(flt %>% select(mu_flt, scs_flt, cntx_flt, mur_l),
        mapping=ggplot2::aes(colour = flt$train_type),
        upper = list(continuous = wrap("cor", method = "spearman")))
### now do a pairs plot
