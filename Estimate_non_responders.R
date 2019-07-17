#### Exploring test-retest reliability and noise #####
library(ggplot2)
rm(list = ls())
# Assume scores are truly zero. what noise is necessary to make a test-retest reliability of 0.91?
# some algebra from the formula reliability^2 = 1 - sigma_error^2 / sigma_obs^2
sigma_obs = 15
reliability_est = 0.98
sigma_error = sqrt(-sigma_obs^2 * (reliability_est^2 - 1))
print(sigma_error)

##################### RUN SOME SIMULATIONS #############################################
#################### HOW MANY PARTICIPANTS WOULD SHOW NO POSITIVE GROWTH WHATSOEVER #####
estimate_nr <- function(n,sigma_noise, d){
  nsim = 1000
  non_responders_count <- vector(length = nsim)
  non_responders_proportion <- vector(length = nsim)
  pop_mu = 100
  pop_sd = 15
  
  for (i in 1:nsim){
    
    # Generate the true scores
    true_scores = rnorm(n*50, mean = 100, sd = 15)
    # Account for measurement error in the pre-intervention measures
    score_ests = true_scores + rnorm(n*50, mean = 0, sd = sigma_noise)
    # Get a sample
    candidates_idx = which(score_ests < 92)
    samp_measure_t1 <- score_ests[candidates_idx[1:n]]
    samp_true_t1 <- true_scores[candidates_idx[1:n]]
    
    # Now say everyone benefits according to the true effect size 
    samp_true_t2 <- samp_true_t1 + (pop_sd * d)
    # Add measurement error to the post-intervention measure
    samp_measure_t2 <- samp_true_t2 + rnorm(length(samp_true_t2), mean = 0, sd = sigma_noise)
    df <- data.frame(t1 = samp_measure_t1,
                     t2 = samp_measure_t2,
                     subj = seq(1,n))
    
    # How many subjects did NOT get better?
    df$delta <- df$t2 - df$t1
    non_responders_count[i] = sum(df$delta <= 0) 
    non_responders_proportion[i] = (sum(df$delta <=0)/n) * 0.3
    
    
  }
  
  mu_pr = mean(non_responders_proportion)
  sd_pr = sd(non_responders_proportion)
  ans_df <- data.frame(mu = mu_pr,
                       sd = sd_pr,
                       d = d,
                       noise = sigma_noise, 
                       n = n)
  return(ans_df)
}


pop_seq <- c(100)
noise_seq <- c(2.9,6.2)
d_seq <- c(0.4,0.6,0.8,1.0)

out <- data.frame()

for (p in pop_seq){
 for (d in d_seq){
   for (v in noise_seq){
  
    nr_df_tmp <- estimate_nr(p,v,d)
    out <- rbind(out, nr_df_tmp)
   }
 }
}



out$noise <- as.factor(out$noise)
levels(out$noise) <-  c("0.98","0.91")


# Some plot 
pd <- position_dodge(width = 0.05)
palette <- c("#6D1238","#B55B81")

px <- ggplot(out, aes(d, mu))+
  geom_hline(yintercept =  0)+
  geom_point(aes(colour = noise), position = pd, size = 2)+
  geom_errorbar(aes(ymin = mu - 1.96*sd, ymax = mu + 1.96*sd, group = noise, colour = noise),
                position = pd, size = 0.75)+
  xlab("Effect size")+
  ylab("Percent of participants")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5))+
  scale_x_continuous(breaks = c(0.4,0.6,0.8,1.0))+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = 0.5))+
  scale_color_manual(values = palette, name = "Reliability")+
  ggtitle("How many people would not show\ndetectable growth in reading skill?")

px
ggsave("Detectable_change.png", px, dpi = 300)

###########################################################
##### WHAT IF WE HAD USED TORGESON'S CRITERION? ###########
###########################################################
estimate_nr_torg <- function(n,sigma_noise, d){
  nsim = 1000
  non_responders_count <- vector(length = nsim)
  non_responders_proportion <- vector(length = nsim)
  pop_mu = 100
  pop_sd = 15
  
  for (i in 1:nsim){
    
    # Generate the true scores
    true_scores = rnorm(n*50, mean = 100, sd = 15)
    score_ests = true_scores + rnorm(n*50, mean = 0, sd = sigma_noise)
    # Get a sample
    candidates_idx = which(score_ests < 92)
    samp_measure_t1 <- score_ests[candidates_idx[1:n]]
    samp_true_t1 <- true_scores[candidates_idx[1:n]]
    
    # Now say everyone gets the true effect size 
    samp_true_t2 <- samp_true_t1 + (pop_sd * d)
    # Add measurement error
    samp_measure_t2 <- samp_true_t2 + rnorm(length(samp_true_t2), mean = 0, sd = sigma_noise)
    df <- data.frame(t1 = samp_measure_t1,
                     t2 = samp_measure_t2,
                     subj = seq(1,n))
    
    # How many subjects did NOT get better?
    df$delta <- df$t2 - df$t1
    non_responders_count[i] = sum(df$t2 < 92)
    non_responders_proportion[i] = (sum(df$t2 < 92)/n) * 0.30
    
    
  }
  
  mu_nr = mean(non_responders_proportion)
  sd_nr= sd(non_responders_proportion)
  ans_df <- data.frame(mu = mu_nr,
                       sd = sd_nr,
                       d = d,
                       noise = sigma_noise, 
                       n = n)
  return(ans_df)
}

out <- data.frame()
for (p in pop_seq){
  for (d in d_seq){
    for (v in noise_seq){
      
      nr_df_tmp <- estimate_nr_torg(p,v,d)
      out <- rbind(out, nr_df_tmp)
    }
  }
}

out$noise <- as.factor(out$noise)
levels(out$noise) <-  c("0.98","0.91")

px2 <- ggplot(out, aes(d, mu))+
  geom_hline(yintercept =  0)+
  geom_point(aes(colour = noise), position = pd, size = 2)+
  geom_errorbar(aes(ymin = mu - 1.96*sd, ymax = mu + 1.96*sd, group = noise, colour = noise),
                position = pd, size = 0.75)+
  xlab("Effect size")+
  ylab("Percent of participants")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5))+
  scale_x_continuous(breaks = c(0.4,0.6,0.8,1.0))+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = 0.5))+
  scale_color_manual(values = palette, name = "Reliability")+
  ggtitle("How many people would be\nclassified as non-responders?")

px2
ggsave("Nonresponders.png", px2, dpi = 300)


