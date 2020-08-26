setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

load("stacked_model.Rdata")
load("char_condid_index.Rdata")

type_vec <- stacked_key6_fit$kept_values$model_settings$covariates_data$characteristics
treat_vec <- stacked_key6_fit$kept_values$model_settings$covariates_data$treatment_fact

mean.list <- list(char.mean = matrix(NA, nrow = 301, ncol = 6),
                  consid.mean = matrix(NA, nrow = 301, ncol = 6),
                  mchar.mean = matrix(NA, nrow = 301, ncol = 6),
                  xchar.mean = matrix(NA, nrow = 301, ncol = 6),
                  nrchar.mean = matrix(NA, nrow = 301, ncol = 6),
                  controlchar.mean = matrix(NA, nrow = 301, ncol = 6),
                  mconsid.mean = matrix(NA, nrow = 301, ncol = 6),
                  xconsid.mean = matrix(NA, nrow = 301, ncol = 6),
                  nrconsid.mean = matrix(NA, nrow = 301, ncol = 6),
                  controlconsid.mean = matrix(NA, nrow = 301, ncol = 6)
                  )

### Try to get uncertainty measures for proportions by treatment.
for(x in 1:301){
  i = stacked_key6_fit[["values_iter"]][["theta_iter"]][[x]]
  mean.list$char.mean[x,] <- apply(i[type_vec==1,], MARGIN = 2, mean)
  mean.list$consid.mean[x,] <- apply(i[type_vec==0,], MARGIN = 2, mean)
  mean.list$mchar.mean[x,] <- apply(i[type_vec==1 & treat_vec==1,], MARGIN = 2, mean)
  mean.list$xchar.mean[x,] <- apply(i[type_vec==1 & treat_vec==2,], MARGIN = 2, mean)
  mean.list$nrchar.mean[x,] <- apply(i[type_vec==1 & treat_vec==3,], MARGIN = 2, mean)
  mean.list$controlchar.mean[x,] <- apply(i[type_vec==1 & treat_vec==4,], MARGIN = 2, mean)
  mean.list$mconsid.mean[x,] <- apply(i[type_vec==0 & treat_vec==1,], MARGIN = 2, mean)
  mean.list$xconsid.mean[x,] <- apply(i[type_vec==0 & treat_vec==2,], MARGIN = 2, mean)
  mean.list$nrconsid.mean[x,] <- apply(i[type_vec==0 & treat_vec==3,], MARGIN = 2, mean)
  mean.list$controlconsid.mean[x,] <- apply(i[type_vec==0 & treat_vec==4,], MARGIN = 2, mean)
}



#Get estimates and error bars for predicting topics across the two questions

#first, create holder matrices for each of the six topics/regressions we will run
legit_holder <- matrix(nrow = 200, ncol = 6)
symp_holder <- matrix(nrow = 200, ncol = 6)
econ_holder <- matrix(nrow = 200, ncol = 6)
cult_holder <- matrix(nrow = 200, ncol = 6)
illeg_holder <- matrix(nrow = 200, ncol = 6)
uncert_holder <- matrix(nrow = 200, ncol = 6)

#now run for-loop of regression on each iteration of the saved thetas 
for(x in 150:301){ #it's only 301 because of the thinning, and 150 because of burn-in
  i = stacked_key6_fit[["values_iter"]][["theta_iter"]][[x]]
  #make into data frame 
  i <- as.data.frame(i)
  print(x)
  
  #split the data frame into two depending on question status 
  thetas_char <- as_tibble(i[1:n_char_docs,])
  thetas_consid <- as_tibble(i[(n_char_docs+1):n_stacked_dfm,])
  
  #name the columns of the data frames
  names(thetas_char) <- c("Legitimacy Char.", "Sympathy Char.",
                                  "Economic Char.", "Culture Char.",
                                  "Illegality Char.", "Uncertainty Char.")
  
  names(thetas_consid) <- c("Legitimacy Consid.", "Sympathy Consid.",
                                    "Economic Consid.", "Culture Consid.",
                                    "Illegality Consid.", "Uncertainty Consid.")
  
  #char_index and consid_index come from the noah_playing2 file 
  thetas_char$index <- char_index
  thetas_consid$index <- consid_index
  
  merged_thetas <- merge(thetas_char, thetas_consid,
                         by="index")
  #run six regressions using one of the two data frames as the outcome 
  predictors <- "`Legitimacy Consid.`+`Sympathy Consid.`+`Economic Consid.`+`Culture Consid.`+`Illegality Consid.`"

   legit.lm <- lm(data=merged_thetas, 
                  formula= as.formula(paste("`Legitimacy Char.` ~", predictors)))
  symp.lm <- lm(data=merged_thetas, 
                 formula= as.formula(paste("`Sympathy Char.` ~ ", predictors)))
  econ.lm <- lm(data=merged_thetas, 
                 formula= as.formula(paste("`Economic Char.` ~ ", predictors)))
  cult.lm <- lm(data=merged_thetas, 
                 formula= as.formula(paste("`Culture Char.` ~ ", predictors)))
  illeg.lm <- lm(data=merged_thetas, 
                  formula= as.formula(paste("`Illegality Char.` ~ ", predictors)))
  uncert.lm <- lm(data=merged_thetas, 
                   formula= as.formula(paste("`Uncertainty Char.` ~ ", predictors)))
  
  #save the coefficients from each of the regressions 
  legit_holder[x-149,] <- summary(legit.lm)$coefficients[,1]
  symp_holder[x-149,] <- summary(symp.lm)$coefficients[,1]
  econ_holder[x-149,] <- summary(econ.lm)$coefficients[,1]
  cult_holder[x-149,] <- summary(cult.lm)$coefficients[,1]
  illeg_holder[x-149,] <- summary(illeg.lm)$coefficients[,1]
  uncert_holder[x-149,] <- summary(uncert.lm)$coefficients[,1]
  
}



#name the columns of the holder matrix

estimate_func <- function(data){
  estimate<- apply(data, MARGIN = 2, FUN = function(x){ 
    quantile(x, probs = c(0.1, 0.5, 0.9), na.rm = T)}) %>% 
    t() %>% as_tibble()
  estimate$var <- rownames(summary(cult.lm)$coefficients)
  names(estimate) <- c("Lower","Median","Upper","Topic")
  return(estimate)
}

#make function for pointrange plots for all six topics
pointrange_plot <- function(model){
  model %>% ggplot(.) + 
    geom_pointrange(aes(x = Topic, y = Median, ymin = Lower, ymax = Upper)) + 
    coord_flip() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    ylab("Estimate") + 
    facet_wrap(~regression)
  
}

#run estimate function on each of the holder matrices 
legit_est <- estimate_func(legit_holder)
symp_est <- estimate_func(symp_holder)
econ_est <- estimate_func(econ_holder)
cult_est <- estimate_func(cult_holder)
illeg_est <- estimate_func(illeg_holder)
uncert_est <- estimate_func(uncert_holder)

#create combined data frame of results with indicator for which regression it is 
combined <- rbind(legit_est, symp_est, econ_est, cult_est, illeg_est, uncert_est)
combined$regression <- c(rep("Legitimacy", 6), rep("Sympathy",6), rep("Economic",6), 
                         rep("Culture", 6), rep("Illegality",6), rep("Uncertainty",6))

#plot each regression using the function from above 
topic_on_topic_reg <- pointrange_plot(model = combined)
ggsave(topic_on_topic_reg, 
       file = "topic_on_topic.pdf",
       width = 6, height = 4, units = "in")


subsets <- names(mean.list)
out <- lapply(1:10, 
              FUN = function(x){
                data.frame(t(apply(mean.list[[x]][151:301,], MARGIN=2, FUN= function(y){
                  c(mean = mean(y), upper = quantile(y, probs=c(0.975)),
                    lower = quantile(y, probs=c(0.025))
                  )
                }
                )))
              }
)
out <- lapply(1:10,
              FUN = function(x){
                out[[x]] <- cbind (out[[x]], 
                                   subset = subsets[x],
                                   topics = c("Legitimacy", "Sympathy", "Economic",
                                              "Culture", "Illegality", "Uncertainty")
                )
              }
)

# Bind into one data frame
outbound <- data.frame(NULL)
for(j in 1:10){
  outbound <- rbind(outbound, out[[j]])
  
}

# Add indicators 
outbound$Question <- case_when(grepl("char", outbound$subset) ~ "Characteristics",
                               grepl("consid", outbound$subset) ~ "Considerations",
)
outbound$Treatment <- case_when(grepl("mc", outbound$subset) ~ "Muslim",
                                grepl("xc", outbound$subset) ~ "Christian",
                                grepl("nrc", outbound$subset) ~ "No Religion",
                                grepl("control", outbound$subset) ~ "Control",
                                TRUE ~ "All"
)
outbound$Treatment <- factor(outbound$Treatment,
                             levels = c("All", "Control", "Muslim", "Christian", "No Religion"))

thetas_by_treat <- outbound %>% filter(Treatment != "All")
treat_mean_thetas <- ggplot(data = thetas_by_treat,
                            aes (x=topics, y = mean, group=Treatment)) +
  geom_point(aes(color=Treatment),
             position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin= lower.2.5., ymax = upper.97.5., color=Treatment),
                 position=position_dodge(width=0.5)) +
  coord_flip()+
  theme_bw() + ylab("Mean Topic Proportion (theta) Given Treatment & Question") +
  facet_wrap(~Question) +
  theme(legend.position="bottom")

thetas_by_type <- outbound %>% filter(Treatment == "All")
type_mean_thetas <- ggplot(data = thetas_by_type,
                            aes (x=topics, y = mean)) +
  geom_point(aes (color=Question),
             position=position_dodge(width=0.3)) +
  geom_linerange(aes(ymin= lower.2.5., ymax = upper.97.5., color = Question),
                 position=position_dodge(width=0.3)) +
  coord_flip()+
  theme_bw() + ylab("Mean Topic Proportion (theta) Given Question")+
  theme(legend.position="bottom")

ggsave(treat_mean_thetas, 
       file = "topics_by_treat.pdf",
       width = 6, height = 4, units = "in")
ggsave(type_mean_thetas, 
       file = "topics_by_question.pdf",
       width = 6, height = 4, units = "in")


## Diagnostic trace plots
plot(mean.list[["char.mean"]][151:301,1], type="l") # Looks good
plot(mean.list[["char.mean"]][151:301,2], type="l") # looks ok
plot(mean.list[["char.mean"]][151:301,3], type="l") # looks ok
plot(mean.list[["char.mean"]][151:301,4], type="l") # looks good
plot(mean.list[["char.mean"]][151:301,5], type="l") # looks good
plot(mean.list[["char.mean"]][151:301,6], type="l") # looks good

plot(mean.list[["consid.mean"]][150:301,1], type="l") # Looks not so good
plot(mean.list[["consid.mean"]][150:301,2], type="l") # looks less good
plot(mean.list[["consid.mean"]][150:301,3], type="l") # looks better
plot(mean.list[["consid.mean"]][150:301,4], type="l") # looks less good
plot(mean.list[["consid.mean"]][150:301,5], type="l") # looks good
plot(mean.list[["consid.mean"]][150:301,6], type="l") # looks good

plot(mean.list[["xconsid.mean"]][,1], type="l") # Looks not so good
plot(mean.list[["xconsid.mean"]][,2], type="l") # looks not so good
plot(mean.list[["xconsid.mean"]][150:301,3], type="l") # looks ok
plot(mean.list[["xconsid.mean"]][150:301,4], type="l") # looks less good
plot(mean.list[["xconsid.mean"]][150:301,5], type="l") # looks good
plot(mean.list[["xconsid.mean"]][150:301,6], type="l") # looks good

plot(mean.list[["mchar.mean"]][150:301,1], type="l") # Looks ok
plot(mean.list[["mchar.mean"]][150:301,2], type="l") # looks ok
plot(mean.list[["mchar.mean"]][150:301,3], type="l") # looks good
plot(mean.list[["mchar.mean"]][150:301,4], type="l") # looks good
plot(mean.list[["mchar.mean"]][150:301,5], type="l") # looks good
plot(mean.list[["mchar.mean"]][150:301,6], type="l") # looks good


###
library(keyATM)

top_words(stacked_key6_fit, n =25)

stacked_key6_fit$keywords_raw

