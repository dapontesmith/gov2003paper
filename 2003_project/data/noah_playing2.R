setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(quanteda)
library(keyATM)
library(readr)
library(ggplot2)
library(kableExtra)
library(dplyr)

df <- read_csv("UK_refugee_2018_clean.csv")

library(quanteda)
library(keyATM)



#remove entries with missing demographic information
df <- df %>% filter(!is.na(white) & !is.na(education) & !is.na(female) & !is.na(age) & !is.na(antipathy))

consid <- df$open_consid
char <- df$open_characterics
index <- as.character(seq(1,nrow(df),1))
df$index <- index

names(char) <- names(consid) <-c(index)



####################################################################################
# Make keyword lists 
keywords6 <- list(
  Legitimacy  = c("flee", "fled", #"flew", "fleer", "penni", #"misfortun", "warzon",
                  "war", "poverti", # "warfar","impoverish", #"disaster", "violence",
                  "famin", "displac", 
                  "persecut", "suffer", "hardship", #"penniless",
                  "need", "needi", 
                  "conflict"
  ),
  Sympathy    = c("brave", "braveri", "desper", "helpless",
                  "frighten", "alon",  "fear", "hope",
                  "hopeless",  "vulner","sad","happi","sympathi", "sympathet",
                  "sorri" #"persever", "tenac", resili",
  ),
  Economic    = c("strain", "overcrowd",  "benefit", #"overspend",
                  "scrounger", "scroung", "money", "money-grab", 
                  "nhs", "lazi", "servic", "econom", "economi",
                  "welfar", "job", #"shortage", "shortfal", #"overpopul"
                  "leech", "freeload", "free",# "freebe", "freebi","short","spong",
                  "sponger", "burden", "tax",
                  "money-grabb", #"scaveng",
                  "spend", "pension"
  ),
  Culture     = c("differ",  "muslim", "outsid", # "jihad", "alien",
                  "languag", "way", "religion", "religi", "english",
                  "valu", "christian", "islam", "dark"), #"mosque",
  Illegality  = c("illeg", "kill", "terrorist","terror", #"villain",
                  "crimin", "crime", #"rape",  "rapist", "terroris",
                  "murder", "robber","isi" # "paedophil", "psychopath"),
  ), 
  Uncertainty = c("sure", "na", "know", "noth", "none", "comment",
                  "opinion","idea" #"generalis"
  )
)

char_wordstem(c("flee","fled","war","poverty","famine","displace","persecute","suffer","hardship","need","neediness"))


keywords3 <- list(  Positive    = c(keywords6$Legitimacy, keywords6$Sympathy),
                    Negative    = c(keywords6$Economic, keywords6$Culture, 
                                    keywords6$Illegality),
                    Uncertainty = c(keywords6$Uncertainty)            )
#################################################################################


#################################################################################
# Create dfm and keydoc objects for responses separately

#create dfm objects, removing stopwords and stemming words 

consid_dfm_stem <- dfm(consid, stem = TRUE,
                       remove = stopwords("english"), remove_punct = TRUE)
char_dfm_stem <- dfm(char, stem = TRUE, 
                     remove = stopwords("english"), remove_punct = TRUE)
## Remove empty documents
char_dfm_stem <- dfm_subset(char_dfm_stem, ntoken(char_dfm_stem) > 0) #2462 obs
consid_dfm_stem <- dfm_subset(consid_dfm_stem, ntoken(consid_dfm_stem) > 0) #2457 obs

#convert data to keyatm objects from Quanteda format
consid_keydocs <- keyATM_read(consid_dfm_stem)
char_keydocs <- keyATM_read(char_dfm_stem)

char_index <- names(char_keydocs) <- 
  char_dfm_stem@docvars$docid_ #Keep indices for later
consid_index <- names(consid_keydocs) <-
  consid_dfm_stem@docvars$docid_ #Keep indices for later

#make vector in df indicating whether document appears in the dfm_stem versions
df$in_char_dfm <- ifelse(df$index %in% char_index, 1, 0)
df$in_consid_dfm <- ifelse(df$index %in% consid_index, 1, 0)


##################################
##run logits on in_char_dfm and in_consid_dfm 

char_logit <- glm(data = df, family = binomial(link = "logit"), 
    in_char_dfm ~ age + white + education + treatment + antipathy + female)

consid_logit <- glm(data = df, family = binomial(link = "logit"), 
                  in_consid_dfm ~ age + white + education + treatment + antipathy + female)
library(stargazer)
stargazer(char_logit, consid_logit ,
          dep.var.labels = c("Characteristics", "Considerations"), 
          title = "Predicting non-response from treatment and covariates",
          omit.stat = "all", 
          column.sep.width =  "-10pt",
          font.size = "small",
          no.space = T,
          label = "response_logit",
          covariate.labels = c("Age","Race","Education","Treatment(Christian)",
                               "Treatment(No Religion)","Treatment(Muslim)","Female"),
          notes = "Baseline condition for treatment is the control condition.")



###########################################################################
# Make dfm and keydoc objects for joint analysis

#make stacked version of the data
index <- as.character(seq(1, nrow(df)*2, 1))
stacked <- c(char, consid) 
names(stacked) <- c(index)
stacked_dfm <- dfm(stacked, stem = TRUE, 
                   remove = stopwords("english"),remove_punct = TRUE)
stacked_dfm <- dfm_subset(stacked_dfm, ntoken(stacked_dfm) > 0)
stacked_keydocs <- keyATM_read(stacked_dfm)
stack_index <- names(stacked_keydocs) <- stacked_dfm@docvars$docid_ 

n_consid_docs <- sum(df$in_consid_dfm, na.rm = T)
n_char_docs <- sum(df$in_char_dfm, na.rm = T)
n_stacked_dfm <- nrow(stacked_dfm)

#docs 1:2483 are characteristics, docs 2484:4961 are considerations

stacked_df <- rbind(df, df)
stacked_df$index <- as.character(seq(1, nrow(stacked_df), 1))
stacked_df$in_stacked_dfm <- ifelse(stacked_df$index %in% stack_index, 1, 0)


############################################################################
#create covariate vectors 
white_char <- df %>% filter(in_char_dfm == 1) %>% pull(white) %>% 
  as_tibble() %>% rename(white = value)
white_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(white) %>%
  as_tibble() %>% rename(white = value)

education_char <- df %>% filter(in_char_dfm == 1) %>% pull(education) %>%
  as_tibble() %>% rename(education = value)
education_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(education) %>%
  as_tibble() %>% rename(education = value)

age_char <- df %>% filter(in_char_dfm == 1) %>% pull(age) %>%
  as_tibble() %>% rename(age = value)
age_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(age) %>% 
  as_tibble() %>% rename(age = value)

female_char <- df %>% filter(in_char_dfm == 1) %>% pull(female) %>% 
  as_tibble() %>% rename(female = value)
female_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(female) %>%
  as_tibble() %>% rename(female = value)



antipathy_char <- df %>% filter(in_char_dfm == 1) %>% pull(antipathy) %>% 
  as_tibble() %>% rename(antipathy = value)
antipathy_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(antipathy) %>%
  as_tibble() %>% rename(antipathy = value)

treatment_char <- df %>% filter(in_char_dfm == 1) %>% pull(treatment) %>% 
  as_tibble() %>% rename(treatment = value)
treatment_consid <- df %>% filter(in_consid_dfm == 1) %>% pull(treatment) %>%
  as_tibble() %>% rename(treatment = value)


covar_char <- as_tibble(cbind(white_char, education_char, age_char, female_char, antipathy_char, treatment_char))
covar_consid <- as_tibble(cbind(white_consid, education_consid, age_consid, female_consid, antipathy_consid, treatment_consid))

# Combine for joint analysis
covar_stacked <- rbind(covar_char, covar_consid)
covar_stacked$characteristics <- factor(c(rep(1, nrow(covar_char)),
                                 rep(0, nrow(covar_consid))))
covar_stacked$female <- factor(covar_stacked$female)

#create numerical treatment variable for ease of dealing with in by_strata later
covar_stacked <- covar_stacked %>% 
  mutate(treatment_dbl = case_when(treatment == "SyrianMuslim" ~ 1,
                                   treatment == "SyrianChristian" ~ 2,
                                   treatment == "SyrianNoReligion" ~ 3, 
                                   treatment == "Control" ~ 4 
                                   ))
covar_stacked <- covar_stacked %>% 
  mutate(treatment_fact = factor(treatment_dbl, levels = c(4,1,2,3))) 
  




######################################################################

elapsedtime <- Sys.time()
char_key6_fit <- keyATM(docs= char_keydocs,       # text input
                        no_keyword_topics = 0,    # number of topics without keywords
                        keywords= keywords6,      # keywords
                        model = "covariates",     # select the model
                        model_settings    = list(covariates_data    = covar_char,    # covariate data
                                                 covariates_formula = ~ white + education + age + female,   # formula to fit covarite
                                                 mh_use             = FALSE),
                        options           = list(seed = 250)
)
print(elapsedtime <- Sys.time() - elapsedtime)

consid_key6_fit <- keyATM(docs= consid_keydocs,   # text input
                        no_keyword_topics = 0,    # number of topics without keywords
                        keywords= keywords6,      # keywords
                        model = "covariates",     # select the model
                        model_settings    = list(covariates_data    = covar_consid,    # covariate data
                                                 covariates_formula = ~ white + education + age + female,   # formula to fit covarite
                                                 mh_use             = FALSE),
                        options           = list(seed = 250)
)

elapsedtime <- Sys.time()
stacked_key6_fit <- keyATM(docs= stacked_keydocs,   # text input
                          no_keyword_topics = 0,    # number of topics without keywords
                          keywords= keywords6,      # keywords
                          model = "covariates",     # select the model
                          model_settings    = list(covariates_data    = covar_stacked,    # covariate data
                                                   covariates_formula = ~ characteristics + white + age + female +
                                                     education + antipathy + treatment_fact,   # formula to fit covariate
                                                   mh_use             = FALSE),
                          options           = list(seed = 250, iterations = 3000, 
                                                   thinning = 10, store_theta = T )
)

save(stack_index, stacked_dfm, stacked_keydocs, stacked_key6_fit, file = "stacked_model.Rdata")

print(elapsedtime <- Sys.time() - elapsedtime)
load("stacked_model.Rdata")

library(xtable)
xtable(top_words(stacked_key6_fit, n = 20), booktabs = T)

## Save output for quick and easy use later.
save(keywords6, char_index, consid_index, stack_index,
     stacked_dfm, stacked_keydocs, stacked_key6_fit,
     char_dfm_stem, char_keydocs, char_key6_fit, 
     consid_dfm_stem, consid_keydocs, consid_key6_fit,
     file = "covar_results_notreat.Rdata"
     )

View(top_words(char_key6_fit, n =30))
View(top_words(consid_key6_fit, n =30))
View(top_words(stacked_key6_fit, n =30))

strata_income <- by_strata_DocTopic(stacked_key6_fit,
                                    by_name= "income",
                                    by_values = seq(1,11,1),
                                    parallel = F)
strata_income_means <- by_strata_DocTopic(stacked_key6_fit,
                                    by_var= "income",
                                    labels = seq(1,11,1),
                                    posterior_mean = T,
                                    parallel = F)
strata_income_gender <- by_strata_DocTopic(stacked_key6_fit,
                                          by_var= "female",
                                          labels = c("male", "female"),
                                          posterior_mean = T,
                                          parallel = F)
strata_income_gender <- by_strata_DocTopic(stacked_key6_fit,
                                           by_name= "female",
                                           by_values = c(0,1),
                                           posterior_mean = T,
                                           parallel = F)



#########################################
  ###### regression of stacked topics 
  
stacked_thetas <- stacked_key6_fit$theta
# docs 1:2462 are characteristics, docs 2463:4919 are considerations
stacked_thetas_char <- as_tibble(stacked_thetas[1:2462,])
names(stacked_thetas_char) <- c("Legitimacy Char.", "Sympathy Char.",
                                  "Economic Char.", "Culture Char.",
                                  "Illegality Char.", "Uncertainty Char."
  )
stacked_thetas_char$index <- char_index
stacked_thetas_consid <- as_tibble(stacked_thetas[2463:4919,])
names(stacked_thetas_consid) <- c("Legitimacy Consid.", "Sympathy Consid.",
                                    "Economic Consid.", "Culture Consid.",
                                    "Illegality Consid.", "Uncertainty Consid."
  )
stacked_thetas_consid$index <- consid_index
merged_thetas <- merge(stacked_thetas_char, stacked_thetas_consid,
                         by="index")
predictors <- "`Legitimacy Consid.`+`Sympathy Consid.`+`Economic Consid.`+`Culture Consid.`+`Illegality Consid.`"
(legit.lm <- lm(data=merged_thetas, 
                  formula= as.formula(paste("`Legitimacy Char.` ~", predictors))))
(symp.lm <- lm(data=merged_thetas, 
               formula= as.formula(paste("`Sympathy Char.` ~ ", predictors))))
(econ.lm <- lm(data=merged_thetas, 
               formula= as.formula(paste("`Economic Char.` ~ ", predictors))))
(cult.lm <- lm(data=merged_thetas, 
                 formula= as.formula(paste("`Culture Char.` ~ ", predictors))))
(illeg.lm <- lm(data=merged_thetas, 
                  formula= as.formula(paste("`Illegality Char.` ~ ", predictors))))
(uncert.lm <- lm(data=merged_thetas, 
                   formula= as.formula(paste("`Uncertainty Char.` ~ ", predictors))))
  #save(uncert.lm, illeg.lm, cult.lm, econ.lm, symp.lm, legit.lm, file="regressions.Rdata")
  #load("regressions.Rdata")
library(stargazer)

stargazer(legit.lm, symp.lm, econ.lm, cult.lm, illeg.lm, uncert.lm, 
            dep.var.labels = c("Legitimacy", "Sympathy", "Economic",
                               "Culture", "Illegality", "Uncertainty"),
            omit.stat = "all", 
            title="Predicting characteristics topics using considerations topics, combined model fitting",
            notes ="Reference category (constant) is the Uncertainty Considerations topic",
          column.sep.width = "-10pt",
          font.size = "small"
  )





############################################3
#predicted means by treatment group
################

load("stacked_model.Rdata")
top_words(stacked_key6_fit, n =25)
#run by_strata_DocTopic
strata_question <- by_strata_DocTopic(stacked_key6_fit,
                                           by_var= "characteristics1",
                                           labels = c(0,1),
                                           posterior_mean = T,
                                           parallel = F)
strata_treatment <- by_strata_DocTopic(stacked_key6_fit,
                                       by_var= "treatment_fact",
                                       labels = c(1,2,3,4),
                                       posterior_mean = T,
                                       parallel = F)

plot(strata_question, var_name = "characteristics1", 
     topics = c(1, 2, 3, 4, 5, 6)) + xlab("Question (1 = Characteristics)")



thetas <- data.frame(covar_stacked, stacked_key6_fit$theta)
consid_thetas <- thetas %>% filter(characteristics==0)
char_thetas <- thetas %>% filter(characteristics==1)

muslimconsid_thetas <- consid_thetas %>% filter(treatment_fact==1)
noreligconsid_thetas <- consid_thetas %>% filter(treatment_fact==3)
controlconsid_thetas <- consid_thetas %>% filter(treatment_fact==4)
christianconsid_thetas <- consid_thetas %>% filter(treatment_fact==2)

muslimchar_thetas <- char_thetas %>% filter(treatment_fact==1)
noreligchar_thetas <- char_thetas %>% filter(treatment_fact==3)
controlchar_thetas <- char_thetas %>% filter(treatment_fact==4)
christianchar_thetas <- char_thetas %>% filter(treatment_fact==2)

  # also do by treatment
thetas_by_type <- c(apply(consid_thetas[,10:15], MARGIN = 2, mean),
      apply(char_thetas[,10:15], MARGIN = 2, mean))
thetas_by_type <- data.frame(mean= thetas_by_type, topic = names(thetas_by_type),
                             type = rep(c("Considerations", "Characteristics"), each =6))

ggplot(data = thetas_by_type, aes (x=topic, y = mean, group=type)) + geom_line(aes(color=type))

thetas_by_treat <- c(apply(muslimconsid_thetas[,10:15], MARGIN = 2, mean),
                     apply(christianconsid_thetas[,10:15], MARGIN = 2, mean),
                     apply(noreligconsid_thetas[,10:15], MARGIN = 2, mean),
                     apply(controlconsid_thetas[,10:15], MARGIN = 2, mean),
                     apply(muslimchar_thetas[,10:15], MARGIN = 2, mean),
                     apply(christianchar_thetas[,10:15], MARGIN = 2, mean),
                     apply(noreligchar_thetas[,10:15], MARGIN = 2, mean),
                     apply(controlchar_thetas[,10:15], MARGIN = 2, mean)
                     )
thetas_by_treat <- data.frame(mean= thetas_by_treat,
                             Topic = rep(c("Legitimacy", "Sympathy", "Economic", "Culture", "Illegality", "Uncertainty"),8),
                             Question = rep(c("Considerations", "Characteristics"), each =6*4),
                             Treatment = rep(c("Muslim", "Christian", "No Religion", "Control"),
                                        each =6, times=2))
treat_mean_thetas <- ggplot(data = thetas_by_treat,
                            aes (x=Topic, y = mean, group=Treatment)) +
  geom_point(aes(color=Treatment), shape=4,
             position=position_dodge(width=0.2)) +
  coord_flip()+
  theme_bw() + ylab("Mean Theta") + facet_wrap(~Question)


#plot model fit - verify that the log likelihoods have converged 
plot_modelfit(stacked_key6_fit)

#plot alphas - this doesn't work since alpha objects are not stored? 
plot_alpha(stacked_key6_fit)

#plot pis 
stacked_key6_fit
plot_pi(stacked_key6_fit)









############################################################
####EVERYTHING BELOW THIS IS MADE EXTRANEOUS BY MY UPDATING KEYATM
#####################################




#write function to get estimates and upper and lower bounds 
get_estimates <- function(num){
  dat <- strata_treatment[["theta"]][[num]] %>% as.data.frame()
  means <- dat %>% map_dbl(mean)
  lower <- map_dbl(dat, function(.x) { 
    return(quantile(.x, 0.1))
    })
  upper <- map_dbl(dat, function(.x) { 
    quantile(.x, 0.9)
    })
  vals <- rbind(lower, means, upper) %>% as_tibble() 
  return(vals)

}

syrianmuslim <- get_estimates(num = 1)[,1:6] #7th column is the iteration, about which we do not care 
syriannoreligion <- get_estimates(num = 2)[,1:6]
control <- get_estimates(num = 3)[,1:6]
syrianchristian <- get_estimates(num = 4)[,1:6]

treatment_predicted <- rbind(t(syrianmuslim), t(syriannoreligion), t(control), t(syrianchristian)) %>% 
  as_tibble()
#as_tibble() so that we can create a variable in the next step without it turning into a list

treatment_predicted$treatment <- c(rep("Syrian Muslim", 6),rep("Syrian No Religion", 6),
                             rep("Control", 6), rep("Syrian Christian", 6))



treatment_predicted$topic <- rep(c("Legitimacy", "Sympathy", "Economic", "Culture", "Illegality", "Uncertainty"), 4) %>% 
  t() %>% as_tibble() %>% t()

names(treatment_predicted) <- c("lower","mean","upper","treatment","topic")


treatment_predicted %>% 
  ggplot(., aes(x = topic, y = mean)) + 
  geom_point()+ 
  geom_linerange(aes(ymin = lower, ymax = upper)) + 
  ylab("Topic proportions (80% CI)") + 
  coord_flip() + 
  facet_wrap(~treatment)



