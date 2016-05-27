library(car)
library(texreg)
library(dplyr)
library(broom)
library(foreign)
library(broom)
library(dplyr)
library(coefplot)
library(ggplot2)
library(coefplot)
library(stargazer)


# VARIABLES OF INTEREST IN YALE DF: 
#     yal363 ---- "difficulty to find out"
#     cc410  ---- candidate choice (1=-1/Obama) (2=1/McCain) "Pres Vote choice" 
#     v243  ---   "Ideology (5=very cons.4 = cons. 3=moderate/not sure; 2 = lib, 1=very lib.)"
#     v211,   --- Race (white=race1 gen black=race2 gen hispanic=race3 gen otherrace=race4+race5+race6+race7+race8
#     v213 ---- education 
#     v246 ---- family_income 
#     v207 ---- birth_year
#     cc329 ---- union household  "Union HH (1=yes)" (1=0) (2 3 4=1) (*=.), 
#     v208  ---- gender  
#     cc403 ---- reported turnout in 2008 (1=yes)
#     v203 ----- voter registration status  (1=1) (2 3=0) (*=.),  (0=no/don't know, 1=yes)"
#     v208-1  -   label var gender "Female (1=yes)" 


#*Family Income
#gen income=v246
#replace income =. if income>15
#gen incomemis = 0
#replace incomemis = 1 if income==15 
#label var incomemis "Income Missing"
#label var income "Income (1=<10k; 14=>150k; 15=RF/Missing)"


#"Education (1=No HS; 6=Post-grad)"
#label var educD1 "Education: No HS"
#label var educD3 "Education: Some college"
#label var educD4 "Education: 2-year college"
#label var educD5 "Education: 4-year college"
#label var educD6 "Education: Post-graduate"



yaledata1.df <- read.dta("data//ReplicationArchive//sourcedata//YaleCCES2008_SecretBallotItems.dta")

vote <- yaledata1.df %>%
  select(yal363, cc410, v243, v211, v213, v246, v207, cc329, v208, cc403, v203) %>%
  # Give variables meaningful names
  rename(vote_choice = cc410,
         ballot_secrecy = yal363,
         ideology = v243,
         race = v211,
         education = v213,
         family_income = v246,
         birth_year = v207,
         unionHH = cc329, # union household  "Union HH (1=yes)" (1=0) (2 3 4=1) (*=.), 
         gender = v208,  #  gender   
         registration_status = v203 # voter registration status  (1=1) (2 3=0) (*=.),  (0=no/don't know, 1=yes)"
          ) %>%

  # drop third party or missing vote choice
  filter(vote_choice %in% c("john mccain (republican)", "barack obama (democrat)")) %>%
  # New variables
  # - ideology_strength: strength of ideology. 
  # - obama: did they vote for obama
  # - liberal: 1 if voted for obama
  # - ballot_secrecy_binary: 1 if think ballot is secret. (ya1363 < 4)
  mutate(obama = vote_choice %in% "barack obama (democrat)",
         ideology = as.numeric(ideology),
         ideology_strength = abs(ideology - 3),
         liberal = as.integer(ideology <= 3),
         age = 2009 - birth_year,
         age_squared = (age^2)/100,
         ballot_secrecy_binary = as.integer(ballot_secrecy) < 4,
         strategic_voter =  
           ifelse(vote_choice == "barack obama (democrat)", 
                  ifelse(ideology > 3, 1, 0), 
                  ifelse(vote_choice == "john mccain (republican)",
                         ifelse(ideology < 3, 1, 0), 
                         0)))


# Regress (Voted obama) on the interaction of ideology strength, liberal and belief in ballot secrecy
model1 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary, data = vote)
         
# Add other regressions ... 
model2 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary +
     age_squared, data = vote)

model3 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + age 
             + race + age_squared + education + family_income, data = vote) 

model4 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + age + race + 
     age_squared, data = vote)

model5 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary, data = vote)
model6 <- lm(strategic_voter ~ liberal + ideology_strength + ballot_secrecy_binary, data = vote)
model7 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + race, data = vote)
model8 <- lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + unionHH, data = vote)
model9 <- lm(strategic_voter ~ liberal + ideology_strength + ballot_secrecy_binary + unionHH, data = vote)
model10 <- lm(strategic_voter ~ ballot_secrecy_binary + ideology_strength + race + 
                education + family_income + unionHH + gender + 
                registration_status + liberal + age + age_squared, data = vote)

model12 <- lm(strategic_voter ~ ballot_secrecy_binary * liberal * ideology_strength + race + 
                education +  family_income + unionHH + gender + registration_status  + 
                age + age_squared, data = vote)

model13 <- lm(strategic_voter ~ ballot_secrecy_binary * ideology * ideology_strength + race + 
                education +  family_income + unionHH + gender + registration_status  + 
                age + age_squared, data = vote)


model_race <-  lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary * race, data = vote)

model_race_union <-  lm(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary * unionHH * race, data = vote)

model_race_union2 <- lm(strategic_voter ~ ideology_strength * ballot_secrecy_binary * unionHH * race, data = vote)


#Determining the effect sizes of RACE BLACK and RACE WHITE

obs1 <- data.frame(race = "black",
                   ideology_strength = mean(vote$ideology_strength, na.rm = TRUE),
                   ballot_secrecy_binary = TRUE,
                   education =  "some college",
                   family_income = "$50,000 - $59,999",
                   unionHH = "i and no one in household union member",
                   gender = "male", 
                   registration_status = "yes",
                   liberal = mean(vote$liberal, na.rm = TRUE),
                   age = mean(vote$age, na.rm = TRUE),
                   age_squared  = mean(vote$age_squared, na.rm = TRUE))

obs2 <- mutate(obs1, race = "white")

newdata <- bind_rows(obs1, obs2)

pred1 <- predict(model12, newdata)

pred_CI <- predict(model12, newdata, interval = "confidence", level = .95)

mw_pred_tidy <- tidy(pred_CI)
mw_pred_tidy <- mutate(mw_pred_tidy, scenario = c("black", "white"))
ggplot(mw_pred_tidy, aes(x = scenario, y = fit, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Effect of Race on Likelihood of Strategic Voting") +
  labs(y="Predicted Effect on Strategic Voting",x="Race") 

#Determining the effect sizes of Ballot Secrecy Binary TRUE and FALSE 

obs3 <- data.frame(race = "white",
                   ideology_strength = mean(vote$ideology_strength, na.rm = TRUE),
                   ballot_secrecy_binary = TRUE,
                   education =  "some college",
                   family_income = "$50,000 - $59,999",
                   unionHH = "i and no one in household union member",
                   gender = "male", 
                   registration_status = "yes",
                   liberal = mean(vote$liberal, na.rm = TRUE),
                   age = mean(vote$age, na.rm = TRUE),
                   age_squared  = mean(vote$age_squared, na.rm = TRUE))

obs4 <- mutate(obs3, ballot_secrecy_binary = FALSE)

newdata <- bind_rows(obs3, obs4)

pred1 <- predict(model12, newdata)

pred_CI <- predict(model12, newdata, interval = "confidence", level = .95)

mw_pred_tidy <- tidy(pred_CI)
mw_pred_tidy <- mutate(mw_pred_tidy, scenario = c("Believe in Secrecy", "Doubt Secrecy"))
ggplot(mw_pred_tidy, aes(x = scenario, y = fit, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Effect of Belief in Ballot Secrecy on Likelihood of Strategic Voting") +
  labs(y="Predicted Effect on Strategic Voting",x="Belief in Ballot Secrecy") 

#Determining the effect sizes of Ideological Strength  

obs5 <- data.frame(race = "white",
                   ideology_strength = 1,
                   ballot_secrecy_binary = TRUE,
                   education =  "some college",
                   family_income = "$50,000 - $59,999",
                   unionHH = "i and no one in household union member",
                   gender = "male", 
                   registration_status = "yes",
                   liberal = mean(vote$liberal, na.rm = TRUE),
                   age = mean(vote$age, na.rm = TRUE),
                   age_squared  = mean(vote$age_squared, na.rm = TRUE))

obs6 <- mutate(obs5, ideology_strength = 2)

newdata <- bind_rows(obs5, obs6)

pred1 <- predict(model12, newdata)

pred_CI <- predict(model12, newdata, interval = "confidence", level = .95)

mw_pred_tidy <- tidy(pred_CI)
mw_pred_tidy <- mutate(mw_pred_tidy, scenario = c("Strong ideology", "Moderate Ideology"))
ggplot(mw_pred_tidy, aes(x = scenario, y = fit, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Effect of Ideological Strength on Likelihood of Strategic Voting") +
  labs(y="Predicted Effect on Strategic Voting",x="Strength of Ideology") 


#Determining the effect sizes of Ideology   

ide_obs1 <- data.frame(race = "white",
                   ideology_strength = 1,
                   ballot_secrecy_binary = TRUE,
                   education =  "some college",
                   family_income = "$50,000 - $59,999",
                   unionHH = "i and no one in household union member",
                   gender = "male", 
                   registration_status = "yes",
                   ideology = 1,
                   age = mean(vote$age, na.rm = TRUE),
                   age_squared  = mean(vote$age_squared, na.rm = TRUE))

ide_obs2 <- mutate(ide_obs1, ideology = 2)
ide_obs3 <- mutate(ide_obs1, ideology = 3)
ide_obs4 <- mutate(ide_obs1, ideology = 4)
ide_obs5 <- mutate(ide_obs1, ideology = 5)

newdata <- bind_rows(ide_obs1, ide_obs2, ide_obs3, ide_obs4, ide_obs5)

pred1 <- predict(model13, newdata)

pred_CI <- predict(model13, newdata, interval = "confidence", level = .95)

mw_pred_tidy <- tidy(pred_CI)
mw_pred_tidy <- mutate(mw_pred_tidy, scenario = c("1", "2", "3", "4", "5"))
ggplot(mw_pred_tidy, aes(x = scenario, y = fit, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Effect of Ideolology  on Likelihood of Strategic Voting") +
  labs(y="Predicted Effect on Strategic Voting",x="Ideology: 1 = Very Liberal, 3 = Moderate, 5 = Very Conservative") 



## This generates the code to generate the html regression table in Markdown
stargazer(model10, model12, model13, model1, type = "html", 
          title            = "Regression models",
          dep.var.labels   = "Predicted Effect on Voting Strategically ",
          star.char = c("&#9786;", "&#9786;&#9786;", "&#9786; &#9786; &#9786;"))


## Functions for fun in class

create_bool_west_coast <- function(variable){
  state_name <- v206
  west_coast_state <- c("California", "Oregon", "Washington", "New York")
  coastal_states <- state_name %in% west_coast_state
  return(coastal_states)
}

#  FOR LOOPS
list_numbers <- sample(x = 1:100, size = 10, replace = FALSE)

for (i in list_numbers) {
  print(paste0("2 times ", i, " is ", i * 2))
}

# another one
list_numbers <- sample(x = 1:100, size = 10, replace = FALSE)

for (i in list_numbers){
  if (i > 50) {
    print(paste0(i, "is larger than 50"))
  } else if i > 20 {
    print(paste0(i, "is larger than 20"))
  } else if i < 20 {
    print(paste0(i, "is less than 20"))    
  }
    
} 
  

}


create_bool_west_coast(v2) <- coastal_states(v206)


### Adding Fold CoVariation tests from Jeff Arnold's code
library("car")
library("ggplot2")
set.seed(123125)
data("vote")

# Select folds for each observation
#' Number of observations
n <- nrow(vote)
# Number of folds
n_folds <- 5
#' Folds for each observation
#' Using the remainder of dividing by number of folds gives equal numbers
#' Using sample shuffles the observations.
fold <- sample((seq_len(n) %% n_folds) + 1)

#' Formula for the model
form <- strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary

# Vector to hold MSE of the folds
mse_folds <- rep(NA, n_folds)

# Loop through each fold
for (i in seq_len(n_folds)) {
  # Test obervations
  test_data <- vote[which(fold == i), ]
  # Training observations
  training_data <- vote[which(fold != i), ]
  # estimate model on training data
  mod <- lm(form, data = training_data)
  # Get residuals for predictions
  err <- predict(mod, newdata = test_data) - test_data[["vote"]]
  mse_folds[i] <- mean(err ^ 2)
}
cv_mse <- mean(mse_folds)

#' But the point of using Cross-Validation is to compare several models
#' 
#' We could copy-and paste the code above, and change the formula each time.
#' Instead, we will use a for-loop to run the model for a set of models
#' 

             

#' Store models in a list
formulae <- list(strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary * 
                   unionHH * race,
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary * race,
                 strategic_voter ~ ballot_secrecy_binary * ideology * ideology_strength + race + 
                   education +  family_income + unionHH + gender + registration_status  + 
                   age + age_squared,
                 strategic_voter ~ ballot_secrecy_binary * liberal * ideology_strength + race + 
                   education +  family_income + unionHH + gender + registration_status  + 
                   age + age_squared,
                 strategic_voter ~ ballot_secrecy_binary + ideology_strength + race + 
                   education + family_income + unionHH + gender + 
                   registration_status + liberal + age + age_squared,
                 strategic_voter ~ liberal + ideology_strength + ballot_secrecy_binary + unionHH,
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + unionHH,
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary, 
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary +
                   age_squared, 
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + age 
                 + race + age_squared + education + family_income, 
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + 
                   age + race + age_squared, 
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary, 
                 strategic_voter ~ liberal + ideology_strength + ballot_secrecy_binary, 
                 strategic_voter ~ liberal * ideology_strength * ballot_secrecy_binary + race,
                 strategic_voter ~ ideology_strength * ballot_secrecy_binary * unionHH * race)

#' Run the model for each formula
models <- list()
for (i in seq_along(formulae)) {
  models[[i]] <- lm(formulae[[i]], data = vote)
}
# Example: model 1
models[[1]]

#' Dataset
dataset <- vote
#' Use 10-fold cross-validation
n_folds <- 10

#' Loop over each model, and calculate cross-validated MSE
model_cv <- list()
for (i in seq_along(models)) {
  #' Extract the formula from the model
  form <- as.formula(models[[i]])
  cat("Running model: ", format(form), "\n")
  #' number of obs
  n <- nrow(dataset)
  #' folds
  fold <- sample((seq_len(n) %% n_folds) + 1)
  #' dependent variable in the model
  yvar <- as.character(form[[2]])
  # Vector to hold MSE of the folds
  mse_folds <- rep(NA, n_folds)
  # Loop through each fold
  for (k in seq_len(n_folds)) {
    # Test obervations
    data_test <- dataset[which(fold == k), , drop = FALSE]
    # Training observations
    data_train <- dataset[which(fold != k), , drop = FALSE]
    # estimate model on training data
    mod_train <- lm(form, data = data_train)
    # Predicted values for new data
    yhat <- predict(mod_train, newdata = data_test)
    # Prediction erorrs
    err <- yhat - data_test[[yvar]]
    # Mean squared error
    mse_folds[k] <- mean(err ^ 2)
  }
  # Cross-validated MSE for that model
  cv_mse <- mean(mse_folds)
  
  # In-sample MSE
  mse_train <- mean(residuals(models[[i]]) ^ 2)
  
  # Save results in a data frame
  model_cv[[i]] <- data_frame(model = i,
                              formula = format(form),
                              mse_cv = cv_mse,
                              mse_train = mse_train)
}
model_cv <- bind_rows(model_cv)

model_cv


#' Other functions:
#' 
#' - package boot: function cv.glm
#' - package DAAG: cv.lm
#' - packages: caret, mlr

