library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)


salary <- read.csv("misc/NBA_season1718_salary.csv")
season <- read.csv("misc/Seasons_Stats.csv")


## Transforming statistict PER GAME

##    Glossary
#PPG -> Point Per Game
#APG -> Assists Per Game
#RPG -> rebounds per game
#MPG -> minutes per game
#TOPG -> Turn Over Per Game
#BPG -> blocks per game
#SPG -> steals per game

## complete glossary: https://www.basketball-reference.com/about/glossary.html

# season dataframe creation
stats17 <- 
  season %>% filter(Year >= 2017) %>% #filter by season 2017 and above
  select(Year:G, MP, PER, FG:PTS) %>% # used variables 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, # create variable per game (check line 15 of this code)
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) 

# merging dataframe of salary with stats17 by player name
stats_salary <- merge(stats17, salary, by.x = "Player", by.y = "Player")
#change variable named "season17_18" to "salary" (Y variable)
names(stats_salary)[40] <- "salary"
#drop team variable
stats_salary <- stats_salary[-39]
#drop rows with null values
stats_salary <- na.omit(stats_salary)


#################################################################
## data vis!
  #correlation
stats_salary_cor <- 
  stats_salary %>% 
  select(salary, PPG, MPG, TOPG, RPG, PER, SPG, APG, Age, G, FG)
ggpairs(stats_salary_cor)
################################################################

# transform Pos (position) to dummy
library("fastDummies")

# Point Guard (PG) - Armador
# Shooting Guard (SG) - Ala-Armador
# Small Forward (SF) - Ala
# Power Forward (PF) - Ala-Pivô
# Center (C) - Pivô

stats_salary <- dummy_columns(.data = stats_salary,
                              select_columns = "Pos",
                              remove_selected_columns = TRUE,
                              remove_most_frequent_dummy = TRUE)


# creating dataframe with bests variables
stats_salary_regression <- stats_salary %>% select(salary, MPG:SPG, Age, G, FG, Pos_C:Pos_SF)

# initial model using stats_salary_regression dataframe
model <- lm(salary~., data=stats_salary_regression)
summary(model)
# some Betas not approved on T test

#need to pass on a stepwise model
stepwised_model <- step(object = model,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(stepwised_model)
# all good betas
###############################################################################
library("nortest")
#shapiro francia test
sf.test(stepwised_model$residuals)
# Shapiro-Francia normality test
# data:  stepwised_model$residuals
# W = 0.98752, p-value = 0.00194
  # this p-value means this model need to pass on box-cox procedure

##############################################################################
library("olsrr")

# Breusch Pagan Test for Heteroskedasticity
ols_test_breusch_pagan(stepwised_model)
# Prob > Chi2   =    1.423997e-26 <- Heteroskedasticity 


################################################################################
## BOX-COX PRECEDURE ###########################################################
library("car")

## adding variables with fitted_values and residuals
stats_salary_regression$yhat <- stepwised_model$fitted.values
stats_salary_regression$error <- stepwised_model$residuals

# generating LAMBDA
lambda_bc <- powerTransform(stats_salary_regression$salary)

#adding lambda to DF
stats_salary_regression$bc_stats_salary <- (((stats_salary_regression$salary ^ 
                                                lambda_bc$lambda) - 1) / 
                                              lambda_bc$lambda)

## new Box-cox model ###########################################################
model_bc <- lm(formula = bc_stats_salary ~. -salary -yhat -error,
               data = stats_salary_regression)


## stepwise again  #############################################################
bc_setp_model <- step(object = model_bc,
                      k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(bc_setp_model)

## heteroscedasticity test ####################################################
ols_test_breusch_pagan(bc_setp_model)
#Prob > Chi2   =    0.9229767 <- NO Heteroskedasticity

# multicollinearity test
ols_vif_tol(bc_setp_model)
# Variables Tolerance      VIF
# 1       MPG 0.2015725 4.960995
# 2       PPG 0.2610415 3.830809
# 3       RPG 0.6037379 1.656348
# 4       Age 0.9577318 1.044134
# 5         G 0.6373543 1.568986

sf.test(bc_setp_model$residuals)
