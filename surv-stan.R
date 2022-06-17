rm(list=ls())
options(timeout=1000)
# library
library(rstan)
# options(mc.cores=parallel::detectCores())
library(dplyr)
library(ggplot2)

set.seed(10)

# rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# generate data
n_obs <-c(1:150)
species<-sample(c('anon_murica', 'antia_toxi', 'termi_ivori', 'bafia_niti', 'ricino_heudelo', 'bombax_puero'), 150, replace=TRUE)
numb_ind <-sample(1: 25 , size = 150, replace = T)
plot <- sample(c('88-2', '88-3', '88-4', '88-9', '90-1', '90-8', '91-12', '92-1', '92-1', '95-3'), 150, replace=TRUE)
area_plat <- sample(c(1155, 6000, 1040, 1837, 22008, 1568, 6552, 4312, 1456), 150, replace=TRUE)
survival<-sample(c('1', '0'), 150, replace=TRUE)
spacing_s<-sample(c('3x3.5', '5x5', '3x5', '2x3.5'), 150, replace=TRUE)
type_plant<-sample(c('pure', 'mixture'), 150, replace=TRUE)
year_plant<-sample(c(1988, 1990, 1991, 1992, 1995), 150, replace=TRUE)


df <- data.frame(n_obs, plot, area_plat, year_plant, species, survival, spacing_s, type_plant, numb_ind)

# write.csv(df, "df.csv", row.names=FALSE)
# data <- read.csv("df.csv")


# prepare data

# df$survival<- as.factor(as.character(df$survival))
df$species<- as.factor(as.character(df$species))
df$plot<- as.factor(as.character(df$plot))

# df$spacing_s<- as.factor(as.character(df$spacing_s))
# df$type_plant<- as.factor(as.character(df$type_plant))


# Inference du le taux de survie et determination des paramètres 
# Model employé

# run surv-stan.stan model
## Preparing variables

df$species_number <-as.numeric(as.factor(df$species))
df$plot_number <-as.numeric(as.factor(df$plot))
df$survival_number <-as.numeric(as.factor(df$survival))-1

## Declare all the variables of the Data block of the Stan model

surv_data<-list(
  n_obs=dim(df)[1],
  n_species=length(levels(as.factor(df$species))),
  species=df$species_number,
  n_plot=length(levels(as.factor(df$plot))),
  plot=df$plot_number,
  survival=df$survival_number,
  t= 2022-df$year_plant,
  C= df$numb_ind/df$area_plat
)

## Run the model with the stan function on you data

surv_model <- stan(
  file = "surv-stan.stan",  # Stan program
  data = surv_data,    # named list of data
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000)            # total number of iterations per chain


# result of the modelling

## Evaluation of the model
surv_model
class(surv_model)

list_of_draws <- extract(surv_model)
print(names(list_of_draws))

library(bayesplot)

color_scheme_set("teal")

mcmc_areas(surv_model, pars = c("theta_s[1]", "theta_s[2]", "theta_s[3]", "theta_s[4]", "theta_s[5]", "theta_s[6]"), prob = 0.89) +
  scale_y_discrete(expand = c(0, 0))

mcmc_areas(surv_model, pars = c("theta_p[1]", "theta_p[2]", "theta_p[3]", "theta_p[4]", "theta_p[5]", "theta_p[6]", "theta_p[7]", "theta_p[8]", "theta_p[9]"), prob = 0.89) +
  scale_y_discrete(expand = c(0, 0))

glimpse(df)



summary(surv_model)

# extract fitted survivorship values
fitted_expo <- rstan::extract(surv_model, pars = "theta_s")$theta_s


# plot le model object
plot(surv_model, pars="theta_s")
plot(surv_model, pars="theta_p")

traceplot(surv_model, pars=c("theta_s", "theta_p", "theta_c", "sigma_p"))

# Explore stan object
a2 <- as.array(surv_model)
e2 <- extract(as.array, permuted = FALSE)


library(EpiNow2)
e2 <- extract_stan_param(surv_model) 


# https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
# http://blackwell.math.yorku.ca/MATH6635/files/Stan_first_examples.html

prop.table(table(df$plot_number, df$survival_number))*100



# Explore parameter computed

library(rstan)
param <- extract(surv_model)
theta_s <- 
  mean(param$theta_s)
theta_p <- 
  mean(param$theta_p)
theta_p <-
  mean(param$sigma_p)
theta_c <-
  mean(param$theta_c)

lp <-
  mean(param$lp__)
