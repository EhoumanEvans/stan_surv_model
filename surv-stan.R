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
plot <- sample(c('88-2', '88-3', '88-4', '88-9', '90-1', '90-8', '91-12', '92-1', '92-1', '95-3'), 150, replace=TRUE)
area_plat <- sample(c('1155', '6000', '1040', '1837', '22008', '1568', '6552', '4312', '1456'), 150, replace=TRUE)
survival<-sample(c('1', '0'), 150, replace=TRUE)
spacing_s<-sample(c('3x3.5', '5x5', '3x5', '2x3.5'), 150, replace=TRUE)
type_plant<-sample(c('pure', 'mixture'), 150, replace=TRUE)
year_plant<-sample(c(1988, 1990, 1991, 1992, 1995), 150, replace=TRUE)

df <- data.frame(n_obs, plot, area_plat, year_plant, species, survival, spacing_s, type_plant)


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
df$species_number <-as.numeric(as.factor(df$species))
df$plot_number <-as.numeric(as.factor(df$plot))
df$survival_number <-as.numeric(as.factor(df$survival))

# Declare all the variable of the Data block of the Stan model

surv_data<-list(
  n_obs=dim(df)[1],
  n_species=length(levels(as.factor(df$species))),
  species=df$species_number,
  n_plot=length(levels(as.factor(df$plot))),
  plot=df$plot_number,
  survival=df$survival_number,
  t= 2022-df$year_plant,
  C= 
)


surv_model <- stan(
  file = "surv-stan.stan",  # Stan program
  data = surv_data,    # named list of data
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000)            # total number of iterations per chain


# extract fitted survivorship values
fitted_expo <- rstan::extract(surv_model, pars = "theta_s")$theta_s


# plot le model object
plot(surv_model, pars="theta_s")
traceplot(surv_model, pars=c("theta_s", "theta_p", "theta_c", "sigma_p"))



test


# ========fichier stan
