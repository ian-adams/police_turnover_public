library(here) # make sure you're in the project
library(readr)
library(readxl)
library(tidyverse)
library(janitor)
library(ggthemes)
library(dplyr)
library(ggplotify)
library(forecast)
library(ggplot2)
library(reshape2)
library(cowplot)
library(patchwork)
library(ggpubr)
library(geomtextpath)
library(rstanarm)
library(tseries)
library(bayestestR)
library(tidybayes)
library(rstan)
library(hdi)
library(grafify)
library(bayesforecast)
library(BEST)
library(bsts)
library(Boom)
library(grafify)

options(mc.cores = 4)



#load data====
## (just import the "turnover_merged.rds" from the import script)
merged_df <- readRDS(here("data","public_data.rds"))

# Add pre-post intervention for grouping
merged_df$pre_post<-ifelse(merged_df$Date>"2020-05-01",1,0)


## Raw plots (Figs 1 and 2)

# Resignations
ggplot(data=merged_df, aes(x=Date, y=Resignation, group=as.factor(pre_post)))+
  geom_jitter(aes(size = abs(Resignation), color=Agency), alpha=.4, show.legend = FALSE)+
  geom_smooth(na.rm = T, se=F, method = "lm", color="black")+
  geom_vline(xintercept = as.POSIXct(as.Date("2020-06-01")), color = "tomato3", linetype = "dashed", size=.9)+
  facet_wrap(~Agency, scales = "free")+
  theme_bw()+
  ggtitle("Voluntary Resignations")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="Monthly Resignations",x="")+
  scale_color_grafify("kelly")

# Retirements
ggplot(data=merged_df, aes(x=Date, y=Retirement, group=as.factor(pre_post)))+
  geom_jitter(aes(size = abs(Retirement), color=Agency), alpha=.4, show.legend = FALSE)+
  geom_smooth(na.rm = T, se=F, method = "lm", color="black")+
  geom_vline(xintercept = as.POSIXct(as.Date("2020-06-01")), color = "tomato3", linetype = "dashed", size=.9)+
  facet_wrap(~Agency, scales = "free")+
  theme_bw()+
  ggtitle("Retirements")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="Monthly Retirements",x="")+
  scale_color_grafify("kelly")



### Chicago Analysis - Resignation
chic_itsa<-merged_df %>%
  filter(Agency=="Chicago")

# Intervention Date -> 955 =8/12/20
table(chic_itsa$pre_post)
#0   1 
#217 26

#Variable for pre-intervention

chic_itsa$time_before<-c(rep(1:217),rep(0,26))

#Variable to test effect across the entire post-period
chic_itsa$time_since<-c(rep(0,217),rep(1:26))

#no seasonality 
ggacf(chic_itsa$Resignation)
ggpacf(chic_itsa$Resignation)

#no periodicity
#stl(chic_itsa$Resignation)

# make matrix for regressors
chic_reg_matrix<-as.matrix(chic_itsa[,c(7:9)])  #floyd, before, post

# Sarima model
set.seed(58)
chic_sarima_resig<-auto.sarima(ts(chic_itsa$Resignation), seasonal = FALSE,# order = c(1, 0, 0),
                               chains = 4,
                               iter = 10000,
                               warmup = 1000,
                               #seed = 58,
                               xreg = chic_reg_matrix,
                               prior_mu0 = gamma(1,1),
                               prior_sigma0 = gamma(1,2))  # (1,0,0) = 1 AR, no MA or diff order; (0,0,0) = no seasonal parameters

summary(chic_sarima_resig)
model(chic_sarima_resig)    #****Chooses a 1,0,1 model
prior_summary(chic_sarima_resig)

#prepare for plotting
chic_sarima_plot<-as.data.frame(summary(chic_sarima_resig))

chic_sarima_plot <- chic_sarima_plot[c(5:7),]

chic_sarima_plot$variable<-c("Short-Term","Pre-Floyd","Long-Term")
chic_sarima_plot$agency<-"Chicago"


names(chic_sarima_plot)<-c("mean","se","ll","ul","ess","rhat","variable")

ggplot(chic_sarima_plot, aes(y=variable))+
  geom_pointrange(aes(x=mean,xmin=ll,xmax=ul))+
  theme_bw()

# Build Countefactual

chic_counter_resig.df <- chic_itsa %>%
  filter(Date <= "2020-05-01")



chic_counter_resig.sd <- sd(chic_counter_resig.df$Resignation)

chic_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*chic_counter_resig.sd,
                                          upper.limit=chic_counter_resig.sd)

chic_counter_resig.ss <- bsts::AddLocalLevel(list(), chic_counter_resig.df$Resignation, chic_counter_sigma.prior)

chic_resig_bsts<- bsts::bsts(chic_counter_resig.df$Resignation,family = "gaussian", state.specification = chic_counter_resig.ss, niter = 10000, seed = 58)


plot(chic_resig_bsts)



chic_resig_pred<-predict(chic_resig_bsts, horizon = 26, burn = bsts::SuggestBurn(0.1, chic_resig_bsts), seed = 58)


chic_resig_pred$median
chic_resig_plot.df<-chic_itsa[1:243,c(1,2)]

chic_resig_plot.df$Mean<-NA
chic_resig_plot.df[218:243,3]<-chic_resig_pred$mean

chic_resig_plot.df$Median<-NA
chic_resig_plot.df[218:243,4]<-chic_resig_pred$median

chic_resig_bsts.interval<-chic_resig_pred$interval
chic_resig_bsts.interval<-t(chic_resig_bsts.interval)

chic_resig_plot.df$LL<-NA
chic_resig_plot.df$UL<-NA
chic_resig_plot.df[218:243,5:6]<-chic_resig_bsts.interval

chic_resig_counter.plot<-ggplot(data = chic_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=chic_resig_plot.df,aes(y=Resignation), color="#009E73", pch=19, alpha = .3, size =3)+
  #geom_smooth(data=chic_forecast_plot_df[1:101,],aes(y=resignations), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[1:217,],aes(y=Resignation), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[218:243,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[218:243,],aes(y=Resignation), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Chicago")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")
#annotate("text", x=as.Date("2016-08-01"),y = 35, label = "+426\nprob>.99", fontface= "bold")

chic_resig_counter.plot

saveRDS(chic_resig_counter.plot, here("output","chic_resig.rds"))


# Chicago Retirements====


chic_itsa<-merged_df %>%
  filter(Agency=="Chicago")

# Intervention Date -> 955 =8/12/20
table(chic_itsa$pre_post)
#0   1 
#217 26

#Variable for pre-intervention

chic_itsa$time_before<-c(rep(1:217),rep(0,26))

#Variable to test effect across the entire post-period
chic_itsa$time_since<-c(rep(0,217),rep(1:26))

#no seasonality 
ggacf(chic_itsa$Retirement)
ggpacf(chic_itsa$Retirement)

#no periodicity
#stl(chic_itsa$Retirement)

# make matrix for regressors
chic_reg_matrix<-as.matrix(chic_itsa[,c(7:9)])  #floyd, before, post

# Sarima model
set.seed(58)
chic_sarima_resig<-auto.sarima(ts(chic_itsa$Retirement), seasonal = FALSE,# order = c(1, 0, 0),
                               chains = 4,
                               iter = 10000,
                               warmup = 1000,
                               #seed = 58,
                               xreg = chic_reg_matrix,
                               prior_mu0 = gamma(1,1),
                               prior_sigma0 = gamma(1,2))  # (1,0,0) = 1 AR, no MA or diff order; (0,0,0) = no seasonal parameters

summary(chic_sarima_resig)
model(chic_sarima_resig)    #****Chooses a 1,0,0 model
prior_summary(chic_sarima_resig)

#prepare for plotting
chic_sarima_plot<-as.data.frame(summary(chic_sarima_resig))

chic_sarima_plot <- chic_sarima_plot[c(4:6),]

chic_sarima_plot$variable<-c("Short-Term","Pre-Floyd","Long-Term")
chic_sarima_plot$agency<-"Chicago"


names(chic_sarima_plot)<-c("mean","se","ll","ul","ess","rhat","variable")

ggplot(chic_sarima_plot, aes(y=variable))+
  geom_pointrange(aes(x=mean,xmin=ll,xmax=ul))+
  theme_bw()

# Build Countefactual

chic_counter_resig.df <- chic_itsa %>%
  filter(Date <= "2020-05-01")



chic_counter_resig.sd <- sd(chic_counter_resig.df$Retirement)

chic_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*chic_counter_resig.sd,
                                          upper.limit=chic_counter_resig.sd)

chic_counter_resig.ss <- bsts::AddLocalLevel(list(), chic_counter_resig.df$Retirement, chic_counter_sigma.prior)

chic_resig_bsts<-bsts::bsts(chic_counter_resig.df$Retirement,family = "gaussian", state.specification = chic_counter_resig.ss, niter = 10000, seed = 58)

plot(chic_resig_bsts)



chic_resig_pred<-predict(chic_resig_bsts, horizon = 26, burn = bsts::SuggestBurn(0.1, chic_resig_bsts), seed = 58)


chic_resig_pred$median
chic_resig_plot.df<-chic_itsa[1:243,c(1,3)]

chic_resig_plot.df$Mean<-NA
chic_resig_plot.df[218:243,3]<-chic_resig_pred$mean

chic_resig_plot.df$Median<-NA
chic_resig_plot.df[218:243,4]<-chic_resig_pred$median

chic_resig_bsts.interval<-chic_resig_pred$interval
chic_resig_bsts.interval<-t(chic_resig_bsts.interval)

chic_resig_plot.df$LL<-NA
chic_resig_plot.df$UL<-NA
chic_resig_plot.df[218:243,5:6]<-chic_resig_bsts.interval

chic_retire_counter.plot<-ggplot(data = chic_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=chic_resig_plot.df,aes(y=Retirement), color="#009E73", pch=19, alpha = .3, size =3)+
  #geom_smooth(data=chic_forecast_plot_df[1:101,],aes(y=Retirements), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[1:217,],aes(y=Retirement), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[218:243,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=chic_resig_plot.df[218:243,],aes(y=Retirement), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Chicago")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")
#annotate("text", x=as.Date("2016-08-01"),y = 200, label = "+240\nprob=.93", fontface= "bold")

chic_retire_counter.plot

saveRDS(chic_retire_counter.plot, here("output","chic_retire.rds"))

#############
#############
############# Memphis

# Memphis Resignations====





# Memphis Retirements====






###################
###################
################### Seattle

# Seattle Resignations====


sea_itsa<-merged_df %>%
  filter(Agency=="Seattle")

ggplot(sea_itsa, aes(x=Date, y=Resignation))+
  geom_line()+geom_smooth()

# Intervention Date -> 955 =8/12/20
table(sea_itsa$pre_post)
#0   1 
#101 28

#Variable for pre-intervention

sea_itsa$time_before<-c(rep(1:101),rep(0,28))

#Variable to test effect across the entire post-period
sea_itsa$time_since<-c(rep(0,101),rep(1:28))

#no seasonality 
ggacf(sea_itsa$Resignation)
ggpacf(sea_itsa$Resignation)

#no periodicity
#stl(sea_itsa$Resignation) 


# make matrix for regressors
sea_reg_matrix<-as.matrix(sea_itsa[,c(7:9)])  #floyd, before, post

# Sarima model
set.seed(58)
sea_sarima_resig<-auto.sarima(ts(sea_itsa$Resignation), seasonal = FALSE,# order = c(1, 0, 0),
                              chains = 4,
                              iter = 10000,
                              warmup = 1000,
                              #seed = 58,
                              xreg = sea_reg_matrix,
                              prior_mu0 = gamma(1,1),
                              prior_sigma0 = gamma(1,2))  # (1,0,0) = 1 AR, no MA or diff order; (0,0,0) = no seasonal parameters

summary(sea_sarima_resig)
model(sea_sarima_resig)    
prior_summary(sea_sarima_resig)
autoplot(sea_sarima_resig, prob = .9)

#prepare for plotting
sea_sarima_plot<-as.data.frame(summary(sea_sarima_resig))

sea_sarima_plot <- sea_sarima_plot[c(3:5),]

sea_sarima_plot$variable<-c("Short-Term","Pre-Floyd","Long-Term")
sea_sarima_plot$agency<-"Seattle"


names(sea_sarima_plot)<-c("mean","se","ll","ul","ess","rhat","variable")

ggplot(sea_sarima_plot, aes(y=variable))+
  geom_pointrange(aes(x=mean,xmin=ll,xmax=ul))+
  theme_bw()

# Build Countefactual

sea_counter_resig.df <- sea_itsa %>%
  filter(Date <= "2020-05-01")



sea_counter_resig.sd <- sd(sea_counter_resig.df$Resignation)

sea_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*sea_counter_resig.sd,
                                         upper.limit=sea_counter_resig.sd)

sea_counter_resig.ss <- bsts::AddLocalLevel(list(), sea_counter_resig.df$Resignation, sea_counter_sigma.prior)  #local level better counterfactual

sea_resig_bsts<- bsts::bsts(sea_counter_resig.df$Resignation,family = "gaussian", state.specification = sea_counter_resig.ss, niter = 10000, seed = 58)


plot(sea_resig_bsts)



sea_resig_pred<-predict(sea_resig_bsts, horizon = 28, burn = bsts::SuggestBurn(0.1, sea_resig_bsts), seed = 58)


sea_resig_pred$median
sea_resig_plot.df<-sea_itsa[1:129,c(1,2)]

sea_resig_plot.df$Mean<-NA
sea_resig_plot.df[102:129,3]<-sea_resig_pred$mean

sea_resig_plot.df$Median<-NA
sea_resig_plot.df[102:129,4]<-sea_resig_pred$median

sea_resig_bsts.interval<-sea_resig_pred$interval
sea_resig_bsts.interval<-t(sea_resig_bsts.interval)

sea_resig_plot.df$LL<-NA
sea_resig_plot.df$UL<-NA
sea_resig_plot.df[102:129,5:6]<-sea_resig_bsts.interval

sea_resig_counter.plot<-ggplot(data = sea_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=sea_resig_plot.df,aes(y=Resignation), color="#009E73", pch=19, alpha = .3, size =3)+
  #geom_smooth(data=chic_forecast_plot_df[1:101,],aes(y=resignations), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[1:101,],aes(y=Resignation), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[102:129,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[102:129,],aes(y=Resignation), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Seattle")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")
#annotate("text", x=as.Date("2017-05-01"),y = 20, label = "+103\nprob>.99", fontface= "bold")

sea_resig_counter.plot

saveRDS(sea_resig_counter.plot, here("output","sea_resig.rds"))


# Seattle Retirements====


sea_itsa<-merged_df %>%
  filter(Agency=="Seattle")

ggplot(sea_itsa, aes(x=Date, y=Retirement))+
  geom_line()+geom_smooth()

# Intervention Date -> 955 =8/12/20
table(sea_itsa$pre_post)
#0   1 
#101 28

#Variable for pre-intervention

sea_itsa$time_before<-c(rep(1:101),rep(0,28))

#Variable to test effect across the entire post-period
sea_itsa$time_since<-c(rep(0,101),rep(1:28))

#no seasonality 
ggacf(sea_itsa$Retirement)
ggpacf(sea_itsa$Retirement)

#no periodicity
#stl(sea_itsa$Retirement) 


# make matrix for regressors
sea_reg_matrix<-as.matrix(sea_itsa[,c(7:9)])  #floyd, before, post

# Sarima model
set.seed(58)
sea_sarima_resig<-auto.sarima(ts(sea_itsa$Retirement), seasonal = FALSE,# order = c(1, 0, 0),
                              chains = 4,
                              iter = 10000,
                              warmup = 1000,
                              #seed = 58,
                              xreg = sea_reg_matrix,
                              prior_mu0 = gamma(1,1),
                              prior_sigma0 = gamma(1,2))  # (1,0,0) = 1 AR, no MA or diff order; (0,0,0) = no seasonal parameters

summary(sea_sarima_resig)
model(sea_sarima_resig)    
prior_summary(sea_sarima_resig)
autoplot(sea_sarima_resig, prob = .9)

#prepare for plotting
sea_sarima_plot<-as.data.frame(summary(sea_sarima_resig))

sea_sarima_plot <- sea_sarima_plot[c(3:5),]

sea_sarima_plot$variable<-c("Short-Term","Pre-Floyd","Long-Term")
sea_sarima_plot$agency<-"Seattle"


names(sea_sarima_plot)<-c("mean","se","ll","ul","ess","rhat","variable")

ggplot(sea_sarima_plot, aes(y=variable))+
  geom_pointrange(aes(x=mean,xmin=ll,xmax=ul))+
  theme_bw()

# Build Countefactual

sea_counter_resig.df <- sea_itsa %>%
  filter(Date <= "2020-05-01")



sea_counter_resig.sd <- sd(sea_counter_resig.df$Retirement)

sea_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*sea_counter_resig.sd,
                                         upper.limit=sea_counter_resig.sd)

sea_counter_resig.ss <- bsts::AddLocalLevel(list(), sea_counter_resig.df$Retirement, sea_counter_sigma.prior)  #local level better counterfactual

sea_resig_bsts<-bsts::bsts(sea_counter_resig.df$Retirement,family = "gaussian", state.specification = sea_counter_resig.ss, niter = 10000, seed = 58)


plot(sea_resig_bsts)



sea_resig_pred<-predict(sea_resig_bsts, horizon = 28, burn = bsts::SuggestBurn(0.1, sea_resig_bsts), seed = 58)


sea_resig_pred$median
sea_resig_plot.df<-sea_itsa[1:129,c(1,3)]

sea_resig_plot.df$Mean<-NA
sea_resig_plot.df[102:129,3]<-sea_resig_pred$mean

sea_resig_plot.df$Median<-NA
sea_resig_plot.df[102:129,4]<-sea_resig_pred$median

sea_resig_bsts.interval<-sea_resig_pred$interval
sea_resig_bsts.interval<-t(sea_resig_bsts.interval)

sea_resig_plot.df$LL<-NA
sea_resig_plot.df$UL<-NA
sea_resig_plot.df[102:129,5:6]<-sea_resig_bsts.interval

sea_retire_counter.plot<-ggplot(data = sea_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=sea_resig_plot.df,aes(y=Retirement), color="#009E73", pch=19, alpha = .3, size =3)+
  #geom_smooth(data=chic_forecast_plot_df[1:101,],aes(y=Retirements), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[1:101,],aes(y=Retirement), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[102:129,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=sea_resig_plot.df[102:129,],aes(y=Retirement), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Seattle")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")
#annotate("text", x=as.Date("2017-05-01"),y = 17, label = "+125\nprob>.99", fontface= "bold")

sea_retire_counter.plot

saveRDS(sea_retire_counter.plot, here("output","sea_retire.rds"))
