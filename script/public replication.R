
library(tidyverse)
library(here)
library(grafify)
library(dplyr)
library(BEST)
library(bsts)
library(ggplot2)


#load data====

merged_df <- readRDS(here("data","processed","public_data.rds"))

# Add pre-post intervention for grouping
merged_df$pre_post<-ifelse(merged_df$Date>"2020-05-01",1,0)


## Raw plots (Figs 1 and 2)====

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



# Chicago Resignations====

# Subset Chicago
chic<-merged_df %>%
  filter(Agency=="Chicago")


# Build bsts model up to intervention

chic_counter_resig.df <- chic %>%
  filter(Date <= "2020-05-01")

# SD prior
chic_counter_resig.sd <- sd(chic_counter_resig.df$Resignation)

chic_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*chic_counter_resig.sd,
                                          upper.limit=chic_counter_resig.sd)

# add local level trend
chic_counter_resig.ss <- bsts::AddLocalLevel(list(), chic_counter_resig.df$Resignation, chic_counter_sigma.prior)

# estimate bsts
chic_resig_bsts<- bsts::bsts(chic_counter_resig.df$Resignation,family = "gaussian", state.specification = chic_counter_resig.ss, niter = 10000, seed = 58)

# forefcast counterfactual
chic_resig_pred<-predict(chic_resig_bsts, horizon = 26, burn = bsts::SuggestBurn(0.1, chic_resig_bsts), seed = 58)

# plotting
chic_resig_plot.df<-chic[,c(1,2)]

chic_resig_plot.df$Mean<-NA
chic_resig_plot.df[218:243,3]<-chic_resig_pred$mean


chic_resig_counter.plot<-ggplot(data = chic_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=chic_resig_plot.df,aes(y=Resignation), color="#009E73", pch=19, alpha = .3, size =3)+
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

chic_resig_counter.plot


# Best Analysis (difference in means)

set.seed(58)
chic_resig_best<-BESTmcmc(chic_resig_plot.df[218:243,]$Resignation,chic_resig_plot.df[218:243,]$Mean, parallel=TRUE)

plot(chic_resig_best)

summary(chic_resig_best)


# Chicago Retirements====

# Build bsts model up to intervention

chic_counter_ret.df <- chic %>%
  filter(Date <= "2020-05-01")

# SD prior
chic_counter_ret.sd <- sd(chic_counter_ret.df$Retirement)

chic_counter_sigmaret.prior <- Boom::SdPrior(sigma.guess=.01*chic_counter_ret.sd,
                                          upper.limit=chic_counter_ret.sd)

# add local level trend
chic_counter_ret.ss <- bsts::AddLocalLevel(list(), chic_counter_ret.df$Retirement, chic_counter_sigmaret.prior)

# estimate bsts
chic_ret_bsts<-bsts::bsts(chic_counter_ret.df$Retirement,family = "gaussian", state.specification = chic_counter_ret.ss, niter = 10000, seed = 58)

# forecast counterfactual
chic_ret_pred<-predict(chic_ret_bsts, horizon = 26, burn = bsts::SuggestBurn(0.1, chic_ret_bsts), seed = 58)

# plotting
chic_ret_plot.df<-chic[,c(1,3)]

chic_ret_plot.df$Mean<-NA
chic_ret_plot.df[218:243,3]<-chic_ret_pred$mean


chic_retire_counter.plot<-ggplot(data = chic_ret_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=chic_ret_plot.df,aes(y=Retirement), color="#009E73", pch=19, alpha = .3, size =3)+
  geom_smooth(data=chic_ret_plot.df[1:217,],aes(y=Retirement), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=chic_ret_plot.df[218:243,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=chic_ret_plot.df[218:243,],aes(y=Retirement), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Chicago")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")

chic_retire_counter.plot

# Best Analysis (difference in means)
set.seed(58)
chic_ret_best<-BESTmcmc(chic_ret_plot.df[218:243,]$Retirement,chic_ret_plot.df[218:243,]$Mean, parallel=TRUE)

plot(chic_ret_best) 

summary(chic_ret_best)




# Memphis Resignations====

# subset Memphis
mem<-merged_df %>%
  filter(Agency=="Memphis")

# Build bsts model up to intervention
memphis_counter_resig.df <- mem %>%
  filter(Date <= "2020-05-01")

# SD prior
memphis_counter_resig.sd <- sd(memphis_counter_resig.df$Resignation)

memphis_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*memphis_counter_resig.sd,
                                             upper.limit=memphis_counter_resig.sd)

# add local level trend
memphis_counter_resig.ss <- bsts::AddLocalLevel(list(), memphis_counter_resig.df$Resignation, memphis_counter_sigma.prior) 

# estimate bsts
memphis_resig_bsts<- bsts::bsts(memphis_counter_resig.df$Resignation,family = "gaussian", state.specification = memphis_counter_resig.ss, niter = 10000, seed = 58)

# forecast counterfactual
memphis_resig_pred<-predict(memphis_resig_bsts, horizon = 30, burn = bsts::SuggestBurn(0.1, memphis_resig_bsts), seed = 58)

# plotting
memphis_resig_plot.df<-mem[,c(1,2)]

memphis_resig_plot.df$Mean<-NA
memphis_resig_plot.df[90:119,3]<-memphis_resig_pred$mean


memphis_resig_counter.plot<-ggplot(data = memphis_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=memphis_resig_plot.df,aes(y=Resignation), color="#009E73", pch=19, alpha = .3, size =3)+
  geom_smooth(data=memphis_resig_plot.df[1:89,],aes(y=Resignation), color="#56B4E9", lwd=1.2,se=F, method = "lm",span=.35)+
  geom_smooth(data=memphis_resig_plot.df[90:119,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=memphis_resig_plot.df[90:119,],aes(y=Resignation), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Memphis")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")

memphis_resig_counter.plot


# Best Analysis (difference in means)

set.seed(58)
memphis_resig_best<-BESTmcmc(memphis_resig_plot.df[90:119,]$Resignation,memphis_resig_plot.df[90:119,]$Mean, parallel=TRUE)

plot(memphis_resig_best)  

summary(memphis_resig_best)



# Memphis Retirements====

# Build bsts model up to intervention

memphis_counter_ret.df <- mem %>%
  filter(Date <= "2020-05-01")

# SD prior
memphis_counter_ret.sd <- sd(memphis_counter_ret.df$Retirement)

memphis_counter_sigma.prior.ret <- Boom::SdPrior(sigma.guess=.01*memphis_counter_ret.sd,
                                                 upper.limit=memphis_counter_ret.sd)

# add local level trend
memphis_counter_ret.ss <- bsts::AddLocalLevel(list(), memphis_counter_ret.df$Retirement, memphis_counter_sigma.prior.ret) 

# estimate bsts
memphis_ret_bsts<-bsts::bsts(memphis_counter_ret.df$Retirement,family = "gaussian", state.specification = memphis_counter_ret.ss, niter = 10000, seed = 58)

# forecast counterfactual
memphis_ret_pred<-predict(memphis_ret_bsts, horizon = 30, burn = bsts::SuggestBurn(0.1, memphis_ret_bsts), seed = 58)


# plotting
memphis_ret_plot.df<-mem[,c(1,3)]

memphis_ret_plot.df$Mean<-NA
memphis_ret_plot.df[90:119,3]<-memphis_ret_pred$mean

memphis_ret_counter.plot<-ggplot(data = memphis_ret_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=memphis_ret_plot.df,aes(y=Retirement), color="#009E73", pch=19, alpha = .3, size =3)+
  geom_smooth(data=memphis_ret_plot.df[1:89,],aes(y=Retirement), color="#56B4E9", lwd=1.2,se=F, method = "lm",span=.35)+
  geom_smooth(data=memphis_ret_plot.df[90:119,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=memphis_ret_plot.df[90:119,],aes(y=Retirement), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Memphis")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")

memphis_ret_counter.plot

# Best Analysis (difference in means)

set.seed(58)
memphis_ret_best<-BESTmcmc(memphis_ret_plot.df[90:119,]$Retirement,memphis_ret_plot.df[90:119,]$Mean, parallel=TRUE)

plot(memphis_ret_best) 

summary(memphis_ret_best)



# Seattle Resignations====

# subset Seattle
sea<-merged_df %>%
  filter(Agency=="Seattle")


# Build bsts model up to intervention

sea_counter_resig.df <- sea %>%
  filter(Date <= "2020-05-01")


# SD prior
sea_counter_resig.sd <- sd(sea_counter_resig.df$Resignation)

sea_counter_sigma.prior <- Boom::SdPrior(sigma.guess=.01*sea_counter_resig.sd,
                                         upper.limit=sea_counter_resig.sd)

# add local level trend
sea_counter_resig.ss <- bsts::AddLocalLevel(list(), sea_counter_resig.df$Resignation, sea_counter_sigma.prior) 

# estimate bsts
sea_resig_bsts<- bsts::bsts(sea_counter_resig.df$Resignation,family = "gaussian", state.specification = sea_counter_resig.ss, niter = 10000, seed = 58)

# forecast counterfactual
sea_resig_pred<-predict(sea_resig_bsts, horizon = 28, burn = bsts::SuggestBurn(0.1, sea_resig_bsts), seed = 58)

# plotting
sea_resig_plot.df<-sea[1:129,c(1,2)]

sea_resig_plot.df$Mean<-NA
sea_resig_plot.df[102:129,3]<-sea_resig_pred$mean



sea_resig_counter.plot<-ggplot(data = sea_resig_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=sea_resig_plot.df,aes(y=Resignation), color="#009E73", pch=19, alpha = .3, size =3)+
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

sea_resig_counter.plot

# Best Analysis (difference in means)

set.seed(58)
sea_resig_best<-BESTmcmc(sea_resig_plot.df[102:129,]$Resignation,sea_resig_plot.df[102:129,]$Mean, parallel=TRUE)

plot(sea_resig_best) 

summary(sea_resig_best)



# Seattle Retirements====



# Build bsts model up to intervention

sea_counter_ret.df <- sea %>%
  filter(Date <= "2020-05-01")

# SD prior
sea_counter_ret.sd <- sd(sea_counter_ret.df$Retirement)

sea_counter_sigmaret.prior <- Boom::SdPrior(sigma.guess=.01*sea_counter_ret.sd,
                                         upper.limit=sea_counter_ret.sd)

# add local level trend
sea_counter_ret.ss <- bsts::AddLocalLevel(list(), sea_counter_ret.df$Retirement, sea_counter_sigmaret.prior) 

# estimate bsts
sea_ret_bsts<-bsts::bsts(sea_counter_ret.df$Retirement,family = "gaussian", state.specification = sea_counter_ret.ss, niter = 10000, seed = 58)

# forecast counterfactual
sea_ret_pred<-predict(sea_ret_bsts, horizon = 28, burn = bsts::SuggestBurn(0.1, sea_ret_bsts), seed = 58)

# plotting
sea_ret_plot.df<-sea[1:129,c(1,3)]

sea_ret_plot.df$Mean<-NA
sea_ret_plot.df[102:129,3]<-sea_ret_pred$mean



sea_retire_counter.plot<-ggplot(data = sea_ret_plot.df, aes( x=as.Date(Date))) +
  geom_jitter(data=sea_ret_plot.df,aes(y=Retirement), color="#009E73", pch=19, alpha = .3, size =3)+
  geom_smooth(data=sea_ret_plot.df[1:101,],aes(y=Retirement), color="#56B4E9", lwd=1.2,se=F, method = "lm")+
  geom_smooth(data=sea_ret_plot.df[102:129,],aes(y=Mean), color = "#E69F00", lty = 5, lwd=1.2, se=F, method = "lm")+
  geom_smooth(data=sea_ret_plot.df[102:129,],aes(y=Retirement), color="#56B4E9", lwd=1.2, se=F, method = "lm")+
  theme_bw()+
  geom_vline(xintercept = as.Date("2020-06-01"), color = "tomato3", linetype = "dashed", size=.5)+
  ggtitle("Seattle")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))+
  labs(y="",x="")+
  scale_color_manual(values=c("Observed"="#009E73","Observed Trend"= "#56B4E9","Counterfactual"= "#E69F00"),name="")+
  theme(legend.position="none")

sea_retire_counter.plot


# Best Analysis (difference in means)
set.seed(58)
sea_ret_best<-BESTmcmc(sea_ret_plot.df[102:129,]$Retirement,sea_ret_plot.df[102:129,]$Mean, parallel=TRUE)

plot(sea_ret_best)

summary(sea_ret_best)
