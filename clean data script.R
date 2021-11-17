##set working directory to Monogenean folder 
setwd("C:/Users/Sterling/Desktop/MONOGENS")
####clean our global environment
remove(list = ls())
###load lots of programs
library(readr)  # reads data files into R studio really easily
library(datasets)  # this gives you access to a bunch of data sets R keeps on hand
library(tidyr)# wickham's way of tidying data
library(plyr)
library(dplyr)  #  wickham's way of manipulating df; includes 5 verbs plus join
library(ggplot2)  # wickham's way of graphing
library(stringr) # wickham’s way of editing character strings
library(lubridate)  # easy way to deal with dates
library(data.table) #set names
library(tibble)
library(lme4)
library(readr)
library(glmmTMB)
library(FSA)
library(psych)
library(lme4)
library(lmerTest)
library(nlme)
library(car)
library(DHARMa)
library(sjPlot)
library(cowplot)
library(sjmisc)
library(effects)
library(sjstats)
library(effectsize)
library(MuMIn)
library(GGally)
library(foreign)
library(ggiraph)
library(ggiraphExtra)
###below is for PCA data visualization 
library(devtools)
library(ggbiplot)
library(extrafont)
library(openintro)
###Reasoning behind using means: Mostly, the means are representative of the populations existing
#on a single fish, which I think better represents growth of populations better than individual measures.
#The other variables are also representative of monogeneans on a single fish and not of individuals on those fish.
#Also, there were large differences in the number of observations, and the independent variables
#would be weighted according to those observations. There is no correlation between number of 
#observations and SE or SD, so while this eliminates the weight of these observations, the observations 
#number itself had no effect on the variation in means and could be eliminated.

####Load in the data
justeggs<- read.csv("theeggsheet.csv")
spread_data <- read.csv("spread_data.csv")
long_means<-read.csv("longmeans.csv")
with_specifics<-read.csv("withspecifics.csv")
mono_data<-read.csv("mono_data.csv")
data_1 <- read.csv("les donnees.csv")
spread_res<-read.csv("spread_datares.csv")
spread_comm<-read.csv("spread_cml.csv")
mono_data<-mono_data %>% rename(mean_length=sp_mean_size)
mono_data2<-mono_data[,c(2,8)]
with_specifics2<-left_join(with_specifics, mono_data2, "host_id")
with_specifics2<- with_specifics2 %>% distinct(X, .keep_all = T)

##multispread function
multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
###Spread em, beed to remove sp_ab, function won't remove unique values
#with_specifics3<- with_specifics2[,-c(1,33)]
#spread_comm<- with_specifics3 %>% multi_spread(species, c(mean_length, stdev_length, se_length, number_observation))
#write.csv(spread_comm, "spread_cml.csv")
###look at some basic stuff, stuff without normal distribution have been transformed to log10
hist(log10(spread_data$act))
hist(log10(spread_data$onch))
hist(spread_data$act_mean_length)
hist(spread_data$onch_mean_length)
hist(spread_data$days_frozen)
hist(log(spread_data$act_number_observation))
hist(log(spread_data$act_stdev_length))
hist(log(spread_data$act_se_length))
hist(log(spread_data$onch_number_observation))
hist(log(spread_data$onch_stdev_length))
hist(log(spread_data$onch_se_length))
hist(spread_data$host_fl)
hist(spread_data$host_sl)
hist(log(spread_data$host_ww))
hist(log(justeggs$eggs_per))
unique(with_specifics$species)
###add in density

with_specifics$sp_den<- with_specifics$sp_ab/with_specifics$host_sl
hist(with_specifics$sp_den)
hist(log(with_specifics$sp_den))
###lm
unique(with_specifics$species)
model1<-lmer(mean_length~sp_den*species+(1|host_id), data = with_specifics)
summary(model1)
model2<-lmer(mean_length~sp_den+species+(1|host_id), data = with_specifics, )
summary(model2)
ggplot(data = with_specifics, aes(x= log(sp_den), y = mean_length, col = species))+
  geom_point()+
  xlab("Species Ab")+
  ylab("length")+
  geom_smooth(method = lm)+
  theme_bw()
anova(model1,model2)
simOutput<-simulateResiduals(fittedModel = model1)
residuals(simOutput)
plot(simOutput)
rand(model1)
hist(resid(model1))
hist(resid(simOutput))
ggpairs(with_spec3[,-c(1,2,17,18)])

with_spec3<-merge(with_specifics2, spread_res[, c("host_id", "res", "res2")], by ="host_id")
omcdiag(mod = model2 )
imcdiag(mod = model2)

###Try without hap
with_specifics2<-filter(with_specifics, species != "hap")
with_specifics2$lake_group<-ifelse(with_specifics2$lake == "SKL", "SKL","RSL_JL")
model2<-lmer(mean_length~sp_den*species+(1|host_id), data = with_specifics2)
summary(model2)
model1<-lmer(mean_length~log(sp_den)+species+(1|host_id), data = with_specifics2)
summary(model1)
anova(model1,model2)
######changing species from interaction term to categorical didn't have an affect on the fit, so use model1
simOutput<-simulateResiduals(fittedModel = model1)
residuals(simOutput)
plot(simOutput)
rand(model1)
#####random effect of model1 significant
r.squaredGLMM(model2)
r.squaredGLMM(model1)
plot(model1)
####Full model
plot_model(model2)
deneffect<-effects:: effect(term = "log(sp_den)",mod =model2)
summary(deneffect)
onch_effects<- as.data.frame(deneffect)
den_plot <- ggplot() + 
  geom_point(data=with_specifics2, aes(log(sp_den), mean_length)) + 
  geom_point(data=onch_effects, aes(x=log(sp_den), y=fit), color="blue") +
  geom_line(data=onch_effects, aes(x=log(sp_den), y=fit), color="blue") +
  geom_ribbon(data= onch_effects, aes(x=log(sp_den), ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Density (worms/mm host)", y="Body length (um)")

den_plot

ggplot(data = with_specifics2, aes(x= log(sp_den), y = mean_length, col = species, shape = lake))+
  geom_point(size = 3)+
  xlab("Ln(Species Density) (abundance/host standard length)")+
  ylab("length(?m)")+
  geom_errorbar( aes(ymin = mean_length-se_length, ymax = mean_length+se_length),width = 0.05)+
  stat_smooth(method = lm, aes(linetype = lake))+
  scale_color_manual(values=c( "gray0","gray50"))+
  annotate(geom = "text", x = -3, y = 350, label = "C")+
  annotate(geom = "text", x = -4, y = 405, label = "A")+
  annotate(geom = "text", x = -3.5, y = 310, label = "D")+
  annotate(geom = "text", x = 0, y = 385, label = "D")+
  annotate(geom = "text", x = -4, y = 355, label = "B")+
  annotate(geom = "text", x = 0, y = 475, label = "B")+
  theme_bw()+
  geom_vline(xintercept =  0)+
  geom_hline(yintercept = 440)+
  geom_hline(yintercept = 463)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

####total density
with_specifics2$tot_den<-with_specifics2$total/with_specifics2$host_sl
with_specifics2$sp_den<-with_specifics2$sp_ab /with_specifics2$host_sl
model2<-lm(mean_length~log(sp_den)*species*lake, data = with_specifics2)
model4<-aov(mean_length~log(sp_den)+species*lake, data = with_specifics2)
anova(model4)
TukeyHSD(model4)
summary(model2)
model1<-lm(mean_length~sp_den+species*lake, data = with_specifics2)
summary(model1)
anova(model1, model2)
Anova(model2, type = "III")
######changing species from interaction term to categorical didn't have an affect on the fit, so use model1
simOutput<-simulateResiduals(fittedModel = model1)
residuals(simOutput)
plot(simOutput)
rand(model1)
###onch alone this the important one!!!!!
spread_comm$on_den<-spread_comm$onch/spread_comm$host_sl
  onchmodel1<-lm(onch_mean_length~log(on_den)*lake, data =spread_comm)
  hist(spread_comm$on_den)
summary(onchmodel1)
spread_comm$tot_den<-spread_comm$total /spread_comm$host_sl
onchmodel2<-lm(onch_mean_length~on_den+lake, data =spread_comm)
summary(onchmodel2)
plot(onchmodel1)
Anova(onchmodel1, type = "III")
TukeyHSD(model1)
ggplot(data = spread_comm, aes(x= log(on_den), y = onch_mean_length, color = lake))+
  geom_point(aes(fill = lake), color= "black", pch=21, size = 3)+
  scale_fill_manual(values = c("grey45", "black", "white"))+
  xlab("ln(Density (abundance/host standard length))")+
  ylab("mean length(µm)")+
  geom_errorbar( aes(ymin = onch_mean_length-onch_se_length, ymax = onch_mean_length+onch_se_length),width = 0.05, color = "black")+
  stat_smooth(method = lm, aes(linetype = lake), color = "black")+
  #scale_colour_manual("lake",values = c("grey45","black","grey99"))+
  #scale_color_manual(values= "gray0")+
  #guides(colour=F)+
  annotate(geom = "text", x = -4, y = 350, label = "A", size = 10)+
  annotate(geom = "text", x = -3, y = 315, label = "B", size = 10)+
  annotate(geom = "text", x = -4, y = 405, label = "A", size = 10)+
  theme_bw()+
 # geom_vline(xintercept =  0)+
 # geom_hline(yintercept = 440)+
#  geom_hline(yintercept = 463)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
####actalone this is the important one!!!!!
spread_comm$act_den<-spread_comm$act/spread_comm$host_sl
hist(spread_comm$act_den)
actmodel1<-lm(act_mean_length~log(act_den)*lake, data =spread_comm)
actmodel3<-lm(act_mean_length~act_den+lake, data =spread_comm)
summary(actmodel1)
summary(actmodel3)
anova(actmodel1,actmodel3)
spread_comm$tot_den<-spread_comm$total /spread_comm$host_sl
actmodel2<-lm(act_mean_length~tot_den, data =spread_comm)
summary(actmodel2)
plot(actmodel1)
Anova(actmodel1, type = "III")###interaction with slope is significant for lakes, but size between lakes is not significant
anova(actmodel1,actmodel3)#no sig diff between the model us 1
anova(actmodel1)
ggplot(data = spread_comm, aes(x= log(act_den), y = act_mean_length, color = lake))+
  geom_point(aes(fill = lake), color= "black", pch=21, size = 3)+
  scale_fill_manual(values = c("grey45", "black", "white"))+
  xlab("ln(Density (abundance/host standard length))")+
  ylab("mean length(µm)")+
  geom_errorbar( aes(ymin = act_mean_length-act_se_length, ymax = act_mean_length+act_se_length),width = 0.05, color = "black")+
  stat_smooth(method = lm, aes(linetype = lake), color = "black")+
  #scale_color_manual("lake",values = c("grey45","black","grey99"))+
  #scale_color_manual(values= "gray0")+
  #guides(colour=F)+
  annotate(geom = "text", x = 0, y = 380, label = "A", size = 10)+
  annotate(geom = "text", x = -3.3, y = 315, label = "A", size = 10)+
  annotate(geom = "text", x = -2.8, y = 350, label = "B", size = 10)+
  theme_bw()+
  # geom_vline(xintercept =  0)+
  # geom_hline(yintercept = 440)+
  #  geom_hline(yintercept = 463)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#####Ergas stuff
Erg<-lm(eggs_per~erg, data = with_specifics2)
summary(Erg)
hist(spread_data$erg)



###See how big things affect life on the gill
spread_data$E<-ifelse(spread_data$pres_erg == 1, "pres","abs")
spreadjusteggs<-filter(spread_data, eggs_per > 0)
cleid<-t.test(log10(eggs_per)~E, data = spreadjusteggs)
cleid ####They dont


sizes<-lm(act_mean_length~onch_mean_length, data = spread_data)
summary(sizes)

ggplot(data = spread_data, aes(x= onch_mean_length, y = act_mean_length))+
  geom_point()+
  xlab("Onchocleidus sp. 1 mean length (um)")+
  ylab("Actinocleidus sp. 1 mean length (um)")+
  ggtitle("Body size of Onchocleidus sp and Actinocleidus sp are positively correlated")+
  geom_smooth(method = lm, col = "gray39")+
  geom_errorbar( aes(ymin = act_mean_length-act_se_length, ymax = act_mean_length+act_se_length),width = 0.05)+
  geom_errorbar( aes(xmin = onch_mean_length-onch_se_length, xmax = onch_mean_length+onch_se_length),width = 0.05)+
  annotate(geom = "text", x=315, y = 400, label = "adj r squared = 0.3955")+
  annotate(geom = "text", x=315, y = 385, label = "p<0.01")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ab<-lm(act~onch, data = spread_data)
summary(ab)
ggplot(data = spread_data, aes(x= onch, y = act))+
  geom_point()+
  xlab("Onchocleidus sp. 1")+
  ylab("Actinocleidus sp. 1")+
  ggtitle("Infrapopulation size of Onchocleidus sp and Actinocleidus sp are positively correlated")+
  geom_smooth(method = lm, col = "gray39")+
  annotate(geom = "text", x=10, y = 100, label = "adj r squared = 0.5435")+
  annotate(geom = "text", x=10, y = 95, label = "p<0.01")+
 
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot(ab)




####separate species
RSL_JL<-filter(spread_data, lake!="SKL")
RSL_JL$ac_den<-RSL_JL$act/RSL_JL$host_sl
RSL_JL$onch_den<-RSL_JL$onch/RSL_JL$host_sl
hist(RSL_JL$ac_den)
hist(RSL_JL$onch_den)
acden<-lm(act_mean_length~log10(ac_den), data = RSL_JL[-25,])
summary(acden)
plot(acden)
ggplot(data = RSL_JL, aes(x= log10(ac_den), y = act_mean_length))+
  geom_point()+
  xlab("log10(Actinocleidus sp. 1 density)")+
  ylab("Actinocleidus mean length (um)")+
  ggtitle("Actinocleidus infrapopulation density positively relates to body size in 2 lakes")+
  geom_smooth(method = lm, col = "gray39")+
  annotate(geom = "text", x=-1.25, y = 400, label = "adj r squared = 0.2205")+
  annotate(geom = "text", x=-1.25, y = 395, label = "p<0.01")+
  geom_errorbar( aes(ymin = act_mean_length-act_se_length, ymax = act_mean_length+act_se_length),width = 0.05)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
onden<-lm(onch_mean_length~log10(onch_den), data = RSL_JL)
summary(onden)
ggplot(data = RSL_JL, aes(x= log10(onch_den), y = onch_mean_length))+
  geom_point()+
  xlab("log10(Onchocleidus sp. 1 Density)")+
  ylab("Onchocleidus mean length (um)")+
  ggtitle("Onchocleidus sp. 1 infrapopulation density positively relates to body size in 2 lakes")+
  geom_smooth(method = lm, col = "gray39")+
  annotate(geom = "text", x=-1.75, y = 450, label = "adj r squared = 0.4948")+
  annotate(geom = "text", x=-1.75, y = 440, label = "p<0.01")+
  geom_errorbar( aes(ymin = onch_mean_length-onch_se_length, ymax = onch_mean_length+onch_se_length),width = 0.05)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


RSL_JL$hap_den<-RSL_JL$hap/RSL_JL$host_sl
hist(RSL_JL$hap_den)
hapden<-lm(hap_mean_length~log10(hap_den), data = RSL_JL[-c(8,9,13),])
summary(hapden)
ggplot(data = RSL_JL[-c(8,9,13),], aes(x=log10(hap_den),y=hap_mean_length))+
  geom_point()
plot(hapden)

hist(with_specifics3$size_cwm)
hist(eggs$eggs_per)
spread_comm$tot_den<- spread_comm$total/spread_comm$host_sl

tots<- lm(size_cwm~log(tot_den)*lake, data = spread_comm)
Anova(tots, type = "III")
summary(tots)

ggplot(data = spread_comm, aes(x= log(tot_den), y = size_cwm))+
  geom_point()+
  xlab("log(Total Density)")+
  ylab("Community weighted mean length (um)")+
  geom_smooth(method = lm, col = "black")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot(tots)

eggs<- spread_comm %>% filter(eggs_per > 0)

egglm<-lm(log10(eggs_per)~size_cwm, data = eggs)
summary(egglm)

write.csv(eggs, "eggy_boys.csv")
write.csv(spread_comm, "spread_w_cwm.csv")
