# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("stringi")
# install.packages('lme4')
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("cowplot")
# install.packages("ggpubr")
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(rstan)
library(lme4)
library(cowplot)
library(ggpubr)

rm(list=ls())
theme_set(theme_bw())
#setwd("C:/Users/LSP005/ownCloud/Professionnel/Projet Gender-neutral")
setwd("C:/Users/LeoVarnet/ownCloud/Professionnel/Projet Gender-neutral")

# Loading data exp 0 ------------------------------------------------------

filename_exp0 = "results_exp0.xlsx" #"results_exp1_clean.xlsx"

L0 <- read_excel(filename_exp0, sheet = 1, na = "---", col_names = FALSE)
L0 <- L0[,-2]
names_here = names(L0)
names(L0) <- c("item", "bias")

# Loading data exp 1 ------------------------------------------------------

filename_exp1 = "results_exp1.xlsx" #"results_exp1_clean.xlsx" #

L1 <- read_excel(filename_exp1, sheet = 1, na = "---", col_names = TRUE, skip = 2)
L1 <- L1[,c(-3:-4)]
names_here = names(L1)
names(L1) <- c("item", "gender",names_here[-1:-2])
L1[L1 == "e"] <- NA
L1[L1 == "cut"] <- NA
L1[,-1:-2] <- sapply(L1[,-1:-2],as.numeric)
L1=melt(L1,c("item","gender"),value.name = "RT")
names(L1)[3]<-"subject"
"Gender-neutral"
L1$"condition"<-"Gender-neutral"
L1$"list"<-"L1"
levels(L1$subject)<-paste("L1",levels(L1$subject))

L2 <- read_excel(filename_exp1, sheet = 2, na = "---", col_names = TRUE, skip = 2)
L2 <- L2[,c(-3:-4)]
names_here = names(L2)
names(L2) <- c("item", "gender",names_here[-1:-2])
L2[L2 == "e"] <- NA
L2[L2 == "cut"] <- NA
L2[,-1:-2] <- sapply(L2[,-1:-2],as.numeric)
L2=melt(L2,c("item","gender"),value.name = "RT")
names(L2)[3]<-"subject"
"Gender-neutral"
L2$"condition"<-"Gender-neutral"
L2$"list"<-"L2"
levels(L2$subject)<-paste("L2",levels(L1$subject))

data_exp1<-rbind(L1, L2)
data_exp1$gender <- factor(data_exp1$gender)
data_exp1$list <- factor(data_exp1$list)
data_exp1$condition <- factor(data_exp1$condition)
data_exp1$iscorrect <- !is.na(data_exp1$RT)
agg_data_exp1 <- data_exp1 %>% group_by(subject,gender,condition) %>% summarise(mean_RT=mean(RT, na.rm = TRUE),mean_PC=mean(iscorrect, na.rm = TRUE),.groups = 'drop')

# Loading data exp 2 ------------------------------------------------------

filename_exp2 = "results_exp2.xlsx" #"results_exp2_clean.xlsx" #

SL1 <- read_excel(filename_exp2, sheet = 1, na = "---", col_names = TRUE, skip = 2)
SL1 <- SL1[,c(-1:-3,-6:-7)]
names_here = names(SL1)
names(SL1) <- c("item", "gender",names_here[-1:-2])
SL1[SL1 == "e"] <- NA
SL1[SL1 == "cut"] <- NA
SL1[,-1:-2] <- sapply(SL1[,-1:-2],as.numeric)
SL1=melt(SL1,c("item","gender"),value.name = "RT")
names(SL1)[3]<-"subject"
SL1$"condition"<-"Gender-neutral"
SL1$"list"<-"L1"
levels(SL1$subject)<-paste("SL1",levels(SL1$subject))

SL2 <- read_excel(filename_exp2, sheet = 2, na = "---", col_names = TRUE, skip = 2)
SL2 <- as.data.frame(SL2)
SL2 <- SL2[,c(-1:-3,-6:-7)]
names_here = names(SL2)
names(SL2) <- c("item", "gender",names_here[-1:-2])
SL2[SL2 == "e"] <- NA
SL2[SL2 == "cut"] <- NA
SL2[,-1:-2] <- sapply(SL2[,-1:-2],as.numeric)
SL2=melt(SL2,c("item","gender"),value.name = "RT")
names(SL2)[3]<-"subject"
SL2$"condition"<-"Gender-neutral"
SL2$"list"<-"L2"
levels(SL2$subject)<-paste("SL2",levels(SL2$subject))

IL1 <- read_excel(filename_exp2, sheet = 3, na = "---", col_names = TRUE, skip = 2)
IL1 <- as.data.frame(IL1)
IL1 <- IL1[,c(-1:-3,-6:-7)]
names_here = names(IL1)
names(IL1) <- c("item", "gender",names_here[-1:-2])
IL1[IL1 == "e"] <- NA
IL1[IL1 == "cut"] <- NA
IL1[,-1:-2] <- sapply(IL1[,-1:-2],as.numeric)
IL1=melt(IL1,c("item","gender"),value.name = "RT")
names(IL1)[3]<-"subject"
IL1$"condition"<-"Double"
IL1$"list"<-"L1"
levels(IL1$subject)<-paste("IL1",levels(IL1$subject))

IL2 <- read_excel(filename_exp2, sheet = 4, na = "---", col_names = TRUE, skip = 2)
IL2 <- as.data.frame(IL2)
IL2 <- IL2[,c(-1:-3,-6:-7)]
names_here = names(IL2)
names(IL2) <- c("item", "gender",names_here[-1:-2])
IL2[IL2 == "e"] <- NA
IL2[IL2 == "cut"] <- NA
IL2[,-1:-2] <- sapply(IL2[,-1:-2],as.numeric)
IL2=melt(IL2,c("item","gender"),value.name = "RT")
names(IL2)[3]<-"subject"
IL2$"condition"<-"Double"
IL2$"list"<-"L2"
levels(IL2$subject)<-paste("IL2",levels(IL2$subject))

data_exp2<-rbind(SL1, SL2, IL1, IL2)
data_exp2$gender <- factor(data_exp2$gender)
data_exp2$condition <- factor(data_exp2$condition)
data_exp2$list <- factor(data_exp2$list)
data_exp2$iscorrect <- !is.na(data_exp2$RT)
agg_data_exp2 <- data_exp2 %>% group_by(subject,gender,condition) %>% summarise(mean_RT=mean(RT, na.rm = TRUE),mean_PC=mean(iscorrect, na.rm = TRUE),.groups = 'drop')
agg_data_exp2$mean_RT_z = (agg_data_exp2$mean_RT - mean(agg_data_exp2$mean_RT, na.rm = TRUE))/sd(agg_data_exp2$mean_RT, na.rm = TRUE)

# Frequentist stats -------------------------------------------------------

# simple ANOVA

lm1_exp1 = aov(mean_RT~gender+Error(subject/gender), data=agg_data_exp2)
summary(lm1_exp1)

lm1_exp2 = aov(mean_RT~condition*gender+Error(subject/gender), data=agg_data_exp2)
summary(lm1_exp2)

# mixed ANOVA with random effect of subject
lm2_exp1 = lmer(mean_RT~gender+(1|subject), data=agg_data_exp1)
summary(lm2_exp1)

lm2_exp2 = lmer(mean_RT~condition*gender+(1|subject), data=agg_data_exp2)
summary(lm2_exp2)

# data for STAN exp 1 -----------------------------------------------------------

# agg_data_exp1_L: mean RT data in list format
agg_data_exp1_L <- list( subject = as.numeric(agg_data_exp1$subject),
                           gender = as.numeric(agg_data_exp1$gender=="M"),
                           condition = as.numeric(agg_data_exp1$condition=="Double"),
                           mean_RT = agg_data_exp1$mean_RT,
                           mean_RT_z = (agg_data_exp1$mean_RT-mean(agg_data_exp1$mean_RT))/sd(agg_data_exp1$mean_RT),
                           N = length(agg_data_exp1$mean_RT),
                           mean_PC = agg_data_exp1$mean_PC)
agg_data_exp1_L$Nsubject = max(agg_data_exp1_L$subject)

# aggpaired_data_exp1_L: mean RT data in list format with only the difference between cond
aggpaired_data_exp1_L = list();
Ndat = length(agg_data_exp1_L$subject)
aggpaired_data_exp1_L$subject = agg_data_exp1_L$subject[seq(1, Ndat, 2)]
aggpaired_data_exp1_L$condition = agg_data_exp1_L$condition[seq(1, Ndat, 2)]
aggpaired_data_exp1_L$mean_RT_diff = diff(agg_data_exp1_L$mean_RT)[seq(1, Ndat, 2)]
aggpaired_data_exp1_L$mean_RT_F = agg_data_exp1_L$mean_RT[seq(1, Ndat, 2)]
aggpaired_data_exp1_L$mean_RT_M = agg_data_exp1_L$mean_RT[seq(2, Ndat, 2)]
aggpaired_data_exp1_L$mean_RT_diff_z = (aggpaired_data_exp1_L$mean_RT_diff-mean(aggpaired_data_exp1_L$mean_RT_diff))/sd(aggpaired_data_exp1_L$mean_RT_diff)
aggpaired_data_exp1_L$N = length(aggpaired_data_exp1_L$subject)
aggpaired_data_exp1_L$mean_PC_F = agg_data_exp1_L$mean_PC[seq(1, Ndat, 2)]
aggpaired_data_exp1_L$mean_PC_M = agg_data_exp1_L$mean_PC[seq(2, Ndat, 2)]

# agg_data_exp1_L: all RT data in list format
RTdata_exp1_L <- list( subject = as.numeric(data_exp1$subject),
                       item = as.numeric(data_exp1$item),
                       gender = as.numeric(data_exp1$gender=="M"),
                       condition = as.numeric(data_exp1$condition=="Double"),
                       iscorrect = data_exp1$iscorrect,
                       RT_z = (data_exp1$RT-mean(data_exp1$RT, na.rm = TRUE))/sd(data_exp1$RT, na.rm = TRUE))
RTdata_exp1_L$Nsubject = max(RTdata_exp1_L$subject)
RTdata_exp1_L$Nitem = max(RTdata_exp1_L$item)
RTdata_exp1_L$N = length(RTdata_exp1_L$RT_z)
PCdata_exp1_S = RTdata_exp1_L

# remove errors
RTdata_exp1_L$subject = RTdata_exp1_L$subject[!is.na(RTdata_exp1_L$RT_z)]
RTdata_exp1_L$item = RTdata_exp1_L$item[!is.na(RTdata_exp1_L$RT_z)]
RTdata_exp1_L$gender = RTdata_exp1_L$gender[!is.na(RTdata_exp1_L$RT_z)]
RTdata_exp1_L$condition = RTdata_exp1_L$condition[!is.na(RTdata_exp1_L$RT_z)]
RTdata_exp1_L$RT_z = RTdata_exp1_L$RT_z[!is.na(RTdata_exp1_L$RT_z)]
RTdata_exp1_L$RT_z = (RTdata_exp1_L$RT_z - mean(RTdata_exp1_L$RT_z))/sd(RTdata_exp1_L$RT_z)
RTdata_exp1_L$N = length(RTdata_exp1_L$RT_z)

# data for STAN exp 2 -----------------------------------------------------------

# agg_data_exp2_L: mean RT data in list format
agg_data_exp2_L <- list( subject = as.numeric(agg_data_exp2$subject),
                           gender = as.numeric(agg_data_exp2$gender=="M"),
                           condition = as.numeric(agg_data_exp2$condition=="Double"),
                           mean_RT_z = (agg_data_exp2$mean_RT-mean(agg_data_exp2$mean_RT))/sd(agg_data_exp2$mean_RT),
                           mean_RT = agg_data_exp2$mean_RT,
                           mean_PC = agg_data_exp2$mean_PC,
                           N = length(agg_data_exp2$mean_RT))
agg_data_exp2_L$Nsubject = max(agg_data_exp2_L$subject)

# aggpaired_data_exp2_L: mean RT data in list format with only the difference between cond
aggpaired_data_exp2_L = list();
Ndat = length(agg_data_exp2_L$subject)
aggpaired_data_exp2_L$subject = agg_data_exp2_L$subject[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$condition = agg_data_exp2_L$condition[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_diff = diff(agg_data_exp2_L$mean_RT)[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_diff_z = (aggpaired_data_exp2_L$mean_RT_diff-mean(aggpaired_data_exp2_L$mean_RT_diff))/sd(aggpaired_data_exp2_L$mean_RT_diff)
aggpaired_data_exp2_L$N = length(aggpaired_data_exp2_L$subject)
aggpaired_data_exp2_L$mean_RT_F = agg_data_exp2_L$mean_RT[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_M = agg_data_exp2_L$mean_RT[seq(2, Ndat, 2)]
aggpaired_data_exp2_L$mean_PC_F = agg_data_exp2_L$mean_PC[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_PC_M = agg_data_exp2_L$mean_PC[seq(2, Ndat, 2)]

# agg_data_exp2_L: all RT data in list format
RTdata_exp2_L <- list( subject = as.numeric(data_exp2$subject),
                       item = as.numeric(data_exp2$item),
                       gender = as.numeric(data_exp2$gender=="M"),
                       condition = as.numeric(data_exp2$condition=="Double"),
                       iscorrect = data_exp2$iscorrect,
                       #RT = data_exp2$RT,
                       RT_z = (data_exp2$RT-mean(data_exp2$RT, na.rm = TRUE))/sd(data_exp2$RT, na.rm = TRUE))
RTdata_exp2_L$Nsubject = max(RTdata_exp2_L$subject)
RTdata_exp2_L$Nitem = max(RTdata_exp2_L$item)
RTdata_exp2_L$N = length(RTdata_exp2_L$RT_z)
CRdata_exp2_L = RTdata_exp2_L

# remove errors
RTdata_exp2_L$subject = RTdata_exp2_L$subject[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$item = RTdata_exp2_L$item[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$gender = RTdata_exp2_L$gender[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$condition = RTdata_exp2_L$condition[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$RT_z = RTdata_exp2_L$RT_z[!is.na(RTdata_exp2_L$RT_z)]
#RTdata_exp2_L$RT = RTdata_exp2_L$RT[!is.na(RTdata_exp2_L$RT)]
#RTdata_exp2_L$RT_z = (RTdata_exp2_L$RT_z - mean(RTdata_exp2_L$RT_z))/sd(RTdata_exp2_L$RT_z)
RTdata_exp2_L$N = length(RTdata_exp2_L$RT_z)

# Plotting ----------------------------------------------------------------

pairedplot <- function(mean_RT_F,mean_RT_M,condition){
  if (min(condition)==max(condition))
    {xvalue = 1
    y = c(mean_RT_F,mean_RT_M)
   d <- data.frame(y=y, 
                  x = rep(c(-1,+1), each=length(mean_RT_F)),
                  id = factor(rep(1:length(mean_RT_F),2)))
  d$xj <- jitter(d$x, amount=.2)
  d$Gender = factor(d$x, labels=c("F","M"))
  pal = c("forestgreen",  "orange")
  ggplot(data=d, aes(y=y)) +
    geom_boxplot(aes(x=xvalue+x, group = Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(aes(x=xvalue+0.5*xj, group = id), color = "gray" ,linewidth = 0.5)+
    geom_point(aes(x=xvalue+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal)  +
    theme_bw()}
  else
  {xvalue0 = 1
  xvalue1 = 5
  y = c(mean_RT_F[condition==0],mean_RT_M[condition==0])
  d0 <- data.frame(y=y, 
                  x = rep(c(-1,+1), each=length(mean_RT_F[condition==0])),
                  id = factor(rep(1:length(mean_RT_F[condition==0]),2)))
  d0$xj <- jitter(d0$x, amount=.2)
  d0$Gender = factor(d0$x, labels=c("F","M"))
  
  y = c(mean_RT_F[condition==1],mean_RT_M[condition==1])
  d1 <- data.frame(y=y, 
                   x = rep(c(-1,+1), each=length(mean_RT_F[condition==1])),
                   id = factor(rep(1:length(mean_RT_F[condition==1]),2)))
  d1$xj <- jitter(d1$x, amount=.2)
  d1$Gender = factor(d1$x, labels=c("F","M"))
  
  pal = c("forestgreen",  "orange")
  ggplot(data=d0, aes(y=y)) +
    geom_boxplot(aes(x=xvalue0+x, group=Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(aes(x=xvalue0+0.5*xj, group=id), color = "gray",linewidth = 0.5)+
    geom_point(aes(x=xvalue0+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
    geom_boxplot(data=d1, aes(x=xvalue1+x, group=Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(data=d1, aes(x=xvalue1+0.5*xj, group=id), color = "gray",linewidth = 0.5)+
    geom_point(data=d1, aes(x=xvalue1+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
    theme_bw()}
  }

# plot1 = ggplot(data = as.data.frame(aggpaired_data_exp1_L)) +
#   geom_segment(mapping = aes(x = 1-0.25, y = mean_RT_F, xend = 1+0.25, yend = mean_RT_M), inherit.aes = FALSE) +
#   geom_violin(mapping = aes(x = 1, y = mean_RT_F), inherit.aes = FALSE,trim=FALSE, fill="chartreuse2", width=0.2, position=position_nudge(x=-0.25))+
#   geom_point(mapping = aes(x = 1-0.25, y = mean_RT_F), inherit.aes = FALSE, colour="black", size = 2.5)+
#   geom_violin(mapping = aes(x = 1, y = mean_RT_M), inherit.aes = FALSE,trim=FALSE, fill="darkgoldenrod2", width=0.2, position=position_nudge(x=0.25))+
# geom_point(mapping = aes(x = 1+0.25, y = mean_RT_M), inherit.aes = FALSE, colour="black", size = 2.5)+
#   
#   ylim(500,4000) + xlim(0.5,1.5) + ggtitle("Experiment 1") + theme(legend.position="none") + labs(y = "mean RT", x = "gender") 
# plot1

plot1 = pairedplot(aggpaired_data_exp1_L$mean_RT_F,aggpaired_data_exp1_L$mean_RT_M,aggpaired_data_exp1_L$condition)
plot1 <- plot1 + ylim(500,4000) + ggtitle("Experiment 1") + theme(legend.position="none") + ylab("Reaction Time (ms)")
plot1 <- plot1 + xlab("") + scale_x_continuous(breaks=1, labels="Gender-unmarked", limits=c(-0.5, 2.5))

plot2 = pairedplot(aggpaired_data_exp2_L$mean_RT_F,aggpaired_data_exp2_L$mean_RT_M,aggpaired_data_exp2_L$condition)
plot2 <- plot2 + ylim(500,4000) + ggtitle("Experiment 2") + theme(legend.position="none") + ylab("Reaction Time (ms)")
plot2 <- plot2 + xlab("") + scale_x_continuous(breaks=c(1,5), labels=c("Gender-unmarked","Contract. double form"))

plot3 = pairedplot(100*aggpaired_data_exp1_L$mean_PC_F,100*aggpaired_data_exp1_L$mean_PC_M,aggpaired_data_exp1_L$condition)
plot3 <- plot3 + ylim(0,100) + ggtitle("Experiment 1") + theme(legend.position="none") + ylab("Percent correct (%)")
plot3 <- plot3 + xlab("Condition") + scale_x_continuous(breaks=1, labels="Gender-unmarked", limits=c(-0.5, 2.5))

plot4 = pairedplot(100*aggpaired_data_exp2_L$mean_PC_F,100*aggpaired_data_exp2_L$mean_PC_M,aggpaired_data_exp2_L$condition)
plot4 <- plot4 + ylim(0,100) + ggtitle("Experiment 2") + theme(legend.position="right") + ylab("Percent correct (%)")
plot4 <- plot4 + xlab("Condition") + scale_x_continuous(breaks=c(1,5), labels=c("Gender-unmarked","Contract. double form"))

plotR = plot_grid(plot2, plot4, nrow=2, axis = "r", align = "v")
plotL = plot_grid(plot1, plot3, nrow=2, labels=c("A","B"), axis = "r", align = "v")
plot_grid(plotL,plotR, ncol=2, rel_widths = c(1/3, 2/3))

# plot1 = ggplot(data = agg_data_exp1, mapping = aes(x = condition, y = mean_RT, group = gender, fill = gender)) + labs(y = "mean RT", x="") +
#   geom_boxplot(width=0.2, position=position_dodge(1.1), outlier.shape = NA, show.legend = FALSE) +   
#   #geom_dotplot(binaxis = "y",stackdir = "center", binwidth = 1, position=position_dodge(0.5), dotsize = 2) + 
#   geom_line(color = "gray") +
#   geom_point(aes(fill = gender, group = gender, color = gender), position=position_jitterdodge(jitter.width = 0.2,dodge = 0.5), stroke = NA) + scale_color_manual(values = pal) + 
#   ylim(500,4000) + ggtitle("Experiment 1") + theme(legend.position="none") + scale_fill_manual(values = pal)
# 
# plot2 = ggplot(data = agg_data_exp2, mapping = aes(x = condition, y = mean_RT, fill = gender)) + labs(y = "mean RT") +
#   geom_boxplot(width=0.2, position=position_dodge(1.1), outlier.shape = NA, show.legend = FALSE) + 
#   geom_point(aes(fill = gender, group = gender, color = gender), position=position_jitterdodge(jitter.width = 0.2,dodge = 0.5), stroke = NA, show.legend = FALSE) + scale_color_manual(values = pal) + 
#   #geom_dotplot(binaxis = "y",stackdir = "center", binwidth = 1, position=position_dodge(0.5), dotsize = 2) + 
#   ylim(500,4000) + ggtitle("Experiment 2") + scale_fill_manual(values = pal)
# 
# plot3 = ggplot(data = agg_data_exp1, mapping = aes(x = condition, y = mean_corr, group = gender, fill = gender)) + labs(y = "percent correct") +
#   geom_boxplot(width=0.2, position=position_dodge(1.1), outlier.shape = NA, show.legend = FALSE) +   
#   #geom_dotplot(binaxis = "y",stackdir = "center", binwidth = 1, position=position_dodge(0.5), dotsize = 2) + 
#   geom_point(aes(fill = gender, group = gender, color = gender), position=position_jitterdodge(jitter.width = 0.2,dodge = 0.5), stroke = NA) + scale_color_manual(values = pal) + 
#   ylim(0,1) + theme(legend.position="none") + scale_fill_manual(values = pal)# + ggtitle("Experiment 1") 
# 
# plot4 = ggplot(data = agg_data_exp2, mapping = aes(x = condition, y = mean_corr, fill = gender)) + labs(y = "percent correct") +
#   geom_boxplot(width=0.2, position=position_dodge(1.1), outlier.shape = NA, show.legend = FALSE) + 
#   geom_point(aes(fill = gender, group = gender, color = gender), position=position_jitterdodge(jitter.width = 0.2,dodge = 0.5), stroke = NA) + scale_color_manual(values = pal) + 
#   #geom_dotplot(binaxis = "y",stackdir = "center", binwidth = 1, position=position_dodge(0.5), dotsize = 2) + 
#   ylim(0,1) + scale_fill_manual(values = pal)#+ ggtitle("Experiment 2")
# 
# plotR = plot_grid(plot2, plot4, nrow=2, axis = "r", align = "v")
# plotL = plot_grid(plot1, plot3, nrow=2, labels=c("A","B"), axis = "r", align = "v")
# plot_grid(plotL,plotR, ncol=2, rel_widths = c(1/3, 2/3))
# 
# paired = data.frame(mean_RT_F = aggpaired_data_exp1_L$mean_RT_F, mean_RT_M = aggpaired_data_exp1_L$mean_RT_M)
# ggpaired(paired, cond1 = "mean_RT_F", cond2 = "mean_RT_M", color = "condition", line.color = "gray", line.size = 0.4, palette = pal)


# analysis of RT data exp 1-----------------------------------------------------
# 
# # simple bayesian ANOVA
# n1.1 <- stan_model(file = 'n1.1.stan')
# lm1.1_exp1 <- sampling(n1.1,
#                      data = agg_data_exp1_L,
#                      chains = 7,             # number of Markov chains
#                      warmup = 3000,          # number of warmup iterations per chain
#                      iter = 7000,            # total number of iterations per chain
#                      refresh = 1000)
# parameters = c("beta_0","beta_gender")#,"sigma"
# print(lm1.1_exp1, pars = parameters)
# plot(lm1.1_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n1.1") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# # simple bayesian paired ANOVA
# n1.2 <- stan_model(file = 'n1.2.stan')
# lm1.2_exp1 <- sampling(n1.2,
#                      data = aggpaired_data_exp1_L,
#                      chains = 7,             # number of Markov chains
#                      warmup = 3000,          # number of warmup iterations per chain
#                      iter = 7000,            # total number of iterations per chain
#                      refresh = 1000)
# parameters = c("beta_0")#,"sigma"
# print(lm1.2_exp1, pars = parameters)
# plot(lm1.2_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n1.2") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# # hierarchical model with random effect of subject (applied on mean RT)
# n1.1h <- stan_model(file = 'n1.1h.stan')
# lm1.1h_exp1 <- sampling(n1.1h,
#                       data = agg_data_exp1_L,
#                       chains = 7,             # number of Markov chains
#                       warmup = 3000,          # number of warmup iterations per chain
#                       iter = 7000,            # total number of iterations per chain
#                       refresh = 1000)
# parameters = c("beta_0","beta_gender")#,"sigma"
# print(lm1.1h_exp1, pars = parameters)
# plot(lm1.1h_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n1.1h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# # hierarchical model with random effect of subject on intercept (applied on RT)
# n2.1h <- stan_model(file = 'n2.1h.stan')
# lm2.1h_exp1 <- sampling(n2.1h,
#                       data = RTdata_exp1_L,
#                       chains = 3,             # number of Markov chains
#                       warmup = 3000,          # number of warmup iterations per chain
#                       iter = 7000,            # total number of iterations per chain
#                       refresh = 1000)
# parameters = c("beta_0","beta_gender")#,"sigma"
# print(lm2.1h_exp1, pars = parameters)
# plot(lm2.1h_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n2.1h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# hierarchical model with random effect of subject and item on intercept (applied on RT)
n2.2h <- stan_model(file = 'n2.2h.stan')
lm2.2h_exp1 <- sampling(n2.2h,
                      data = RTdata_exp1_L,
                      chains = 7,             # number of Markov chains
                      warmup = 3000,          # number of warmup iterations per chain
                      iter = 7000,            # total number of iterations per chain
                      refresh = 1000)
parameters = c("beta_0","beta_gender")#,"sigma"
parameter_names = rev(c(expression(beta[0]),expression(beta[gender])))#,"sigma"
print(lm2.2h_exp1, pars = parameters)
#plot(lm2.2h_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n2.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot5 <- stan_plot(lm2.2h_exp1, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  ggtitle("reaction-time models") + theme(plot.title = element_text(size = 12)) +
  #ggtitle("") + 
  xlim(-1,1) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot5 = plot5 + scale_y_continuous(breaks=1:length(parameters),labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#

## analysis of correctness data

n3.2h <- stan_model(file = 'n3.2h.stan')
lm3.2h_exp1 <- sampling(n3.2h,
                      data = PCdata_exp1_S,
                      chains = 7,             # number of Markov chains
                      warmup = 3000,          # number of warmup iterations per chain
                      iter = 7000,            # total number of iterations per chain
                      refresh = 1000)
parameters = c("beta_0","beta_gender")#,"plapse","sigma"
parameter_names = rev(c(expression(beta[0]),expression(beta[gender])))#,expression(plapse),"sigma"
print(lm3.2h_exp1, pars = parameters)
#plot(lm3.2h_exp1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("n3.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot6 <- stan_plot(lm3.2h_exp1, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  ggtitle("percent-correct models") + theme(plot.title = element_text(size = 12)) +
  #ggtitle("") + 
  xlim(-4,4) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot6 = plot6 + scale_y_continuous(breaks=1:length(parameters),labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#


# # analysis of RT data exp 2-----------------------------------------------------
# 
# # simple bayesian ANOVA
# m1.1 <- stan_model(file = 'm1.1.stan')
# fit.m1.1 <- sampling(m1.1,
#                      data = agg_data_exp2_L,
#                      chains = 7,             # number of Markov chains
#                      warmup = 3000,          # number of warmup iterations per chain
#                      iter = 7000,            # total number of iterations per chain
#                      refresh = 1000)
# parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"sigma"
# print(fit.m1.1, pars = parameters)
# plot(fit.m1.1, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m1.1") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)

# # simple bayesian paired ANOVA
# m1.2 <- stan_model(file = 'm1.2.stan')
# fit.m1.2 <- sampling(m1.2,
#                      data = aggpaired_data_exp2_L,
#                      chains = 7,             # number of Markov chains
#                      warmup = 3000,          # number of warmup iterations per chain
#                      iter = 7000,            # total number of iterations per chain
#                      refresh = 1000)
# parameters = c("beta_0","beta_condition")#,"sigma"
# print(fit.m1.2, pars = parameters)
# plot(fit.m1.2, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m1.2") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# # hierarchical model with random effect of subject (applied on mean RT)
# m1.1h <- stan_model(file = 'm1.1h.stan')
# fit.m1.1h <- sampling(m1.1h,
#                      data = agg_RTdata4STAN,
#                      chains = 7,             # number of Markov chains
#                      warmup = 3000,          # number of warmup iterations per chain
#                      iter = 7000,            # total number of iterations per chain
#                      refresh = 1000)
# parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"sigma"
# print(fit.m1.1h, pars = parameters)
# plot(fit.m1.1h, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m1.1h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
# 
# # hierarchical model with random effect of subject on intercept (applied on RT)
# m2.1h <- stan_model(file = 'm2.1h.stan')
# fit.m2.1h <- sampling(m2.1h,
#                       data = RTdata4STAN,
#                       chains = 3,             # number of Markov chains
#                       warmup = 3000,          # number of warmup iterations per chain
#                       iter = 7000,            # total number of iterations per chain
#                       refresh = 1000)
# parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"sigma"
# print(fit.m2.1h, pars = parameters)
# plot(fit.m2.1h, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m2.1h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)

# hierarchical model with random effect of subject and item on intercept (applied on RT)
m2.2h <- stan_model(file = 'm2.2h.stan')
lm2.2h_exp2 <- sampling(m2.2h,
                      data = RTdata_exp2_L,
                      chains = 7,             # number of Markov chains
                      warmup = 3000,          # number of warmup iterations per chain
                      iter = 7000,            # total number of iterations per chain
                      refresh = 1000)
parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"sigma"
#parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition","sigma","gammaz_0i","gammaz_0s")
parameter_names = rev(c(expression(beta[0]),expression(beta[gender]),expression(beta[condition]),expression(beta[gender%*%condition])))#,"sigma"
print(lm2.2h_exp2, pars = parameters)
#plot(lm2.2h_exp2, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m2.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot7 <- stan_plot(lm2.2h_exp2, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  #ggtitle("reaction-time model") + 
  ggtitle("") + 
  xlim(-1,1) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot7 = plot7 + scale_y_continuous(breaks=1:length(parameters),labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#


## analysis of correctness data

m3.2h <- stan_model(file = 'm3.2h.stan')
fit.m3.2h <- sampling(m3.2h,
                      data = CRdata_exp2_L,
                      chains = 7,             # number of Markov chains
                      warmup = 3000,          # number of warmup iterations per chain
                      iter = 7000,            # total number of iterations per chain
                      refresh = 1000)
parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"plapse","sigma"
parameter_names = rev(c(" ",expression(beta[0]),expression(beta[gender]),expression(beta[condition]),expression(beta[gender%*%condition])))#,"sigma"
print(fit.m3.2h, pars = parameters)
#plot(fit.m3.2h, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m3.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot8 <- stan_plot(fit.m3.2h, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  #ggtitle("percent-correct model") + #ggtitle("percent-correct model") + 
  #ggtitle("") + 
  xlim(-4,4) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot8 = plot8 + scale_y_continuous(labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#

plotR = plot_grid(plot6, plot8, nrow=2, rel_heights = c(2/5, 3/5), axis = "l", align = "hv")
plotL = plot_grid(plot5, plot7, nrow=2, rel_heights = c(2/5, 3/5), axis = "l", align = "hv")
#plot_grid(plotL,plotR, ncol=2,labels=c("A. Reaction-time model","B. Percent-correct model"))
plot_grid(plotL,plotR, ncol=2,labels=c("A.","B."))


plot_grid(plot5,plot6, ncol=2, labels=c("A. Reaction-time model","B. Percent-correct model"), axis = "r", align = "v")

## test for an effect of stereotypes
agg_item <- list(       item = as.numeric(L0$item),
                       bias = as.numeric(L0$bias),
                       N = length(L0$item))

gammaz = (extract(lm2.2h_exp1, "gammaz_0i"))
agg_item$gammazRT_exp1 = colMeans(gammaz$gammaz_0i)
gammaz = (extract(lm3.2h_exp1, "gammaz_0i"))
agg_item$gammazCR_exp1 = colMeans(gammaz$gammaz_0i)

gammaz = (extract(lm2.2h_exp2, "gammaz_0i"))
agg_item$gammazRT_exp2 = colMeans(gammaz$gammaz_0i)
gammaz = (extract(lm3.2h_exp1, "gammaz_0i"))
agg_item$gammazCR_exp2 = colMeans(gammaz$gammaz_0i)

agg_item$y = matrix(0,length(agg_item$gammazCR_exp2),2)
agg_item$N = length(agg_item$gammazCR_exp2)
agg_item$K = 2
agg_item$y[,1] = agg_item$gammazCR_exp2
agg_item$y[,2] = agg_item$gammazRT_exp2

multivariatemodel <- stan_model(file = 'multivariate.stan')
multivariatemodel.1 <- sampling(multivariatemodel,
                       data = agg_item,
                       chains = 7,             # number of Markov chains
                       warmup = 3000,          # number of warmup iterations per chain
                       iter = 7000,            # total number of iterations per chain
                       refresh = 1000)
parameters = c("mu","Sigma","R")
print(multivariatemodel.1, pars = parameters)


