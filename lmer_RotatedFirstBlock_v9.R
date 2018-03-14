#affective ratings data; collected summer-fall 2017
#linear mixed model of first order images post rotation

#SET UP====================================================================================================================================
library(ez)
library(lme4)
library(lmerTest)
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(mousetrap)
library(lattice)
library(ggpubr)
library(ggExtra)
library(Hmisc)
library(tidyverse)
library(viridis)
library(psych)
library(beanplot)
library(snpar)
library(magrittr)
library(psych)
library(car)
library(sjmisc)
library(sjPlot)

#Go to folder with data and script
setwd("~/Dropbox/Dartmouth/Research/Whalen/Projects/affective_ratings/Rotation_FirstBlockOnly/")

#NONPCA DF==============================================================================================================================
#Read in data of first block only 
arousal_data <- read.csv("~/Dropbox/Dartmouth/Research/Whalen/Projects/affective_ratings/Rotation_FirstBlockOnly/arousal_firstblockonly.csv")
arousal_data_trimmed <- arousal_data[,1:37]
valence_data <- read.csv("~/Dropbox/Dartmouth/Research/Whalen/Projects/affective_ratings/Rotation_FirstBlockOnly/valence_firstblockonly.csv")
#Put data into long form (for mixed effects model)
rating_data_long <-melt(arousal_data_trimmed, id=c("Internal_ID", measure.vars = c("Ethnicity",	"Race",	"Sex",	"Age",	"Timing", "Phrasing")))
colnames(rating_data_long)[8] <- "Item"
colnames(rating_data_long)[9] <- "Arousal"
temp_rating_data_long <-melt(valence_data, id=c("Internal_ID",	measure.vars = c("Ethnicity",	"Race",	"Sex",	"Age",	"Timing", "Phrasing")))
colnames(temp_rating_data_long)[8] <- "Item"
colnames(temp_rating_data_long)[9] <- "Valence"
#Only unique column in long data frames are Valence and Arousal, so
#add Valence column to rating_data_long so both Arousal and Valence are in a single dataframe
rating_data_long$Valence <- temp_rating_data_long$Valence
rm(temp_rating_data_long)

#Convert variables to factors
rating_data_long$Internal_ID <- factor(rating_data_long$Internal_ID)
rating_data_long$Timing <- factor(rating_data_long$Timing)
rating_data_long$Phrasing <- factor(rating_data_long$Phrasing)
#timing within subject; phrasing btw

#remove subject 61 bc not working for PCA
rating_data_long_omit <- rating_data_long[rating_data_long$Internal_ID !="61",] #61 is N/A

#make gender consistent
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "f"] <- "F"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "m"] <- "M"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "female"] <- "F"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "Female"] <- "F"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "Male"] <- "M"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "male"] <- "M"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "female35"] <- "F"
rating_data_long_omit$Sex[rating_data_long_omit$Sex == "FEMALE"] <- "F"

rating_data_long_omit$Sex <- factor(rating_data_long_omit$Sex)

contrasts(rating_data_long_omit$Sex) <- contr.sum(2)
contrasts(rating_data_long_omit$Timing) <- contr.sum(2)
contrasts(rating_data_long_omit$Phrasing) <- contr.sum(2)

describe(rating_data_long_omit)


#*ITEM DATAFRAME=================================================================================================================
items_ratings <- rating_data_long_omit %>%
  group_by(Item, Timing) %>%
  dplyr::summarize(Valence = mean(Valence), Arousal = mean(Arousal))

#write.csv(items_ratings, "items_ratings_gender.csv")

#descriptives by item
itemssummary <- ddply(items_ratings, c("Valence", "Arousal"), summarise,
                      N    = length(Valence),
                      mean = mean(Valence),
                      sd   = sd(Valence),
                      median = median(Valence)
)


#NONPCA GRAPHS=================================================================================================================
#*fulldata===========================
#box 
plot(rating_data_long_omit$Valence ~ rating_data_long_omit$Timing)
plot(rating_data_long_omit$Arousal ~ rating_data_long_omit$Timing)
plot(rating_data_long_omit$Valence ~ rating_data_long_omit$Phrasing)
plot(rating_data_long_omit$Arousal ~ rating_data_long_omit$Phrasing)

#valence arousal interaction
int1 <- ggplot(rating_data_long_omit, aes(Valence, Arousal, colour = Timing))
int1 + stat_summary(fun.y = mean, geom = "line", size = 2, aes(group=Timing))

#densisty
ggplot() + 
  geom_density(data=
                 rating_data_long_omit, aes(x=Valence, group=Timing, fill=Timing),alpha=0.5, adjust=2) + 
  xlab("Valence") +
  ylab("Density")


#scatter with hist #points are layered bc so many integer repetitions
fulldatascatter <- ggscatter(rating_data_long_omit, x = ("Valence"), y = ("Arousal"),
                         color = "Timing",
                         add = "reg.line", conf.int = TRUE, xlim=c(0,8), ylim =c(0,8))
fulldatascatter + (aes(color = Timing))
ggExtra::ggMarginal(fulldatascatter, type = "histogram", fill = "light blue")

#same result from ggplot
ggplot(rating_data_long_omit, aes(x = Valence, y = Arousal, color = Timing)) +
  geom_point() +
  geom_smooth(method = "lm")

#jittering to add noise to mask the layering of integers
ggplot(rating_data_long_omit, aes(x = Valence, y = Arousal, color = Timing)) +
  geom_jitter(width = .5, height = .5) +
  geom_smooth(method = "lm")

#create count df
countdf <- rating_data_long_omit %>%
  group_by(Valence, Arousal, Timing) %>%
  dplyr::summarize(count=n())
  
#use count df for size plot
ggplot(countdf,aes(x=Valence, y = Arousal)) +
  geom_point(aes(size=count, color = count)) +
  scale_color_viridis() +
  facet_grid(.~Timing)

#2d density
ggplot(rating_data_long_omit, aes(x = Valence, y = Arousal)) + 
  geom_density2d() + 
  facet_grid(Timing~Sex)

#or
ggplot(data= rating_data_long_omit, aes(x=Valence, y=Arousal) ) + 
  geom_point(aes(color= Timing), alpha= .25) +
  coord_cartesian(ylim= c(0, 7), xlim= c(0,7)) +
  stat_density2d(mapping= aes(color= Timing), 
                 geom="contour", bins=7, size= 1)+
  scale_alpha_continuous(limits=c(0,8))+
  scale_color_discrete("Timing")


#sex hist
ggplot(rating_data_long_omit, aes(x=Valence, fill=Sex), xlim = c(0, 8)) +
  geom_histogram(binwidth=.5, position="dodge")

#timing hist
ggplot(rating_data_long_omit, aes(x=Valence, fill=Timing), xlim = c(0, 8)) +
  geom_histogram(binwidth=.2, position="dodge") +
  facet_grid(Timing~Sex)

#nonPCA spaghetti
xyplot(Valence ~ Arousal | Timing, data = rating_data_long_omit, groups = Timing)

#beanplot 
beanplot(Valence ~ Timing, data = rating_data_long_omit, what=c(1,1,1,0), log="",
         ylab = "Valence", side = "both",
         border = NA, beanlinewd = 0.5,  overallline = "median", 
         col = list( "brown2", "cadetblue3"))
#legend("topright", fill = c("brown2", "cadetblue3"), c("Longer, "Shorter"))

#violin
p<-ggplot(rating_data_long_omit, aes(x=Timing, y=Valence, color=Sex)) +
  geom_violin(trim=FALSE)
p

#bean
beanplot(Valence~Timing*Sex, xlab="Valence", ylab="Condition", horizontal = TRUE, main = "Memory Accuracy", side = "both", col = list("purple", c("lightblue", "black")), fill = c("purple", "lightblue"), legend = c("Long", "Short"), data = rating_data_long_omit)

#diverging bars
rating_data_long_omit$Valence_z <- (((rating_data_long_omit$Valence - mean(rating_data_long_omit$Valence))/sd(rating_data_long_omit$Valence))) 
rating_data_long_omit$Valence_type <- ifelse(rating_data_long_omit$Valence_z < 0, "below", "above")  # above / below avg flag
rating_data_long_omit <- rating_data_long_omit[order(rating_data_long_omit$Valence_z), ]  # sort
rating_data_long_omit$`Timing` <- factor(rating_data_long_omit$`Timing`, levels = rating_data_long_omit$`Timing`)  

ggplot(rating_data_long_omit, aes(x=`Timing`, y=Valence_z, label=Valence_z)) + 
  geom_bar(stat='identity', aes(fill=Valence_type), width=.5)  +
  scale_fill_manual(name="Valence", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Valence from Full Data", 
       title= "Diverging Bars") + 
  coord_flip()


#ITEMS GRAPH======================================================================================
#line
ggplot(items_ratings, aes(x=Valence, y=Arousal, group=Timing, color=Timing)) +
  geom_line() + theme_bw()+
  #facet_grid(.~Timing)

#integer bar
par(mar=c(1,1,1,1))
items_ratings$Valence <- as.integer(items_ratings$Valence)
barplot(table(items_ratings$Timing,items_ratings$Valence),beside=T,
        cex.names=1,legend.text=c("Female","Male"),
        args.legend=list(x=7,y=7,cex=1),
        col=c("pink","light blue"))

#scatter by timing 
itemscatterSex <- ggscatter(items_ratings, x = "Valence", y = "Arousal",
                            color = "Timing",
                            add = "reg.line", conf.int = TRUE)
itemscatterSex + stat_cor(aes(color = Timing), label.x = 6) + scale_alpha_continuous(limits=c(0,3))
ggExtra::ggMarginal(itemscatterSex, type = "histogram", fill = "light blue")

#*int effect on items=====from model section====
#arousal no phrasing to test intercept
arousal_test <- lmer(Arousal ~ Timing + (1 | Item), data=items_ratings) #
summary(arousal_test)
#(Intercept)   4.5932     0.0940 29.0000  48.861  < 2e-16 ***


#scatter by sex 
itemscatterSex <- ggscatter(items_ratings, x = "Valence", y = "Arousal",
                         color = "Sex",
                         add = "reg.line", conf.int = TRUE)
itemscatterSex + stat_cor(aes(color = Sex), label.x = 6)
ggExtra::ggMarginal(itemscatterSex, type = "histogram", fill = "light blue")

#int
int2 <- ggplot(items_ratings, aes(Arousal, Valence, colour = Timing))
int2+ stat_summary(fun.y = mean, geom = "line", size = 2, aes(group=Timing))


#count hist
ggplot(data=items_ratings, aes(items_ratings$Valence)) + geom_histogram(breaks=seq(0, 5, by=.25), col="red", aes(fill=..count..)) + scale_fill_gradient("Count", low="green", high="blue")

ggplot(items_ratings, aes(x = Valence, fill = Timing), xlim=c(0,8)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count", low="green", high="blue") 


#timing hist
hist <- ggplot(items_ratings, aes(x=Valence, fill=Timing), xlim = c(0, 7)) +
  geom_histogram(binwidth=1, position="dodge")
hist + scale_x_continuous(limit = c(0, 7)) + facet_grid(.~Timing)

#gender hist
ggplot(items_ratings, aes(x=Valence, fill=Sex), xlim = c(0, 8)) +
  geom_histogram(binwidth=1, position="dodge")

#density gender
ggplot(items_ratings, aes(x=Valence, fill=Sex)) + geom_density(alpha=.3)

#density timing
ggplot(items_ratings, aes(x=Valence, fill=Timing)) + geom_density(alpha=.3)


#2d density
d <- ggplot(items_ratings, aes(x = Valence, y = Arousal), xlim = c(8)) + 
  geom_density2d(aes(color= Timing)) + 
  facet_grid(Sex~Timing)
d + scale_x_continuous(limit = c(0, 8))
#or
ggplot(data= items_ratings, aes(x=Valence, y=Arousal) ) + 
  geom_point(aes(color= Timing), alpha= .25) +
  coord_cartesian(ylim= c(0, 7), xlim= c(0,7)) + scale_y_reverse() +
  stat_density2d(mapping= aes(color= Timing), 
                 geom="contour", bins=4, size= 2)+
                 scale_alpha_continuous(limits=c(0,1e-5))+
                 scale_color_discrete("Timing")

#density timing
ggplot(items_ratings, aes(x=Valence, fill=Sex)) + geom_density(alpha=.3)

#slight gender effect
boxplot(items_ratings$Valence~items_ratings$Timing*items_ratings$Sex, ylim=c(0,3))

#bean
beanplot(Valence~Timing, xlab="Valence", ylab="Condition", horizontal = TRUE, main = "Memory Accuracy", side = "both", col = list("purple", c("lightblue", "black")), fill = c("purple", "lightblue"), legend = c("Long", "Short"), data = items_ratings)

#violin
d<-ggplot(items_ratings, aes(x=Timing, y=Valence, color=Timing)) +
  geom_violin(trim=FALSE)
d

#diverging bars
#Data Prep
items_ratings$`Valence` <- rownames(items_ratings)  # create new column for car names
items_ratings$Valence_z <- (((items_ratings$Valence - mean(items_ratings$Valence))/sd(items_ratings$Valence)))  # compute normalized mpg
items_ratings$Valence_type <- ifelse(items_ratings$Valence_z < 0, "below", "above")  # above / below avg flag
items_ratings <- items_ratings[order(items_ratings$Valence_z), ]  # sort
items_ratings$`Timing` <- factor(items_ratings$`Timing`, levels = items_ratings$`Timing`)  

ggplot(items_ratings, aes(x=`Timing`, y=Valence_z, label=Valence_z)) + 
  geom_bar(stat='identity', aes(fill=Valence_type), width=.5)  +
  scale_fill_manual(name="Valence", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Valence from Item Data", 
       title= "Diverging Bars") + 
  coord_flip()


#*full data distribution comparison####
#KM test to compare dist on full data
#gender
xgen <- rating_data_long_omit$Valence[rating_data_long_omit$Sex=="M"]
ygen <- rating_data_long_omit$Valence[rating_data_long_omit$Sex=="F"]
xgen <- as.numeric(xgen)
ygen <- as.numeric(ygen)
KS.test(xgen, ygen)
#D = 0.08871, p-value = 1.125e-05
#alternative hypothesis: two.sided

#valence difference
xtime <- rating_data_long_omit$Valence[rating_data_long_omit$Timing=="2"]
ytime <- rating_data_long_omit$Valence[rating_data_long_omit$Timing=="7"]
xtime <- as.numeric(xtime)
ytime <- as.numeric(ytime)
KS.test(xtime, ytime)
#	Two-sample kernel Kolmogorov-Smirnov test
#D = 0.049346, p-value = 0.02805

#*item distribution comparison####
#KM test to compare dist on item data
xtime_item <- items_ratings$Valence[items_ratings$Timing=="2"]
ytime_item <- items_ratings$Valence[items_ratings$Timing=="7"]
xtime_item <- as.numeric(xtime_item)
ytime_item <- as.numeric(ytime_item)
KS.test(xtime_item, ytime_item) #not sig different


#PCA===================================================================================================
ReducedDataFrame <- subset(rating_data_long_omit, select=c(1, 8, 9, 10))

#FULL DATA
#scale ON SUBJECT LEVEL, NOT ON GROUP LEVEL
scaled_valence <- scale_within(
  ReducedDataFrame, variables="Valence",
  within="Internal_ID", scale = TRUE, center = TRUE)

scaled_arousal <- scale_within(
  ReducedDataFrame, variables="Arousal",
  within="Internal_ID", scale = TRUE, center = TRUE)

scaledarousal <- scale(ReducedDataFrame$Arousal, center = TRUE, scale = TRUE)
scaledvalence <- scale(ReducedDataFrame$Valence, center = TRUE, scale = TRUE)

#*PCA ONLY DATAFRAME of subject-scaled valence and arousal 
#PCADataFrame <- cbind(scaled_arousal$Arousal, scaled_valence$Valence)
PCADataFrame <- cbind(scaledarousal, scaledvalence)

PCADataFrame_omit <- na.omit(PCADataFrame)
sum(is.na(PCADataFrame_omit)) #omit subject 61 based on it returning NaN per above


#####PCA DF ITEMS*########
#ITEMS PCA====================================================================================================
#scaled_ITEMS_valence <- scale_within(
  #items_ratings, variables="Valence", scale = TRUE, center = TRUE)

#scaled_ITEMS_arousal <- scale_within(
  #items_ratings, variables="Arousal", scale = TRUE, center = TRUE)

#*ITEMS PCA ONLY DATAFRAME 
#PCADataFrame_ITEMS <- cbind(scaled_ITEMS_arousal$Arousal, scaled_ITEMS_arousal$Valence)
PCADataFrame_ITEMS <- subset(items_ratings, select=c(3, 4))


#*computation===========================================================================================================
#varmax
#*full data========================
#scaled on group level
fit <- principal(PCADataFrame_omit, nfactors=2, rotate="varimax")
fit 
#                       RC1  RC2
#SS loadings           1.30 0.70
#Proportion Var        0.65 0.35

fit$scores
fit$values

#loading
#RC1
loadings_fac1 = fit$loadings[,1]
eigenv_fac1 = sum(loadings_fac1^2); eigenv_fac1
#RC2
loadings_fac2 = fit$loadings[,0]
eigenv_fac2 = sum(loadings_fac1^2); eigenv_fac2

load = fit$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(fit_FinalDataFrame),cex=.7)

fulldata_variance_explained <- as.table(rbind(c(RC1 = 65, RC2 = 35)))
barplot(fulldata_variance_explained, main="Percent Variance Explained", col=c("darkblue"))



#*RCA on item data==========
fit_ITEMS <- principal(PCADataFrame_ITEMS, nfactors=2, rotate="varimax")
fit_ITEMS
#                       RC1  RC2
#SS loadings           1.78 0.22
#Proportion Var        0.89 0.11
items_variance_explained <- as.table(rbind(c(RC1 = 89, RC2 = 11)))
barplot(items_variance_explained, main="Percent Variance Explained", col=c("darkblue"))


#*RCA DF===================================================================================================================
#new data frame with all data
#*FULL RCA DF====
fit_FinalDataFrame <- cbind(fit$scores, rating_data_long_omit)

#refactor in new dataframe
fit_FinalDataFrame$Timing <- factor(fit_FinalDataFrame$Timing)
fit_FinalDataFrame$Phrasing <- factor(fit_FinalDataFrame$Phrasing)
fit_FinalDataFrame$Item <- factor(fit_FinalDataFrame$Item)
str(fit_FinalDataFrame)

#matrix plot
scatterplotMatrix(~ PC2 + Timing + Phrasing, data = fit_FinalDataFrame)
scatterplotMatrix(~ PC1 + Timing + Phrasing, data = fit_FinalDataFrame)

#*POSTPCA GRAPHS================================================================================
#density timing
ggplot(fit_FinalDataFrame, aes(x=PC1, fill=Timing)) + geom_density(alpha=.3)

#density
ggplot() + 
  geom_density(data=fit_FinalDataFrame, aes(x=RC2, group=Timing, fill=Timing),alpha=0.5) + 
  xlab("RC2") +
  ylab("Density")

#interaction
int2 <- ggplot(fit_FinalDataFrame, aes(RC1, RC2, colour = Timing))
int2 + stat_summary(fun.y = mean, geom = "line", size = 2, aes(group=Timing))


#scatter 
#PCA
scatter <- ggscatter(fit_FinalDataFrame, x = "RC1", y = "RC2",
                     color = "Timing",
                     add = "reg.line", conf.int = TRUE, xlim=c(-4,8), ylim =c(-4,8))
#for stats 
scatter + stat_cor(aes(color = Timing), label.x = 1)

#for hist
ggExtra::ggMarginal(scatter, type = "histogram", fill = "light blue")

#PCA spaghetti 
xyplot(RC1 ~ RC2 | Timing, data = fit_FinalDataFrame, groups = Timing)

#*ITEMS RCA data===================================================================
#using df with all rotated values scaled by group, create item level DF
RCA_ratings <- fit_FinalDataFrame %>%
  group_by(Item, Timing) %>%
  dplyr::summarize(RC1=mean(RC1), RC2 = mean(RC2))

RCA_item_scatter <- ggplot(RCA_ratings, aes(x = RC1, y = RC2)) +
  geom_point() + aes(color = Timing, add = "reg.line", conf.int = TRUE) +
  geom_smooth(method = "lm")
RCA_item_scatter
ggExtra::ggMarginal(RCA_item_scatter, type = "histogram", fill = "light blue")

RCAscatter <- ggscatter(RCA_ratings, x = "RC1", y = "RC2",
                     color = "Timing",
                     add = "reg.line", conf.int = TRUE)
RCAscatter + stat_cor(aes(color = Timing), label.x = 1)

#interaction
int3 <- ggplot(RCA_ratings, aes(RC1, RC2, colour = Timing))
int3 + stat_summary(fun.y = mean, geom = "line", size = 2, aes(group=Timing))

#density
ggplot() + 
  geom_density(data=RCA_ratings, aes(x=RC2, group=Timing, fill=Timing),alpha=0.5) + 
  xlab("RC2") +
  ylab("Density")
ggplot() + 
  geom_density(data=RCA_ratings, aes(x=RC1, group=Timing, fill=Timing),alpha=0.5) + 
  xlab("RC1") +
  ylab("Density")


#diverging bars rotated items
RCA_ratings$`RC2` <- rownames(RCA_ratings)  # create new column for car names
RCA_ratings$RC2_z <- (((RCA_ratings$RC2 - mean(RCA_ratings$RC2))/sd(RCA_ratings$RC2)))  # compute normalized mpg
RCA_ratings$RC2_type <- ifelse(RCA_ratings$RC2_z < 0, "below", "above")  # above / below avg flag
RCA_ratings <- RCA_ratings[order(RCA_ratings$RC2_z), ]  # sort
RCA_ratings$`Timing` <- factor(RCA_ratings$`Timing`, levels = RCA_ratings$`Timing`)  

ggplot(RCA_ratings, aes(x=`Timing`, y=RC2_z, label=RC2_z)) + 
  geom_bar(stat='identity', aes(fill=RC2_type), width=.5)  +
  scale_fill_manual(name="RC2", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Values from Rotated Item Data", 
       title= "Diverging Bars") + 
  coord_flip()


#*distribution tests on RCA####
#*WILCOX
wilcox.test(RC2~Timing,data=RCA_ratings)
#	Wilcoxon rank sum test
#data:  RC2 by Timing
#W = 98, p-value = 1.727e-08

#*KRUSKAL
kruskal.test(RC2~Timing,data=RCA_ratings)
#To “officially” test for a difference in scoring tendancies of people with different
#we use a Kruskal-Wallis Test.
#data:  RC2 by Timing
#Kruskal-Wallis chi-squared = 27.084, df = 1, p-value = 1.948e-07


#LMER MODELS=============================================================================================
#*no phrasing model===BEST MODEL=======================================================================================
#*RC
#FULL DATA
RC_PCAmodel <- lmer(RC2 ~ Timing + (1|Internal_ID) + (1|Item), data = fit_FinalDataFrame, REML=FALSE) 
summary(RC_PCAmodel) 
sjt.lmer(RC_PCAmodel)  
anova(RC_PCAmodel)
#F.value  Pr(>F)  
#Timing 1.8807  1.8807     1 97.993  3.8514 0.05254 .

#ITEMS LEVEL####PRESENTED 
ratings_RC_PCAmodel <- lmer(RC2 ~ Timing + (1|Item), data = RCA_ratings, REML=FALSE) 
summary(ratings_RC_PCAmodel) 
sjt.lmer(ratings_RC_PCAmodel)  
anova(ratings_RC_PCAmodel)
#Timing7      0.27912    0.02679 30.00000  10.418 1.75e-11 ***


#*NON PCA MODEL DATA
#Use mixed model with random intercepts for both subjects (Internal_ID) and images (Item)
arousal_results <- lmer(Arousal ~ Timing * Phrasing + (1|Internal_ID) + (1|Item), data=rating_data_long) #
summary(arousal_results)
#Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
#Formula: Arousal ~ Timing * Phrasing + (1 | Internal_ID) + (1 | Item)
#Data: rating_data_long
#REML criterion at convergence: 9793.7
#Scaled residuals: 
#Min      1Q  Median      3Q     Max 
#-4.5347 -0.5842  0.0689  0.6629  4.7076 
#Random effects:
#Groups      Name        Variance Std.Dev.
#Internal_ID (Intercept) 1.538    1.240   
#Item        (Intercept) 0.249    0.499   
#Residual                1.367    1.169   
#Number of obs: 2970, groups:  Internal_ID, 99; Item, 30
#Fixed effects:
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)         4.3208     0.2726 113.8300  15.853   <2e-16 ***
#Timing7             0.4205     0.3596  95.0000   1.169    0.245    
#Phrasing1           0.2821     0.3672  95.0000   0.768    0.444    
#Timing7:Phrasing1  -0.2407     0.5068  95.0000  -0.475    0.636    
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Correlation of Fixed Effects:
#(Intr) Timng7 Phrsn1
#Timing7     -0.673              
#Phrasing1   -0.659  0.500       
#Tmng7:Phrs1  0.478 -0.710 -0.725
sjt.lmer(arousal_results) 
anova(arousal_results)

#lmer arousal not rotated
#ITEMS
ratings_arousal_results <- lmer(Arousal ~ Timing + (1|Item), data=items_ratings) #
summary(ratings_arousal_results)
anova(ratings_arousal_results)

#FULL DATA
valence_results <- lmer(Valence ~ Timing + (1|Internal_ID) + (1|Item), data=rating_data_long) #
summary(valence_results)
#Linear mixed model fit by REML t-tests use Satterthwaite approximations to degrees of freedom [lmerMod]
#Formula: Valence ~ Timing * Phrasing + (1 | Internal_ID) + (1 | Item)
#Data: rating_data_long
#REML criterion at convergence: 8010.6
#Scaled residuals: 
#Min      1Q  Median      3Q     Max 
#-3.0625 -0.6317 -0.1330  0.5030  6.7780 
#Random effects:
#Groups      Name        Variance Std.Dev.
#Internal_ID (Intercept) 0.4919   0.7013  
#Item        (Intercept) 0.2046   0.4523  
#Residual                0.7595   0.8715  
#Number of obs: 2970, groups:  Internal_ID, 99; Item, 30
#Fixed effects:
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)         1.86944    0.16843 121.67000  11.099   <2e-16 ***
#Timing7             0.16656    0.20551  95.00000   0.810    0.420    
#Phrasing1           0.10012    0.20984  95.00000   0.477    0.634    
#Timing7:Phrasing1   0.04413    0.28961  95.00000   0.152    0.879    
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Correlation of Fixed Effects:
#(Intr) Timng7 Phrsn1
#Timing7     -0.623              
#Phrasing1   -0.610  0.500       
#Tmng7:Phrs1  0.442 -0.710 -0.725
sjt.lmer(valence_results) 
anova(valence_results)

#lmer arousal not rotated
ratings_valence_results <- lmer(Valence ~ Timing + (1|Item), data=items_ratings) #
summary(ratings_valence_results)



#CLUSTERING#########################
library(amap)
meta <- read.csv("IAPS_Metadata.csv")
forclus_2<-subset(meta, select=c("Item", "Description", "Valence_allsubjects_standard", "Arousal_allsubjects_standard", "Valence_allsubjects_study_2", "Arousal_allsubjects_study_2","Valence_allsubjects_diff_2", "Arousal_allsubjects_diff_2"))
forclus_7<-subset(meta, select=c("Item", "Description", "Valence_allsubjects_standard", "Arousal_allsubjects_standard", "Valence_allsubjects_study_7", "Arousal_allsubjects_study_7","Valence_allsubjects_diff_7", "Arousal_allsubjects_diff_7"))
hdist <- Dist(forclus_7, method="correlation", upper=T)

library(scatterplot3d)
library(mclust)
iclust(meta[,3:32])
omega(meta[,3:32]) 
library(corrplot)
corrplot(cor(meta[3:32]), order = "hclust", tl.col='black', tl.cex=.75) 

#kmeans
items_2_Cluster <- kmeans(forclus_2[5:6], 3, nstart = 20)
items_2_Cluster$cluster
items_2_Cluster$cluster <- as.factor(items_2_Cluster$cluster)
ggplot(forclus_2, aes(Valence_allsubjects_study_2, Arousal_allsubjects_study_2, color = items_2_Cluster$cluster)) + geom_point()

#2dcluster
library(cluster)
clusplot(forclus_2, items_2_Cluster$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#items label clsuter--2
item_2 <- kmeans(forclus_2[,c("Valence_allsubjects_study_2",	"Arousal_allsubjects_study_2")], centers=3, nstart=10)
o=order(item_2$cluster)
data.frame(forclus_2$Item[o],item_2$cluster[o])

plot(forclus_2$Valence_allsubjects_study_2, forclus_2$Arousal_allsubjects_study_2, type="n", xlim=c(0,5), xlab="Valence_2", ylab="Arousal_2")
text(x=forclus_2$Valence_allsubjects_study_2, forclus_2$Arousal_allsubjects_study_2, labels=forclus_2$Item,col=item_2$cluster+1)

#description label cluster--2
item_2 <- kmeans(forclus_2[,c("Valence_allsubjects_study_2",	"Arousal_allsubjects_study_2")], centers=3, nstart=10)
o=order(item_2$cluster)
data.frame(forclus_2$Item[o],item_2$cluster[o])

plot(forclus_2$Valence_allsubjects_study_2, forclus_2$Arousal_allsubjects_study_2, type="n", xlim=c(0,5), xlab="Valence_2", ylab="Arousal_2")
text(x=forclus_2$Valence_allsubjects_study_2, forclus_2$Arousal_allsubjects_study_2, labels=forclus_2$Description,col=item_2$cluster+1)


#items label clsuter--7
item_7 <- kmeans(forclus_7[,c("Valence_allsubjects_study_7",	"Arousal_allsubjects_study_7")], centers=3, nstart=10)
o=order(item_7$cluster)
data.frame(forclus_7$Item[o],item_7$cluster[o])

plot(forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, type="n", xlim=c(0,5), xlab="Valence_7", ylab="Arousal_7")
text(x=forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, labels=forclus_7$Item,col=item_7$cluster+1)

#description label cluster--7
item_7 <- kmeans(forclus_7[,c("Valence_allsubjects_study_7",	"Arousal_allsubjects_study_7")], centers=3, nstart=10)
o=order(item_7$cluster)
data.frame(forclus_7$Item[o],item_7$cluster[o])

plot(forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, type="n", xlim=c(0,5), xlab="Valence_7", ylab="Arousal_7")
text(x=forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, labels=forclus_7$Description,col=item_7$cluster+1)

#description label cluster--7--with labels as standard ratings
item_7 <- kmeans(forclus_7[,c("Valence_allsubjects_study_7",	"Arousal_allsubjects_study_7")], centers=3, nstart=10)
o=order(item_7$cluster)
data.frame(forclus_7$Item[o],item_7$cluster[o])

plot(forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, type="n", xlim=c(0,5), xlab="Valence_7", ylab="Arousal_7")
text(x=forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, labels=forclus_7$Valence_allsubjects_standard,col=item_7$cluster+1)

#description label cluster--7--with labels as DIFFERENCE scores
item_7 <- kmeans(forclus_7[,c("Valence_allsubjects_study_7",	"Arousal_allsubjects_study_7")], centers=3, nstart=10)
o=order(item_7$cluster)
data.frame(forclus_7$Item[o],item_7$cluster[o])

plot(forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, type="n", xlim=c(0,5), xlab="Valence_7", ylab="Arousal_7")
text(x=forclus_7$Valence_allsubjects_study_7, forclus_7$Arousal_allsubjects_study_7, labels=forclus_7$Valence_allsubjects_diff_7,col=item_7$cluster+1)
