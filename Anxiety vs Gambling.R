setwd("~/OneDrive - wesleyan.edu/OneDrive - wesleyan.edu/Sophomore/Applied Data Analysis")

load("/Volumes/qac/qac201/Studies and Codebooks/NESARC/Data/NESARC_pds.RData")

#List out variables I am interested in and take subset to include only these variables
#GENAXDX12 - GENERALIZED ANXIETY IN LAST 12 MONTHS (NON-HIERARCHICAL)
#GENAXDXP12 - GENERALIZED ANXIETY PRIOR TO LAST 12 MONTHS (NON-HIERARCHICAL)
#S12Q2A1 - EVER GAMBLE TO GET OUT OF A BAD MOOD
#S12Q2A2 - EVER GAMBLE TO FORGET YOUR PROBLEMS
#S12Q2A3 - MORE THAN ONCE TRY TO QUIT OR CUT DOWN ON GAMBLING, BUT COULDN'T DO IT
#S12Q2A8 - EVER HAVE JOB OR SCHOOL TROUBLE BECAUSE OF GAMBLING

# keep variables pertinent to my research question
var.to.keep<-c("GENAXDX12","GENAXDXP12","S12Q2A1","S12Q2A2", "S12Q2A3", "S12Q2A8", "AGE")
myData<-NESARC[,var.to.keep]
myData<-na.omit(myData)

#Load in necessary libraries
library(ggplot2)
library(descr)
install.packages("ColorBrewer")
library(RColorBrewer)

######################################
#Data Management and Frequency Tables
######################################
# EVER HAD 6+ MONTH PERIOD FELT VERY TENSE/NERVOUS/WORRIED 
#MOST OF TIME ABOUT EVERYDAY PROBLEMS? 
freq(myData$S9Q1B)
#for this data set, 1 represents "yes", 2 represents "no", and 
#9 represents "unknown". In this particular frequency table, 87.87%
#of participants reported "no" to this question, very few, 0.83% reported
#"yes", and 4.04% reported "unknown" and 7.25% reported NA. 

# EVER GAMBLE TO GET OUT OF A BAD MOOD
freq(myData$S12Q2A1)
#for this data set, 1 represents "yes", 2 represents "no", and 
#9 represents "unknown". In this particular frequency table, 24.53%
#of participants reported "no" to this question. 1.31% reported
#"yes", and very few, 0.03% reported "unknown". a majority of the
#participants, 74.11% of them in fact, reported that this question
#did not apply to them, so we can assume that they do not gamble,
#so they cannot answer this question. 

#MORE THAN ONCE TRY TO QUIT OR CUT DOWN ON GAMBLING, BUT COULDN'T DO IT
freq(myData$S12Q2A3)
#for this data set, 1 represents "yes", 2 represents "no", and 
#9 represents "unknown". In this particular frequency table, 25.08%
#of participants reported "no" to this question. Very few participants, 
#0.76% reported "yes", and even fewer, 0.04% reported "unknown". 
#a majority of the participants, 74.11% of them in fact, 
#reported that this question did not apply to them, so we can assume 
#that they do not gamble, so they cannot answer this question. 

######################################
#Project Component E: Data Management
######################################
#data managing DSM anxiety diagnosis
myData$anxdiag<-NA
myData$anxdiag[myData$GENAXDX12==1|myData$GENAXDXP12==1]<-1
myData$anxdiag[myData$GENAXDX12==0|myData$GENAXDXP12==0]<-0
freq(myData$anxdiag)

#data managing S12Q2A1 - EVER GAMBLE TO GET OUT OF A BAD MOOD
myData$badmood<-NA
myData$badmood[myData$S12Q2A1==1]<-1
myData$badmood[myData$S12Q2A1==2]<-0
myData$badmood[myData$S12Q2A1==9]<-NA
freq(myData$badmood)

#data managing S12Q2A2 - EVER GAMBLE TO FORGET YOUR PROBLEMS
myData$forgetprobs<-NA
myData$forgetprobs[myData$S12Q2A2==1]<-1
myData$forgetprobs[myData$S12Q2A2==2]<-0
myData$forgetprobs[myData$S12Q2A2==9]<-NA
freq(myData$forgetprobs)

#data managing S12Q2A3 - MORE THAN ONCE TRY TO QUIT OR CUT DOWN ON GAMBLING, 
#BUT COULDN'T DO IT
myData$trytoquit<-NA
myData$trytoquit[myData$S12Q2A3==1]<-1
myData$trytoquit[myData$S12Q2A3==2]<-0
myData$trytoquit[myData$S12Q2A3==9]<-NA
freq(myData$trytoquit)

#data managing S12Q2A8 - EVER HAVE JOB OR SCHOOL TROUBLE BECAUSE OF GAMBLING
myData$jobschooltrouble<-NA
myData$jobschooltrouble[myData$S12Q2A8==1]<-1
myData$jobschooltrouble[myData$S12Q2A8==2]<-0
myData$jobschooltrouble[myData$S12Q2A8==9]<-NA
freq(myData$jobschooltrouble)

#data managing AGE
myData$generation<-NA
myData$generation[myData$AGE<=25]<-1
myData$generation[myData$AGE<=41 & myData$AGE>=26]<-2
myData$generation[myData$AGE<=60 & myData$AGE>=42]<-3
myData$generation[myData$AGE<=81 & myData$AGE>=61]<-4
myData$generation[myData$AGE>=82]<-5

myData<-na.omit(myData)
######################################
#Project Component F: Univariate Graphs
######################################
#univariate plot of anxdiag
ggplot(data=myData)+
  geom_bar(aes(x=as.factor(anxdiag)))+
  ggtitle("Ever Been Diagnosed by DSM for Anxiety?")

#univariate plot of badmood
ggplot(data=myData)+
  geom_bar(aes(x=as.factor(badmood)))+
  ggtitle("Ever Gamble to Get out of a Bad Mood?")

#univariate plot of forgetprobs
ggplot(data=myData)+
  geom_bar(aes(x=as.factor(forgetprobs)))+
  ggtitle("Ever Gamble to Forget Problems?")

#univariate plot of trytoquit
ggplot(data=myData)+
  geom_bar(aes(x=as.factor(trytoquit)))+
  ggtitle("Ever Tried to Quit/Cut Down Unsuccessfully?")

#univariate plot of jobschooltrouble
ggplot(data=myData)+
  geom_bar(aes(x=as.factor(jobschooltrouble)))+
  ggtitle("Ever Have Job/School Trouble because of Gambling?")

######################################
#Project Component G: Bivariate Graphs
######################################
#explanatory variable: anxdiag, categorical
#response variable: badmood, categorical; forgetprobs, categorical; 
# trytoquit, categorical; jobschooltrouble, categorical

#badmood vs. anxdiag
ggplot(data=myData) + 
  stat_summary(aes(x=as.factor(anxdiag), y=badmood),  fun="mean", geom="bar") +
  ylab("Proportion of Subjects That Have Gambled to Get Out of a Bad Mood") + 
  xlab("Anxiety Diagnosis")+
  ggtitle("Gambling to get out of a Bad Mood vs. Anxiety")
#this graph reveals that there is a larger proportion of people with anxiety
#who gamble to get out of a bad mood, than people without anxiety. This 
#corresponds exactly with my predictions, that people with anxiety are
#more likely to display gambling behaviors. The graph does not reveal anything
#especially surprising to me. 

#forgetprobs vs. anxdiag
ggplot(data=myData) + 
  stat_summary(aes(x=as.factor(anxdiag), y=forgetprobs),  fun="mean", geom="bar") +
  ylab("Proportion of Subjects That Have Gambled to Forget Their Problems") + 
  xlab("Anxiety Diagnosis")+
  ggtitle("Gambling to Forget vs. Anxiety")
#this graph reveals that there is a larger proportion of people with anxiety
#who gamble to forget their problems than people without anxiety. This 
#again confirms my predictions, that people with anxiety are more likely
#more likely gamble to forget their problems, and more generally, that people
#with anxiety are more likely to exhibit gambling behaviors than those 
#without anxiety. The graph does not reveal anything especially surprising 
#to me.

#trytoquit vs. anxdiag
ggplot(data=myData) + 
  stat_summary(aes(x=as.factor(anxdiag), y=trytoquit),  fun="mean", geom="bar") +
  ylab("Proportion of Subjects That Have Tried to Quit Gambling Unsuccessfully") + 
  xlab("Anxiety Diagnosis")+
  ggtitle("Tried to Quit Gambling Unsuccessfully vs. Anxiety")
#this graph reveals that there is still a larger proportion of people 
#with anxiety who have tried to quit gambling compared tho those who do not
#have anxiety. However, the proportion of those without anxiety that have
#unsuccessfully tried to quit gambling is surprisingly close, relative to
#the other two graphs I generated, to the proportion of subjects with anxiety
#that have unsuccessfully quit gambling. This does still confirm my 
#predictions that people with anxiety are still more likely to try to 
#unsuccessfully quit gambling than people without anxiety, and more generally,
#that people with anxiety are more likely to exhibit gambling behaviors 
#than those without anxiety. 

#jobschooltrouble vs. anxdiag
ggplot(data=myData) + 
  stat_summary(aes(x=as.factor(anxdiag), y=jobschooltrouble),  fun="mean", geom="bar") +
  ylab("Proportion of Subjects That Have/Have Had Job/School Trouble because of Gambling") +
  xlab("Anxiety Diagnosis")+
  ggtitle("Job/School Trouble due to Gambling vs. Anxiety")
#similar to the tried to quit graph, this graph also reveals that there is 
#still a larger proportion of people with anxiety who have had job/school 
#trouble due to gambling compared tho those who do not have anxiety. 
#However, the proportion of those without anxiety that have had job/school
#trouble due to gambling is surprisingly close, relative to two other graphs 
#I generated, to the proportion of subjects with anxiety that have had job/
#school trouble due to gambling. This does still confirm my predictions that 
#people with anxiety are still more likely to have job/school trouble due to 
#gambling than people without anxiety, and more generally,that people with
#anxiety are more likely to exhibit gambling behaviors than those without 
#anxiety. 

######################################
#Project Component H: Chi-Square Hypothesis Tests
######################################
#badmood and anxdiag chi square
myChi1 <- chisq.test(myData$badmood, myData$anxdiag) 
myChi1
myChi1$observed # for actual, observed cell counts 
prop.table(myChi1$observed, 2) # for column percentages 
prop.table(myChi1$observed, 1) # for row percentages
#When examining the association between gambling due to a bad mood
#(categorical response) and anxiety diagnosis 
#(categorical explanatory), a chi-square test of independence 
#revealed that among those who gambled because they were in a bad mood
#were more likely to have anxiety (1.77%) when compared to those without
#anxiety(6.00%).
#X2 =46.869, 1 df, p=7.59e-12.


#forgetprobs and anxdiag chi square
myChi2 <- chisq.test(myData$forgetprobs, myData$anxdiag) 
myChi2
myChi2$observed # for actual, observed cell counts 
prop.table(myChi2$observed, 2) # for column percentages 
prop.table(myChi2$observed, 1) # for row percentages
#When examining the association between gambling to forget problems
#(categorical response) and anxiety diagnosis 
#(categorical explanatory), a chi-square test of independence 
#revealed that among those who gambled to forget their problems 
#were more likely to have anxiety (1.83%) when compared to those without 
#anxiety(6.63%).
#X2 =40.217, 1 df, p=2.273e-10.

#trytoquit and anxdiag chi square
myChi3 <- chisq.test(myData$trytoquit, myData$anxdiag) 
myChi3
myChi3$observed # for actual, observed cell counts 
prop.table(myChi3$observed, 2) # for column percentages 
prop.table(myChi3$observed, 1) # for row percentages
#When examining the association between those who tried to quit gambling
#(categorical response) and anxiety diagnosis 
#(categorical explanatory), a chi-square test of independence 
#revealed that among those who tried to quit gambling
#weren't more likely to have anxiety (1.98%) when compared to those without
#anxiety(2.45%).
#X2 =0.16168, 1 df, p=0.6876.

#jobschooltrouble and anxdiag chi square
myChi4 <- chisq.test(myData$jobschooltrouble, myData$anxdiag) 
myChi4
myChi4$observed # for actual, observed cell counts 
prop.table(myChi4$observed, 2) # for column percentages 
prop.table(myChi4$observed, 1) # for row percentages
#When examining the association between experiencing job/school troubles
#due to gambling(categorical response) and anxiety diagnosis 
#(categorical explanatory), a chi-square test of independence 
#revealed that among those who experienced job/school trouble due to gambling
#weren't more likely to have anxiety (1.97%) when compared to those without
#anxiety(10.26%).
#X2 =9.6978, 1 df, p=0.001845.

######################################
#Project Component I: Graphing with Addt'l Variable
######################################
#badmood anxdiag generation
ggplot(data=myData)+
  stat_summary(aes(x=as.factor(anxdiag), fill=as.factor(generation), y=badmood),
               fun="mean", geom="bar", position="dodge")+
  xlab("Diagnosed with Anxiety")+
  ylab("Proportion that Gambled to Get Out of a Bad Mood")+
  ggtitle("Proportion of Subjects Who Are Diagnosed with Anxiety at Each
  Generation who Gambled to Get Gut of a Bad Mood") + 
  scale_fill_brewer("Generation", labels=c("Millennials", "Gen X", "Baby Boomers"
                                           , "Silent Generation", 
                                           "Greatest Generation"), palette="Reds")
#across all generations, the proportion of people with anxiety who gambled to get 
#out of a bad mood was greater, though in the greatest generation, the difference
#between those with anxiety who gambled to get out of a bad mood and those who didn't
#gamble was the greatest. the baby boomers had the greatest proportion of those 
#with anxiety and those who did not gamble, and the silent generation had the smallest
#proportion of those with anxiety who did not gamble. the greatest generation had 
#the highest proportion of those with anxiety and gambled to get out of a bad mood.
#the silent generation had the lowest proportion of those with anxiety and gambled
#to get out of a bad mood. 

#forgetprobs anxdiag generation
ggplot(data=myData)+
  stat_summary(aes(x=as.factor(anxdiag), fill=as.factor(generation), y=forgetprobs),
               fun="mean", geom="bar", position="dodge")+
  xlab("Diagnosed with Anxiety")+
  ylab("Proportion that Gambled to Forget Problems")+
  ggtitle("Proportion of Subjects Who Are Diagnosed with Anxiety at Each
  Generation Who Gambled to Forget Problems") + 
  scale_fill_brewer("Generation", labels=c("Millennials", "Gen X", "Baby Boomers"
                                           , "Silent Generation", 
                                           "Greatest Generation"), palette="Purples")
#across all generations, the proportion of people with anxiety who gambled to forget 
#problems was greater that those who did not gamble to forget problems, though in 
#the millennial generation, the difference between those with anxiety who gambled 
#to forget problems and those who didn't gamble was the greatest. the baby boomers 
#had the greatest proportion of those with anxiety and those who did not gamble, 
#and the silent generation had the smallest proportion of those with anxiety who 
#did not gamble. the millennial generation had the highest proportion of those 
#with anxiety and gambled to get out of a bad mood.the silent generation had the 
#lowest proportion of those with anxiety and gambled to get out of a bad mood. 
#there also seems to be no data for those with anxiety in the greatest generation who
#gambled to forget their problems. 

#trytoquit anxdiag generation
ggplot(data=myData)+
  stat_summary(aes(x=as.factor(anxdiag), fill=as.factor(generation), y=trytoquit),
               fun="mean", geom="bar", position="dodge")+
  xlab("Diagnosed with Anxiety")+
  ylab("Proportion that Tried to Quit Gambling but Failed")+
  ggtitle("Proportion of Subjects Who Are Diagnosed with Anxiety at Each
  Generation Who Tried to Quit Gambling but Failed") + 
  scale_fill_brewer("Generation", labels=c("Millennials", "Gen X", "Baby Boomers"
                                           , "Silent Generation", 
                                           "Greatest Generation"), palette="Greens")
#across all generations, the proportion of people with anxiety who tried to quit 
#gambling but failed was greater that those who did not, though in the millennial 
#generation, the difference between those with anxiety who tried to quit gambling 
#and failed and those who didn't gamble was the greatest. the baby boomers 
#had the greatest proportion of those with anxiety and those who did not gamble, 
#and the silent generation had the smallest proportion of those with anxiety who 
#did not gamble. the millennial generation had the highest proportion of those 
#with anxiety and tried to quit gambling but failed. genx had the lowest proportion 
#of those with anxiety and tried to quit gambling but failed. there also seems to
#be no data for those with anxiety in the silent generation and those with anxiety
#in the greatest generation who tried to quit gambling but failed. 

#jobschooltrouble anxdiag generation
ggplot(data=myData)+
  stat_summary(aes(x=as.factor(anxdiag), fill=as.factor(generation), y=jobschooltrouble),
               fun="mean", geom="bar", position="dodge")+
  xlab("Diagnosed with Anxiety")+
  ylab("Proportion that Had Job or School Trouble due to Gambling")+
  ggtitle("Proportion of Subjects Who Are Diagnosed with Anxiety at Each
  Generation Who Had Job or School trouble due to Gambling") + 
  scale_fill_brewer("Generation", labels=c("Millennials", "Gen X", "Baby Boomers"
                                           , "Silent Generation", 
                                           "Greatest Generation"), palette="Blues")
#across all generations, the proportion of people with anxiety who had job/school 
#trouble due to gambling was greater that those who did not, though in the millennial 
#generation, the difference between those with anxiety who tried to quit gambling 
#and failed and those who didn't gamble was the greatest. the baby boomers 
#had the greatest proportion of those with anxiety and those who did not gamble, 
#and the silent generation had the smallest proportion of those with anxiety who 
#did not gamble. genx had the highest proportion of those with anxiety and had 
#job/school trouble due to gambling. the baby boomers had the lowest proportion 
#of those with anxiety and had job/school trouble due to gambling. there also 
#seems to be no data for those with anxiety in genx, those with anxiety in the 
#silent generation, and those with anxiety in the greatest generation who had
#job/school trouble due to gambling. 

######################################
#Project Component J: Logistic Regressions
######################################
#anxdiag(x) and badmood(y)
my.logreg1 <- glm(badmood ~ factor(anxdiag), data = myData, family = "binomial") 
summary(my.logreg1)  # for p-values 
exp(my.logreg1$coefficients)  # for odds ratios 
exp(confint(my.logreg1))  # for confidence intervals on the odds ratios
#graph
#myData$anxdiag<-as.factor(myData$anxdiag)
graphdata<-expand.grid(anxdiag=c(0,1))
graphdata<-cbind(graphdata, predict(my.logreg1, newdata=graphdata, type="link", se=TRUE))
graphdata<-cbind(graphdata, PredictedProb=plogis(graphdata$fit),
                 LL=plogis(graphdata$fit-1.96*graphdata$se.fit),
                 UL=plogis(graphdata$fit+1.96*graphdata$se.fit))
ggplot(data=graphdata)+
  geom_line(aes(x=anxdiag, y=PredictedProb), color="red", size=2)+
  geom_errorbar(aes(x=anxdiag, y=PredictedProb, ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=anxdiag, y=PredictedProb), color="black", size=3)

#anxdiag and forgetprobs
my.logreg2 <- glm(forgetprobs ~ factor(anxdiag), data = myData, family = "binomial") 
summary(my.logreg2)  # for p-values 
exp(my.logreg2$coefficients)  # for odds ratios 
exp(confint(my.logreg2))  # for confidence intervals on the odds ratios
#graph
myData$anxdiag<-as.factor(myData$anxdiag)
graphdata1<-expand.grid(anxdiag=c(0,1))
graphdata1<-cbind(graphdata1, predict(my.logreg2, newdata=graphdata1, type="link", se=TRUE))
graphdata1<-cbind(graphdata1, PredictedProb=plogis(graphdata1$fit),
                 LL=plogis(graphdata1$fit-1.96*graphdata1$se.fit),
                 UL=plogis(graphdata1$fit+1.96*graphdata1$se.fit))
ggplot(data=graphdata1)+
  geom_line(aes(x=anxdiag, y=PredictedProb), color="red", size=2)+
  geom_errorbar(aes(x=anxdiag, y=PredictedProb, ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=anxdiag, y=PredictedProb), color="black", size=3)

#anxdiag and trytoquit
my.logreg3 <- glm(trytoquit ~ factor(anxdiag), data = myData, family = "binomial") 
summary(my.logreg3)  # for p-values 
exp(my.logreg3$coefficients)  # for odds ratios 
exp(confint(my.logreg3))  # for confidence intervals on the odds ratios
#graph
myData$anxdiag<-as.factor(myData$anxdiag)
graphdata2<-expand.grid(anxdiag=c(0,1))
graphdata2<-cbind(graphdata2, predict(my.logreg3, newdata=graphdata2, type="link", se=TRUE))
graphdata2<-cbind(graphdata2, PredictedProb=plogis(graphdata2$fit),
                 LL=plogis(graphdata2$fit-1.96*graphdata2$se.fit),
                 UL=plogis(graphdata2$fit+1.96*graphdata2$se.fit))
ggplot(data=graphdata2)+
  geom_line(aes(x=anxdiag, y=PredictedProb), color="red", size=2)+
  geom_errorbar(aes(x=anxdiag, y=PredictedProb, ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=anxdiag, y=PredictedProb), color="black", size=3)

#anxdiag and jobschooltrouble
my.logreg4 <- glm(jobschooltrouble ~ factor(anxdiag), data = myData, family = "binomial") 
summary(my.logreg4)  # for p-values 
exp(my.logreg4$coefficients)  # for odds ratios 
exp(confint(my.logreg4))  # for confidence intervals on the odds ratios
#graph
myData$anxdiag<-as.factor(myData$anxdiag)
graphdata3<-expand.grid(anxdiag=c(0,1))
graphdata3<-cbind(graphdata3, predict(my.logreg4, newdata=graphdata3, type="link", se=TRUE))
graphdata3<-cbind(graphdata3, PredictedProb=plogis(graphdata3$fit),
                 LL=plogis(graphdata3$fit-1.96*graphdata3$se.fit),
                 UL=plogis(graphdata3$fit+1.96*graphdata3$se.fit))
ggplot(data=graphdata3)+
  geom_line(aes(x=anxdiag, y=PredictedProb), color="red", size=2)+
  geom_errorbar(aes(x=anxdiag, y=PredictedProb, ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=anxdiag, y=PredictedProb), color="black", size=3)

######################################
#Project Component L: Logistic Graphs with Addt'l Variable
######################################
#badmood anxdiag generation
mod1<-glm(badmood~anxdiag+generation, family="binomial", data=myData)
summary(mod1)
exp(mod1$coefficients)  # for odds ratios 
exp(confint(mod1)) #CI
graphdata4<-expand.grid(anxdiag=c('0','1'),
                        generation=c(0, 1, 2, 3, 4 ))
graphdata4<-cbind(graphdata4, predict(mod1, newdata=graphdata4, type="link", se=TRUE))
graphdata4<-cbind(graphdata4, badmood=plogis(graphdata4$fit),
                  LL=plogis(graphdata4$fit-1.96*graphdata4$se.fit),
                  UL=plogis(graphdata4$fit+1.96*graphdata4$se.fit))
ggplot(data=graphdata4)+
  geom_line(aes(x=factor(generation), y=badmood, color=as.factor(anxdiag), group=anxdiag), size=2)+
  geom_errorbar(aes(x=factor(generation), y=badmood, color=as.factor(anxdiag), ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=factor(generation), y=badmood, color=as.factor(anxdiag)), color="black", size=3)+
  xlab("Generation")+
  ylab("Proportion that to Get Gut of a Bad Mood")+
  ggtitle("Proportion of Subjects in Each Generation Who Are Diagnosed with Anxiety Who Gambled to Get Out of a Bad Mood") + 
  scale_color_manual("Anxiety Diagnosis", labels=c("No", "Yes"), values=c("blue","red"))+
  scale_x_discrete(labels=c("Millennials", "Gen X", "Baby Boomers"
                            , "Silent Generation", 
                            "Greatest Generation"), guide=guide_axis(angle=45))

#forgetprobs anxdiag generation
mod2<-glm(forgetprobs~anxdiag+generation, family="binomial", data=myData)
summary(mod2)
exp(mod2$coefficients)  # for odds ratios 
exp(confint(mod2)) #CI
graphdata5<-expand.grid(anxdiag=c('0','1'),
                        generation=c(0, 1, 2, 3, 4 ))
graphdata5<-cbind(graphdata5, predict(mod1, newdata=graphdata5, type="link", se=TRUE))
graphdata5<-cbind(graphdata5, forgetprobs=plogis(graphdata5$fit),
                  LL=plogis(graphdata5$fit-1.96*graphdata5$se.fit),
                  UL=plogis(graphdata5$fit+1.96*graphdata5$se.fit))
ggplot(data=graphdata5)+
  geom_line(aes(x=factor(generation), y=forgetprobs, color=as.factor(anxdiag), group=anxdiag), size=2)+
  geom_errorbar(aes(x=factor(generation), y=forgetprobs, color=as.factor(anxdiag), ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=factor(generation), y=forgetprobs, color=as.factor(anxdiag)), color="black", size=3)+
  xlab("Generation")+
  ylab("Proportion that Gambled to Forget Problems")+
  ggtitle("Proportion of Subjects in Each Generation Who Are Diagnosed with Anxiety Who Gambled to Forget Problems") + 
  scale_color_manual("Anxiety Diagnosis", labels=c("No", "Yes"), values=c("blue","red"))+
  scale_x_discrete(labels=c("Millennials", "Gen X", "Baby Boomers"
                            , "Silent Generation", 
                            "Greatest Generation"))

#trytoquit anxdiag generation
mod3<-glm(trytoquit~anxdiag+generation, family="binomial", data=myData)
summary(mod3)
exp(mod3$coefficients)  # for odds ratios 
exp(confint(mod3)) #CI
graphdata6<-expand.grid(anxdiag=c('0','1'),
                        generation=c(0, 1, 2, 3, 4 ))
graphdata6<-cbind(graphdata6, predict(mod1, newdata=graphdata6, type="link", se=TRUE))
graphdata6<-cbind(graphdata6, trytoquit=plogis(graphdata6$fit),
                  LL=plogis(graphdata6$fit-1.96*graphdata6$se.fit),
                  UL=plogis(graphdata6$fit+1.96*graphdata6$se.fit))
ggplot(data=graphdata6)+
  geom_line(aes(x=factor(generation), y=trytoquit, color=as.factor(anxdiag), group=anxdiag), size=2)+
  geom_errorbar(aes(x=factor(generation), y=trytoquit, color=as.factor(anxdiag), ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=factor(generation), y=trytoquit, color=as.factor(anxdiag)), color="black", size=3)+
  xlab("Generation")+
  ylab("Proportion that Tried to Quit Gambling but Failed")+
  ggtitle("Proportion of Subjects in Each Generation Who Are Diagnosed with Anxiety Who Tried to Quit Gambling But Failed") + 
  scale_color_manual("Anxiety Diagnosis", labels=c("No", "Yes"), values=c("blue","red"))+
  scale_x_discrete(labels=c("Millennials", "Gen X", "Baby Boomers"
                            , "Silent Generation", 
                            "Greatest Generation"))

#jobschooltrouble anxdiag generation
mod4<-glm(jobschooltrouble~anxdiag+generation, family="binomial", data=myData)
summary(mod4)
exp(mod4$coefficients)  # for odds ratios 
exp(confint(mod4)) #CI
graphdata7<-expand.grid(anxdiag=c('0','1'),
                        generation=c(0, 1, 2, 3, 4 ))
graphdata7<-cbind(graphdata7, predict(mod1, newdata=graphdata7, type="link", se=TRUE))
graphdata7<-cbind(graphdata7, jobschooltrouble=plogis(graphdata7$fit),
                  LL=plogis(graphdata7$fit-1.96*graphdata7$se.fit),
                  UL=plogis(graphdata7$fit+1.96*graphdata7$se.fit))
ggplot(data=graphdata7)+
  geom_line(aes(x=factor(generation),  y=jobschooltrouble, color=as.factor(anxdiag), group=anxdiag), size=2)+
  geom_errorbar(aes(x=factor(generation), y=jobschooltrouble, color=as.factor(anxdiag), ymin=LL, ymax=UL),width=0.1, size=2)+
  geom_point(aes(x=factor(generation), y=jobschooltrouble, color=as.factor(anxdiag)), color="black", size=3)+
  xlab("Generation")+
  ylab("Proportion that Had Job or School Trouble due to Gambling")+
  ggtitle("Proportion of Subjects in Each Generation Who Are Diagnosed with Anxiety Who Had Job or School trouble due to Gambling") + 
  scale_color_manual("Anxiety Diagnosis", labels=c("No", "Yes"), values=c("blue","red"))+
  scale_x_discrete(labels=c("Millennials", "Gen X", "Baby Boomers"
                            , "Silent Generation", 
                            "Greatest Generation"))






