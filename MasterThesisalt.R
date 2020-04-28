###Gabriel Agbesi Atsyor####
##Masters Thesis R script###

#setting the working directory
getwd()
setwd("C:/Users/GHOOST/Desktop/New Lit/data")

#attaching packages
library(lavaan)
library(foreign)
library(car)
library(psych)
library(ggplot2)
library(summarytools)
library(knitr)
library(gridExtra)
library(kableExtra)
library(stargazer)
library(multcomp)
library(corregp)
library(MASS)
#attaching the data
data<-read.csv("International Students Survey.csv")

attach(data)

##FACTORS INFLUENCING STUDENTS DECISION TO STUDY IN RUSSIA

#Data preparation: Demographic information

#Age
table(Age)

summary(as.numeric(Age))

data$age<-recode(as.numeric(Age),"17:21=1; 22:26=2;27:hi=3")

table(data$age)

data$age<-factor(data$age,lab=c("17 to 21 yrs", "22 to 26 yrs", " 27 yrs and older")) 


table(as.numeric(What.degree.are.you.currently.studying.for.))
table(What.degree.are.you.currently.studying.for.)

study<-as.numeric(What.degree.are.you.currently.studying.for.)
table(study)
data$degree<-recode(study, "1=0; 2=1")
data$degree<-factor(data$degree,lab=c("Bachelor", "Masters")) 
#Degree


#Language of instruction
data$language.of.instruction<-What.is.the.language.of.instruction.for.your.program.

data$family.income<-What.was.your.annual.family.income.when.you.were.applying.to.study.abroad..estimate.in.US.dollars.
#country regions
table(Home.country)

data$world.region[data$Home.country == 'Algeria'| 
              data$Home.country == 'Botswana'| data$Home.country == 'Cameroon'|
              data$Home.country == 'Chad'| data$Home.country == 'Congo'|
              data$Home.country == 'DR Congo'|data$Home.country == 'Eritrea'|
              data$Home.country == 'Ivory Coast'|
              data$Home.country == 'Gambia'|data$Home.country == 'Ghana'|
              data$Home.country == 'Kenya'|data$Home.country == 'Madagascar'|
              data$Home.country == 'Niger'|data$Home.country == 'Nigeria'|
              data$Home.country == 'South Africa'|data$Home.country == 'Sudan'|
              data$Home.country == 'Uganda'|data$Home.country == 'Zambia'] <- 'Africa'

data$world.region[data$Home.country == 'Bangladesh'| 
              data$Home.country == 'India'| data$Home.country == 'Nepal'|
              data$Home.country == 'Pakistan'| 
              data$Home.country == 'Sri Lanka'|data$Home.country == 'Indonesia'|
              data$Home.country == 'Philippines'|data$Home.country == 'Thailand'|
              data$Home.country == 'Vietnam'|data$Home.country == 'China'|
              data$Home.country == 'Japan'|data$Home.country == 'Mongolia'| 
              data$Home.country == 'South Korea'|data$Home.country == 'Hong Kong'|
              data$Home.country == 'Taiwan'] <- 'Asia'

data$world.region[data$Home.country == 'Australia'| data$Home.country == 'Austria'| 
              data$Home.country == 'Bosnia and Herzegovina'|
              data$Home.country == 'Bulgaria'| data$Home.country == 'Europe'|
              data$Home.country == 'France'| data$Home.country == 'Germany'|
              data$Home.country == 'Italy'|data$Home.country == 'Poland'|
              data$Home.country == 'Portugal'|data$Home.country == 'Serbia'|
              data$Home.country == 'Spain'|data$Home.country == 'Switzerland'|
              data$Home.country == 'Republic of North Macedonia'|
              data$Home.country == 'USA'] <- 'Europe'


data$world.region[data$Home.country == 'Armenia'| 
              data$Home.country == 'Azerbaijan'|data$Home.country == 'Belarus'|
              data$Home.country == 'Estonia'|data$Home.country == 'Georgia'|
              data$Home.country == 'Georgia'|data$Home.country == 'Kazakhstan'|
              data$Home.country == 'Kyrgyzstan'|data$Home.country == 'Latvia'|
              data$Home.country == 'Moldova'|data$Home.country == 'Tajikistan'|
              data$Home.country == 'Turkmenistan'|data$Home.country == 'Ukraine'|
              data$Home.country == 'Uzbekistan'] <- 'CIS'


data$world.region[data$Home.country == 'Bahrain'| 
              data$Home.country == 'Egypt'| data$Home.country == 'Iran'|
              data$Home.country == 'Israel'| data$Home.country == 'Lebanon'|
              data$Home.country == 'Syria'| 
              data$Home.country == 'Turkey'] <- 'Middle East'


data$world.region[data$Home.country == 'Brazil'| 
              data$Home.country == 'Colombia'|data$Home.country == 'Ecuador'|
              data$Home.country == 'Guatemala'| data$Home.country == 'Haiti'|
              data$Home.country == 'Mexico'|data$Home.country == 'Venezuela'|
              data$Home.country == 'Nicaragua'] <- 'Southern America'


table(data$world.region, useNA = "ifany")

attach(data)


##Regression Analysis:Factors that influenced the decision of international students to study in Russia

#Data Preparation: Push Factors

is.numeric(Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution)
Competitive.University.admission.process<-Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution
is.numeric(Competitive.University.admission.process)

is.numeric(Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market)
Perceived.advantage.of.international.degree<-Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
is.numeric(Perceived.advantage.of.international.degree)


#creating a data frame for push factors
PushFactors <-data.frame(world.region, Unavailability.of.the.desired.study.program,Low.quality.of.education
                         ,Competitive.University.admission.process
                         ,Perceived.advantage.of.international.degree
                         ,Unavailability.of.scholarship.opportunities
                         ,Encouragement.from.my.family.to.study.abroad,Encouragement.from..my.friends.to.study.abroad
                         ,Better.earning.prospects.abroad, The.social.prestige.of.studying.abroad
                         ,To.experience.a.different.culture)

#exploratory factor analysis to allow for indexing.

#principal component analysis
pushpc <- princomp(~Unavailability.of.the.desired.study.program+Low.quality.of.education
                   +Competitive.University.admission.process
                   +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                   +Unavailability.of.scholarship.opportunities
                   +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                   +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                   +To.experience.a.different.culture, data = PushFactors, cor = FALSE, na.action = na.omit) 

summary(pushpc)

biplot(pushpc)

#factor analysis
push.efa <- factanal(~Unavailability.of.the.desired.study.program+Low.quality.of.education
                     +Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution
                     +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                     +Unavailability.of.scholarship.opportunities
                     +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                     +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                     +To.experience.a.different.culture, 
                     factors = 6, data = PushFactors , cor = FALSE, na.action = na.omit)

print(push.efa, digits=2, cutoff=.3, sort=TRUE)


push.efa1 <- factanal(~Unavailability.of.the.desired.study.program+Low.quality.of.education
                      +Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution
                      +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                      +Unavailability.of.scholarship.opportunities
                      +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                      +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                      +To.experience.a.different.culture, 
                      factors = 5, data = PushFactors, cor = FALSE, na.action = na.omit)

print(push.efa1, digits=2, cutoff=.3, sort=TRUE)


push.efa2 <- factanal(~Unavailability.of.the.desired.study.program+Low.quality.of.education
                      +Competitive.University.admission.process
                      +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                      +Unavailability.of.scholarship.opportunities
                      +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                      +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                      +To.experience.a.different.culture, 
                      factors = 4, data = PushFactors, cor = FALSE, na.action = na.omit)

print(push.efa2, digits=2, cutoff=.3, sort=TRUE)


attach(PushFactors)
#with p-value 0.0415, four factors are sufficient.

#indexing the correlated factors

#encouragement from family and friends
cor.test(Encouragement.from..my.friends.to.study.abroad,Encouragement.from.my.family.to.study.abroad)
encouragement.from.family.friends<-(Encouragement.from..my.friends.to.study.abroad+Encouragement.from.my.family.to.study.abroad)/2
table(encouragement.from.family.friends)
PushFactors$encouragement.from.family.friends<-recode(encouragement.from.family.friends, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PushFactors$encouragement.from.family.friends)

#advantages of studying abroad
cor.test(Better.earning.prospects.abroad,The.social.prestige.of.studying.abroad)
cor.test(The.social.prestige.of.studying.abroad, Perceived.advantage.of.international.degree)
cor.test(Better.earning.prospects.abroad, Perceived.advantage.of.international.degree)
advantages.of.studying.abroad<-(Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad)/2
table(advantages.of.studying.abroad)
PushFactors$advantages.of.studying.abroad<-recode(advantages.of.studying.abroad, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PushFactors$advantages.of.studying.abroad)

#access to education
attach(PushFactors)
cor.test(Competitive.University.admission.process,Unavailability.of.scholarship.opportunities)
access.to.education<-(Unavailability.of.the.desired.study.program+Low.quality.of.education)/2
table(access.to.education)
PushFactors$access.to.education<-recode(access.to.education, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PushFactors$access.to.education)

attach(PushFactors)

#checking for cronbach's aplha to establish reliability

PushFactorsHC <-data.frame(world.region, access.to.education, Competitive.University.admission.process
                           ,Perceived.advantage.of.international.degree
                           ,Unavailability.of.scholarship.opportunities
                           ,encouragement.from.family.friends
                           ,advantages.of.studying.abroad
                           ,To.experience.a.different.culture)
attach(PushFactorsHC)
PF<-na.omit(PushFactorsHC)
attach(PF)
dim(PF)
pca<-prcomp(PF[,c(2:8)], center = TRUE, scale. = TRUE)

summary(pca)

attach(PF)

library(ggbiplot)

ggbiplot(pca)

ggbiplot(pca, labels=rownames(PF))

ggbiplot(pca, ellipse=TRUE,choices=c(1,3), groups=world.region)




psych::alpha(PushFactorsHC)

install.packages("varrank")
library(varrank)


v <- varrank(data.df = PushFactorsHC, method = "estevez", variable.important = "access.to.education", 
             discretization.method = "sturges", algorithm = "forward", scheme="mid", verbose = FALSE)

summary(v)

library(dplyr)

res <- PushFactors %>%
  dplyr::group_by(access.to.education) %>%
  dplyr::arrange(dplyr::desc(world.region)) %>%
  rank_in_group(.)

manova(access.to.education+To.experience.a.different.culture~world.region)

res.man <- manova(cbind(access.to.education, Competitive.University.admission.process
                        ,Perceived.advantage.of.international.degree
                        ,Unavailability.of.scholarship.opportunities
                        ,encouragement.from.family.friends
                        ,advantages.of.studying.abroad
                        ,To.experience.a.different.culture) ~ world.region, data = PushFactorsHC)

summary(res.man)
summary.aov(res.man)

library(MASS)

fit <- lda(world.region ~ access.to.education + Competitive.University.admission.process, 
           data=PushFactorsHC, na.action="na.omit", CV=TRUE)

fit

plot(fit)


#Regression analysis
#Push factors in Home country that influenced the decision of international students to study in Russia


?corregp
push.crg <- corregp(world.region ~ access.to.education, data = PushFactorsHC, b = 40)

summary(push.crg)

plot(push.crg, x_ell=TRUE)

push.crg<- corregp(world.region ~ access.to.education*Competitive.University.admission.process, 
                   data = PushFactors)

summary(push.crg)

?plot.corregp

p<- corregp(formula = as.factor(access.to.education)~world.region, part = 6, data = data)
is.factor(access.to.education)

#Empty model
model0<-glm(degree~1, data = PushFactors, family = binomial(link=logit))

summary(model0)

model1<-glm(degree~1+encouragement.from.family.friends+advantages.of.studying.abroad+access.to.education+
              Competitive.University.admission.process+Perceived.advantage.of.international.degree+
              Unavailability.of.scholarship.opportunities+To.experience.a.different.culture, 
            data = PushFactors, family = binomial(link=logit))

summary(model1)

model2<-glm(degree~1+encouragement.from.family.friends+advantages.of.studying.abroad+access.to.education+
              Competitive.University.admission.process+Perceived.advantage.of.international.degree+
              Unavailability.of.scholarship.opportunities+To.experience.a.different.culture+as.numeric(age)+
              as.numeric(Home.country)+as.numeric(Gender)+language.of.instruction, 
            data = PushFactors, family = binomial(link=logit))

summary(model2)


summary(model0)
model1<glm(degree~encouragement.from.family.friends+advantages.of.studying.abroad+access.to.education+
             Competitive.University.admission.process+Perceived.advantage.of.international.degree+
             Unavailability.of.scholarship.opportunities+To.experience.a.different.culture, data = PushFactors,
           family = binomial(link=logit))

summary(model1)

m<-glm(degree~advantages.of.studying.abroad+
         Unavailability.of.scholarship.opportunities+Competitive.University.admission.process+
         (access.to.education+encouragement.from.family.friends+Perceived.advantage.of.international.degree+
            To.experience.a.different.culture)*world.region+as.numeric(age)+as.numeric(Home.country)+
         as.numeric(Gender)+language.of.instruction, data = PushFactors,family = binomial(link=logit))

summary(m)

#Full Model
model1<-lm(as.numeric(language.of.instruction)~encouragement.from.family.friends+
             advantages.of.studying.abroad+access.to.education+
             Competitive.University.admission.process+Perceived.advantage.of.international.degree+
             Unavailability.of.scholarship.opportunities+To.experience.a.different.culture, data = PushFactors)

summary(model1)

#Full Model and controls
model2<-lm(as.numeric(language.of.instruction)~encouragement.from.family.friends+
             advantages.of.studying.abroad+access.to.education+
             Competitive.University.admission.process+Perceived.advantage.of.international.degree+
             Unavailability.of.scholarship.opportunities+To.experience.a.different.culture+as.numeric(age)+
             as.numeric(Home.country)+as.numeric(Gender), data = PushFactors)

summary(model2)

#Full Model with interaction effect
model3<-lm(as.numeric(language.of.instruction)~advantages.of.studying.abroad+
             Unavailability.of.scholarship.opportunities+Competitive.University.admission.process+
             (access.to.education+encouragement.from.family.friends+Perceived.advantage.of.international.degree+
                To.experience.a.different.culture)*world.region, data = PushFactors)

summary(model3)

#Full Model and control with interaction effects
model4<-lm(as.numeric(language.of.instruction)~advantages.of.studying.abroad+
             Unavailability.of.scholarship.opportunities+Competitive.University.admission.process+
             (access.to.education+encouragement.from.family.friends+Perceived.advantage.of.international.degree+
                To.experience.a.different.culture)*world.region+as.numeric(age)+as.numeric(Home.country)+
             as.numeric(Gender), data = PushFactors)

summary(model4)

#Graph for the models
stargazer(model1, model2, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out="model12.doc",
          covariate.labels =c("Constant","Encouragement from family and friends","Benefits of studying abroad",
                              "Access to education", "Competitive university admission process",
                              "Perceived advantage of an international degree",
                              "Unavailability of scholarship opportunities", "Experience a different culture",
                              "Age", "Home Country", "Gender"))


stargazer(model3, model4, title="Regression Results: Push Factors in Home Country", align=TRUE,column.sep.width = "-5pt",
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out="model34.doc",
          covariate.labels =c("Constant","Benefits of studying abroad","Unavailability of scholarship opportunities",
                             "Competitive university admission process","Access to education",
                             "Encouragement from family and friends","Perceived advantage of an international degree",
                             "Experience a different culture","Asia", "CIS", "Europe", "Middle East", "South America",
                             "Age", "Home Country", "Gender",
                             "Access to education*Asia", "Access to education*CIS", "Access to education*Europe",
                             "Access to education*Middle East", "Access to education*South America","Encouragement from family and friends*Asia",
                             "Encouragement from family and friends*CIS", "Encouragement from family and friends*Europe",
                             "Encouragement from family and friends*Middle East","Encouragement from family and friends*South America",
                             "Perceived advantage of an international degree*Asia","Perceived advantage of an international degree*CIS", 
                             "Perceived advantage of an international degree*Europe","Perceived advantage of an international degree*Middle East",
                             "Perceived advantage of an international degree*South America","Experience a different culture*Asia",
                             "Experience a different culture*CIS", "Experience a different culture*Europe",
                             "Encouragement from family and friends*Middle East","Experience a different culture*South America"))

#checking multicolinearity
vif(model1)

vif(model2)

vif(model3)

vif(model4)


####Pull factors that influenced the decision of international students to study in Russia

Recommendations.from.family.friends<-Personal.recommendations.from.parents..relatives..and.friends
#creating a data frame for push factors

PullFactors<-data.frame(language.of.instruction, age, Gender, world.region, Home.country,
                        Availability.of.desired.study.program,Higher.quality.of.education..compared.to.home.country.,
                        Low.cost.of.living,Low.tuition.fees,Awarded.scholarships.or.tuition.waiver,Attraction.to.Russian.culture..society,
                        Career.prospects.in.Russia,Recommendations.from.family.friends,cultural.proximity.with.home,
                        geographical.proximity.with.home,Quality.and.reputation.of.the.University,Recognition.of.the.degree.in.my.home.country,
                        Quality.of.the.teaching.staff,The.reputation.of.the.alumni,The.reputation.of.the.international.community,
                        HSE.position.in.international.university.rankings,Cost.of.tuition.for.international.students,Availability.of.scholarships,
                        Support.services.for.international.students,Graduates.employment.rates,HSE.s.international.strategic.alliances,
                        Local.employers.preference.of..degrees.awarded.by.HSE)

#exploratory factor analysis to allow for index.
#principal component analysis
fit.pc <- princomp(~Availability.of.desired.study.program+Higher.quality.of.education..compared.to.home.country.
                   +Low.cost.of.living+Low.tuition.fees+Awarded.scholarships.or.tuition.waiver+Attraction.to.Russian.culture..society
                   +Career.prospects.in.Russia+Personal.recommendations.from.parents..relatives..and.friends+cultural.proximity.with.home
                   +geographical.proximity.with.home+Quality.and.reputation.of.the.University+Recognition.of.the.degree.in.my.home.country
                   +Quality.of.the.teaching.staff+The.reputation.of.the.alumni+The.reputation.of.the.international.community
                   +HSE.position.in.international.university.rankings+Cost.of.tuition.for.international.students
                   +Availability.of.scholarships+Support.services.for.international.students+Graduates.employment.rates
                   +HSE.s.international.strategic.alliances+Local.employers.preference.of..degrees.awarded.by.HSE, 
                   data = PullFactors, cor = FALSE, na.action = na.omit)

summary(fit.pc)

fit.efa <- factanal(~Availability.of.desired.study.program+Higher.quality.of.education..compared.to.home.country.
                    +Low.cost.of.living+Low.tuition.fees+Awarded.scholarships.or.tuition.waiver+Attraction.to.Russian.culture..society
                    +Career.prospects.in.Russia+Personal.recommendations.from.parents..relatives..and.friends+cultural.proximity.with.home
                    +geographical.proximity.with.home+Quality.and.reputation.of.the.University+Recognition.of.the.degree.in.my.home.country
                    +Quality.of.the.teaching.staff+The.reputation.of.the.alumni+The.reputation.of.the.international.community
                    +HSE.position.in.international.university.rankings+Cost.of.tuition.for.international.students
                    +Availability.of.scholarships+Support.services.for.international.students+Graduates.employment.rates
                    +HSE.s.international.strategic.alliances+Local.employers.preference.of..degrees.awarded.by.HSE,
                    factors = 11, data = PullFactors, cor = FALSE, na.action = na.omit)

print(fit.efa, digits=2, cutoff=.3, sort=TRUE)


fit.efa2 <- factanal(~Availability.of.desired.study.program+Higher.quality.of.education..compared.to.home.country.
                     +Low.cost.of.living+Low.tuition.fees+Awarded.scholarships.or.tuition.waiver+Attraction.to.Russian.culture..society
                     +Career.prospects.in.Russia+Personal.recommendations.from.parents..relatives..and.friends+cultural.proximity.with.home
                     +geographical.proximity.with.home+Quality.and.reputation.of.the.University+Recognition.of.the.degree.in.my.home.country
                     +Quality.of.the.teaching.staff+The.reputation.of.the.alumni+The.reputation.of.the.international.community
                     +HSE.position.in.international.university.rankings+Cost.of.tuition.for.international.students
                     +Availability.of.scholarships+Support.services.for.international.students+Graduates.employment.rates
                     +HSE.s.international.strategic.alliances+Local.employers.preference.of..degrees.awarded.by.HSE,
                     factors = 8, data = PullFactors, cor = FALSE, na.action = na.omit)

print(fit.efa2, digits=2, cutoff=.3, sort=TRUE)

#with p-value 6.54e-10 8 factors are sufficient.

#indexing the correlated pull factors

#prospect of employment 
cor.test(Graduates.employment.rates,Local.employers.preference.of..degrees.awarded.by.HSE)
cor.test(Career.prospects.in.Russia,Local.employers.preference.of..degrees.awarded.by.HSE)
cor.test(Graduates.employment.rates,Career.prospects.in.Russia)
employment.prospect<-(Graduates.employment.rates+Local.employers.preference.of..degrees.awarded.by.HSE+
                        Career.prospects.in.Russia)/3
employment.prospect<-round(employment.prospect,2)
table(employment.prospect)
PullFactors$employment.prospect<-recode(employment.prospect, "1:1.33=1; 1.67:2.33=2; 2.67:3.33=3; 3.67:4.33=4; 4.67:5=5")
table(PullFactors$employment.prospect)

#proximity
cor.test(cultural.proximity.with.home,geographical.proximity.with.home)
proximity<-(cultural.proximity.with.home+geographical.proximity.with.home)/2
table(proximity)
PullFactors$proximity<-recode(proximity, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PullFactors$proximity)

#cost
cor.test(Low.cost.of.living,Low.tuition.fees)
cor.test(Low.cost.of.living,Cost.of.tuition.for.international.students)
cor.test(Low.tuition.fees,Cost.of.tuition.for.international.students)
cost.of.living<-(Low.cost.of.living+Low.tuition.fees+Cost.of.tuition.for.international.students)/3
cost.of.living<-round(cost.of.living,2)
table(cost.of.living)
PullFactors$cost.of.living<-recode(cost.of.living, "1:1.33=1; 1.67:2.33=2; 2.67:3.33=3; 3.67:4.33=4; 4.67:5=5")
table(PullFactors$cost.of.living)

#scholarships
cor.test(Awarded.scholarships.or.tuition.waiver,Availability.of.scholarships)
scholarship<-(Awarded.scholarships.or.tuition.waiver+Availability.of.scholarships)/2
table(scholarship)
PullFactors$scholarship<-recode(scholarship, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PullFactors$scholarship)

#HSE Quality
cor.test(Quality.and.reputation.of.the.University,Quality.of.the.teaching.staff)
HSE.quality<-(Quality.and.reputation.of.the.University+Quality.of.the.teaching.staff)/2
table(HSE.quality)
PullFactors$HSE.quality<-recode(HSE.quality, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PullFactors$HSE.quality)

#HSE Reputation
cor.test(The.reputation.of.the.alumni, The.reputation.of.the.international.community)
HSE.reputation<-(The.reputation.of.the.alumni+The.reputation.of.the.international.community)/2
table(HSE.reputation)
PullFactors$HSE.reputation<-recode(HSE.reputation, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PullFactors$HSE.reputation)

#Program Choice
cor.test(Higher.quality.of.education..compared.to.home.country., Availability.of.desired.study.program)
program.choice<-(Higher.quality.of.education..compared.to.home.country.+ Availability.of.desired.study.program)/2
table(program.choice)
PullFactors$program.choice<-recode(program.choice, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PullFactors$program.choice)

attach(PullFactors)

#checking for realibility
PullFactorsRuHSE<-data.frame(program.choice,cost.of.living,proximity, scholarship,HSE.quality, 
                             HSE.reputation, Attraction.to.Russian.culture..society,
                             Recognition.of.the.degree.in.my.home.country,Recommendations.from.family.friends,
                             HSE.position.in.international.university.rankings,Support.services.for.international.students,
                             HSE.s.international.strategic.alliances,employment.prospect)
psych::alpha(PullFactorsRuHSE)

#Full model 

model<-glm(degree~employment.prospect+proximity+
             cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
             Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
             Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
             Support.services.for.international.students+HSE.s.international.strategic.alliances, 
           data = PullFactors, family = binomial(link=logit))

summary(model)

modela<-glm(degree~employment.prospect+proximity+
             cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
             Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
             Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
             Support.services.for.international.students+HSE.s.international.strategic.alliances+
              as.numeric(age)+as.numeric(Home.country)+as.numeric(Gender), 
           data = PullFactors, family = binomial(link=logit))

summary(modela)

model5<-lm(as.numeric(language.of.instruction)~employment.prospect+Career.prospects.in.Russia+proximity+
             cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
             Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
             Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
             Support.services.for.international.students+HSE.s.international.strategic.alliances, 
           data = PullFactors)

summary(model5)

#Full model with controls
model6<-lm(as.numeric(language.of.instruction)~employment.prospect+proximity+
             cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
             Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
             Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
             Support.services.for.international.students+HSE.s.international.strategic.alliances+
             as.numeric(age)+as.numeric(Home.country)+as.numeric(Gender), data = PullFactors)

summary(model6)

#Fullmodel with interaction effect
model7<-lm(as.numeric(language.of.instruction)~employment.prospect+scholarship+
             HSE.quality+HSE.reputation+Recognition.of.the.degree.in.my.home.country+
             Support.services.for.international.students+Attraction.to.Russian.culture..society+
             (Recommendations.from.family.friends+proximity+cost.of.living+ 
                program.choice+HSE.position.in.international.university.rankings+
                HSE.s.international.strategic.alliances)*world.region, data = PullFactors)

summary(model7)


#Fullmodel and controls with interaction effect
model8<-lm(as.numeric(language.of.instruction)~employment.prospect+scholarship+
             HSE.quality+HSE.reputation+Recognition.of.the.degree.in.my.home.country+
             Support.services.for.international.students+Attraction.to.Russian.culture..society+
             (Recommendations.from.family.friends+proximity+cost.of.living+ 
                program.choice+HSE.position.in.international.university.rankings+
                HSE.s.international.strategic.alliances)*world.region+
              as.numeric(age)+as.numeric(Home.country)+as.numeric(Gender), data = PullFactors)

summary(model8)

#Graph for the models
stargazer(model5, model6, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow",out = "cc.doc",
          covariate.labels =c("Constant","Employment prospects","Proximity","Cost of living", "Scholarship",
                              "HSE quality", "HSE reputation", "Program choice","Recommendations from family and friends",
                              "Attraction to Russian culture", "Recognition of HSE degree in my Home Country",
                              "HSE position in University rankings","Support services for International Students",
                              "HSE international stragtegic alliances","Age", "Home Country", "Gender"))

stargazer(model7, model8, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out = "dd.doc",
          covariate.labels =c("Constant","Employment prospects","Scholarship","HSE quality","HSE reputation",
                              "Recognition of HSE degree in my Home Country","Support services for International Students",
                              "Attraction to Russian culture", "Recommendations from family and friends","Proximity",
                              "Cost of living", "Choice of program", "HSE position in University rankings",
                              "HSE international alliances","Asia", "CIS","Europe", "Middle East", "South America", 
                              "Age", "Home Country", "Gender", "Recomendations from family and friends*Asia",
                              "Recomendations from family and friends*CIS", 
                              "Recomendations from family and friends*Europe","Recomendations from family and friends*Middle East", 
                              "Recomendations from family and friends*South America","Proximity*Asia", "Proximity*CIS", "Proximity*Europe",
                              "Proximity*Middle East","Proximity*South America", "Cost of living*Asia","Cost of living*CIS", 
                              "Cost of living*Europe","Cost of living*Middle East","Cost of living*South America","Choice of program*Asia", 
                              "Choice of program*CIS", "Choice of program*Europe","Choice of program*Middle East",
                              "Choice of program*South America", "HSE position in University rankings*Asia",
                              "HSE position in University rankings*CIS", "HSE position in University rankings*Europe",
                              "HSE position in University rankings*Middle East","HSE position in University rankings*South America",
                              "HSE international alliances*Asia", "HSE international alliances*CIS", "HSE international alliances*Europe",
                              "HSE international alliances*Middle East","HSE international alliances*South America"))

#checking for multicolinearity
vif(model5)
vif(model6)
vif(model7)
vif(model8)


#####Creating the Crosstab function

crosstab <- function (..., dec.places = NULL,
                      type = NULL,
                      style = "wide",
                      row.vars = NULL,
                      col.vars = NULL,
                      percentages = TRUE, 
                      addmargins = TRUE,
                      subtotals=TRUE)
  
  ###################################################################################
#                                                                                 #
# Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
# School of Environmental Sciences, University of Liverpool, UK.                  #
#                                                                                 #
# Adapted from the function ctab() in the catspec packge.                         #
#                                                                                 #
# Version: 12th July 2013                                                         #
#                                                                                 #
# Output best viewed using the companion function print.crosstab()                #
#                                                                                 #
###################################################################################


#Declare function used to convert frequency counts into relevant type of proportion or percentage
{
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }
  
  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  
  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))    
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)
  
  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types
  
  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables
  
  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct  
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count  
    type <- "frequency"
  }
  
  
  
  #Check for integrity of requested analysis and adjust values of function arguments as required
  
  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }
  
  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }
  
  if ((length(type)>1) & (subtotals==FALSE)) { 
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }
  
  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }
  
  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
  
  
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }
  
  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }
  
  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'
  
  args <- list(...)    
  
  if (length(args) > 1) {
    if (!all(sapply(args, is.factor))) 
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages", 
                    "addmargins", "subtotals")) if (is.null(get(opt))) 
                      assign(opt, eval(parse(text = paste("tbl$", opt, 
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }
  
  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))
  
  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }
  
  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency") 
    crosstab <- tbl
  else 
    crosstab <- mk.pcnt.tbl(tbl, type[1])
  
  
  #If multiple table types requested, create and add these to 
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency") 
        crosstab <- tbl
      else crosstab <- mk.pcnt.tbl(tbl, type[i])
      crosstab <- as.data.frame.table(crosstab)
      crosstab[z] <- i
      tbldat <- rbind(tbldat, crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(crosstab))[z - 1] <- ""
  }
  
  
  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {
    
    vars <- c(row.vars,col.vars)
    
    if (length(type)==1) {
      if (type=="row.pct") 
      { crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else 
      { if (type=="column.pct") 
      { crosstab <- addmargins(crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else 
        { if (type=="joint.pct") 
        { crosstab <- addmargins(crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        }
          else #must be total.pct OR frequency
          { crosstab <- addmargins(crosstab)
          tbl <- addmargins(tbl)
          }
        }
      } 
    }
    
    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }
    
  }  
  
  
  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    
    #Create version of crosstab in ftable format
    t1 <- crosstab 
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
    
    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
    
    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
    
    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]
    
    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
      }
    }
    
    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals
    
    t1 <- t1[((lab==0) | (lab==n.row.vars)),]
    
    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""
    
    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL
    
  }
  
  
  
  #Create output object 'result' [class: crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals
  
  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$crosstab <- crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]  
  result$crosstab.nosub <- t1  #crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
  class(result) <- "crosstab"    
  
  #Return 'result' as output of function
  result
  
}



print.crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
  
  ###################################################################################
  #                                                                                 #
  # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
  # School of Environmental Sciences, University of Liverpool, UK.                  #
  #                                                                                 #
  # Adapted from the function print.ctab() in the catspec packge.                   #
  #                                                                                 #
  # Version: 12th July 2013                                                         #
  #                                                                                 #
  # Designed to provide optimal viewing of the output from crosstab()               #
  #                                                                                 #
  ###################################################################################
  
  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  if (length(x$type)>1) {
    z<-length(names(dimnames(x$crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z) 
    } else {
      col.vars<-c(z,col.vars)
    }
  }
  
  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$crosstab,x$dec.places))
    }
  }
  
  
  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
    
    tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)
    
    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)
    
  }
  
  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {
    
    t1 <- x$crosstab.nosub
    
    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
    
    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
    
    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }
    
    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
    
  }
  
}


library(sjPlot)
library(sjmisc)
library(sjlabelled)
theme_set(theme_bw())


##Descriptive Statistics: Demographic information

#degree
freq(degree, display.type = F,report.nas = F, headings = T, cumul = F, style = "grid")
plot_frq(degree, geom.colors = c("red", "yellow"), title = "Degree")

crosstab(data, row.vars = c("degree"), 
         col.vars ="world.region", type = "f", style = "long", 
         addmargins = T, dec.places = 0, subtotals = T)

#language of instruction
freq(language.of.instruction, display.type = F, 
     report.nas = F, headings = T, cumul = F, style = "grid")
plot_frq(language.of.instruction, geom.colors = "#336699", title = "Language of instruction")

crosstab(data, row.vars = c("language.of.instruction"), 
         col.vars ="world.region", type = "f", style = "long", 
         addmargins = T, dec.places = 0, subtotals = T)

#Gender
freq(Gender, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")
plot_frq(Gender, geom.colors = "#336699", title = "Gender")

crosstab(data, row.vars = c("Gender"), 
         col.vars ="world.region", type = "f", style = "long", 
         addmargins = T, dec.places = 0, subtotals = T)


#Age
freq(age, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")
plot_frq(age, geom.colors = "#336699", title = "Age")
crosstab(data, row.vars = c("age"), 
         col.vars ="world.region", type = "f", style = "long", 
         addmargins = T, dec.places = 0, subtotals = T)


#World Region
freq(world.region, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")
plot_frq(world.region, geom.colors = "#336699", title = "World Region")

#Family income
freq(What.was.your.annual.family.income.when.you.were.applying.to.study.abroad..estimate.in.US.dollars., 
     display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")


crosstab(data, row.vars = "family.income", 
         col.vars ="world.region", 
         type = "f", style = "long", addmargins = T, dec.places = 0, subtotals = T)

#Length of stay in Russia
freq(How.long.have.you.been.in.Russia.studying.for.your.current.program., 
     display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#Student finance status
freq(How.are.you.financing.your.participation.in.the.program., 
     display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

crosstab(data, row.vars = c("How.are.you.financing.your.participation.in.the.program."), 
         col.vars ="world.region", type = "f", style = "long", 
         addmargins = T, dec.places = 0, subtotals = T)

#stay in Russia
freq(Have.you.ever.been.in.Russia.before.you.enrolled.for.your.current.program, 
     display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#Data preparation: Push factors in home country influencing the decision to study in Russia


#Access to education : index of unavailable program and low quality of education
PushFactors$education.access <- factor(PushFactors$access.to.education,levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(PushFactors$education.access, PushFactors$access.to.education)
freq(PushFactors$education.access, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#competitive University admission in home country
PushFactors$competitive.uni.admission<-factor(PushFactors$Competitive.University.admission.process,
                                          levels = c(1,2,3,4,5),
                                          labels = c("Not at all influential", 
                                                     "Slightly influential", 
                                                     "Somewhat influential", 
                                                     "Very influential", 
                                                     "Extremely influential"))

table(PushFactors$competitive.uni.admission, PushFactors$Competitive.University.admission.process)
freq(PushFactors$competitive.uni.admission, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#Advantage of international degree
PushFactors$advantage.of.international.degree <-factor(PushFactors$Perceived.advantage.of.international.degree,
                                            levels = c(1,2,3,4,5),
                                            labels = c("Not at all influential", 
                                                       "Slightly influential", 
                                                       "Somewhat influential", 
                                                       "Very influential", 
                                                       "Extremely influential"))

table(PushFactors$advantage.of.international.degree, PushFactors$Perceived.advantage.of.international.degree)
freq(PushFactors$advantage.of.international.degree, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#unavailability of scholarships

PushFactors$unavailability.of.scholarship <- factor(PushFactors$Unavailability.of.scholarship.opportunities,
                                        levels = c(1,2,3,4,5),
                                        labels = c("Not at all influential", 
                                                   "Slightly influential", 
                                                   "Somewhat influential", 
                                                   "Very influential", 
                                                   "Extremely influential"))

table(PushFactors$unavailability.of.scholarship, PushFactors$Unavailability.of.scholarship.opportunities)
freq(PushFactors$unavailability.of.scholarship, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#encouragement from family and Friends
PushFactors$family.friends.encourage <- factor(PushFactors$encouragement.from.family.friends,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential", 
                                               "Slightly influential", 
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(PushFactors$family.friends.encourage, PushFactors$encouragement.from.family.friends)
freq(PushFactors$family.friends.encourage, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#advantage of studying abroad: index of better earning prospects and social prestige
PushFactors$benefits.of.studying.abroad <- factor(PushFactors$advantages.of.studying.abroad,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(PushFactors$benefits.of.studying.abroad, PushFactors$advantages.of.studying.abroad)
freq(PushFactors$benefits.of.studying.abroad, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

#experience different culture

PushFactors$different.culture <- factor(PushFactors$To.experience.a.different.culture,
                                       levels = c(1,2,3,4,5),
                                       labels = c("Not at all influential", 
                                                  "Slightly influential", 
                                                  "Somewhat influential", 
                                                  "Very influential", 
                                                  "Extremely influential"))

table(PushFactors$different.culture, PushFactors$To.experience.a.different.culture)
freq(PushFactors$different.culture, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

attach(PushFactors)

cor.test(as.numeric(language.of.instruction), as.numeric(different.culture))
language.of.instruction
#anova 

#access to education
kruskal.test(as.numeric(education.access) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$education.access), PushFactors$world.region,
                     p.adjust.method = "BH")

#competitive admission
kruskal.test(as.numeric(competitive.uni.admission) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$competitive.uni.admission), PushFactors$world.region,
                     p.adjust.method = "BH")


#advantage of international degree
kruskal.test(as.numeric(advantage.of.international.degree) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$advantage.of.international.degree), PushFactors$world.region,
                     p.adjust.method = "BH")

#unavailability of scholarships
kruskal.test(as.numeric(unavailability.of.scholarship) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$unavailability.of.scholarship), PushFactors$world.region,
                     p.adjust.method = "BH")

#encouragement from family and friends
kruskal.test(as.numeric(family.friends.encourage) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$family.friends.encourage), PushFactors$world.region,
                     p.adjust.method = "BH")


#benefits of studying abroad
kruskal.test(as.numeric(benefits.of.studying.abroad) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$benefits.of.studying.abroad), PushFactors$world.region,
                     p.adjust.method = "BH")


#experience a different culture
kruskal.test(as.numeric(different.culture) ~ world.region, data = PushFactors)

pairwise.wilcox.test(as.numeric(PushFactors$different.culture), PushFactors$world.region,
                     p.adjust.method = "BH")


#Descriptive Statistics: Push Factors influencing the decision to move to Russia

#push factors
pushfactor<-data.frame(education.access,competitive.uni.admission,advantage.of.international.degree,
                       unavailability.of.scholarship,encouragement.family.friends, benefits.of.studying.abroad, 
                       different.culture)


#Plots

#Plots for the push factors in the home countries

#Push factors
plot_stackfrq(pushfactor, title = "Push Factors in Home Country", geom.colors = "gs")

#access to education
plot_grpfrq(education.access, world.region, geom.colors = "gs", 
            title = "Lack of access to education in Home Country")

#competitive admission
plot_grpfrq(competitive.uni.admission, world.region, geom.colors = "gs", 
            title = "Competitive University admissions process in Home Country")

#advantage of international degree
plot_grpfrq(advantage.of.international.degree, world.region, geom.colors = "gs", 
            title = "Perceived advantage of an international degree")

#unavailability of scholarships
plot_grpfrq(unavailability.of.scholarship, world.region, geom.colors = "gs", 
            title = "Unavailability of scholarship opportunities in Home Country")

#encouragement from family and friends
plot_grpfrq(family.friends.encourage, world.region, geom.colors = "gs", 
            title = "Encoragement from family and friends")

#benefits of studying abroad
plot_grpfrq(benefits.of.studying.abroad, world.region, geom.colors = "gs", 
            title = "Perceived benefits of studying abroad")

#experience a different culture
plot_grpfrq(different.culture, world.region, geom.colors = "gs", 
            title = "To experience a diiferent culture")


#Data preparation: Pull factors in Russia influencing the decision to study in Russia

#Prospect of employment in Russia
PullFactors$employment_prospect <- factor(PullFactors$employment.prospect,
                                  levels = c(1,2,3,4,5),
                                  labels = c("Not at all influential", 
                                             "Slightly influential", 
                                             "Somewhat influential", 
                                             "Very influential", 
                                             "Extremely influential"))

table(PullFactors$employment_prospect, PullFactors$employment.prospect)

#Proximity : index of geographical and cultural proximity
PullFactors$geo.cul.proximity <- factor(PullFactors$proximity,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(PullFactors$geo.cul.proximity, PullFactors$proximity)

#low cost of living
PullFactors$living.cost <- factor(PullFactors$cost.of.living,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(PullFactors$living.cost, PullFactors$cost.of.living)

#Scholarships
PullFactors$available.scholarship <- factor(PullFactors$scholarship,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential",  
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(PullFactors$available.scholarship, PullFactors$scholarship)

#HSE quality: index of quality of teachers and quality of HSE
PullFactors$HSE_quality <- factor(PullFactors$HSE.quality,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential", 
                                               "Slightly influential", 
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(PullFactors$HSE_quality, PullFactors$HSE.quality)

#HSE reputation: Index of reputation of international community and alumni
PullFactors$HSE_reputation <- factor(PullFactors$HSE.reputation,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential", 
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(PullFactors$HSE_reputation, PullFactors$HSE.reputation)

#Program choice: index of available desired program & higher quality of education
PullFactors$program_choice <- factor(PullFactors$program.choice,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential", 
                                        "Slightly influential", 
                                        "Somewhat influential", 
                                        "Very influential", 
                                        "Extremely influential"))

table(PullFactors$program_choice, PullFactors$program.choice)

#Attraction to Russian culture
PullFactors$russian.culture <- factor(PullFactors$Attraction.to.Russian.culture..society,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(PullFactors$russian.culture, PullFactors$Attraction.to.Russian.culture..society)


#Recognition of HSE degree
PullFactors$recognition.of.HSE.degree <- factor(PullFactors$Recognition.of.the.degree.in.my.home.country,
                                                levels = c(1,2,3,4,5),
                                                labels = c("Not at all influential", 
                                                           "Slightly influential", 
                                                           "Somewhat influential", 
                                                           "Very influential", 
                                                           "Extremely influential"))

table(PullFactors$recognition.of.HSE.degree, PullFactors$Recognition.of.the.degree.in.my.home.country)


#Recommendations from family and friends
PullFactors$family.friends.recommendations <- factor(PullFactors$Recommendations.from.family.friends,
                                         levels = c(1,2,3,4,5),
                                         labels = c("Not at all influential", 
                                                    "Slightly influential", 
                                                    "Somewhat influential", 
                                                    "Very influential", 
                                                    "Extremely influential"))

table(PullFactors$family.friends.recommendations, PullFactors$Recommendations.from.family.friends)

#HSE rank
PullFactors$HSE.rank <- factor(PullFactors$HSE.position.in.international.university.rankings,
                               levels = c(1,2,3,4,5),
                               labels = c("Not at all influential",
                                          "Slightly influential",
                                          "Somewhat influential",
                                          "Very influential",
                                          "Extremely influential"))

table(PullFactors$HSE.rank, PullFactors$HSE.position.in.international.university.rankings)

#Support Services for international students
PullFactors$international.students.support <- factor(PullFactors$Support.services.for.international.students,
                                                     levels = c(1,2,3,4,5),
                                                     labels = c("Not at all influential", 
                                                                "Slightly influential", 
                                                                "Somewhat influential", 
                                                                "Very influential", 
                                                                "Extremely influential"))

table(PullFactors$international.students.support, PullFactors$Support.services.for.international.students)

#HSE international strategic alliances
HSE.alliances<-as.factor(HSE.s.international.strategic.alliances)
PullFactors$HSE.alliances <- factor(PullFactors$HSE.s.international.strategic.alliances,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential",
                                               "Slightly influential",
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(PullFactors$HSE.alliances, PullFactors$HSE.s.international.strategic.alliances)

attach(PullFactors)

#anova
#employment prospect
kruskal.test(as.numeric(employment_prospect) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$employment_prospect), PullFactors$world.region,
                     p.adjust.method = "BH")


plot_grpfrq(employment_prospect, world.region, geom.colors = "gs", title = "Employment prospect in Russia")

#proximity
kruskal.test(as.numeric(geo.cul.proximity) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$geo.cul.proximity), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(geo.cul.proximity, world.region, geom.colors = "gs", title = "Proximity")


#cost of living
kruskal.test(as.numeric(living.cost) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$living.cost), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(living.cost, world.region, geom.colors = "gs", title = "Cost of living in Russia")


#scholarship
kruskal.test(as.numeric(available.scholarship) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$available.scholarship), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(available.scholarship, world.region, geom.colors = "gs", title = "Availability of Scholarship in Russia")


#HSE quality
kruskal.test(as.numeric(HSE_quality) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$HSE_quality), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(HSE_quality, world.region, geom.colors = "gs", title = "HSE quality")


#HSE reputation
kruskal.test(as.numeric(HSE_reputation) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$HSE_reputation), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(HSE_reputation, world.region, geom.colors = "gs", title = "HSE reputation")


#Choice of program
kruskal.test(as.numeric(program_choice) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$program_choice), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(program_choice, world.region, geom.colors = "gs", title = "Choice of program")


#Attraction to Russian culture
kruskal.test(as.numeric(russian.culture) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$russian.culture), PullFactors$world.region,
                     p.adjust.method = "BH")


#Attraction to Russian culture
plot_grpfrq(russian.culture, world.region, geom.colors = "gs", title = "Attraction to Russian culture")


#Recognition of degree
kruskal.test(as.numeric(recognition.of.HSE.degree) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$recognition.of.HSE.degree), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(recognition.of.HSE.degree, world.region, geom.colors = "gs", title = "Recognition of HSE degree in my Home Country")


#recommendations from family and friends
kruskal.test(as.numeric(family.friends.recommendations) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$family.friends.recommendations), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(family.friends.recommendations, world.region, geom.colors = "gs", title = "Recommendations from family and friends")

#HSE rank
kruskal.test(as.numeric(HSE.rank) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$HSE.rank), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(HSE.rank, world.region, geom.colors = "gs", title = "HSE's position on World Universities ranking")

#Support for international students
kruskal.test(as.numeric(international.students.support) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$international.students.support), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(international.students.support, world.region, geom.colors = "gs", 
            title = "Support services for international students in HSE")

#HSE alliances
kruskal.test(as.numeric(HSE.alliances) ~ world.region, data = PullFactors)

pairwise.wilcox.test(as.numeric(PullFactors$HSE.alliances), PullFactors$world.region,
                     p.adjust.method = "BH")

plot_grpfrq(HSE.alliances, world.region, geom.colors = "gs", title = "HSE strategic international alliances")


#Descriptive Statistics: Pull Factors in HSE influencing the decision to move to Russia
Pullfactor<-data.frame(employment_prospect,geo.cul.proximity, living.cost, available.scholarship,
                       HSE_quality, HSE_reputation, program_choice, russian.culture, recognition.of.HSE.degree,
                       family.friends.recommendations, HSE.rank, international.students.support, HSE.alliances)


#Plots for the pull factors in Russia
plot_stackfrq(Pullfactor, title = "PUll factors in Russia and HSE", geom.colors = "gs")

PF<-data.frame(world.region, employment_prospect,geo.cul.proximity, living.cost, available.scholarship,
                       HSE_quality, HSE_reputation, program_choice, russian.culture, recognition.of.HSE.degree,
                       family.friends.recommendations, HSE.rank, international.students.support, HSE.alliances)
attach(PF)
library(corregp)

summary(PF)




pf.crg <- corregp(world.region~employment_prospect+geo.cul.proximity+living.cost+available.scholarship+
                  HSE_quality+ HSE_reputation+ program_choice+ russian.culture+recognition.of.HSE.degree+
                  family.friends.recommendations + HSE.rank+ international.students.support+ HSE.alliances,
                    data = PF, b = 40)


summary(pf.crg)

plot(pf.crg, x_ell = TRUE)
anova(pf.crg)


coef(pf.crg, axes = 1:5)


###STUDENTS POST GRADUATION PLANS##

#Descriptive Statistics
#Post Graduation migration plans
freq(What.are.your.plans.after.graduation., display.type = F, report.nas = F, 
     headings = T, cumul = F, style = "grid")

plot_grpfrq(What.are.your.plans.after.graduation., world.region, geom.colors = "gs", 
            title = "Post graduation plans")
plot_grpfrq(What.are.your.plans.after.graduation., degree, geom.colors = "gs", 
            title = "Post graduation plans")

#Stay in Russia
freq(What.will.be.your.reason.for.staying.in.Russia.after.graduation., display.type = F, report.nas = F, 
     headings = T, cumul = F, style = "grid")

plot_grpfrq(What.will.be.your.reason.for.staying.in.Russia.after.graduation., world.region, geom.colors = "gs", 
            title = "Reasons for staying in Russia after graduation")
plot_grpfrq(What.are.your.plans.after.graduation., degree, geom.colors = "gs", 
            title = "Reasons for staying in Russia after graduation")

#Return home
freq(What.will.be.your.reason.for.returning.home.after.graduation., display.type = F, report.nas = F, 
     headings = T, cumul = F, style = "grid")

plot_grpfrq(What.will.be.your.reason.for.returning.home.after.graduation., world.region, geom.colors = "gs", 
            title = "Reasons for returning home after graduation")
plot_grpfrq(What.will.be.your.reason.for.returning.home.after.graduation., degree, geom.colors = "gs", 
            title = "Reasons for returning home after graduation")

#move to another country
freq(What.will.be.your.reason.for.moving.to.another.country.after.graduation., display.type = F, report.nas = F, 
     headings = T, cumul = F, style = "grid")
plot_grpfrq(What.will.be.your.reason.for.moving.to.another.country.after.graduation., world.region, geom.colors = "gs", 
            title = "Reasons for moving to another country after graduation")
plot_grpfrq(What.will.be.your.reason.for.moving.to.another.country.after.graduation., degree, geom.colors = "gs", 
            title = "Reasons for moving to another country after graduation", show.na = F)

#Stay in Russia after graduation

#Better job opportunities
job.opportunities <-as.factor(Better.job.opportunities..in.comparison.with.home.country.)
job.opportunities <- factor(job.opportunities,
                            levels = c(1,2,3,4,5),
                            labels = c("Not at all influential", 
                                       "Slightly influential", 
                                       "Somewhat influential", 
                                       "Very influential", 
                                       "Extremely influential"))

table(job.opportunities, Better.job.opportunities..in.comparison.with.home.country.)

#Higher quality of life
high.quality.life<-as.factor(Higher.quality.of.life..in.comparison.with.home.country.)
high.quality.life <- factor(high.quality.life,
                            levels = c(1,2,3,4,5),
                            labels = c("Not at all influential", 
                                       "Slightly influential", 
                                       "Somewhat influential", 
                                       "Very influential", 
                                       "Extremely influential"))

table(high.quality.life, Higher.quality.of.life..in.comparison.with.home.country.)

#Better career opportunities
career.opportunities<-as.factor(Better.career.opportunities.and.advancement.in.chosen.profession)
career.opportunities <- factor(career.opportunities,
                               levels = c(1,2,3,4,5),
                               labels = c("Not at all influential", 
                                          "Slightly influential", 
                                          "Somewhat influential", 
                                          "Very influential", 
                                          "Extremely influential"))

table(career.opportunities,Better.career.opportunities.and.advancement.in.chosen.profession)

#high income level
high.income.level<-as.factor(Higher.income.level)
high.income.level <- factor(high.income.level,
                            levels = c(1,2,3,4,5),
                            labels = c("Not at all influential", 
                                       "Slightly influential",  
                                       "Somewhat influential", 
                                       "Very influential", 
                                       "Extremely influential"))

table(high.income.level, Higher.income.level)

#Ties to family and friends
family.friends.ties<-as.factor(Ties.to.family.and.friends)
family.friends.ties <- factor(family.friends.ties,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(family.friends.ties,Ties.to.family.and.friends)

#international experience
international.experience<-as.factor(Gain.international.experience)
international.experience <- factor(international.experience,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential",
                                              "Slightly influential",
                                              "Somewhat influential",
                                              "Very influential",
                                              "Extremely influential"))

table(international.experience, Gain.international.experience)

#family expectations
familyexpectations<-as.factor(Family.expectations)
familyexpectations <- factor(familyexpectations,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential", 
                                        "Slightly influential", 
                                        "Somewhat influential", 
                                        "Very influential",
                                        "Extremely influential"))

table(familyexpectations, Family.expectations)

#Restrcitive cultural practices
cultural.practices<-as.factor(Restrictive.cultural.practices..eg..pressure.to.marry.)
cultural.practices <- factor(cultural.practices,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential", 
                                        "Slightly influential", 
                                        "Somewhat influential", 
                                        "Very influential", 
                                        "Extremely influential"))

table(cultural.practices, Restrictive.cultural.practices..eg..pressure.to.marry.)

#limited jobs
limited.jobs<-as.factor(Limited.job.opportunities.in.home.country)
limited.jobs <- factor(limited.jobs,
                       levels = c(1,2,3,4,5),
                       labels = c("Not at all influential", 
                                  "Slightly influential", 
                                  "Somewhat influential", 
                                  "Very influential", 
                                  "Extremely influential"))

table(limited.jobs, Limited.job.opportunities.in.home.country)

#lower income levels
lower.income<-as.factor(Lower.income.levels)
lower.income <- factor(lower.income,
                       levels = c(1,2,3,4,5),
                       labels = c("Not at all influential", 
                                  "Slightly influential", 
                                  "Somewhat influential", 
                                  "Very influential", 
                                  "Extremely influential"))

table(lower.income, Lower.income.levels)


#Lower quality of life
lower.quality.life<-as.factor(Lower.quality.of.life.2)
lower.quality.life <- factor(lower.quality.life,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential",
                                        "Slightly influential",
                                        "Somewhat influential", 
                                        "Very influential", 
                                        "Extremely influential"))

table(lower.quality.life, Lower.quality.of.life.2)


#Political persecution
politicalpersecution<-as.factor(Political.persecution)
politicalpersecution <- factor(politicalpersecution,
                               levels = c(1,2,3,4,5),
                               labels = c("Not at all influential", 
                                          "Slightly influential", 
                                          "Somewhat influential", 
                                          "Very influential", 
                                          "Extremely influential"))

table(politicalpersecution, Political.persecution)

#Dangers to ones own life
danger.to.ones.life<-as.factor(Danger.or.fear.for.one.s.own.life)
danger.to.ones.life <- factor(Danger.or.fear.for.one.s.own.life,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(danger.to.ones.life, Danger.or.fear.for.one.s.own.life)

#Factors influencing the decision to stay in Russia
RStayFactors<-data.frame(job.opportunities,
                         high.quality.life,career.opportunities,
                         high.income.level, family.friends.ties,
                         international.experience, familyexpectations,
                         cultural.practices,limited.jobs,lower.income,lower.quality.life,
                         politicalpersecution,danger.to.ones.life)

RussiaStay_factors<-data.frame(world.region, degree, Better.job.opportunities..in.comparison.with.home.country.+
                                 Higher.quality.of.life..in.comparison.with.home.country.+
                                 Better.career.opportunities.and.advancement.in.chosen.profession+
                                 Higher.income.level+ Ties.to.family.and.friends+
                                 Gain.international.experience+ Family.expectations+
                                 Restrictive.cultural.practices..eg..pressure.to.marry.+
                                 Limited.job.opportunities.in.home.country+Lower.income.levels+
                                 Lower.quality.of.life.2+Political.persecution+Danger.or.fear.for.one.s.own.life)

#Crosstabulation of Russia Stay factors with Region and degree

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.job.opportunities..in.comparison.with.home.country.", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.quality.of.life..in.comparison.with.home.country.", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.career.opportunities.and.advancement.in.chosen.profession", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.income.level", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Ties.to.family.and.friends", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Gain.international.experience", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Family.expectations", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Limited.job.opportunities.in.home.country", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)


crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.income.levels", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.quality.of.life.2", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Political.persecution", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
        col.vars ="Danger.or.fear.for.one.s.own.life", 
        type = c("f", "t"), style = "long", 
        addmargins = T, dec.places = 2, subtotals = T)


#Graphs

#Factors influencing the decision to stay
plot_stackfrq(RStayFactors)

#Better job opportunities
plot_grpfrq(job.opportunities, world.region, geom.colors = "gs", title = "Better job opportunities")
plot_grpfrq(job.opportunities, degree, geom.colors = "gs", title = "Better job opportunities")

#Higher quality of life
plot_grpfrq(high.quality.life, world.region, geom.colors = "gs", title = "Higher quality of life")
plot_grpfrq(high.quality.life, degree, geom.colors = "gs", title = "Higher quality of life")

#Better career opportunities
plot_grpfrq(career.opportunities, world.region, geom.colors = "gs", title = "Better career opportunities")
plot_grpfrq(career.opportunities, degree, geom.colors = "gs", title = "Better career opportunities")

#high income level
plot_grpfrq(high.income.level, world.region, geom.colors = "gs", title = "high income level")
plot_grpfrq(high.income.level, degree, geom.colors = "gs", title = "high income level")

#Ties to family and friends
plot_grpfrq(family.friends.ties, world.region, geom.colors = "gs", title = "Ties to family and friends")
plot_grpfrq(family.friends.ties, degree, geom.colors = "gs", title = "Ties to family and friends")

#international experience
plot_grpfrq(international.experience, world.region, geom.colors = "gs", title = "Gain international experience")
plot_grpfrq(international.experience, degree, geom.colors = "gs", title = "Gain international experience")

#family expectations
plot_grpfrq(familyexpectations, world.region, geom.colors = "gs", title = "Family expectations")
plot_grpfrq(familyexpectations, degree, geom.colors = "gs", title = "Family expectations")


#Restrcitive cultural practices
plot_grpfrq(cultural.practices, world.region, geom.colors = "gs", title = "Restrictive cultural practices")
plot_grpfrq(cultural.practices, degree, geom.colors = "gs", title = "Restrictive cultural practices")

#limited jobs
plot_grpfrq(limited.jobs, world.region, geom.colors = "gs", title = "Limited job opportunities")
plot_grpfrq(limited.jobs, degree, geom.colors = "gs", title = "Limited job opportunities")

#lower income levels
plot_grpfrq(lower.income, world.region, geom.colors = "gs", title = "Lower income level")
plot_grpfrq(lower.income, degree, geom.colors = "gs", title = "Lower income level")

#Lower quality of life
plot_grpfrq(lower.quality.life, world.region, geom.colors = "gs", title = "Lower quality of life")
plot_grpfrq(lower.quality.life, degree, geom.colors = "gs", title = "Lower quality of life")

#Political persecution
plot_grpfrq(politicalpersecution, world.region, geom.colors = "gs", title = "Political persecution")
plot_grpfrq(politicalpersecution, degree, geom.colors = "gs", title = "Political persecution")

#Dangers to ones own life
plot_grpfrq(danger.to.ones.life, world.region, geom.colors = "gs", title = "Danger to one's own life")
plot_grpfrq(danger.to.ones.life, degree, geom.colors = "gs", title = "Danger to one's own life")


#Data preparation Returning home: Pull factors in home country

#Better job opportunities
professional.opportunities <-as.factor(Better.professional.opportunities.in.home.country)
professional.opportunities <- factor(professional.opportunities,
                                     levels = c(1,2,3,4,5),
                                     labels = c("Not at all influential", 
                                                "Slightly influential", 
                                                "Somewhat influential", 
                                                "Very influential", 
                                                "Extremely influential"))

table(professional.opportunities, Better.professional.opportunities.in.home.country)

#Better quality of life
better.quality.life<-as.factor(Better.quality.of.living.in.home.country)
better.quality.life <- factor(better.quality.life,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(better.quality.life, Better.quality.of.living.in.home.country)

#Feeling more comfortable at Home
home.comfort<-as.factor(Feeling.more.comfortable.at.home)
home.comfort <- factor(home.comfort,
                       levels = c(1,2,3,4,5),
                       labels = c("Not at all influential", 
                                  "Slightly influential", 
                                  "Somewhat influential", 
                                  "Very influential", 
                                  "Extremely influential"))

table(home.comfort,Feeling.more.comfortable.at.home)

#higher income level
higher.income <-as.factor(Higher.income.levels)
higher.income <- factor(higher.income,
                        levels = c(1,2,3,4,5),
                        labels = c("Not at all influential", 
                                   "Slightly influential",  
                                   "Somewhat influential", 
                                   "Very influential", 
                                   "Extremely influential"))

table(higher.income, Higher.income.levels)

#Family ties back home
family.ties<-as.factor(Family.ties.back.home)
family.ties <- factor(family.ties,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential", 
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(family.ties, Family.ties.back.home)

#Feelings of alienation
feelings.of.alienation <-as.factor(Feelings.of.alienation.from.the.Russian.culture.and.population)
feelings.of.alienation <- factor(feelings.of.alienation,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Not at all influential", 
                                            "Slightly influential", 
                                            "Somewhat influential", 
                                            "Very influential", 
                                            "Extremely influential"))

table(feelings.of.alienation, Feelings.of.alienation.from.the.Russian.culture.and.population)

#Difficulties in finding job
job.difficulties<-as.factor(Difficulties.in.finding.a.job)
job.difficulties <- factor(job.difficulties,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential", 
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(job.difficulties, Difficulties.in.finding.a.job)

#Poor working conditions
poor.work<-as.factor(Poor.working.conditions)
poor.work <- factor(poor.work,
                    levels = c(1,2,3,4,5),
                    labels = c("Not at all influential", 
                               "Slightly influential", 
                               "Somewhat influential", 
                               "Very influential", 
                               "Extremely influential"))

table(poor.work, Poor.working.conditions)

#Lower quality of life
low.life.quality <-as.factor(Lower.quality.of.life)
low.life.quality <- factor(low.life.quality,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential",  
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(low.life.quality, Lower.quality.of.life)

#Perceived or Experienced discrimination
discrimination<-as.factor(Perceived.or.experienced.discrimination)
discrimination <- factor(discrimination,
                         levels = c(1,2,3,4,5),
                         labels = c("Not at all influential", 
                                    "Slightly influential", 
                                    "Somewhat influential", 
                                    "Very influential", 
                                    "Extremely influential"))

table(discrimination, Perceived.or.experienced.discrimination)

#Crime and low level of safety
crime.safety<-as.factor(Crime.and.low.level.of.safety)
crime.safety <- factor(crime.safety,
                       levels = c(1,2,3,4,5),
                       labels = c("Not at all influential", 
                                  "Slightly influential", 
                                  "Somewhat influential", 
                                  "Very influential", 
                                  "Extremely influential"))

table(crime.safety, Crime.and.low.level.of.safety)


#Strict migration process
strict.migration<-as.factor(Strict.migration.process.difficulties.in.getting.visas.)
strict.migration <- factor(strict.migration,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential", 
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(strict.migration, Strict.migration.process.difficulties.in.getting.visas.)

#Factors influencing the decision to stay in Russia
HCReturnFactors<-data.frame(professional.opportunities,better.quality.life,home.comfort,
                         higher.income, family.ties,feelings.of.alienation, poor.work,
                         low.life.quality,discrimination,crime.safety,strict.migration)

HomeReturn_factors<-data.frame(world.region, degree, Better.professional.opportunities.in.home.country+
                                 Better.quality.of.living.in.home.country+Feeling.more.comfortable.at.home+
                                 Higher.income.levels+ Family.ties.back.home+
                                 Feelings.of.alienation.from.the.Russian.culture.and.population+ 
                                 Difficulties.in.finding.a.job+ Poor.working.conditions+
                                 Lower.quality.of.life+ Crime.and.low.level.of.safety+ 
                                 Strict.migration.process.difficulties.in.getting.visas.)


#Crosstabulation of Return home factors with Region and degree

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.professional.opportunities.in.home.country", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.quality.of.living.in.home.country", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.income.levels", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Family.ties.back.home", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Feelings.of.alienation.from.the.Russian.culture.and.population", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Difficulties.in.finding.a.job", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Poor.working.conditions", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.quality.of.life", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Crime.and.low.level.of.safety", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Strict.migration.process.difficulties.in.getting.visas.", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

#Graphs

#Factors influencing the decision to stay
plot_stackfrq(HCReturnFactors)

#Better professional opportunities
plot_grpfrq(professional.opportunities, world.region, geom.colors = "gs", title = "Better professional opportunities")
plot_grpfrq(professional.opportunities, degree, geom.colors = "gs", title = "Better professional opportunities")


#Better quality of life
plot_grpfrq(better.quality.life, world.region, geom.colors = "gs", title = "Better quality of life")
plot_grpfrq(better.quality.life, degree, geom.colors = "gs", title = "Better quality of life")

#Feeling more comfortable at Home
plot_grpfrq(home.comfort, world.region, geom.colors = "gs", title = "Feeling more comfortable at Home")
plot_grpfrq(home.comfort, degree, geom.colors = "gs", title = "Feeling more comfortable at Home")

#Higher income level
plot_grpfrq(higher.income, world.region, geom.colors = "gs", title = "Higher income level")
plot_grpfrq(higher.income, degree, geom.colors = "gs", title = "Higher income level")

#Family ties back home
plot_grpfrq(family.ties, world.region, geom.colors = "gs", title = "Family ties back home")
plot_grpfrq(family.ties, degree, geom.colors = "gs", title = "Family ties back home")

#Feelings of alienation
plot_grpfrq(feelings.of.alienation, world.region, geom.colors = "gs", title = "Feelings of alienation")
plot_grpfrq(feelings.of.alienation, degree, geom.colors = "gs", title = "Feelings of alienation")

#Difficulties in finding job
plot_grpfrq(job.difficulties, world.region, geom.colors = "gs", title = "Difficulties in finding jobs")
plot_grpfrq(job.difficulties, degree, geom.colors = "gs", title = "Difficulties in finding jobs")

#Poor working conditions
plot_grpfrq(poor.work, world.region, geom.colors = "gs", title = "Poor working conditions")
plot_grpfrq(poor.work, degree, geom.colors = "gs", title = "Poor working conditions")

#Lower quality of life
plot_grpfrq(low.life.quality, world.region, geom.colors = "gs", title = "Lower quality of life")
plot_grpfrq(low.life.quality, degree, geom.colors = "gs", title = "Lower quality of life")

#Perceived or Experienced discrimination
plot_grpfrq(discrimination, world.region, geom.colors = "gs", title = "Perceived or Experienced discrimination")
plot_grpfrq(discrimination, degree, geom.colors = "gs", title = "Perceived or Experienced discrimination")

#Crime and low level of safety
plot_grpfrq(crime.safety, world.region, geom.colors = "gs", title = "Crime and low level of safety")
plot_grpfrq(crime.safety, degree, geom.colors = "gs", title = "Crime and low level of safety")

#Strict migration process
plot_grpfrq(strict.migration, world.region, geom.colors = "gs", title = "Strict migration process")
plot_grpfrq(strict.migration, degree, geom.colors = "gs", title = "Strict migration process")


#Data preparation: Moving to another country

#Better job opportunities
better_job.opportunities <-as.factor(Better.job.opportunities)
better_job.opportunities <- factor(better_job.opportunities,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(better_job.opportunities, Better.job.opportunities)

#higher quality of life
high_quality.life<-as.factor(Higher.quality.of.life)
high_quality.life <- factor(high_quality.life,
                            levels = c(1,2,3,4,5),
                            labels = c("Not at all influential", 
                                       "Slightly influential", 
                                       "Somewhat influential", 
                                       "Very influential", 
                                       "Extremely influential"))

table(high_quality.life, Higher.quality.of.life)

#Better career opportunities
better.career<-as.factor(Better.career.opportunities.and.advancement.in.chosen.profession.1)
better.career <- factor(better.career,
                        levels = c(1,2,3,4,5),
                        labels = c("Not at all influential", 
                                   "Slightly influential", 
                                   "Somewhat influential", 
                                   "Very influential", 
                                   "Extremely influential"))

table(better.career, Better.career.opportunities.and.advancement.in.chosen.profession.1)

#higher income level
high.income <-as.factor(Higher.income.levels.1)
high.income <- factor(high.income,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential",  
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(high.income, Higher.income.levels.1)

#Family and Friends ties 
family_friends.ties<-as.factor(Ties.to.family.and.friends.1)
family_friends.ties <- factor(family_friends.ties,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(family_friends.ties, Ties.to.family.and.friends.1)

#Gain international experience
gain.experience<-as.factor(Gain.international.experience.1)
gain.experience <- factor(gain.experience,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(gain.experience, Gain.international.experience.1)

#Flexible immigration process 
flexible.immigration<-as.factor(Flexible.immigration.process)
flexible.immigration <- factor(flexible.immigration,
                               levels = c(1,2,3,4,5),
                               labels = c("Not at all influential", 
                                          "Slightly influential", 
                                          "Somewhat influential", 
                                          "Very influential", 
                                          "Extremely influential"))

table(flexible.immigration, Flexible.immigration.process)

#Moving to another country: Push factors in Russia
#Feelings of alienation
feeling.alienation <-as.factor(Feelings.of.alienation.from.the.Russian.culture.and.population.1)
feeling.alienation <- factor(feeling.alienation,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential", 
                                        "Slightly influential", 
                                        "Somewhat influential", 
                                        "Very influential", 
                                        "Extremely influential"))

table(feeling.alienation, Feelings.of.alienation.from.the.Russian.culture.and.population.1)

#Difficulties in finding job
finding.job<-as.factor(Difficulties.in.finding.a.job.1)
finding.job <- factor(finding.job,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential", 
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(finding.job, Difficulties.in.finding.a.job.1)

#Poor working conditions
work.conditions<-as.factor(Poor.working.conditions.1)
work.conditions <- factor(work.conditions,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(work.conditions, Poor.working.conditions.1)

#Lower quality of life
low.quality <-as.factor(Lower.quality.of.life.1)
low.quality <- factor(low.quality,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential",  
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(low.quality, Lower.quality.of.life.1)

#Perceived or Experienced discrimination
perceived.discrimination<-as.factor(Perceived.or.experienced.discrimination.1)
perceived.discrimination <- factor(perceived.discrimination,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(perceived.discrimination, Perceived.or.experienced.discrimination.1)

#Crime and low level of safety
crime.safety.levels<-as.factor(Crime.and.low.level.of.safety.1)
crime.safety.levels <- factor(crime.safety.levels,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(crime.safety.levels, Crime.and.low.level.of.safety.1)


#Strict migration process
strict.visa<-as.factor(Strict.migration.process.difficulties.in.getting.visas..1)
strict.visa <- factor(strict.visa,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential", 
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(strict.visa, Strict.migration.process.difficulties.in.getting.visas..1)


#Moving to another country: Push factors in home country
#family expectations
family_expectations<-as.factor(Family.expectations.1)
family_expectations <- factor(family_expectations,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential",
                                         "Extremely influential"))

table(family_expectations, Family.expectations.1)

#Availability of scholarship
restrictive.practices<-as.factor(Restrictive.cultural.practices..eg..pressure.to.marry..1)
restrictive.practices <- factor(restrictive.practices,
                                levels = c(1,2,3,4,5),
                                labels = c("Not at all influential", 
                                           "Slightly influential", 
                                           "Somewhat influential", 
                                           "Very influential", 
                                           "Extremely influential"))

table(restrictive.practices, Restrictive.cultural.practices..eg..pressure.to.marry..1)

#limited jobs
limited.jobs.opportunities<-as.factor(Limited.job.opportunities.in.home.country.1)
limited.jobs.opportunities <- factor(limited.jobs.opportunities,
                                     levels = c(1,2,3,4,5),
                                     labels = c("Not at all influential", 
                                                "Slightly influential", 
                                                "Somewhat influential", 
                                                "Very influential", 
                                                "Extremely influential"))

table(limited.jobs.opportunities, Limited.job.opportunities.in.home.country.1)

#lower income levels
low.income<-as.factor(Lower.income.levels.1)
low.income <- factor(low.income,
                     levels = c(1,2,3,4,5),
                     labels = c("Not at all influential", 
                                "Slightly influential", 
                                "Somewhat influential", 
                                "Very influential", 
                                "Extremely influential"))

table(low.income, Lower.income.levels.1)


#Lower quality of life
low_lifequality<-as.factor(Lower.quality.of.life.3)
low_lifequality <- factor(low_lifequality,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential",
                                     "Slightly influential",
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(low_lifequality, Lower.quality.of.life.3)


#Political persecution
political_persecution<-as.factor(Political.persecution.1)
political_persecution <- factor(political_persecution,
                                levels = c(1,2,3,4,5),
                                labels = c("Not at all influential", 
                                           "Slightly influential", 
                                           "Somewhat influential", 
                                           "Very influential", 
                                           "Extremely influential"))

table(political_persecution, Political.persecution.1)

#Dangers to ones own life
danger.to.life<-as.factor(Danger.or.fear.for.one.s.own.life.1)
danger.to.life <- factor(danger.to.life,
                         levels = c(1,2,3,4,5),
                         labels = c("Not at all influential", 
                                    "Slightly influential", 
                                    "Somewhat influential", 
                                    "Very influential", 
                                    "Extremely influential"))

table(danger.to.life, Danger.or.fear.for.one.s.own.life.1)

#Factors influencing the decision to stay in Russia
Move2CountryFactors<-data.frame(better_job.opportunities,high_quality.life,better.career,high.income, 
                                family_friends.ties,gain.experience, flexible.immigration,feeling.alienation,
                                finding.job,work.conditions,low.quality,perceived.discrimination, crime.safety.levels,
                                strict.visa, family_expectations, restrictive.practices, limited.jobs.opportunities,
                                low.income, low_lifequality, political_persecution, danger.to.life)

MoveCountry_factors<-data.frame(world.region, degree, Better.job.opportunities,Higher.quality.of.life,
                                Better.career.opportunities.and.advancement.in.chosen.profession.1,
                                Higher.income.levels.1,Ties.to.family.and.friends.1,Gain.international.experience.1,
                                Flexible.immigration.process,Feelings.of.alienation.from.the.Russian.culture.and.population.1,
                                Difficulties.in.finding.a.job.1,Poor.working.conditions.1,Lower.quality.of.life.1, 
                                Perceived.or.experienced.discrimination.1, Crime.and.low.level.of.safety.1,
                                Strict.migration.process.difficulties.in.getting.visas..1,Family.expectations.1,
                                Restrictive.cultural.practices..eg..pressure.to.marry..1,
                                Limited.job.opportunities.in.home.country.1, Lower.income.levels.1,
                                Lower.quality.of.life.3, Political.persecution.1, Danger.or.fear.for.one.s.own.life.1)


#Crosstabulation of Return home factors with Region and degree

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.job.opportunities", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.quality.of.life", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.career.opportunities.and.advancement.in.chosen.profession.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.income.levels.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Ties.to.family.and.friends.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)


crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Gain.international.experience.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Flexible.immigration.process", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Feelings.of.alienation.from.the.Russian.culture.and.population.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Difficulties.in.finding.a.job.1,Poor.working.conditions.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.quality.of.life.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Perceived.or.experienced.discrimination.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Crime.and.low.level.of.safety.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Strict.migration.process.difficulties.in.getting.visas..1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Restrictive.cultural.practices..eg..pressure.to.marry..1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Family.expectations.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Limited.job.opportunities.in.home.country.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.income.levels.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Lower.quality.of.life.3", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Political.persecution.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Danger.or.fear.for.one.s.own.life.1", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

#Graphs

#Factors influencing the decision to stay
plot_stackfrq(Move2CountryFactors)

#Better job opportunities
plot_grpfrq(better_job.opportunities, world.region, geom.colors = "gs", title = "Better job opportunities")
plot_grpfrq(better_job.opportunities, degree, geom.colors = "gs", title = "Better job opportunities")

#Higher quality of life
plot_grpfrq(high_quality.life, world.region, geom.colors = "gs", title = "Higher quality of life")
plot_grpfrq(high_quality.life, degree, geom.colors = "gs", title = "Higher quality of life")

#Better career opportunities
plot_grpfrq(better.career, world.region, geom.colors = "gs", title = "Better career opportunities")
plot_grpfrq(better.career, degree, geom.colors = "gs", title = "Better career opportunities")

#Higher income level
plot_grpfrq(high.income, world.region, geom.colors = "gs", title = "Higher income level")
plot_grpfrq(high.income, degree, geom.colors = "gs", title = "Higher income level")

#Family and Friends ties 
plot_grpfrq(family_friends.ties, world.region, geom.colors = "gs", title = "Family and Friends ties")
plot_grpfrq(family_friends.ties, degree, geom.colors = "gs", title = "Family and Friends ties")

#Gain international experience
plot_grpfrq(gain.experience, world.region, geom.colors = "gs", title = "Gain international experience")
plot_grpfrq(gain.experience, degree, geom.colors = "gs", title = "Gain international experience")

#Flexible immigration process 
plot_grpfrq(flexible.immigration, world.region, geom.colors = "gs", title = "Flexible immigration process ")
plot_grpfrq(flexible.immigration, degree, geom.colors = "gs", title = "Flexible immigration process")

#Feelings of alienation
plot_grpfrq(feeling.alienation, world.region, geom.colors = "gs", title = "Feelings of alienation")
plot_grpfrq(feeling.alienation, degree, geom.colors = "gs", title = "Feelings of alienation")

#Difficulties in finding job
plot_grpfrq(finding.job, world.region, geom.colors = "gs", title = "Difficulties in finding jobs")
plot_grpfrq(finding.job, degree, geom.colors = "gs", title = "Difficulties in finding jobs")

#Poor working conditions
plot_grpfrq(work.conditions, world.region, geom.colors = "gs", title = "Poor working conditions")
plot_grpfrq(work.conditions, degree, geom.colors = "gs", title = "Poor working conditions")

#Lower quality of life
plot_grpfrq(low.quality, world.region, geom.colors = "gs", title = "Lower quality of life")
plot_grpfrq(low.quality, degree, geom.colors = "gs", title = "Lower quality of life")

#Perceived or Experienced discrimination
plot_grpfrq(perceived.discrimination, world.region, geom.colors = "gs", title = "Perceived or Experienced discrimination")
plot_grpfrq(perceived.discrimination, degree, geom.colors = "gs", title = "Perceived or Experienced discrimination")

#Crime and low level of safety
plot_grpfrq(crime.safety.levels, world.region, geom.colors = "gs", title = "Crime and low level of safety")
plot_grpfrq(crime.safety.levels, degree, geom.colors = "gs", title = "Crime and low level of safety")

#Strict migration process
plot_grpfrq(strict.visa, world.region, geom.colors = "gs", title = "Strict migration process")
plot_grpfrq(strict.visa, degree, geom.colors = "gs", title = "Strict migration process")

#Family expectations
plot_grpfrq(family_expectations, world.region, geom.colors = "gs", title = "Family expectations")
plot_grpfrq(job.opportunities, degree, geom.colors = "gs", title = "Family expectations")

#Restrictive Cultural practices
plot_grpfrq(restrictive.practices, world.region, geom.colors = "gs", title = "Restrictive Cultural practices")
plot_grpfrq(restrictive.practices, degree, geom.colors = "gs", title = "Restrictive Cultural practices")

#Limited jobs
plot_grpfrq(limited.jobs.opportunities, world.region, geom.colors = "gs", title = "Limited jobs opportunities")
plot_grpfrq(limited.jobs.opportunities, degree, geom.colors = "gs", title = "Limited jobs opportunities")

#Lower income levels
plot_grpfrq(low.income, world.region, geom.colors = "gs", title = "Lower income levels")
plot_grpfrq(low.income, degree, geom.colors = "gs", title = "Lower income levels")

#Lower quality of life
plot_grpfrq(low_lifequality, world.region, geom.colors = "gs", title = "Lower quality of life")
plot_grpfrq(low_lifequality, degree, geom.colors = "gs", title = "Lower quality of life")

#Political persecution
plot_grpfrq(political_persecution, world.region, geom.colors = "gs", title = "Political persecution")
plot_grpfrq(political_persecution, degree, geom.colors = "gs", title = "Political persecution")

#Dangers to ones own life
plot_grpfrq(danger.to.life, world.region, geom.colors = "gs", title = "Dangers to ones own life")
plot_grpfrq(danger.to.life, degree, geom.colors = "gs", title = "Dangers to ones own life")
