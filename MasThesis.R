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

#attaching the data
data<-read.csv("International Students Surveyr.csv")

attach(data)

##FACTORS INFLUENCING STUDENTS DECISION TO STUDY IN RUSSIA

#Data preparation: Demographic information

#Age
table(Age)

summary(as.numeric(Age))

data$age<-recode(as.numeric(Age),"17:21=1; 22:26=2;27:hi=3")

table(data$age)

data$age<-factor(data$age,lab=c("17 to 21 yrs", "22 to 26 yrs", " 27 yrs and older")) 

#Degree
data$degree<-What.degree.are.you.currently.studying.for.

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
              data$Home.country == 'Uzbekistan'] <- 'Commonwealth of Independent States'


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
PushFactors <-data.frame(language.of.instruction, degree,
                         age, Gender, world.region, Home.country,
                         Unavailability.of.the.desired.study.program,Low.quality.of.education
                         ,Competitive.University.admission.process
                         ,Perceived.advantage.of.international.degree
                         ,Unavailability.of.scholarship.opportunities
                         ,Encouragement.from.my.family.to.study.abroad,Encouragement.from..my.friends.to.study.abroad
                         ,Better.earning.prospects.abroad, The.social.prestige.of.studying.abroad
                         ,To.experience.a.different.culture)

#exploratory factor analysis to allow for indexing.

#principal component analysis
pushpc <- princomp(~Unavailability.of.the.desired.study.program+Low.quality.of.education
                   +Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution
                   +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                   +Unavailability.of.scholarship.opportunities
                   +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                   +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                   +To.experience.a.different.culture, data = PushFactors, cor = FALSE, na.action = na.omit) 

summary(pushpc)

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
                      +Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution
                      +Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market
                      +Unavailability.of.scholarship.opportunities
                      +Encouragement.from.my.family.to.study.abroad+Encouragement.from..my.friends.to.study.abroad
                      +Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad
                      +To.experience.a.different.culture, 
                      factors = 4, data = PushFactors, cor = FALSE, na.action = na.omit)

print(push.efa2, digits=2, cutoff=.3, sort=TRUE)

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
advantages.of.studying.abroad<-(Better.earning.prospects.abroad+The.social.prestige.of.studying.abroad)/2
table(advantages.of.studying.abroad)
PushFactors$benefits.of.studying.abroad<-recode(advantages.of.studying.abroad, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PushFactors$benefits.of.studying.abroad)

#access to education
cor.test(Unavailability.of.the.desired.study.program,Low.quality.of.education)
access.to.education<-(Unavailability.of.the.desired.study.program+Low.quality.of.education)/2
table(access.to.education)
PushFactors$access.to.education<-recode(access.to.education, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
table(PushFactors$access.to.education)

attach(PushFactors)

#checking for cronbach's aplha to establish reliability

PushFactorsHC <-data.frame(access.to.education, Competitive.University.admission.process
                           ,Perceived.advantage.of.international.degree
                           ,Unavailability.of.scholarship.opportunities
                           ,encouragement.from.family.friends
                           ,advantages.of.studying.abroad
                           ,To.experience.a.different.culture)


psych::alpha(PushFactorsHC)

#Regression analysis
#Push factors in Home country that influenced the decision of international students to study in Russia

#Empty model
model0<-lm(as.numeric(language.of.instruction)~1, data = PushFactors)

summary(model0)

#Full Model
model1<-lm(as.numeric(language.of.instruction)~encouragement.from.family.friends+
             benefits.of.studying.abroad+access.to.education+
             Competitive.University.admission.process+Perceived.advantage.of.international.degree+
             Unavailability.of.scholarship.opportunities+To.experience.a.different.culture, data = PushFactors)

summary(model1)

#Full Model and controls
model2<-lm(as.numeric(language.of.instruction)~encouragement.from.family.friends+
             benefits.of.studying.abroad+access.to.education+
             Competitive.University.admission.process+Perceived.advantage.of.international.degree+
             Unavailability.of.scholarship.opportunities+To.experience.a.different.culture+as.numeric(age)+
             as.numeric(Home.country)+as.numeric(Gender), data = PushFactors)

summary(model2)

#Full Model with interaction effect
model3<-lm(as.numeric(language.of.instruction)~benefits.of.studying.abroad+
             Unavailability.of.scholarship.opportunities+Competitive.University.admission.process+
             (access.to.education+encouragement.from.family.friends+Perceived.advantage.of.international.degree+
                To.experience.a.different.culture)*world.region, data = PushFactors)

summary(model3)

#Full Model and control with interaction effects
model4<-lm(as.numeric(language.of.instruction)~benefits.of.studying.abroad+
             Unavailability.of.scholarship.opportunities+Competitive.University.admission.process+
             (access.to.education+encouragement.from.family.friends+Perceived.advantage.of.international.degree+
                To.experience.a.different.culture)*world.region+as.numeric(age)+as.numeric(Gender), data = PushFactors)

summary(model4)

#Graph for the models
stargazer(model1, model2, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out="a.doc",
          covariate.labels =c("Constant","Encouragement from family and friends","Benefits of studying abroad",
                              "Access to education", "Competitive university admission process",
                              "Perceieved advantage of an international degree",
                              "Unavailability of scholarship opportunities", "Experience a different culture",
                              "Age", "Home Country", "Gender"))

stargazer(model3, model4, title="Regression Results: Push Factors in Home Country", align=TRUE,column.sep.width = "-5pt",
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out="b.doc",
          covariate.labels =c("Constant","Benefits of studying abroad","Unavailability of scholarship opportunities",
                             "Competitive university admission process","Access to education",
                             "Encouragement from family and friends","Perceieved advantage of an international degree",
                             "Experience a different culture","Asia", "CIS", "Europe", "Middle East", "South America",
                             "Age", "Gender",  "Access to education*Asia", "Access to education*CIS", "Access to education*Europe",
                             "Access to education*Middle East", "Access to education*South America","Encouragement from family and friends*Asia",
                             "Encouragement from family and friends*CIS", "Encouragement from family and friends*Europe",
                             "Encouragement from family and friends*Middle East","Encouragement from family and friends*South America",
                             "Perceieved advantage of an international degree*Asia","Perceieved advantage of an international degree*CIS", 
                             "Perceieved advantage of an international degree*Europe","Perceieved advantage of an international degree*Middle East",
                             "Perceieved advantage of an international degree*South America","Experience a different culture*Asia",
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
employment.prospect<-(Graduates.employment.rates+Local.employers.preference.of..degrees.awarded.by.HSE)/2
employment.prospect<-round(employment.prospect,2)
table(employment.prospect)
PullFactors$employment.prospect<-recode(employment.prospect, "1=1; 1.5:2=2; 2.5:3=3; 3.5:4=4; 4.5:5=5")
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
                             HSE.reputation, Attraction.to.Russian.culture..society,Career.prospects.in.Russia,
                             Recognition.of.the.degree.in.my.home.country,Recommendations.from.family.friends,
                             HSE.position.in.international.university.rankings,Support.services.for.international.students,
                             HSE.s.international.strategic.alliances,employment.prospect)
psych::alpha(PullFactorsRuHSE)

#Full model 
model5<-lm(as.numeric(language.of.instruction)~employment.prospect+Career.prospects.in.Russia+proximity+
             cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
             Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
             Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
             Support.services.for.international.students+HSE.s.international.strategic.alliances, 
           data = PullFactors)

summary(model5)

#Full model with controls
model6a<-lm(as.numeric(language.of.instruction)~employment.prospect+Career.prospects.in.Russia+
              proximity+cost.of.living+scholarship+HSE.quality+HSE.reputation+program.choice+
              Recommendations.from.family.friends+Attraction.to.Russian.culture..society+
              Recognition.of.the.degree.in.my.home.country+HSE.position.in.international.university.rankings+
              Support.services.for.international.students+HSE.s.international.strategic.alliances+
              as.numeric(age)+as.numeric(Home.country)+as.numeric(Gender), data = PullFactors)

summary(model6a)

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
             Support.services.for.international.students+Career.prospects.in.Russia+
             Attraction.to.Russian.culture..society+(Recommendations.from.family.friends+proximity+cost.of.living+ 
                program.choice+HSE.position.in.international.university.rankings+
                HSE.s.international.strategic.alliances)*world.region, data = PullFactors)

summary(model7)

model7a<-lm(as.numeric(language.of.instruction)~employment.prospect+scholarship+
             HSE.quality+HSE.reputation+Recognition.of.the.degree.in.my.home.country+
             Support.services.for.international.students+Career.prospects.in.Russia+
             Attraction.to.Russian.culture..society+
              (Recommendations.from.family.friends+proximity+cost.of.living+program.choice+
                 HSE.position.in.international.university.rankings+
                 HSE.s.international.strategic.alliances)*world.region, data = PullFactors)

summary(model7a)

#Fullmodel and controls with interaction effect
model8<-lm(as.numeric(language.of.instruction)~employment.prospect+scholarship+
             HSE.quality+HSE.reputation+Recognition.of.the.degree.in.my.home.country+
             Support.services.for.international.students+Attraction.to.Russian.culture..society+
             (Recommendations.from.family.friends+proximity+cost.of.living+program.choice+
                HSE.position.in.international.university.rankings+HSE.s.international.strategic.alliances)*world.region+
             as.numeric(age)+as.numeric(Gender), data = PullFactors)

summary(model8)

#Graph for the models
stargazer(model5, model6, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow",out = "c.doc",
          covariate.labels =c("Constant","Employment prospects","Proximity","Cost of living", "Scholarship",
                              "HSE quality", "HSE reputation", "Program choice","Recommendations from family and friends",
                              "Attraction to Russian culture", "Recognition of HSE degree in my Home Country",
                              "HSE position in University rankings","Support services for International Students",
                              "HSE international stragtegic alliances","Age", "Home Country", "Gender"))

stargazer(model7, model8, title="Regression Results: Push Factors in Home Country", align=TRUE,
          no.space=TRUE, single.row=T,type = "text", intercept.bottom = F,dep.var.labels = "Move to Moscow", out = "d.doc",
          covariate.labels =c("Constant","Employment prospects","Scholarship","HSE quality","HSE reputation",
                              "Recognition of HSE degree in my Home Country","Support services for International Students",
                              "Attraction to Russian culture", "Recommendations from family and friends","Proximity",
                              "Cost of living", "Choice of program", "HSE position in University rankings",
                              "HSE international alliances","Asia", "CIS","Europe", "Middle East", "South America", 
                              "Age", "Gender", "Recomendations from family and friends*Asia","Recomendations from family and friends*CIS", 
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

theme_set(theme_bw())
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

#unavailable program
unavailable.program <-as.factor(Unavailability.of.the.desired.study.program)
unavailable.program <- factor(unavailable.program,levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(unavailable.program, Unavailability.of.the.desired.study.program)

#quality of education
low.educational.quality<-as.factor(Low.quality.of.education)
low.educational.quality <- factor(low.educational.quality,
                                  levels = c(1,2,3,4,5),
                                  labels = c("Not at all influential", 
                                             "Slightly influential", 
                                             "Somewhat influential", 
                                             "Very influential", 
                                             "Extremely influential"))

table(low.educational.quality, Low.quality.of.education)

#competitive University admission in home country
competitive.admission<-as.factor(Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution)
competitive.admission <- factor(competitive.admission,
                                levels = c(1,2,3,4,5),
                                labels = c("Not at all influential", 
                                           "Slightly influential", 
                                           "Somewhat influential", 
                                           "Very influential", 
                                           "Extremely influential"))

table(competitive.admission, Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution)

#Advantage of international degree
advantage.of.international.degree<-as.factor(Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market)
advantage.of.international.degree <- factor(advantage.of.international.degree,
                                            levels = c(1,2,3,4,5),
                                            labels = c("Not at all influential", 
                                                       "Slightly influential", 
                                                       "Somewhat influential", 
                                                       "Very influential", 
                                                       "Extremely influential"))

table(advantage.of.international.degree, Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market)

#unavailability of scholarships
unavailability.of.scholarship<-as.factor(Unavailability.of.scholarship.opportunities)
unavailability.of.scholarship <- factor(unavailability.of.scholarship,
                                        levels = c(1,2,3,4,5),
                                        labels = c("Not at all influential", 
                                                   "Slightly influential", 
                                                   "Somewhat influential", 
                                                   "Very influential", 
                                                   "Extremely influential"))

table(unavailability.of.scholarship, Unavailability.of.scholarship.opportunities)

#encouragement from family
encouragement.from.family<-as.factor(Encouragement.from.my.family.to.study.abroad)
encouragement.from.family <- factor(encouragement.from.family,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential", 
                                               "Slightly influential", 
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(encouragement.from.family, Encouragement.from.my.family.to.study.abroad)

#encouragement from friends
encouragement.from.friends<-as.factor(Encouragement.from..my.friends.to.study.abroad)
encouragement.from.friends <- factor(encouragement.from.friends,
                                     levels = c(1,2,3,4,5),
                                     labels = c("Not at all influential", 
                                                "Slightly influential", 
                                                "Somewhat influential", 
                                                "Very influential", 
                                                "Extremely influential"))

table(encouragement.from.friends, Encouragement.from..my.friends.to.study.abroad)

#better earning prospects
better.earning.prospects<-as.factor(Better.earning.prospects.abroad)
better.earning.prospects <- factor(better.earning.prospects,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(better.earning.prospects, Better.earning.prospects.abroad)

#social prestige
social.prestige<-as.factor(The.social.prestige.of.studying.abroad)
social.prestige <- factor(social.prestige,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(social.prestige, The.social.prestige.of.studying.abroad)

#experience different culture
experience.different.culture<-as.factor(To.experience.a.different.culture)
experience.different.culture <- factor(experience.different.culture,
                                       levels = c(1,2,3,4,5),
                                       labels = c("Not at all influential", 
                                                  "Slightly influential", 
                                                  "Somewhat influential", 
                                                  "Very influential", 
                                                  "Extremely influential"))

table(experience.different.culture, To.experience.a.different.culture)

#Descriptive Statistics: Push Factors influencing the decision to move to Russia

#push factors
pushfactor<-data.frame(unavailable.program,low.educational.quality,competitive.admission,
                       advantage.of.international.degree,unavailability.of.scholarship,
                       encouragement.from.family,encouragement.from.friends,better.earning.prospects,
                       social.prestige,experience.different.culture)

freq(pushfactor, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

HCpushfactor<-data.frame(degree, world.region, unavailable.program,low.educational.quality,competitive.admission,
                         advantage.of.international.degree,unavailability.of.scholarship,
                         encouragement.from.family,encouragement.from.friends,better.earning.prospects,
                         social.prestige,experience.different.culture)

#Crosstab between Push Factors and World Region with degree

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Unavailability.of.the.desired.study.program", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)


crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Low.quality.of.education", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Competitive.university.admission.process..difficult.to.gain.admission.to.a.quality.local.institution", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Perceived.advantage.of.international.degree.over.a.local.one.at.the.local.job.market", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Unavailability.of.scholarship.opportunities", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Encouragement.from.my.family.to.study.abroad", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Encouragement.from..my.friends.to.study.abroad", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Better.earning.prospects.abroad", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="The.social.prestige.of.studying.abroad", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="To.experience.a.different.culture", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

is.factor(world.region)
#Anova
#Unavailable program
data$region<-as.factor(world.region)
attach(data)

leveneTest(Unavailability.of.the.desired.study.program ~ region, data = data)
res.aov <- aov(Unavailability.of.the.desired.study.program ~ region, data = data)

# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)
summary(glht(res.aov, linfct = mcp(region = "Tukey")))

#Graphs


#Plots for the push factors in the home countries

#Push factors
plot_stackfrq(pushfactor)

#Unavailability of desired study program
plot_grpfrq(unavailable.program, world.region, geom.colors = "gs", title = "Unavailability of desired study program in Home Country")
plot_grpfrq(unavailable.program, degree, geom.colors = "gs", title = "Unavailability of Program")

#Low quality of education
plot_grpfrq(low.educational.quality, world.region, geom.colors = "gs", title = "Low quality of education in Home Country")
plot_grpfrq(low.educational.quality, degree, geom.colors = "gs", title = "Low quality of education")

#Competitive univeristy admission
plot_grpfrq(competitive.admission, world.region, geom.colors = "gs", title = "Competitive University admission process in Home Country")
plot_grpfrq(competitive.admission, degree, geom.colors = "gs", title = "Competitive university admission")

#Perceived advantage of international degree
plot_grpfrq(advantage.of.international.degree, world.region, geom.colors = "gs", 
            title = "Perceived advantage of international degree than a local degree")

plot_grpfrq(advantage.of.international.degree, degree, geom.colors = "gs", 
            title = "Perceived advantage of internatioal degree")

#Unavailability of Scholarship
plot_grpfrq(unavailability.of.scholarship, world.region, geom.colors = "gs", title = "Unavailability of scholarship opportunities in Home Country")
plot_grpfrq(unavailability.of.scholarship, degree, geom.colors = "gs", title = "Unavailability of scholarship")


#Encouragement from family
plot_grpfrq(encouragement.from.family, world.region, geom.colors = "gs", title = "Encouragement from family")
plot_grpfrq(encouragement.from.family, degree, geom.colors = "gs", title = "Encouragement from family")

#Encouragement from friends
plot_grpfrq(encouragement.from.friends, world.region, geom.colors = "gs", title = "Encouragement from friends")
plot_grpfrq(encouragement.from.friends, degree, geom.colors = "gs", title = "Encouragement from friends")

#Better earning prospects
plot_grpfrq(better.earning.prospects, world.region, geom.colors = "gs", title = "Better earning prospects abroad")
plot_grpfrq(better.earning.prospects, degree, geom.colors = "gs", title = "Better earning prospects")

#The social prestige of studying abroad
plot_grpfrq(social.prestige, world.region, geom.colors = "gs", title = "The social prestige of studying abroad")
plot_grpfrq(social.prestige, degree, geom.colors = "gs", title = "The.social.prestige.of.studying.abroad")

#Experience different culture
plot_grpfrq(experience.different.culture, world.region, geom.colors = "gs", title = "EXperience different culture abroad")
plot_grpfrq(experience.different.culture, degree, geom.colors = "gs", title = "EXperience different culture")


#Data preparation: Pull factors in Russia influencing the decision to study in Russia

#availability of desired program
available.study.program <-as.factor(Availability.of.desired.study.program)
available.study.program <- factor(available.study.program,
                                  levels = c(1,2,3,4,5),
                                  labels = c("Not at all influential", 
                                             "Slightly influential", 
                                             "Somewhat influential", 
                                             "Very influential", 
                                             "Extremely influential"))

table(available.study.program, Availability.of.desired.study.program)

#high quality of education
high.educational.quality<-as.factor(Higher.quality.of.education..compared.to.home.country.)
high.educational.quality <- factor(high.educational.quality,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(high.educational.quality, Higher.quality.of.education..compared.to.home.country.)

#low cost of living
low.cost.living<-as.factor(Low.cost.of.living)
low.cost.living <- factor(low.cost.living,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(low.cost.living,Low.cost.of.living)

#low tuition fees
low.tuition<-as.factor(Low.tuition.fees)
low.tuition <- factor(low.tuition,
                      levels = c(1,2,3,4,5),
                      labels = c("Not at all influential", 
                                 "Slightly influential",  
                                 "Somewhat influential", 
                                 "Very influential", 
                                 "Extremely influential"))

table(low.tuition, Low.tuition.fees)

#Awarded scholarships
scholarship.tuitionwaiver<-as.factor(Awarded.scholarships.or.tuition.waiver)
scholarship.tuitionwaiver <- factor(scholarship.tuitionwaiver,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential", 
                                               "Slightly influential", 
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(scholarship.tuitionwaiver,Awarded.scholarships.or.tuition.waiver)

#Attraction to Russian culture
russian.culture<-as.factor(Attraction.to.Russian.culture..society)
russian.culture <- factor(russian.culture,
                          levels = c(1,2,3,4,5),
                          labels = c("Not at all influential", 
                                     "Slightly influential", 
                                     "Somewhat influential", 
                                     "Very influential", 
                                     "Extremely influential"))

table(russian.culture, Attraction.to.Russian.culture..society)

#Career prospects in Russia
career.prospects<-as.factor(Career.prospects.in.Russia)
career.prospects <- factor(career.prospects,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential", 
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(career.prospects, Career.prospects.in.Russia)

#Recommendations from family and friends
family.friends.recommendations<-as.factor(Personal.recommendations.from.parents..relatives..and.friends)
family.friends.recommendations <- factor(family.friends.recommendations,
                                         levels = c(1,2,3,4,5),
                                         labels = c("Not at all influential", 
                                                    "Slightly influential", 
                                                    "Somewhat influential", 
                                                    "Very influential", 
                                                    "Extremely influential"))

table(family.friends.recommendations, Personal.recommendations.from.parents..relatives..and.friends)

#Cultural Proximity
cultural.proximity<-as.factor(cultural.proximity.with.home)
cultural.proximity <- factor(cultural.proximity,
                             levels = c(1,2,3,4,5),
                             labels = c("Not at all influential", 
                                        "Slightly influential", 
                                        "Somewhat influential", 
                                        "Very influential", 
                                        "Extremely influential"))

table(cultural.proximity, cultural.proximity.with.home)

#Geographical proximity
geographical.proximity<-as.factor(geographical.proximity.with.home)
geographical.proximity <- factor(geographical.proximity,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Not at all influential", 
                                            "Slightly influential", 
                                            "Somewhat influential", 
                                            "Very influential", 
                                            "Extremely influential"))

table(geographical.proximity, geographical.proximity.with.home)

#Descriptive Statistics: Pull Factors in Russia influencing the decision to move to Russia
Pullfactor_Russia<-data.frame(available.study.program, high.educational.quality, low.cost.living, 
                              low.tuition, scholarship.tuitionwaiver, russian.culture, career.prospects,
                              family.friends.recommendations, cultural.proximity, geographical.proximity)

freq(Pullfactor_Russia, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

Pullfactor_Ru<-data.frame(world.region, degree, available.study.program, high.educational.quality, low.cost.living, 
                          low.tuition, scholarship.tuitionwaiver, russian.culture, career.prospects,
                          family.friends.recommendations, cultural.proximity, geographical.proximity)

#Crosstab between Pull Factors in Russia and World Region with degree

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Unavailability.of.the.desired.study.program", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Higher.quality.of.education..compared.to.home.country.", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Low.cost.of.living", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Availability.of.scholarships", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Attraction.to.Russian.culture..society", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Career.prospects.in.Russia", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Personal.recommendations.from.parents..relatives..and.friends", 
         type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="cultural.proximity.with.home", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="geographical.proximity.with.home", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

#Plots for the pull factors in Russia
plot_stackfrq(Pullfactor_Russia)

#Availability of desired study program
plot_grpfrq(available.study.program, world.region, geom.colors = "gs", title = "Availability of desired study program in Russia")
plot_grpfrq(available.study.program, degree, geom.colors = "gs", title = "Availability of desired study program")

#High quality of education
plot_grpfrq(high.educational.quality, world.region, geom.colors = "gs", title = "High quality of education in Russia")
plot_grpfrq(high.educational.quality, degree, geom.colors = "gs", title = "High quality of education")

#Low cost of living
plot_grpfrq(low.cost.living, world.region, geom.colors = "gs", title = "Low cost of living in Russia")
plot_grpfrq(low.cost.living, degree, geom.colors = "gs", title = "Low cost of living")

#Low tuition fees
plot_grpfrq(low.tuition, world.region, geom.colors = "gs", title = "Low tuition fees in Russia")
plot_grpfrq(low.tuition, degree, geom.colors = "gs", title = "Low tuition fees")

#Scholarship or tuition waiver
plot_grpfrq(scholarship.tuitionwaiver, world.region, geom.colors = "gs", title = "Scholarship or tuition waiver")
plot_grpfrq(scholarship.tuitionwaiver, degree, geom.colors = "gs", title = "Scholarship or tuition waiver")

#Attraction to Russian culture
plot_grpfrq(russian.culture, world.region, geom.colors = "gs", title = "Attraction to Russian culture")
plot_grpfrq(russian.culture, degree, geom.colors = "gs", title = "Attraction to Russian culture")

#Career prospects in Russia
plot_grpfrq(career.prospects, world.region, geom.colors = "gs", title = "Career prospects in Russia")
plot_grpfrq(career.prospects, degree, geom.colors = "gs", title = "Career prospects in Russia")

#Recommendations from family and friends
plot_grpfrq(family.friends.recommendations, world.region, geom.colors = "gs", title = "Recommendations from family and friends")
plot_grpfrq(family.friends.recommendations, degree, geom.colors = "gs", title = "Recommendations from family and friends")

#cultural.proximity
plot_grpfrq(cultural.proximity, world.region, geom.colors = "gs", title = "Home Country's cultural proximity to Russia")
plot_grpfrq(cultural.proximity, degree, geom.colors = "gs", title = "Cultural proximity")

#Geograhical proximity
plot_grpfrq(geographical.proximity, world.region, geom.colors = "gs", title = "Home Country's geograhical proximity to Russia")
plot_grpfrq(geographical.proximity, degree, geom.colors = "gs", title = "Geograhical proximity")


#Data preparation: Pull factors in HSE

#Quality and Reputation of HSE
HSE.qualityandreputation <-as.factor(Quality.and.reputation.of.the.University)
HSE.qualityandreputation <- factor(HSE.qualityandreputation,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Not at all influential", 
                                              "Slightly influential", 
                                              "Somewhat influential", 
                                              "Very influential", 
                                              "Extremely influential"))

table(HSE.qualityandreputation, Quality.and.reputation.of.the.University)

#Recognition of HSE degree
recognition.of.HSE.degree<-as.factor(Recognition.of.the.degree.in.my.home.country)
recognition.of.HSE.degree <- factor(recognition.of.HSE.degree,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Not at all influential", 
                                               "Slightly influential", 
                                               "Somewhat influential", 
                                               "Very influential", 
                                               "Extremely influential"))

table(recognition.of.HSE.degree, Recognition.of.the.degree.in.my.home.country)

#Quality of teaching staff
quality.teachers<-as.factor(Quality.of.the.teaching.staff)
quality.teachers <- factor(quality.teachers,
                           levels = c(1,2,3,4,5),
                           labels = c("Not at all influential", 
                                      "Slightly influential", 
                                      "Somewhat influential", 
                                      "Very influential", 
                                      "Extremely influential"))

table(quality.teachers,Quality.of.the.teaching.staff)

#Reputation of the alumni
alumni.reputation<-as.factor(The.reputation.of.the.alumni)
alumni.reputation <- factor(alumni.reputation,
                            levels = c(1,2,3,4,5),
                            labels = c("Not at all influential", 
                                       "Slightly influential",  
                                       "Somewhat influential", 
                                       "Very influential", 
                                       "Extremely influential"))

table(alumni.reputation, The.reputation.of.the.alumni)

#reputation of the international community
internationalcommunity.reputation<-as.factor(The.reputation.of.the.international.community)
internationalcommunity.reputation <- factor(internationalcommunity.reputation,
                                            levels = c(1,2,3,4,5),
                                            labels = c("Not at all influential", 
                                                       "Slightly influential", 
                                                       "Somewhat influential", 
                                                       "Very influential", 
                                                       "Extremely influential"))

table(internationalcommunity.reputation,The.reputation.of.the.international.community)

#HSE rank
HSE.rank<-as.factor(HSE.position.in.international.university.rankings)
HSE.rank <- factor(HSE.rank,
                   levels = c(1,2,3,4,5),
                   labels = c("Not at all influential",
                              "Slightly influential",
                              "Somewhat influential",
                              "Very influential",
                              "Extremely influential"))

table(HSE.rank, HSE.position.in.international.university.rankings)

#Cost of tuition
tuition.cost<-as.factor(Cost.of.tuition.for.international.students)
tuition.cost <- factor(tuition.cost,
                       levels = c(1,2,3,4,5),
                       labels = c("Not at all influential", 
                                  "Slightly influential", 
                                  "Somewhat influential", 
                                  "Very influential",
                                  "Extremely influential"))

table(tuition.cost, Cost.of.tuition.for.international.students)

#Availability of scholarship
available.scholarships<-as.factor(Availability.of.scholarships)
available.scholarships <- factor(available.scholarships,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Not at all influential", 
                                            "Slightly influential", 
                                            "Somewhat influential", 
                                            "Very influential", 
                                            "Extremely influential"))

table(available.scholarships,Availability.of.scholarships)

#Support Services for international students
international.students.support<-as.factor(Support.services.for.international.students)
international.students.support <- factor(international.students.support,
                                         levels = c(1,2,3,4,5),
                                         labels = c("Not at all influential", 
                                                    "Slightly influential", 
                                                    "Somewhat influential", 
                                                    "Very influential", 
                                                    "Extremely influential"))

table(international.students.support, Support.services.for.international.students)

#Graduate employment rate
graduate.employment<-as.factor(Graduates.employment.rates)
graduate.employment <- factor(graduate.employment,
                              levels = c(1,2,3,4,5),
                              labels = c("Not at all influential", 
                                         "Slightly influential", 
                                         "Somewhat influential", 
                                         "Very influential", 
                                         "Extremely influential"))

table(graduate.employment, Graduates.employment.rates)


#HSE international strategic alliances
HSE.alliances<-as.factor(HSE.s.international.strategic.alliances)
HSE.alliances <- factor(HSE.alliances,
                        levels = c(1,2,3,4,5),
                        labels = c("Not at all influential",
                                   "Slightly influential",
                                   "Somewhat influential", 
                                   "Very influential", 
                                   "Extremely influential"))

table(HSE.alliances, HSE.s.international.strategic.alliances)

#Local Employers preference for HSE degrees
employers.preference.for.HSE.degrees<-as.factor(Local.employers.preference.of..degrees.awarded.by.HSE)
employers.preference.for.HSE.degrees <- factor(employers.preference.for.HSE.degrees,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Not at all influential", 
                                                          "Slightly influential", 
                                                          "Somewhat influential", 
                                                          "Very influential", 
                                                          "Extremely influential"))

table(employers.preference.for.HSE.degrees, Local.employers.preference.of..degrees.awarded.by.HSE)

#Descriptive Statistics: Pull Factors in HSE influencing the decision to move to Russia
Pullfactor_HSE<-data.frame(HSE.qualityandreputation,recognition.of.HSE.degree,quality.teachers,
                           alumni.reputation,internationalcommunity.reputation,HSE.rank,tuition.cost,
                           available.scholarships,international.students.support,graduate.employment,
                           HSE.alliances,employers.preference.for.HSE.degrees)

freq(Pullfactor_HSE, display.type = F, report.nas = F, headings = T, cumul = F, style = "grid")

HSEPullfactor<-data.frame(world.region, degree, HSE.qualityandreputation,recognition.of.HSE.degree,quality.teachers,
                           alumni.reputation,internationalcommunity.reputation,HSE.rank,tuition.cost,
                           available.scholarships,international.students.support,graduate.employment,
                           HSE.alliances,employers.preference.for.HSE.degrees)

#Crosstab between Pull Factors in HSE and World Region with degree
crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Quality.and.reputation.of.the.University", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Recognition.of.the.degree.in.my.home.country", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Quality.of.the.teaching.staff", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="The.reputation.of.the.alumni", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="The.reputation.of.the.international.community", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="HSE.position.in.international.university.rankings", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Cost.of.tuition.for.international.students", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Availability.of.scholarships", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Support.services.for.international.students", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Graduates.employment.rates", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="HSE.s.international.strategic.alliances", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)

crosstab(data, row.vars = c("world.region","degree"), 
         col.vars ="Local.employers.preference.of..degrees.awarded.by.HSE", type = c("f", "t"), style = "long", 
         addmargins = T, dec.places = 2, subtotals = T)


#Plots for the pull factors in HSE
plot_stackfrq(Pullfactor_HSE)

#Quality and reputation of HSE
plot_grpfrq(HSE.qualityandreputation, world.region, geom.colors = "gs", title = "Quality and reputation of HSE")
plot_grpfrq(HSE.qualityandreputation, degree, geom.colors = "gs", title = "Quality and reputation of HSE")

#Recognition of degree
plot_grpfrq(recognition.of.HSE.degree, world.region, geom.colors = "gs", title = "Recognition of HSE degree in my Home Country")
plot_grpfrq(recognition.of.HSE.degree, degree, geom.colors = "gs", title = "Recognition of degree")

#Quality of teachers
plot_grpfrq(quality.teachers, world.region, geom.colors = "gs", title = "Quality of HSE teachers")
plot_grpfrq(quality.teachers, degree, geom.colors = "gs", title = "Quality of teachers")

#Reputation of alumni
plot_grpfrq(alumni.reputation, world.region, geom.colors = "gs", title = "Reputation of HSE alumni")
plot_grpfrq(alumni.reputation, degree, geom.colors = "gs", title = "Reputation of alumni")

#Reputation of International Community
plot_grpfrq(internationalcommunity.reputation, world.region, geom.colors = "gs", title = "Reputation of HSE International Community")
plot_grpfrq(internationalcommunity.reputation, degree, geom.colors = "gs", title = "Reputation of International Community")

#HSE's rank
plot_grpfrq(HSE.rank, world.region, geom.colors = "gs", title = "HSE's position on World Universities ranking")
plot_grpfrq(HSE.rank, degree, geom.colors = "gs", title = "HSE's rank")

#Cost of tuition
plot_grpfrq(tuition.cost, world.region, geom.colors = "gs", title = "Cost of tuition in HSE")
plot_grpfrq(tuition.cost, degree, geom.colors = "gs", title = "Cost of tuition")

#Recognition of degree
plot_grpfrq(available.scholarships, world.region, geom.colors = "gs", title = "Availability of scholarships in HSE")
plot_grpfrq(available.scholarships, degree, geom.colors = "gs", title = "Availability of scholarships")

#Support services for international students
plot_grpfrq(international.students.support, world.region, geom.colors = "gs", title = "Support services for international students in HSE")
plot_grpfrq(international.students.support, degree, geom.colors = "gs", title = "Support services for international students")

#Graduate employment rate
plot_grpfrq(graduate.employment, world.region, geom.colors = "gs", title = "HSE's Graduate employment rate")
plot_grpfrq(graduate.employment, degree, geom.colors = "gs", title = "Graduate employment rate")

#HSE alliances
plot_grpfrq(HSE.alliances, world.region, geom.colors = "gs", title = "HSE strategic international alliances")
plot_grpfrq(HSE.alliances, degree, geom.colors = "gs", title = "HSE alliances")

#Recognition of degree
plot_grpfrq(employers.preference.for.HSE.degrees, world.region, geom.colors = "gs", title = "Local employers preference for HSE degrees")
plot_grpfrq(employers.preference.for.HSE.degrees, degree, geom.colors = "gs", title = "Local preference for HSE degrees")


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
