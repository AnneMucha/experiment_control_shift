library(lme4)
library(MASS)
library(ggplot2)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr) 
library(car)

datei1 <- "results_rawFin_anon_clean_fin_2b.csv"
str(datei1)
summary(datei1)
dat1 <- read.csv(datei1, header=TRUE, sep=";", dec=",")

#Datei anzeigen
summary(dat1)
str(dat1)
View(dat1)

d <- filter(dat1, Exp=="cs")
#Datentyp für Faktoren festlegen
d$Item <- factor(d$Item)
d$Condition <- factor(d$Condition)

d$Fact1 <- plyr::revalue(d$Condition, c("1"="att", "2"="att", "3"="nonAtt", "4"="nonAtt"))
d$Fact2 <- plyr::revalue(d$Condition, c("1"="active", "2"="passive", "3"="active",  "4"="passive"))

summary(d)
str(d)
saveRDS(d,"d.RDS") # saves an RDS file of d-Date 

# control shift responses in multiple choice study:
#logistic regression mixed effects model w/ random intercepts for ppt + item:
mdlshift1 <- glmer(Shift ~ Fact2 + Fact1 + Fact2:Fact1 + (1|ID) + (1|Item),
                  data = d, family = 'binomial', control=glmerControl(optimizer="bobyqa"))

summary(mdlshift1)

#logistic regression mixed effects model w/ random intercepts for ppt + item + by item random slopes for voice:
mdlshift2 <- glmer(Shift ~ Fact2 + Fact1 + Fact2:Fact1 + (1|ID) + (1+Fact2|Item),
                             data = d, family = 'binomial', control=glmerControl(optimizer="bobyqa"))

# Problem: 0 control shift-Antworten in der nonatt/active-Bedingung ("separation") 

install.packages("effects")
library(effects)

plot(allEffects(mdlshift2))

# Paket f?r Firth logistic regression
install.packages("logistf")
library(logistf)

mod1 <- logistf(Shift ~ Fact2 + Fact1 + Fact1:Fact2, data = d)
summary(mod1)
mod2 <- logistf(Shift ~ Fact2 + Fact1, data = d) # ohne Interaktion
summary(mod2)
anova(mod1, mod2) 

confint(mod1)

plot(Effect(c("Fact1", "Fact2"), mod1), multiline = T, ci.style = "bars")

# Mit nicht-transformierter y-Achse (hier sieht man, dass die Schaetzung bei dem Modell ganz ok ist)
plot(Effect(c("Fact1", "Fact2"), mod1), multiline = T, ci.style = "bars", type = "response")

citation("logistf")


# zusaetzlich: Effekt von Verbtyp in den Passiv-Bedingungen testen:
str(d)
d.pass <- filter(d, Fact2=="passive") # subsetting for passive items

str(d.pass)

# Modell nur mit random intercepts
mdl.pass <- glmer(Shift ~ Fact1 + (1|ID) + (1|Item),
                     data = d.pass, family = 'binomial', control=glmerControl(optimizer="bobyqa"))

summary(mdl.pass)

# Konfidenzintervalle fuer Vergleich der Passiv-Bedingungen (Wald ist die einzige Methode, die problemlos durchlauft)
confint.merMod(mdl.pass, method="Wald") 

#zus?tzlich by-item random slopes -> im SuB-Papier berichtet
mdl.pass2 <- glmer(Shift ~ Fact1 + (1 |ID) + (1 + Fact1 |Item),
                               data = d.pass, family = 'binomial', control=glmerControl(optimizer="bobyqa"))

summary(mdl.pass2)
confint.merMod(mdl.pass2, method="Wald")

anova(mdl.pass, mdl.pass2) # Modellvergleich mit vs. ohne random slopes


#---------------------------
# split control responses in multiple choice study:

#logistic regression mixed effects model w/ random intercepts for ppt + item:
mdlsplit1 <- glmer(Split ~ Fact2 + Fact1 + Fact2:Fact1 + (1|ID) + (1|Item),
                   data = d, family = 'binomial', control=glmerControl(optimizer="bobyqa"))

summary(mdlsplit1)


# testing effect of verb type on split responses -> im SuB-Papier berichtet (Fussnote)

mdlsplit1.verbtest <- glmer(Split ~ Fact1 + (1|ID) + (1|Item),
                      data = d, family = 'binomial', control=glmerControl(optimizer="bobyqa")) 

summary(mdlsplit1.verbtest) 
confint(mdlsplit1.verbtest, method = "Wald")

#-------------------------------
# Rating study

# fuer die deskriptive Statistik (Tabelle im SuB-Papier)
tapply(d$Rating1,list(d$Fact1,d$Fact2),mean) 
tapply(d$Rating1,list(d$Fact1,d$Fact2),sd)  

# lmem w/ FEs verb type + voice + interaction, REs: random intercepts by ppt + item, random by ppt slopes for verb type + voice, by item random slopes for voice
mdl.ratings1 <- lmer(Rating1 ~ Fact1 + Fact2 + Fact1:Fact2 + (1 + Fact1 + Fact2 |ID) + (1 + Fact2|Item), data = d, REML = FALSE) 

summary(mdl.ratings1)
confint.merMod(mdl.ratings1)

# removing random slopes for verb type
mdl.ratings1b <- lmer(Rating1 ~ Fact1 + Fact2 + Fact1:Fact2 + (1 + Fact2 |ID) + (1 + Fact2|Item), data = d, REML = FALSE) 

# random slopes fuerr beide Faktoren -> im SuB-Papier berichtet
mdl.ratings1c <- lmer(Rating1 ~ Fact1 + Fact2 + Fact1:Fact2 + (1 + Fact1 + Fact2 |ID) + (1 + Fact1 + Fact2|Item), data = d, REML = FALSE) 

#comparing models
anova(mdl.ratings1, mdl.ratings1b) # Modell 1 mit random slopes *
anova(mdl.ratings1c, mdl.ratings1) # 1c ***

summary(mdl.ratings1c)
confint(mdl.ratings1c)

# testen, ob verb type in den Passivbedingungen einen Unterschied macht
mdl.ratings.pass <- lmer(Rating1 ~ Fact1 + (1|ID) + (1|Item), data = d.pass, REML = FALSE) 

summary(mdl.ratings.pass)
confint.merMod(mdl.ratings.pass) 

# Modell mit random slopes fuerr Items (wie in dem logit-Modell fuerr die multiple choice Daten) --> im SuB-Paper berichtet
mdl.ratings.pass2 <- lmer(Rating1 ~ Fact1 + (1 |ID) + (1 + Fact1|Item), data = d.pass, REML = FALSE) 

anova(mdl.ratings.pass,mdl.ratings.pass2) # *** --> signifikant besserer model fit mit by-item random slopes

summary(mdl.ratings.pass2)
confint.merMod(mdl.ratings.pass2)

# by-ptp random slopes 
mdl.ratings.pass3 <- lmer(Rating1 ~ Fact1 + (1 + Fact1|ID) + (1 + Fact1|Item), data = d.pass, REML = FALSE) 
  
anova(mdl.ratings.pass2,mdl.ratings.pass3) # kein Unterschied, Ergebnisse von mdl.ratings.pass2 berichtet


q()
y



