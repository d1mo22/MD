0
#  READING CREDSCO.TXT
setwd("d:/downloads/MD/MD")

# Load hotel sample dataset
dd <- read.table("hotel_sample_5000.csv", header=T, sep=",")

dim(dd)

names(dd)
attach(dd)
sapply(dd, class)

summary(dd)

summary(canceled)
#table(canceled,useNA="ifany" )
#which(is.na(dd$canceled))
#dd<-dd[-3310,]

# Convert response to numeric (Bernoulli coding)
dd$canceled <- as.numeric(dd$canceled)

#Build the response binary according to bernoulli coding: 1=Target event
#DictamenBern<-Dictamen
#DictamenBern[DictamenBern == 0 ] <- NA
#DictamenBern[DictamenBern == 2 ] <- 0
#table(DictamenBern)

names(dd)

#build logistic regression
model1 <- glm(canceled ~ lead_time + week_nights + adults +
                deposit_type + customer_type + adr,
              family = binomial, data = dd)

summary(model1)

#relevel permet decidir quin nivell volem de referència pel terme independent
# (Teacher used Tipo.trabajo → now customer_type)
typeWork <- model.matrix(~ customer_type - 1, data = dd)
typeWork
head(typeWork)
head(customer_type)

class(typeWork)

barplot(table(customer_type))
summary(typeWork)

barplot(colSums(typeWork))
barplot(colSums(typeWork), cex.names=0.9, las=2)
x <- barplot(colSums(typeWork), cex.names=0.9, xaxt="n")
text(x=x, y=0, labels=colnames(typeWork), adj=1, xpd=TRUE, srt=20)

# build model 2 with all dummy variables
model2 = glm(canceled ~ lead_time + week_nights + adults +
               deposit_type + adr + typeWork,
             family = binomial, data = dd)
summary(model2)
head(typeWork)

# remove one dummy (avoid multicollinearity)
model2 = glm(canceled ~ lead_time + week_nights + adults +
               deposit_type + adr + typeWork[,-ncol(typeWork)],
             family = binomial, data = dd)
summary(model2)

# try selecting specific dummy columns (similar to teacher example)
model2 = glm(canceled ~ lead_time + week_nights + adults +
               deposit_type + adr + typeWork[,2:4],
             family = binomial, data = dd)
summary(model2)

model2$rank
model2$family
model2$method

model2$deviance
model2$null.deviance
deltadev <- model2$null.deviance - model2$deviance
deltadev

model2$aic

confint(model2)

step(model2)

n <- dim(dd)[1]

learn <- sample(1:n, round(0.67*n))

# lengths of variables (teacher style checks)
length(lead_time)
length(week_nights)
length(adults)

dim(dd[learn,])

length(lead_time[learn])
length(week_nights[learn])
length(adults[learn])

# train set model
model2 = glm(canceled[learn] ~ lead_time[learn] + week_nights[learn] +
               adults[learn] + deposit_type[learn] + adr[learn] +
               typeWork[learn, 2:4],
             family = binomial, data = dd[learn,])
summary(model2)

# full model again (teacher routine)
model2 = glm(canceled ~ lead_time + week_nights + adults +
               deposit_type + adr + typeWork[,-ncol(typeWork)],
             family = binomial, data = dd)
summary(model2)

attributes(model2)
model2$coefficients

exp(model2$coefficients)

# select dummy columns (teacher style)
indexos <- c(2,4)
model2 = glm(canceled ~ lead_time + week_nights + adults +
               deposit_type + adr + typeWork[,indexos],
             family = binomial, data = dd)
summary(model2)
exp(model2$coefficients)

# plots (teacher style)
plot(model2$linear.predictors, model2$fitted.values)
plot(model2)

anova(model2, test="Chisq")

anova(model1, model2, test="Chisq")
anova(step(model2), test="Chisq")
