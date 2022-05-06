framingham <- na.omit(framingham)

randombeans <- sample(1:3658, 3200)
training <- framingham[randombeans,]
testing <- framingham[-randombeans,]


ggplot(framingham, aes(x = cigsPerDay)) + geom_histogram(colour = 4, fill = "white", bins = 10) + 
  labs(title = "Number of Cigarettes Smoked by Framingham Residents Daily", x = "Cigarettes Smoked Daily", y = "Number of Observations")

ggplot(framingham, aes(x = totChol)) + geom_histogram(colour = 4, fill = "white", bins = 50) + 
  labs(title = "Cholesterol  of Framingham Residents ", x = "Cholesterol Level", y = "Number of Observations")


ggplot(framingham, aes(x = glucose)) + geom_histogram(colour = 4, fill = "white", bins = 80) + 
  labs( x = "Glucose Level", y = "Number of Observations")

model <-glm(TenYearCHD ~ totChol +
               male + 
               age + 
               education + 
               currentSmoker + 
               cigsPerDay + 
               BPMeds + 
               prevalentStroke + 
               prevalentHyp + 
               diabetes + 
               sysBP + 
               diaBP + 
               BMI + 
               heartRate + 
               glucose 
             , training, family = "binomial")
summary(model)
library(tab)
tabglm(model)

testpredictions <- predict(model, testing, type = "response")
mean(round(testpredictions) == testing$TenYearCHD)


  stat_sig_model = glm(TenYearCHD ~ male +
                         age
                          + totChol +
                         cigsPerDay +
                         sysBP +
                         glucose
                       , training, family = "binomial")
  summary(stat_sig_model)
  
  testpredictions_sig <- predict(stat_sig_model, testing, type = "response")
  mean(round(testpredictions_sig) == testing$TenYearCHD)

men = subset(framingham, male == "1")
women = subset(framingham, male == "0")




male_model <- glm(TenYearCHD ~ totChol +
                            age + 
                            education + 
                            currentSmoker + 
                            cigsPerDay + 
                            BPMeds + 
                            prevalentStroke + 
                            prevalentHyp + 
                            diabetes + 
                            sysBP + 
                            diaBP + 
                            BMI + 
                            heartRate + 
                            glucose 
                          , men, family = "binomial")
summary(male_model)


female_model <- glm(TenYearCHD ~ totChol +
                    age + 
                    education + 
                    currentSmoker + 
                    cigsPerDay + 
                    BPMeds + 
                    prevalentStroke + 
                    prevalentHyp + 
                    diabetes + 
                    sysBP + 
                    diaBP + 
                    BMI + 
                    heartRate + 
                    glucose 
                  , women, family = "binomial") 
summary(female_model)

t.test(men$TenYearCHD, women$TenYearCHD )

meds = subset(framingham, BPMeds == "1")
nomeds = subset(framingham, BPMeds == "0")
t.test(meds$TenYearCHD, nomeds$TenYearCHD )


library(ggplot2)

ggplot(framingham, aes(x = glucose, color = TenYearCHD)) + geom_density(size = 1)
ggplot(framingham, aes(x = cigsPerDay, color = TenYearCHD)) + geom_density(size = 1)




library(rpart)
library(randomForest)
framingham <- na.omit(framingham)

chd_dum <- as.factor(framingham$TenYearCHD)
summary(chd_dum)


chd_mod.rf <- randomForest(chd_dum ~ male +
                             age
                           + totChol +
                             cigsPerDay +
                             sysBP +
                             glucose, data = framingham, mtry = 2,
                         importance = TRUE, na.action = na.omit)
rf_model_predictions <- predict(chd_mod.rf, framingham)

print(chd_mod.rf)
plot(chd_mod.rf)
varImpPlot(chd_mod.rf, main = "Variable Importance Plot For Certain Factors and their Impact on CHD" )




library(rpart)
# The rpart.plot library plots trees!
library(rpart.plot)
rp.model <- rpart(TenYearCHD ~ male +
                 age + BMI
               + totChol +
                 cigsPerDay +
                 sysBP +
                 glucose, framingham)

rpart.plot(rp.model, digits = 4)




library(caTools)
split <- sample.split(framingham, SplitRatio = 0.7)

train_cl <- subset(framingham, split == "TRUE")
test_cl <- subset(framingham, split == "FALSE")

train_scale <- scale(train_cl[c(1, 2, 3, 5, 10, 11, 15)])
test_scale <-  scale(test_cl[c(1, 2, 3, 5, 10, 11, 15)])
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$TenYearCHD,
                      k = 19)

misClassError <- mean(classifier_knn != test_cl$TenYearCHD)
print(paste('Accuracy =', 1-misClassError))



