train = read.csv("train.csv",stringsAsFactors = F)
test = read.csv("test.csv",stringsAsFactors = F)


library(mice)
library(Rcpp)
library(lattice)
library(ggplot2)
library(dplyr)

train$Age[is.na(train$Age)] = mean(train$Age, na.rm = T)
train1 = train

train1$Title = gsub('(.*,)|(\\..*)','',train1$Name)
table(train1$Title)


train2 = train1

drare <- c('Don','Lady','the Countess','Capt','Col','Dr','Major','Rev','Sir','Jonkheer')

train2$Title = gsub('(.*, )|(\\..*)','',train1$Name)
train2$Title[train2$Title == "Ms"] = "Miss"
train2$Title[train2$Title == "Mlle"] = "Miss"
train2$Title[train2$Title == "Mme"] = "Mrs"
train2$Title[train2$Title %in% drare] = "Rare_Title"

train3 = subset(train2, select = -c(Cabin, Parch,Ticket))

#Logistic Regression
survlog = glm(Survived ~ Pclass + Sex + Age + SibSp + Embarked + Title, family = "binomial", data = train3)

test1 = test
test1$Title = gsub('(.*, )|(\\..*)','',test$Name)
test1 = subset(test1, select = -c(Parch, Ticket, Cabin))
test1$Title[test1$Title == "Ms"] = "Miss"
drare1 = c(drare, "Dona")
test1$Title[test1$Title %in% drare1] = "Rare_Title"

predsurvlog = predict(survlog, newdata = test1, type = "response")


#Decision Tree

library(rpart)
library(rpart.plot)

survCart = rpart(Survived ~ Pclass + Sex + Age + SibSp + Embarked + Title, data = train3, method = "class")

predCa = predict(survCart, newdata = test1, type = "class")
submissionCart = data.frame(PassengerId = test1$PassengerId, Survived = predCa)

ggplot(train3, aes(x = Pclass, y = Survived)) + geom_point()
ggplot(train3, aes(x = Age, y = Sex)) + geom_point(aes(color = Survived))
ggplot(train3, aes(x = Age, y = Title)) + geom_point(aes(color = Survived))
ggplot(train3, aes(x = SibSp, y = Title)) + geom_point(aes(color = Survived))
ggplot(train3, aes(x = SibSp, y = Age)) + geom_point(aes(color = Survived))


surnCart = rpart(Survived~ Pclass + Sex + Age + SibSp + Embarked + Title, data = train3, method = "class")
prp(surnCart)
predCa = predict(surnCart, newdata = test1)[,2]
submissionCart = data.frame(PassengerId = test1$PassengerId, Survived = predCa)

submissionCart$Survived[submissionCart$Survived > 0.400] = 1
submissionCart$Survived[submissionCart$Survived <= 0.400] = 0
write.csv(submissionCart, "submissionCART.csv", row.names = FALSE)

#Random forest
library(randomForest)
train3$Survived = as.factor(train3$Survived)
train3$Title = as.factor(train3$Title)
surnrf = randomForest(Survived ~ Pclass + Sex + Age + Embarked + Title, data = train3)

predrf = predict(surnrf, newdata = test1)[,2]

test1$Survived = as.factor(test1$Survived)
test1$Title = as.factor(test1$Title)

predrf = predict(surnrf, newdata = test1, type = "prob")[,2]

submissionRF = data.frame(PassengerId = test1$PassengerId, Survived = predrf)
submissionRF$Survived[submissionRF$Survived > 0.330] = 1
submissionRF$Survived[submissionRF$Survived <= 0.330] = 0

write.csv(submissionRF, "submissionRF1.csv", row.names = FALSE)

#Cart resubmission

ggplot(train3, aes(x = Age, y = Embarked)) + geom_point(aes(color = Survived))
test1$Embarked[is.na(test1$Embarked)] = ""

train3$Child[train3$Age < 18] <- 'Child'
train3$Child[train3$Age >= 18] <- 'Adult'
test1$Child[test1$Age < 18] <- 'Child'
test1$Child[test1$Age >= 18] <- 'Adult'

train3$Child <- as.factor(train3$Child)

train3$Embarked[train3$PassengerId == '62'] = "C"
train3$Embarked[train3$PassengerId == '830'] = "C"

test1$Fare[153] <- 8.05

train3$Mother <- 'Not Mother'
test1$Mother <- 'Not Mother'
train3$Mother[train3$Sex == 'female' & train3$Parch > 0 & train3$Age > 18 & train3$Title != 'Miss'] <- 'Mother'
test1$Mother[test1$Sex == 'female' & test1$Parch > 0 & test1$Age > 18 & test1$Title != 'Miss'] <- 'Mother'
newrandom <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Fare + Parch + Embarked + Title + Size + Child + Mother, data = train3)
train3$Mother <- as.factor(train3$Mother)
test1$Mother <- as.factor(test1$Mother)

importance <- importance(newrandom)
varImportance <- data.frame(Variables = row.names(importance),Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% mutate(Rank = paste0('#', dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables,Importance), y = Importance, fill = Importance)) + geom_bar(stat = 'identity') + geom_text(aes(x = Variables, y = 0.5, label = Rank), colour = 'red') + coord_flip()

prediction <- predict(newrandom, test1)
solution <- data.frame(PassengerID = test1$PassengerId, Survived = prediction)
write.csv(solution, "solution_rf.csv", row.names = F)
