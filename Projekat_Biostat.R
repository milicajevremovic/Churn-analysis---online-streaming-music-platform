library(ggplot2)

#ucitavanje podataka
data <- read.csv("abcChurn.csv")

#opis podataka
str(data)

# izbacivanje identifikatora
data$X <- NULL
data$id <- NULL

# kategorije
unique(data$metod_placanja)
unique(data$metod_registrovanja)
length(unique(data$clansto))
length(unique(data$cena_izabranog_plana))
length(unique(data$uplaceno))
length(unique(data$grad))

# prebacivanje u faktore
data$metod_placanja <- as.factor(data$metod_placanja) 
data$metod_registrovanja <- as.factor(data$metod_registrovanja)
data$clansto <- as.factor(data$clansto)
data$autoProduzetak <- as.factor(data$autoProduzetak)
data$churn <- as.factor(data$churn)

ggplot(data = data, mapping = aes(x = clansto)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~clansto,data = data))

ggplot(data = data, mapping = aes(x = pol)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~pol,data = data))

ggplot(data = data, mapping = aes(x = grad)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~grad,data = data))

ggplot(data = data, mapping = aes(x = metod_registrovanja)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~metod_registrovanja,data = data))

ggplot(data = data, mapping = aes(x = autoProduzetak)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~autoProduzetak,data = data))

ggplot(data = data, mapping = aes(x = churn)) + 
  geom_histogram(bins = 30,stat = "count")
sort(xtabs(~churn,data = data))

str(data)

summary(data)

numerics <- c(2:7,8,10,16,17)

#korelacija izmedju numerickih
corr.matrix <- cor(data[,numerics])
library(corrplot)
corrplot.mixed(corr.matrix, number.cex = 0.75, tl.cex = 0.57)

apply(X = data[,numerics],
      MARGIN = 2,
      FUN = shapiro.test)

# izbacivanje uplaceno jer ima istu informativnu moc kao izbor plana
data$uplaceno <- NULL

# izbacivanje datuma zbog jer trenutno nije koristan za analizu
data$datum_registrovanja <- NULL
data$datumIsticanjaClanstva <- NULL
data$datumLogovanja <- NULL
data$datumTransakcije <- NULL

# izbacivanje clanstva zbog disbalansa klasa
data$clansto$NULL

apply(X = data,
      MARGIN = 2,
      FUN = function(x)length(which(is.na(x))))

library(caret)
library(rpart)

# kreiranje trening i test skupa podataka
train.indices <- createDataPartition(data$churn,p =  0.75,list = F)
train.set <- data[train.indices,]
test.set <- data[-train.indices,]

# treniranje inicijalnog drveta odlucivanja
tree1 <- rpart(churn~.,
               data = train.set, 
               method = "class")

tree1.pred <- predict(tree1,
                      newdata = test.set,
                      type = "class")

tree1.cm <- table(true = test.set$churn, pred = tree1.pred)
tree1.cm


compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[2,2] # true positive
  TN <- cmatrix[1,1] # true negative
  FP <- cmatrix[1,2] # false positive
  FN <- cmatrix[2,1] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

# novo drvo sa ogranicenjima za kompleksnost i minimalan broj za grananje
tree2 <- rpart(churn ~ ., data = train.set, method = "class",
               control = rpart.control(minsplit = 10, cp = 0.001))

tree2.pred <- predict(tree2, newdata = test.set, type = "class")


tree2.cm <- table(true=test.set$churn, pred=tree2.pred)
tree2.cm


tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval

data.frame(rbind(tree1.eval, tree2.eval), 
           row.names = c("tree 1", "tree 2"))

library(e1071)


numFolds = trainControl( method = "cv", number = 10 )


cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.001)) 

# odredjianje najboljih parametara cross validacijom
dt.cv <- train(churn ~ ., 
               data = train.set, 
               method = "rpart", 
               control = rpart.control(minsplit = 10), 
               trControl = numFolds, 
               tuneGrid = cpGrid)
dt.cv


plot(dt.cv)

tree3 <- prune(tree2, cp = 0.041)


print(tree3)


tree3.pred <- predict(tree3, newdata = test.set, type = "class")

tree3.cm <- table(true = test.set$churn, predicted = tree3.pred)
tree3.cm

tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval

data.frame(rbind(tree1.eval, tree2.eval, tree3.eval),
           row.names = c(paste("tree", 1:3)))
### drvo odlucivanja acc = 0.951, recall = 0.4897

install.packages("randomForest")
library(randomForest)

rf <- randomForest(churn ~ ., data = train.set)

rf.pred <- predict(rf, newdata = test.set)

rf.cm <- table(true=test.set$churn, pred=rf.pred)
rf.cm

# acc = 0.952 reall = 0.494
rf.eval <- compute.eval.metrics(rf.cm)
rf.eval

#### Analiza prezivljavanja ####

install.packages(c("survival", "survminer"))

library("survminer")
library("survival")
datas <- data
head(datas)

sfit<-(survfit(Surv(clansto, churn)~ 1, data = datas))
ggsurvplot(sfit,
           conf.int = TRUE,
           risk.table.col = "strata", #menjanje boje po grupama
           ggtheme = theme_bw(), # promeni ggplot2 temu
           palette = c("#E7B800","#2E9FDF"),
           xlim = c(0,600))

summary(sfit)$table

fit <- survfit(Surv(clansto, churn) ~ autoProduzetak, data = datas)
print(fit)

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)
# Interpetacija Kaplan Majer plot-a
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table.col = "strata", 
           linetype = "strata",
           surv.median.line = "hv",
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF")
)
# plot za kracenje krive uz xlim
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table.col = "strata", 
           linetype = "strata",
           surv.median.line = "hv",
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF"),
           xlim = c(0,600)
)
# mediana vremena za svaku grupu prezivljavanja
summary(fit)$table

#plot kumulativa
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", 
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF"),
           fun = "event"
)

survdiff <- survdiff(Surv(clansto, churn) ~ autoProduzetak, data = datas)
survdiff
##Dobijamo rezultat da nam je log rank test za razliku u prezivljavanju daje p-vrednost od p = 2e-16. 

fit <- coxph(Surv(clansto, churn) ~ autoProduzetak, data = datas)
fit



