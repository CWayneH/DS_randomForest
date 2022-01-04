library(randomForest)
library(ggplot2)
library(caret)
library(ggpubr)
library(Metrics)
library(ggpubr)
library(corrplot)



# read parameters
# trailingOnly = TRUE means the first parameter start with 
# the actual parameter, which pass into R file
args = commandArgs(trailingOnly=TRUE)

# parse parameters
if (length(args)==0){
  stop("USAGE: Rscript randomForest.R with no arguments", call.=FALSE)
}

i <- 1
while(i < length(args)){
  if(args[i] == "--fold")
    fold <- args[i + 1]
  else if(args[i] == "--train")
    train_input <- args[i + 1]
  else if(args[i] == "--test")
    test_input <- args[i + 1]
  else if(args[i] == "--report")
    report_output <- args[i + 1]
  else if(args[i] == "--predict")
    predict_output <- args[i + 1]
  i <- i + 1
}

# read train_input & test_input 
# train_data <- read.csv(file = train_input, header = T, stringsAsFactors = F)
# test_data <- read.csv(file = test_input, header = T, stringsAsFactors = F)

fold <- 5
train_data <- read.csv(file = "ds_final/train_salary.csv", header = T, stringsAsFactors = F)
test_data <- read.csv(file = "ds_final/test_salary.csv", header = T, stringsAsFactors = F)

# ================== add IsTrain for merge two dataset ===================
train_data$IsTrain <- TRUE
test_data$IsTrain <- FALSE

# ================================ merge =================================
merge <- rbind(train_data, test_data)

# ============================ rename feature ============================
merge <- merge[-1]
colnames(merge) <- c("x1", "x2", "x3", "x4", "x5", 
                     "x6", "x7", "x8", "y", "x10",
                     "x11", "x12", "x13", "x14", "x15",
                     "x16", "x17", "x18", "x19", "x20",
                     "x21", "x22", "x23", "x24", "x25",
                     "x26", "x27", "x28", "IsTrain")

# = deal with NA and NULL value, let them be median(num) and mode(char) =
merge[is.na(merge$x26), "x26"] <- median(merge$x26, na.rm = T)

# ====================== split into train and test ======================
train_data <- merge[merge$IsTrain == T, ]
test_data <- merge[merge$IsTrain == F, ]

# remove outliers

# ==================== convert char feature to factor ====================
train_data[sapply(train_data, is.character)] <- lapply(train_data
                                                       [sapply(train_data, 
                                                               is.character)], 
                                                       as.factor)


# ================= select feature through visualization =================

trn_data <- c()
trn_data$x4 <- train_data$x4
trn_data$x6 <- train_data$x6
trn_data$x7 <- train_data$x7
trn_data$x10 <- train_data$x10
trn_data$x11 <- train_data$x11
trn_data$x17 <- train_data$x17
trn_data$x18 <- train_data$x18
trn_data$x19 <- train_data$x19
trn_data$x20 <- train_data$x20
trn_data$x21 <- train_data$x21
trn_data$x22 <- train_data$x22
trn_data$x23 <- train_data$x23
trn_data$x24 <- train_data$x24
trn_data$x25 <- train_data$x25
trn_data$x26 <- train_data$x26

trn_data$y <- train_data$y
trn_data <- as.data.frame(trn_data)
corrplot(cor(trn_data),
         method = "square",
         type = "lower" # show only lower side
)

# ========================= transfer to formula ==========================

interest <- as.formula(y ~ x4 + x6 + x7 + x10 + x11)


# ================ analysis data through visualization and
#                                        remove outliers =================

ggscatter(train_data, x = "x4", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "x4", ylab = "salary")

train_data <- subset(train_data, 
                     !(train_data$x4 < 3e+06 & train_data$y > 1500000))

train_data <- subset(train_data,
                     !(train_data$x4 > 4e+06 & train_data$y < 1000000))
#-----------------------------------------------------------------------

ggscatter(train_data, x = "x6", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "x6", ylab = "salary")

train_data <- subset(train_data,
                     !(train_data$x6 > 60 & train_data$y < 200000))

train_data <- subset(train_data,
                     !(train_data$x6 < 30 & train_data$y > 750000))
#-----------------------------------------------------------------------

ggscatter(train_data, x = "x7", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "x7", ylab = "salary")


train_data <- subset(train_data,
                     !(train_data$x7 < 10 & train_data$y > 6e+05))


train_data <- subset(train_data,
                     !(train_data$x7 > 30 & train_data$y < 1.8e+05))


#-----------------------------------------------------------------------
ggscatter(train_data, x = "x10", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "x10", ylab = "salary")

train_data <- subset(train_data,
                     !(train_data$x10 > 1e+06 & train_data$y < 3e+05))

#-----------------------------------------------------------------------

ggscatter(train_data, x = "x11", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "x11", ylab = "salary")

train_data <- subset(train_data,
                     !(train_data$x11 > 500000 & train_data$y < 500000))

# =========================== add ID into data ===========================
ID <- c(1:nrow(train_data))
data <- cbind(ID, train_data)

# ============================== fold times ==============================
k <- as.numeric(fold)

# ========================== random id for split =========================
set.seed(1234)
data$gp <- runif(dim(data)[1])
split_train <- 0.6
split_test <- 0.2 + split_train

# ============================= null vector ==============================
set <- c()
training <- c()
validation <- c()
test <- c()

# ======================= k-fold cross validation ========================
for(i in 1 : k){
  
  
  # ========= select and split train_data, test_data, valid_data =========
  train_d <- subset(data, data$gp <= split_train)   # <= 0.6
  test_d <- subset(data, data$gp > split_train)     # > 0.6
  test_d <- subset(test_d, test_d$gp <= split_test)    # > 0.6, <= 0.8
  valid_d <- subset(data, data$gp > split_train) 
  
  
  # ================= set random seed to reproduce model =================
  set.seed(4312)
  
  # ========================== construct model ===========================
  model <- randomForest(formula = interest, data = train_data, ntree = 50,
                        importance = T)
  
  # =============== predict using model applied to train_d ===============
  pred_train <- predict(model, newdata = train_d, 
                        data.frame(Level = 6.5), type = "response")
  
  result_train <- data.frame(ID = train_d$ID, 
                             Base_salary = train_d$y,
                             predictions = pred_train)
  
  rmse_train <- rmse(result_train$Base_salary,
                     result_train$predictions) #testRMSE
  
  rmse_train <- round(rmse_train, 2)
  
  # =============== predict using model applied to test_d ===============
  pred_test <- predict(model, newdata = test_d, 
                       data.frame(Level = 6.5), type = "response")
  
  result_test <- data.frame(ID = test_d$ID, 
                            Base_salary = test_d$y,
                            predictions = pred_test)
  
  rmse_test <- rmse(result_test$Base_salary,
                    result_test$predictions) #testRMSE
  
  rmse_test <- round(rmse_test, 2)
  
  # =============== predict using model applied to valid_d ==============
  pred_valid <- predict(model, newdata = valid_d, 
                        data.frame(Level = 6.5), type = "response")
  
  
  result_valid <- data.frame(ID = valid_d$ID, 
                             Base_salary = valid_d$y,
                             predictions = pred_valid)
  
  rmse_valid <- rmse(result_valid$Base_salary,
                     result_valid$predictions) #testRMSE
  
  rmse_valid <- round(rmse_valid, 2)
  
  # cuz test, validation, train data must right shift => d$gp right shift
  data$gp <- (data$gp + (1/k)) %% 1
  
  # =========================== add to vector ============================ 
  set <- c(set, paste("fold", i, sep = ""))
  training <- c(training, rmse_train)
  test <- c(test, rmse_test)
  validation <- c(validation, rmse_valid)
}

# =========================== calculate average ===========================
set <- c(set, "ave.")
training <- c(training, round(mean(training), 2))
validation <- c(validation, round(mean(validation), 2))
test <- c(test, round(mean(test), 2))

# ============================ report_output =============================
out_data <- data.frame(set, training, validation, test)

# ========================= final predict output =========================

final_ID <- 1 : nrow(test_data)
pred_final <- predict(model, newdata = test_data, 
                      data.frame(Level = 6.5), type = "response")

result_final <- data.frame(ID = final_ID,
                           Base_salary = test_data$y,
                           predictions = pred_final)

result_final <- round(result_final, 2)

# ============================ write to file ============================
write.table(out_data, file = report_output, row.names = F, quote = F, sep = ',')

write.table(result_final, file = predict_output, row.names = F, quote = F, sep = ',')

# ======================== print Final Test RMSE ========================
print(paste('Fianl Test RMSE: ',round(rmse(result_final$Base_salary,
                                           result_final$predictions)
                                      , 2) 
)
) #testRMSE
