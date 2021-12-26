library(randomForest)
library(ggplot2)
library(Metrics)


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
train_data <- read.csv(file = train_input, header = T, stringsAsFactors = F)
test_data <- read.csv(file = test_input, header = T, stringsAsFactors = F)

# fold <- 5
# train_data <- read.csv(file = "ds_final/train_salary.csv", header = T, stringsAsFactors = F)
# test_data <- read.csv(file = "ds_final/test_salary.csv", header = T, stringsAsFactors = F)

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

# =============== boxplot(round(train_data$x4/50000, 0)) ================
train_data$x4 <- round(train_data$x4 / 50000, 0)

train_data <- subset(train_data, 
                     train_data$x4 <= 16)

# ================ boxplot(round(train_data$x6 / 4, 0)) =================
train_data$x6 <- round(train_data$x6 / 4, 0)
train_data <- subset(train_data, 
                     train_data$x6 <= 6)

boxplot(round(train_data$x7 / 4, 0))
train_data$x7 <- round(train_data$x7 / 4, 0)
train_data <- subset(train_data, 
                     train_data$x7 <= 6)

train_data$x10 <- round(train_data$x10 / 10000, 0)
train_data$x10 <- round(train_data$x10, 0)
train_data <- subset(train_data, 
                     train_data$x10 <= 20)

train_data$x11 <- round(train_data$x11 / 10000, 0)
train_data$x11 <- round(train_data$x11, 0)
train_data <- subset(train_data, 
                     train_data$x11 <= 20)

# ==================== convert char feature to factor ====================
train_data[sapply(train_data, is.character)] <- lapply(train_data
                                                       [sapply(train_data, 
                                                               is.character)], 
                                                       as.factor)
# # ================ analysis data through visualization =================
# 
# d3 <- ggplot(train_data, aes(x = x3, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d3
# #-----------------------------------------------------------------------
# 
# d4 <- ggplot(train_data, aes(x = x4, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d4
# #-----------------------------------------------------------------------
# 
# d6 <- ggplot(train_data, aes(x = x6, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d6
# #-----------------------------------------------------------------------
# 
# d7 <- ggplot(train_data, aes(x = x7, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d7
# #-----------------------------------------------------------------------
# 
# d10 <- ggplot(train_data, aes(x = x10, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d10
# 
# d11 <- ggplot(train_data, aes(x = x11, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d11
# 
# d12 <- ggplot(train_data, aes(x = x12, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d12
# 
# d14 <- ggplot(train_data, aes(x = x14, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d14
# 
# d15 <- ggplot(train_data, aes(x = x15, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d15
# 
# d16 <- ggplot(train_data, aes(x = x16, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d16
# 
# d17 <- ggplot(train_data, aes(x = x17, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d17
# 
# d18 <- ggplot(train_data, aes(x = x18, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d18
# 
# d19 <- ggplot(train_data, aes(x = x19, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d19
# 
# d20 <- ggplot(train_data, aes(x = x20, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d20
# 
# d21 <- ggplot(train_data, aes(x = x21, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d21
# 
# d22 <- ggplot(train_data, aes(x = x22, y = y)) + 
#   geom_point() + 
#   stat_smooth(method="lm", se=F) + theme_minimal()
# d22


# ========================= transfer to formula ==========================
interest <- as.formula(y ~ x4 + x6 + x7 + x10 + x11)

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
  model <- randomForest(formula = interest, data = train_data, ntree = 50)
  
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
