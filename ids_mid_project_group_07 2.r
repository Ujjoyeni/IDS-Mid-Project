install.packages("dplyr")
library(dplyr)
mydata<-read.csv("E:/AIUB ( Study )/8th Semester/Introduction To Data Science/Mid/Project/heart_disease_uci - modified.csv",header = TRUE, sep = ",", na.strings = c("?", "", "NA"))
str(mydata)
head(mydata)
summary(mydata)
colSums(is.na(mydata))

check_outliers <- function(column) {
  
  column <- column[!is.na(column)]
  
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- column[column < lower_bound | column > upper_bound]
  
  return(outliers)
}

table(mydata$thal)

age_outliers <- check_outliers(mydata$age)
print(paste("Outliers in age:", length(age_outliers)))
ca_outliers <- check_outliers(mydata$ca)
print(paste("Outliers in ca:", length(ca_outliers)))
mean_age <- mean(mydata$age, na.rm = TRUE)
mydata$age <- ifelse(is.na(mydata$age), mean_age, mydata$age)
sum(is.na(mydata$age))
mydata$ca <- as.numeric(mydata$ca)  
median_ca <- median(mydata$ca, na.rm = TRUE)
mydata$ca[is.na(mydata$ca)] <- median_ca
sum(is.na(mydata$ca))
mydata$thal <- ifelse(is.na(mydata$thal), "normal", mydata$thal)
sum(is.na(mydata$thal)) 
any(is.na(mydata))
mydata$age <- ifelse(mydata$age <= 30, "young", 
              ifelse(mydata$age <= 60, "middle-aged", "old"))

mydata$thal <- ifelse(mydata$thal == "normal", 1, 
                      ifelse(mydata$thal == "fixed defect", 2, 3))

head(mydata[, c("age", "thal")])
mydata$trestbps <- (mydata$trestbps - min(mydata$trestbps, na.rm = TRUE)) / 
  (max(mydata$trestbps, na.rm = TRUE) - min(mydata$trestbps, na.rm = TRUE))

mydata$chol <- (mydata$chol - min(mydata$chol, na.rm = TRUE)) / 
  (max(mydata$chol, na.rm = TRUE) - min(mydata$chol, na.rm = TRUE))

mydata$thalch <- (mydata$thalch - min(mydata$thalch, na.rm = TRUE)) / 
  (max(mydata$thalch, na.rm = TRUE) - min(mydata$thalch, na.rm = TRUE))

mydata$oldpeak <- (mydata$oldpeak - min(mydata$oldpeak, na.rm = TRUE)) / 
  (max(mydata$oldpeak, na.rm = TRUE) - min(mydata$oldpeak, na.rm = TRUE))

head(mydata)
mydata_no_duplicates <- mydata[!duplicated(mydata), ]
nrow(mydata_no_duplicates)
head(mydata)
valid_age_categories <- c("young", "middle-aged", "old")
mydata$age[!(mydata$age %in% valid_age_categories)] <- NA  
mydata$age

valid_sex_values <- c("Male", "Female")
mydata$sex[!(mydata$sex %in% valid_sex_values)] <- NA  
mydata$sex

mydata$chol[mydata$chol < 0 | mydata$chol > 1] <- NA  
mydata$chol

mydata$trestbps[mydata$trestbps < 0 | mydata$trestbps > 1] <- NA
mydata$trestbps

valid_fbs_values <- c(TRUE, FALSE)
mydata$fbs[!(mydata$fbs %in% valid_fbs_values)] <- NA  
mydata$fbs

valid_restecg_values <- c("normal", "lv hypertrophy", "st-t wave abnormality")
mydata$restecg[!(mydata$restecg %in% valid_restecg_values)] <- NA
mydata$restecg

mydata$thalch[mydata$thalch < 0 | mydata$thalch > 1] <- NA  
mydata$thalch

valid_exang_values <- c(TRUE, FALSE)
mydata$exang[!(mydata$exang %in% valid_exang_values)] <- FALSE  
mydata$exang

mydata$oldpeak[mydata$oldpeak < 0 | mydata$oldpeak > 6] <- NA 
mydata$oldpeak

mydata$ca[mydata$ca < 0 | mydata$ca > 3] <- NA  
mydata$ca

valid_thal_values <- c("normal", "fixed defect", "reversable defect")
mydata$thal[!(mydata$thal %in% valid_thal_values)] <- NA 
mydata$thal

mydata$num[!(mydata$num %in% c(0, 1))] <- NA  
mydata$num

valid_dataset_values <- c("Cleveland", "Hungarian", "Switzerland", "Indian")
mydata$dataset[!(mydata$dataset %in% valid_dataset_values)] <- NA 
mydata$dataset


colSums(is.na(mydata))  

head(mydata)

print("Before")
mydata[41, ]
mydata$sex[41] <- "Female"
print("After")
mydata[41, ]

table(mydata$num)

mydata$num <- as.factor(mydata$num)

majority <- mydata %>% filter(num == "0")
minority <- mydata %>% filter(num == "1")

minority_upsampled <- minority %>%
  slice_sample(n = nrow(majority), replace = TRUE)  


balanced_dataset <- bind_rows(majority, minority_upsampled) %>%
  arrange(runif(n()))


cat("Before:")
print(table(mydata$num))

cat("After:")
print(table(balanced_dataset$num))

set.seed(123)
n <- nrow(mydata)
train_data <- sample(1:n, 0.7 * n)

train <-  mydata [train_data, ]
test <-  mydata [-train_data, ]


dim(train)
dim(test)

numeric_cols <- c("chol", "thalch")

for (col in numeric_cols) {
  cat("\n---", col, "---\n")
  mean_val <- mean(mydata[[col]], na.rm = TRUE)
  median_val <- median(mydata[[col]], na.rm = TRUE)
  mode_val <- names(sort(table(mydata[[col]]), decreasing = TRUE))[1]
  
  cat("Mean:", mean_val, "\n")
  cat("Median:", median_val, "\n")
  cat("Mode:", mode_val, "\n")
}

categorical_cols <- c("age", "cp")

for (col in categorical_cols) {
  cat("\n---", col, "---\n")
  mode_val <- names(sort(table(mydata[[col]]), decreasing = TRUE))[1]
  
  cat("Mode:", mode_val, "\n")
}

mydata$ca <- as.numeric(mydata$ca)
mydata$ca

mydata$oldpeak <- as.numeric(mydata$oldpeak)
mydata$oldpeak

range_ca <- range(mydata$ca, na.rm = TRUE)
iqr_ca <- IQR(mydata$ca, na.rm = TRUE)
var_ca <- var(mydata$ca, na.rm = TRUE)
sd_ca <- sd(mydata$ca, na.rm = TRUE)

cat("Spread for 'ca' (number of vessels):\n")
cat("Range:", range_ca[1], "to", range_ca[2], "\n")
cat("IQR:", iqr_ca, "\n")
cat("Variance:", var_ca, "\n")
cat("Standard Deviation:", sd_ca, "\n\n")

range_oldpeak <- range(mydata$oldpeak, na.rm = TRUE)
iqr_oldpeak <- IQR(mydata$oldpeak, na.rm = TRUE)
var_oldpeak <- var(mydata$oldpeak, na.rm = TRUE)
sd_oldpeak <- sd(mydata$oldpeak, na.rm = TRUE)

cat("Spread for 'oldpeak':\n")
cat("Range:", range_oldpeak[1], "to", range_oldpeak[2], "\n")
cat("IQR:", iqr_oldpeak, "\n")
cat("Variance:", var_oldpeak, "\n")
cat("Standard Deviation:", sd_oldpeak, "\n")
