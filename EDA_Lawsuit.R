library(data.table)
library(ggplot2)
library(car)
library(dplyr)
library(tidyr)
library(scales)
dt<-fread("Lawsuit.csv")

#Data Preprocessing
#============================================================================
#Gender: 1=Male, 0=Female；Rank: 1=Assistant, 2=Associate, 3=Full；Dept: 1~6
#Clin: 1=Clinical, 0=Research；Cert: 1=Certified, 0=Not certified
dt[, Gender := ifelse(Gender == 1, "Male", "Female")]
dt[, Gender := factor(Gender, levels = c("Male", "Female"))]

dt[, Rank := factor(Rank, levels = c(1,2,3),
                    labels = c("Assistant","Associate","Full"))]

dt[, Dept := factor(Dept, levels = 1:6,
                    labels = c("Biochem/MolBio","Physiology","Genetics",
                               "Pediatrics","Medicine","Surgery"))]

dt[, Clin := factor(ifelse(Clin == 1, "Clinical", "Research"))]
dt[, Cert := factor(ifelse(Cert == 1, "Certified", "NotCertified"))]

#Check for missing values
cat("\n=== missing value ===\n"); print(sapply(dt, function(x) sum(is.na(x))))
cat("\n=== summary ===\n"); print(summary(dt[, .(Prate, Exper, Sal94, Sal95)]))
#=============================================================================


#Overview of Data and Visualization
#=============================================================================
#statistics by gender
desc_by_gender <- dt[, .(
  n = .N,
  mean_Sal94 = mean(Sal94, na.rm = TRUE),
  sd_Sal94   = sd(Sal94, na.rm = TRUE),
  mean_Sal95 = mean(Sal95, na.rm = TRUE),
  sd_Sal95   = sd(Sal95, na.rm = TRUE),
  mean_Prate = mean(Prate, na.rm = TRUE),
  mean_Exper = mean(Exper, na.rm = TRUE)
), by = Gender]

# Reshape to long format
dt_long <- dt %>%
  pivot_longer(cols = c(Sal94, Sal95),
               names_to = "Year",
               values_to = "Salary") %>%
  mutate(Year = ifelse(Year == "Sal94", "1994", "1995"))

# Combined boxplot with salaries in 100k
ggplot(dt_long, aes(x = Gender, y = Salary/1000, fill = Gender)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  facet_wrap(~ Year) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  labs(title = "Salary by Gender (1994 vs 1995)",
       y = "")

# Single boxplot of Sal95 (or any salary column)
ggplot(dt, aes(y = Sal95)) +
  geom_boxplot(fill = "purple", alpha = 0.3,) +
  labs(title = "Boxplot of Salary (1995)",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::label_number(suffix = "k", scale = 1/1000))

# Salary by Rank and Gender (in 1000s)
ggplot(dt, aes(x = Rank, y = Sal95/1000, fill = Gender)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  labs(title = "Salary by Rank and Gender (1995)", 
       y ="", x="")

# Side-by-side histogram with different shades of purple

ggplot(dt_long, aes(x = Salary/1000, fill = Year)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.6) +
  scale_fill_manual(values = c("1994" = "#9370DB",  # Medium Purple
                               "1995" = "#D8BFD8")) +  # Thistle
  scale_x_continuous(labels = label_number(suffix = "k")) +
  labs(title = "Salary Histogram (1994 vs 1995)",
       x = "", y = "") +
  theme(legend.position = )

# Salary vs Experience
ggplot(dt, aes(x = Exper, y = Sal95, color = Gender)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  labs(title = "Salary vs Experience by Gender", y = "Salary", x = "Years of Experience")

# Salary vs Publication Rate
ggplot(dt, aes(x = Prate, y = Sal95, color = Gender)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  labs(title = "Salary vs Publication Rate by Gender", y = "Salary", x = "Publication Rate")

# Salary by Department and Gender
ggplot(dt, aes(x = Dept, y = Sal95, fill = Gender)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  labs(title = "Salary by Department and Gender", y = "Salary", x = "Department") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#==============================================================================


# Statistic Test
#=============================================================================
# (A) T-test for salary by gender -p-value small, difference is significance
tt94 <- t.test(Sal94 ~ Gender, data = dt)
tt95 <- t.test(Sal95 ~ Gender, data = dt)

cat("T-test: Sal94 ~ Gender\n"); print(tt94)
cat("\nT-test: Sal95 ~ Gender\n"); print(tt95)


# (B) chi-test：Rank with gender, p-value = 3.729e-09 (~0.0000000037), very significant
xt <- table(dt$Gender, dt$Rank)
chisq_rg <- chisq.test(xt)


cat("Chi-square test: Rank ~ Gender\n"); print(chisq_rg)
#=============================================================================


#Linear Regression Model
#=============================================================================
# log(sal)~Exper, Prate, Dept, Cert, Clin）
m1 <- lm(log(Sal95) ~ Gender + Exper + Prate + Dept + Cert + Clin , data = dt)
vif(m1)
cat("\n=== OLS Model: Sal95 ~ Gender + Exper + Prate + Dept + Cert + Clin ===\n"); print(summary(m1))
#============================================================================


#Logistic Regression Model
#============================================================================
# Full professor=1，or =0; FullProf ~ Gender + Exper + Prate + Dept + Cert + Clin
dt[, FullProf := as.integer(Rank == "Full")]
glm_full <- glm(FullProf ~ Gender + Exper + Prate + Dept + Cert + Clin,
                data = dt, family = binomial())


cat("=== Logistic Regression: FullProf (1) ~ Gender + Exper + Prate + Dept + Cert + Clin ===\n")
print(summary(glm_full))
#odds ratio 41.6%
exp(cbind(OR = coef(glm_full), confint(glm_full))) 

#Assistant ~ Gender + Exper + Prate + Dept + Cert + Clin
dt[, Assistant := as.integer(Rank == "Associate")]
glm_assist <- glm(Assistant ~ Gender + Exper + Prate + Dept + Cert + Clin,
               data = dt, family = binomial())

summary(glm_assist)
exp(cbind(OR = coef(glm_assist), confint(glm_assist))) 
#============================================================================


#Random Forest
#============================================================================
#FUll PROf
library(randomForest)
set.seed(2025)
dt[, FullProf := factor(ifelse(Rank == "Full", "Yes", "No"))]
idx <- sample.int(nrow(dt), size = floor(0.7 * nrow(dt)))
train <- dt[idx]
test  <- dt[-idx]
rf_full <- randomForest(
  FullProf ~ Gender + Exper + Prate + Dept + Cert + Clin + Sal94,
  data = train,
  ntree = 500,        
  importance = TRUE,
  na.action = na.omit
)
print(rf_full)
pred_class <- predict(rf_full, newdata = test, type = "class")
cm <- table(Actual = test$FullProf, Pred = pred_class)
acc <- sum(diag(cm)) / sum(cm)

cat("\n[FullProf ] TrainSet Confusion Matrix：\n"); print(cm)
cat(sprintf("\n[FullProf ] TrainSet Accuracy：%.2f%%\n", acc * 100))
print(importance(rf_full))
varImpPlot(rf_full, main = "Variable Importance (FullProf RF)")


# Salary
rf_sal <- randomForest(
  Sal95 ~ Gender + Exper + Prate + Dept + Cert + Clin + Sal94,
  data = train,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)
print(rf_sal)  

pred_sal <- predict(rf_sal, newdata = test)
rmse <- sqrt(mean((pred_sal - test$Sal95)^2))
mae  <- mean(abs(pred_sal - test$Sal95))

cat(sprintf("\n[Sal95]  testset RMSE：%.0f\n", rmse))
cat(sprintf("[Sal95] testset MAE ：%.0f\n", mae))


print(importance(rf_sal))
varImpPlot(rf_sal, main = "Variable Importance (Sal95 RF)")
