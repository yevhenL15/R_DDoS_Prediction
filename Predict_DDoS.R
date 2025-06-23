# Завантаження пакету, якщо потрібно
if (!require("readr")) install.packages("readr")
library(readr)

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

if (!require("caret")) install.packages("caret")
library(caret)

if (!require("ROCR")) install.packages("ROCR")
library(ROCR)

if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Завантаження даних
data <- read.csv("data/Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv", sep=",")

# Описова статистика
summary(data)

dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Аналіз даних для зазначених змінних
variables <- c("Flow.Duration", "Total.Fwd.Packets", "Total.Backward.Packets", 
               "Total.Length.of.Fwd.Packets", "Total.Length.of.Bwd.Packets", 
               "Fwd.Packet.Length.Max", "Bwd.Packet.Length.Max", 
               "Fwd.IAT.Mean", "Bwd.IAT.Mean", "Label")

# Перетворення змінної Label на числовий формат
df$Label <- as.numeric(as.factor(df$Label))

# Підрахунок кількості пропущених значень
missing_values <- sapply(df[variables], function(x) sum(is.na(x)))
print(missing_values)

# Статистичний опис змінних
summary(df[variables])

# Підготовка даних для кругової діаграми
label_data <- data %>%
  group_by(Label) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = ifelse(Label == 0, "Безпечний трафік", "DDoS трафік"))

# Створення кругової діаграми для Label
  p1 <- ggplot(label_data, aes(x = "", y = percentage, fill = label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Розподіл трафіку за змінною Label", x = "", y = "") +
  scale_fill_manual(values = c("Безпечний трафік" = "blue", "DDoS трафік" = "red"))
  
  ggsave("plots/1_traffic_distribution_pie.png", p1, width = 10, height = 8, dpi = 300)
  
# Візуалізація Flow.Duration
  p2 <- ggplot(data, aes(x = `Flow.Duration`, fill = as.factor(Label))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Розподіл тривалості потоку за змінною Label", x = "Тривалість потоку (мікросекунди)", y = "Кількість") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Label", labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/2_flow_duration_histogram.png", p2, width = 12, height = 8, dpi = 300)
  
# Візуалізація Total.Fwd.Packets
  p3 <- ggplot(data, aes(x = `Total.Fwd.Packets`, fill = as.factor(Label))) +
  geom_histogram(position = "dodge", bins = 50) + # Збільшення розміру бінів
  scale_x_log10() + # Використання логарифмічної шкали для осі x
  labs(title = "Розподіл загальної кількості отриманих пакетів за змінною Label", 
       x = "Кількість отриманих пакетів (логарифмічна шкала)", 
       y = "Частота") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    name = "Label", 
                    labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/3_total_fwd_packets.png", p3, width = 12, height = 8, dpi = 300)

# Візуалізація Total.Backward.Packets
  p4 <- ggplot(data, aes(x = `Total.Backward.Packets`, fill = as.factor(Label))) +
  geom_histogram(position = "dodge", bins = 50) +
  scale_x_log10() +
  labs(title = "Розподіл кількості пакетів, надісланих у відповідь за змінною Label", 
       x = "Кількість надісланих у відповідь пакетів (логарифмічна шкала)", 
       y = "Кількість") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    name = "Label", 
                    labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/4_total_bwd_packets.png", p4, width = 12, height = 8, dpi = 300)

# Візуалізація Total.Length.of.Fwd.Packets
  p5 <- ggplot(data, aes(x = `Total.Length.of.Fwd.Packets`, fill = as.factor(Label))) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(title = "Розподіл загальної довжини отриманих пакетів за змінною Label", 
       x = "Загальна довжина пакетів вперед (логарифмічна шкала)", 
       y = "Щільність") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    name = "Label", 
                    labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/5_fwd_packet_length_density.png", p5, width = 12, height = 8, dpi = 300)

# Візуалізація Total.Length.of.Bwd.Packets
  p6 <- ggplot(data, aes(x = Label, y = `Total.Length.of.Bwd.Packets`, color = as.factor(Label))) +
  geom_jitter(alpha = 0.5) +
  scale_y_log10() +
  labs(title = "Розподіл загальної довжини пакетів, надісланих у відповідь за змінною Label", 
       x = "Label", 
       y = "Загальна довжина пакетів назад (логарифмічна шкала)") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     name = "Label", 
                     labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/6_bwd_packet_length_scatter.png", p6, width = 12, height = 8, dpi = 300)


# Візуалізація Fwd.Packet.Length.Max
  p7 <- ggplot(data, aes(x = Label, y = `Fwd.Packet.Length.Max`, color = as.factor(Label))) +
  geom_jitter(alpha = 0.5) +
  scale_y_log10() +
  labs(title = "Розподіл максимальної довжини ориманного пакету за змінною Label", 
       x = "Максимальна довжина пакету вперед (логарифмічна шкала)", 
       y = "Кількість") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     name = "Label", 
                     labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/7_fwd_packet_length_max.png", p7, width = 12, height = 8, dpi = 300)
  
# Візуалізація Bwd.Packet.Length.Max
  p8 <- ggplot(data, aes(x = `Bwd.Packet.Length.Max`, fill = as.factor(Label))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Розподіл максимальної довжини пакету, надісланого у відповідь за змінною Label", x = "Максимальна довжина пакету назад", y = "Кількість") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Label", labels = c("Безпечний трафік", "DDoS трафік"))
  ggsave("plots/8_bwd_packet_length_max.png", p8, width = 12, height = 8, dpi = 300)

# Візуалізація Fwd IAT Mean
  p9 <- ggplot(data, aes(x = `Fwd.IAT.Mean`, fill = as.factor(Label))) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(title = "Розподіл середнього інтервалу між отриманими пакетами за змінною Label", 
       x = "Середній інтервал між пакетами вперед (логарифмічна шкала)", 
       y = "Щільність") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    name = "Label", 
                    labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/9_fwd_iat_mean_density.png", p9, width = 12, height = 8, dpi = 300)

# Візуалізація Bwd.IAT.Mean
  p10 <- ggplot(data, aes(x = `Bwd.IAT.Mean`, fill = as.factor(Label))) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(title = "Розподіл середнього інтервалу між пакетами, надісланими у відповідь за змінною Label", 
       x = "Середній інтервал між пакетами назад (логарифмічна шкала)", 
       y = "Щільність") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    name = "Label", 
                    labels = c("Безпечний трафік", "DDoS трафік"))
  
  ggsave("plots/_10_bwd_iat_mean_density.png", p10, width = 12, height = 8, dpi = 300)

# Кореляція між змінними 
cor_matrix <- cor(df[variables], use = "complete.obs")

# Графік кореляційної матриці
png("plots/11_correlation_matrix.png")
corrplot(cor_matrix, method = "circle", title = "Correlation Matrix")
dev.off()


#________________________________________________________________________________________________________________


set.seed(123)  
split <- createDataPartition(data$Label, p = 0.75, list = FALSE)
trainData <- data[split,]
testData <- data[-split,]

# Навчання моделі логістичної регресії
model <- glm(Label ~ Bwd.Packet.Length.Max + Fwd.Packet.Length.Max
             + Total.Length.of.Fwd.Packets, data = trainData, family = "binomial")

# Передбачення на тестовій вибірці
predictions <- predict(model, testData, type = "response")
prediction_class <- ifelse(predictions > 0.5, 1, 0)

# Оцінка моделі
confusionMatrix <- confusionMatrix(as.factor(prediction_class), as.factor(testData$Label))
print(confusionMatrix)


#________________________________________________________________________________________________________________

# Видалення стовпців з усіма пропущеними значеннями
data <- data %>% select_if(~ !all(is.na(.)))

# Перетворення міток у фактор
data$Label <- as.factor(data$Label)

# Вибір тільки числових змінних
numeric_data <- data %>% select(where(is.numeric))

# Видалення числових змінних з нульовою стандартною девіацією
numeric_data <- numeric_data %>% select_if(~ sd(., na.rm = TRUE) != 0)

# Об'єднання числових змінних з мітками
data <- bind_cols(numeric_data, data %>% select(Label))

# Розділення даних на навчальну та тестову вибірки
set.seed(123) # Для відтворюваності результатів
trainIndex <- createDataPartition(data$Label, p = 0.75, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]

# Перевірка кореляцій для вибору релевантних змінних
correlation_matrix <- cor(select(dataTrain, -Label), use = "complete.obs")
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.9)
dataTrain <- dataTrain[ , -highly_correlated]
dataTest <- dataTest[ , -highly_correlated]

# Побудова моделі логістичної регресії
model <- train(Label ~ ., data = dataTrain, method = "glm", family = "binomial",
               trControl = trainControl(method = "cv", number = 5))

# Оцінка моделі на тестовій вибірці
predictions <- predict(model, newdata = dataTest)
confMatrix <- confusionMatrix(predictions, dataTest$Label)

# Виведення результатів
print(confMatrix)


#________________________________________________________________________________________________________________

set.seed(123)
split <- createDataPartition(data$Label, p = 0.75, list = FALSE)
trainData <- data[split,]
testData <- data[-split,]


# Навчання моделі випадкового лісу
rf_model <- randomForest(Label ~ Bwd.Packet.Length.Max + Fwd.Packet.Length.Max
                         + Total.Length.of.Fwd.Packets,
                         data = trainData, ntree = 500, mtry = 3, importance = TRUE)

# Передбачення на тестовій вибірці
rf_predictions <- predict(rf_model, testData)

# Оцінка моделі випадкового лісу
rf_confusionMatrix <- confusionMatrix(as.factor(rf_predictions), as.factor(testData$Label))
print(rf_confusionMatrix)


# ===== SAVING RESULTS =====

# Save summary statistics
sink("results/01_summary_statistics.txt")
cat("=== DATASET SUMMARY STATISTICS ===\n\n")
print(summary(data))
cat("\n=== MISSING VALUES ANALYSIS ===\n\n")
print(missing_values)
cat("\n=== TRAFFIC DISTRIBUTION ===\n\n")
print(table(data$Label))
cat("\n=== TRAFFIC PERCENTAGES ===\n\n")
print(prop.table(table(data$Label)) * 100)
sink()

# Save Logistic Regression Results (Simple Model)
sink("results/02_logistic_regression_simple.txt")
cat("=== SIMPLE LOGISTIC REGRESSION MODEL ===\n\n")
cat("Model Formula: Label ~ Bwd.Packet.Length.Max + Fwd.Packet.Length.Max + Total.Length.of.Fwd.Packets\n\n")
print(summary(model))
cat("\n=== CONFUSION MATRIX ===\n\n")
print(confusionMatrix)
sink()

# Save Enhanced Logistic Regression Results
sink("results/03_logistic_regression_enhanced.txt")
cat("=== ENHANCED LOGISTIC REGRESSION MODEL ===\n\n")
cat("Features used: All numeric features after correlation filtering\n")
cat("Cross-validation: 5-fold\n\n")
cat("=== MODEL SUMMARY ===\n\n")
print(model)
cat("\n=== CONFUSION MATRIX ===\n\n")
print(confMatrix)
sink()

# Save Random Forest Results
sink("results/04_random_forest_model.txt")
cat("=== RANDOM FOREST MODEL ===\n\n")
cat("Model Parameters:\n")
cat("- Number of trees: 500\n")
cat("- mtry: 3\n")
cat("- Features: Bwd.Packet.Length.Max + Fwd.Packet.Length.Max + Total.Length.of.Fwd.Packets\n\n")
print(rf_model)
cat("\n=== FEATURE IMPORTANCE ===\n\n")
print(importance(rf_model))
cat("\n=== CONFUSION MATRIX ===\n\n")
print(rf_confusionMatrix)
sink()
