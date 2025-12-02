# ------------------------------------------------------
# File: Data analysis using R program.R
# Purpose: Brief description of the script
# Author: Pirada Naewkam
# Date: 2025-11-29
#
# Description:
#   Detailed description or steps
#
# Input: input files or data
# Output: output files or results
# ------------------------------------------------------


# Lesson 01: Simple linear regression analysis ----------------------------

# Load all libraries
library(tidyverse)
library(broom)
library(psych)
library(modelr)
library(ggfortify)

# Load all data
url <- "https://github.com/prasertcbs/basic-dataset/raw/master/height_mass_women.csv"
df <- read_csv(url)
df
summary(df) # ดูข้อมูลคร่าว ๆ
describe(df)  # psych library

df %>% ggplot(aes(x = Height_m, y = Mass_kg)) + geom_point() + 
  stat_smooth(method = "lm", se = FALSE)

model <- lm(Mass_kg ~ Height_m, data = df)  # formula
model
summary(model)

# broom package for overview
tidy(model)
glance(model)

# mean square error
model$residuals
mse(model, df)
sum((model$residuals ** 2) / length(df$Height_m)) # คำนวณมาจากแบบนี้

plot(model) # base r

autoplot(model, which = 1:6, ncol = 3) # ggfortify package


# Lesson 02: Multiple Regression Analysis ---------------------------------

# Load all libraries
pacman::p_load(
  tidyverse,
  openxlsx
)
options(scipen = 99)  # disable scientific (E) notion

# read marketing data
df <- read_csv("https://github.com/prasertcbs/basic-dataset/raw/master/marketing.csv")
df

# evaluate model (formula)
model <- lm(sales ~ youtube + facebook + newspaper, data = df)
model |> summary()  # new version pipe

# prediction formula
# y_hat <- 3.5266 + 0.045765youtube + 0.188530facebook - 0.001037newspaper

model$fitted.values   # ค่าที่ model ทำนาย sales ได้
model$residuals   # ค่าผลต่างของข้อมูลจริง กับ ค่าที่ model ทำนายได้ (fitted values)

df <- df |> mutate(predicted = model$fitted.values, residual = model$residuals)
df

# export
d_new
# write_csv(d_new, file = "Data analysis using R program/marketing_predict.csv")
# openxlsx::write.xlsx(d_new, file = "Data analysis using R program/marketing_predict.xlsx")

# calc r-square
ssr <- sum(df$residual ^ 2)
sst <- sum((df$sales - mean(df$sales)) ^ 2)
r2 <- 1 - (ssr / sst)
r2  # 0.8972106

d_new <- data.frame(
  youtube = c(200, 150),
  facebook = c(80, 100),
  newspaper = c(10, 5)
)
d_new

predict(model, d_new)

d_new <- openxlsx::read.xlsx("Data analysis using R program/marketing_newdata.xlsx")
d_new

p <- predict(model, d_new)
p

d_new <- d_new |> mutate(predicted = p)
d_new

# เช็คว่ามีตัวแปรไหนที่เพิ่มค่าเข้าไปก็ไม่ significant
model |> summary()  # newspaper
model2 <- lm(sales ~ youtube + facebook, data = df)
model2 |> summary()
p2 <- predict(model2, d_new)
p2
d_new <- d_new |> mutate(predicted2 = p2)
d_new

# df <- df |> select(-predicted, -residual)
df$predicted <- NULL
df$residual <- NULL
df
model3 <- lm(sales ~ ., data = df)
# model3 <- lm(sales ~ youtube + facebook + newspaper, data = df)
model3 |> summary()

model4 <- lm(sales ~ . -newspaper, data = df)
# model4 <- lm(sales ~ youtube + facebook, data = df)
model4 |> summary()


# Lesson 03: Basic Logistic Regression ------------------------------------

# Load all libraries
library(tidyverse)
library(broom)
library(caret)  # confusion matrix

# Load all data
url <- "https://github.com/prasertcbs/basic-dataset/raw/master/study_hours.csv"
df <- read_csv(url)
df

df |> ggplot(aes(x = Hours, y = Pass)) + geom_point()
df |> ggplot(aes(x = Hours, y = Pass)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE)
df |> ggplot(aes(x = Hours, y = Pass)) + geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(y = "prop of passing exam")

# การรันโมเดล (สมการ)
model <- glm(Pass ~ Hours, data = df, family = "binomial")
model
summary(model)
tidy(model)
glance(model)

model$fitted.values   # ค่าที่ทำนายได้ (predict = were fitted values)
tidy(predict(model, type = "response")) # ค่าที่ทำนายได้ (predict)

prob <- predict(model, type = "response")
predict <- prob > 0.5 # TRUE, FALSE
predict
predict <- as.numeric(prob > 0.5) # 1, 0
predict

# confusion matrix
confusionMatrix(data = factor(predict, level = c(0, 1), labels = c("fail", "pass")),
                reference = factor(df$Pass, levels = c(0, 1), labels = c("fail", "pass")))

confusionMatrix(data = factor(predict, level = c(1, 0), labels = c("pass", "fail")),
                reference = factor(df$Pass, levels = c(1, 0), labels = c("pass", "fail")),
                dnn = c("Predict", "Actual"))

df$Pass2 <- df$Pass
df
df$Pass2[df$Hours == 2.25] <- 0
df

confusionMatrix(data = factor(predict, level = c(1, 0), labels = c("pass", "fail")),
                reference = factor(df$Pass2, levels = c(1, 0), labels = c("pass", "fail")),
                dnn = c("Predict", "Actual"))

coef(model)
coef(model)[1] / coef(model)[2]

ggplot(data = df, aes(x = Hours, y = Pass)) + geom_point() +
  labs(y = "prob of passing exam") +
  geom_vline(xintercept = abs(coef(model)[1] / coef(model)[2]), color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.5, color = "green", linetype = "dashed") +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
  

# Lesson 04: t-test for the difference of means between two groups --------

# Load all libraries
library(tidyverse)
library(car)

# สร้าง test data ขึ้นมา
n <- 100
d1 <- data.frame(gender = rep("F", n),
                 height = rnorm(n, mean = 159, sd = 1.2))
d2 <- data.frame(gender = rep("M", n),
                 height = rnorm(n, mean = 170.3, sd = 6.3))
df <- rbind(d1, d2) # คอลัมน์ต้องเหมือนกัน เพราะว่าเราจะเอา row data มารวมกัน (ซึ่ง column ก็ต้องเป็นประเภทเดียวกัน)
df

# ดูค่าเฉลี่ยต่าง ๆ
df |> group_by(gender) |> 
  summarise(n = length(height), mean = mean(height), sd = sd(height))

df |> ggplot(aes(x = gender, y = height, color = gender)) + geom_violin() + geom_boxplot(width = 0.4)

# ทดสอบว่า variance มีความแตกต่างกันหรือไม่
lt <- leveneTest(height ~ gender, data = df)
lt
lt$`Pr(>F)`[1]  # ดึงค่า p-value ของ variance ออกมา

# ทดสอบ t.test กับ long format data
m <- t.test(height ~ gender, data = df,
            alternative = "two.sided",
            var.equal = ifelse(lt$`Pr(>F)`[1] > 0.05, TRUE, FALSE),
            conf.level = 0.95)
m

m <- t.test(height ~ gender, data = df,
  alternative = "two.sided",
  var.equal = TRUE,
  conf.level = 0.95)
m

m2 <- t.test(d1$height, d2$height,
             alternative = "two.sided",
             var.equal = ifelse(lt$`Pr(>F)`[1] > 0.05, TRUE, FALSE),
             conf.level = 0.95)
m2


# Lesson 05: Performing a paired t-test -----------------------------------

# Load all libraries
pacman::p_load(
  tidyverse,
  dlookr,
  rstatix,
  ggpubr,
  ggstatsplot
)
options(scipen = 99)  # disable scientific (E) notion

# Load all data
data <- read_csv("https://github.com/prasertcbs/R/raw/main/src/paired_t-test_data.csv")
data

diff <- data$beta - data$gamma
diff

shapiro.test(diff)

ggqqplot(diff)

mean(data$beta): mean(data$gamma)

# perform paired t-test (for long format)
t.test(data$beta, data$gamma, paired = TRUE)  # beta - gamma
t.test(data$gamma, data$beta, paired = TRUE)  # gamma - beta

data   # wide format

df <- data |> 
  gather(key = "maker", value = "score", -id) |>  # เพื่อแปลงเป็น long format
  arrange(maker, id)
df

df |> ggpubr::ggboxplot(x = "maker", y = "score", color = "maker", add = "mean") +
  expand_limits(y = 0)

# หรือเขียน t-test formula style ได้
# t.test(formula = score ~ maker, paired = TRUE, data = df) # ติด error ไม่รู้ทำไม

ggstatsplot::ggwithinstats(data = df,
              x = maker,
              y = score,
              type = "parametric") + ylab("taste score")
# ggsave("Data analysis using R program/coffee_maker-paired.svg", width = 7, height = 5)
# system("open Data analysis using R program/coffee_maker-paired.svg")


# Lesson 06: One-Way ANOVA analysis ---------------------------------------

# Load all libraries
library(tidyverse)

# Load all data
df <- read_csv("https://github.com/prasertcbs/basic-dataset/raw/master/anova_one_way_wide_format.csv")
df   # wide format

df <- df |> gather() |> transmute(fertilizer = key, height = value)  # long format
df

# plotting
df |> ggplot(aes(x = fertilizer, y = height, color = fertilizer)) + geom_point()

df |> ggplot(aes(x = fertilizer, y = height, color = fertilizer)) + geom_point() +
  stat_summary(fun.y = mean, color = "black", geom = "point", size = 4)

df |> ggplot(aes(x = fertilizer, y = height, color = fertilizer)) + geom_point() +
  stat_summary(fun.y = mean, color = "black", geom = "point", size = 4) +
  geom_hline(yintercept = mean(df$height), color = "maroon", linetype = "dashed")

# anova test
model <- aov(height ~ fertilizer, data = df, )
summary(model)  #  มี sig สักกลุ่ม

# หา sig ด้วย post-hoc
ph <- TukeyHSD(model)
ph


# Lesson 07: Explore and Visualize Bangkok PM2.5 --------------------------

# Load all libraries
library(tidyverse)
library(lubridate)  # เมื่อทำงานกับวันที่เยอะ ๆ

# Load all data
url <- "https://raw.githubusercontent.com/prasertcbs/basic-dataset/refs/heads/master/Bangkok_pm25.txt"
df <- read_tsv(url, skip = 10, col_names = FALSE)
df
colnames(df) <- c("year", "month", "day", "hour", "pm2_5", "X6", "X7")
df

# ลบ column ที่ไม่ใช้งาน
# df <- df |> select(-X6, -X7)
df <- df |> select(year:pm2_5)
df

df <- df |> mutate("date_time" = ISOdate(year, month, day, hour),
                   "local_date_time" = date_time + hours(7),
                   "local_hour_time" = hour(local_date_time))
df

# visualize data
df |> ggplot(aes(pm2_5)) + geom_histogram()

# ดูเป็นรายวัน
df |> group_by(date(local_date_time)) |> 
  summarize("avg_pm2_5" = mean(pm2_5)) |>   # จากที่ลอง test, summarize กับ summarise ได้ผลลัพธ์ตัวเลขเท่ากันนะ
  ggplot(aes(avg_pm2_5)) + geom_histogram()

# ดูเป็นรายชั่วโมง
df |> ggplot(aes(x = local_hour_time, y = pm2_5, color = as.factor(local_hour_time))) + geom_point()

# top five best days (lowest PM 2.5)
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarize("avg_pm2_5" = mean(pm2_5)) |> 
  arrange(avg_pm2_5) |> 
  top_n(-5)

# top five worst days (highest PM 2.5)
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarize("avg_pm2_5" = mean(pm2_5)) |> 
  arrange(desc(avg_pm2_5)) |> 
  top_n(5)

# ดูตามรายเดือน
df |> mutate("month" = month(local_date_time)) |> 
  group_by(month) |> 
  summarize("avg_pm2_5" = mean(pm2_5))

# ดูตามรายสัปดาห์
df |> mutate("weekday" = weekdays(local_date_time)) |> 
  group_by(weekday) |> 
  summarize("avg_pm2_5" = mean(pm2_5))

df |> 
  filter(year == 2018 | year == 2019 & year != 2020) |> 
  ggplot(aes(x = as.factor(month(local_date_time)), y = pm2_5, fill = as.factor(year(local_date_time)))) +
  geom_boxplot()

# time-series style plotting
df |> mutate("month" = month(local_date_time),
             "year" = year(local_date_time)) |> 
  group_by(year, month) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 
  ggplot(aes(x = month, y = avg_pm2_5, color = as.factor(year))) + geom_line()


# Lesson 08: Showing daily PM 2.5 dust data in Bangkok with heatmap -------

# Load all libraries
library(tidyverse)
library(lubridate)

# Load all data
url <- "https://raw.githubusercontent.com/prasertcbs/basic-dataset/refs/heads/master/Bangkok_pm25.txt"
df <- read_tsv(url, skip = 10, col_names = FALSE)
df
colnames(df) <- c("year", "month", "day", "hour", "pm2_5", "X6", "X7")
df

# ลบ column ที่ไม่ใช้งาน
# df <- df |> select(-X6, -X7)
df <- df |> select(year:pm2_5)
df

df <- df |> mutate("date_time" = ISOdate(year, month, day, hour),
                   "local_date_time" = date_time + hours(7),
                   "local_hour_time" = hour(local_date_time),
                   "local_week_time" = week(local_date_time)) # เพิ่ม local week time เข้ามา
df

# หาค่าเฉลี่ยรายวัน
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 

  # basic visualization
  ggplot(aes(x = date, y = avg_pm2_5, color = as.factor(year(date)))) + geom_line()

# ถ้าข้อมูลเพื่อเปรียบเทียบเยอะมาก ๆ ทำเป็น heatmap ก็จะเข้าใจได้ง่ายกว่านะ
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 
  mutate("day" = day(date),
         "month" = month(date),
         "year" = year(date)) |> 
  filter(year == 2018) |> 

  # basic heatmap visualization
  ggplot(aes(x = day, y = month, fill = avg_pm2_5)) + geom_tile()

# advance heatmap visualization in 1 year
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 
  
  # ให้เป็น as.factor เพื่อ discrete ข้อมูลออกจากกัน ไม่ให้เป็น continuous data
  mutate("day" = as.factor(day(date)),
         "month" = as.factor(month(date)),
         "year" = as.factor(year(date))) |> 
  filter(year == 2018) |> 

  # heatmap visualization
  ggplot(aes(x = day, y = month, fill = avg_pm2_5)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red")
  
# advance heatmap visualization in all years
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 
  
  # ให้เป็น as.factor เพื่อ discrete ข้อมูลออกจากกัน ไม่ให้เป็น continuous data
  mutate("day" = as.factor(day(date)),
         "month" = as.factor(month(date)),
         "year" = as.factor(year(date))) |> 

  # heatmap visualization
  ggplot(aes(x = day, y = month, fill = avg_pm2_5)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red") +
  facet_grid(year ~ .)


# ทำให้ color label discrete (ไม่เป็น continuous) ด้วย
# air quality index
# https://en.wikipedia.org/wiki/Air_quality_index
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
df |> mutate("date" = date(local_date_time)) |> 
  group_by(date) |> 
  summarise("avg_pm2_5" = mean(pm2_5)) |> 
  
  # ให้เป็น as.factor เพื่อ discrete ข้อมูลออกจากกัน ไม่ให้เป็น continuous data
  mutate(
    "pm_level" = cut(avg_pm2_5,
                          breaks = c(0, 9.0, 35.4, 55.4, 125.4, 225.4, 325.4),
                          label = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealty", "Very Unhealthy", "Hazardous")),
    "day" = as.factor(day(date)),
    "month" = as.factor(month(date)),
    "year" = as.factor(year(date))) |> 
  
  # heatmap visualization
  ggplot(aes(x = day, y = month, fill = pm_level)) + geom_tile(color = "white") +
  scale_fill_manual(values = c("green", "yellow", "orange", "red", "magenta4", "indianred4")) +
  facet_grid(year ~ .)


# The End -----------------------------------------------------------------

# จบไปอีกหนึ่ง เก่งมาก ๆ เลยนะ เราเข้าใจขึ้นเยอะเลย ขอบคุณที่ไม่หยุดที่จะเรียนรู้ ขอบคุณที่พยายามมาตลอด
# เธอทำได้แน่ สักวันต้องเป็นวันของเธอแน่

