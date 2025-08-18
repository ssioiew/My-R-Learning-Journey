# ------------------------------------------------------
# File: tidymodels_linear_regression_mtcars.R
# Purpose: Demonstration of a predictive modeling workflow using tidymodels.
# Author: Pirada Naewkam
# Date: 2025-08-13
#
# Description:
#   This script builds a linear regression model to predict a car's miles per gallon (mpg)
#   using horsepower (hp) and weight (wt) from the built-in `mtcars` dataset.
#   It demonstrates the key steps of a tidymodels workflow:
#   1. Data splitting into training and testing sets.
#   2. Model specification and fitting.
#   3. Model evaluation using RMSE, R-squared, and MAE.
#   4. Basic feature engineering with the `recipes` package (dummy variables, normalization).
#   5. Combining a recipe and a model into a `workflow`.
#
# Input: The built-in `mtcars` dataset in R.
# Output: Console output showing model predictions and performance metrics.
# ------------------------------------------------------


# Step 0: Setup All Environments ------------------------------------------

library(tidymodels) # Predictive models
# library(parsnip)  # เพื่อเรียกใช้โมเดลในการทำนาย (linear regression)


# Step 1: Retrieve the Data -----------------------------------------------

# ใช้ข้อมูล mtcars ที่มีมากับ R อยู่แล้ว
head(mtcars)


# Step 2: Data Wraggling --------------------------------------------------

# แบ่งข้อมูลออกเป็น training set กับ testing set (3:1)
set.seed(1024)  # สุ่ม
split <- initial_split(mtcars, prop = 0.8)  # default กำหนดให้ training set เป็น 3/4

# ดึงข้อมูล training และ testing set ออกมาจากตัวแปร split
train_data <- training(split)  # ใช้ในการสร้างโมเดล
test_data <- testing(split)  # ใช้ในการทดสอบโมเดล


# Step 3: Build a Model ---------------------------------------------------

# Specify the Model (ระบุชนิดของโมเดล) >>> จะสร้าง linear regression
lm_spec <- linear_reg() %>%   # แค่ประกาศชนิดของโมเดล (ยังไม่ได้มีการคำนวณใด ๆ)
  set_engine("lm")  # ระบุว่าจะใช้ engine (เครื่องมือ) ชื่อ "lm" ในการสร้างโมเดล

# หรือ
# lm_spec <- linear_reg(mode = "regression", engine = "lm") # ได้เหมือนกัน

# Fit the Model (สอนโมเดลด้วยข้อมูล)
lm_fit <- lm_spec %>% 
  fit(mpg ~ hp + wt, data = train_data)

# ลองใช้โมเดลที่สอนไปแล้ว "ทำนาย" ข้อมูล test_data
predictions <- predict(lm_fit, new_data = test_data)


# Step 4: Evaluating Model Performance ------------------------------------

# นำคอลัมน์ .pred ที่ทำนายได้ ไปรวมกับ test_data เดิม
result_df <- bind_cols(test_data, predictions)

# ดูผลลัพธ์ของตารางที่รวมกันแล้ว (สนใจแค่คอลัมน์ mpg (ค่าจริง) และ .pred (ค่าทำนาย))
result_df %>% 
  select(mpg, .pred)

# สร้างชุดของค่าสถิติที่ต้องการวัด
regression_metrics <- metric_set(rmse, rsq, mae)  # mae คือ Mean Absolute Error

# คำนวณค่าสถิติจากตาราง result_df
regression_metrics(result_df, truth = mpg, estimate = .pred)


# Step 5: Feature Engineering with Recipes --------------------------------

# สร้าง "สูตรอาหาร" (Recipe) >>> เป็นแค่แผนการ
car_recipe <- recipe(mpg ~ ., data = train_data) %>%  # . หมายถึง ทุกตัวแปร
  
  # แปลงตัวแปรที่เป็นกลุ่ม/ปัจจัยทั้งหมดให้เป็น dummy variables
  step_dummy(all_nominal_predictors()) %>% 
  
  # ปรับสเกลตัวแปรที่เป็นตัวเลขทั้งหมด
  step_normalize(all_numeric_predictors())

# ให้ Recipe เรียนรู้พารามิเตอร์จาก train data
prepped_recipe <- prep(car_recipe)  # ก็จะบอกว่าเรียนรู้มาให้แล้ว (Trained)

# "Bake" the Data (นำ Recipe ที่เรียนรู้แล้ว ไป "อบ" หรือ "ประยุกต์ใช้" กับข้อมูลจริง ๆ)
baked_train_data <- bake(prepped_recipe, new_data = train_data)
head(baked_train_data)

# นำ Recipe เดียวกัน ไปใช้กับ test_data
baked_test_data <- bake(prepped_recipe, new_data = test_data)
head(baked_test_data)

# สร้าง Workflow เพื่อรวม feature engineering กับประเภทสเปคของโมเดล
# สร้าง workflow ว่าง ๆ ขึ้นมาก่อน
lm_workflow <- workflow() %>% 
  
  # เพิ่มสูตรอาหาร (recipe) ของเราเข้าไป
  add_recipe(car_recipe) %>% 
  
  # เพิ่มสเปคโมเดล (model spec) เข้าไป
  add_model(lm_spec)

# Fit the Workflow
set.seed(1024)
lm_workflow_fitted <- fit(lm_workflow, data = train_data)

# Predict with the Fitted Workflow
predictions <- predict(lm_workflow_fitted, new_data = test_data)

# นำผลลัพธ์ไปวัดผล
result_df <- bind_cols(test_data, predictions)
regression_metrics <- metric_set(rmse, rsq)
regression_metrics(result_df, truth = mpg, estimate = .pred)


# Step 6: Cross-Validation ------------------------------------------------

# สร้างแผนแบ่งข้อมูล train_data ออกเป็น 10 ส่วน (10-fold CV)
set.seed(1024)
folds <- vfold_cv(train_data, v = 10)

# Fit the Workflow to Folds
# นำ workflow ไป fit กับข้อมูล cross-validation ทั้ง 10 ชุด
cv_results <- fit_resamples(
  lm_workflow,
  resamples = folds
)

# รวบรวมและสรุปผลลัพธ์จากทั้ง 10 รอบ
collect_metrics(cv_results)


# Step 7: Compare Models --------------------------------------------------

# สร้าง model spec ใหม่ สำหรับ Decision Tree
tree_spec <- decision_tree() %>% 
  set_mode("regression") %>% 
  set_engine("rpart")

# สร้าง Workflow ให้ Decision Tree
tree_workflow <- workflow() %>% 
  add_recipe(car_recipe) %>% 
  add_model(tree_spec)

# Fit Resamples สำหรับโมเดลใหม่นี้
set.seed(1024)
tree_result <- fit_resamples(
  tree_workflow,
  resamples = folds
)

# เปรียบเทียบผลลัพธ์ระหว่าง 2 โมเดล
collect_metrics(cv_results) # จาก Step 6
collect_metrics(tree_result)

# ถามสิ่งที่เราต้องการ หากเราต้องการโมเดลที่ "แม่นยำมากที่สุด" หรือ "อธิบาย/เข้าใจข้อมูล" ของเราได้ดีที่สุด
# ถ้าเราต้องการความแม่นยำมากที่สุด ในกรณีนี้ Decision Tree ถือว่าชนะเลิศ


# Step 8: Model Tuning ----------------------------------------------------

# สร้าง Model Spec ที่มีช่องว่างให้จูน
tune_spec <- decision_tree(
  cost_complexity = tune(),   # บอกว่าเราจะจูนค่านี้
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# สร้าง Workflow สำหรับการจูน
tune_workflow <- workflow() %>% 
  add_recipe(car_recipe) %>% 
  add_model(tune_spec)

# สร้าง Grid ของค่าพารามิเตอร์ที่ต้องการลอง (Grid Search)
#  กำหนด parameter object ก่อน
tree_params <- parameters(tune_spec)

tree_grid <- grid_regular(
  tree_params,
  level = 5 # ลอง tree_depth 5 ระดับ และ cost_complexity 4 ระดับ รวมทั้งหมด 5 * 4 = 20 ชุดค่าผสม
)

# รันการจูนของ Grid Search (ใช้เวลานานเพราะต้อง train model เยอะมาก >>> 20 ชุดค่าผสม * 10 folds = 200 models)
set.seed(1024)
tree_tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = tree_grid
)

# สำรวจและเลือกผลลัพธ์ที่ดีที่สุด
collect_metrics(tree_tune_results)
show_best(tree_tune_results, metric = "rmse")

# เลือกชุดพารามิเตอร์ที่ดีที่สุด
best_params <- select_best(tree_tune_results, metric = "rmse")

# Finalize the Workflow and Fit on All Training Data
# นำค่าที่ดีที่สุดมาใส่ใน Workflow
final_workflow <- finalize_workflow(tune_workflow, best_params)

# สอนโมเดลครั้งสุดท้ายด้วย train_data ทั้งหมด
final_fit <- fit(final_workflow, data = train_data)


# Step 9: Final Evaluation ------------------------------------------------

# นำโมเดลที่ดีที่สุด 2 ตัว มาทดสอบด้วย test_data (ซึ่งไม่มีโมเดลไหนเคยเห็นมาก่อนเลย)
# ซึ่งได้แก่ lm_fit (Linear Regression พื้นฐาน) และ final_fit (Decision Tree ที่ผ่านการจูนจนได้พารามิเตอร์ที่ดีที่สุดมาแล้ว)

# ทำนายและวัดประสิทธิภาพของ Linear Regression บน test set
lm_test_results <- last_fit(
  lm_workflow,   # ใช้ workflow ที่ยังไม่ได้ fit
  split     # ใช้อ็อบเจกต์ split ข้อมูล
)

# ดูผลลัพธ์ค่า metrics หรือ ดูค่าทำนายเทียบกับค่าจริง
collect_metrics(lm_test_results)
collect_predictions(lm_test_results)

# ทำำนายและวัดประสิทธิภาพของ Tune Descision Tree บน test set (เหมือนกัน)
tree_test_results <- last_fit(
  final_workflow,
  split
)

collect_metrics(tree_test_results)
collect_predictions(tree_test_results)

# รวมสองผลลัพธ์เข้าด้วยกัน (Final Scoreboard)
lm_test_results <- collect_metrics(lm_test_results) %>% 
  mutate(model = "Linear Regression")

tree_test_results <- collect_metrics(tree_test_results) %>% 
  mutate(model = "Tuned Decision Tree")

bind_rows(lm_test_results, tree_test_results) %>% 
  select(model, .metric, .estimate)

# ผลลัพธ์ที่ได้กลับตาลปัตร โมเดลที่ดีที่สุดสำหรับ test_data คือ Linear Regresion แบบพื้นฐาน ซึ่งมี rmse อยู่ที่ 3.28
# ในขณะที่ Tuned Decision Tree เท่าก้บ 4.98 ซึ่งเป็นผลมาจากการ "Overfitting" ของโมเดล Tuned Decision Tree
# ซึ่งสามารถเกิดขึ้นได้บ่อยมาก


# Step 10: Try Using the Random Forest Model ------------------------------

# เริ่มจูนโมเดล Random Forest
