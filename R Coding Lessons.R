# ------------------------------------------------------
# File: R Coding Lessons
# Purpose: To develop myself
# Author: Pirada Naewkam
# Date: 2025-03-26
# ------------------------------------------------------


# Lesson 1 สร้างกราฟสวย ๆ ด้วยข้อมูลที่สนุก ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างข้อมูลเกี่ยวกับจำนวนคนที่ชอบผลไม้ต่าง ๆ
fruit_data_barplot <- data.frame(
  fruit = c("Apple", "Banana", "Orange", "Grapes", "Pineapple"),
  count = c(120, 200, 150, 100, 180)
)

# ขั้นตอนที่ 2: สร้างกราฟแท่งแสดงจำนวนคนที่ชอบผลไม้แต่ละชนิด
library(ggplot2)
ggplot(fruit_data_barplot, aes(x = fruit, y = count, fill = fruit)) +
  geom_bar(stat = "identity", size = 1.5, width = 0.7) +
  theme_minimal() +
  labs(title = "Fruit Preference Survey", x = "Fruit", y = "Number of People") +
  scale_fill_brewer(palette = "Set3") + # เลือกสีแบบ palette "Set3" ให้สีสดใส
theme(
  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "lightgray", size = 0.5),  # เพิ่ม gridlines สีเทา
  panel.grid.minor = element_blank(),  # เอา gridlines เล็กออก
  legend.position = "none"  # เอาตำแหน่ง legend ออก
)

  
# Lesson 2 สร้างกราฟเส้น (Line Plot) เพื่อแสดงการเปลี่ยนแปลงข้อมูล ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างข้อมูลที่ใช้ในการสร้างกราฟเส้น
fruit_data_lineplot <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May"),
  apple = c(120, 140, 160, 180, 200),
  banana = c(200, 210, 220, 230, 240),
  orange = c(150, 170, 190, 200, 210)
)

# ขั้นตอนที่ 2: แปลงข้อมูลให้เป็น long format
library(tidyr)
fruit_data_long_format <- fruit_data_lineplot %>%
  pivot_longer(cols = -month, names_to = "fruit", values_to = "count")

# ขั้นตอนที่ 3: สร้างกราฟเส้น (Line Plot)
library(ggplot2)
ggplot(fruit_data_long_format, aes(x = month, y = count, group = fruit, color = fruit)) +
  geom_line(aes(linetype = fruit), size = 1.5) +  # สร้างเส้นแบบ linetype ให้แตกต่างกัน
  geom_point(size = 3) +  # เพิ่มจุดแต่ละเดือน
    labs(
    title = "Fruit Preference Over Time",
    subtitle = "Change in the number of people liking different fruits",
    x = "Month",
    y = "Number of People"
  ) +
  scale_color_manual(values = c("apple" = "red", "banana" = "yellow", "orange" = "orange")) + # กำหนดสีให้แต่ละเส้น
theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5),
  )


# Lesson 3 สร้างกราฟการกระจาย (Scatter Plot) เพื่อดูความสัมพันธ์ของข้อมูล ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างข้อมูลเวลาเรียน และ คะแนนสอบที่ได้ ของนักเรียน
student_data <- data.frame(
  study_hours = c(1, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8, 9, 10),
  exam_score = c(45, 50, 52, 55, 60, 62, 68, 74, 78, 85, 90, 95)
)

# ขั้นตอนที่ 2: สร้างกราฟ Scatter Plot
library(ggplot2)
ggplot(student_data, aes(x = study_hours, y = exam_score)) +
  geom_point(color = "lightblue", size = 3) +  # ใช้ geom_point() เพื่อสร้างจุดของข้อมูล
  
  # ขั้นตอนที่ 3: เพิ่มเส้นแนวโน้ม (Trend Line)
  geom_smooth(method = "lm", color = "pink", se = FALSE) +  # เพิ่มเส้นแนวโนม (Linear Model) โดยไม่ต้องแสดงแถบความเชื่อมั่น
  labs(
    title = "Relationship Between Study Hours and Exam Score",
    x = "Study Hours",
    y = "Exam Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 14),
    panel.grid = element_line(color = "lightgrey", size = 0.5),
  )


# Lesson 4 สร้างกราฟ Boxplot ใน R ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
student_data_boxplot <- data.frame(
  gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  exam_score = c(75, 80, 85, 90, 70, 65, 95, 100, 60, 55)
)

# ขั้นตอนที่ 2: สร้างกราฟ Boxplot
library(ggplot2)
ggplot(student_data_boxplot, aes(x = gender, y = exam_score, fill = gender)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "red", outlier.size = 3) +  # สร้างกราฟ Boxplot + outliers
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Boxplot of Exam Scores by Gender",
    x = "Gender",
    y = "Exam Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5),
    legend.position = "none"
  )


# Lesson 5 สร้างกราฟฮิสโตแกรม (Histogram) เพื่อดูการกระจายของข้อมูล --------------------------------------------------------------- 

# ขั้นตอนที่ 1 สร้างชุดข้อมูลตัวอย่าง
student_data_histogram <- data.frame(
  exam_score = c(75, 80, 85, 90, 70, 65, 95, 100, 60, 55, 88, 92, 78, 89, 94, 91, 84, 76, 82, 87)
)

# ขั้นตอนที่ 2: สร้างกราฟฮิสโตแกรม
library(ggplot2)
ggplot(student_data_histogram, aes(x = exam_score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +  # สร้างฮิสโตแกรม
  labs(
    title = "Histogram of Exam Scores",
    x = "Exam Score",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
  )


# Lesson 6 การแบ่งพล็อตเป็นหลาย ๆ กลุ่มในกราฟเดียว (Faceting) --------------------------------------------------------------- 

# การใช้ facet_wrap() เพื่อแบ่งพล็อตของกริดตามตัวแปรเดียว
  # ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
student_data_facet <- data.frame(
  gender = rep(c("Male", "Female"), each = 50),
  exam_score = c(rnorm(50, mean = 75, sd = 10), rnorm(50, mean = 80, sd = 12))
)

  # ขั้นตอนที่ 2: พล็อตฮิสโตแกรม แยกตาม gender
library(ggplot2)
ggplot(student_data_facet, aes(x = exam_score, fill = gender)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_wrap(~ gender) +  # ใช้ facet_wrap() แยกพล็อตตามค่าของ gender
  labs(
    title = "Distribution of Exam Scores by Gender",
    x = "Exam Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
  )

# การใช้ facet_grid เพื่อแบ่งพล็อตเป้นแภว/คอลัมน์ตาม 2 ตัวแปร
  # ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
  student_data_facet2 <- data.frame(
  gender = rep(c("Male", "Female"), each = 50),
  study_group = rep(c("Morning", "Evening"), times = 50),
  study_hours = rnorm(100, mean = 5, sd = 2),
  exam_score = rnorm(100, mean = 75, sd = 10)
)

  # ขั้นตอนที่ 2: สร้าง Scatter Plot แยกตาม gender และ study_group
library(ggplot2)
ggplot(student_data_facet2, aes(x = study_group, y = exam_score, fill = gender)) +
  geom_boxplot() +
  facet_grid(gender ~ study_group) +  # ใช้ facet_grid() แบ่งพล็อตเป็นตาราง
  labs(
    title = "Study Hours VS Exam Score by Gender & Study Group",
    x = "Study Group", # แก้ไขให้ตรงกับ aes(x = study_group)
    y = "Exam Score",
    fill = "Student Gender" # แก้ไขชื่อ legend
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
  )


# Lesson 7 การปรับแต่งธีม (Theme Customization) ใน ggplot2 --------------------------------------------------------------- 

# การใช้ธีมสำเร็จรูป
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Relationship between Horsepower and MPG",
    x = "Horsepower",
    y = "Miles per Gallon"
  ) +
  theme_minimal() + # ใช้ธีม minimal

# การปรับแต่งแต่ละส่วนของกราฟด้วย theme()
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkblue"),
    axis.title =  element_text(size = 16, face = "italic"),
    axis.text = element_text(size = 14, color = "darkred"),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),   # ซ่อนเส้นกริดเล็ก
    legend.position = "top", # ย้าย legend ไปด้านบน
    
    # การลบองค์ประกอบบางส่วนออกจากกราฟ
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )

# การใช้ธีมส่วนตัว (Custom Theme) เมื่อต้องการใช้ธีมเดิมซ้ำ ๆ
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "blue"),
    legend.position = "bottom"
  )

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, color = "red") +
  labs(title = "Custom Theme Example") +
  my_theme  # ใช้ธีมที่สร้างไว้

# การใช้ scale_fill_brewer() เพื่อเลือกโทนสีอัตโนมัติ
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +  # ใช้พาเลตสีสำเร็จรูป (1-4)
  labs(title = "Number of Cars by Cylinder") +
  theme_minimal()


# Lesson 8 การเพิ่ม Annotation และ Text บนกราฟ ggplot2 ---------------------------------------------------------------

# การใช้ annotate() เพื่อเพิ่มข้อความ & ลูกศร
library(ggplot2)
ggplot(mtcars, aes(x = hp, y = mpg, label = rownames(mtcars))) +
  geom_point(size = 3, color = "blue") +
  annotate("text", x = 250, y = 30, label = "High HP, Low MPG", size = 6, color = "red", fontface = "bold") +
  annotate("segment", x = 230, xend = 280, y = 28, yend = 18, color = "red", arrow = arrow()) +
  
  # การใช้ geom_text() และ geom_label() เพื่อใส่ค่าบนจุดของข้อมูล
  geom_text(vjust = -1, size = 2.5, color = "black") +  # vjust = -1 เพื่อขยับข้อความขึ้นเล็กน้อย
  labs(title = "Car Performance HP VS MPG") +
  theme_minimal()

# การใช้ geom_richtext() เพื่อใส่ข้อความสีสัน & HTML
library(ggtext)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, color = "blue") +
  geom_richtext(aes(x = 250, y = 30, label = "**High HP, Low MPG** <br> <span style='color:red;'>Warning Zone</span>"),
                fill = "white", label.colour = "black", size = 6) +
  labs(title = "Car Performance: HP VS MPG") +
  theme_minimal()

# การใช้ geom_curve() เพื่อวาดเส้นโค้งแทนลูกศร
library(ggplot2)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, color = "blue") +
  geom_curve(aes(x = 230, xend = 280, y = 28, yend = 18), curvature = -0.3, arrow = arrow(), color = "red") +
  annotate("text", x = 250, y = 30, label = "High HP, Low MPG", size = 6, color = "red", fontface = "bold") +
  labs(title = "Car Performance: HP VS MPG") +
  theme_minimal()


# Lesson 9 สร้าง Heatmap เพื่อแสดงความสัมพันธ์ระหว่างตัวแปร ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
library(dplyr)
temperature_data <- expand.grid(
  month = month.abb,   # Jan, Feb, Mar, ...
  city = c("Bangkok", "Tokyo", "New York")
) %>%
  mutate(temperature = round(runif(n(), min = -5, max = 45), 1)) # สุ่มค่าอุณหภูมิ (mock data)

# ขั้นตอนที่ 2: สร้าง Heatmap
library(ggplot2)
ggplot(temperature_data, aes(x = month, y = city, fill = temperature)) +
  geom_tile(color = "white") +  # ใช้ geom_tile() เพื่อสร้างกริดของ Heatmap
  scale_fill_gradient(low = "blue", high = "red") + # กำหนดสีจากเย็นไปร้อน
  labs(
    title = "Monthly Average Temperature in Different Cities",
    x = "Month",
    y = "City",
    fill = "Temp (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_blank()
  )


# Lesson 10 สร้าง Pie Chart ด้วยการใช้ ggplot2 ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
major_data <- data.frame(
  major = c("Science", "Arts", "Engineering", "Business", "Medicine"),
  students = c(300, 200, 250, 150, 100)
)

major_data$percentage <- round(major_data$students / sum(major_data$students) * 100, 1) # คำนวณเปอร์เซ็นต์ของนักเรียนในแต่ละกลุ่ม

major_data$label <- paste0(major_data$major, " (", major_data$percentage, "%)")  # สร้างข้อความ Label สำหรับกราฟ

# ขั้นตอนที่ 2: สร้าง Pie Chart ด้วย ggplot2
library(ggplot2)
ggplot(major_data, aes(x = "", y = students, fill = major)) +
  geom_bar(stat = "identity", width = 1, color = "white") + # สร้างแท่งข้อมูลแบบวงกลม
  coord_polar(theta = "y") +  # แปลงเป็น Pie Chart
  theme_void() +  # ลบพื้นหลังและเส้นกริดออก
  labs(
    title = "Student Major Distribution",
    fill = "Major"
    ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  ) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  scale_fill_brewer(palette = "Pastel1")  # เลือกชุดสี Pastel1


# Lesson 11 สร้างกราฟแบบ Violin Plot + การเปรียบเทียบกลุ่มด้วยสถิติ ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
set.seed(123) # ตั้งค่า seed เพื่อให้ผลลัพธ์สม่ำเสมอ
exam_sample <- data.frame(
  group = rep(c("Group A", "Group B"), each = 50),
  score = c(rnorm(50, mean = 75, sd = 8),   # กลุ่ม A
            rnorm(50, mean = 82, sd = 6))   # กลุ่ม B
)

# ขั้นตอนที่ 2: สร้าง Violin Plot พื้นฐาน
library(ggplot2)
library(ggpubr)
ggplot(exam_sample, aes(x = group, y = score, fill = group)) +
  geom_violin(trim = F, alpha = 0.7, color = NA) +  # สร้างกราฟ violin พร้อมกับตั้ง alpha = ความโปร่งแสง
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) + # เพิ่ม Boxplot ขนาดเล็กไว้ด้านใน
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) + # เพิ่มจุดของข้อมูลจริง
  
  # ขั้นตอนที่ 3: เพิ่มสถิติเปรียบเทียบกลุ่มด้วย ggpubr
  stat_compare_means(method = "t.test",   # ใช้วิธีทางสถิติ t-test
                     label = "p.format",   # แสดงค่า p-value เป็นตัวเลข
                     label.x = 1.5,
                     label.y = max(exam_sample$score),   # ตำแหน่ง label
                     size = 5) +     
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +  # กำหนดสีเอง
  labs(
    title = "Exam Score Distribution (with Statistical Test)",
    subtitle = "T-test comparison between groups",
    x = "Group",
    y = "Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90")
  )


# Lesson 12 สร้าง Interactive Graphs ด้วย ploty ---------------------------------------------------------------

# สร้าง Interactive Bar Chart
fruit_data_inter_barchart <- data.frame(
  fruit = c("Apple", "Banana", "Orange", "Grapes", "Pineapple"),
  count = c(120, 200, 150, 100, 180)
)

library(plotly)
fig_barchart <- plot_ly(fruit_data_inter_barchart, x = ~fruit, y = ~count, type = "bar", color = ~fruit) %>%
  layout(title = "Fruit Preference Survey",
         xaxis = list(title = "Fruit"),
         yaxis = list(title = "Number of People"))

fig_barchart

# สร้าง Interactive Line Plot
  # สร้างชุดข้อมูลตัวอย่าง
fruit_data_inter_lineplot <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May"),
  apple = c(120, 140, 160, 180, 200),
  banana = c(200, 210, 220, 230, 240),
  orange = c(150, 170, 190, 200, 210)
)

  # แปลงชุดข้อมูลเป็น Long Format
library(tidyr)
fruit_data_inter_lineplot_long <- fruit_data_inter_lineplot %>%
  pivot_longer(cols = -month, names_to = "fruit", values_to =  "count")

  # สร้าง Interactive Line Plot
library(plotly)
fig_lineplot <- plot_ly(fruit_data_inter_lineplot_long, x = ~month, y = ~count, color = ~fruit, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Fruit Preference Over Time",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Number of People"))

fig_lineplot

# สร้าง Interactive Scatter Plot
  # สร้างชุดข้อมูลตัวอย่าง
student_data_inter_scatterplot <- data.frame(
  study_hours = c(1, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8, 9, 10),
  exam_score = c(45, 50, 52, 55, 60, 62, 68, 74, 78, 85, 90, 95)
)

  # สร้าง Scatter Plot แบบ Interactive
library(plotly)
fig_scatterplot <- plot_ly(student_data_inter_scatterplot, x = ~study_hours, y = ~exam_score, type = "scatter", mode = "markers",
                           marker = list(size = 10, color = "blue")) %>%
  layout(title = "Study Hours VS Exam Score",
         xaxis = list(title = "Study Hours"),
         yaxis = list(title = "Exam Score"))

fig_scatterplot

# สร้าง Interactive Boxplot
  # สร้างชุดข้อมูลตัวอย่าง
student_data_inter_boxplot <- data.frame(
  gender = rep(c("Male", "Female"), each = 10),
  exam_score = c(75, 80, 85, 90, 70, 65, 95, 100, 60, 55, 88, 92, 78, 89, 94, 91, 84, 76, 82, 87)
)

  # สร้าง Interactive Boxplot
library(plotly)
fig_boxplot <- plot_ly(student_data_inter_boxplot, x = ~gender, y = ~exam_score, type = "box", color = ~gender) %>%
  layout(title = "Boxplot of Exam Scores by Gender",
         xaxis = list(title = "Gender"),
         yaxis = list(title = "Exam Score"))

fig_boxplot

# ทำให้ ggplot2 เป็น Interactive ด้วย ggplotly()
  # สร้างกราฟ ggplot2 ปกติก่อน
library(ggplot2)
p <- ggplot(student_data, aes(x = study_hours, y = exam_score)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Study Hours VS Exam Score",
       x = "Study Hours",
       y = "Exam Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey")
  )

  # แปลงเป็น Interactive Graph
library(plotly)
ggplotly(p)


# Lesson 13 การจัดการข้อมูลขั้นสูงด้วย dplyr (Advanced Data Manipulatation) ---------------------------------------------------------------

# การสรุปข้อมูลแบบกลุ่มด้วย group_by() + summarise()
  # สร้างชุดข้อมูลตัวอย่าง
df <- data.frame(
  gender = c("Male", "Female", "Male", "Female", "Male"),
  score = c(90, 85, 88, 92, 75)
)

# คำนวณค่าเฉลี่ยของคะแนน โดยแยกตามเพศ
library(dplyr)
df %>%
  group_by(gender) %>%
  summarise(avg_score = mean(score))

# การสร้างคอลัมน์ใหม่ด้วย mutate()
library(dplyr)
df %>%
  mutate(score_percent = score/100)

# การจัดการเงื่อนไขหลายระดับด้วย case_when()
library(dplyr)
df %>%
  mutate(performance = case_when(
    score >= 90 ~ "Excellent",
    score >= 80 ~ "Good",
    T ~ "Needs Improvement"
  ))

# การใช้ across() เพื่อจัดการหลายคอลัมน์พร้อมกัน
library(dplyr)
df %>%
  mutate(across(where(is.numeric), ~ . / 10))


# Lesson 14 การใช้ group_by() และ summarise() ---------------------------------------------------------------

# group_by()
  # สร้างชุดข้อมูลตัวอย่าง
df_groupby <- data.frame(
  category = c("A", "A", "B", "B", "A", "B"),
  value = c(10, 20, 30, 40, 50, 60)
)

library(dplyr)
df_groupby %>%
  group_by(category)

# summarise() เพื่อสรุปข้อมูลในแต่ละกลุ่ม หลังจาก group_by()
library(dplyr)
df_groupby %>%
  group_by(category) %>%
  summarise(
    mean_value = mean(value),
    max_value = max(value),
    min_value = min(value)
  )

# การใช้ n() ใน summarise()
library(dplyr)
df_groupby %>%
  group_by(category) %>%
  summarise(count = n())  # จำนวนช้อมูลแต่ละกลุ่มจะถูกนับให้เป็น n


# Lesson 15 การใช้ filer() และ arrange() ใน dplyr ---------------------------------------------------------------

# filter() คัดกรองแถวของข้อมูลตามเงื่อนไข
  # สร้างชุดข้อมูลตัวอย่าง
df_filter <- data.frame(
  id = 1:6,
  category = c(rep(c("A", "B"), times = 3)),
  value = c(10, 50, 30, 80, 60, 90)
)

library(dplyr)
df_filter %>%
  filter(value > 50)  # เลือกเฉพาะแถวที่ value > 50

  # คัดกรองหลายเงื่อนไขพร้อมกัน
    # เลือกข้อมูลที่มี value > 30 และ อยู่ในกลุ่ม A
library(dplyr)
df_filter %>%
  filter(value > 30 & category == "A")

    # เลือกช้อมูลที่อยู่ในกลุ่ม A หรือ มีค่า value > 80
library(dplyr)
df_filter %>%
  filter(category == "A" | value > 80)

    # เลือกเฉพาะข้อมูลที่ไม่อยู่ในกลุ่ม B
library(dplyr)
df_filter %>%
  filter(category != "B")

# arrange() เพื่อเรียงลำดับข้อมูล
  # เรียงจากค่าน้อยไปมาก (Ascending Order)
library(dplyr)
df_filter %>%
  arrange(value)

  # เรียงจากค่ามากไปน้อย
library(dplyr)
df_filter %>%
  arrange(desc(value))

  # เรียงตามหลายคอลัมน์
library(dplyr)
df_filter %>%
  arrange(category, desc(value))  # ข้อมูลจะถูกจับกลุ่มตาม category ก่อน แล้วเรียงตาม value จากมากไปน้อยภายในแต่ละกลุ่ม


# Lesson 16 สร้างกราฟแบบ Area Plot และการปรับแต่งขึ้นสูงด้วย ggplot2 ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างและเตรียมข้อมูล
set.seed(123)
sales_data <- data.frame(
  month = rep(month.abb[1:12], times = 3),
  product = rep(c("Laptop", "Smartphone", "Tablet"), each = 12),
  sales = pmax(round(c(
    rnorm(12, mean = 50, sd = 10),   # Laptop
    rnorm(12, mean = 80, sd = 15),   # Smartphone
    rnorm(12, mean = 30, sd = 5)     # Tablet
  )), 0)
) %>%
  mutate(
    month = factor(month, levels = month.abb[1:12]),
    product = factor(product)
  ) %>%
  arrange(month, product)

# ขั้นตอนที่ 2: สร้างกราฟ
library(ggplot2)
library(viridis)

ggplot(sales_data, aes(x = month, y = sales, fill = product, group = product)) +
  geom_area(alpha = 0.7, position = "stack", color = "white", size = 0.3) +
  scale_fill_viridis(discrete = TRUE, option = "inferno") +  # plasma, magma, inferno
  scale_x_discrete(limits = month.abb[1:12]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # ปรับขอบเขตแกน Y แบบปลอดภัย
  labs(
    title = "ยอดขายสินค้าเทคโนโลยีรายเดือน",
    x = "เดือน",
    y = "ยอดขาย (ล้านบาท)",
    fill = "ประเภทสินค้า"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),   # จัดให้ text สเกลแกน x เอียง 45 องศา
    panel.background = element_rect(fill = "gray90")
  )  


# Lesson 17 สร้างกราฟแบบ Radar Chart (Spider Chart) ใน R ---------------------------------------------------------------

# ขั้นตอนที่ 1: เตรียมข้อมูลสำหรับ Radar Chart
  # สร้างชุดข้อมูลตัวอย่าง
employee_skills <- data.frame(
  employee = c("John", "Sarah", "Mike"),  # ชื่อพนักงาน
  programming = c(8, 6, 9),               # คะแนนทักษะ Programming
  analysis = c(7, 9, 6),                  # คะแนนทักษะ Analysis
  design = c(6, 8, 5),                    # คะแนนทักษะ Design
  communication = c(9, 7, 6),             # คะแนนทักษะ Communication
  teamwork = c(8, 9, 7)                   # คะแนนทักษะ Teamwork
)

  # เตรียม object ค่าสเกล max AND min เพื่อสร้าง radar graph
max_min <- data.frame(
  programming = c(10, 0),
  analysis = c(10, 0),
  design = c(10, 0),
  communication = c(10, 0),
  teamwork = c(10, 0)
)

# รวมข้อมูลกับค่าจริง (ตัวอย่างข้อมูล John)
radarplot_data <- rbind(max_min,
                        employee_skills[1, -1],
                        employee_skills[2, -1],
                        employee_skills[3, -1]
                        )

  # ปรับข้อมูลให้อยู่ในรูปแบบที่ ggradar ต้องการ
library(RColorBrewer)
color <- brewer.pal(n = 3, name = "Set3")
alpha_values <- c(0.7, 0.5, 0.3)

library(fmsb)
radarchart(
  radarplot_data,
  pfcol = mapply(function(col, alpha) adjustcolor(col, alpha = alpha), 
                 color, alpha_values),  # ใช้สีจาก RColorBrewer + alpha
  pcol = color,  # สีเส้นตรงนี้จะใช้สีหลักจาก palette
  plwd = 2,
  title = "ทักษะพนักงาน (3 คน)",
  cglcol = "#E0E0E0",  # สีเส้นกริด (เทาอ่อน)
  cglty = 1,            # เส้นกริดแบบทึบ
  axislabcol = "#555555" # สีข้อความแกน
  
)

# เพิ่ม legend
legend("topright", 
       legend = employee_skills$employee,
       col = c("#1F77B4", "#FF7F0E", "#2CA02C"),
       lwd = 2,
       fill = c(rgb(0.2, 0.5, 0.9, 0.5), rgb(1, 0.5, 0, 0.5), rgb(0, 0.8, 0, 0.5)),
       border = NA
)


# Lesson 18 การสร้างกราฟแบบเคลื่อนไหว (Animated Visualization) ด้วย gganimate ---------------------------------------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
sales_data_animated <- data.frame(
  year = rep(2020:2023, each = 3),
  product = rep(c("A", "B", "C"), times = 4),
  sales = c(50, 70, 30, 60, 80, 40, 70, 90, 50, 80, 100, 60)
)

# ขั้นตอนที่ 2: สร้างกราฟพื้นฐานด้วย ggplot2
library(ggplot2)
library(RColorBrewer)
p_animated <- ggplot(sales_data_animated, aes(x = product, y = sales, fill = product)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sales Growth Over Years",
    x = "Product",
    y = "Sales",
    fill = "Type of Product"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_rect(fill = "gray90"),
    panel.grid = element_line(color = "gray"),
    legend.background = element_rect(fill = "gray90")
  )

p_animated

# ขั้นตอนที่ 3: เพิ่มการเคลื่อนไหวด้วย ggannimate
library(gganimate)
p_animated <- p_animated +
  transition_time(year) + # กำหนดตัวแปรจากเวลา (ปี)
  ease_aes("linear") +  # กำหนดความเร็วการเคลื่อนไหว
  enter_fade() +  # ให้ข้อมูลเก่าจางหาย
  exit_shrink() # ให้ข้อมูลใหม่โผล่มาแบบขยายตัว

  # แสดงผลกราฟเคลื่อนไหว
library(av)
animate(p_animated,
        duration = 5,
        fps = 20,
        width = 800,
        height = 600,
        res = 144, # ความละเอียด (resolution)
        renderer = av_renderer()
)
anim_save("sales_animation.mp4")


# Lesson 19 สร้างแผนที่เชิงพื้นที่ (Geospatial Mapping) ด้วย ggplot2 และ sf ---------------------------------------------------------------

# ขั้นตอนที่ 1: โหลดข้อมูลแผนที่โลก
library(rnaturalearth)
library(sf)
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# ขั้นตอนที่ 2: สร้างแผนที่พื้นฐาน
library(ggplot2)
ggplot() +
  geom_sf(data = world_map, fill = "lightgray", color = "black") +
  labs(title = "World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "gray90"),
  )

# ขั้นตอนที่ 3: เพิ่มข้อมูลจุดสถานที่สำคัญ
landmarks <- data.frame(
  name = c("Big Ben", "Statue of Liberty", "Eiffel Tower"),
  lat = c(51.507, 40.6892, 48.8584),
  lon = c(-0.1246, -74.0445, 2.2945)
)

  # แปลงเป็น Spatial Object
library(sf)
landmarks_sf <- st_as_sf(landmarks, coords = c("lon", "lat"), crs = 4326)

  # วาดแผนที่พร้อมจุดสถานที่
library(ggplot2)
ggplot() +
  geom_sf(data = world_map, fill = "lightgray", color = "black") +
  geom_sf(data = landmarks_sf, color = "red", size = 3, shape = 21, fill = "yellow") +
  labs(title = "Crucial Location in World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "gray90"),
  )

# ขั้นตอนที่ 4: ระบายสีตามข้อมูล (Choropleth Map)
  # เพิ่มข้อมูลประชากร
world_map$population <- sample(1e7:1e9, nrow(world_map), replace = TRUE)

  # สร้าง Choropleth Map
library(ggplot2)
library(viridis)
ggplot() +
  geom_sf(data = world_map, aes(fill = population), color = "black") +
  geom_sf(data = landmarks_sf, color = "red", size = 3, shape = 21, fill = "red") +
  scale_fill_viridis_c(name = "Population", option = "C") +
  labs(title = "Crucial Location in World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "gray90"),
    legend.position = "bottom"
  )


# Lesson 20 สร้างกราฟ 3 มิติ (3D Visualization) ด้วย plotly ---------------------------------------------------------------

### 3D Virsualization จะต้องใช้ syntax ของ plotly เท่านั้น ###

# ขั้นตอนที่ 1:สร้างชุดข้อมูล 3 มิติ
student_3d_data <- data.frame(
  study_hours = 1:10,
  exam_score = seq(45, 90, by = 5),
  stress_level = seq(90, 45, by = -5)
)

# ขั้นตอนที่ 2: สร้าง 3D Scatter Plot
library(plotly)
library(viridis)
library(dplyr)
fig_3d <- plot_ly(student_3d_data,
                  x = ~study_hours,
                  y = ~exam_score,
                  z = ~stress_level,
                  type = "scatter3d",
                  mode = "markers",
                  marker = list(
                    size = 5,
                    color = ~exam_score,   # กำหนดสีตามคะแนนสอบ
                    colorscale = "Viridis",
                    opacity = 0.8
                  )) %>%
  layout(
    scene = list(
      xaxis = list(title = "Study Hours"),
      yaxis = list(title = "Exam Score"),
      zaxis = list(title = "Stress Level")
    ),
    title = "3D Relationship: Study, Score, and Stress"
  )

fig_3d

# ขั้นตอนที่ 3: สร้าง 3D Surface Plot (สำหรับข้อมูลแบบตาราง)
  # สร้างชุดข้อมูลตัวอย่าง (ความสัมพันธ์ระหว่าง X-Y-Z)
x <- seq(0, 10, length.out = 20)
y <- seq(0, 10, length.out = 20)
z_matrix <- outer(x, y, function(a, b) sin(a) * cos(b))


  # สร้าง Surface Plot
library(plotly)
library(dplyr)
fig_surface <- plot_ly(
                       x = x,
                       y = y,
                       z = z_matrix,
                       type = "surface",
                       color = ~z_matrix,   # แสดงสีตามค่า z_matrix
                       colors = colorRamp(c("blue", "red"))   # กำหนดสีแบบ gradient
                       ) %>%
  layout(
    scene = list(
      xaxis = list(title = "X Axis"),
      yaxis = list(title = "Y Axis"),
      zaxis = list(title = "Z Axis")
    ),
    title = "3D Surface Plot: sin(x) * cos(y)"
  )

fig_surface


# Lesson 21 Machine Learning ---------------------------------------------------------------

# ทำนายคะแนนสอบด้วย Linear Regression
  # ขั้นตอนที่ 1: โหลดข้อมูลจาก Lesson 3 เพื่อสร้างโมเดล
student_data   # from Lesson 3

  # ขั้นตอนที่ 2: แบ่งข้อมูลออกเป็น Training sets และ Testing sets
library(caret)
set.seed(123)
train_index <- sample(1:nrow(student_data), size = floor(0.8 * nrow(student_data))) # สุ่ม 80% ของข้อมูลไว้ให้ train_data
train_data <- student_data[train_index, ]
test_data <- student_data[-train_index, ]

  # ขั้นตอนที่ 3: สร้างโมเดล Linear Regression
model_linear <- lm(exam_score ~ study_hours, data = train_data)
summary(model_linear)  # ดูผลลัพธ์

  # ขั้นตอนที่ 4: ทำนายผลและประเมินโมเดล
prediction_linear <- predict(model_linear, test_data)
prediction_linear   # ทำนายผล
library(caret)
postResample(prediction_linear, test_data$exam_score)  # ดู RMSE, R-squared

# ลองทำ Classification (จำแนกกลุ่ม)
  # ขั้นตอนที่ 1: โหลดข้อมูลจาก Lesson 4 เพื่อทำนายว่าเป็น "Male" หรือ "Female"
student_data_boxplot   # from Lesson 4

  # ขั้นตอนที่ 2: แปลง gender เป็น Factor
student_data_boxplot$gender <- as.factor(student_data_boxplot$gender)

  # ขั้นตอนที่ 3: สร้างโมเดล Logistic Regression
model_logistic <- glm(gender ~ exam_score, data = student_data_boxplot, family = "binomial")
summary(model_logistic) # ดูผลลัพธ์

  # ขั้นตอนที่ 4: ทำนายผล
prediction_logistic <- predict(model_logistic, type = "response")
prediction_logistic <- ifelse(prediction_logistic > 0.5, "Female", "Male")
table(Predicted = prediction_logistic, Actual = student_data_boxplot$gender)

# ฝึกใช้ Library ยอดนิยม อย่าง caret
  # ขั้นตอนที่ 1: สร้างโมเดล
library(caret)
library(randomForest)
model_rf <- randomForest(exam_score ~ study_hours, data = train_data, ntree = 50)
model_rf   # ดูผลลัพธ์

  # ขั้นตอนที่ 2: ทำนายผลและประเมินผลโมเดล
prediction_rf <- predict(model_rf, test_data)
prediction_rf   # ทำนายผล
postResample(prediction_rf, test_data$exam_score) # ประเมินโมเดล

# ฝึกใช้ tidymodels (Modern Approach)
  # ขั้นตอนที่ 1:กำหนดโมเดล
library(tidymodels)
library(dplyr)

model_linear_tidy <- linear_reg() %>%
  set_engine("lm")

  # ขั้นตอนที่ 2: สร้าง Workflow
library(dplyr)
linear_workflow <- workflow() %>%
  add_model(model_linear_tidy) %>%
  add_formula(exam_score ~ study_hours)

  # ขั้นตอนที่ 3: ฝึกโมเดล
library(dplyr)
linear_fit <- linear_workflow %>%
  fit(data = train_data)

# ขั้นตอนที่ 4: ทำนายผล
predict(linear_fit, test_data)


# Lesson 22 การทำงานกับข้อมูลลำดับชีวภาพ (Biological Sequence Da --------

# ขั้นตอนที่ 1: อ่านไฟล์นี้เข้ามาใน RStudio
  # ตัวอย่างไฟล์ FASTA
# my_dna_sequences.fasta
# >seq1 Description of sequence 1
# ATGCAGTAGC
# >seq2 Another sequence
# GCTAGCTAGC
# >seq3 A third one
# CGATCGATCG

library(Biostrings)
dna_sequences <- readDNAStringSet("C:/Old Volume/Work/งานนอก/R Coding/my_dna_sequences.fasta")

# ขั้นตอนที่ 2: ตรวจสอบ Object DNAStringSet
library(Biostrings)
names(dna_sequences)  # ดูชื่อ header ของลำดับ
width(dna_sequences)  # ดูความยาวของแต่ละลำดับ >>> 10 base = 10 nucleotide
dna_sequences[[1]]  # เข้าถึงลำดับแรก

# ขั้นตอนที่ 3: การดำเนินการพื้นฐานกับลำดับ
library(Biostrings)
letterFrequency(dna_sequences[[1]], letters = c("A", "T", "G", "C"))  # นับความถี่ของเบส
subseq(dna_sequences[[2]], start = 3, end = 7)  # ตัดต่อลำดับเพื่อดึงส่วนย่อยของลำดับ
reverseComplement(dna_sequences[[3]])  # หาลำดับคู่สมที่กลับด้าน (Reverse Complement)

# ขั้นตอนที่ 4: การสร้าง DNAStringSet Object
library(Biostrings)
my_dna <- DNAStringSet(c("seq_a" = "ACGTACGT",
                         "seq_b" = "CGTACGTA"
                         )
                       )

# ขั้นตอนที่ 5: การเขียน XStringSet Object ลงในไฟล์ FASTA
writeXStringSet(my_dna, file = "C:/Old Volume/Work/งานนอก/R Coding/my_new_sequences.fasta")

# ขั้นตอนที่ 5: การอ่านไฟล์ FASTQ
  # ตัวอย่างไฟล์ FASTQ
#' @SEQ_ID1
#' GATACA
#' +
#'   ~!~~~~
#'   @SEQ_ID2
#' AGCTTG
#' +
#'   IIIIII

library(ShortRead)
fastq_sequences <- readFastq("C:/Old Volume/Work/งานนอก/R Coding/my_sequences.fastq")

  # การเข้าถึงข้อมูลใน Object ShortReadQ
library(ShortRead)
sread(fastq_sequences)  # เข้าถึงลำดับ (Sequence)
quality(fastq_sequences)  # เข้าถึงคุณภาพ (Quality Scores)
id(fastq_sequences) # เข้าถึง Identifier (ID)

# ขั้นตอนที่ 6: การทำ Sequence Alignment เบื้องต้นด้วย Biostrings
  # ทำ Pairwaise Alignment จากชุดข้อมูลตัวอย่าง
library(Biostrings)
sequence1 <- DNAString("GATTACA")
sequence2 <- DNAString("GATCA")

library(pwalign)
pairwise_alignment <- pairwiseAlignment(sequence1, sequence2)
pairwise_alignment   # เช็คผลการทำ alignment

  # เข้าถึงข้อมูลเฉพาะส่วนจากการทำ Alignment
alignedPattern(pairwise_alignment)  # ส่วนที่ถูกจัดเรียงของ sequence ที่ 1
alignedSubject(pairwise_alignment)  # ส่วนที่ถูกจัดเรียงของ sequence ที่ 2

  # การดูคะแนนของการทำ Alignment
score(pairwise_alignment)

# ขั้นตอนที่ 7: การทำ Multiple Sequence Alignment เบื้องต้น
  # สร้างชุดข้อมูลตัวอย่าง 4 sequences
library(Biostrings)
sequences_to_align <- DNAStringSet(
  c("seqA" = "ACGTACGTA",
    "seqB" = "ACGT--GTA",
    "seqC" = "ACGTAGG-A",
    "seqD" = "ACGTACGGA"
  )
)

  # ทำ Multiple Sequence Alignment
library(msa)
multiple_alignment <- msa(sequences_to_align, method = "ClustalW")
multiple_alignment

  # การดูผลลัพธ์การ Alignment
alignment_matrix <- as.character(multiple_alignment)
alignment_matrix

# ขั้นตอนที่ 8: การคำนวณ Distance Matrix จาก sequence ที่ถูก Alignment แล้ว
  # แปลง MSA Object กลับเป็น DNAStringSet
aligned_sequences <- as(multiple_alignment, "DNAStringSet")
class(aligned_sequences)

  # คำนวณ Distance Matrix
library(pwalign)
distance_matrix <- stringDist(aligned_sequences)
distance_matrix

# ขั้นตอนที่ 9: การสร้าง Phylogenetic Tree เบื้องต้นจาก Distance Matrix

  # สร้าง Phylogenetic Tree โดยใช้วิธี Neighbor-Joining (NJ)
library(ape)
phylogenetic_tree_nj <- nj(distance_matrix)
phylogenetic_tree_nj   # แสดงผลลัพธ์ (โครงสร้างของ Tree)

plot(phylogenetic_tree_nj, main = "Plylogenetic Tree") # วาดรูป Phylogenetic Tree

  # สร้าง Phylogenetic Tree โดยใช้วิธี UPGMA >>> ต้องไปใช้ phangorn เพราะว่า ape ติด error
library(phangorn)
phylogenetic_tree_upgma_phangorn <- upgma(distance_matrix)
phylogenetic_tree_upgma_phangorn   # แสดงผลลัพธ์ (โครงสร้างของ Tree)

plot(phylogenetic_tree_upgma_phangorn, main = "Phylogenetic Tree (UPGMA - phangorn)") # วาดรูป Phylogenetic Tree


# Lesson 23 การแสดงผล Phylogenetic Tree ให้สวยงามด้วย ggtree --------------

# ขั้นตอนที่ 1: สร้าง ggtree Object จาก Phylogenetic Tree ที่สร้างด้วย phangorn
library(ggtree)
tree_plot <- ggtree(phylogenetic_tree_upgma_phangorn)

# ขั้นตอนที่ 2: เพิ่ม Labels ให้กับ Tips ของ Tree
library(ggtree)
tree_plot_labeled <- tree_plot +
  geom_tiplab()

print(tree_plot_labeled)

# ขั้นตอนที่ 3: ปรับแต่งรูปลักษณ์ของ Tree
  # [ตัวอย่าง] เปลี่ยนสีของกิ่งเป็นสีน้ำเงิน
library(ggtree)
tree_plot_colored <- tree_plot +
  geom_tree(color = "blue")

print(tree_plot_colored)

  # [ตัวอย่าง] เปลี่ยน Layout เป็นแบบวงกลม
library(ggtree)
tree_plot_circular <- ggtree(phylogenetic_tree_upgma_phangorn, layout = "circular") +
  geom_tiplab()

print(tree_plot_circular)

# ขั้นตอนที่ 4: แสดงความยาวของกิ่ง (Branch Lengths)
library(ggtree)
tree_plot_with_lengths <- ggtree(phylogenetic_tree_upgma_phangorn)

  # เพิ่ม Scale Bar เพื่อแสดงความยาวของกิ่ง
library(ggtree)
tree_plot_with_lengths <- tree_plot +
  geom_treescale()

print(tree_plot_with_lengths)

  # ปรับแต่ง Scale Bar
library(ggtree)
tree_plot_with_lengths_custom <- tree_plot +
  geom_treescale(x = 0, y = 0, color = "red", linesize = 1, fontsize = 4) +
  geom_tiplab() # เพิ่มชื่อ Taxa กลับเข้าไปด้วย

print(tree_plot_with_lengths_custom)

# ขั้นตอนที่ 5: การเปลี่ยน Layout ของ Tree
  # Layout แบบ Rectangular (ค่าเริ่มต้น)
library(ggtree)
tree_plot_rectangular <- ggtree(phylogenetic_tree_upgma_phangorn, layout = "rectangular") +
  geom_treescale() +
  geom_tiplab()

print(tree_plot_rectangular)

  # Layout แบบ Circular
library(ggtree)
tree_plot_circular2 <- ggtree(phylogenetic_tree_upgma_phangorn, layout = "circular") +
  geom_tiplab(aes(angle = angle)) + # เพิ่ม angle ให้ label เพื่อให้อ่านง่ายใน layout วงกลม
  geom_treescale(x = 0, y = 1)  # ปรับแต่ง scale bar นิดหน่อย

print(tree_plot_circular2)

  # Layout แบบ Fan
library(ggtree)
tree_plot_fan <- ggtree(phylogenetic_tree_upgma_phangorn, layout = "fan") +
  geom_tiplab(align = TRUE) + # จัดให้ label ชิดขอบ
  geom_treescale(x = 0, y = 1)  # ปรับตำแหน่ง scale bar นิดหน่อย

print(tree_plot_fan)

# ขั้นตอนที่ 6: การเพิ่ม Label ให้กับ Internal Nodes
  # สร้าง ggtree object (ใช้ layout แบบ rectangular เพื่อให้เห็นชัดเจน)
library(ggtree)
tree_plot_with_nodelab <- ggtree(phylogenetic_tree_upgma_phangorn, layout = "rectangular") +
  geom_tiplab() +
  geom_treescale() +

  # เพิ่ม Label แบบระบุ Node ID
  geom_text2(aes(subset = (node == 5), label = "Node 5"), hjust = -0.5, vjust = -0.5, size = 3, color = "red") +
  geom_text2(aes(subset = (node == 6), label = "Node 5"), hjust = -0.5, vjust = -0.5, size = 3, color = "blue") +
  geom_text2(aes(subset = (node == 7), label = "Node 5"), hjust = -0.5, vjust = -0.5, size = 3, color = "green") 
  
print(tree_plot_with_nodelab) # แสดงผลลัพธ์

# ขั้นตอนที่ 7: ปรับแต่งลักษณะของกิ่ง (Branches)
  # เปลี่ยนสีของกิ่งทั้งหมด
library(ggtree)
tree_plot_branch_color <- ggtree(phylogenetic_tree_upgma_phangorn) +
  geom_tiplab() +
  geom_treescale() +
  geom_nodelab() +
  geom_tree(color = "purple") # เปลี่ยนสีของกิ่งเป็นสีม่วง

print(tree_plot_branch_color)

  # เปลี่ยนชนิดของเส้นของกิ่งทั้งหมด
library(ape)
library(ggtree)
simple_tree <- read.tree(text = "((A:1, B:1):2, C:3);") # สร้าง Tree Object อย่างง่าย

plot_simple_tree <- ggtree(simple_tree, linetype = "dashed") +
  geom_tiplab() +
  geom_treescale()

print(plot_simple_tree)

# ขั้นตอนที่ 8: การตกแต่ง Tree เพื่อการนำเสนอ
  # เน้นปรับ Tip Labels
library(ggtree)
ggtree(simple_tree) +
  geom_tiplab(color = "darkgreen", size = 4, fontface = "italic")

  # ปรับรูปร่างเส้นกิ่ง (linetype, สี, ความหนา)
library(ggtree)
ggtree(simple_tree, linetype = "dashed") +
  geom_tiplab()

  # การเพิ่ม Bootstrap/Support values
library(ggtree)
library(ape)
library(ggplot2)

simple_tree <- read.tree(text = "((A:1, B:1):2, C:3);")

simple_tree$node.label <- c("95", "80")  # ต้องใส่แค่ internal nodes เท่านั้น!!

plot_bootstrap <- ggtree(simple_tree) + 
  geom_tiplab() +
  geom_treescale() +
  labs(title = "Phylogenetic Tree with Bootstrap Values") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

plot_bootstrap$data  # ตรวจสอบ node label ว่ามีหรือไม่

bootstrap_data <- subset(plot_bootstrap$data, !isTip & !is.na(label))  # ดึงเฉพาะ internal nodes ที่มี label

plot_bootstrap <- plot_bootstrap + geom_text2(data = bootstrap_data,   
                    aes(label = label),
                    color = "blue",
                    size = 3,
                    hjust = -0.3,
                    vjust = 0.5)

print(plot_bootstrap)


# Lesson 24 การ Annotate ข้อมูลเพิ่มเติมลงบน Phylogenetic Tree  --------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่างต้นไม้และข้อมูลประกอบ
library(ape)
library(tibble) # เพื่อเตรียมข้อมูล
tree <- read.tree(text = "((A:1, B:1):2, (C:1, D:1):2);") # ข้อมูลโครงสร้าง tree
data <- tibble::tibble(   # ข้อมูลประกอบให้กับ tree
  label = c("A", "B", "C", "D"),
  expression = c(3.4, 2.1, 4.5, 1.8),
  group = c("X", "X", "Y", "Y")
)
data

# ขั้นตอนที่ 2: วาดต้นไม้และใส่ Barplot ด้วย ggtreeExtra
library(ggtree)
library(ggtreeExtra)
plot_ggtreeExtra <- ggtree(tree, layout = "circular") %<+% data + # ใช้ operator %<+% เพื่อนำ data object มาสร้าง tree
  geom_fruit(   # ใช้เพื่อเพิ่มกราฟเสริมให้กับต้นไม้ (เช่น barplot)
    geom = geom_bar,
    mapping = aes(x = expression, fill = group),
    stat = "identity",
    width = 0.6,
    offset = 0.05
  ) +
  scale_fill_manual(values = c("X" = "#FF9999", "Y" = "#9999FF")) +
  labs(fill = "Group") +
  theme(
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "gray90"),
    legend.position = "right")

print(plot_ggtreeExtra) # แสดงผล


# Lesson 25 การเพิ่ม Annotation หลายชั้นให้กับ Phylogenetic Tree --------

# ขั้นตอนที่ 1: สร้าง tree
library(ggtree)
tree <- read.tree(text = "((A:1, B:1):2, (C:1, D:1):2);")

# ขั้นตอนที่ 2: สร้างข้อมูล barplot และ heatmap
data <- data.frame(
  taxa = c("A", "B", "C", "D"),
  expression = c(10, 15, 8, 20),
  activity = c(0.5, 0.9, 0.7, 0.3),
  group = c("X", "X", "Y", "Y")
)

# ขั้นตอนที่ 3: วาดต้นไม้และใส่ annotation หลายแบบ
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
plot_multiple_annotation <- ggtree(tree, layout = "circular") %<+% data +
  geom_tiplab(size = 3) +
  geom_fruit(
    geom = geom_bar,
    mapping = aes(x = expression, fill = group),
    stat = "identity",
    width = 0.6,
    offset = 0.05,
    inherit.aes = FALSE   # ปิดการแชร์ aes(x และ y)
  ) +
  scale_fill_manual(name = "Group", values = c("X" = "#FF9999", "Y" = "#9999FF")) +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(x = "Activity", y = taxa, color = activity),
    width = 0.2,
    offset = 0.15,
    inherit.aes = FALSE   # ปิดการแชร์ aes(x และ y)
  ) +
  scale_color_gradient(name = "Activity", low = "#FFEDA0", high = "#F03B20") +
  
  # ggplot2 ไม่สามารถใช้ fill พร้อมกันได้ 2 function/parameter กันภายใน 1 กราฟ >>> แยกเป็น fill & color
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
    )

print(plot_multiple_annotation) # แสดงผล


# Lesson 26 Ancestral State Reconstruction (ASR) on Phylogenetic Trees using --------

# ขั้นตอนที่ 1: เตรียม Tree และ Trait Data
library(ape)
library(ggtree)
library(dplyr)

# สร้าง tree ตัวอย่าง (4 taxa)
tree <- read.tree(text = "((A:1, B:1):2, (C:1, D:1):2);")
plot(tree, main = "Example Tree")

# สร้างข้อมูลลักษณะ (Discrete Trait) สำหรับแต่ละ tip
states <- factor(c("A", "A", "B", "B"))
names(states) <- tree$tip.label
print(states)

# ขั้นตอนที่ 2: ทำ ASR ด้วย Maximum Likelihood (ML)
# ใช้ฟังก์ชัน ace() จาก ape
asr_ml <- ace(x = states, phy = tree, type = "discrete", model = "ER")  # ER = Equal Rates

# ดู likelihood ของแต่ละ state ที่ internal nodes
print("Likelihoods at internal nodes:")
asr_ml$lik.anc

# ขั้นตอนที่ 3: Visualization ด้วย Base R (Pie Chart at Nodes)
state_colors <- c("A" = "skyblue", "B" = "salmon")
plot(tree, main = "ASR (ML - ER model) with Pie at Nodes")
nodelabels(
  pie = asr_ml$lik.anc,
  piecol = state_colors[colnames(asr_ml$lik.anc)],
  cex = 0.8
)
legend("topright", legend = names(state_colors), fill = state_colors, title = "States", cex = 0.8)

# ขั้นตอนที่ 4: Visualization ด้วย ggtree (Node Points)
# เตรียมข้อมูลสำหรับ ggtree
# - tips: node 1-4
# - internal nodes: node 5-6
tip_states_df <- data.frame(
  node = 1:Ntip(tree),
  label = tree$tip.label,
  state = states[tree$tip.label]
)
internal_states_df <- as.data.frame(asr_ml$lik.anc) %>%
  mutate(node = (Ntip(tree) + 1):(Ntip(tree) + Nnode(tree))) %>%
  mutate(state = colnames(asr_ml$lik.anc)[apply(asr_ml$lik.anc, 1, which.max)]) %>%
  select(node, state)
combined_states <- bind_rows(
  tip_states_df %>% select(node, state),
  internal_states_df %>% select(node, state)
)

# วาด tree ด้วย ggtree และแสดง state ที่ tips และ internal nodes
library(ggplot2)
p_asr <- ggtree(tree) %<+% combined_states +
  geom_tiplab(offset = 0.1, align = TRUE) +
  geom_nodepoint(aes(color = state), size = 5, alpha = 0.8) +
  geom_tippoint(aes(color = state), size = 3) +
  scale_color_manual(values = state_colors, name = "State") +
  labs(title = "ASR (ML - ER model) with ggtree") +
  theme_tree2() +
  theme(legend.position = "right")

print(p_asr)

# ขั้นตอนที่ 5: ทดลองโมเดลวิวัฒนาการอื่น (SYM, ARD)
asr_ml_sym <- ace(x = states, phy = tree, type = "discrete", model = "SYM")
cat("\n--- Result for SYM model ---\n")
asr_ml_sym$lik.anc
asr_ml_sym$rate
print(paste("Log-likelihood (SYM):", asr_ml_sym$loglik))

# Visualization: Pie chart for SYM model
plot(tree, main = "ASR (ML - SYM model)") # ใช้ base R ในการ plot tree
nodelabels(
  pie = asr_ml_sym$lik.anc,
  piecol = state_colors[colnames(asr_ml_sym$lik.anc)],
  cex = 0.8 # ขนาดของ pie chart
)
legend("topright", legend = names(state_colors), fill = state_colors, title = "States", cex = 0.8)

# ขั้นตอนที่ 6: หมายเหตุและข้อเสนอแนะ
# - สามารถเปลี่ยน model เป็น "ARD" ได้เช่นกัน
# - สามารถนำไปประยุกต์กับ tree และ trait data ที่ซับซ้อนกว่าได้
# - หากต้องการแสดง likelihood เป็น pie chart ที่ internal nodes ใน ggtree สามารถใช้ geom_nodepie() จาก ggtreeExtra (ต้องติดตั้งเพิ่ม)


# Lesson 27 Beta Diversity & Ordination in R ------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูล Community Matrix ตัวอย่างอย่างง่าย
community_matrix <- data.frame(
  Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5"),
  SpeciesA = c(10, 5, 0, 2, 8),
  SpeciesB = c(0, 8, 15, 1, 5),
  SpeciesC = c(20, 10, 5, 0, 12),
  SpeciesD = c(5, 0, 8, 15, 1),
  SpeciesE = c(1, 2, 0, 10, 0),
  SpeciesF = c(0, 0, 1, 5, 7)
)

# ขั้นตอนที่ 2: ทำให้ Sample Column data เป็นชื่อแถว (row names) เพื่อให้ฟังก์ชันใน vegan ทำงานได้อย่างถูกต้อง
rownames(community_matrix) <- community_matrix$Sample

  # ลบ Sample Column ทิ้งไป
community_matrix$Sample <- NULL

# ขั้นตอนที่ 3: สร้างข้อมูล Sample Metadata อย่างง่าย
sample_metadata <- data.frame(
  Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5"),
  Environment = c("Forest", "Forest", "Grassland", "Grassland", "Forest")
)

# ขั้นตอนที่ 4: ทำให้ชื่อกลุ่มตัวอย่างเป็นชื่อแถว
rownames(sample_metadata) <- sample_metadata$Sample

  # ลบ Sample Column ทิ้งไป
sample_metadata$Sample <- NULL

# ขั้นตอนที่ 5: ตรวจสอบข้อมูลที่สร้างขึ้น
print("Community Matrix:")
community_matrix

cat("\n") # ใช้ cat() เพื่อพิมพ์บรรทัดว่าง

print("Sample Metadata:")
print(sample_metadata)

# ขั้นตอนที่ 6: คำนวณ Beta Diversity Distance Matrix ด้วยวิธี Bray-Curtis
library(vegan)
beta_diversity_matrix <- vegdist(community_matrix, method = "bray")

  # ตรวจสอบผลลัพธ์ Distance Matrix ที่ได้
print("Beta Diversity Distance Matrix (Bray-Curtis):")
beta_diversity_matrix

# ขั้นตอนที่ 7: ทำ Ordination (Principle Co-ordinates Analysis - PCoA/MDS)
library(vegan)
ordination_result <- wcmdscale(beta_diversity_matrix, k = 2) # จำนวนมิติที่ต้องการแสดงผล นิยมใช้ 2 มิติ

  # ตรวจสอบผลลัพธ์ของ Ordination
print("Ordination Result (PCoA Coordinates):")
ordination_result

# ขั้นตอนที่ 8: วาดกราฟ Ordination (PCoA Plot) ด้วย ggplot2
  # แปลง ordination_result ให้เป็น dataframe
ordination_data <- data.frame(
  Sample = rownames(ordination_result),
  PCoA1 = ordination_result[, 1],   # Coordinates บน Axis 1
  PCoA2 = ordination_result[, 2]   # Coordinates บน Axis 2
)

# รวมข้อมูล Ordination กับ Sample Metadata
  # นำ Sample rownames กลับมาเป็น colnames
library(tibble)
sample_metadata_with_col <- rownames_to_column(sample_metadata, var = "Sample")

  # รวม data ของ sample_metadata_with_col
library(dplyr)
ordination_data <- left_join(ordination_data, sample_metadata_with_col, by = "Sample")

library(ggplot2)
pcoa_plot <- ggplot(ordination_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(color = Environment), size = 3, stroke = 1.5) +  # Plot จุด ใช้สีตามค่าในคอลัมน์ Environment
  labs(
    title = "PCoA Plot of Community Samples",
    x = "PCoA Axis 1",
    y = "PCoA Axis 2",
    color = "Environment"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 0.75),
    legend.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(pcoa_plot)  # แสดงผลกราฟ

# ขั้นตอนที่ 9: ประเมินผลลัพธ์ Ordination (ดูค่า Eigenvalues และ Variance Explained)
  # ทำ Ordination โดยใช้ PCoA (wcmdscale) จาก Distance Matrix เพื่อให้ได้ Eigenvalues
library(vegan)
ordination_result_wcmd <- wcmdscale(beta_diversity_matrix, k = 2, eig = TRUE) # eig = TRUE คือให้คำนวณ eigenvalues ด้วย

print("Ordination Result (PCoA Coordinates from wcmdscale):")
ordination_result_wcmd$points # $points คือ coordinates ใน k มิติที่เราเลือก

  # เก็บค่า Eigenvalues จาก wcmd object
eigenvalues <- ordination_result_wcmd$eig

  # คำนวณสัดส่วนของ Variance ที่อธิบายโดยแต่ละแกน (Percentage of Variance Explained)
variance_explained <- eigenvalues / sum(eigenvalues)

  # ดูสัดส่วนของ Variance ที่อธิบายโดยแกนแรก ๆ
print("Proportion of Variance Explained by Each Axis:")
round(variance_explained, 3) # ปัดเศษให้ดูง่าย

  # คำนวณสัดส่วนของ Variance ที่อธิบายโดยสองแกนแรก (Axis 1 และ Axis 2)
variance_explained_first_two_axes <- sum(variance_explained[1:2])
print(paste("Total Proportion of Variance Explained by the first two axes:", round(variance_explained_first_two_axes, 3)))

  # ค่าสะสมของ Variance Explained (Cumulative Variance Explained)
cumulative_variance_explained <- cumsum(variance_explained)
print("Cumulative Proportion of Variance Explained:")
print(round(cumulative_variance_explained, 3))

# ขั้นตอนที่ 10: ทดสอบทางสถิติสำหรับ Beta Diversity (เช่น PERMANOVA)
library(vegan)
permanova_result <- adonis2(beta_diversity_matrix ~ Environment, data = sample_metadata_with_col)

  # ตรวจสอบผลลัพธ์ของ PERMANOVA
print("PERMANOVA Result:")
permanova_result  # ถ้าหาก Pr(>F) < 0.05 จะถือว่ามีความแตกต่างขององค์ประกอบกันอย่างมีนัยสำคัญ (P-value)

# ขั้นตอนที่ 11: เพิ่มค่า P-value จาก PERMANOVA ลงบนกราฟ Ordination Plot
  # ดึงค่า P-value จากผลลัพธ์ PERMANOVA
permanova_pvalue <- permanova_result$`Pr(>F)`[1] # ดึงค่า P-value ของ row ที่ 1 เท่านั้น

  # สร้างข้อความสำหรับแสดงบนกราฟ
pvalue_text <- paste0("PERMANOVA P-value = ", round(permanova_pvalue, 3)) # ใช้ paste0 เพื่อรวมข้อความกับค่า P-value ที่ปัดเศษแล้ว

# เพิ่มข้อความ P-value ลงบนกราฟ PCoA และ เลือกตำแหน่งมุมใดมุมหนึ่งที่ไม่มีจุดข้อมูลเยอะ ๆ เช่น มุมขวาบน
pcoa_plot_with_pvalue <- pcoa_plot +
  annotate(
    "text",         # บอกว่าต้องการเพิ่มข้อความ
    x = max(ordination_data$PCoA1) * 0.9, # ตำแหน่งแกน x (อาจจะปรับค่า 0.9 ให้เหมาะสม)
    y = max(ordination_data$PCoA2) * 0.9, # ตำแหน่งแกน y (อาจจะปรับค่า 0.9 ให้เหมาะสม)
    label = pvalue_text,
    hjust = 1, vjust = 1 # จัดตำแหน่งข้อความ (hjust=1 คือชิดขวา, vjust=1 คือชิดบน)
  )

print(pcoa_plot_with_pvalue)  # แสดงผลลัพธ์

  # หากต้องการเพิ่ม % variance เข้าไปใน axis.title
percent_variance_axis1 <- round(variance_explained[1] * 100, 2) # คูณ 100 เพื่อเป็น %, ปัดทศนิยม 2 ตำแหน่ง
percent_variance_axis2 <- round(variance_explained[2] * 100, 2)  # คูณ 100 เพื่อเป็น %, ปัดทศนิยม 2 ตำแหน่ง

  # สร้างข้อความ label ให้แกน x และ แกน y
x_label_with_variance <- paste0("PCoA Axis 1 (", percent_variance_axis1, " %)")
y_label_with_variance <- paste0("PCoA Axis 2 (", percent_variance_axis2, " %)")

  # แก้ไขกราฟ PCoA Plot โดยเปลี่ยน Axis Labels ใน labs()
pcoa_plot_with_variance_labels <- pcoa_plot_with_pvalue +
  labs(
    title = "PCoA Plot of Community Samples",
    x = x_label_with_variance,
    y = y_label_with_variance,
    color = "Environment"
  )

print(pcoa_plot_with_variance_labels) # แสดงผลลัพธ์


# Lesson 28 การสร้างเอกสารแบบ Interactive และ Reproducible ด้วย R  --------

# **ลองทำ:**
#   
# 1.  สร้างไฟล์ R Markdown ใน RStudio ตามขั้นตอนข้างบนค่ะ/ครับ
# 2.  ลองอ่านเนื้อหาใน Template ที่ RStudio สร้างให้ดูค่ะ/ครับ
# 3.  สังเกตส่วนที่เป็น YAML Header, ส่วนที่เป็นข้อความธรรมดา, และส่วนที่เป็น Code Chunks (ที่อยู่ภายใน ````{r}` และ ```` `)
# 4.  ลองกดปุ่ม **Knit** (ที่เป็นรูปไหมพรม) ที่อยู่ด้านบนของหน้าต่าง Editor ดูค่ะ/ครับ RStudio จะประมวลผลไฟล์ .Rmd ของคุณ และสร้างเอกสาร Output ตามที่คุณเลือก (เช่น ไฟล์ .html) ขึ้นมา

# เมื่อกด Knit แล้วได้ผลลัพธ์เป็นเอกสารออกมา บอกพี่เจมี่ด้วยนะคะ/ครับ! 😊


# ลองทำ:
#   
# เปิดไฟล์ .Rmd ที่คุณสร้างไว้ในขั้นตอนที่ 1
# คัดลอก Markdown Syntax ตัวอย่างข้างบนนี้ไปวางต่อท้ายเนื้อหาเดิมในไฟล์ค่ะ/ครับ
# กดปุ่ม Knit อีกครั้ง (รูปไหมพรม)
# สังเกตผลลัพธ์ในเอกสาร Output (เช่น ไฟล์ HTML) ว่าข้อความที่คุณพิมพ์และสัญลักษณ์ Markdown ถูกจัดรูปแบบตามที่เราคาดหวังหรือไม่

# ลองทำ:
#   
# เปิดไฟล์ .Rmd ของคุณ
# 
# ลองเพิ่ม Code Chunk ใหม่ๆ ลงไปในเอกสารค่ะ/ครับ (อาจจะต่อจากส่วน Markdown ที่คุณฝึกไปแล้ว)
# 
# ในแต่ละ Code Chunk ลองใส่โค้ด R ง่ายๆ ที่คุณเคยใช้ เช่น
# 
# โค้ดคำนวณค่าเฉลี่ย
# โค้ดสร้าง Data Frame อย่างง่าย
# โค้ดสร้างกราฟ Plot (อาจจะใช้โค้ด ggplot2 ง่ายๆ จาก Lesson ก่อนๆ ก็ได้ค่ะ/ครับ)

# ลองดูตัวอย่างโครงสร้างไฟล์ .Rmd ที่มีการผสมผสานข้อความกับ Code Chunk แบบนี้ค่ะ/ครับ ลองคัดลอกเนื้อหานี้ไปใส่ในไฟล์ .Rmd ของคุณ แทนที่เนื้อหาเดิมทั้งหมดเลยก็ได้ค่ะ/ครับ


# Lesson 29 การจัดการและการวิเคราะห์ข้อมูล Variant (ไฟล์ VCF) ใน R --------

# ขั้นตอนที่ 1: อ่านไฟล์ VCF
library(vcfR)
vcf_file_path <- "C:/Old Volume/Work/งานนอก/R Coding/ALL.chr20.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.SUBSET.vcf"

  # อ่านไฟล์ VCF เข้ามาใน R
vcf_object <- read.vcfR(vcf_file_path)

  # ตรวจสอบว่าอ่านไฟล์สำเร็จหรือไม่ และดูข้อมูลเบื้องต้นของ object
print("อ่านไฟล์ VCF สำเร็จแล้ว:")
vcf_object

  # ดึงข้อมูลส่วน Fixed Information ออกมาดู
library(vcfR)
fix_data <- getFIX(vcf_object)
fix_data   # แสดงข้อมูล

  # ดึงข้อมูลส่วน Genotype ออกมาดู
gt_data <- extract.gt(vcf_object)
head(gt_data)

# ขั้นตอนที่ 2: แปลงข้อมูล Genotype จากรูปแบบข้อความ (gt_data) ให้เป็นตัวเลข
library(vcfR)
numeric_gt_data <- gt.to.numeric(gt_data, na_is_zero = FALSE, return.alleles = FALSE)

  # สร้างตาราง Lookup (Lookup Table) สำหรับแปลง Genotype
genotype_map <- c(
  "0/0" = 0, "0|0" = 0,   # homozygous reference -> 0
  "0/1" = 1, "0|1" = 1,   # heterozygous -> 1
  "1/1" = 2, "1|1" = 2,   # homozygous variant -> 2
  "./." = NA, ".|." = NA   # missing data -> NA
)

  # ใช้ตาราง Lookup แปลงข้อมูลใน gt_data ทั้งหมด
numeric_gt_data_efficient <- genotype_map[gt_data]

  # จัดรูปแบบให้เป็น Matrix ที่ถูกต้อง (อาจจะต้องกำหนดขนาดและชื่อแถว/คอลัมน์ใหม่)
numeric_gt_data_efficient <- matrix(
  numeric_gt_data_efficient,
  nrow = nrow(gt_data),
  ncol = ncol(gt_data),
  dimnames = dimnames(gt_data)
)

  # ทำให้ data ใน object เป็น Numeric Matrix (ขั้นตอนสำคัญ)
numeric_gt_data_efficient <- apply(numeric_gt_data_efficient, 2, as.numeric)
numeric_gt_data_efficient   # ตรวจสอบข้อมูล

# ขั้นตอนที่ 3: คำนวณ Distance Matrix
  # สลับ Matrix Variance จาก Variants x Samples ให้เป็น Samples x Variants
transpose_gt_data <- t(numeric_gt_data_efficient)

sample_distance_matrix <- dist(transpose_gt_data, method = "euclidean") # คำนวณ Distance Matrix ระหว่าง Samples
sample_distance_matrix   # ตรวจสอบข้อมูล

# ขั้นตอนที่ 4: ทำ Hierarchical Clustering จาก Distance Matrix
sample_clustering <- hclust(sample_distance_matrix, method = "complete")

  # ดูข้อมูลเบื้องต้นของ Clustering Object
print("ผลลัพธ์จากการทำ Hierarchical Clustering:")
sample_clustering

# ขั้นตอนที่ 5: สร้าง Heatmap
data_for_heatmap <- transpose_gt_data

library(pheatmap)
pheatmap(
  data_for_heatmap,   # input จะต้องเป็น matrix
  clustering_distance_rows = "euclidean",   # คำนวณระยะห่าง และ clustering แถว (Samples)
  cluster_cols = FALSE,   # ไม่ต้องทำ clustering column (Variants)
  clustering_method = "complete",   # กำหนดวิธี clustering แถว (Samples)
  color = cm.colors(100),   # กำหนดโทนสี
  main = "Genetic Distance Heatmap (Sub Samples)",
  display_numbers = FALSE   # ไม่แสดงค่าตัวเลขในแต่ละช่อง
)
print("สร้าง Heatmap เสร็จแล้ว!")

# png("my_heatmap.png", width = 1000, height = 800, res = 150)  # หากต้องการบันทึกเป็นไฟล์รูปภาพ
# dev.off()

# ขั้นตอนที่ 6: ทำ Principle Component Analysis (PCA) ด้วย ฟังก์ชัน dudi.pca()
  # สร้าง Object ใหม่สำหรับเก็บข้อมูลที่เติมค่า NA แล้ว โดยคัดลอกมาจาก transposed_gt_data
imputed_data_for_pca <- transpose_gt_data

  # วนไปทีละคอลัมน์ (Variants) ใน imputed_data_for_pca
for (i in 1:ncol(imputed_data_for_pca)) {
  
    # คำนวณค่าเฉลี่ยของคอลัมน์ที่ i โดยไม่รวมค่า NA
  mean_val <- mean(imputed_data_for_pca[, i], na.rm = TRUE)
  
  # แทนที่ค่า NA ในคอลัมน์ที่ i ด้วยค่าเฉลี่ยที่คำนวณได้
  imputed_data_for_pca[is.na(imputed_data_for_pca[, i]), i] <- mean_val
}

anyNA(imputed_data_for_pca) # ผลลัพธ์ควรจะเป็น FALSE

  # ลบคอลัมน์ (Variants) ที่ยังมีค่า NA หรือ NaN หลงเหลืออยู่
print(paste("จำนวน Variants ก่อนลบคอลัมน์ NA/NaN:", ncol(imputed_data_for_pca)))  # 1812841
imputed_data_for_pca_clean <- imputed_data_for_pca[, colSums(is.na(imputed_data_for_pca) | is.nan(imputed_data_for_pca)) <= 0]
print(paste("จำนวน Variants ที่เหลือหลังลบคอลัมน์ NA/NaN:", ncol(imputed_data_for_pca_clean)))  # 1811717

  # ตรวจสอบอีกครั้งว่า Object ใหม่ ไม่มีค่า NA หรือ NaN แล้ว
anyNA(imputed_data_for_pca_clean) # ผลลัพธ์ควรเป็น FALSE
any(is.nan(imputed_data_for_pca_clean)) # ผลลัพธ์ควรเป็น FALSE

library(ade4)
print("กำลังทำ PCA...")
pca_result <- dudi.pca(imputed_data_for_pca_clean, scale = FALSE, nf = 2, scannf = FALSE)
print("ทำ PCA เสร็จแล้ว!")

print(pca_result)   # ดูข้อมูลเบื้องต้นของผลลัพธ์ PCA

# ขั้นตอนที่ 7: สร้างกราฟ PCA Scatter Plot
  # สร้าง Data Frame สำหรับเก็บ Scores ของ Samples บนแกนหลักที่ 1 และ 2
pca_scores <- as.data.frame(pca_result$li)

  # เพิ่มชื่อ Samples เป็น column ใน data frame เพื่อ label จุดบนกราฟ
pca_scores$Sample <- rownames(pca_scores)

library(ggplot2)
pca_plot <- ggplot(pca_scores, aes(x = Axis1, y = Axis2)) +
  geom_point(size = 3) +
  geom_text(aes(label = Sample), vjust = 2) +
  labs(
    title = "PCA Plot of Genetic Variants",
    x = paste("PC1 (", format(pca_result$eig[1] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = ""),
    y = paste("PC2 (", format(pca_result$eig[2] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = "")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(pca_plot)

# ขั้นตอนที่ 8: เข้าถึงข้อมูลของ metadata ของ 1000 Genomes Project
library(kgp)
metadata_kgp <- kgp3

print("ข้อมูล Metadata จาก Library kgp:")
head(metadata_kgp)

# ขั้นตอนที่ 9: merge ข้อมูลของ pca_scores และ metadata ทีได้
print("กำลังรวมข้อมูล Scores PCA และ Metadata...")
pca_scores_with_metadata <- merge(pca_scores, metadata_kgp, by.x = "Sample", by.y = "fid")
print("รวมข้อมูลเสร็จแล้ว!")

print("Data Frame Scores PCA พร้อม Metadata:")
pca_scores_with_metadata

# ขั้นตอนที่ 10: สร้างกราฟ PCA Scatter Plot ด้วย ggplot2 และใส่สีตามคอลัมน์ 'population'
library(ggplot2)
pca_plot_colored_by_population <- ggplot(pca_scores_with_metadata, aes(x = Axis1, y = Axis2, color = population)) +
  geom_point(size = 3) +
  geom_text(aes(label = Sample), vjust = 2) + # label แต่ละจุดด้วย Sample column
  labs(
    title = "PCA Plot of Genetic Variants (Colored by Population)",
    x = paste("PC1 (", format(pca_result$eig[1] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = ""),
    y = paste("PC2 (", format(pca_result$eig[2] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = ""),
    color = "Population"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(pca_plot_colored_by_population)

  # ลองกำหนดสีตาม sex column
class(pca_scores_with_metadata$sex) # พบว่าเป็น integer
pca_scores_with_metadata$sex <- as.factor(pca_scores_with_metadata$sex) # แปลงให้เป็นเชิง quality (factor)
class(pca_scores_with_metadata$sex)

pca_plot_colored_by_sex <- ggplot(pca_scores_with_metadata, aes(x = Axis1, y = Axis2, color = sex)) +
  geom_point(size = 3) +
  geom_text(aes(label = Sample), vjust = 2) +
  labs(
    title = "PCA Plot of Genetic Variants (Colored by Sex)",
    x = paste("PC1 (", format(pca_result$eig[1] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = ""),
    y = paste("PC2 (", format(pca_result$eig[2] / sum(pca_result$eig) * 100, digits = 2), "%)", sep = ""),
    color = "Sex"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(pca_plot_colored_by_sex)  # แสดงผลลัพธ์


# Lesson 30 GRanges เบื้องต้น – จัดการข้อมูลจีโนมใน Bioconductor ----------

# ขั้นตอนที่ 1: สร้าง Object GRanges พื้นฐาน
library(GenomicRanges)
gr_example <- GRanges(
  seqnames = c("chr1", "chr2", "chr3"),
  ranges = IRanges(start = c(100, 200, 300), end = c(150, 250, 350)),
  strand = c("+", "-", "+"),
  score = c(5.5, 3.2, 4.1)  # ค่า score เป็นตัวอย่าง
)

gr_example   # แสดงผลลัพธ์

# ขั้นตอนที่ 2: การเข้าถึงและการสำรวจข้อมูลใน GRanges
  # เข้าถึงชื่อของ Chromosome
seqnames(gr_example)

  # เข้าถึงช่วง (ranges) ทั้งหมด
ranges(gr_example)

  # ดึงค่า Start และ End position ของแต่ละช่วง
start(gr_example)
end(gr_example)

  # เข้าถึงข้อมูลเมตาดาต้า (metadata) เช่น column "score"
mcols(gr_example)$score

# ขั้นตอนที่ 3: การ subset และ filter GRanges
  # กรองเฉพาะ GRanges ที่ score > 4
gr_filtered <- gr_example[mcols(gr_example)$score > 4]
gr_filtered

# ขั้นตอนที่ 4: การหา Overlaps ระหว่าง GRanges Objects
  # สร้าง GRanges object อีกชุดหนึ่งเพื่อเปรียบเทียบ
gr1 <- GRanges(
  seqnames = c("chr1", "chr2"),
  ranges = IRanges(start = c(120, 210), end = c(180, 260)),
  strand = c("+", "-")
)

gr2 <- GRanges(
  seqnames = c("chr1", "chr1", "chr2"),
  ranges = IRanges(start = c(130, 190, 220), end = c(170, 240, 270)),
  strand = c("+", "-", "-")
)

  # ค้นหาข้อมูลที่ทับซ้อนกันระหว่าง gr1 และ gr2
hits <- findOverlaps(gr1, gr2)
hits

  # ดูดัชนีของช่วงที่มีการทับซ้อนในแต่ละ object
queryHits(hits) # index ใน gr1
subjectHits(hits) # index ใน gr2

# ขั้นตอนที่ 5: การคำนวณ Coverage
cov <- coverage(gr_example)
cov

  # หากต้องการดูคะแนน coverage ของ chr1
if ("chr1" %in% names(cov)) {
  print(as.vector(cov[["chr1"]]))
}

# ขั้นตอนที่ 6: การ Set Operations บน GRanges
  # สร้างตัวอย่าง GRanges สองชุด
gr_a <- GRanges(seqnames = "chr1", ranges = IRanges(c(100, 300), width = 100))  # start @ 100 & 300
gr_b <- GRanges(seqnames = "chr1", ranges = IRanges(c(150, 350), width = 100))  # start @ 150 & 350

  # Union ของ gr_a และ gr_b (รวมช่วงที่ทับซ้อน)
gr_union <- union(gr_a, gr_b)
gr_union

  # Intersection ระหว่าง gr_a และ gr_b (ช่วงที่ทับซ้อนกันเท่านั้น)
gr_intersection <- intersect(gr_a, gr_b)
gr_intersection

  # setdiff: หาช่วงที่อยู่ใน gr_a แต่ไม่ทับซ้อนกับ gr_b
gr_difference <- setdiff(gr_a, gr_b)
gr_difference

# ขั้นตอนที่ 7: การปรับเปลี่ยนและจัดตำแหน่งด้วยฟังก์ชันช่วยเหลืออื่น ๆ
  # shift: เลื่อนตำแหน่งของ GRanges ไปทางขวา (เพิ่มค่า start และ end)
gr_shift <- shift(gr_example, shift = 50)
gr_shift

  # flank: สร้างช่วงใหม่ที่อยู่ห่างจากตำแหน่งเริ่มต้น/สิ้นสุด
gr_flank <- flank(gr_example, width = 30, start = TRUE) # upstream OR downstream (coordinate เท่าเดิม)
gr_flank

  # resize: ปรับขนาดช่วงให้มีความยาว 80 bp
gr_resized <- resize(gr_example, width = 80)
gr_resized

# ขั้นตอนที่ 8: การผสานข้อมูล Annotation
  # สมมติว่าเรามี data frame สำหรับ annotation
annotations <- data.frame(
  seqnames = (c("chr1", "chr2")),
  start = c(110, 320),
  end = c(160, 370),
  gene = c("GeneA", "GeneB")
)

  # แปลงเป็น GRanges
gr_annotations <- GRanges(
  seqnames = annotations$seqnames,
  ranges = IRanges(start = annotations$start, end = annotations$end),
  gene = annotations$gene
)
gr_annotations

# ขั้นตอนที่ 9: การจัดการข้อมูลขนาดใหญ่ (Scalability)
  # กรองเฉพาะข้อมูล chromosome ที่ต้องการ
gr_filtered_chrom <- keepSeqlevels(gr_example, value = "chr1", pruning.mode = "coarse")
gr_filtered_chrom

  # การจัดเรียง GRanges
gr_sort <- sort(gr_example)
gr_sort


# Lesson 31 GRanges + SumExp ----------------------------------------------

# ขั้นตอนที่ 1: สร้างข้อมูลตัวอย่างสำหรับ SummarizedExperiment
  # สร้าง matrix ของระดับการแสดงออก (genes x samples) & (assay matrix)
library(SummarizedExperiment)
set.seed(42)
assay_data <- matrix(rnorm(5*3, mean = 10, sd = 2), nrow = 5, ncol = 3)
rownames(assay_data) <- paste0("Gene", 1:5)
colnames(assay_data) <- paste0("Sample", 1:3)
assay_data   # แสดงผลลัพธ์

  # สร้าง GRanges สำหรับ rowRanges: สมมติข้อมูลของ 5 gene บนจีโนม
library(GenomicRanges)
granges_data <- GRanges(
  seqnames = Rle(c("chr1", "chr2", "chr3", "chr1", "chr2")),
  ranges = IRanges(start = c(100, 500, 900, 1300, 1700),
                   width = 100),
  strand = Rle(strand(c("+", "-", "+", "+", "-")))
)
granges_data

  # สร้าง colData สำหรับ metadata
col_data <- DataFrame(
  condition = factor(c("Control", "Treatment", "Control")),
  age = c(35, 42, 29)
)
col_data

# ขั้นตอนที่ 2: สร้าง SummarizedExperiment Object
  # รวมข้อมูล assay, rowRanges และ colData เข้าด้วยกัน
se <- SummarizedExperiment(
  assay = list(counts = assay_data),
  rowRanges = granges_data,
  colData = col_data
)
se

# ขั้นตอนที่ 3: การเข้าถึงและใช้งานข้อมูลใน SummarizedExperiment
  # ดู assay data
assay(se, "counts")

  # ดู rowRanges (GRanges ที่เราสร้างไว้)
rowRanges(se)

  # ดู colData (metadata ของตัวอย่าง)
colData(se)

  # subset rowRanges ที่ seqnames เป็น "chr1"
idx <- which(seqnames(rowRanges(se)) == "chr1")
se_chr1 <- se[idx, ]
se_chr1


# Lesson 32 วิเคราะห์ RNA‑seq ด้วย DESeq2 ---------------------------------

# ขั้นตอนที่ 1: สร้างชุดข้อมูลตัวอย่าง
  # สร้าง count matrix สำหรับ 6 ยีน และ 4 ตัวอย่าง
set.seed(42)
countData <- matrix(
  rpois(6*4, lambda = 10),   # rpois(n, lambda): สร้างค่าจำนวนเต็มสุ่มโดยมี lambda คือตัวกลาง
  nrow = 6,
  ncol = 4
)
rownames(countData) <- paste0("Gene", 1:6)
colnames(countData) <- paste0("Sample", 1:4)
countData

  # สร้าง colData (Metadata)
colData <- data.frame(
  condition = factor(c(rep("control", 2), rep("treatment", 2)))
)
rownames(colData) <- colnames(countData)  # ใส่ rownames ให้ตรงกับชื่อ Sample
colData

# ขั้นตอนที่ 2: สร้าง DESepDataSet Object
library(DESeq2)
dds <- DESeqDataSetFromMatrix(
  countData = countData,
  colData = colData,
  design = ~ condition   # เลือกตัวแปรที่ต้องการตรวจสอบผลกระทบ
)
dds

# ขั้นตอนที่ 3: การวิเคราะห์ Differential Expression
  # การรัน DESep
dds <- DESeq(dds)

  # ดึงผลลัพธ์
res <- results(dds)
summary(res)  # ไม่มีความแตกต่างอย่างมีนัยสำคัญ

# ขั้นตอนที่ 4: ตรวจสอบและ Virsualization ผลลัพธ์
  # ตรวจสอบค่า Normalized Counts
norm_counts <- counts(dds, normalized = TRUE)
norm_counts

  # Volcano Plot
resDF <- as.data.frame(res)
resDF$Gene <- rownames(resDF)

library(ggplot2)
library(ggrepel)

  # ตั้งเงื่อนไขเพื่อกำหนดความสำคัญ: 
  # ยีนที่มี padj < 0.05 และ |log2FoldChange| > 1 ถือว่า Significant
resDF$sig <- "Not Significant"
resDF$sig[!is.na(resDF$padj) & resDF$padj < 0.05 & abs(resDF$log2FoldChange) > 1] <- "Significant"
resDF$sig <- factor(resDF$sig, levels = c("Not Significant", "Significant"))

# กำหนด Volcano Plot ที่มีการเพิ่มเส้น threshold และปรับสีให้ดูเป็น professional
p <- ggplot(resDF, aes(x = log2FoldChange, y = -log10(padj), color = sig)) +
  geom_point(alpha = 0.8, size = 3) +
  
  # กำหนดสีโดยให้ยีน significant เป็นสีแดง และยีนที่ไม่ significant เป็นสีเทา
  scale_color_manual(values = c("Not Significant" = "grey50", "Significant" = "red2")) +
  
  # เส้นแนวตั้งแสดง threshold ที่ log2 fold change = -1 และ 1
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black", size = 0.5) +
  # เส้นแนวนอนแสดง threshold ที่ adjusted p-value = 0.05 (–log10(0.05))
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", size = 0.5) +
  
  labs(
    title = "Volcano Plot of Differential Expression", 
    x = "Log2 Fold Change", 
    y = "-Log10 Adjusted P-value") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    axis.title = element_text(face = "bold", size = 14),
    axis.line = element_line(size = 0.75),
    
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

  # ตัวเลือกเพิ่มเติม: การเพิ่ม label สำหรับยีนที่มีความสำคัญ
topGenes <- resDF[order(resDF$padj), ][1:6, ]   # ตัวอย่างนี้จะเลือก label ให้ 10 ยีนที่มีค่า padj ต่ำที่สุด
p <- p + geom_text_repel(data = topGenes, aes(label = Gene),
                         size = 4,
                         box.padding = 0.3,
                         point.padding = 0.5,
                         max.overlaps = Inf)

print(p)  # แสดง Volcano Plot


# Lesson 33 PCA และ Data Transformation สำหรับ RNA‑seq --------------------

# ขั้นตอนที่ 1: การแปลงข้อมูลด้วย VST และการทำ PCA
library(DESeq2)
vstData <- vst(dds, blind = FALSE, nsub = nrow(dds))  # ให้ condition แปลงด้วย

library(ggplot2)
pcaPlot <- plotPCA(vstData, intgroup = "condition") +
  ggtitle("PCA of RNA-seq Data") +
  theme_bw()
print(pcaPlot)

  # หรือสามารถใช้ ggplot2 เพื่อปรับแต่ง PCA Plot
  # ดึงข้อมูล PCA ออกมาในรูปแบบ data.frame
pcaData <- plotPCA(vstData, intgroup = "condition", returnData = TRUE)
rownames(pcaData) <- NULL
percentVar <- round(100 * attr(pcaData, "percentVar"), 2)

  # สร้าง PCA plot ด้วย ggplot2
library(ggplot2)
p <- ggplot(pcaData, aes(x = PC1, y = PC2, color = condition)) +
  geom_point(size = 3, alpha = 0.8) +
  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  labs(color = "Condition") +
  
  ggtitle("PCA of RNA-seq Data") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    axis.title = element_text(size = 14),
    axis.line = element_line(size = 0.75),
    
    panel.grid = element_blank()
    
  )
print(p)


# Lesson 34 การวิเคราะห์ Gene Ontology (GO) และ Pathway Analysis ----------

# ขั้นตอนที่ 1: เตรียม Gene ID ให้พร้อมสำหรับการวิเคราะห์
  # ตัวอย่าง Gene Symbols ของมนุษย์
my_gene_symbols <- c("TP53", "BRCA1", "EGFR", "MYC", "JUN", "NON_EXISTENT_GENE")  # ใส่ยีนที่ไม่มีจริง 1 ตัวเพื่อดูผลลัพธ์

print("รายการ Gene Symbol ที่ต้องการแปลง: ")
my_gene_symbols

  # ใช้ bitr() เพื่อแปลง Gene ID; Biological Id TranslatoR
library(clusterProfiler)
library(org.Hs.eg.db)
gene_id_mapping <- bitr(
  geneID = my_gene_symbols,
  fromType = "SYMBOL",   # ระบุประเภท ID ของยีนที่เรามีอยู่ (ในที่นี้คือ Gene Symbol)
  toType = "ENTREZID",   # ระบุประเภท ID ที่เราต้องการแปลงไป (ในที่นี้คือ Entrez Gene ID)
  OrgDb = org.Hs.eg.db   # ระบุฐานข้อมูล Annotation ของสิ่งมีชีวิตที่ใช้ (org.Hs.eg.db สำหรับมนุษย์)
)

print("ผลลัพธ์การแปลง Gene ID:")
gene_id_mapping

# ขั้นตอนที่ 2: การวิเคราะห์ GO Enrichment Test
genes_for_go <- gene_id_mapping$ENTREZID

  # ตรวจสอบ Gene IDs ที่จะนำไปวิเคราะห์
print("Entrez Gene IDs ที่จะใช้สำหรับการวิเคราะห์ GO Enrichment:")
genes_for_go

  # ทำ GO Enrichment Analysis ด้วยฟังก์ชัน enrichGO()
library(clusterProfiler)
go_enrich_result <- enrichGO(
  gene = genes_for_go,   # รายการ Gene ID ของยีนที่เราสนใจ (Entrez ID)
  OrgDb = org.Hs.eg.db,   # ฐานข้อมูล Annotation ของสิ่งมีชีวิตที่เราใช้ (มนุษย์)
  ont = "ALL",   # ระบุ Ontology ที่ต้องการวิเคราะห์ (Biological Process)
  pAdjustMethod = "BH",   # วิธีการปรับค่า P-value สำหรับ Multiple Testing (Benjamini-Hochberg)
  pvalueCutoff = 0.05,   # เกณฑ์ P-value ที่ **ปรับค่าแล้ว** ที่เราถือว่ามีนัยสำคัญ
  qvalueCutoff = 0.05,   # เกณฑ์ q-value (มักจะคล้ายกับ p.adjust) ที่ถือว่ามีนัยสำคัญ
  readable = TRUE   # ตั้งเป็น TRUE เพื่อให้ผลลัพธ์แสดง Gene Symbol ร่วมด้วย (ถ้ามีในฐานข้อมูล)
)

print("ผลลัพธ์การวิเคราะห์ GO Enrichment (Biological Process):")
View(as.data.frame(go_enrich_result))

# ขั้นตอนที่ 3: การแสดงผล (Visualization) ผลลัพธ์ GO Enrichment (Dot Plot)
  # ตรวจสอบว่าผลลัพธ์ GO Enrichment มี GO Terms ที่ Enriched หรือไม่
if(nrow(go_enrich_result@result) > 0) {
  
  print("กำลังสร้าง GO Enrichment Dot Plot...")
  
  dotplot_go <- dotplot(go_enrich_result, showCategory = 10) +   # แสดงเฉพาะ 10 GO Terms แรกที่มีนัยสำคัญที่สุด (ตาม p.adjust โดย Default)
    ggplot2::ggtitle("Enriched GO Terms (Biological Process)") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 12),
      axis.line = ggplot2::element_line(size = 0.75),
      legend.text = ggplot2::element_text(size = 10)
      )
  
  print(dotplot_go)
} else{
  print("ไม่มี GO Terms ใด Enriched อย่างมีนัยสำคัญตามเกณฑ์ที่ตั้งไว้ (pvalueCutoff, qvalueCutoff)")
  print("ลองปรับเกณฑ์ pvalueCutoff หรือ qvalueCutoff ให้สูงขึ้น ถ้าจำเป็น (แต่ควรระวัง False Positives)")
}

# ggplot2::ggsave("dotplot_go.png", plot = dotplot_go, width = 8, height = 6, dpi = 300)

# ขั้นตอนที่ 4: การแสดงผล (Visualization) ผลลัพธ์ GO Enrichment (Cnetplot)
  # ตรวจสอบอีกครั้งว่ามี GO Terms ที่ Enriched ก่อนสร้างกราฟ
if(nrow(go_enrich_result@result > 0)) {
  
  print("กำลังสร้าง GO Enrichment Cnetplot...")
  
  # สร้าง foldChange ปลอมให้ยีน
  fake_fc <- c(-1.5, 2.0, 0.5, -0.3, 1.2)
  names(fake_fc) <- gene_id_mapping$ENTREZID[1:5]
  
  cnetplot_go <- cnetplot(
    go_enrich_result,
    showCategory = 5,   # เลือกแสดงเฉพาะ 5 GO Terms แรกที่มีนัยสำคัญสูงสุดใน Network
    foldChange = fake_fc,
    layout = "graphopt",   # เลือก layout ของ network ได้ (เช่น "kk", "dh", "graphopt", "gem")
    circular = TRUE,   # circular = TRUE # ทำ network เป็นวงกลม (บาง layout ใช้ได้)
    node_label = "all"
  ) +
    ggplot2::ggtitle("GO Enrichment Cnetplot (Biological Process)") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5))
  
  print(cnetplot_go)
} else{
  print("ไม่มี GO Terms ใด Enriched อย่างมีนัยสำคัญ จึงไม่สามารถสร้าง Cnetplot ได้")
}

# ggplot2::ggsave("cnetplot_go.png", plot = cnetplot_go, width = 8, height = 6, dpi = 300)

# ขั้นตอนที่ 5: การวิเคราะห์ Pathway Enrichment Test (KEGG)
  # ตรวจสอบ Gene IDs ที่จะนำไปวิเคราะห์ Pathway
genes_for_go   # ใช้ชุดเดียวที่ใช้สำหรับ GO

  # ทำ KEGG Pathway Enrichment Analysis ด้วยฟังก์ชัน enrichKEGG()
kegg_enrich_result <- enrichKEGG(
  gene = genes_for_go,   # รายการ Gene ID ของยีนที่สนใจ (Entrez ID)
  organism = "hsa",   # รหัสสิ่งมีชีวิตใน KEGG ("hsa" สำหรับ Homo sapiens)
  pAdjustMethod = "BH",   # วิธีปรับค่า P-value (Benjamini-Hochberg)
  pvalueCutoff = 0.05,    # เกณฑ์ P-value ที่ปรับค่าแล้ว
  qvalueCutoff = 0.05,   # เกณฑ์ q-value
)

  # แปลงผลลัพธ์ให้แสดง Gene Symbol
kegg_enrich_result <- setReadable(
  kegg_enrich_result,
  OrgDb = org.Hs.eg.db,
  keyType = "ENTREZID"
)

print("ผลลัพธ์การวิเคราะห์ KEGG Pathway Enrichment:")
View(as.data.frame(kegg_enrich_result))

# ขั้นตอนที่ 6: การแสดงผล (Visualization) ผลลัพธ์ Pathway Enrichment (KEGG)
  # สามารถใช้ฟังก์ชัน dotplot() และ cnetplot() ที่มาจาก package enrichplot
library(dplyr)
library(ggplot2)
top10 <- as.data.frame(kegg_enrich_result) %>%
  arrange(p.adjust) %>%
  slice_head(n = 10)

dotplot_kegg <-
  ggplot(top10, aes(
    y = reorder(Description, p.adjust),
    x = -log10(p.adjust),
    size = Count,
    color = p.adjust
    )) +
  
  geom_point() +
  scale_color_gradient(low = "#E31A1C", high = "#1F78B4", name = "Adjusted P-value") +
  
  labs(
    title = "Top 10 Enriched KEGG Pathways",
    y = "KEGG Pathway",
    x = "-log10 Adjusted P-value",
    size = "Gene Count"
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(dotplot_kegg)

# ggsave("dotplot_kegg.png", dotplot_kegg, width = 8, height = 6, dpi = 300)

print("กำลังสร้าง KEGG Pathway Enrichment Cnetplot...")

# ตรวจสอบอีกครั้งว่ามี Pathways ที่ Enriched
if (nrow(kegg_enrich_result@result) > 0) {
  
  cnetplot_kegg <- cnetplot(
    kegg_enrich_result,
    showCategory = 5, # เลือกแสดงเฉพาะ 5 Pathways แรกที่มีนัยสำคัญสูงสุดใน Network
    foldChange = fake_fc,
    layout = "kk"
  ) +
    ggplot2::ggtitle("KEGG Pathway Enrichment Cnetplot") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))
  
  print(cnetplot_kegg) # แสดงกราฟ Cnetplot สำหรับ KEGG
} else {
  print("ไม่มี KEGG Pathways ใด Enriched อย่างมีนัยสำคัญ จึงไม่สามารถสร้าง Cnetplot ได้")
}

# ggsave("cnetplot_kegg.png", cnetplot_kegg, width = 8, height = 6, dpi = 300)

# ขั้นตอนที่ 7: การวิเคราะห์ Gene Set Enrichment Analysis (GSEA) บน GO Ontology
  # ใช้ Entrez ID ที่แปลงได้จากขั้นตอนที่ 1
mapped_entrez_ids <- gene_id_mapping$ENTREZID
## TODO: เพิ่มการเรียกใช้ฟังก์ชัน GSEA (เช่น gseGO หรือ gseKEGG) และการแสดงผลลัพธ์

  # สร้างชุด Entrez ID ที่เป็น Background (สมมติว่านี่คือยีนทั้งหมดที่เราวิเคราะห์ได้)
all_possible_entrez <- keys(org.Hs.eg.db, keytype = "ENTREZID") # ดึง Entrez ID ทั้งหมดจากฐานข้อมูลมนุษย์


  # สุ่มเลือก Entrez ID ส่วนหนึ่งมาเป็น Background สมมติ 500 ยีน
set.seed(42)
background_entrez_id <- sample(all_possible_entrez, 500)

  # รวม Entrez ID จากยีนตัวอย่างของเราเข้าไปใน Background เพื่อให้แน่ใจว่ามีอยู่ใน list
background_entrez_list <- unique(c(background_entrez_id, mapped_entrez_ids))

  # สร้างค่าตัวอย่าง log2 Fold Change (log2FC) สำหรับยีนทั้งหมดใน Background
log2fc_values <- rnorm(length(background_entrez_id), mean = 0, sd = 1)  # สร้างค่า log2FC แบบสุ่ม



# Lesson 35 การวิเคราะห์ Multi-Omics Data Integration ใน R ----------------
## TODO: เพิ่มเนื้อหาสำหรับบทเรียนนี้



# Lesson xxx การวิเคราะห์ข้อมูลทางพันธุกรรมแบบ Interactive ---------------------------

# ขั้นตอนที่ 1: สร้าง UI
library(shiny)
library(Biostrings)
library(ggtree)
library(shiny)
library(ape)
ui <- fluidPage(
  titlePanel("DNA Sequence Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sequence_input", "Enter DNA sequence:"),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sequence Info", tableOutput("sequence_info")),
        tabPanel("Base Composition", plotOutput("base_composition")),
        tabPanel("Phylogenetic Tree", plotOutput("phylo_tree"))
      )
    )
  )
)

# ขั้นตอนที่ 2: สร้าง server
server <- function(input, output) {
  # สร้าง reactive value สำหรับ DNA sequence
  sequence <- reactive({
    req(input$sequence_input)  # ตรวจสอบว่ามีข้อมูลหรือไม่
    DNAString(input$sequence_input)
  })
  
  # ขั้นตอนที่ 3: สร้าง output สำหรับ sequence info (Placeholder)
  output$sequence_info <- renderTable({
    req(sequence())
    data.frame(Property = "Length", Value = length(sequence()))
  })
  
# ขั้นตอนที่ 3: สร้าง output สำหรับ base composition
  output$base_composition <- renderPlot({
    req(sequence())  # ตรวจสอบว่ามี sequence หรือไม่
    seq <- sequence()
    
    # คำนวณความถี่ของแต่ละเบส
    freq <- letterFrequency(seq, letters = c("A", "T", "G", "C"))
    
    # สร้างกราฟแท่ง
    barplot(freq, 
            main = "Base Composition",
            col = c("green", "red", "blue", "yellow"),
            xlab = "Nucleotide",
            ylab = "Frequency")
  })
  
# ขั้นตอนที่ 4: สร้าง output สำหรับ phylogenetic tree
  output$phylo_tree <- renderPlot({
    req(sequence())  # ตรวจสอบว่ามี sequence หรือไม่
    seq <- sequence()
    
    # สร้าง tree แบบสุ่มตามความยาวของ sequence
    n <- length(seq)
    tree <- rcoal(n)  # สร้าง tree แบบสุ่ม
    
    # วาด tree
    plot(tree, 
         main = paste("Phylogenetic Tree (", n, " sequences)"),
         cex = 0.8)
  })
}

shinyApp(ui = ui, server = server)
