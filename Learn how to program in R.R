# ------------------------------------------------------
# File: Learn how to program in R.R
# Purpose: Brief description of the script
# Author: Pirada Naewkam
# Date: 2025-11-25
#
# Description:
#   Detailed description or steps
#
# Input: input files or data
# Output: output files or results
# ------------------------------------------------------


# Lesson 01: Get to know R Language ---------------------------------------

# กำหนดตัวแปรเพื่อให้ทำงาน (function)
celsius2fah <- function(celsius) {
  (celsius * 9/5) + 32 
}

fah2celsius <- function(fah) {
  (fah - 32) * 5/9
}

# ใช้ function เพื่อทำ loop ตามค่าที่ต้องการ
temperatureTable <- function() {
  for (celsius in 0:100) {
    print(sprintf("%dC = %.1fF", celsius, celsius2fah(celsius)))
  }
}


# Lesson 02: Basic Data Types ---------------------------------------------

w <- 5
w * 2
a <- w * 7

gpa <- 3.45
students <- 30L

# เช็คประเภท data types
class(w)
class(gpa)
class(students)

firstName <- "Peter"
firstname <- "Jenny"
class(firstName)
class(firstname)

# Date
birthdate <- as.Date("1998-07-28")  # 28 July 1998
class(birthdate)

# vector
menu <- c("green tea", "lemon tea", "mocha", "latte", "espresso")
menu[1]
menu[3]
menu[3:5]
price <- c(45, 50, 35, 70, 60)
d <- data.frame(menu, price)

orders <- c(5, 10, 3, 20, 100)
d2 <- data.frame(menu, price, orders)
d2$revenue <- d2$price * d2$orders
sum(d2$revenue)


# Lesson 03: Mathematical Calculations ------------------------------------

a <- 9
b <- 2

a + b
a - b
a * b
a / b
a^2
a**2

1000000
1e6
2e6
2*10^6
2e-3
2 * 10 ^ (-3)
a %/% b
a %% b
4+(6/2)+(5*2)

4+6/2+5*2^3
4+(6/2)+(5*(2^3))

4+6/2+5*2^3^4
4+(6/2)+(5*(2^(3^4)))


# Lesson 04: Creating basic functions -------------------------------------

rectangle <- function(w, h) {
  w * h
}
rectangle(5, 7)

rectangle2 <- function(w, h) {
  return(w * h)
}
rectangle2(4, 3)
a <- rectangle2(4, 5)

square <- function(side) {
  return(side * side) # side ^ 2
}
b <- square(5)

square2 <- function(side) {
  return(rectangle(side, side))
}
square2(4)

greeting <- function(lang) {
  if(lang == "th"){
    return("sawadee")
  } else {
    return("hello")
  }
}

greeting("th")
greeting("vn")


# Lesson 05: Using the if() to Check Conditions ---------------------------

speed_check <- function(speed) {
  if (speed > 100) {
    print("too fast")
    print("fine 1,000 baht")
  }
}

speed_check(120)
speed_check(70)

ticket_fare <- function(age) {
  if(age <= 5) {
    print("free")
  } else  {
    print("fare = 50")
  }
}
ticket_fare(35)

ticket_fare2 <- function(age) {
  if(age <= 5 || age >= 60) {
    print("free")
  } else  {
    print("fare = 50")
  }
}
ticket_fare2(35)
ticket_fare2(70)

ticket_fare3 <- function(age, isLocal) {
  if((age <= 5 || age >= 60) && isLocal == TRUE) {
    print("free")
  } else  {
    print("fare = 50")
  }
}
ticket_fare3(70, FALSE)
ticket_fare3(70, TRUE)

greeting <- function(country) {
  if(country == "th") {
    print("sawadee")
  } else if (country == "en") {
    print("hello")
  } else {
    print(":-)")
  }
}
greeting("th")
greeting("en")
greeting("aa")


# Lesson 06: Repetition with a for loop -----------------------------------

demo_loop <- function() {
  for (i in 1:10) {
    print(i)
  }
}
demo_loop()

demo_loop2 <- function(m, n) {
  for (i in m:n) {
    print(i)
  }
}
demo_loop2(11, 20)

count_down <- function() {
  for (i in seq(from = 10, to = 1, by = -1)) {
    print(i)
  }
}
count_down()

sum_odd <- function(m, n) {
  total <- 0
  for (i in seq(from = m, to = n, by = 2)) {
    print(i)
    total <- total + i
  }
  return(total)
}
sum_odd(1, 20)

demo2 <- function(v) {
  for (i in v) {
    print(i)
  }
}

a <- c(2, 3, 7, 2, 4, 8)
demo2(a)

phone_fortune <- function(phoneNumber) {
  total <- 0
  for (i in 1:nchar(phoneNumber)) {
    print(substr(phoneNumber, i, i))
    total <- total + as.integer(substr(phoneNumber, i, i))
  }
  return(total)
}

phone_fortune("0637043578")


# Lesson 07: Creating a Nested Loop ---------------------------------------

m_table <- function() {
  for (i in 2:5) {
    for (j in 1:12) {
      print(sprintf("%d x %d = %d", i, j, i * j))
    }
  }
}
m_table()

playingCard <- function() {
  rank <- c("A", 2:10, "J", "Q", "K")
  # suit <- c("Spades", "Hearts", "Diamonds", "Clubs")
  suit <- c("\u2660", "\u2665", "\U2666", "\U2663")
  # deck <- character(52)
  deck <- character(length(rank) * length(suit))
  i <- 1
  for (s in suit) {
    for (r in rank) {
      deck[i] <- paste(r, s, sep = "")
      i <- i + 1
    }
  }
  return(deck)
}

d <- playingCard()
sample(d)


# Lesson 08: Performing a cross join with the merge() function ------------

playingCard <- function() {
  rank <- c("A", 2:10, "J", "Q", "K")
  # suit <- c("Spades", "Hearts", "Diamonds", "Clubs")
  suit <- c("\u2660", "\u2665", "\U2666", "\U2663")
  # deck <- character(52)
  deck <- character(length(rank) * length(suit))
  i <- 1
  for (s in suit) {
    for (r in rank) {
      deck[i] <- paste(r, s, sep = "")
      i <- i + 1
    }
  }
  return(deck)
}

playingCard_crossjoin <- function() {
  rank <- c("A", 2:10, "J", "Q", "K")
  # suit <- c("Spades", "Hearts", "Diamonds", "Clubs")
  suit <- c("\u2660", "\u2665", "\U2666", "\U2663")
  d <- merge(rank, suit, all = TRUE)
  deck <- paste(d$x, d$y, sep = "")
}

a <- playingCard_crossjoin()

# การประยุกต์ใช้ เช่นการสร้างทีมเพื่อให้แข่งครบทุกคู่
t <- c("alpha", "beta", "gramma")
m <- merge(t, t, all = TRUE)
m
m[m$x != m$y, ]
m[m$x > m$y, ]


# Lesson 09: Repetition with a while loop ---------------------------------

# while loop
demo <- function(n) {
  i <- 1
  while (i <= n) {
    print(i)
    i <- i + 1
  }
}
demo(10)

# for loop
demo_for <- function(n) {
  for (i in 1:n) {
    print(i)
  }
}
demo_for(10)

# ตัวอย่างการใช้ while loop
# การฝากเงิน 100 บาท ดอกเบี้ย 50% ต่อปี
# ต้องฝากกี่ปีถึงจะมีเงิน 150 บาท
saving <- function(pv, rate, fv) {
  i <- 1
  amt <- 0
  while (amt <= fv) {
    amt <- pv * (1 + rate) ^ i   # 100 * (1.05) ^ 1
    print(sprintf("year %d, amt = %.2f", i, amt))
    i <- i + 1 
  }
  return(i - 1)
}
saving(100, 0.05, 150)


# Lesson 10: Repeating tasks with the repeat loop -------------------------

# Types of loop
# 1. for loop
demo_for <- function(n) {
  for (i in 1:n) {
    print(i)
  }
}
demo_for(10)

# 2. while loop
demo_while <- function(n) {
  i <- 1
  while (i <= n) {
    print(i)
    i <- i + 1
  }
}
demo_while(10)

# *** repeat loop ***
demo_repeat <- function(n) {
  i <- 1
  repeat {
    
    # statement
    print(i)
    i <- i + 1
    
    # condition checking
    if (i > n) {
      break   # exit from loop (repeat)
    }
  }
  print("done")
}
demo_repeat(10)

# มีประโยชน์เมื่อต้องการทำซ้ำก่อน แล้วค่อยตรวจสอบเงื่อนไข
# ตัวอย่าง
two_dice <- function(target_number) {
  i <- 1
  repeat {
    n1 <- sample(1:6, 1)
    n2 <- sample(1:6, 1)
    print(sprintf("%d: %d %d", i, n1, n2))
    i <- i + 1
    if ((n1 + n2) == target_number) {
      break
    }
  }
  print(paste(i - 1, "times"))
}
two_dice(2)

two_dice_while <- function(target_number) {
  i <- 1
  n1 <- sample(1:6, 1)
  n2 <- sample(1:6, 1)
  while ((n1 + n2) != target_number) {
    print(sprintf("%d: %d %d", i, n1, n2))
    i <- i + 1
    n1 <- sample(1:6, 1)
    n2 <- sample(1:6, 1)
  }
  print(sprintf("%d: %d %d", i, n1, n2))
  print(paste(i, "times"))
}
two_dice_while(2)


# Lesson 11: Using RStudio Debugger ---------------------------------------

greeting <- function(country) {
  if (country == "th") {
    print("sawadee")
  } else if (country == "en") {
    print("hello")
  } else if (country == "cn") {
    print("Ni hao")
  } else {
    print(":-)")
  }
}
greeting("cn")  # set break point here!!!

phone_fortune <- function(phoneNumber) {
  total <- 0
  for (i in 1:nchar(phoneNumber)) {
    s <- substr(phoneNumber, i, i)
    total = total + as.integer(s)
  }
  return(total)
}
a <- phone_fortune("0637043578")  # set break point here!!!


# Lesson 12: Understand "List" data type ----------------------------------

dim1 <- c(20, 10, 5)
dim1
dim1[3]

dim2 <- list(20, 10, 5)
dim2[3]
dim2[[3]]

# ความต่างของการเก็บเป็น list กับ vector เฉย ๆ
p <- list("Peter", as.Date("1990-04-13"))
p
v <- c("Peter", as.Date("1990-04-13"))  # จะพยายามแปลง datatype ให้เป็นประเภทเดียวกัน
v

dim3 <- list(h = 20, w = 10, d = 5)
dim3
dim3[3]
dim3$d

person <- list(firstname = "Peter",
               lastname = "James",
               scores = c(80, 75, 82),
               test = c(eng = 80, math = 90))
person$scores
a <- unlist(person)
a

b <- unlist(person)
b


# Lesson 13: Access to data in List ---------------------------------------

p <- list("Peter", "James", 25, "F", c(80, 90, 75))
p[2]  # return as list
p[[2]]  # return as vector
p[3]
# p[3] + 5  # error
p[[3]] + 5

person <- list(firstname = "Peter",
               lastname = "James",
               age = 25,
               gender = "M",
               score = c(eng = 80, maths = 90, sci = 75))
person$firstname
person$age + 5
person["lastname"]
# person["age"] + 7 # error
person$score
sum(person$score)
person[3]

person$firstname <- "Taylor"
person
person$phone <- "0637043578"
person
person$phone <- NULL
person

View(women)
m <- lm(weight ~ height, data = women)
summary(m)
m$coefficients
m$coefficients[2]
m$coefficients["height"]
m$model$height
m$model["height"]


# Lesson 14: Create a function returns a List -----------------------------

basic_stats <- function(x) {
  xbar <- mean(x)
  deviation <- x - xbar
  deviation_sq <- deviation ^ 2
  ss <- sum(deviation_sq)
  var.p <- ss / length(x)
  var.s <- ss / (length(x) - 1)
  calc <- data.frame(x, xbar, deviation, deviation_sq)
  list(
    raw = x,
    n = length(x),
    mean = xbar,
    deviation = deviation,
    deviation_sq = deviation_sq,
    var.p = var.p,
    var.s = var.s,
    sd.p = sqrt(var.p),
    sd.s = sqrt(var.s),
    calc = calc
  )
}

y <- c(50, 45, 60, 70, 35)
s <- basic_stats(y)
s$calc
s$calc$x
s$var.p

plot_basic_stats <- function(s) {
  x <- 1:s$n
  plot(x, s$raw, pch = 16, col = "skyblue", cex = 1.5)
  abline(h = s$mean, col = "limegreen")
  for (i in x) {
    lines(c(i, i), c(s$raw[i], s$mean), col = "orange", lty = 2, lwd = 2)
  }
}
plot_basic_stats(s)


# Lesson 15: Create a function returns multiple values at once ------------

x <- sample(1:100, size = 50, replace = TRUE)
x
summary(x)

three_means <- function(x) {
  mean <- mean(x)
  harmonic <- length(x) / sum(1 / x)
  geometric <- prod(x) ^ (1/length(x))  # prod() คูณทุกค่าใน vector
  
  # named vector
  # return(c(average = mean, harmean = harmonic, geomean = geometric))
  c(average = mean, harmean = harmonic, geomean = geometric)
}

x <- c(10, 5, 7, 9, 3, 1)
m <- three_means(x)
m["harmean"]

quadratic <- function(a, b, c) {
  t <- b ^ 2 - 4 * a * c
  c(x1 = (-b + sqrt(t)) / (2 * a), x2 = (-b - sqrt(t)) / (2 * a))
}
x <- quadratic(2, 7, 3)
x <- quadratic(10, 7, 3)
x
x[1]
x["x1"]


# Lesson 16: Understand passing values to functions -----------------------

rolldice <- function() {
  sample(1:6, 1)
}
rolldice()

rolldice2 <- function(n) {
  # sample(1:6, n)
  sample(x = 1:6, size =  n)
}
rolldice2(3)

rolldice3 <- function(n = 1) {
  # sample(1:6, n)
  sample(x = 1:6, size =  n)
}
rolldice3()

rolldice4 <- function(n = 1, replace = TRUE) {
  # sample(1:6, n)
  sample(x = 1:6, size =  n, replace = replace)
}
rolldice4(3)
rolldice4(3, replace = FALSE)
rolldice4(n = 3, replace = FALSE)


# Lesson 17: Creating a function with unknown number of arguments  --------

# ellipsis (...)
add <- function(...) {
  b <- unlist(list(...))  # แปลง list ให้เป็น vector
  sum(b)
  # for (e in b) {
  #   print(e)
  # }
}

add(2, 3, 4, 10)

add2 <- function(v) {
 sum(v)
}

add2(c(2, 3, 4, 10))

npv <- function(rate, ...) {
  s <- 0
  cashflow <- unlist(list(...))
  for (i in 1:length(cashflow)) {
    s <- s + cashflow[i] / (1 + rate) ^ i
  }
  return(s)
}

-1000 + npv(0.10, 100, 200, 150, 500) # รับ cashflow มาเป็น elipsis

npv2 <- function(rate, cashflow) {
  s <- 0
  for (i in 1:length(cashflow)) {
    s <- s + cashflow[i] / (1 + rate) ^ i
  }
  return(s)
}

-1000 + npv2(0.10, c(100, 200, 150, 500)) # รับ cashflow มาเป็น vector


# Lesson 18: Creating a binary operator function --------------------------

# binary operator function
"%*%" <- function(a, b) {
  paste(a, b, sep = "")
}  
"hello" %*% "world"

"%union%" <- function(a, b) {
  unique(c(a, b))
}
a <- c("mocha", "espresso", "apple", "coke")
b <- c("apple", "latte", "coke")
a %union% b

"%intersect%" <- function(a, b) {
  a[a %in% b]
}
a %intersect% b

"%except%" <- function(a, b) {
  a[!(a %in% b)]
}
a %except% b


# Lesson 19: Source an R script to use a script in another file -----------

source("C:/Old Volume/Work/งานนอก/R Coding/pkLib.R")

a <- sq_wa2thaiArea(789)
a


# Lesson 20: Application of %% (modulus or remainder of division) ---------

# mod or %%
# จะเขียนย่อแบบนี้
isOdd <- function(n) {
  ifelse(n %% 2 == 1, TRUE, FALSE)
}

# หรือเขียนเต็ม ก็ได้
isOdd <- function(n) {
  if (n %% 2 == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
isOdd(30)

isEven <- function(n) {
  ifelse(n %% 2 == 0, TRUE, FALSE)
}

isEven <- function(n) {
  if (n %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
isEven(21)

# โค้ดที่ดี คือการเอาโค้ดที่มีอยู่ก่อนหน้ามาใช้ (ไม่ซ้ำซ้อน)
isEven <- function(n) {
  !isOdd(n)
}
isEven(20)
isEven(21)

# หาปีอธิกสุรทิน
leapyear <- function(year) {
  # ifelse(year %% 4 == 0, TRUE, FALSE)
  if (year %% 4 == 0) {
    TRUE   # ไม่ต้องพิมพ์ว่า return() ก็ได้ (แต่ best practice พิมพ์ไว้ดีกว่านะ)
  } else {
    FALSE
  }
}
leapyear(2016)
leapyear(2015)

# หาปีนักษัตร
zodiac <- function(buddhistYear) {
  z <- c("Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig",
         "Rat", "Ox", "Tiger", "Rabbit", "Dragon")
  return(z[(buddhistYear %% 12) + 1])
}
zodiac(2559)  # Monkey (ตรงกับความจริง)

# สร้าง for loop เพื่อไล่ดูปีนักษัตรได้เลย
for (y in 2568:2580) {
  print(sprintf("%d: %s", y, zodiac(y)), quote = FALSE)
}


# Lesson 21: Basic algorithm for find GCD ---------------------------------

# greatest common divisor

# 48, 18
gcd_loop <- function(a, b) {
  d <- ifelse(a < b, a, b)  # ให้ d ส่งค่าที่น้อยกว่า
  while (d >= 1) {
    print(sprintf("d = %d, a mod d = %d, b mod d = %d", d, a %% d, b %% d))
    if (a %% d == 0 && b %% d == 0) {
      return(d)
    } else {
      d <- d - 1
    }
  }
}
gcd_loop(48, 18)

# binary method algorithm: https://en.wikipedia.org/wiki/Greatest_common_divisor
# Input: a, b positive integers
# Output: g and d such that g is odd and gcd(a, b) = g×2^d
# d := 0
# while a and b are both even do
#   a := a/2
#   b := b/2
#   d := d + 1
# while a ≠ b do
#   if a is even then a := a/2
#   else if b is even then b := b/2
#   else if a > b then a := (a – b)/2
#   else b := (b – a)/2
# g := a
# output g, d

gcd <- function(a, b) {
  d <- 0
  print(sprintf("a = %d, b = %d, d = %d", a, b, d))
  while (a %% 2 == 0 && b %% 2 == 0) {
   a <- a / 2
   b <- b / 2
   d <- d + 1
   print(sprintf("a = %d, b = %d, d = %d", a, b, d))
  }
  while (a != b) {
    if (a %% 2 == 0) {
      a <- a / 2
    } else if (b %% 2 == 0) {
      b <- b / 2
    } else if (a > b) {
      a <- (a - b) / 2
    } else {
      b <- (b - a) / 2
    }
   print(sprintf("a = %d, b = %d, d = %d", a, b, d))
  }
  return(a * 2 ^ d)
}
gcd(48, 18)


# Lesson 22: Convert square wa/meters to rai, ngan, square wa -------------

# เปลี่ยนตารางวา >>> ไร่ งาน ตารางวาไทย
sq_wa2thaiArea <- function(sq_wa) {
  rai <- sq_wa %/% 400 
  ngan <- (sq_wa - (rai * 400)) %/% 100
  wa <- sq_wa - (rai * 400) - (ngan * 100)
  # wa <- sq_wa %% 100
  
  # named vector
  return(c(rai = rai, ngan = ngan, wa = wa))
}
a <- sq_wa2thaiArea(720)
a[1]
a["rai"]

# ตารางเมตร >>> ไร่ งาน ตารางวา
sq_m2thaiArea <- function(sq_m) {
  sq_wa2thaiArea(sq_m / 4)  # best practice (ใช้ function ที่มีอยู่แล้ว)
}
sq_m2thaiArea(1600)
b <- sq_m2thaiArea(2100)
paste(b, collapse = "-")


# Lesson 23: Extract Open Government Data of Thailand ---------------------

# load all libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

# load input data
# csv
url <- "https://data.mhesi.go.th/dataset/78177753-2c92-4e8f-bc37-8d25dada4995/resource/09ee28c4-137f-4965-97a3-37ee5fa31eef/download/univ_grd_11_07_2563.csv"
df <- read_csv(url)
df

# excel
df <- openxlsx::read.xlsx("https://data.mhesi.go.th/dataset/78177753-2c92-4e8f-bc37-8d25dada4995/resource/0a70600f-dc5b-4095-b527-90040f8ed8ce/download/univ_grd_11_07_2563.xlsx")
df

# read tis-620, cp874, windows-874
url2 <- "https://data.go.th/dataset/fd781923-6c64-4cbc-90b8-83ad77c96ecd/resource/a542d7d4-bc27-4c03-81ef-bef0a5213210/download/-..-65.csv"
# df <- read_csv(url2)
df <- read_csv(url2, locale = locale(encoding = "cp874"))
df

# api-key from data.go.th >>> password = user token
api_key <- getPass::getPass()
headers <- c("api-key" = api_key)
url3 <- "https://opend.data.go.th/get-ckan/datastore_search?resource_id=0a70600f-dc5b-4095-b527-90040f8ed8ce&limit=5"

r <- GET(url3, add_headers(headers))
j <- content(r, "text")
# cat(j)  # ดูแบบ json
prettify(j)
# writeLines(prettify(j), "outfile.json")

dfs <- fromJSON(j)
dfs
dfs$result$fields
dfs$result$records

# หรือเรียกใช้ api ใหม่ให้เป็นระเบียบ
resource_id <- "0a70600f-dc5b-4095-b527-90040f8ed8ce"
limit <- 10
url3 <- glue("https://opend.data.go.th/get-ckan/datastore_search?resource_id={resource_id}&limit={limit}")

r <- GET(url3, add_headers(headers))
j <- content(r, "text")
prettify(j)

dfs <- fromJSON(j)
dfs
dfs$result$fields
dfs$result$records

# ถ้าจะทำให้ดีกว่านั้น
get_data_api <- function(api_key, resource_id, limit = 10) {
  headers <- c("api-key" = api_key)
  url3 <- glue("https://opend.data.go.th/get-ckan/datastore_search?resource_id={resource_id}&limit={limit}")
  
  r <- GET(url3, add_headers(headers))
  j <- content(r, "text")
  prettify(j)
  
  dfs <- fromJSON(j)
  return(dfs$result$records)
}

df <- get_data_api(api_key, "0a70600f-dc5b-4095-b527-90040f8ed8ce", 5)
df


# The End -----------------------------------------------------------------

# เก่งมาก พยายามได้ดีมาก ๆ เลย ยินดีด้วยนะ พยายามต่อไป อย่าได้หยุดล่ะ เธอทำได้แน่นอน สักวันจะต้องมีวันของเธอ

