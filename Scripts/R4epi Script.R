#1 Descriptive Analysis Chapter
#1.2 Numerical Descriptions of Catergorical Variables

demo <- tibble(
  id = c("001", "002", "003", "004"),
  age = c(30,67,52,56),
  edu =c(3,1,4,2),
  edu_char = c( "Some college", "Less than high school", "College graduate", 
                "High school graduate")
)
view(demo)
getwd()

demo <- demo |> mutate(
  edu_f = factor(x=edu, levels=1:4, labels=c( "Less than high school", "High school graduate", "Some college", "College graduate"))
)
as.numeric (demo$edu_f)
table(demo$edu_char)

demo <- demo |> mutate(
  edu_5catf = factor(x=edu, levels=1:5, labels=c( "Less than high school", "High school graduate", "Some college", "College graduate","Graduate school"))
)
view(demo)
table(demo$edu_char)
table(demo$edu_5catf)

demo <- demo |> mutate(
  edu_f_from_char = factor(x=edu_char, levels = c("Less than high school", "High school graduate", "Some college", "College graduate", "Graduate school"))
)
table(demo)
view(demo)
table(demo$edu_f_from_char)
#1.3 Measures of Central Tendecy (Mean mode and Median)
height_and_weight_20 <- tibble(
  id = c(
    "001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", 
    "012", "013", "014", "015", "016", "017", "018", "019", "020"),
  sex = c(1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2),
  sex_f = factor(sex, 1:2, c("Male", "Female")),
  ht_in = c(71, 69, 64, 65, 73, 69, 68, 73, 71, 66, 71, 69, 66, 68, 75, 69, 66, 65, 65, 65),
  wt_lbs = c(190, 176, 130, 154, 173, 182, 140, 185, 157, 155, 213, 151, 147, 196, 212, 190, 194, 176, 176, 102
  ))
height_and_weight_20
install.packages("gmodels")

height_and_weight_20 |> group_by(sex_f) |> summarise(n=n())
height_and_weight_20 |> count(sex_f) |> mutate(prop=n/sum(n))
height_and_weight_20 |> count(sex_f) |> mutate(percent= n/sum(n)*100)
height_and_weight_20 <- height_and_weight_20 |> mutate(sex_f = replace( sex, c(2,9),NA)) |> print()
height_and_weight_20 |> filter(!is.na(sex_f)) |> count(sex_f) |> mutate(percent=n/sum(n)*100)

install.packages("freqtables")
height_and_weight_20 |> filter(!is.na (sex_f)) |> freq_table(sex_f)
#Measures of Dispersion
height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)
height_and_weight_20
mean(height_and_weight_20$ht_in)
median(height_and_weight_20$ht_in)
mode(height_and_weight_20$ht_in)

#creating out own mode value for now
mode_val <- function(x) {
  value_counts <-table(x)
  max_count <- max(value_counts)
  index <- value_counts==max_count 
  unique_values <- names(value_counts)
  result <- unique_values[index]
  no_mode <- length(value_counts) == length(result)
  if(no_mode) {result <- NA}
  result
}
mode_val(height_and_weight_20$ht_in)

height_and_weight_20 %>% summarise(
  min_weight    = min(wt_lbs),
  mean_weight   = mean(wt_lbs),
  median_weight = median(wt_lbs),
  mode_weight   = mode_val(wt_lbs) %>% as.double(),
  max_weight    = max(wt_lbs)
)
height_and_weight_20 <- height_and_weight_20 %>% mutate(ht_in=replace(ht_in,c(1,2), NA)) %>% print()
height_and_weight_20 %>% filter(!is.na(ht_in)) %>%
  summarise(
    min_height    = min(ht_in),
    mean_height   = mean(ht_in),
    median_height = median(ht_in),
    mode_height   = mode_val(ht_in),
    max_height    = max(ht_in)
  )

height_and_weight_20 %>% 
  summarise(
    min_height    = min(ht_in, na.rm = TRUE),
    mean_height   = mean(ht_in, na.rm = TRUE),
    median_height = median(ht_in, na.rm = TRUE),
    mode_height   = mode_val(ht_in),
    max_height    = max(ht_in, na.rm = TRUE)
  )
install.packages("meantables")
height_and_weight_20 %>% 
  filter(!is.na(ht_in)) %>% mean_table(ht_in)

height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)
height_and_weight_20

height_and_weight_20 %>% 
  summarise(
    min_height  = min(ht_in),
    mean_height = mean(ht_in),
    max_height  = max(ht_in),
    range       = max_height - min_height
  )
var(c(rep(58,3), rep(78,3)))

sim_data <- tibble(
  all_68     = rep(68, 20),
  half_58_78 = c(rep(58, 10), rep(78, 10)),
  even_58_78 = seq(from = 58, to = 78, length.out = 20),
  half_48_88 = c(rep(48, 10), rep(88, 10)),
  even_48_88 = seq(from = 48, to = 88, length.out = 20)
)

sim_data

tibble(
  Column   = names(sim_data),
  Mean     = purrr::map_dbl(sim_data, mean),
  Variance = purrr::map_dbl(sim_data, var),
  SD       = purrr::map_dbl(sim_data, sd)
)

#1.4Continuous outcome vs Continous Predictor
set.seed(123)
df <- tibble(id=1:20,
             x=sample(x = 0:100, size = 20, replace = TRUE),
             y= sample(x = 0:100, size = 20, replace = TRUE)
)
df
ggplot(df, aes(x,y)) + geom_point()+
  geom_text(aes(label=id),nudge_x=1.5,nudge_y=2)+ theme_bw()
cor.test(x=df$x, y=df$y)

tibble(x = 1:10,y = 100:109,r = cor(x, y)) %>% 
  ggplot() +geom_point(aes(x, y)) +
  geom_text(aes(x = 2.5, y = 107.5, label = paste("r = ", r)), col = "blue") +
  theme_classic()

df <- tibble(  x = 1:-8,y = 100:91)
df
df %>% 
  mutate(r = cor(x, y)) %>% 
  ggplot() + geom_point(aes(x, y)) +
  geom_text(aes(x = -6, y = 98, label = paste("r = ", r)), col = "blue") +
  theme_classic()

class <- tibble(
  ht_in = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
            64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
            64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
            69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
            61, 69, 66, NA),
  wt_lbs = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
             125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
             186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
             147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
             110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
             163, 141, NA)
)
class
ggplot(class, aes(ht_in, wt_lbs)) +geom_jitter() +theme_classic()
cor.test(class$ht_in, class$wt_lbs)

options(scipen = 999)
cor.test(class$ht_in, class$wt_lbs)

ggplot(class, aes(ht_in, wt_lbs)) +geom_smooth(method = "lm") +
geom_jitter() +theme_classic()

#1.5 Continuous Outcome vs Categorical predictor
class <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>% 
  print()

class_summary <- class %>% filter(!is.na(ht_in)) %>% group_by(gender) %>% 
  summarise(
    n                    = n(),
    mean                 = mean(ht_in),
    `standard deviation` = sd(ht_in),
    min                  = min(ht_in),
    max                  = max(ht_in)
  ) %>% print()

class %>% 
  filter(!is.na(ht_in)) %>% 
  ggplot(aes(x = gender, y = ht_in)) +
  geom_jitter(aes(col = gender), width = 0.20) +
  geom_segment(
    aes(x = c(0.75, 1.75), y = mean, xend = c(1.25, 2.25), yend = mean, col = gender), 
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Height (Inches)") +
  scale_color_manual(values = c("#BC581A", "#00519B")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12))

class_summary <- class %>% filter(!is.na(bmi)) %>% group_by(gender, age_group) %>% 
summarise(
    n = n(),
    mean = mean(bmi),
    'standard deviation' = sd(bmi),
    min = min(bmi),
    max = max(bmi)) %>% print()

class %>% 
  filter(!is.na(bmi)) %>% 
  ggplot(aes(x = age_group, y = bmi)) +
  facet_wrap(vars(gender)) +
  geom_jitter(aes(col = age_group), width = 0.20) +
  geom_segment(
    aes(x = rep(c(0.75, 1.75), 2), y = mean, xend = rep(c(1.25, 2.25), 2), yend = mean, 
        col = age_group),
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Age Group") +
  scale_y_continuous("BMI") +
  scale_color_manual(values = c("#BC581A", "#00519B")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 10))

#1.6 Categorical outcome vs Catergotical Predictor
class <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA),
  genhlth   = c(2, 2, 3, 3, 2, 1, 2, 2, 2, 1, 3, 3, 1, 2, 2, 1, 2, NA, 3, 2, 3, 
                1, 2, 2, 2, 4, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 3, 3, 2, 1, 3, 3, 
                2, 2, 3, 3, 2, 3, 2, 2, 3, 5, 3, 2, 3, 2, 3, 3, 2, 2, 3, 3, 3, 
                1, 2, 2, 1, 3),
  persdoc   = c(1, 2, 2, 1, 2, 0, 0, 1, 2, 0, 2, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
                0, 1, 1, 1, 1, 2, 0, 0, 1, 1, 2, 1, 2, 0, 0, 2, 0, 0, 2, 2, 0, 
                NA, 0, 0, 0, 2, 0, 2, NA, 0, 2, 1, 1, 1, 2, 2, 0, 0, 0, 1, 2, 
                1, 1, 0, 0, 0, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat  = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese")),
    genhlth   = factor(genhlth, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    persdoc    = factor(persdoc, labels = c("No", "Yes, only one", "Yes, more than one"))
  ) %>% 
  print()

df <- filter(class, !is.na(bmi_3cat))
CrossTable(df$persdoc,df$genhlth)
#2 Data Management
df <- tibble(id = c(1, 2, 3),x  = c(0, 1, 0))
df %>% filter(df$x==0)
# 2.1 Creating and Modifying columns 

class <- tibble(names = c("John", "Brad", "Sally", "Anne"), heights = c(68,71,63,72))
class
class$heights <- class$heights/12
class
class$grades <- c(89,92,86,98)
class
class$heights[3]
class[["heights"]]

class[["heights"]] <- class[["heights"]] * 12
class

class[["rank"]] <- c(3, 2, 4, 1)
class

study_data <- tibble(id = c(1, 2, 3, 4),site = c("TX", "CA", "tx", "CA"))
study_data
study_data$site[3] <- "TX"
study_data
# 2.2 Subsetting Data Frames
set.seed(123)
drug_trial <- tibble(
id =rep(1:20,each=3),
year = rep(0:2,times=20),
age = sample(35:75,20,TRUE) %>% rep(each=3),
drug = sample(c("Placebo","Active"), 20, TRUE)%>% rep(each=3),
se_headache = if_else(drug=="Placebo",sample(0:1,60,TRUE,c(.95,.05)),sample(0:1,60,TRUE,c(.10,.90))),
se_diarrhea = if_else(drug == "Placebo",sample(0:1, 60, TRUE, c(.98,.02)), sample(0:1, 60, TRUE, c(.20, .80))),
se_dry_mouth = if_else(drug == "Placebo", sample(0:1, 60, TRUE, c(.97,.03)), sample(0:1, 60, TRUE, c(.30, .70))),
mi = if_else(drug == "Placebo", sample(0:1, 60, TRUE, c(.85, .15)), sample(0:1, 60, TRUE, c(.80, .20)))
)
drug_trial


drug_trial %>% 
  mutate(complete = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
drug_trial %>% 
  mutate(complete = 0)

drug_trial
class(drug_trial)
drug_trial_df <- as.data.frame(drug_trial)
class(drug_trial_df)
drug_trial_df$complete <- c(0,1)
drug_trial_df

drug_trial %>% mutate(complete = rep(0, 60))
drug_trial %>% mutate(mi_f = factor(mi, c(0, 1), c("No", "Yes")))

drug_trial %>% mutate(age_center = age - mean(age))

drug_trial %>% 
  mutate(
se_headache_f  = factor(se_headache, c(0, 1), c("No", "Yes")),
se_diarrhea_f  = factor(se_diarrhea, c(0, 1), c("N0", "Yes")),
se_dry_mouth_f = factor(se_dry_mouth, c(0, 1), c("No", "Yes"))
)

drug_trial %>% rowwise() %>% mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) > 0)

drug_trial_sub <- drug_trial %>% select(id, year, starts_with("se")) %>% 
print()
sum(drug_trial_sub$se_diarrhea)

drug_trial_sub %>% rowwise() %>% 
mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) > 0,
all_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) == 3
  )

# 2.3 Working with Dates
getwd()
birth_dates <- read.csv("Data/birth_dates.csv")
class(birth_dates)
birth_dates %>% mutate(posix_to_date = as.Date(dob_actual))%>%
  select(dob_actual,posix_to_date)
str(birth_dates)

birth_dates %>% mutate(dob_typical_into_date = as.Date(dob_typical,format = "%m/%d/%Y"))%>%
  select(dob_typical_into_date,dob_typical)
select(birth_dates,dob_long)
birth_dates %>% mutate(dob_long_into_date = as.Date(dob_long, format = "%B %d, %Y"))%>%
  select(dob_long_into_date,dob_long)
birth_dates %>% mutate(dob_abbreviated = format(dob_actual, "%d %b %y")) %>% 
  select(dob_actual, dob_abbreviated)
Sys.Date()
lubridate::today()
Sys.time()
lubridate::now()
month.name
month.abb
seq.Date(from = as.Date("2020-01-01"),
         to = as.Date("2020-01-15"),
         by = "days")
ages <- birth_dates %>% select(name_first, dob = dob_default) %>% 
print()
ages <- ages %>% mutate(today= as.Date("2020-05-07")) %>%
  print()
str(ages)
ages %>% mutate(age_subtraction =  as.numeric(today - dob) / 365.25,
age_difftime = as.numeric(difftime(today, dob))/365.25,
age_lubridate   = (dob %--% today)/ years(1))

ages %>% mutate(age_years=(dob%--%today) / years(1),
                age_last = trunc(age_years),
                age_near = round(age_years))
ages <- ages%>% select(-today) %>% print()
ages %>% 
  mutate(day   = day(dob),month = month(dob),year  = year(dob))
ages%>% arrange(dob)
ages %>% arrange(desc(dob))
# 2.4 Working with Character Strings
#couldnt fine the rds file, will make one and come back


#2.5 Conditional Operations
?if_else
rainy_days <- tibble(day=1:5,weather = c("rain", "rain", "rain", "no rain","rain"))
rainy_days
rainy_days %>% mutate(raincoat = if_else(weather=="rain","wear","no wear"))
2*3
df <- tibble(id = c(1, 1, 2, 2),outcome  = c(0, 1, 1, 1)) 
df %>% mutate(treatment = if_else(row_number()%% 2==1, "A", "B"))

blood_pressure <- tibble(
  id     = 1:10,
  sysbp  = c(152, 120, 119, 123, 135, 83, 191, 147, 209, 166),
  diasbp = c(78, 60, 88, 76, 85, 54, 116, 95, 100, 106))
blood_pressure %>% mutate(bp = if_else(sysbp < 120 & diasbp < 80, "Normal", "Not Normal"))

blood_pressure %>% 
  mutate(bp = case_when(
      sysbp < 120 & diasbp < 80 ~ "Normal",
      sysbp >= 120 & sysbp < 130 & diasbp < 80 ~ "Elevated",
      sysbp >= 130 & sysbp < 140 | diasbp >= 80 & diasbp < 90 ~ "Hypertension Stage 1",
      sysbp >= 140 | diasbp >= 90 ~ "Hypertension Stage 2"))

#2.6 Working with multiple Data frames
trial <- tibble(
  year    = c(2016, 2017, 2018, 2019),
  n       = c(501, 499, 498, 502),
  outcome = c(51, 52, 49, 50)) 
trial_2020 <- tibble(year = 2020, n = 500,outcome = 48, adv_event = 3) 
trial %>% bind_rows(trial_2020)

trial_2020 <- tibble(year = 2020,count = 500,adv_event = 3,outcomes  = 48)
trial %>% bind_rows(trial_2020)
trial %>% 
  bind_rows(trial_2020 %>% 
rename(n = count,
outcome = outcomes))

df1 <- tibble(
  color = c("red", "green", "blue"),
  size  = c("small", "medium", "large"))
df2 <- tibble(amount = c(1, 4, 3),dose= c(10, 20, 30))
df1
df2
df1 %>% bind_cols(df2)
# left_join() keeps all the rows from the x data frame in the resulting combined data frame. However, it only keeps the rows from the y data frame that have a key value match in the x data frame.
# Right_join() keeps all the rows from the y data frame in the resulting combined data frame, and only keep the rows from the x data frame that have a key value match in the y data frame. 
# Full join keeps all the rows from both data frames in the resulting combined data frame. 
# Inner join keeps only the rows from both data frames that have a key value match in the opposite data frame in the resulting combined data frame

#One to One merge
demographics <- tibble(
  id       = c("1001", "1002", "1003", "1004"),
  dob      = as.Date(c("1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12")),
  race_eth = c(1, 2, 2, 4))

grip_strength <- tibble(
  id     = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)) 
demographics
grip_strength
demographics %>% left_join(grip_strength, by = "id")

demographics <- tibble(
  id = c("1001", "1002", "1003", "1004", "1005"),
  dob = as.Date(c("1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12", "1942-06-07")),
  race_eth = c(1, 2, 2, 4, 3))
demographics
demographics %>% left_join(grip_strength, by = "id")
grip_strength <- tibble(
  pid    = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)) 
grip_strength
demographics %>% left_join(grip_strength, by = c("id" = "pid"))
# make sure that the first column name you pass to the named vector (i.e., "id") is the name of the key column in the x data frame and that the second column name you pass to the named vector (i.e., "pid") is the name of the key column in the y data frame.

#One to many merge
grip_strength <- tibble(
  id     = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit  = rep(c("pre", "post"), 4),
  grip_r = c(32, 33, 28, 27, 32, 34, 22, 27),
  grip_l = c(30, 32, 30, 30, 28, 30, 22, 26))
grip_strength
demographics
demographics %>% left_join(grip_strength, by = "id")
# Merging multiple databses with multiple key columns(Many to many)
emr <- tibble(
  id     = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit  = rep(c("pre", "post"), 4),
  weight = c(105, 99, 200, 201, 136, 133, 170, 175))
emr
demographics %>% left_join(grip_strength, by = "id") %>% left_join(emr, by = c("id", "visit"))
# Restructuring Data Frames
babies <- tibble(
  id   = 1001:1008,
  sex  = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19)) %>% print()

babies_long <- babies %>% pivot_longer(cols = starts_with("weight"),
names_to  = "months",
names_prefix = "weight_",
values_to  = "weight") %>% print()

babies %>% pivot_longer(cols = starts_with("weight"),names_to = "months")

babies %>% pivot_longer(
cols = starts_with("weight"),
names_to= "months",
names_prefix = "weight_",
values_to  = "weight")

babies %>% pivot_longer(
cols = starts_with("weight"),
names_to  = "months",
names_prefix = "weight_",
values_to = "weight") %>% mutate(months = as.integer(months))

set.seed(123)
babies <- tibble(
  id       = 1001:1008,
  sex      = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19),
  length_3  = c(17, 19, 23, 20, 18, 22, 21, 18),
  length_6  = round(length_3 + rnorm(8, 2, 1)),
  length_9  = round(length_6 + rnorm(8, 2, 1)),
  length_12 = round(length_9 + rnorm(8, 2, 1)),
) %>% print()

babies_long <- babies %>% pivot_longer(
cols = c(-id,-sex),
names_to  = "months",
names_prefix = "weight_",
values_to = "weight") %>% print()

