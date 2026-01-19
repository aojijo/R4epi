# Descriptive Analysis
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