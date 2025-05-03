library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Load Excel files
students_path <- "no of students in kpk universities.xlsx"
teachers_path <- "no of teachers in kpk universities.xlsx"

# Read data (assuming first sheet)
students_raw <- read_excel(students_path)
teachers_raw <- read_excel(teachers_path)

# Transpose, clean, and convert to tidy format
students_df <- as.data.frame(t(students_raw))
colnames(students_df) <- "Number_of_Students"
students_df$University <- rownames(students_df)
students_df <- students_df %>% slice(-1) %>% select(University, Number_of_Students)
students_df$Number_of_Students <- as.numeric(students_df$Number_of_Students)

teachers_df <- as.data.frame(t(teachers_raw))
colnames(teachers_df) <- "Number_of_Teachers"
teachers_df$University <- rownames(teachers_df)
teachers_df <- teachers_df %>% slice(-1) %>% select(University, Number_of_Teachers)
teachers_df$Number_of_Teachers <- as.numeric(teachers_df$Number_of_Teachers)

# Merge and compute ratio
merged_df <- inner_join(students_df, teachers_df, by = "University") %>%
  mutate(Student_Teacher_Ratio = round(Number_of_Students / Number_of_Teachers, 2)) %>%
  arrange(desc(Student_Teacher_Ratio))

# Plot 1: Top 10 by student count
fig_students <- merged_df %>%
  arrange(desc(Number_of_Students)) %>%
  head(10) %>%
  plot_ly(
    x = ~Number_of_Students,
    y = ~reorder(University, Number_of_Students),
    type = 'bar',
    orientation = 'h',
    marker = list(color = 'royalblue')
  ) %>%
  layout(title = "Top 10 Universities by Student Population",
         xaxis = list(title = "Number of Students"),
         yaxis = list(title = ""))

# Plot 2: Top 10 by teachers
fig_teachers <- merged_df %>%
  arrange(desc(Number_of_Teachers)) %>%
  head(10) %>%
  plot_ly(
    x = ~Number_of_Teachers,
    y = ~reorder(University, Number_of_Teachers),
    type = 'bar',
    orientation = 'h',
    marker = list(color = 'darkorange')
  ) %>%
  layout(title = "Top 10 Universities by Number of Teachers",
         xaxis = list(title = "Number of Teachers"),
         yaxis = list(title = ""))

# Plot 3: Top 10 by student-teacher ratio
fig_ratio <- merged_df %>%
  arrange(desc(Student_Teacher_Ratio)) %>%
  head(10) %>%
  plot_ly(
    x = ~Student_Teacher_Ratio,
    y = ~reorder(University, Student_Teacher_Ratio),
    type = 'bar',
    orientation = 'h',
    marker = list(color = 'crimson')
  ) %>%
  layout(title = "Top 10 Universities by Student-Teacher Ratio",
         xaxis = list(title = "Ratio"),
         yaxis = list(title = ""))

# Save as HTML
saveWidget(fig_students, "top_students_universities.html")
saveWidget(fig_teachers, "top_teachers_universities.html")
saveWidget(fig_ratio, "top_ratio_universities.html")
