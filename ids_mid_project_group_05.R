student_data <- read.csv("ids_mid_project_group_05.csv")
head(student_data)
str(student_data)

summary(student_data)
dim(student_data)
names(student_data)

colSums(is.na(student_data))

student_data$Age[is.na(student_data$Age)] <- mean(student_data$Age, na.rm = TRUE)
student_data$Age <- as.integer(round(student_data$Age))
sum(is.na(student_data$Age))

mode_gender <- names(which.max(table(student_data$Gender)))
mode_gender
student_data$Gender[is.na(student_data$Gender)] <- mode_gender
sum(is.na(student_data$Gender))

mode_parent_edu <- names(which.max(table(student_data$ParentalEducation)))
mode_parent_edu
student_data$ParentalEducation[is.na(student_data$ParentalEducation)] <- mode_parent_edu
sum(is.na(student_data$ParentalEducation))

med_absence <- median(student_data$Absences, na.rm = TRUE)
med_absence
student_data$Absences[is.na(student_data$Absences)] <- med_absence
sum(is.na(student_data$Absences))

mode_tutoring <- names(which.max(table(student_data$Tutoring)))
mode_tutoring
student_data$Tutoring[is.na(student_data$Tutoring)] <- mode_tutoring
sum(is.na(student_data$Tutoring))

mode_parent_sup <- names(which.max(table(student_data$ParentalSupport)))
mode_parent_sup
student_data$ParentalSupport[is.na(student_data$ParentalSupport)] <- mode_parent_sup
sum(is.na(student_data$ParentalSupport))

mode_eca <- names(which.max(table(student_data$Extracurricular)))
mode_eca
student_data$Extracurricular[is.na(student_data$Extracurricular)] <- mode_eca
sum(is.na(student_data$Extracurricular))

mode_sports <- names(which.max(table(student_data$Sports)))
mode_sports
student_data$Sports[is.na(student_data$Sports)] <- mode_sports
sum(is.na(student_data$Sports))

mean_gpa <- mean(student_data$GPA, na.rm = TRUE)
mean_gpa
student_data$GPA[is.na(student_data$GPA)] <- mean_gpa
sum(is.na(student_data$GPA))

anyDuplicated(student_data)
student_data <- student_data[!duplicated(student_data), ]

numeric_cols <- c("Age", "StudyTimeWeekly", "Absences", "GPA")

for(col in numeric_cols) {
  x <- student_data[[col]]
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  outliers <- student_data[x < lower_bound | x > upper_bound, ]
  
  cat("\n============================\n")
  cat("Outliers in column:", col, "\n")
  cat("============================\n")
  
  if(nrow(outliers) == 0){
    cat("No outliers found.\n")
  } else {
    print(outliers)
  }
  
  student_data[[col]][x < lower_bound] <- lower_bound
  student_data[[col]][x > upper_bound] <- upper_bound
}

outliers <- NULL

for(col in numeric_cols) {
  x <- student_data[[col]]
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  outliers <- student_data[x < lower_bound | x > upper_bound, ]
  
  cat("\n============================\n")
  cat("Checking column:", col, "\n")
  cat("============================\n")
  
  if(nrow(outliers) == 0){
    cat("No outliers found.\n")
  } else {
    print(outliers)
  }
}

invalid_age <- which(!student_data$Age %in% 15:18)
student_data$Age[invalid_age]
invalid_age

student_data$Age[!student_data$Age %in% 15:18] <- NA
mean_age_NA <- mean(student_data$Age, na.rm = TRUE)
mean_age_NA
student_data$Age[is.na(student_data$Age)] <- mean_age_NA
student_data$Age <- as.integer(round(student_data$Age))

invalid_age <- which(!student_data$Age %in% 15:18)
student_data$Age[invalid_age]

invalid_gender <- which(!student_data$Gender %in% c(0,1))
invalid_gender

invalid_ethnicity <- which(!student_data$Ethnicity %in% 0:3)
student_data$Ethnicity[invalid_ethnicity]

ethnicity_map <- c(
  "Caucasian" = 0,
  "African American" = 1,
  "Asian" = 2,
  "Other" = 3
)

student_data$Ethnicity <- ifelse(
  student_data$Ethnicity %in% names(ethnicity_map),
  ethnicity_map[student_data$Ethnicity],
  student_data$Ethnicity
)

invalid_ethnicity <- which(!student_data$Ethnicity %in% 0:3)
student_data$Ethnicity[invalid_ethnicity]

student_data$Ethnicity[!student_data$Ethnicity %in% 0:3] <- NA
mode_eth <- names(which.max(table(student_data$Ethnicity)))
mode_eth
student_data$Ethnicity[is.na(student_data$Ethnicity)] <- mode_eth

invalid_ethnicity <- which(!student_data$Ethnicity %in% 0:3)
student_data$Ethnicity[invalid_ethnicity]

invalid_parent_edu <- which(!student_data$ParentalEducation %in% 0:4)
invalid_parent_edu

invalid_study_time <- student_data$StudyTimeWeekly < 0 | student_data$StudyTimeWeekly > 20
student_data$StudyTimeWeekly[invalid_study_time]

invalid_absence <- which(!student_data$Absences %in% 0:30)
invalid_absence
student_data$Absences[invalid_absence]

student_data$Absences[!student_data$Absences %in% 0:30] <- NA
med_absence_in <- median(student_data$Absences, na.rm = TRUE)
med_absence_in
student_data$Absences[is.na(student_data$Absences)] <- med_absence_in

invalid_absence <- which(!student_data$Absences %in% 0:30)
student_data$Absences[invalid_absence]

invalid_tutoring <- which(!student_data$Tutoring %in% c(0,1))
invalid_tutoring
student_data$Tutoring[invalid_tutoring]

student_data$Tutoring[!student_data$Tutoring %in% c(0,1)] <- NA
mode_Tu <- names(which.max(table(student_data$Tutoring)))
mode_Tu
student_data$Tutoring[is.na(student_data$Tutoring)] <- mode_Tu

invalid_tutoring <- which(!student_data$Tutoring %in% c(0,1))
student_data$Tutoring[invalid_tutoring]

invalid_pa_su <- which(!student_data$ParentalSupport %in% 0:4)
invalid_pa_su

invalid_extracurricular <- which(!student_data$Extracurricular %in% c(0,1))
invalid_extracurricular

invalid_game <- which(!student_data$Sports %in% c(0,1))
invalid_game

invalid_song <- which(!student_data$Music %in% c(0,1))
invalid_song

invalid_volun <- which(!student_data$Volunteering %in% c(0,1))
invalid_volun

invalid_gradeclass <- which(!student_data$GradeClass %in% 0:4)
invalid_gradeclass

student_data$Gender <- factor(student_data$Gender,
                              levels = c(0,1),
                              labels = c("Male","Female"))
table(student_data$Gender)

student_high_gpa <- student_data[student_data$GPA > 3, ]
head(student_high_gpa)


correlation_value <- cor(student_data$Height, student_data$GPA, use = "complete.obs")
correlation_value
correlation_value <- cor(student_data$StudentID, student_data$GPA, use = "complete.obs")
correlation_value


aggregate(GPA ~ GradeClass, data = student_data, mean)
aggregate(Absences ~ GradeClass, data = student_data, mean)
aggregate(StudyTimeWeekly ~ GradeClass, data = student_data, mean)

aggregate(GPA ~ Gender, data = student_data, mean)
aggregate(StudyTimeWeekly ~ Gender, data = student_data, mean)

aggregate(Absences ~ GradeClass, data = student_data, sd)
tapply(student_data$Absences, student_data$GradeClass, range)
tapply(student_data$Absences, student_data$GradeClass, IQR)

table(student_data$GradeClass)
max_count <- max(table(student_data$GradeClass))
balanced_data <- do.call(rbind, lapply(split(student_data, student_data$GradeClass), function(x) {
  x[sample(nrow(x), max_count, replace = TRUE), ]
}))
table(balanced_data$GradeClass)

balanced_data$GPA_norm <- (balanced_data$GPA - min(balanced_data$GPA)) / 
  (max(balanced_data$GPA) - min(balanced_data$GPA))
head(balanced_data$GPA_norm)

set.seed(123)
train_index <- sample(1:nrow(balanced_data), 0.7 * nrow(balanced_data))
train_data <- balanced_data[train_index, ]
test_data <- balanced_data[-train_index, ]
nrow(train_data)
nrow(test_data)
