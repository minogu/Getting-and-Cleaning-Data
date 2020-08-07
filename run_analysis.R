## Set proper working directory
old_dir <- getwd()
setwd("~/R/UCI HAR Dataset")

## Install necessary packages
library(dplyr)

## Merge 2 set of data (train and test) using bind_rows function from dplyr
## package.
## Do this for main variable data, vector of subjects and vector of activities.
## Remove test and train datasets to clean up memory, keep only "combined" 
## datasets / vectors.

train_data <- read.table("./train/X_train.txt")
test_data <- read.table("./test/X_test.txt")
combined_data <- bind_rows(train_data, test_data)
rm(train_data, test_data)

train_subjects <- read.table("./train/subject_train.txt")
test_subjects <- read.table("./test/subject_test.txt")
combined_subjects <- bind_rows(train_subjects, test_subjects)
rm(train_subjects, test_subjects)

train_activities <- read.table("./train/y_train.txt")
test_activities <- read.table("./test/y_test.txt")
combined_activities <- bind_rows(train_activities, test_activities)
rm(train_activities, test_activities)


## Read vector of variable labels from the features.txt file.
## Identify variables containing mean() or std() character strings and their 
## position in the list of variables using grepl and filter functions.
## Then select those variables from combined_data and overwrite combined_data.

feature_list <- read.table("./features.txt")
names(feature_list) <- c("nr", "name")
feature_list <- filter(feature_list, grepl("(mean\\(\\))|(std\\(\\))", name))
combined_data <- select(combined_data, feature_list$nr)

## Transform variable names from features.txt into more descriptive ones using
## mutate, sub and gsub functions as well as regular expressions:
## - replace "t" and "f" prefixes with "time" and "freq";
## - uppercase first letter in "mean" and "std" strings;
## - remove parentheses and dash characters (replace them with "").
## Then name columns in combined_data with transformed variable names.

feature_list <- feature_list %>% mutate(name = sub("^t", "time", name)) %>%
                                 mutate(name = sub("^f", "freq", name)) %>%
                                 mutate(name = sub("mean", "Mean", name)) %>%
                                 mutate(name = sub("std", "Std", name)) %>%
                                 mutate(name = gsub("[-()]", "", name))

names(combined_data) <- feature_list$name


## Replace activity numbers with descriptive names in the combined_activities
## vector. 
## To do that, first load activity labels from activity_labels.txt file. 
## Then use left_join function to assign corresponding activity label to each
## activity id in combined_activities. Finally, add those labels as new 
## variable (activityName) to our main dataset (combined_data).
## I used mutate() function with parameter .before = 1 to insert new column at 
## the beginning of the data frame.

activity_labels <- read.table("./activity_labels.txt")
names(activity_labels) <- c("nr", "name")
names(combined_activities) <- "nr"

combined_data <- combined_data %>% 
                  mutate(activityName = 
                  left_join(combined_activities, activity_labels)$name, 
                  .before = 1)

## Add variable subjectId at the beginning of combined_data dataset.

names(combined_subjects) <- "nr"
combined_data <- combined_data %>% mutate(subjectId = combined_subjects$nr, 
                                          .before = 1)

## Use group_by function to group our dataset by activityName and subjectId.
## Use summarize_all function to compute mean on all non-grouped variables.

combined_data <- combined_data %>% 
                  group_by(subjectId, activityName) %>%
                  summarize_all(mean)

## Write down result to output_data.txt file.
## Reset working directory to old path.

write.table(combined_data, "./output_data.txt", row.names = FALSE, 
            quote = FALSE)
setwd(old_dir)