#practising123
#14.1.2019
#this is wrangling part for chapter 3, the tables are Student performansce data that can be studied or downloaded here: https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)
library(ggplot2)
library(GGally)

mat <- read.table("data/student-mat.csv",sep = ";", header=TRUE)
por <- read.csv("data/student-por.csv", sep = ";", header=TRUE)
summary(mat)
dim(mat)
str(mat)
summary(por)
dim(por)
str(por)

join_by <- c( "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
dim(mat_por)
mat_por <- inner_join(mat, por, by = join_by, suffix = c(".mat", ".por"))
dim(mat_por)
str(mat_por)
summary(mat_por)
glimpse(mat_por)
colnames(mat_por)
# create a new data frame with only the joined columns
alc <- select(mat_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(mat)[!colnames(mat) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'mat_por' with the same original name
  two_columns <- select(mat_por, starts_with(column_name))
  
  
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- select(two_columns,1)
  }
}
alc <- mutate(alc, alc_use = Dalc + Walc / 2)
alc <- mutate(alc, high_use = alc_use > 2.0)

write.table(alc, file = "data/alc.csv", row.names = TRUE)



