#practising123
#5.1.19
#First wrangling excercise.
install.packages("dplyr")
install.packages("GGally")
install.packages('ggplot2', dep = TRUE)
library(dplyr)
library(ggplot2)
library(GGally)

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-
data.txt", sep="\t", header=TRUE)
dim(lrn14)
str(lrn14)
#reading the table from web and saving to lrn14. 183 rows/persons, 60 variables.

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
deep_columns <- select(lrn14, one_of(deep_questions))
deep <- rowMeans((deep_columns))
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(lrn14, one_of(surface_questions))
surf <- rowMeans(surface_columns)
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
strategic_columns <- select(lrn14, one_of(strategic_questions))
stra <- rowMeans(strategic_columns)

#taking means of courses to create new combination variables


keep_columns <- c("gender","Age","Attitude", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))
#decide columns to take and retrieve all of them to "learning2014"
#Choosing the columns, creating new data frame learning2014 for analyzing.
learning2014$deep <- deep
learning2014$stra <- stra
learning2014$surf <- surf
learning2014 <-filter(learning2014, Points>0)
str(learning2014)
dim(learning2014)
#insert compilation variables and filtering zero-pointers.

write.table(learning2014, file = "data/learning2014.txt", row.names = TRUE)

new_table <- read.table(file = "data/learning2014.txt", header=TRUE)
dim(new_table)
str(new_table)
new_table
learning2014
summarize((new_table))
#saving and loading from /data

getwd()
?summary()
p <- ggpairs(new_table, mapping = aes(col=gender, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p
#creating plot from the loaded table and printing it.
summary(new_table$Age)
table(cut(new_table$Age, breaks = seq(15,55,by=10)))
#


summary(new_table$gender) #Only 56/166 were men
summary(new_table$Age) 
summary(new_table$Points)
summary(new_table$stra)
summary(new_table$surf)
summary(new_table$deep)
summary(new_table$Attitude)

# initialize plot with data and aesthetic mapping
p1 <- ggplot(new_table, aes(x = Attitude, y = Points, col = gender))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title and draw the plot
p4 <- p3 + ggtitle("Student's attitude versus exam points")
p4
qplot(Points,deep, col=gender, data = new_table) + geom_smooth(method = "lm")
my_model <- lm(Points ~ Attitude + stra + surf, data = new_table)
summary(my_model)

#Apparently Attitude having .34 regression to points is significant, attitude has ***
my_model <- lm(Points ~ Attitude, data = new_table)
plot(my_model, which =c(1))
plot(my_model, which =c(1))
plot(my_model, which =c(2))
plot(my_model, which =c(5))
