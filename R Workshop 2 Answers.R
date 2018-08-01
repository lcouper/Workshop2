### Workshop 2 Answers

#1) 
View(UCBAdmissions) 

#1.1) 
ucb = UCBAdmissions

# 1.2)
ucb = as.data.frame(ucb)

#1.3) 
sum(ucb$Freq)

# 1.4) 
Admit = ucb[ucb$Admit == "Admitted",]
Reject = ucb[ucb$Admit == "Rejected",]
dim(Admit)
dim(Reject)

# 1.5)
sum(Admit$Freq)/ sum(ucb$Freq)

# 1.6)
AdmitM = Admit[Admit$Gender == "Male",]
AdmitF = Admit[Admit$Gender == "Female",]

# 1.7)
sum(AdmitM$Freq)/ sum(Admit$Freq)
sum(AdmitF$Freq)/ sum(Admit$Freq)

# 1.8) 
class(ucb$Dept) # Can see that they are factors - lets recode them

# 1.9)
# install.packages("plyr")
library(plyr)

# 1.10)
ucb$Dept = revalue(ucb$Dept, c("A"="Education", "B"="Biology", "C" = "History",
                               "D" = "Linguistics", "E" = "Math", "F" = "Physics"))

# 1.11)
ucb$Gender = revalue(ucb$Gender, c("Male" = "M", "Female" = "F"))

# 1.12)
ucbAgg = aggregate(ucb$Freq ~ ucb$Dept, FUN = "sum")

# 1.13.1) 
ucb2 = aggregate(ucb$Freq ~ ucb$Dept + ucb$Admit, FUN = "sum")

# 1.13.2)
colnames(ucb2) = c("Dept", "Admit", "Freq")

# 1.13.3)
ucb2A = ucb2[ucb2$Admit == "Admitted",]

# 1.13.4)
PerAdm = (ucb2A$Freq / ucbAgg[,2]) * 100

# 1.13.5)
PerAdm = as.integer(PerAdm)

# 1.13.6)
AdmbyD = cbind(PerAdm, ucbAgg[,1])

# 2)
md = read.csv("~/Downloads/MicrobiomeData.csv", header = TRUE, row.names = 1)

# 2.1)
md = md[,-1]

# 2.2)
rowSums(md) # Notice that they are all the same 

# 2.3)
mdt = t(md)

# 2.4)
class(mdt) # It got converted to a matrix during the tranpose step.
mdt = as.data.frame(mdt)

# 2.4)
mean(mdt$Rickettsia)

# 2.5)
Ricks = arrange(mdt, Rickettsia)

# 2.6)
sort(colSums(mdt)) # Shows us that Bacillus is the second most common (recall instructions say to discount "Other")

# 2.7)
Samples = rownames(mdt)

# 2.8)
mdt2 = cbind(Samples, mdt)

# 2.9)
Melted = melt(mdt2)

# 2.10)
hist(Melted$value)
# This shows us the distribution of sequence values for different bacteria,
# but ignores the specific name of the bacteria 
# I.e. doesn't care if it's called Rickettsia, Bacillus, other, etc, 
# It just looks at how many sequences are found in each case

# 3)
grades = read.csv("~/Downloads/Grades.csv", header= TRUE)

# 3.1)
class(grades$LastName)
grades$LastName = as.character(grades$LastName)

# 3.2)
class(grades$ReadingLevel)
class(grades$MathLevel)
# These should be factors since these are non-continuous data, with hierarchy (Reading level 9 > Reading Level 8)
grades$ReadingLevel = as.factor(grades$ReadingLevel)
grades$MathLevel = as.factor(grades$MathLevel)

# 3.3)
str(grades)

# 3.4)
View(grades) # the 2nd and 3rd row are duplicates

# 3.5)
duplicated(grades)
# This returns a logical output indicating whether that row is a duplicte of another row

# 3.6)
grades = grades[!duplicated(grades),]
# The ! means "not". It basically means to do the opposite of whatever follows

# 3.7) 
grades[grades$LetterGrade != "F",]

# 3.8.1)
Agg1 = aggregate(grades$NumGrade ~ grades$MathLevel, FUN = mean)

# 3.8.2)
Agg2 = aggregate(grades$NumGrade ~ grades$ReadingLevel, FUN = mean)

# 3.8.3)
plot(Agg1)
plot(Agg2)
# Reading level and math level do appear to correlate with students number grade on this exam

# 3.9)
# Suggest breaking apart data set first to include only those students who studied 3 or more hours
ThreeHours = grades[grades$HoursStudied >= 3,]
ThreeHours[which.min(ThreeHours$NumGrade),]

# 3.10)
# You could sort the dataset from low to high (or high to low) exam grades
Sorted = arrange(grades, NumGrade)

# 3.11)
gradesM = melt(grades, id.vars = c("LastName", "NumGrade", "LetterGrade", "HoursStudied"))

# 3.12)
PreExam = c(72, 74, 50, 79, 90, 88, 80, 70, 82, 90, 95, 90, 60)

# 3.13)
grades2 = cbind(grades, PreExam)

# 3.14)
grades2 = grades2[,c(1,2,7, 3, 4, 5, 6)]

# 3.15)
mean(grades2$PreExam)
mean(grades2$NumGrade)

# 3.16)
PrePost = grades2$NumGrade -grades2$PreExam

# 3.17)
grades2[which.max(PrePost),] # Synder improved the most

