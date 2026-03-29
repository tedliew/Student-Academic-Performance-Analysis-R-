#Teo Voon Xiong,TP068594
#Heng Kai Yong, TP066518
#Liew Wee Ted, TP065294
#William Soon Wei Shawnn, TP069933


#import data/csv and install packages
student_prediction = read.csv("C:\\Users\\xiong\\OneDrive - Asia Pacific University\\Year2Sem1\\PFDA\\Assignment\\student_prediction.csv", header = TRUE)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plotrix")
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotrix)

#CLEANING

#changing column names (more understandable names)

names(student_prediction) = c("Student_ID", "Age_Group", "Gender", "HighSchool_Type", "Schorship_Type", "Part_Time", 
                              "Sports_Arts", "Partner", "Total_Salary",
                              "Commute_Type", "Accomodation", "Mom_EducationLevel", "Father_EducationLevel", 
                              "NumofSiblings", "ParentsStatus", "Mom_Occupation", 
                              "Father_Occupation", "StudyHours", "Frequency_Reading(Non_Scientific)", 
                              "Frequency_Reading(Scientific)", "Attendance(Seminar_Conf)",
                              "Project_impact", "Class_Attendance", "Type_MidtermPrep", "Frequency_MidtermPrep", 
                              "Frequency_NotesTaking", "ListeningInClass_Frequency",
                              "Effects_Discussion", "Classroom_Flip", "CGPA", "Expected_CGPA", "ID_Course", "Final_Grade")

#Eliminate data
#null values
student_prediction <-na.omit(student_prediction)

#duplication_removal
student_prediction <- unique(student_prediction)


#copy another dataset to prevent error in data replacement
student <- student_prediction

#replace the row with understandable value
student_prediction$Age_Group <- factor(student_prediction$Age_Group, levels = 1:3, labels = c("18-21","22-25","above 26"))
student_prediction$Gender <- factor(student_prediction$Gender,levels = 1:2,labels=c("female","male"))
student_prediction$HighSchool_Type <- factor(student_prediction$HighSchool_Type,levels=1:3,labels=c("private","state","other"))
student_prediction$Schorship_Type <- factor(student_prediction$Schorship_Type,levels=1:5, labels=c("None","25%","50%","75%","Full"))
student_prediction$Part_Time <- factor(student_prediction$Part_Time,levels = 1:2,labels=c("Yes","No"))
student_prediction$Sports_Arts <- factor(student_prediction$Sports_Arts,levels=1:2,labels=c("Yes","No"))
student_prediction$Partner <- factor(student_prediction$Partner,levels=1:2,labels=c("Yes","No"))
student_prediction$Total_Salary <- factor(student_prediction$Total_Salary,levels=1:5,labels=c("USD 135-300","USD 201-270","USD 271-340","USD 341-410","above USD 410"))
student_prediction$Commute_Type <- factor(student_prediction$Commute_Type,level=1:4,labels=c("bus","private_car_taxi", "bicycle","other"))
student_prediction$Accomodation <- factor(student_prediction$Accomodation,levels=1:4, labels=c("rental", "dorm", "family", "other"))
student_prediction$Mom_EducationLevel <- factor(student_prediction$Mom_EducationLevel, levels=1:6, labels =c("Primary", "Secondary", "High", "Uni", "Master","PHD"))
student_prediction$Father_EducationLevel <- factor(student_prediction$Father_EducationLevel, levels=1:6, labels =c("Primary", "Secondary", "High", "Uni", "Master","PHD"))
student_prediction$NumofSiblings <- factor(student_prediction$NumofSiblings, levels = 1:5, labels = c("1", "2", "3", "4", "5 or above"))
student_prediction$ParentsStatus <- factor(student_prediction$ParentsStatus, levels = 1:3, labels = c ("married", "divorced", "Passed_Away"))
student_prediction$Mom_Occupation <- factor(student_prediction$Mom_Occupation, levels = 1:6, labels = c("retired", "housewife", "government", "private_sector", "self_employed", "other"))
student_prediction$Father_Occupation <-factor(student_prediction$Father_Occupation, levels= 1:5, labels = c("retired", "government", "private_sector", "self_employed", "other"))
student_prediction$StudyHours <-factor(student_prediction$StudyHours, levels= 1:5, labels=c("None", "<5hours", "6-10 hours", "11-20hours","more than 20 hours"))
student_prediction$"Frequency_Reading(Non_Scientific)"<- factor(student_prediction$"Frequency_Reading(Non_Scientific)",levels = 1:3, labels = c("None", "Sometimes", "Often"))
student_prediction$"Frequency_Reading(Scientific)" <- factor(student_prediction$"Frequency_Reading(Scientific)",levels = 1:3, labels = c("None", "Sometimes", "Often"))
student_prediction$"Attendance(Seminar_Conf)" <- factor(student_prediction$"Attendance(Seminar_Conf)", levels = 1:2, labels =c("Yes", "No"))
student_prediction$Project_impact <- factor(student_prediction$Project_impact, levels = 1:3, labels = c("postive", "negative", "neutral"))
student_prediction$Class_Attendance <-factor(student_prediction$Class_Attendance, levels = 1:3, labels =c("Always", "Sometimes", "Never" ))
student_prediction$Type_MidtermPrep <- factor(student_prediction$Type_MidtermPrep, levels = 1:3, labels = c("alone", "With_Friends", "NotApplicable"))
student_prediction$Frequency_MidtermPrep <- factor(student_prediction$Frequency_MidtermPrep, levels = 1:3, labels=c("CloseToExam", "RegularlyInSem", "Never"))
student_prediction$Frequency_NotesTaking <- factor(student_prediction$Frequency_NotesTaking, levels = 1:3, labels = c("Never", "Sometimes", "always"))
student_prediction$ListeningInClass_Frequency <-factor(student_prediction$ListeningInClass_Frequency, levels = 1:3, labels = c("Never", "Sometimes", "always"))
student_prediction$Effects_Discussion <-factor(student_prediction$Effects_Discussion, levels = 1:3, labels = c("Never","Sometimes", "Always"))
student_prediction$Classroom_Flip <-factor(student_prediction$Classroom_Flip, levels = 1:3, labels =c("not_useful", "useful", "Not_Applicable"))
student_prediction$CGPA <-factor(student_prediction$CGPA, levels = 1:5, labels =c("<2", "2.00-2.49", "2.50 - 2.99", "3.00 - 3.49", "3.49>"))
student_prediction$Expected_CGPA <-factor(student_prediction$Expected_CGPA,levels = 1:5, labels =c("<2", "2.00-2.49", "2.50 - 2.99", "3.00 - 3.49", "3.49>"))
student_prediction$Final_Grade <-factor(student_prediction$Final_Grade, levels = 0 :7, labels=c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"))


#PREPROCESSING
#To show the sample data of the student prediction
head(student_prediction$Mom_EducationLevel,10)
tail(student_prediction$Mom_EducationLevel,10)

summary(student_prediction$Mom_EducationLevel)
summary(student_prediction$Final_Grade)



#Objective 1 - To investigate the relationship between taking notes in class and Grade.
#Analysis 1-1 -	Find the correlation between frequency of notes taking and final grades.
#data preparation
A<-nrow(subset(student_prediction,Frequency_NotesTaking == "Never")) # 53 rows does never taking notes
B<-nrow(subset(student_prediction,Frequency_NotesTaking == "Sometimes")) # 587 rows does sometimes taking notes
C<-nrow(subset(student_prediction,Frequency_NotesTaking == "always")) # 894 rows does always taking notes

grade_counts <- student_prediction %>% filter(Frequency_NotesTaking == "Never") %>%
  group_by(Frequency_NotesTaking, Final_Grade) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / A * 100) %>%
  arrange(Frequency_NotesTaking, Final_Grade)  %>% print(n=Inf);

#to create subset of data that frequency is equals to sometimes
grade_counts_A <- student_prediction %>% 
  group_by(Frequency_NotesTaking, Final_Grade) %>% filter(Frequency_NotesTaking == "Sometimes") %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / B * 100) %>%
  arrange(Frequency_NotesTaking, Final_Grade)  %>% print(n=Inf);

#to create subset of data that frequency is equals to always
grade_counts_B <- student_prediction %>% filter(Frequency_NotesTaking == "always") %>%
  group_by(Frequency_NotesTaking, Final_Grade) %>%
  summarise(Count = n(), ) %>%
  mutate(Percentage = Count / C * 100) %>%
  arrange(Frequency_NotesTaking, Final_Grade)  %>% print(n=Inf);

#to combine the three different data with their respective percentage by category
combined_data <- bind_rows(grade_counts,grade_counts_A,grade_counts_B) %>% print(n=Inf);

#plot
ggplot(combined_data, aes(sample = Percentage, color = Frequency_NotesTaking)) + 
  stat_qq(size = 2) + 
  stat_qq_line(size = 2) +
  labs(
    title = "Quantile-quantile between Frequency_NotesTaking and Final_Grade",
    x = "Z-score",
    y = "Count",
    fill = "Final_Grade"
  )


#Analysis 1-2 -	Compare the distribution of final grades based on the frequency of notes taking. 
hello<-student_prediction 
hello$Frequency_NotesTaking <- factor(student_prediction$Frequency_NotesTaking, levels = c("Never", "Sometimes", "always"), labels = 1:3)
hello$Final_Grade <-factor(student_prediction$Final_Grade, levels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"),labels = 1:8)
df <- hello %>% 
  group_by(Final_Grade, Frequency_NotesTaking) %>% 
  summarise(Count = n(), .groups = "drop") %>%
  print(n=Inf)
Grade = as.numeric(df$Final_Grade)
Notes = as.numeric(df$Frequency_NotesTaking)

ggplot(df, aes(x = as.factor(Final_Grade), y = as.factor(Frequency_NotesTaking))) +
  geom_point(aes(size = Count, color = as.factor(Frequency_NotesTaking)), position = position_jitter(width = 0.3, height = 0.3), alpha = 0.5) +
  scale_size_area(max_size = 30) +
  geom_text(aes(label = Count), vjust = 1.5, color = "black") +
  labs(title = "Bubble Plot of Final Grade and Frequency of Notes Taking",
       x = "Final Grade", y = "Frequency of Notes Taking", color = "Frequency of Notes Taking", size = "Count") +
  theme_minimal()


#Analysis 1-3 - Analyze if students who always take notes in class also start preparing for exams earlier or more regularly, and the impact on their grades.
grade_counts_extra_A3 <- student_prediction %>%
  group_by(Frequency_NotesTaking, Frequency_MidtermPrep, Final_Grade) %>%
  summarise(Count = n()) %>%
  arrange(as.character(interaction(Frequency_NotesTaking, Frequency_MidtermPrep)), Final_Grade) %>%
  print(n = Inf)

ggplot(grade_counts_extra_A3, aes(x = interaction(Frequency_NotesTaking, Frequency_MidtermPrep, sep = ":", lex.order = TRUE), y = Count, fill = Final_Grade)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count), position = position_stack(vjust =0.5)) +
  labs(title = "Stacked Bar Chart between final grade and frequency of notes taking and frequency of midterm preparation",
       x = "(Frequency_notestaking: midterm preparation)", y = "Count") 


#Analysis 1-4	Investigate the relationship between the effect of discussion and frequency of notes taking based on the final grades.
grade_counts_extra_4 <- student_prediction %>%
  group_by(Frequency_NotesTaking, Effects_Discussion, Final_Grade) %>%
  summarise(Count = n()) %>%
  arrange(as.character(interaction(Frequency_NotesTaking, Effects_Discussion)), Final_Grade) %>%
  print(n = Inf)

ggplot(grade_counts_extra_4, aes(x = interaction(Frequency_NotesTaking, Effects_Discussion, sep = ":", lex.order = TRUE), y = Count, color = Final_Grade, group = Final_Grade)) +
  geom_line() +
  labs(title = "Line Graph between frequency of notes taking, effects of discussion, and final grade",
       x = "(Frequency_notestaking : Effects_Discussion)", y = "Count") +
  theme_minimal()


#Analysis 1-5	How the education level of a student’s mother might affect the student’s final grade, among students who do not, sometimes, and always take notes.
grade_counts_extra_A <- student_prediction %>%
  group_by(Frequency_NotesTaking, Mom_EducationLevel, Final_Grade) %>%
  summarise(Count = n()) %>%
  arrange(Frequency_NotesTaking, Mom_EducationLevel, Final_Grade) %>%
  print(n = Inf)

grade_counts_table <- with(grade_counts_extra_A, table(Frequency_NotesTaking, Mom_EducationLevel))

# Perform the Chi-square test of independence
chi_square_result <- chisq.test(grade_counts_table)

# Print the result
print(chi_square_result)


ggplot(grade_counts_extra_A, aes(x = interaction(Frequency_NotesTaking, Mom_EducationLevel, sep = ":", lex.order = TRUE),y=Mom_EducationLevel)) +
  geom_count(aes(size=Count,color=Count)) +
  labs(
    title = "Counts Plot of Final Grades by Mother's Education Level and Notes Taking Frequency",
    x = "(Frequency_NotesTaking(Never, Sometimes, Always):Mother's Education Level(Primary to PHD))",
    y = "Mother's Education Level"
  ) +
  theme_minimal()


#Extra Feature 1
# Summarise() : to aggregate data to create summary statistics. In my cases, I have using this function to find the count for each of the combinations of variables.
# Mutate(): To add new rows. In my cases, I am using the mutate function to create a new column called percentage for my data preparation to plot the graph.
# Arrange(): To arrange order of rows for a data frame. I have used it to arrange the sequence for my data frame.
# Labs(): To set the labels and annotations of a plot. Used to set the labels for x-axis, y-axis and the title.
# Theme_minimal(): Applies a minimalistic theme for plot. To provide better visualization for my graph.
# Ggtitle(): To set title for my plot.
# As.numeric(): Convert the categorical variable to a numerical type.
# Position(position = “position_dodge” // “position_jitter” // “stack”): Adjusting the position of element in my bar chart.
# As.character(): To convert the variable into character data type.
# Interaction(): Creates interaction terms between variables in plotting graph. Mostly used for combining and interacting two independent variables for analysing purpose.
# Facet_wrap(): To separate the variables into multiple facets.




#Objective 2: To investigate the relationship between Mother's education level and Grade.
#Analysis 2-1 -- To show the distribution of mother's education level and father's education level
Mom_Edu = student_prediction %>% 
  select(EducationLevel = Mom_EducationLevel) %>% 
  group_by(EducationLevel) %>%
  summarize(Total = n()) %>%
  cbind(Parent = "Mom")

Dad_Edu = student_prediction %>% 
  select(EducationLevel = Father_EducationLevel) %>% 
  group_by(EducationLevel) %>%
  summarize(Total = n()) %>% 
  cbind(Parent = "Dad")

Edu = rbind(Mom_Edu,Dad_Edu)

ggplot(Edu,aes(fill = Parent, x = EducationLevel, y = Total)) + 
  geom_bar(position = "dodge",stat = "identity", width = 0.9) + 
  geom_text(aes(label=Total))  + 
  labs(title = "Distribution of parents' education level")

#Show summary
summary(Mom_Edu$Total)
summary(Dad_Edu$Total)


#Analysis 2-2 -- To find out how mother's education level affects student's grades.
#Mom_High is above high school
#Categorize the group
Mom_High <- subset(student_prediction,Mom_EducationLevel=="High"|Mom_EducationLevel == "Uni"|
                     Mom_EducationLevel=="Master"|Mom_EducationLevel=="PHD" ,select = c(Final_Grade)) 

Mom_Low <- subset(student_prediction,Mom_EducationLevel=="Primary"|
                    Mom_EducationLevel=="Secondary",select = c(Final_Grade)) 

#Calculate the number of each grade and add label "High Level" and "Low Level"
Copy_Mom_High <- Mom_High %>% group_by(Final_Grade) %>%
  summarise(Total = n()) %>% cbind(Mom_Level = "High Level")
Copy_Mom_Low <- Mom_Low %>% group_by(Final_Grade) %>% 
  summarise(Total = n()) %>% cbind(Mom_Level = "Low Level")

Mom_mix = rbind(Copy_Mom_High,Copy_Mom_Low)

ggplot(Mom_mix, aes(x=Final_Grade, y=Total, group = Mom_Level, color = Mom_Level)) + 
  geom_line(stat = "identity",linewidth = 2) + 
  labs(title = "Line chart of count of each grade(Mother)")

ggplot(Mom_mix,aes(sample = Total,color = Mom_Level)) + 
  stat_qq(size = 5) + 
  stat_qq_line(size = 2)+
  labs(title = "QQ-plot from Mom_EducationLevel and each grade and level", 
       x = "Theoretical Quantile", y = "Actual Quantile")

#Show summary
summary(Copy_Mom_High$Total)
summary(Copy_Mom_Low$Total)


#Analysis 2-3 -- To determine how father's education level affects student's grades.
#Seperate the education level to high and low
Father_High <- subset(student_prediction,Father_EducationLevel=="High"|Father_EducationLevel == "Uni"|
                        Father_EducationLevel=="Master"|Father_EducationLevel=="PHD" ,select = c(Final_Grade))
Father_Low <- subset(student_prediction,Father_EducationLevel=="Primary"|Father_EducationLevel=="Secondary",
                     select = c(Final_Grade))

#Count the number of each final grade
Copy_Father_High <- Father_High %>% group_by(Final_Grade) %>% summarise(Total = n()) %>% 
  cbind(Father_Level = "High Level")
Copy_Father_Low <- Father_Low %>% group_by(Final_Grade) %>% summarise(Total = n()) %>% 
  cbind(Father_Level = "Low Level") 

Father_mix = rbind(Copy_Father_High,Copy_Father_Low)

ggplot(Father_mix, aes(x=Final_Grade, y=Total, group = Father_Level, color = Father_Level)) + 
  geom_line(stat = "identity",linewidth = 2) + 
  labs(title = "Line chart of count of each grade(Father)")

ggplot(Father_mix,aes(sample = Total, color = Father_Level)) + 
  stat_qq(size = 5) + 
  stat_qq_line(size = 2) + 
  labs(title = "QQ-plot from Father_EducationLevel and each grade", 
       x = "Theoretical Quantile", y = "Actual Quantile")

#Show summary
summary(Copy_Father_High$Total)
summary(Copy_Father_Low$Total)


#Analysis 2-4 --To analyse the impact of parent's education level to grade of student. 
#Add a new subset to compare "Father_level" and "Mom_Level"
df_parents <- select(student_prediction,c(Mom_EducationLevel,Father_EducationLevel,Final_Grade))

#Make the values in df_parents become character to change value name easier
df_parents$Mom_EducationLevel <- as.character(df_parents$Mom_EducationLevel)
df_parents$Father_EducationLevel <- as.character(df_parents$Father_EducationLevel)

#Category the education level
df_parents$Mom_EducationLevel[df_parents$Mom_EducationLevel=="High"|
                                df_parents$Mom_EducationLevel=="Uni"|
                                df_parents$Mom_EducationLevel=="Master"|
                                df_parents$Mom_EducationLevel=="PHD"] <- "High_Level"
df_parents$Mom_EducationLevel[df_parents$Mom_EducationLevel=="Primary"|
                                df_parents$Mom_EducationLevel=="Secondary"] <- "Low_Level"

df_parents$Father_EducationLevel[df_parents$Father_EducationLevel=="High"|
                                   df_parents$Father_EducationLevel=="Uni"|
                                   df_parents$Father_EducationLevel=="Master"|
                                   df_parents$Father_EducationLevel=="PHD"] = "High_Level"
df_parents$Father_EducationLevel[df_parents$Father_EducationLevel=="Primary"|
                                   df_parents$Father_EducationLevel=="Secondary"] = "Low_Level"

#Make the values become factor
df_parents$Mom_EducationLevel <- as.factor(df_parents$Mom_EducationLevel)
df_parents$Father_EducationLevel <- as.factor(df_parents$Father_EducationLevel)
str(df_parents)

#Count each level of education of parent 
summary_df_parents <- df_parents %>% 
  group_by(Mom_EducationLevel,Father_EducationLevel,Final_Grade) %>%
  summarise(Total = n())

Level_Mother_Father = interaction(summary_df_parents$Mom_EducationLevel,summary_df_parents$Father_EducationLevel)

ggplot(summary_df_parents, aes(fill = Level_Mother_Father,x = Final_Grade, y = Total)) + 
  geom_bar(position = "dodge", stat = "identity",width=0.8) + 
  geom_text(aes(label=Total)) + 
  labs(title = "Category of each level and total number of student in each grade")


#Extra Feature 2
# lab (): Analysis 1, 2, 3, 4. Used to add title and label the x-axis and y-axis of diagram. 
# n () : Analysis 1, 2, 3. Used to calculate the number of row for summarise() function which is grouped by group_by() function. 
# stat_qq (): Analysis 2, 3. Used to produce the quantile-quantile plot. 
# stat_qq_line (): Analysis 2, 3. Used to connect the points of theorical distribution.




# Objective 3 -	To investigate the relationship between the frequency of listening in class and the grade.
#Preprocessing
#Finding number of rows for Frequency of Taking Notes
subset(student_prediction,ListeningInClass_Frequency == "Never")
subset(student_prediction,ListeningInClass_Frequency == "Sometimes")
subset(student_prediction,ListeningInClass_Frequency == "always")
x_and_A <- nrow(subset(student_prediction,ListeningInClass_Frequency == "Never"))  #314 rows never listens in class
x_and_A
x_and_B <-nrow(subset(student_prediction,ListeningInClass_Frequency == "Sometimes")) #840 rows sometimes listen in class
x_and_B
x_and_C <-nrow(subset(student_prediction,ListeningInClass_Frequency == "always")) #380 rows always listen in class
x_and_C

#sampling same type and number of random row from each frequency of note taking
r_and_A <- sample_n(subset(student_prediction,ListeningInClass_Frequency == "Never", select = c(Final_Grade,ListeningInClass_Frequency)),25)
r_and_A
r_and_B <- sample_n(subset(student_prediction,ListeningInClass_Frequency == "Sometimes", select = c(Final_Grade,ListeningInClass_Frequency)),25)
r_and_B
r_and_C <- sample_n(subset(student_prediction,ListeningInClass_Frequency == "always", select = c(Final_Grade,ListeningInClass_Frequency)),25)
r_and_C
# Based on Gender listening to the class and s
always_taking_notes <- student_prediction %>%
  filter(ListeningInClass_Frequency == "always")
gender_note_takers <- always_taking_notes %>%
  group_by(Gender) %>%
  summarize(Count = n())
print(gender_note_takers)
View(gender_note_takers)

grade_counts <-rbind(r_and_A,r_and_B,r_and_C)

#Filtering Listening in class count and final grade

grade_counts_Never <- student_prediction %>%
  filter(ListeningInClass_Frequency == "Never") %>%
  group_by(ListeningInClass_Frequency, Final_Grade) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(ListeningInClass_Frequency, Final_Grade) %>%
  print(n = Inf)
View(grade_counts_Never)

grade_counts_Sometimes <- student_prediction %>%
  filter(ListeningInClass_Frequency == "Sometimes") %>%
  group_by(ListeningInClass_Frequency, Final_Grade) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(ListeningInClass_Frequency, Final_Grade) %>%
  print(n = Inf)
View(grade_counts_Sometimes)

grade_counts_always <- student_prediction %>%
  filter(ListeningInClass_Frequency == "always") %>%
  group_by(ListeningInClass_Frequency, Final_Grade) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(ListeningInClass_Frequency, Final_Grade) %>%
  print(n = Inf)
View(grade_counts_always)

#Male and Female AA or BA who always take notes
notes_and_AA_or_BA_takers <- student_prediction %>%
  filter(ListeningInClass_Frequency == "always" & (Final_Grade == "AA" | Final_Grade == "BA" | Final_Grade == "BB" | Final_Grade == "CB" | Final_Grade == "CC" | Final_Grade == "DC" | Final_Grade == "DD" | Final_Grade == "Fail")) 

gender_and_AA_or_BA_takers <- notes_and_AA_or_BA_takers %>%
  group_by(Gender,ListeningInClass_Frequency, Final_Grade) %>%
  summarize(Count = n())

gender_and_AA_or_BA_takers <- gender_and_AA_or_BA_takers %>%
  complete(Final_Grade, fill = list(Count = 0))

# Print the result
print(gender_and_AA_or_BA_takers)
View(gender_and_AA_or_BA_takers)

#Male and Female Grades who sometimes take notes
all_grades_Sometimes_notes_and_AA_or_BA_takers <- student_prediction %>%
  filter(ListeningInClass_Frequency == "Sometimes" & (Final_Grade == "AA" | Final_Grade == "BA" | Final_Grade == "BB" | Final_Grade == "CB" | Final_Grade == "CC" | Final_Grade == "DC" | Final_Grade == "DD" | Final_Grade == "Fail")) # Include all grades

gender_sometimes_notes_all_grade <- all_grades_Sometimes_notes_and_AA_or_BA_takers %>%
  group_by(Gender,ListeningInClass_Frequency, Final_Grade) %>%
  summarize(Count = n())

gender_sometimes_notes_all_grade <- gender_sometimes_notes_all_grade %>%
  complete(Final_Grade, fill = list(Count = 0))

# Print the result
print(gender_sometimes_notes_all_grade)
View(gender_sometimes_notes_all_grade)


#Never take notes
all_grades_Never_notes_and_AA_or_BA_takers <- student_prediction %>%
  filter(ListeningInClass_Frequency == "Never" & (Final_Grade %in% c("AA", "BA", "BB", "CB", "CC", "DC", "DD", "Fail")))

gender_never_notes_all_grades <- all_grades_Never_notes_and_AA_or_BA_takers %>%
  group_by(Gender,ListeningInClass_Frequency, Final_Grade) %>%
  summarize(Count = n())

gender_never_notes_all_grades <- gender_never_notes_all_grades %>%
  complete(Final_Grade, fill = list(Count = 0))

# Print the result
print(gender_never_notes_all_grades)
View(gender_never_notes_all_grades)

all = rbind(gender_and_AA_or_BA_takers, gender_sometimes_notes_all_grade, gender_never_notes_all_grades)

#corelation
df = data.frame (Listen = as.numeric(student$ListeningInClass_Frequency),Grade = as.numeric(student$Final_Grade))
cor(df)

#Visualization 3
#Pie for Listening in class
a <- c(x_and_A, x_and_B, x_and_C)
l <- c("Never", "Sometimes", "Always")


# Analysis 3-1  - To determine the distribution of students who never, sometimes, and always listening in class
# Create a 3D pie chart 
pie3D(a, labels = l, main = "FREQUENCY OF LISTENING IN CLASS", col = c("green", "blue", "red"), explode = 0.1)


# Analysis 3-2  - To determine the relationship between gender and the frequency of listening in class
#male
male_data <- all %>% filter(Gender == "male")
ggplot(male_data, aes(x = Final_Grade, y = ListeningInClass_Frequency)) +
  geom_point(aes(size = Count)) +
  labs(title = "Male Students - Final Grade vs. Listening Frequency")

#female 
female_data <- all %>% filter(Gender == "female")
ggplot(female_data, aes(x = Final_Grade, y = ListeningInClass_Frequency)) +
  geom_point(aes(size = Count)) +
  labs(title = "Male Students - Final Grade vs. Listening Frequency")


# Analysis 3-3 - To analyse the relationship between final grade and students who always listen to class
#always listen
ggplot(gender_and_AA_or_BA_takers, aes(x = Final_Grade, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Students by Listening in Class Frequency (ALWAYS) and Gender") +
  xlab("Final Grade") +
  ylab("Count") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink"))

# Analysis 3-4	- To analyse the relationship between final grade and students who sometimes listen to class.
#sometimes Listen
ggplot(gender_sometimes_notes_all_grade, aes(x = Final_Grade, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Students by Listening in Class Frequency (SOMETIMES) and Gender") +
  xlab("Final Grade") +
  ylab("Count") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink"))


# Analysis 3-5	- To analyse the relationship between final grade and students who never listen to class.
ggplot(gender_never_notes_all_grades, aes(x = Final_Grade, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Students by Listening in Class Frequency (NEVER) and Gender") +
  xlab("Final Grade") +
  ylab("Count") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink"))


# Analysis 3-6	- To analyse the relationship between final grade and students who never, sometimes, and always listen to class.
#all combined
ggplot(all, aes(x = Final_Grade, y = Count, fill = ListeningInClass_Frequency )) + geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(group = ListeningInClass_Frequency,),arrow = arrow())+
  labs(title = "Count of Students Final Grades based on gender") + 
  xlab("Final Grade") + 
  ylab("Count") +
  scale_fill_manual(values = c("Never" = "blue", "Sometimes" = "red", "always" = "green")) +
  geom_point(aes(color=ListeningInClass_Frequency)) + facet_wrap(~ListeningInClass_Frequency)


#line charts
# Create a plot using base R plot function
plot(x = all$Count[all$ListeningInClass_Frequency == "Never"], type = "o", xlab = "Final Grade", ylab = "Y Count", col = "red")
ggplot(data = all, aes(x = Final_Grade, y = Count, group = factor(ListeningInClass_Frequency))) +
  geom_line(arrow = arrow(type = "closed", angle = 20)) +
  geom_point(aes(color = factor(ListeningInClass_Frequency))) +
  labs(x = "Final Grade", y = "Count")

#combined 
# Proving Hypothesis (FALSE)
alwaysListeninclass_alwaystakenotes_momabovehighschool_studyhoursHigh <- student_prediction %>%
  filter (ListeningInClass_Frequency == "always" & Final_Grade == "AA" &
            (Mom_EducationLevel == "Uni" | Mom_EducationLevel == "Master" | Mom_EducationLevel == "PHD") & 
            StudyHours == "6-10 hours")
combined <-alwaysListeninclass_alwaystakenotes_momabovehighschool_studyhoursHigh %>%
  group_by(ListeningInClass_Frequency, Frequency_NotesTaking, Mom_EducationLevel, StudyHours, Final_Grade) %>%
  summarize(Count = n())
combined <- combined %>%
  complete(Final_Grade, fill = list(Count = 0))
View(combined)


# Analysis 3-7	- To determine How the frequency of taking notes of a student’s might affect the student’s final grade, among students who do not, sometimes, and always listen to class (Two independent)
#1 bar
Notes_ListenNEVER_Grade <- student_prediction %>%
  filter(
    ListeningInClass_Frequency == "Never" &
      Final_Grade %in% c("AA", "BA", "BB", "CB", "CC", "DC", "DD", "Fail") &
      Frequency_NotesTaking %in% c("always", "Sometimes", "Never")
  )

NLG <- Notes_ListenNEVER_Grade %>%
  group_by(ListeningInClass_Frequency, Frequency_NotesTaking, Final_Grade) %>%
  summarize(Count = n())

ggplot(NLG, aes(x = Final_Grade, y = Count, fill = Frequency_NotesTaking)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Graph of Counts for 'Never' Listening Frequency",
       x = "Final Grade",
       y = "Count",
       fill = "Notes Taking Frequency") +
  theme_minimal()

#2 bar 
Notes_ListenSometimes_Grade <- student_prediction %>%
  filter(
    ListeningInClass_Frequency == "Sometimes" &
      Final_Grade %in% c("AA", "BA", "BB", "CB", "CC", "DC", "DD", "Fail") &
      Frequency_NotesTaking %in% c("always", "Sometimes", "Never")
  )

NLGs <- Notes_ListenSometimes_Grade %>%
  group_by(ListeningInClass_Frequency, Frequency_NotesTaking, Final_Grade) %>%
  summarize(Count = n())

ggplot(NLGs, aes(x = Final_Grade, y = Count, fill = Frequency_NotesTaking)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Graph of Counts for 'Sometimes' Listening Frequency",
       x = "Final Grade",
       y = "Count",
       fill = "Notes Taking Frequency") +
  theme_minimal()


#3 bar
Notes_ListenALWAYS_Grade <- student_prediction %>%
  filter(
    ListeningInClass_Frequency == "always" &
      Final_Grade %in% c("AA", "BA", "BB", "CB", "CC", "DC", "DD", "Fail") &
      Frequency_NotesTaking %in% c("always", "Sometimes", "Never")
  )
NLGA <- Notes_ListenNEVER_Grade %>%
  group_by(ListeningInClass_Frequency, Frequency_NotesTaking, Final_Grade) %>%
  summarize(Count = n())

ggplot(NLGA, aes(x = Final_Grade, y = Count, fill = Frequency_NotesTaking)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Graph of Counts for 'always' Listening Frequency",
       x = "Final Grade",
       y = "Count",
       fill = "Notes Taking Frequency") +
  theme_minimal()




#Extra Feature 3
# theme_minimal () – creates a white background for visualization can be seen in most figures like Figure 3.40 and Figure 3.37
# labs – adds labels to figures for clearer visuals can be seen in most figures like Figure 3.32 and Figure 3.43
# explode – allow pie charts to stand out more without changing original data can be seen in Figure 3.25
# position = “dodge” – used to maintains the vertical alignment of a geometric element while modifying its horizontal placement can be seen in most figures like Figure 3.45 and Figure 3.37
# fill = “……….” = - allows me to visually convey information about different categories in your data, making it easier to interpret and understand the patterns in plots such as frequency of listening in class in most figures 
# Facet_wrap(): To separate the variables into multiple facets allowing for easier visualization used in my graphs in figures like 3.37
# Angle = 20 - used to adjust the angle for the line graph in figure 3.39
# Arrow- arrow() -  used to add arrow for line graph in figure 3.39





#Objective 4: To investigate the relationship between the weekly study hours and the grade
#Analysis 4-1: To determine the distribution of final grade for each weekly study hours
#STACKED BAR CHART
data <- student_prediction[, c("StudyHours", "Final_Grade")]


# Count the occurrences of each StudyHours and Final_Grade combination
study_grade_counts <- table(data$StudyHours, data$Final_Grade)

# Create a dataframe for plotting
data <- as.data.frame.table(study_grade_counts)

# Rename the columns for better clarity
colnames(data) <- c("StudyHours", "Final_Grade", "Count")

# Make sure the StudyHours and Final_Grade columns are factors for correct ordering
data$StudyHours <- factor(data$StudyHours, levels=c("None", "<5hours", "6-10hours", "11-20hours", ">20hours"))
data$Final_Grade <- factor(data$Final_Grade, levels=c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"))

# Plot the grouped bar chart
ggplot(data, aes(fill=Final_Grade, y=Count, x=StudyHours)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Study Hours vs Final Grade",
       x = "Study Hours",
       y = "Count") +
  theme_minimal()

#DONUT CHART
data <- student_prediction[, c("StudyHours", "Final_Grade")]

# Count the occurrences of each StudyHours level
study_hours_counts <- table(data$StudyHours)

# Create a dataframe for plotting
data <- data.frame(
  StudyHours = names(study_hours_counts),
  count = as.numeric(study_hours_counts)
)

# Calculate fraction, ymax, ymin, labelPosition, and label
data$fraction <- data$count / sum(data$count)
data$percentage <- scales::percent(data$fraction)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n = -1))
data$labelPosition <- cumsum(data$fraction) - 0.5 * data$fraction
data$label <- paste("StudyHours: ", data$StudyHours, "\nCount: ", data$count, "\nPercentage: ", data$percentage)

# Make the plot with title
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = StudyHours)) +
  geom_rect() +
  geom_label(aes(x = 3.5, y = labelPosition, label = label), size = 3) +
  scale_fill_brewer(palette = "Set3") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Study Hours Donut Chart")



#Analysis 4-2: Does gender along with the weekly study hours affect the final grade
# Filter data for male students
male_data <- student_prediction[student_prediction$Gender == "male", ]

# Create a grouped bar chart MALE
ggplot(male_data, aes(x = StudyHours, fill = Final_Grade)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Study Hours vs Final Grade for Male Students",
       x = "Study Hours",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()

# Create a grouped bar chart FEMALE
# Filter data for male students FEMALE
female_data <- student_prediction[student_prediction$Gender == "female", ]

ggplot(female_data, aes(x = StudyHours, fill = Final_Grade)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Study Hours vs Final Grade for Female Students",
       x = "Study Hours",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()



#Analysis 4-3: Does student attendance along with the weekly study hours affect the final grade

# Filter data for Class_Attendance ALWAYS 
always_data <- student_prediction[student_prediction$Class_Attendance == "Always", ]

# Create a grouped bar chart ALWAYS
ggplot(always_data, aes(x = StudyHours, fill = Final_Grade)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Study Hours vs Final Grade for 'Always' Class attendance Students",
       x = "Study Hours",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()

# Filter data for Class_Attendance "Sometimes"
sometimes_data <- student_prediction[student_prediction$Class_Attendance == "Sometimes", ]

# Create a grouped bar chart SOMETIMES
ggplot(always_data, aes(x = StudyHours, fill = Final_Grade)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Study Hours vs Final Grade for 'Sometimes' Class attendance Students",
       x = "Study Hours",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()

# Filter data for Class_Attendance "Never"  
never_data <- student_prediction[student_prediction$Class_Attendance == "Never", ]

# Create a grouped bar chart "Never"
ggplot(never_data, aes(x = StudyHours, fill = Final_Grade)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Study Hours vs Final Grade for 'Never' Class attendance Students",
       x = "Study Hours",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal()


#Extra Feature 4
# scales::percent(data$fraction): It converts the values in the data frame "data"'s "fraction" column to percentages.
#                                 To produce a percentage count label in the donut chart. (Holtz, Donut chart with GGPLOT2)
# cumsum(data$fraction): It calculates the total of the values in the "fraction" column.
#                       To determine the cumulative proportion of counts for each variable (study hours/final grades)
# paste(): It is used to concatenate and show the joined data and used
# geom_rect(): Rectangular shapes can be added to a ggplot2 plot using this method. 
#             To produce have a rounded rectangle outline on the labels
# scale_fill_brewer(): This function uses colour palettes from the RColorBrewer package to set the colour scale for plotted filled regions 
#                       To provide aesthetics.
# coord_polar(theta = “y”): To convert Cartesian coordinates along the y-axis into polar coordinates in the donut chart. 
#                           To produce a circular representation of the data.
# xlim(): This sets the limit of a ggplot2 plot's x-axis.
#         To limit the x-axis.      
# theme_void(): This function eliminates the plot's backdrop, titles, and axis labels in order 
#               To produce a blank theme.
# theme(): This function allows you to alter the plot's look by changing different elements like the legend, title, and axis text.
#           To provide aesthetics.
# ggtitle(): It is to add a title to all the charts created. 
#             To provide a title.
# labs(): This function is used to set the title, y-axis label, and x-axis label in a ggplot2 plot.
#         To provide a title.
# theme_minimal(): This programme sets up the ggplot2 plot's theme to be minimalistic. (Holtz, Grouped and stacked barplot)
#                 To provide minimal aesthetics.