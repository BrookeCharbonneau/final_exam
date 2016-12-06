library(tidyverse)
library(apaTables)

raw_data <- read_csv(file="exam_data_f16.csv")

str(raw_data)
# View(raw_data)

raw_data <- read_csv(file="exam_data_f16.csv" ,na=c("","-999","NA"))


#create sub files
categorical_variables <- select(raw_data, gender, age)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)
glimpse(categorical_variables)

agreeableness_items <- select(raw_data, A1, A2, A3, A4, A5)
conscientiousness_items <- select(raw_data, C1, C2, C3, C4, C5)
performance_items <- select(raw_data, JP1, JP2, JP3, JP4, JP5)


#remove bad values and mutate
psych::describe(agreeableness_items)
is_bad_value <- agreeableness_items<1 | agreeableness_items>6
agreeableness_items[is_bad_value] <- NA
View(agreeableness_items)

agreeableness_items <- mutate(agreeableness_items,A1=7-A1)


psych::describe(conscientiousness_items)

conscientiousness_items <- mutate(conscientiousness_items,C4=7-C4)
conscientiousness_items <- mutate(conscientiousness_items,C5=7-C5)


psych::describe(performance_items)

performance_items <- mutate(performance_items,JP1=7-JP1)
performance_items <- mutate(performance_items,JP2=7-JP2)


#create overall scores instead of individual items
agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness_items),check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance_items),check.keys=FALSE)$scores

analytic_data <- cbind(agreeableness, conscientiousness, performance, categorical_variables)

# View(analytic_data)

write_csv(analytic_data,path="final_exam_analytic_data_Charbonneau.csv")


#cor table
analytic_data2 <- select(analytic_data, agreeableness, conscientiousness, performance, age)
#View(analytic_data2)
apa.cor.table(analytic_data2,filename="Table_1.doc",table.number=1)

#get reliabilities
r1 <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)
r1

r2 <- psych::alpha(as.data.frame(conscientiousness_items),check.keys=FALSE)
r2

r3 <- psych::alpha(as.data.frame(performance_items),check.keys=FALSE)
r3


#non-linear relation checking graph
psych::pairs.panels(as.data.frame(analytic_data2))


#regression table 1
#conscientiousness-agreeableness
block1 = lm(performance~conscientiousness,data=analytic_data)

block2 = lm(performance~conscientiousness+agreeableness,data=analytic_data)

apa.reg.table(block1, block2, filename="Table2.doc",table.number=2)


#regression table 2
#men
analytic_data_male <- filter(analytic_data, gender=="Male")
analytic_data_male <- select(analytic_data_male, -gender)
#View(analytic_data_male)

#conscientiousness-agreeableness - men
block3 = lm(performance~conscientiousness,data=analytic_data_male)

block4 = lm(performance~conscientiousness+agreeableness,data=analytic_data_male)

apa.reg.table(block3, block4, filename="Table3.doc",table.number=3)


#regression table 3
#women
analytic_data_female <- filter(analytic_data, gender=="Female")
analytic_data_female <- select(analytic_data_female, -gender)
#View(analytic_data_female)

#conscientiousness-agreeableness - women
block5 = lm(performance~conscientiousness,data=analytic_data_female)

block6 = lm(performance~conscientiousness+agreeableness,data=analytic_data_female)

apa.reg.table(block5, block6, filename="Table4.doc",table.number=4)






