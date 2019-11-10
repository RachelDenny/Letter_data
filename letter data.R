install.packages("e1071")
library("e1071")
#This library contains the SVM function we want
install.packages("readr")
library ("readr")
urlfile="https://raw.githubusercontent.com/RachelDenny/Machine-Learning-with-R-datasets/master/letterdata.csv"
letter_data <-read_csv(url(urlfile))
#Call your data by putting it into a CSV 
install.packages("ISLR")
library("ISLR")
#Make sure your random test/train data is reproducible (important for science people)
set.seed(125)
#set.seed as long as the number is the same makes the random test data the same for eveyone
#select differnt numbers and see how your outcome changes!!!!
testclass80_l <-sample(2, nrow(letter_data), replace=T, prob=c(0.80,0.20))
train80class_l <-sample(1:nrow(letter_data), 0.80 * nrow(letter_data))
test80class_l <-setdiff(1:nrow(letter_data), train80class_l)
xclasstrain80_l <-letter_data[train80class_l, -15]
xclasstest80_l <-letter_data[test80class_l, -15]
#I keep everything at 80 for the training data but it is really 80/20
### This makes it easier to remeber and type later for me, do what you want here
#train data = 80 percent
#test data = 20 percent
m_class80_l = svm(formula = letter ~ ., 
                  data = xclasstrain80_l, 
                  type = 'C-classification', 
                  kernel = 'linear') 

#linear svm model with gamma of 2 and cost of 10
m_class80_l
#predict the model
p_class80_l <-predict(m_class80_l, xclasstest80_l, type="response")
head(p_class80_l)
table(p_class80_l, xclasstest80_l$letter)
agreement_80_l <-p_class80_l==xclasstest80_l$letter
table(agreement_80_l)
View(xclasstest80_l)
prop.table(table(agreement_80_l))