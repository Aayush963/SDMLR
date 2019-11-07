dataFrame <- read.csv("Admission_Predict.csv")

#Dataset URL: https://www.kaggle.com/mohansacharya/graduate-admissions

View(dataFrame)

#Data Cleaning

#The function below will return the rows missing values in True/False form. False for wherever there is
#missing value.

complete.cases(dataFrame)

which(complete.cases(dataFrame))

which(!complete.cases(dataFrame)) #This returns 0 as output, therfore this dataset does not have any NA values.

#Removing the colomn Serial No. as this is not required in analyzing the dataset
dataFrame$Serial.No. <- NULL

View(dataFrame)

##################################################################################################
install.packages('sjPlot')
library('sjPlot')



#This shows the correlation between the variables
cor(dataFrame)

#This shows the visual interpretation of correlation
sjp.corr(dataFrame)

summary(dataFrame$GRE.Score)
hist(dataFrame$GRE.Score)

summary(dataFrame$TOEFL.Score)
hist(dataFrame$TOEFL.Score)

summary(dataFrame$CGPA)

library('outliers')

outlier(dataFrame$GRE.Score)
outlier(dataFrame$TOEFL.Score)
outlier(dataFrame$CGPA)


boxplot(dataFrame$GRE.Score,main = 'Outlier GRE')
boxplot(dataFrame$TOEFL.Score,main = 'Outlier Toefl')
boxplot(dataFrame$CGPA, main = 'Outlier CGPA')  


install.packages('rgl')
install.packages('plotly')
library('rgl')
library('plotly')

scatter3d(x = dataFrame$Chance.of.Admit, y = dataFrame$GRE.Score)

scatterplot(x = dataFrame$Chance.of.Admit, y = dataFrame$GRE.Score, main = 'Admit Chance vs GRE Score', regLine = TRUE)


scatterplot(x = dataFrame$Chance.of.Admit, y = dataFrame$CGPA, main = 'Admit Chance vs CGPA',regLine = TRUE)

scatterplot(x = dataFrame$Chance.of.Admit, y = dataFrame$TOEFL.Score, main = 'Admit Chance vs TOEFL',regLine = TRUE)

#Visualizing the data


plot_ly(x = dataFrame$Chance.of.Admit, y = dataFrame$CGPA)
plot_ly(x = dataFrame$Chance.of.Admit, y = dataFrame$GRE.Score)
plot_ly(x = dataFrame$Chance.of.Admit, y = dataFrame$TOEFL.Score)


chart_link = api_create(p, filename="scatter-color")
chart_link


##########################################################################################################

#Modelling the DataFrame

#linear Regression of Admit on all the the variables.
model1 <- lm(Chance.of.Admit ~ ., data = dataFrame)
summary(model1)

#linear Regression of Admit on all the the variables including the Interaction term for Gre Score & Toefl Score
 
model2 <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating +
               SOP + LOR + CGPA + Research + I(GRE.Score*TOEFL.Score), data = dataFrame)
summary(model2)

#linear Regression of Admit on all the the variables including the Interaction term for Gre Score & Cgpa

model3 <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating +
               SOP + LOR + CGPA + Research + I(GRE.Score*CGPA), data = dataFrame)
summary(model3)

#linear Regression of Admit on all the the variables including the Interaction term for SOP & LOR

model4 <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating +
               SOP + LOR + CGPA + Research + I(SOP*LOR), data = dataFrame)
summary(model4)

#linear Regression of Admit on GRE, Toefl & CGPA, trying to create a Parsimonious Model.

model5 <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score +  CGPA, data = dataFrame)
summary(model5)


AIC(model1)
BIC(model1)


AIC(model2)
BIC(model2)


AIC(model3)
BIC(model3)


AIC(model4)
BIC(model4)


AIC(model5)
BIC(model5)
