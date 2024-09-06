la_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(la_greenery)
# chr to int
la_greenery$Median_age <- as.numeric(as.character(la_greenery$Median_age))
la_greenery$Median_household_income <- as.numeric(as.character(la_greenery$Median_household_income))
la_greenery$Per_capita_income <- as.numeric(as.character(la_greenery$Per_capita_income))
la_greenery$Average_household_size <- as.numeric(as.character(la_greenery$Average_household_size))
la_greenery$Median_Owner_occupied_units_value <- as.numeric(as.character(la_greenery$Median_Owner_occupied_units_value))
la_greenery$Employed_Civilian_labor_force_percentage <- as.numeric(as.character(la_greenery$Employed_Civilian_labor_force_percentage))
la_greenery$Unemployed_Civilian_labor_force_percentage <- as.numeric(as.character(la_greenery$Unemployed_Civilian_labor_force_percentage))
la_greenery$White_percentage <- as.numeric(as.character(la_greenery$White_percentage))
la_greenery$Black_or_African_American_percentage <- as.numeric(as.character(la_greenery$Black_or_African_American_percentage))
la_greenery$American_Indian_and_Alaska_Native_percentage <- as.numeric(as.character(la_greenery$American_Indian_and_Alaska_Native_percentage))
la_greenery$Asian_percentage <- as.numeric(as.character(la_greenery$Asian_percentage))
la_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage <- as.numeric(as.character(la_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage))
str(la_greenery)
dim(la_greenery)

#remove nas
la_greenery <- na.omit(la_greenery)
dim(la_greenery)

#Descriptive statistics
install.packages('summarytools')
library(summarytools)
view(dfSummary(la_greenery))

#normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
la_greenery$GVI_nor<-normalize(la_greenery$GVI)
la_greenery$Median_age_nor<-normalize(la_greenery$Median_age)
la_greenery$Median_household_income_nor<-normalize(la_greenery$Median_household_income)
la_greenery$Per_capita_income_nor<-normalize(la_greenery$Per_capita_income)
la_greenery$Average_household_size_nor<-normalize(la_greenery$Average_household_size)
la_greenery$Median_Owner_occupied_units_value_nor<-normalize(la_greenery$Median_Owner_occupied_units_value)
la_greenery$Median_built_year_nor<-normalize(la_greenery$Median_built_year)
la_greenery$Employed_Civilian_labor_force_percentage_nor<-normalize(la_greenery$Employed_Civilian_labor_force_percentage)
la_greenery$Unemployed_Civilian_labor_force_percentage_nor<-normalize(la_greenery$Unemployed_Civilian_labor_force_percentage)
la_greenery$White_percentage_nor<-normalize(la_greenery$White_percentage)
la_greenery$Black_or_African_American_percentage_nor<-normalize(la_greenery$Black_or_African_American_percentage)
la_greenery$American_Indian_and_Alaska_Native_percentage_nor<-normalize(la_greenery$American_Indian_and_Alaska_Native_percentage)
la_greenery$Asian_percentage_nor<-normalize(la_greenery$Asian_percentage)
la_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage_nor<-normalize(la_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
la_greenery$Population_density_nor<-normalize(la_greenery$Population_density)
la_greenery$Household_density_nor<-normalize(la_greenery$Households_density)

write.csv(la_greenery,"/Users/Aria/Downloads/la_greenery_normalization.csv", row.names = FALSE)

la_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(la_greenery)
dim(la_greenery)

install.packages("pwr")
library(pwr)

cor(la_greenery$GVI, la_greenery$Median_age)
cor(la_greenery$GVI, la_greenery$Median_household_income)
cor(la_greenery$GVI, la_greenery$Per_capita_income)
cor(la_greenery$GVI, la_greenery$Average_household_size)
cor(la_greenery$GVI, la_greenery$Median_Owner_occupied_units_value)
cor(la_greenery$GVI, la_greenery$Median_built_year)
cor(la_greenery$GVI, la_greenery$Employed_Civilian_labor_force_percentage)
cor(la_greenery$GVI, la_greenery$Unemployed_Civilian_labor_force_percentage)
cor(la_greenery$GVI, la_greenery$White_percentage)
cor(la_greenery$GVI, la_greenery$Black_or_African_American_percentage)
cor(la_greenery$GVI, la_greenery$American_Indian_and_Alaska_Native_percentage)
cor(la_greenery$GVI, la_greenery$Asian_percentage)
cor(la_greenery$GVI, la_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
cor(la_greenery$GVI, la_greenery$Population_density)
cor(la_greenery$GVI, la_greenery$Household_density)
pwr.r.test(r = 0.238, sig.level = 0.05, power = 0.8)

#build models
mod_la_1 <- lm(GVI ~ ., data=la_greenery)
summary(mod_la_1)
#check multicollinearity
library(car)
vif(mod_la_1)
#remove columns
la_greenery_update = subset(la_greenery, select = -c(Population_density,Household_density,Per_capita_income, White_percentage)) 
mod_la_2 <- lm(GVI ~ ., data=la_greenery_update) 
vif(mod_la_2)
summary(mod_la_2)

la_greenery_update = subset(la_greenery, select = -c(Population_density,Household_density,Per_capita_income, Black_or_African_American_percentage)) 
mod_la_3 <- lm(GVI ~ ., data=la_greenery_update) 
vif(mod_la_3)
summary(mod_la_3)

# check normality of errors
mean(mod_la_2$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_la_2)

# check normality of errors
mean(mod_la_3$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_la_3)

#power test
#IVs = 5, df = 1484, a = 0.05, r2 = 0.13
#f2 = r2/ (1-r2) = 0.403 / 0.597 = 0.675
pwr.f2.test(u = 11, v = 967, f2 = 0.675, sig.level = 0.05, power = NULL)

