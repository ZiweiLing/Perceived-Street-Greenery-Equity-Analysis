nyc_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(nyc_greenery)
# chr to int
nyc_greenery$Median_age <- as.numeric(as.character(nyc_greenery$Median_age))
nyc_greenery$Median_household_income <- as.numeric(as.character(nyc_greenery$Median_household_income))
nyc_greenery$Per_capita_income <- as.numeric(as.character(nyc_greenery$Per_capita_income))
nyc_greenery$Median_Owner_occupied_units_value <- as.numeric(as.character(nyc_greenery$Median_Owner_occupied_units_value))
nyc_greenery$Average_household_size <- as.numeric(as.character(nyc_greenery$Average_household_size))
nyc_greenery$Employed_Civilian_labor_force_percentage <- as.numeric(as.character(nyc_greenery$Employed_Civilian_labor_force_percentage))
nyc_greenery$Unemployed_Civilian_labor_force_percentage <- as.numeric(as.character(nyc_greenery$Unemployed_Civilian_labor_force_percentage))
nyc_greenery$White_percentage <- as.numeric(as.character(nyc_greenery$White_percentage))
nyc_greenery$Black_or_African_American_percentage <- as.numeric(as.character(nyc_greenery$Black_or_African_American_percentage))
nyc_greenery$American_Indian_and_Alaska_Native_percentage <- as.numeric(as.character(nyc_greenery$American_Indian_and_Alaska_Native_percentage))
nyc_greenery$Asian_percentage <- as.numeric(as.character(nyc_greenery$Asian_percentage))
nyc_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage <- as.numeric(as.character(nyc_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage))
str(nyc_greenery)
dim(nyc_greenery)

#remove nas
nyc_greenery <- na.omit(nyc_greenery)
dim(nyc_greenery)

#Descriptive statistics
install.packages('summarytools')
library(summarytools)
view(dfSummary(nyc_greenery))

#normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
nyc_greenery$GVI_nor<-normalize(nyc_greenery$GVI)
nyc_greenery$Median_age_nor<-normalize(nyc_greenery$Median_age)
nyc_greenery$Median_household_income_nor<-normalize(nyc_greenery$Median_household_income)
nyc_greenery$Per_capita_income_nor<-normalize(nyc_greenery$Per_capita_income)
nyc_greenery$Average_household_size_nor<-normalize(nyc_greenery$Average_household_size)
nyc_greenery$Median_Owner_occupied_units_value_nor<-normalize(nyc_greenery$Median_Owner_occupied_units_value)
nyc_greenery$Median_built_year_nor<-normalize(nyc_greenery$Median_built_year)
nyc_greenery$Employed_Civilian_labor_force_percentage_nor<-normalize(nyc_greenery$Employed_Civilian_labor_force_percentage)
nyc_greenery$Unemployed_Civilian_labor_force_percentage_nor<-normalize(nyc_greenery$Unemployed_Civilian_labor_force_percentage)
nyc_greenery$White_percentage_nor<-normalize(nyc_greenery$White_percentage)
nyc_greenery$Black_or_African_American_percentage_nor<-normalize(nyc_greenery$Black_or_African_American_percentage)
nyc_greenery$American_Indian_and_Alaska_Native_percentage_nor<-normalize(nyc_greenery$American_Indian_and_Alaska_Native_percentage)
nyc_greenery$Asian_percentage_nor<-normalize(nyc_greenery$Asian_percentage)
nyc_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage_nor<-normalize(nyc_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
nyc_greenery$Population_density_nor<-normalize(nyc_greenery$Population_density)
nyc_greenery$Household_density_nor<-normalize(nyc_greenery$Household_density)
write.csv(nyc_greenery,"/Users/Aria/Downloads/nyc_greenery_normalization.csv", row.names = FALSE)


# #detect outliers
# outliers <- function(x) {
#   Q1 <- quantile(x, probs=.25)
#   Q3 <- quantile(x, probs=.75)
#   iqr = Q3-Q1
#   upper_limit = Q3 + (iqr*1.5)
#   lower_limit = Q1 - (iqr*1.5)
#   x > upper_limit | x < lower_limit
# }
# remove_outliers <- function(df, cols = names(df)) {
#   for (col in cols) {
#     df <- df[!outliers(df[[col]]),]
#   }
#   df
# }
# nyc_greenery_clean = remove_outliers(nyc_greenery, c('GVI', 'Median_age', 'Employed_Civilian_labor_force_percentage', 'Households_density'))
# dim(nyc_greenery_clean)
# boxplot(nyc_greenery_clean)



nyc_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(nyc_greenery)
dim(nyc_greenery)

install.packages("pwr")
library(pwr)

cor(nyc_greenery$GVI, nyc_greenery$Median_age)
cor(nyc_greenery$GVI, nyc_greenery$Median_household_income)
cor(nyc_greenery$GVI, nyc_greenery$Per_capita_income)
cor(nyc_greenery$GVI, nyc_greenery$Average_household_size)
cor(nyc_greenery$GVI, nyc_greenery$Median_Owner_occupied_units_value)
cor(nyc_greenery$GVI, nyc_greenery$Median_built_year)
cor(nyc_greenery$GVI, nyc_greenery$Employed_Civilian_labor_force_percentage)
cor(nyc_greenery$GVI, nyc_greenery$Unemployed_Civilian_labor_force_percentage)
cor(nyc_greenery$GVI, nyc_greenery$White_percentage)
cor(nyc_greenery$GVI, nyc_greenery$Black_or_African_American_percentage)
cor(nyc_greenery$GVI, nyc_greenery$American_Indian_and_Alaska_Native_percentage)
cor(nyc_greenery$GVI, nyc_greenery$Asian_percentage)
cor(nyc_greenery$GVI, nyc_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
cor(nyc_greenery$GVI, nyc_greenery$Population_density)
cor(nyc_greenery$GVI, nyc_greenery$Household_density)
pwr.r.test(r = 0.1, sig.level = 0.05, power = 0.8)

#build models
mod_nyc_1 <- lm(GVI ~ ., data=nyc_greenery)
summary(mod_nyc_1)
#check multicollinearity
library(car)
vif(mod_nyc_1)
#remove columns
nyc_greenery_update = subset(nyc_greenery, select = -c(Population_density,Household_density,Per_capita_income)) 
#Revise models, check multicollinearity
mod_nyc_2 <- lm(GVI ~ ., data=nyc_greenery_update)                             
vif(mod_nyc_2)
summary(mod_nyc_2)

nyc_greenery_update = subset(nyc_greenery, select = -c(Population_density,Household_density,Per_capita_income, White_percentage)) 
mod_nyc_3 <- lm(GVI ~ ., data=nyc_greenery_update) 
vif(mod_nyc_3)
summary(mod_nyc_3)

nyc_greenery_update = subset(nyc_greenery, select = -c(Population_density,Household_density,Per_capita_income, Black_or_African_American_percentage)) 
mod_nyc_4 <- lm(GVI ~ ., data=nyc_greenery_update) 
vif(mod_nyc_4)
summary(mod_nyc_4)

# check normality of errors
mean(mod_nyc_3$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_nyc_3)

# check normality of errors
mean(mod_nyc_4$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_nyc_4)

#power test
#IVs = 5, df = 1484, a = 0.05, r2 = 0.13
#f2 = r2/ (1-r2) = 0.13 / 0.87 = 0.1494
pwr.f2.test(u = 11, v = 1810, f2 = 0.1494, sig.level = 0.05, power = NULL)

