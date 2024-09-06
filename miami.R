miami_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(miami_greenery)
# chr to int
miami_greenery$Median_age <- as.numeric(as.character(miami_greenery$Median_age))
miami_greenery$Median_household_income <- as.numeric(as.character(miami_greenery$Median_household_income))
miami_greenery$Per_capita_income <- as.numeric(as.character(miami_greenery$Per_capita_income))
miami_greenery$Average_household_size <- as.numeric(as.character(miami_greenery$Average_household_size))
miami_greenery$Median_Owner_occupied_units_value <- as.numeric(as.character(miami_greenery$Median_Owner_occupied_units_value))
miami_greenery$Employed_Civilian_labor_force_percentage <- as.numeric(as.character(miami_greenery$Employed_Civilian_labor_force_percentage))
miami_greenery$Unemployed_Civilian_labor_force_percentage <- as.numeric(as.character(miami_greenery$Unemployed_Civilian_labor_force_percentage))
miami_greenery$White_percentage <- as.numeric(as.character(miami_greenery$White_percentage))
miami_greenery$Black_or_African_American_percentage <- as.numeric(as.character(miami_greenery$Black_or_African_American_percentage))
miami_greenery$American_Indian_and_Alaska_Native_percentage <- as.numeric(as.character(miami_greenery$American_Indian_and_Alaska_Native_percentage))
miami_greenery$Asian_percentage <- as.numeric(as.character(miami_greenery$Asian_percentage))
miami_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage <- as.numeric(as.character(miami_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage))
str(miami_greenery)
dim(miami_greenery)

#remove nas
miami_greenery <- na.omit(miami_greenery)
dim(miami_greenery)

#Descriptive statistics
install.packages('summarytools')
library(summarytools)
view(dfSummary(miami_greenery))

#normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
miami_greenery$GVI_nor<-normalize(miami_greenery$GVI)
miami_greenery$Median_age_nor<-normalize(miami_greenery$Median_age)
miami_greenery$Median_household_income_nor<-normalize(miami_greenery$Median_household_income)
miami_greenery$Per_capita_income_nor<-normalize(miami_greenery$Per_capita_income)
miami_greenery$Average_household_size_nor<-normalize(miami_greenery$Average_household_size)
miami_greenery$Median_Owner_occupied_units_value_nor<-normalize(miami_greenery$Median_Owner_occupied_units_value)
miami_greenery$Median_built_year_nor<-normalize(miami_greenery$Median_built_year)
miami_greenery$Employed_Civilian_labor_force_percentage_nor<-normalize(miami_greenery$Employed_Civilian_labor_force_percentage)
miami_greenery$Unemployed_Civilian_labor_force_percentage_nor<-normalize(miami_greenery$Unemployed_Civilian_labor_force_percentage)
miami_greenery$White_percentage_nor<-normalize(miami_greenery$White_percentage)
miami_greenery$Black_or_African_American_percentage_nor<-normalize(miami_greenery$Black_or_African_American_percentage)
miami_greenery$American_Indian_and_Alaska_Native_percentage_nor<-normalize(miami_greenery$American_Indian_and_Alaska_Native_percentage)
miami_greenery$Asian_percentage_nor<-normalize(miami_greenery$Asian_percentage)
miami_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage_nor<-normalize(miami_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
miami_greenery$Population_density_nor<-normalize(miami_greenery$Population_density)
miami_greenery$Household_density_nor<-normalize(miami_greenery$Households_density)

write.csv(miami_greenery,"/Users/Aria/Downloads/miami_greenery_clean.csv", row.names = FALSE)

miami_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(miami_greenery)
dim(miami_greenery)


cor(miami_greenery$GVI, miami_greenery$Median_age)
cor(miami_greenery$GVI, miami_greenery$Median_household_income)
cor(miami_greenery$GVI, miami_greenery$Per_capita_income)
cor(miami_greenery$GVI, miami_greenery$Average_household_size)
cor(miami_greenery$GVI, miami_greenery$Median_Owner_occupied_units_value)
cor(miami_greenery$GVI, miami_greenery$Median_built_year)
cor(miami_greenery$GVI, miami_greenery$Employed_Civilian_labor_force_percentage)
cor(miami_greenery$GVI, miami_greenery$Unemployed_Civilian_labor_force_percentage)
cor(miami_greenery$GVI, miami_greenery$White_percentage)
cor(miami_greenery$GVI, miami_greenery$Black_or_African_American_percentage)
cor(miami_greenery$GVI, miami_greenery$American_Indian_and_Alaska_Native_percentage)
cor(miami_greenery$GVI, miami_greenery$Asian_percentage)
cor(miami_greenery$GVI, miami_greenery$Native_Hawaiian_and_Other_Pacific_Islander_percentage)
cor(miami_greenery$GVI, miami_greenery$Population_density)
cor(miami_greenery$GVI, miami_greenery$Household_density)
pwr.r.test(r = 0.18, sig.level = 0.05, power = 0.8)

#build models
mod_miami_1 <- lm(GVI ~ ., data=miami_greenery)
summary(mod_miami_1)
#check multicollinearity
library(car)
vif(mod_miami_1)
#remove columns
miami_greenery_update = subset(miami_greenery, select = -c(Population_density,Household_density,Per_capita_income, White_percentage, Employed_Civilian_labor_force_percentage)) 
mod_miami_2 <- lm(GVI ~ ., data=miami_greenery_update) 
vif(mod_miami_2)
summary(mod_miami_2)

#define weights to use
wt <- 1 / lm(abs(mod_miami_2$residuals) ~ mod_miami_2$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model_1 <- lm(GVI ~ ., data = miami_greenery_update, weights=wt)
vif(wls_model_1)
summary(wls_model_1)
# check normality of errors
mean(wls_model_1$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(wls_model_1)

miami_greenery_update = subset(miami_greenery, select = -c(Population_density,Household_density,Per_capita_income, Black_or_African_American_percentage, Employed_Civilian_labor_force_percentage)) 
mod_miami_3 <- lm(GVI ~ ., data=miami_greenery_update) 
vif(mod_miami_3)
summary(mod_miami_3)

#define weights to use
wt <- 1 / lm(abs(mod_miami_3$residuals) ~ mod_miami_3$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model_2 <- lm(GVI ~ ., data = miami_greenery_update, weights=wt)
vif(wls_model_2)
summary(wls_model_2)
# check normality of errors
mean(wls_model_2$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(wls_model_2)



#view summary of model
summary(wls_model)

#power test
#IVs = 5, df = 1484, a = 0.05, r2 = 0.13
#f2 = r2/ (1-r2) = 0.16 / 0.84 = 0.19
install.packages("pwr")
library(pwr)
pwr.f2.test(u = 10, v = 98, f2 = 0.19, sig.level = 0.05, power = NULL)

