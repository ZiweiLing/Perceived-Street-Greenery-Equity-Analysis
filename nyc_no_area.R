nyc_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(nyc_greenery)
# chr to int
nyc_greenery$Median_age <- as.numeric(as.character(nyc_greenery$Median_age))
nyc_greenery$Median_household_income <- as.numeric(as.character(nyc_greenery$Median_household_income))
nyc_greenery$Per_capita_income <- as.numeric(as.character(nyc_greenery$Per_capita_income))
nyc_greenery$Average_household_size <- as.numeric(as.character(nyc_greenery$Average_household_size))
nyc_greenery$Median_Owner_occupied_units_value <- as.numeric(as.character(nyc_greenery$Median_Owner_occupied_units_value))
str(nyc_greenery)
dim(nyc_greenery)

#remove nas
nyc_greenery <- na.omit(nyc_greenery)
dim(nyc_greenery)

#detect outliers
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x > upper_limit | x < lower_limit
}
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
nyc_greenery_clean = remove_outliers(nyc_greenery, c('GVI', 'Area', 'Median_age', 'White_population', 'Black_or_African_American_population', 'Asian_population', 'Hispanic_or_Latino_population', 'Employed_Population', 'Unemployed_Population', 'Total_households', 'Median_household_income', 'Per_capita_income', 'Average_household_size', 'Median_Owner_occupied_units_value', 'Median_built_year'))
boxplot(nyc_greenery_clean)
dim(nyc_greenery_clean)

write.csv(greenery,"/Users/Aria/Downloads/nyc_greenery_clean.csv", row.names = FALSE)

install.packages('summarytools')
library(summarytools)
view(dfSummary(greenery))

#build models
mod_nyc_1 <- lm(GVI ~ ., data=nyc_greenery_clean)
summary(mod_nyc_1)
#check multicollinearity
library(car)
vif(mod_nyc_1)
#remove columns
nyc_greenery_update = subset(nyc_greenery_clean, select = -c(Employed_Population,Total_households)) 

mod_nyc_2 <- lm(GVI ~ ., data=nyc_greenery_update)                             
vif(mod_nyc_2)
nyc_greenery_update = subset(nyc_greenery_clean, select = -c(Employed_Population,Total_households,Per_capita_income)) 
mod_nyc_3 <- lm(GVI ~ ., data=nyc_greenery_update) 
vif(mod_nyc_3)
summary(mod_nyc_3)
# check normality of errors
mean(model_2$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(model_2)



#IVs = 5, df = 1484, a = 0.05, r2 = 0.1128
#f2 = r2/ (1-r2) = 0.1128 / 0.8872 = 0.127
install.packages("pwr")
library(pwr)
pwr.f2.test(u = 5, v = 1484, f2 = 0.127, sig.level = 0.05, power = NULL)
cor(greenery$GVI, greenery$Median_household_income_dollars)
cor(greenery$GVI, greenery$Employed_population_aged_16_years_and_over)
cor(greenery$GVI, greenery$Unemployed_population_aged_16_years_and_over)
cor(greenery$GVI, greenery$Average_household_size_of_owner.occupied_unit)
cor(greenery$GVI, greenery$Median_housing_units_built_year)
cor(greenery$GVI, greenery$Median_population_age)
pwr.r.test(r = 0.14, sig.level = 0.05, power = 0.8)


scaled.nyc_greenery <- scale(nyc_greenery_update)
# check that we get mean of 0 and sd of 1
colMeans(scaled.nyc_greenery)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.nyc_greenery, 2, sd)
head(scaled.nyc_greenery)
#array to df
df = as.data.frame(scaled.nyc_greenery)
head(df)
mod_nyc_4 <- lm(GVI ~ ., data=df) 
vif(mod_nyc_4)
summary(mod_nyc_4)
# check normality of errors
mean(mod_nyc_4$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_nyc_4)