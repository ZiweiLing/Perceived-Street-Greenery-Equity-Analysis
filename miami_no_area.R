miami_greenery<-read.csv(file.choose(),head=TRUE)
#check df column types
str(miami_greenery)
# chr to int
miami_greenery$Median_age <- as.numeric(as.character(miami_greenery$Median_age))
miami_greenery$Median_household_income <- as.numeric(as.character(miami_greenery$Median_household_income))
miami_greenery$Per_capita_income <- as.numeric(as.character(miami_greenery$Per_capita_income))
miami_greenery$Average_household_size <- as.numeric(as.character(miami_greenery$Average_household_size))
miami_greenery$Median_Owner_occupied_units_value <- as.numeric(as.character(miami_greenery$Median_Owner_occupied_units_value))
str(miami_greenery)
dim(miami_greenery)

#remove nas
miami_greenery <- na.omit(miami_greenery)
dim(miami_greenery)

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
miami_greenery_clean = remove_outliers(miami_greenery, c('GVI', 'Area', 'Median_age', 'White_population', 'Black_or_African_American_population', 'Asian_population', 'Hispanic_or_Latino_population', 'Employed_Population', 'Unemployed_Population', 'Total_households', 'Median_household_income', 'Per_capita_income', 'Average_household_size', 'Median_Owner_occupied_units_value', 'Median_built_year'))
boxplot(miami_greenery_clean)
dim(miami_greenery_clean)
ylim=c(0,1000)

write.csv(greenery,"/Users/Aria/Downloads/nyc_greenery_clean.csv", row.names = FALSE)

install.packages('summarytools')
library(summarytools)
view(dfSummary(greenery))

#build models
mod_miami_1 <- lm(GVI ~ ., data=miami_greenery_clean)
summary(mod_miami_1)
#check multicollinearity
library(car)
vif(mod_miami_1)
#remove columns
miami_greenery_update = subset(miami_greenery_clean, select = -c(Employed_Population,Total_households)) 

mod_miami_2 <- lm(GVI ~ ., data=miami_greenery_update)                             
vif(mod_miami_2)
miami_greenery_update = subset(miami_greenery_clean, select = -c(Employed_Population,Total_households,Per_capita_income)) 
mod_miami_3 <- lm(GVI ~ ., data=miami_greenery_update) 
vif(mod_miami_3)
summary(mod_miami_3)
miami_greenery_update = subset(miami_greenery_clean, select = -c(Employed_Population,Total_households,Per_capita_income, Hispanic_or_Latino_population)) 
mod_miami_4 <- lm(GVI ~ ., data=miami_greenery_update) 
vif(mod_miami_4)
summary(mod_miami_4)
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


scaled.miami_greenery <- scale(miami_greenery_update)
# check that we get mean of 0 and sd of 1
colMeans(scaled.miami_greenery)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.miami_greenery, 2, sd)
head(scaled.miami_greenery)
#array to df
df = as.data.frame(scaled.miami_greenery)
head(df)
mod_miami_5 <- lm(GVI ~ ., data=df) 
vif(mod_miami_5)
summary(mod_miami_5)
# check normality of errors
mean(mod_nyc_5$residuals)
#check homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
# linear model
plot(mod_nyc_5)