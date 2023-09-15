## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(vtable)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggcorrplot)
library(tidyverse)
library(readxl)
library(quantregForest)
library(olsrr)
library(MASS)


## ----Data Import-------------------------------------------------------------------------------------

kv_house_data = read.csv("/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/data/kc_house_data.csv")
names(kv_house_data)

zipcode_region = read.csv("/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/data/Zipcode.csv", header=T)



## ----add extra columns-------------------------------------------------------------------------------

# add columns: year and month by splitting column date
kv_house_data$year = substr(kv_house_data$date, 1,4)
kv_house_data$month = substr(kv_house_data$date, 5,6)

# add columns: ratio_lotliving, unitPrice
kv_house_data$ratio_lotliving = kv_house_data$sqft_lot/kv_house_data$sqft_living
kv_house_data$unitPrice = kv_house_data$price/kv_house_data$sqft_living

# add column: region by left joining two data sets kv_house_data and zipcode_region to identify region for data based on zipcode
kv_house_data = left_join(kv_house_data, zipcode_region, by="zipcode")
kv_house_data$region = as.factor(kv_house_data$region)

# add column: diff_yr_renovated_and_year = year - yr_renovated if yr_renovated !=0 else NA
kv_house_data$diff_yr_renovated_and_year = as.integer(kv_house_data$year)-kv_house_data$yr_renovated
kv_house_data$diff_yr_renovated_and_year[kv_house_data$yr_renovated==0] = NA

# add column: renovation (3 levels: never renovated, renovated long time ago, renovated recently) by binning data based on diff_yr_renovated_and_year or yr_renovated
kv_house_data$renovation = "renovated long time ago"
kv_house_data$renovation[kv_house_data$diff_yr_renovated_and_year<=20] = "renovated recently"
kv_house_data$renovation[kv_house_data$yr_renovated==0] = "never renovated"
kv_house_data$renovation = as.factor(kv_house_data$renovation)

# add column: basement (tw0 levels: 1 if sqft_basement !=0 else 0) by binning data based on sqft_basement
kv_house_data$basement = 0
kv_house_data$basement[kv_house_data$sqft_basement!=0] = 1
kv_house_data$basement = as.factor(kv_house_data$basement)


## ----data cleasing-----------------------------------------------------------------------------------

# Select a threshold for separating family houses and farm land
# According to the context of the data set, the majority of data here should be family houses rather than farm land => Define the threshold based on this idea
# Since a family house usually takes majority of space/area for the living purpose, then the ratio_lotliving should be relatively small
# After observing histograms for ratio_lotliving data, 10 is chosen as the threshold for identifying family houses (ratio_lotliving<10) vs farm land (ratio_lotliving>=10)

par(mfrow=c(1,2))
hist(kv_house_data$ratio_lotliving, main = "ratio_lotliving", xlab = "ratio_lotliving")
hist(kv_house_data$ratio_lotliving[kv_house_data$ratio_lotliving<100],
     main = "ratio_lotliving (ratio<100)", xlab = "ratio_lotliving")
hist(kv_house_data$ratio_lotliving[kv_house_data$ratio_lotliving<10],
     main = "ratio_lotliving (ratio<10)", xlab = "ratio_lotliving")
nrow(kv_house_data) # total: 21613
sum(kv_house_data$ratio_lotliving>=10) # farm land: 2426
sum(kv_house_data$ratio_lotliving<10) # family houses: 19187

# We just focus on all the family houses, and exclude farm land in this project
# Histograms and boxplots for getting an overview of the distribution of the data
kv_house_data_family = kv_house_data[kv_house_data$ratio_lotliving<10,]
hist(kv_house_data_family$ratio_lotliving, xlab="ratio_lotliving")
boxplot(kv_house_data_family$ratio_lotliving, main = "kv_house_data_family$ratio_lotliving")
hist(kv_house_data_family$sqft_living, xlab="sqft_living")
boxplot(kv_house_data_family$sqft_living, main = "kv_house_data_family$sqft_living")

# By looking at the summary statistics of the data, we can identify some extreme values in the data set by considering the context
summary(kv_house_data_family)

# Remove extreme values - Part 1: no bedrooms or no bathrooms
kv_house_data_family = subset(kv_house_data_family, subset = (kv_house_data_family$bedrooms!=0 & kv_house_data_family$bathrooms!=0))

# Remove extreme values - Part 1: remove data with bedrooms = 33 (maximum)
kv_house_data_family = subset(kv_house_data_family, subset = (kv_house_data_family$bedrooms!=33))
# Double check if we need to further remove some data with extreme values
summary(kv_house_data_family)
hist(subset(kv_house_data_family, subset = (kv_house_data_family$bedrooms>4))$bedrooms)
hist(subset(kv_house_data_family, subset = (kv_house_data_family$bedrooms>6))$bedrooms)
hist(subset(kv_house_data_family, subset = (kv_house_data_family$bathrooms>=3))$bathrooms)
hist(subset(kv_house_data_family, subset = (kv_house_data_family$bathrooms>=6))$bathrooms)

# Histogram and boxplot
hist(kv_house_data_family$bedrooms, xlab="bedrooms", main="Histogram of kv_house_data_family$bedrooms after removing extreme values")
boxplot(kv_house_data_family$bedrooms, ylab="bedrooms", main="Boxplot of kv_house_data_family$bedrooms after removing extreme values")

# Use unitPrice=price/sqft_living as response variable, forcing the response variable to fall into a smaller range: from a scale of 10^6 to a scale of 10^2 which is better for model fitting
par(mfrow=c(1,2))
hist(kv_house_data_family$price, xlab = "price")
boxplot(kv_house_data_family$price, xlab = "price")
hist(kv_house_data_family$unitPrice, xlab = "unit price") 
boxplot(kv_house_data_family$unitPrice, xlab = "unitPrice")

# Get a summary of all the numerical features before dropping any useless feature/column
st(kv_house_data_family[,sapply(kv_house_data_family,is.numeric)])

# Drop columns: id, date, lat, long, zipcode
names(kv_house_data_family)
kv_house_data_family = subset(kv_house_data_family, select = -c(id, date, lat, long, zipcode))
# Get a summary of all the remaining numerical feature values after dropping useless features/columns
st(kv_house_data_family[,sapply(kv_house_data_family,is.numeric)])

# Count the missing values by column wise
# We can see there is no missing data => no need to deal with missing data 
print("Count of missing values by column wise")
count_of_missing_values = sapply(kv_house_data_family, function(x) sum(is.na(x)))
data.frame(count_of_missing_values)

# Convert column "waterfront", "view", "condition", "grade", "year", "month", "basement", "renovation", "region" into factor => They are treated as categorical variables in this project
names(kv_house_data_family)
cat_cols = c("waterfront", "view", "condition", "grade", "year", "month", "basement", "renovation", "region")
kv_house_data_family[cat_cols] <- lapply(kv_house_data_family[cat_cols], as.factor)



## ----data transformation-----------------------------------------------------------------------------

# Goal: Perform some data transformation to make data relatively normally distributed => better data visualization (fewer data will be marked as outliers on the boxplot)
# Note: For skewed data, log can be a good transformation function to be considered.

# unitPrice => no transformation is applied here
par(mfrow=c(1,2))
hist(kv_house_data_family$unitPrice, xlab="unitPrice", col="lightgrey", main="unitPrice")
hist(log(kv_house_data_family$unitPrice), xlab="log(unitPrice)", col="lightgrey", main="log(unitPrice)")
boxplot(log(kv_house_data_family$unitPrice), xlab="log(unitPrice)", main="log(unitPrice)")

# sqft_living => log(sqft_living)
par(mfrow=c(1,2))
hist(kv_house_data_family$sqft_living, xlab="sqft_living", col="lightgrey", main="sqft_living")
hist(log(kv_house_data_family$sqft_living), xlab="log(sqft_living)", col="lightgrey", main="log(kv_house_data_family$sqft_living)")
boxplot(log(kv_house_data_family$sqft_living), xlab="log(sqft_living)", main="log(sqft_living)")
kv_house_data_family$log_sqft_living = log(kv_house_data_family$sqft_living)

# sqft_lot => log(sqft_lot)
par(mfrow=c(1,2))
hist(kv_house_data_family$sqft_lot, xlab="sqft_lot", col="lightgrey", main="sqft_lot")
hist(log(kv_house_data_family$sqft_lot), xlab="log(sqft_lot)", col="lightgrey", main="log(sqft_lot)")
boxplot(log(kv_house_data_family$sqft_lot), xlab="log(sqft_lot)",
        main="log(sqft_lot)")
kv_house_data_family$log_sqft_lot = log(kv_house_data_family$sqft_lot)

# sqft_lot15 => log(sqft_lot15)
par(mfrow=c(1,2))
hist(kv_house_data_family$sqft_lot15, xlab="sqft_lot15", col="lightgrey", main="sqft_lot15")
hist(log(kv_house_data_family$sqft_lot15), xlab="log(sqft_lot15)", 
     col="lightgrey", main="log(sqft_lot15)")
boxplot(log(kv_house_data_family$sqft_lot15), xlab="log(sqft_lot15)", 
        main="log(sqft_lot15)")
kv_house_data_family$log_sqft_lot15 = log(kv_house_data_family$sqft_lot15)



## ----More data visualizations for demonstration purposes---------------------------------------------
# Get numeric columns
varnames = names(kv_house_data_family)
numeric_or_not = sapply(kv_house_data_family, is.numeric)
numeric_col_indices = which(numeric_or_not==TRUE)

# Draw boxplots for numerical variables
boxplot_list = list()
for (i in 1: length(numeric_col_indices)) {
  curr_col_index = numeric_col_indices[i]
  p = ggplot(kv_house_data_family, aes_string(y=varnames[curr_col_index])) + 
    geom_boxplot(color="#454545", fill="orange", alpha=0.5) +
    theme(legend.position="none")
  boxplot_list[[i]] <- p
}
grid.arrange(grobs=boxplot_list, ncol=4)

# Save boxplots generated
g <- arrangeGrob(grobs=boxplot_list, ncol=4)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/boxplot_list.png", g)

# Draw side-by-side boxplots for categorical variables
sidebyside_boxplot_list = list()
for (i in 1:length(cat_cols)) {
  p = ggplot(kv_house_data_family, aes_string(x=cat_cols[i], y="unitPrice",fill=cat_cols[i])) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")
  if (cat_cols[i] %in% c("renovation", "region")) {
    p = p + theme(axis.text.x=element_text(angle = -90, hjust = 0))
  }
  sidebyside_boxplot_list[[i]] <- p
}

print(length(sidebyside_boxplot_list)) # There are 9 side-by-side boxplots in total
grid.arrange(grobs=sidebyside_boxplot_list[c(1, 2, 3)], nrow=2, ncol=2)
grid.arrange(grobs=sidebyside_boxplot_list[c(4, 5, 6)], nrow=2, ncol=2)
grid.arrange(grobs=sidebyside_boxplot_list[c(7, 8, 9)], nrow=2, ncol=2)

# Save side-by-side boxplots generated
g <- arrangeGrob(grobs=sidebyside_boxplot_list[c(1, 2, 3)], nrow=2, ncol=2)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/sidebyside_boxplot_list_part1.png", g)

g <- arrangeGrob(grobs=sidebyside_boxplot_list[c(4, 5, 6)], nrow=2, ncol=2)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/sidebyside_boxplot_list_part2.png", g)

g <- arrangeGrob(grobs=sidebyside_boxplot_list[c(7, 8, 9)], nrow=2, ncol=2)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/sidebyside_boxplot_list_part3.png", g)

# Draw side-by-side boxplots for bedrooms and floors
# The number of bedrooms and floors might affect the house price or unitPrice a lot, so we draw side-by-side 
# boxplots to take a further look at that
sidebyside_boxplot_list_extra = list()
kv_house_data_family$bedrooms = as.factor(kv_house_data_family$bedrooms)
kv_house_data_family$floors = as.factor(kv_house_data_family$floors)

sidebyside_boxplot_list_extra[[1]] = ggplot(kv_house_data_family, aes(x=bedrooms, y=unitPrice,fill=bedrooms)) + geom_boxplot(alpha=0.3) + theme(legend.position="none")
sidebyside_boxplot_list_extra[[2]] = ggplot(kv_house_data_family, aes(x=floors, y=unitPrice,fill=floors)) + geom_boxplot(alpha=0.3) + theme(legend.position="none")

grid.arrange(grobs=sidebyside_boxplot_list_extra, ncol=2)

# Save the two extra side-by-side boxplots generated
g <- arrangeGrob(grobs=sidebyside_boxplot_list_extra, ncol=2)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/sidebyside_boxplot_list_extra.png", g)

# We temporarily converted bedrooms and floors into factors/categorical variables in order to draw side-by-side boxplots for them. Now we convert them back to be numerical variables for further model fitting.
kv_house_data_family$bedrooms = as.numeric(kv_house_data_family$bedrooms)
kv_house_data_family$floors = as.numeric(kv_house_data_family$floors)

# Draw a trend plot for mean price over month
# Check whether (mean) price changes dramatically over month => whether to consider month as a significant feature
mean_price_over_month <- as.data.frame(
  kv_house_data_family %>%
  group_by(month) %>%
  summarise(mean_price = mean(price))
)
mean_price_over_month$month = as.numeric(mean_price_over_month$month)
g <- ggplot(mean_price_over_month, aes(x=month, y=mean_price)) + 
        geom_line() + 
        scale_x_continuous(breaks=seq(1,12,1)) +
        ggtitle("price trend over months")
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/price _trend_over_months.png", g)

# Draw histograms for numerical variables
par(mfrow=c(3,3))
histogram_list = list()
for (i in 1: length(numeric_col_indices)) {
  curr_col_index = numeric_col_indices[i] 
  # p = hist(kv_house_data_family[, curr_col_index], xlab=varnames[curr_col_index], col="lightgrey", main=paste0("Histogram of kv_house_data_family$", varnames[curr_col_index]))
  p<-ggplot(kv_house_data_family, aes_string(x=names(numeric_col_indices)[i])) + geom_histogram(color="black", fill="grey")
  histogram_list[[i]] <- p
}
n_of_histograms = length(histogram_list)
grid.arrange(grobs=histogram_list[1: round(n_of_histograms/2)], ncol=3)
grid.arrange(grobs=histogram_list[(round(n_of_histograms/2)+1):n_of_histograms], ncol=3)

# Save the histograms generated
g <- arrangeGrob(grobs=histogram_list[1: round(n_of_histograms/2)], ncol=3)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/histogram_list_part1.png", g)
g <- arrangeGrob(grobs=histogram_list[(round(n_of_histograms/2)+1):n_of_histograms], ncol=3)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/histogram_list_part2.png", g)

# Generate spearman correlation matrix
# We only focus on numeric variables => Exception: We ignore column diff_yr_renovated_and_year which contains many NAs
kv_house_data_family$view = as.numeric(kv_house_data_family$view)
kv_house_data_family$condition = as.numeric(kv_house_data_family$condition)
kv_house_data_family$grade = as.numeric(kv_house_data_family$grade)
temp = which(sapply(kv_house_data_family, is.numeric)==TRUE)
temp = temp[which(names(temp)!="diff_yr_renovated_and_year")]
correlation_matrix = data.frame(round(cor(kv_house_data_family[ , temp], method="spearman"),1))
print(correlation_matrix) 
ggcorrplot(cor(x=kv_house_data_family[, temp], y=NULL, method="spearman"), hc.order = TRUE,
   outline.col = "white",
   ggtheme = ggplot2::theme_gray,
   colors = c("#6D9EC1", "white", "#E46726")) + ggplot2::theme(axis.text.x=element_text(angle = -90, hjust = 0))

# Save the spearman correlation matrix plot generated
ggsave("/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/spearman_correlation_matrix_plot.png")



## ----More data visualizations for region, also for demonstration purposes----------------------------
# Get all unique regions
unique_regions <- unique(kv_house_data$region)

# Count the number of unique regions - 37 regions in total
num_regions <- length(unique_regions)
num_regions

# Draw and save the boxplot of price by region
g <- ggplot(kv_house_data_family, aes(x=region, y=price,fill=region)) + geom_boxplot(alpha=0.3) + theme(legend.position="none", axis.text.x=element_text(angle = -90, hjust = 0))
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/boxplot_of_price_by_region.png", g)

# Draw and save the boxplot of unitPrice by region
g <- ggplot(kv_house_data_family, aes(x=region, y=unitPrice,fill=region)) + geom_boxplot(alpha=0.3) + theme(legend.position="none", axis.text.x=element_text(angle = -90, hjust = 0))
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/boxplot_of_unitPrice_by_region.png", g)

# Draw and save the histogram of unitPrice by region
g <- ggplot(kv_house_data_family, aes(x = unitPrice, fill = region)) + 
  geom_histogram(position = "dodge", bins = 20) +
  labs(x = "Unit Price", y = "Count") +
  ggtitle("Histogram of Unit Price by Region") +
  theme_bw()
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/histogram_of_unitPrice_by_region.png", g)

# Draw and save the bar chart of the count of houses by region
g <- ggplot(kv_house_data_family, aes(x = region, fill = region)) + 
  geom_bar() +
  labs(x = "Region", y = "Count") +
  ggtitle("Count of Houses by Region") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/barchart_of_count_of_houses_by_region.png", g)



## ----remove data points from minor regions and remove columns that will not be used in the model-----

# Find minor regions and remove data points from those regions 
# We only focus on family houses in major regions: kv_house_data_family_filtered
region_count = table(kv_house_data_family$region)
region_count = sort(region_count, decreasing = F)
minor_region = names(region_count)[1:6]
kv_house_data_family$region = as.character(kv_house_data_family$region)

kv_house_data_family_filtered = subset(kv_house_data_family, !region %in% minor_region)
kv_house_data_family_filtered$region = as.factor(kv_house_data_family_filtered$region)

# Prepare the dataset for full model by removing unnecessary columns
# From the previous sections, we decide that applying log transformation on sqft_living, sqft_lot, sqft_lot15 might be helpful, so we remove those original columns. And since we will use unitPrice as our response variable, we can remove column price.
kv_house_data_family_filtered = subset(kv_house_data_family_filtered, select = -c(diff_yr_renovated_and_year, sqft_living, sqft_lot, sqft_lot15, price, yr_renovated, ratio_lotliving))



## ----spearman matrix for kv_house_data_family_filtered-----------------------------------------------
temp2 = which(sapply(kv_house_data_family_filtered, is.numeric)==TRUE)
correlation_matrix2 = data.frame(round(cor(kv_house_data_family_filtered[ , temp2], method="spearman"),1))
print(correlation_matrix2) 
g <- ggcorrplot(cor(x=kv_house_data_family_filtered[, temp2], y=NULL, method="spearman"), hc.order = TRUE,
   outline.col = "white",
   ggtheme = ggplot2::theme_gray,
   colors = c("#6D9EC1", "white", "#E46726"),
   lab = TRUE)
print(g)
ggsave(file="/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/plots/spearman_matrix_plot_for_filtered_dataset.png", g)


## ----remove columns with high multicollinearity------------------------------------------------------

# Two columns can be thought to have high multicollinearity if their spearman correlation coefficient >=0.8

# From the spearman matrix plot for kv_house_data_family_filtered in the last section, we can observe that there are two pairs: (log_sqft_living, sqft_above) = 0.83 and (log_sqft_lot, log_sqft_lot15) = 0.91

# Thus we may need to delete one of the two variables in each pair, but we would like to see the results from stepwise AIC feature selection first



## ----feature selection - stepwise AIC----------------------------------------------------------------

# Split the data into training (80%) and testing/holdout (20%) datasets
set.seed(447)
n_kv_house_data_family_filtered = nrow(kv_house_data_family_filtered)
train = sample(n_kv_house_data_family_filtered, n_kv_house_data_family_filtered * 0.8)
kv_house_train = kv_house_data_family_filtered[train, ]
kv_house_test = kv_house_data_family_filtered[-train, ]

# Fit the simple linear regression model to training set for performing the stepwise AIC feature selection
model_fSelect = lm(unitPrice ~., data = kv_house_train)
AICstep = stepAIC(model_fSelect, direction="both", trace=1, steps=20)
summary(AICstep) 

# From the summary of the step-wise AIC, we can see that removing floors and sqft_basement can help reduce and minimize AIC.
# Combined with concerns regarding high multicollinearity: (log_sqft_living, sqft_above) = 0.83 and (log_sqft_lot, log_sqft_lot15) = 0.91 in the last section, we finally decide to remove columns: sqft_basement, floors, log_sqft_lot
kv_house_data_family_filtered_after_feature_selection = subset(kv_house_data_family_filtered, select = -c(sqft_basement, floors, log_sqft_lot))
names(kv_house_data_family_filtered_after_feature_selection)



## ----diagonostic plots for checking whether the assumption of homogeneity holds or not---------------

# Fit a multiple linear regression
multiple_lm_model = lm(unitPrice ~., data = kv_house_train)
plot(multiple_lm_model)
multiple_lm_model.res = resid(multiple_lm_model)
summary(multiple_lm_model)

# From the associated residual plots, we can observe that there is no obvious pattern in those plots. The funnel-shaped pattern in some residual plots might be just due to the uneven distribution of the data, that is, there are more data with unitPrice in the middle range.
plot(kv_house_train$log_sqft_living, multiple_lm_model.res,
     xlab="log_sqft_living", ylab="Residuals", 
     main = "")

plot(kv_house_train$bedrooms, multiple_lm_model.res,
     xlab="bedrooms", ylab="Residuals", 
     main = "")

plot(kv_house_train$yr_built, multiple_lm_model.res,
     xlab="yr_built", ylab="Residuals", 
     main = "")

plot(kv_house_train$sqft_above, multiple_lm_model.res,
     xlab="sqft_above", ylab="Residuals", 
     main = "")

plot(kv_house_train$log_sqft_lot, multiple_lm_model.res,
     xlab="log_sqft_lot", ylab="Residuals", 
     main = "")

plot(kv_house_train$sqft_living15, multiple_lm_model.res,
     xlab="sqft_living15", ylab="Residuals", 
     main = "")

plot(kv_house_train$floors, multiple_lm_model.res,
     xlab="floors", ylab="Residuals", 
     main = "")



## ----check by fitting a multiple regression model----------------------------------------------------

# Fit a multiple regression model with selected features/columns and a multiple regression model with all features/columns to the training set
# By looking at and comparing the summary of these two models, we can check whether those features/columns being removed are statistically insignificant to unitPrice or not.
# From these two summary, we can confirm that it is necessary to remove those columns/features for better model fitting.

multiple_lm_model_subset_check_original = lm(unitPrice ~ ., data = kv_house_data_family_filtered)
summary(multiple_lm_model_subset_check_original)

multiple_lm_model_subset_check = lm(unitPrice ~ bedrooms+bathrooms+waterfront+view+condition+grade^2+yr_built+sqft_living15+year+month+region+renovation+basement+log_sqft_living+log_sqft_lot15, data = kv_house_data_family_filtered)
summary(multiple_lm_model_subset_check)



## ----save kv_house_data_family_filtered, kv_house_data_family_filtered_after_feature_selection as .rds----

# Save data frames as .rds files for easier future reference
saveRDS(kv_house_data_family_filtered, "/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/data/kv_house_data_family_filtered.rds")

saveRDS(kv_house_data_family_filtered_after_feature_selection, "/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/data/kv_house_data_family_filtered_after_feature_selection.rds")



## ----notes for the following functions for model fitting and performance evaluation------------------

# The current version of codes are implemented to fit one basic multiple linear regression model with all variables + one multiple linear regression model with a subset of variables selected based on step-wise AIC and high multicollinearity + one quantile random forest

# Model performance is compared through metrics based on average length, interval score, and coverage rate of the 50% and 80% prediction intervals

# You can add more models and performance evaluation metrics by modifying functions: myMethod, myPredict, crossValidate, intervalScore, perfMeas to suit your own needs



## ----myMethod----------------------------------------------------------------------------------------

#' @param train training set that will be used for model fitting
#' @return a list of fitted models: multiple linear regression model(s) + quantile random forest
myMethod = function(train) {
  
  # Fit a basic multiple regression model with all variables
  multiple_lm_model = lm(unitPrice ~., data = train)
  
  # Fit a multiple regression model with a subset of variables (suggested by stepwise AIC feature selection)
  # Note: From the side-by-side boxplot of unitPrice vs grade, we observe that there is a non-linear/quadratic
  # pattern, so we use grade^2 in our model
  multiple_lm_model_subset = lm(unitPrice ~ bedrooms+bathrooms+waterfront+view+condition+grade^2+yr_built+sqft_living15+year+month+region+renovation+basement+log_sqft_living+log_sqft_lot15, data = train)
  
  # Fit a quantile random forest model
  qrf_x = subset(train, select = -c(unitPrice))
  qrf_y = train$unitPrice
  # Use ntrees = 100 instead of default value to save time here, otherwise it needs at least 20 minutes to run
  # Feel free to change the value of ntree
  # Note: From some pre-experiments, the model performance did not vary dramatically when changing ntrees, so 
  # we decide to save the computational cost by applying a smaller ntrees
  quantregForest_model = quantregForest(qrf_x, qrf_y, ntrees = 100)
  
  # Save the three models fitted above
  list(multiple_lm_model=multiple_lm_model, multiple_lm_model_subset=multiple_lm_model_subset, quantregForest_model=quantregForest_model)
  
}



## ----myPredict---------------------------------------------------------------------------------------

#' @param myMethodObj a list of fitted models for a continuous response: multiple linear regression model(s) + one quantile random forest
#' @param holdout holdout set for model testing
#' @return 50% and 80% prediction intervals associated with the fitted models
myPredict <- function(myMethodObj, holdout){
  
  n_model = length(myMethodObj) # one or more multiple linear regression models + one Quantile Random Forest
  predQRF50and80 = predict(myMethodObj[[n_model]], what=c(.1,.25, .5, .75, .9), newdata=holdout)
  
  predMLR50 = data.frame()
  predMLR80 = data.frame()
  
  for (i in 1:(n_model-1)) {
    pred50 = predict(myMethodObj[[i]], newdata=holdout, interval="prediction", level=0.5)
    pred80 = predict(myMethodObj[[i]], newdata=holdout, interval="prediction", level=0.8)
    if (i==1) {
      predMLR50 = pred50
      predMLR80 = pred80
    } else {
      predMLR50 = cbind(predMLR50, pred50)
      predMLR80 = cbind(predMLR80, pred80)
    }
  }
  
  predMLR50 = predMLR50 * exp(holdout$log_sqft_living)
  predMLR80 = predMLR80 * exp(holdout$log_sqft_living)
  predQRF50and80 = predQRF50and80 * exp(holdout$log_sqft_living)
  
  colnames(predMLR50)=c("fit0","lwr0","upr0","fit1","lwr1","upr1")
  colnames(predMLR80)=c("fit0","lwr0","upr0","fit1","lwr1","upr1")
  colnames(predQRF50and80) = c("q0.1", "q0.25", "q0.5", "q0.75", "q0.9")
  
  list(predMLR50=predMLR50, predMLR80=predMLR80, predQRF50and80=predQRF50and80)

}


## ----intervalScore-----------------------------------------------------------------------------------

#' @param predObj a set of prediction intervals associated with a fitted model
#' @param actual actual values of the response variable in the holdout set
#' @param level level of the prediction intervals
#' @param lwr_index column index of the lower bound of the prediction intervals
#' @param upr_index column index of the upper bound of the prediction intervals
#' @return a list including level, average length, interval score, coverage rate for this set of prediction intervals
intervalScore = function(predObj, actual, level, lwr_index, upr_index) { 
  n = nrow(predObj)
  alpha = 1-level
  ilow = (actual<predObj[,lwr_index]) # over-estimation
  ihigh = (actual>predObj[,upr_index]) # under-estimation
  sumlength = sum(predObj[,upr_index]-predObj[,lwr_index]) # sum of lengths of prediction intervals 
  sumlow = sum(predObj[ilow,lwr_index]-actual[ilow])*2/alpha
  sumhigh = sum(actual[ihigh]-predObj[ihigh,upr_index])*2/alpha
  avglength = sumlength/n
  IS = (sumlength+sumlow+sumhigh)/n # average length + average under/over penalties 
  cover = mean(actual>= predObj[,lwr_index] & actual<=predObj[,upr_index])
  summ = c(level,avglength,IS,cover) # summary with level, average length, interval score, coverage rate
  names(summ) = c("level","avglength","IS", "cover")
  summ
}



## ----perfMeas----------------------------------------------------------------------------------------

#' @param predObj a set of prediction intervals associated with one or more fitted models (for a continuous response variable)
#' @param holdout_y actual values of the response variable in the holdout set
#' @param level level of the prediction intervals
#' @param n_model the number of fitted models of which prediction interval values are contained in predObj; By default n_model= 1, this means predObj only contains values for one model
#' @return a list that includes model performance measures (level, average length, interval score, coverage rate) for each set of prediction intervals in predObj
perfMeas <- function(predObj, holdout_y, level, n_model=1){
  
  perfMeas = matrix(0,1,4)
  for (i in 1: n_model) {
    lwr_index = 2+3*(i-1)
    upr_index = 3*i
    curr = intervalScore(predObj, holdout_y, level, lwr_index, upr_index)
    if (i==1) {
      perfMeas = curr
    } else {
      perfMeas = c(perfMeas, curr)
    }
  }
  # print(perfMeas)
  perfMeas
  
}



## ----Cross Validation Template-----------------------------------------------------------------------

#' @param Kfold number of folds for cross-validation
#' @param iperm permutation vector for rows
#' @param datafr assume data frame has y x1 ... xp, can modify below to be more general
#' @param nMLR number of multiple linear regression models to be fitted
#' @return a list of performance matrices for models fitted: multiple linear regression model(s) + quantile random forest
# Note: This function includes steps: perform Kfold cross validation; get model performance metrics for each fold + overall(average) model performance metrics  
crossValidate = function(Kfold, iperm, datafr, nMLR) { 
  n = nrow(datafr); 
  nhold = round(n/Kfold)
  pred = list() # Save predictions in order to be able to compare them for different methods
  
  # matrices for storing performance measures: average length of prediction intervals, interval scores, 
  # coverage rate at levels 50% and 80%
  perfMatMLR_0.5 = matrix(0,Kfold,nMLR*4)  
  perfMatMLR_0.8 = matrix(0,Kfold,nMLR*4)
  perfMatQRF_0.5 = matrix(0,Kfold,4)  
  perfMatQRF_0.8 = matrix(0,Kfold,4)

  for (k in 1:Kfold) { 
    ilow = (k-1)*nhold+1; ihigh = k*nhold
    if(k==Kfold) { ihigh = n }
    ifold = iperm[ilow:ihigh] # holdout set
    myMethodObj = myMethod(datafr[-ifold,]) # fit model to training set
    pred[[k]] = myPredict(myMethodObj, datafr[ifold,]) # predict values of holdout set using model fitted
    
    perfMatMLR_0.5[k,] = perfMeas(pred[[k]]$predMLR50, datafr[ifold,]$unitPrice * exp(datafr[ifold,]$log_sqft_living), 0.5, n_model=nMLR)
    perfMatMLR_0.8[k,] = perfMeas(pred[[k]]$predMLR80, datafr[ifold,]$unitPrice * exp(datafr[ifold,]$log_sqft_living), 0.8, n_model=nMLR)
    
    # pred[[k]]$predQRF50and80 has columns: "q0.1", "q0.25", "q0.5", "q0.75", "q0.9"
    perfMatQRF_0.5[k,] = perfMeas(pred[[k]]$predQRF50and80[, c("q0.5", "q0.25", "q0.75")], datafr[ifold,]$unitPrice * exp(datafr[ifold,]$log_sqft_living), 0.5)
    perfMatQRF_0.8[k,] = perfMeas(pred[[k]]$predQRF50and80[, c("q0.5", "q0.1", "q0.9")], datafr[ifold,]$unitPrice * exp(datafr[ifold,]$log_sqft_living), 0.8)
    
  }
  
  perfAverageMLR_0.5 = colMeans(perfMatMLR_0.5)
  perfAverageMLR_0.8 = colMeans(perfMatMLR_0.8)
  perfAverageQRF_0.5 = colMeans(perfMatQRF_0.5)
  perfAverageQRF_0.8 = colMeans(perfMatQRF_0.8)
  
  list(perfMatMLR_0.5=perfMatMLR_0.5, perfAverageMLR_0.5=perfAverageMLR_0.5, perfMatMLR_0.8=perfMatMLR_0.8, perfAverageMLR_0.8=perfAverageMLR_0.8, perfMatQRF_0.5=perfMatQRF_0.5, perfAverageQRF_0.5=perfAverageQRF_0.5, perfMatQRF_0.8=perfMatQRF_0.8, perfAverageQRF_0.8=perfAverageQRF_0.8, pred=pred)
  
}



## ----import the cleaned and transformed dataset from saved .rds files--------------------------------

kv_house_data_family_filtered_for_CV = readRDS("/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/data/kv_house_data_family_filtered.rds")



## ----apply Cross Validation (CV) - main function-----------------------------------------------------

n = nrow(kv_house_data_family_filtered_for_CV) 
seed = 12345
set.seed(seed)
iperm = sample(n)

# Perform 3-fold cross-validation
# Note: It will may take at least 20 min to run this main function call due to the quantile random forest involved
modelCVOutput = crossValidate(Kfold=3, iperm, kv_house_data_family_filtered_for_CV, nMLR=2)
modelCVOutput

saveRDS(modelCVOutput, "/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/outputs/model_cross_validation_output.rds")



## ----import model performance outputs from model_cross_validation_output.rds saved in order to generate the performance tables in the following sections----

performanceOutput = readRDS("/Users/yujiech/Documents/GitHub/House Price Prediction for King County in USA/outputs/model_cross_validation_output.rds")



## ----performance table - overall---------------------------------------------------------------------

performanceOutputMLR_0.5 = performanceOutput$perfAverageMLR_0.5
performanceOutputMLR_0.8 = performanceOutput$perfAverageMLR_0.8

performanceOutputQRF_0.5 = performanceOutput$perfAverageQRF_0.5
performanceOutputQRF_0.8 = performanceOutput$perfAverageQRF_0.8

performance_table_MLR = rbind(performanceOutputMLR_0.5, performanceOutputMLR_0.8)[, -c(1, 5)]
colnames(performance_table_MLR) = c("avg length (all variables)", "IS (all variables)", "coverage rate (all variables)", "avg length (subset of variables)", "IS (subset of variables)", "coverage rate (subset of variables)")

performance_table_MLR_all_variables = round(performance_table_MLR[, 1:3], 2)
colnames(performance_table_MLR_all_variables) = c("avg length", "IS", "coverage rate")

performance_table_MLR_subset_of_variables = round(performance_table_MLR[, 4:6], 2)
colnames(performance_table_MLR_subset_of_variables) = c("avg length", "IS", "coverage rate")

performance_table_QRF = round(rbind(performanceOutputQRF_0.5, performanceOutputQRF_0.8)[, -1], 2)
colnames(performance_table_QRF) = c("avg length", "IS", "coverage rate")

performance_tables = list(performance_table_MLR=performance_table_MLR, performance_table_MLR_all_variables=performance_table_MLR_all_variables, performance_table_MLR_subset_of_variables=performance_table_MLR_subset_of_variables, performance_table_QRF=performance_table_QRF)

performance_tables



## ----performance table - per fold--------------------------------------------------------------------

performanceOutputMLR_0.5_per_fold = performanceOutput$perfMatMLR_0.5
performanceOutputMLR_0.8_per_fold = performanceOutput$perfMatMLR_0.8

performanceOutputQRF_0.5_per_fold = performanceOutput$perfMatQRF_0.5
performanceOutputQRF_0.8_per_fold = performanceOutput$perfMatQRF_0.8

# For Multiple Linear Regression models - 50% PI 
performanceOutputMLR_0.5_per_fold = round(performanceOutputMLR_0.5_per_fold[, -5], 2)
rownames(performanceOutputMLR_0.5_per_fold) = c("fold 1", "fold 2", "fold 3")
colnames(performanceOutputMLR_0.5_per_fold) = c("prediction level", "avg length (all variables)", "IS (all variables)", "coverage rate (all variables)", "avg length (subset of variables)", "IS (subset of variables)", "coverage rate (subset of variables)")

# For Multiple Linear Regression models - 80% PI 
performanceOutputMLR_0.8_per_fold = round(performanceOutputMLR_0.8_per_fold[, -5], 2)
rownames(performanceOutputMLR_0.8_per_fold) = c("fold 1", "fold 2", "fold 3")
colnames(performanceOutputMLR_0.8_per_fold) = c("prediction level", "avg length (all variables)", "IS (all variables)", "coverage rate (all variables)", "avg length (subset of variables)", "IS (subset of variables)", "coverage rate (subset of variables)")

# For Quantile Random Forest - 50% PI 
rownames(performanceOutputQRF_0.5_per_fold) = c("fold 1", "fold 2", "fold 3")
colnames(performanceOutputQRF_0.5_per_fold) = c("prediction level", "avg length", "IS", "coverage rate")

# For Quantile Random Forest - 80% PI 
rownames(performanceOutputQRF_0.8_per_fold) = c("fold 1", "fold 2", "fold 3")
colnames(performanceOutputQRF_0.8_per_fold) = c("prediction level", "avg length", "IS", "coverage rate")

performance_tables_per_fold = list(performanceOutputMLR_0.5_per_fold=performanceOutputMLR_0.5_per_fold, performanceOutputMLR_0.8_per_fold=performanceOutputMLR_0.8_per_fold, performanceOutputQRF_0.5_per_fold=performanceOutputQRF_0.5_per_fold, performanceOutputQRF_0.8_per_fold=performanceOutputQRF_0.8_per_fold)

performance_tables_per_fold


