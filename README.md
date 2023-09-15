# House-Price-Prediction-for-King-County-in-USA

<b> Programming Language used: </b> R
<br>
<b> ML: </b> Multiple Linear Regression and Quantile Random Forest; Stepwise AIC feature selection, Multi-colliearity, k-fold cross validation
<br>
<b> Data Visualization: </b> ggplot2, ggcorrplot, RColorBrewer

### 1.1 Introduction and Goals
The housing market is an ever-changing landscape that requires a deep understanding of market trends and buyer behaviour. The aim of this project is to develop a model for predicting the future price of houses in King County, USA. 

To accomplish this goal, we utilized a combination of categorical and numerical predictor variables, including sqftliving, bathrooms, bedrooms, view, and others. In addition, we applied data transformation techniques to preprocess the data before building the models. We applied visualizations, including (side-by-side) boxplots, histograms, bar charts and etc. to overview the properties and distributions of the dataset.

Our preliminary methodology consisted of using multiple linear regression and Quantile random forest models to analyze the dataset.

### 1.2 Highlights
We began our analysis by conducting exploratory data analysis to identify the relationship between predictor variables and the target variable, which in this case is unit price of the house, which is calculated as the ratio of the price of the house to the square footage of living space. We found that there were significant correlations between several predictor variables and the
target variable. Additionally, we utilized feature engineering techniques to transform the data and enhance the performance of our models.

To evaluate the performance of the models, we compared the multiple linear regression and Quantile random forest models based on their in-sample performance using AIC. We found that both models had similar performance in predicting future house prices. However, when comparing the out-of-sample predictions and cross-validated performance measures, the Quantile random forest model outperformed the multiple linear regression model.

### 1.3 Main Conclusions
Our main conclusions from this analysis were that both the multiple linear regression and Quantile random forest models provided accurate predictions of future house prices. However, the Quantile random forest model outperformed the multiple linear regression model. These findings have important implications for the real estate industry, as they can help inform decisions related to buying and selling properties. For example, real estate agents and buyers can use our model to predict the future price of a house and make more informed decisions based on the data. Furthermore, our analysis demonstrates the power of statistical techniques in predicting housing prices, which can be applied to other housing markets as well.

### 1.4 Potential Future Directions
Looking ahead, there are several potential future directions for this research. One potential area of focus is to explore other machine learning models and algorithms to further improve the accuracy of our predictions. Additionally, we can expand the dataset to include additional predictor variables, such as location, neighborhood demographics, and other market trends. By
continuing to refine our models and data, we can develop a more robust and accurate system for predicting housing prices in King County and beyond.
