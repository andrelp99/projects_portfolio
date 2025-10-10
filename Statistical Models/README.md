## Statistical Models

Project completed using RMarkdown during the **Statistical Models** exam, academic year 2019/20, Bachelor’s Degree in Statistics and Information Management (SGI), University of Milano – Bicocca.

Assignment summary:
Analysis of restaurant meal costs as a function of food quality, elegance, service, and location.
Tasks included descriptive statistics, scatter plots, multicollinearity checks, multiple regression modeling, interpretation of parameters, residual analysis, confidence intervals, and application of the Akaike Information Criterion for variable selection.

### Tasks

You must submit a report indicating the exercise number and the corresponding question, followed by the results and the required comments for each point.
The file res.Rdata contains data on the cost of a single meal at several restaurants in a given city.
The goal is to evaluate the meal cost as a function of the following variables:
- food quality score (X1)
- restaurant elegance (X2)
- service quality (X3)
- restaurant location (X4), coded as 0 for restaurants located in the city center and 1 for those in the suburbs.

1. Report and comment on the data in relation to the application context using descriptive statistics. Produce and discuss the scatter plot of meal cost versus food quality, highlighting the restaurant’s location in the plot. Comment on the figure.
2. Compute the Variance Inflation Factor (VIF) for the quantitative variables. Explain what it represents and comment on the obtained values.
3. Estimate a multiple linear regression model including all explanatory variables. State the null and alternative hypotheses of the test performed on the parameter corresponding to the restaurant location variable, and comment on the outcome based on the decision rule.
4. Estimate the model again, this time excluding the location variable. Report and discuss, with reference to the application context, the results provided by the model summary. List the properties of the regression coefficient estimators.
5. Plot the residuals against the explanatory variable X2, and produce the quantile–quantile plot of the studentized residuals. Comment on both graphs.
6. Describe how to construct joint confidence intervals for pairs of regression coefficients. Draw the contour plot of the joint distribution of the parameters corresponding to restaurant elegance and service quality. Add the confidence interval endpoints for each individual parameter to the plot and comment on the figure.
7. Determine a confidence interval for the predicted mean cost when the explanatory variables take the following values: X1 = 23, X2 = 19, X3 = 27. Comment on the result.
8. Describe Akaike’s Information Criterion (AIC) and explain how it is used for the selection of explanatory variables in a multiple linear regression model.
