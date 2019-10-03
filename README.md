# finance_article
R codes used in my finance article, the objective is to estimate latent volatilities and predict them. Also use VAR-LASSO to analyze the relationships between assets in terms of how volatilities of different stocks affect each other. 
The data is comprised by 75 of the most traded stocks of the brazilian stock market. 
We apply a univariate garch and a non-parametric moving window methods to find the underlying volatility then we predict one day ahead using var-lasso. For evaluation we use realized volatilty measures from the R package: GetHFData, author: Marcelo Perlim UFRGS.
