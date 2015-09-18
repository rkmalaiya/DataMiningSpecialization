# Impact of various factors(specially transmission) on MPG in Cars
# Evaluate Dataset

## FIT MODEL
### Identify Corelation
Let's try to observe the correlation between mpg and other variables (except cyl, am and vs, since these are categorical variable)

```
##         hp       drat         wt       disp       carb       qsec 
## -0.7761684  0.6811719 -0.8676594 -0.8475514 -0.5509251  0.4186840
```
### Fit a Linear Model
Based on correlation found above let's use AIC to choose attributes for Linear Model

```r
library(MASS)
lm_initial <- lm(mpg ~ am + hp + drat + wt + disp + carb + qsec, data = mtcars)
step <- stepAIC(lm_initial, direction="both")
```
Model recommendation by AIC

```r
step$anova # display results
```

```
## Stepwise Model Path 
## Analysis of Deviance Table
## 
## Initial Model:
## mpg ~ am + hp + drat + wt + disp + carb + qsec
## 
## Final Model:
## mpg ~ am + wt + qsec
## 
## 
##     Step Df  Deviance Resid. Df Resid. Dev      AIC
## 1                            24   149.9865 65.43390
## 2 - carb  1 0.1067476        25   150.0933 63.45667
## 3 - drat  1 3.3445512        26   153.4378 62.16190
## 4 - disp  1 6.6286537        27   160.0665 61.51530
## 5   - hp  1 9.2194693        28   169.2859 61.30730
```
From above analysis, we can conclude that below should be the final model

```r
lm <- lm(mpg ~ am + wt + qsec, data = mtcars)
```
## Plot of Model
![](RegressionModels_Week3_Project_2pages_files/figure-html/unnamed-chunk-6-1.png) 
## Coefficients as per the model

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -3.91700 -0.05971  2.08100  2.46600  4.60600  9.61800
```
# Questions of Interest
- **Is an automatic or manual transmission better for MPG**
From above coefficients we can understand that ***Manual Transmission is better*** than Automatic Transmission.
- **Quantify the MPG difference between automatic and manual transmissions**
If we keep weight and qsec constant (mean) then extra milega that will be provided by Manual Transmission as compared to Automatic is 2.9358372. However, for every increase in weight by single unit gain in mileage is 8.637114 and for every increase in qsec gain in mileage is 13.7795037 more than Automatic Transmission.

# Executive Summary
For a Car having same weigth Manual Transamission gives better mileage than Automatic Transmission. Also, for every increase in weight and / or qsec, corresponding increase in Manual transmission is in range of > 2.9358372 and < 9.863.
