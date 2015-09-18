# Machine Learning on HAR Dataset
Ritesh Kumar Malaiya  
September 18, 2015  



**Load the Data**


```r
pmlTrain <- fread("pml-training.csv")
pmlTest <- fread("pml-testing.csv")

## Let's look at data
pmlTrain <- tbl_dt(pmlTrain)
```

# Exploratory Analysis

**Let's check on basic properties of the data**

 * How big the data is?

```r
dim(pmlTrain)
```

```
## [1] 19622   160
```

 * What all columns are present

```r
names(pmlTrain[, 1:50, with=F])
```

```
##  [1] "V1"                   "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "kurtosis_roll_belt"  
## [13] "kurtosis_picth_belt"  "kurtosis_yaw_belt"    "skewness_roll_belt"  
## [16] "skewness_roll_belt.1" "skewness_yaw_belt"    "max_roll_belt"       
## [19] "max_picth_belt"       "max_yaw_belt"         "min_roll_belt"       
## [22] "min_pitch_belt"       "min_yaw_belt"         "amplitude_roll_belt" 
## [25] "amplitude_pitch_belt" "amplitude_yaw_belt"   "var_total_accel_belt"
## [28] "avg_roll_belt"        "stddev_roll_belt"     "var_roll_belt"       
## [31] "avg_pitch_belt"       "stddev_pitch_belt"    "var_pitch_belt"      
## [34] "avg_yaw_belt"         "stddev_yaw_belt"      "var_yaw_belt"        
## [37] "gyros_belt_x"         "gyros_belt_y"         "gyros_belt_z"        
## [40] "accel_belt_x"         "accel_belt_y"         "accel_belt_z"        
## [43] "magnet_belt_x"        "magnet_belt_y"        "magnet_belt_z"       
## [46] "roll_arm"             "pitch_arm"            "yaw_arm"             
## [49] "total_accel_arm"      "var_accel_arm"
```

 * Basic properties of Data (Mean, Min, Max etc)

```r
summary(pmlTrain[, 7:25, with=F])
```

```
##    num_window      roll_belt        pitch_belt          yaw_belt      
##  Min.   :  1.0   Min.   :-28.90   Min.   :-55.8000   Min.   :-180.00  
##  1st Qu.:222.0   1st Qu.:  1.10   1st Qu.:  1.7600   1st Qu.: -88.30  
##  Median :424.0   Median :113.00   Median :  5.2800   Median : -13.00  
##  Mean   :430.6   Mean   : 64.41   Mean   :  0.3053   Mean   : -11.21  
##  3rd Qu.:644.0   3rd Qu.:123.00   3rd Qu.: 14.9000   3rd Qu.:  12.90  
##  Max.   :864.0   Max.   :162.00   Max.   : 60.3000   Max.   : 179.00  
##                                                                       
##  total_accel_belt kurtosis_roll_belt kurtosis_picth_belt
##  Min.   : 0.00    Length:19622       Length:19622       
##  1st Qu.: 3.00    Class :character   Class :character   
##  Median :17.00    Mode  :character   Mode  :character   
##  Mean   :11.31                                          
##  3rd Qu.:18.00                                          
##  Max.   :29.00                                          
##                                                         
##  kurtosis_yaw_belt  skewness_roll_belt skewness_roll_belt.1
##  Length:19622       Length:19622       Length:19622        
##  Class :character   Class :character   Class :character    
##  Mode  :character   Mode  :character   Mode  :character    
##                                                            
##                                                            
##                                                            
##                                                            
##  skewness_yaw_belt  max_roll_belt     max_picth_belt  max_yaw_belt      
##  Length:19622       Min.   :-94.300   Min.   : 3.00   Length:19622      
##  Class :character   1st Qu.:-88.000   1st Qu.: 5.00   Class :character  
##  Mode  :character   Median : -5.100   Median :18.00   Mode  :character  
##                     Mean   : -6.667   Mean   :12.92                     
##                     3rd Qu.: 18.500   3rd Qu.:19.00                     
##                     Max.   :180.000   Max.   :30.00                     
##                     NA's   :19216     NA's   :19216                     
##  min_roll_belt     min_pitch_belt  min_yaw_belt       amplitude_roll_belt
##  Min.   :-180.00   Min.   : 0.00   Length:19622       Min.   :  0.000    
##  1st Qu.: -88.40   1st Qu.: 3.00   Class :character   1st Qu.:  0.300    
##  Median :  -7.85   Median :16.00   Mode  :character   Median :  1.000    
##  Mean   : -10.44   Mean   :10.76                      Mean   :  3.769    
##  3rd Qu.:   9.05   3rd Qu.:17.00                      3rd Qu.:  2.083    
##  Max.   : 173.00   Max.   :23.00                      Max.   :360.000    
##  NA's   :19216     NA's   :19216                      NA's   :19216      
##  amplitude_pitch_belt
##  Min.   : 0.000      
##  1st Qu.: 1.000      
##  Median : 1.000      
##  Mean   : 2.167      
##  3rd Qu.: 2.000      
##  Max.   :12.000      
##  NA's   :19216
```

Through above quick glance, we can see that there are many variables which are not having values. Further observation of RAW data shows that many variables have values only when new_window variable is Yes. Hence, Let's observe data for new_window = Yes and No.



```r
pmlTrain_yes <- filter(pmlTrain, new_window == 'yes', num_window == 2)
pmlTrain_no <- filter(pmlTrain, new_window == 'no', num_window == 2)

head(pmlTrain_yes[,7:20, with=F], n= 1)
```

```
##   num_window roll_belt pitch_belt yaw_belt total_accel_belt
## 1          2     -27.8         60    -92.2                8
##   kurtosis_roll_belt kurtosis_picth_belt kurtosis_yaw_belt
## 1          -1.513346            1.299859           #DIV/0!
##   skewness_roll_belt skewness_roll_belt.1 skewness_yaw_belt max_roll_belt
## 1          -0.224472            -0.910566           #DIV/0!         -92.2
##   max_picth_belt max_yaw_belt
## 1             10         -1.5
```

```r
head(pmlTrain_no[,7:20, with=F], n = 2)
```

```
##   num_window roll_belt pitch_belt yaw_belt total_accel_belt
## 1          2     -22.7       58.2    -93.1                6
## 2          2     -24.7       58.0    -94.3                8
##   kurtosis_roll_belt kurtosis_picth_belt kurtosis_yaw_belt
## 1                                                         
## 2                                                         
##   skewness_roll_belt skewness_roll_belt.1 skewness_yaw_belt max_roll_belt
## 1                                                                      NA
## 2                                                                      NA
##   max_picth_belt max_yaw_belt
## 1             NA             
## 2             NA
```


**We can clearly see** that when *new_window is Y*, there are few columns that contains Summary of the Data, which is not present when *new_window = N*

# Data Preprocessing

## Remove Irrelevant Columns

 Let's remove the columns which may noy be necessary for Model Learning purposes.

 * First 7 columns are housekeeping data and can be ignored
 * As we observed above, there are few columns which are summary columns and may not be fit for Model Learning purposes. Summary columns are *kurtosis_, skewness_, max_, min_, amplitude_, var_total_,  avg_, stddev_, var_* 


```r
pmlTrain_sel <- pmlTrain[ ,-(1:7), with = F]

pmlTrain_sel <- dplyr::select(pmlTrain_sel, -contains("kurtosis_"), -contains("skewness_"), -contains("max_"), -contains("min_"),
                              -contains("amplitude_"), -contains("var_total_"), -contains("avg_"), -contains("stddev_"), -contains("var_") )

pmlTest_sel <- pmlTest[, names(pmlTrain_sel), with=F]

classe <- as.factor(pmlTrain_sel$classe)
pmlTrain_sel <- data.frame(sapply(select(pmlTrain_sel, -classe), as.numeric)) 


#classe <- as.factor(pmlTest_sel$classe)
pmlTest_sel <- data.frame(sapply(select(pmlTest_sel, -classe), as.numeric)) 
#pmlTest_sel$classe <- classe
```

## Identify Preprocessing Alogrithms required

### Outliers

**Checking for Outliers**
We can clearly see that there is quite difference in the measuring units of the data, also, we can see quite a few outliers. Hence we will perform *Centering and Scaling* while preprocesing data. However, we don't need other preprocessing like Imputation.


```r
boxplot(pmlTrain_sel)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-8-1.png) 

**Removing Outliers**

```r
pmlTrain_sel <- rm.outlier(pmlTrain_sel, fill = T)
```

### Correlation between Data
We can see that data is *almost* normally correlated. Hence we won't require preprocessing like BoxCox, YeoJohnson etc.

```r
cor <- cor(pmlTrain_sel, use = "p")
hist(cor)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-10-1.png) 

We haven't decided yet whether a proprocessing like PC transformation is required. We would decide on this after observing Model accuracy obtained.

### Class Balance

We can observe that Training data has more samples for Class A. We can either
* Balance the data for each Class (R Package _'Unbalanced'_)
* Carefully choose Model Training Algo which can handle class imbalance.

Since the class are not highly imbalanced, hence we would rather choose a appropriate Modelling Algo than performing Class Balancing processing.


```r
pmlTrain_sel_temp <- pmlTrain_sel
pmlTrain_sel_temp$classe <- classe
table(pmlTrain_sel_temp$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

## Preprocessing Data 

```r
preObj <- preProcess(pmlTrain_sel, method = c("center","scale"))

pmlTrain_prep <- predict(preObj, pmlTrain_sel)
pmlTest_prep <- predict(preObj, pmlTest_sel)

boxplot(pmlTrain_prep)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-12-1.png) 

# Model Training

We now need to decide which **model training Algorithm** should be choosen 

Till now we have observed from our Data:
```
* Doesn't have much missing values (after removing irrelevant columns)
* Is normally correlated
* Class A is imbalanced as compared to other classes
```

Based on this observation, we can somewhat identify that a Probability based Algo, will might get biased towards class A and may not provide high accuracy. We will also, try Neural, Vector Machine and Forest based Algos.


```r
registerDoParallel(cores=2)
control <- trainControl(method="repeatedcv", number=2, repeats=2)
```

## Probability Based Model
* **Naive Bayes**

```r
## Train and Plot a model
#pmlTrain_prep$classe <- classe
modelNB <- train(classe~., data=pmlTrain_prep, method="nb", trControl=control)
save(modelNB, file = "modelNB.rda")
pmlTrain_pred <- predict(modelNB)
```

* **Performance**


```r
plot(modelNB)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-15-1.png) 

```r
confusionMatrix(classe, pmlTrain_pred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4146  200  535  653   46
##          B  300 2678  527  258   34
##          C  148  218 2903  144    9
##          D  178   14  544 2347  133
##          E   98  319  176  121 2893
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7628          
##                  95% CI : (0.7568, 0.7687)
##     No Information Rate : 0.2482          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7017          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8513   0.7810   0.6196   0.6662   0.9287
## Specificity            0.9028   0.9309   0.9653   0.9460   0.9567
## Pos Pred Value         0.7430   0.7053   0.8483   0.7298   0.8021
## Neg Pred Value         0.9484   0.9525   0.8900   0.9283   0.9861
## Prevalence             0.2482   0.1748   0.2388   0.1795   0.1588
## Detection Rate         0.2113   0.1365   0.1479   0.1196   0.1474
## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      0.8771   0.8559   0.7924   0.8061   0.9427
```

## Neural Network Based Model

* __Model Averaged Neural Network__

```r
modelavNNET <- train(classe~., data=pmlTrain_prep, method="avNNet", trControl=control)
save(modelavNNET, file = "modelavNNET.rda")
pmlTrain_pred <- predict(modelavNNET)
```

* __Performance__

```r
plot(modelavNNET)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-17-1.png) 

```r
confusionMatrix(classe, pmlTrain_pred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 5325   40   27  155   33
##          B  767 2120  552  110  248
##          C  205  123 2947   74   73
##          D  203  140  538 2052  283
##          E   80  544  383  216 2384
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7557          
##                  95% CI : (0.7496, 0.7617)
##     No Information Rate : 0.3353          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6891          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8093   0.7145   0.6627   0.7871   0.7891
## Specificity            0.9804   0.8993   0.9687   0.9316   0.9263
## Pos Pred Value         0.9543   0.5583   0.8612   0.6381   0.6609
## Neg Pred Value         0.9106   0.9465   0.9074   0.9662   0.9602
## Prevalence             0.3353   0.1512   0.2266   0.1329   0.1540
## Detection Rate         0.2714   0.1080   0.1502   0.1046   0.1215
## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      0.8949   0.8069   0.8157   0.8594   0.8577
```

## Vector Machine Based Model

-**Support Vector Machines with Radial Basis Function Kernel**

```r
modelSVM <- train(classe~., data=pmlTrain_prep, method="svmRadial", trControl=control)
save(modelSVM, file = "modelSVM.rda")
pmlTrain_pred <- predict(modelSVM)
```
* **Performance**

```r
plot(modelSVM)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-19-1.png) 

```r
confusionMatrix(classe, pmlTrain_pred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 5562    3   14    0    1
##          B  286 3440   71    0    0
##          C    3  101 3289   24    5
##          D    8    0  293 2912    3
##          E    0   16  100   88 3403
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9482         
##                  95% CI : (0.945, 0.9513)
##     No Information Rate : 0.2986         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9344         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9493   0.9663   0.8731   0.9630   0.9974
## Specificity            0.9987   0.9778   0.9916   0.9817   0.9874
## Pos Pred Value         0.9968   0.9060   0.9611   0.9055   0.9434
## Neg Pred Value         0.9788   0.9924   0.9705   0.9932   0.9994
## Prevalence             0.2986   0.1814   0.1920   0.1541   0.1739
## Detection Rate         0.2835   0.1753   0.1676   0.1484   0.1734
## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      0.9740   0.9720   0.9324   0.9723   0.9924
```

## Forest Based Model

* **Random Forest**


```r
modelrf <- train(classe~., data=pmlTrain_prep, method="rf", trControl=control)
save(modelrf, file = "modelRF.rda")
pmlTrain_pred <- predict(modelrf)
```
* **Performance**


```r
plot(modelrf)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-21-1.png) 

```r
confusionMatrix(classe, pmlTrain_pred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 5580    0    0    0    0
##          B    0 3797    0    0    0
##          C    0    0 3422    0    0
##          D    0    0    0 3216    0
##          E    0    0    0    0 3607
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9998, 1)
##     No Information Rate : 0.2844     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2844   0.1935   0.1744   0.1639   0.1838
## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

## Comparing Models


```r
results <- resamples(list(nb=modelNB, avNNet=modelavNNET, SVM=modelSVM, rf=modelrf))
# summarize the distributions
summary(results)
```

```
## 
## Call:
## summary.resamples(object = results)
## 
## Models: nb, avNNet, SVM, rf 
## Number of resamples: 4 
## 
## Accuracy 
##          Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## nb     0.7323  0.7413 0.7447 0.7426  0.7460 0.7489    0
## avNNet 0.7811  0.7818 0.7880 0.7884  0.7946 0.7967    0
## SVM    0.9092  0.9138 0.9171 0.9158  0.9191 0.9197    0
## rf     0.9878  0.9889 0.9893 0.9891  0.9895 0.9902    0
## 
## Kappa 
##          Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## nb     0.6633  0.6747 0.6790 0.6764  0.6808 0.6844    0
## avNNet 0.7216  0.7224 0.7307 0.7312  0.7395 0.7417    0
## SVM    0.8849  0.8907 0.8950 0.8933  0.8975 0.8982    0
## rf     0.9845  0.9860 0.9865 0.9863  0.9868 0.9876    0
```

```r
# boxplots of results
bwplot(results)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-22-1.png) 

```r
# dot plots of results
dotplot(results)
```

![](coursera_project_human_activity_recognition_attempt2_files/figure-html/unnamed-chunk-22-2.png) 

## Prediction of Test Data


```r
pmlTest_pred_nb <- predict(modelNB, newdata = pmlTest_sel)
pmlTest_pred_nnet <- predict(modelavNNET, newdata = pmlTest_sel)
pmlTest_pred_svm <- predict(modelSVM, newdata = pmlTest_sel)
pmlTest_pred_rf <- predict(modelrf, newdata = pmlTest_sel)
```


* **Naive Bayes**

```r
pmlTest_pred_nb
```

```
##  [1] C C D A A A D D A C C D D A C D C D C D
## Levels: A B C D E
```
* **Neural Network**

```r
pmlTest_pred_nnet
```

```
##  [1] A A A A A C A A A A A A B A A A A B A B
## Levels: A B C D E
```
* **Support Vector Machine**

```r
pmlTest_pred_svm
```

```
##  [1] E E E E E E E E E E E E E E E E E E E E
## Levels: A B C D E
```
* **Random Forest**

```r
pmlTest_pred_rf
```

```
##  [1] E B B A A E E A B E B B B B E A E A E A
## Levels: A B C D E
```


