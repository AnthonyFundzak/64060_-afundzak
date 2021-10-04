Index_Train <- createDataPartition(UniversalBank$Personal.Loan, p=.06, list=FALSE)
Train <- UniversalBank_[Index_Train,]
Test <- UniversalBank_[-Index_Train,]
QUESTION ONE
summary(UniversalBank)
ID            Age          Experience       Income          ZIP.Code         Family          CCAvg          Education    
Min.   :   1   Min.   :23.00   Min.   :-3.0   Min.   :  8.00   Min.   : 9307   Min.   :1.000   Min.   : 0.000   Min.   :1.000  
1st Qu.:1251   1st Qu.:35.00   1st Qu.:10.0   1st Qu.: 39.00   1st Qu.:91911   1st Qu.:1.000   1st Qu.: 0.700   1st Qu.:1.000  
Median :2500   Median :45.00   Median :20.0   Median : 64.00   Median :93437   Median :2.000   Median : 1.500   Median :2.000  
Mean   :2500   Mean   :45.34   Mean   :20.1   Mean   : 73.77   Mean   :93152   Mean   :2.396   Mean   : 1.938   Mean   :1.881  
3rd Qu.:3750   3rd Qu.:55.00   3rd Qu.:30.0   3rd Qu.: 98.00   3rd Qu.:94608   3rd Qu.:3.000   3rd Qu.: 2.500   3rd Qu.:3.000  
Max.   :5000   Max.   :67.00   Max.   :43.0   Max.   :224.00   Max.   :96651   Max.   :4.000   Max.   :10.000   Max.   :3.000  
Mortgage     Personal.Loan   Securities.Account   CD.Account         Online         CreditCard   
Min.   :  0.0   Min.   :0.000   Min.   :0.0000     Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
1st Qu.:  0.0   1st Qu.:0.000   1st Qu.:0.0000     1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000  
Median :  0.0   Median :0.000   Median :0.0000     Median :0.0000   Median :1.0000   Median :0.000  
Mean   : 56.5   Mean   :0.096   Mean   :0.1044     Mean   :0.0604   Mean   :0.5968   Mean   :0.294  
3rd Qu.:101.0   3rd Qu.:0.000   3rd Qu.:0.0000     3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
Max.   :635.0   Max.   :1.000   Max.   :1.0000     Max.   :1.0000   Max.   :1.0000   Max.   :1.000  
UniversalBank_normalized <- predict(norm_model, UniversalBank)
UniversalBank_normalized <- UniversalBank_normalized[,-1]
UniversalBank_normalized <- UniversalBank_normalized[,-4]
Transforming categorical predictors of education into dummy
dummyUniversal <- dummy_cols(UniversalBank_normalized select_columns = 'education')
Index_Train <- createDataPartition(UniversalBank_normalized$Personal.Loan, p=.05, list=FALSE)
Train <- UniversalBank_normalized[Index_Train,]
Test <- UniversalBank_normalized[-Index_Train,]
Train_Predictors <- Train[,8:3]
Test_Predictors <- Test[,8:3]
Train_Labels <- Train[,2]
Test_Labels <- Test[,2]
Predicted_Test <- knn(Train_Predictors,Test_Predictors,cl=Train_Labels, k=1)
head(Predicted_Test)
[1] 0.782608695652174  0.826086956521739  0.847826086956522 
[4] 0.108695652173913  0.0869565217391304 0.630434782608696 
43 Levels: 0.0217391304347826 ... 0.978260869565217
This customer would be classified as NOT being approved for a loan from UniversalBank
QUESTION 2
Predicted_Test <- knn(Train_Predictors,Test_Predictors,cl=Train_Labels, k=4)
head(Predicted_Test)
[1] 0.260869565217391 0.478260869565217 0.652173913043478
[4] 0.260869565217391 0.239130434782609 0.630434782608696
43 Levels: 0.0217391304347826 0.0434782608695652 ... 0.978260869565217
After testing numerous k values in Predicted_Test and head(Predicted_Test), a choice of k that balances between overfittng and ignoring the predictor information is k = 6.
This is because the values of percent error testing and percent error training are close and strong
Predicted_Test <- knn(Train_Predictors,Test_Predictors,cl=Train_Labels, k=6)
QUESTION 3
For   k=1   CrossTable(x=Test_Labels,y=Predicted_Test, prop.chisq = FALSE)
QUESTION 4
Using the best k of k=1, this customer would be classified as YES being approved for a lon from UniversalBank
Using the best k means the best fit the best true/true data for the knn test. 
Index_Train <- createDataPartition(UniversalBank_normalized$Personal.Loan, p=.05, list=FALSE)
Train <- UniversalBank_normalized[Index_Train,]
Test <- UniversalBank_normalized[-Index_Train,]
Train_Predictors <- Train[,8:3]
Test_Predictors <- Test[,8:3]
Train_Labels <- Train[,1]
Test_Labels <- Test[,1]
Predicted_Test <- knn(Train_Predictors,Test_Predictors,cl=Train_Labels, k=1)
head(Predicted_Test)
QUESTION 5
Index_Train <- createDataPartition(UniversalBank_normalized$Personal.Loan, p=.05, list=FALSE)
Index_Train <- createDataPartition(UniversalBank_normalized$Personal.Loan, p=.03, list=FALSE)
Index_Train <- createDataPartition(UniversalBank_normalized$Personal.Loan, p=.02, list=FALSE)
Train <- UniversalBank_normalized[Index_Train,]
Test <- UniversalBank_normalized[-Index_Train,]
Train_Predictors <- Train[,8:3]
Test_Predictors <- Test[,8:3]
Train_Labels <- Train[,1]
Test_Labels <- Test[,1]
CrossTable(x=Test_Labels,y=Predicted_Test, prop.chisq = FALSE)
CrossTable(x=Train_Labels,y=Predicted_Test, prop.chisq = FALSE)
CrossTable(x=Predicted_Test,y=Predicted_Test, prop.chisq = FALSE)
Each confusion matrix is different but not but much variation. Some of the casues of change inclue;
differences in accuracy of classified and predicted values,senstivty of the data to TP,FP predicitons data,
Increasing and decressing the best K and the threshold of the k value also shifted amounts of TP,FP in each 
confusion matrix test ran. 
