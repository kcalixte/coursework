#More sales data

#Let’s return to the orange juice assignment and investigate how store demographics are related to demand.
#Let’s start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). Try them individually and then all together.
model1 <- lm(logmove ~ log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + EDUC + HHLARGE, data= oj)
summary(model1)

# What demographics are significantly (t > 2 standard deviations) related to demand?
  #A All of them

# How much did the adjusted R-squared improve with the addition of these variables?
  #A When all of them are added the adjusted R-value went up by approx .03 

# Let’s focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers with advanced education").
# What are the means and percentiles of each of these variables?
mean(oj$HHLARGE)
    #[1] 0.1156024
quantile(oj$HHLARGE)
    # 0%        25%        50%        75%       100% 
    # 0.01350636 0.09793763 0.11122120 0.13516767 0.21635434 

mean(oj$EDUC)
#[1] 0.2252196
quantile(oj$EDUC)
# 0%        25%        50%        75%       100% 
# 0.04955029 0.14598491 0.22939040 0.28439465 0.52836201 

# Using your coefficient estimates from the regression in 1b:
# If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the predicted values for rows that have the median and 75th percentiles for HHLARGE.
model1 <- lm(logmove ~ log(price)*brand*feat, data= oj)
oj$predicted <- fitted(model1)
plotMedian <- (filter(oj, oj$HHLARGE == median(HHLARGE)))
plotQuantile <- (filter(oj, oj$HHLARGE == quantile(HHLARGE, .75)))
ggplot(plotQuantile, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ brand)
ggplot(plotMedian, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ brand)

ggplot(plotQuantile, aes(x = week, y = predicted)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotMedian, aes(x = week, y = predicted)) + geom_point() + geom_smooth(method = "lm")
    #
    # Median: 8.75
    # 75 percentile: 8.47

# If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change each week on average?
plotMedian <- (filter(oj, oj$EDUC== median(EDUC)))
plotQuantile <- (filter(oj, oj$EDUC== quantile(EDUC, .75)))
ggplot(plotMedian, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotQuantile, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")


ggplot(plotQuantile, aes(x = week, y = predicted)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotMedian, aes(x = week, y = predicted)) + geom_point() + geom_smooth(method = "lm")
  #logmove increases more on average each week at the 75th percentile then it does at the median 

# Based on this analysis, which is the more important predictor of demand?
    #According to the predicted values HHLARGE is more important
    # Education is more important
    # ___Household size is more important___

# Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model to test this.
# What are the coefficients on the interaction terms?
model1 <- lm(logmove ~ log(price)*brand*feat + log(price)*HHLARGE + log(price)*EDUC, data= oj)
  #look at line 110 in oj.R

      #                                 Estimate Std. Error t value Pr(>|t|)   
      #log(price):HHLARGE               -4.72898    0.48502  -9.750  < 2e-16 ***
      #log(price):EDUC                   3.69490    0.13337  27.703  < 2e-16 ***

      #____ Coefficients:
      #                         Estimate Std. Error t value Pr(>|t|)    
      #   log(price):HHLARGE       -6.4933     1.2442  -5.219 1.81e-07 ***
      #   log(price):EDUC           2.1552     0.5947   3.624  0.00029 ***
      #   HHLARGE:EDUC            -20.2578     4.3595  -4.647 3.39e-06 ***
      #   log(price):HHLARGE:EDUC  12.7063     5.0518   2.515  0.01190 *  ______

# Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. Do your estimates make sense based on your intuition?
    #Yes this does make sense based on intuition. Larger Households are more sensitive to price changes because they must buy things in large quantities. 
    #Those with higher education most likely have a higher annual income which makes them less sensitive to the changes in a products price 


# What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?

    #HHLARGE          0.91991  
    #EDUC             -3.05531
    # The signs are switched and their absolute values are smaller


    # ____HHLARGE         4.6602
    #   EDUC          -0.6981____

# Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change? Based on this, which is more important to price sensitivity?
oj$predicted <- fitted(model1)


plotMedian <- (filter(oj, oj$HHLARGE == median(HHLARGE)))
plotQuantile <- (filter(oj, oj$HHLARGE == quantile(HHLARGE, .75)))
ggplot(plotQuantile, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotMedian, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")

ggplot(plotQuantile, aes(x = price, y = predicted)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotMedian, aes(x = price, y = predicted)) + geom_point() + geom_smooth(method = "lm")



plotMedian <- (filter(oj, oj$EDUC== median(EDUC)))
plotQuantile <- (filter(oj, oj$EDUC== quantile(EDUC, .75)))
ggplot(plotMedian, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotQuantile, aes(x = week, y = logmove)) + geom_point() + geom_smooth(method = "lm")

ggplot(plotQuantile, aes(x = price, y = predicted)) + geom_point() + geom_smooth(method = "lm")
ggplot(plotMedian, aes(x = price, y = predicted)) + geom_point() + geom_smooth(method = "lm")

    #HHLARGE is still more price sensitive

# You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.
    #HHLARGE alone affects demand directly because large household buy more by default
    #HHLARGE interacting with price shows price sensitivity because they have to purchase high quanities but they don't want to pay too much money

# Let’s split our data into a training set and a test set. An easy way to do this is with the sample command. The following will randomly select 20% of the rows in our data frame: indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))

# Now let’s use this index to create a training and a test set, try: OJtest=oj[index, ] and Ojtrain=oj[-index, ]. What did this do? How many rows does the test set have? How many rows does the training set have?
OJtest=oj[indexes, ]
Ojtrain=oj[-indexes, ] 
    #A This created a test dataframe and a training data frame by pulling out the records at the specified indexes. The "-" in front of the specified indexes take the other indexes.
    #A OJtest has 5789 and OJtrain has 23158

# Now let’s run the very simple model logmove ~ log(price) + brand on the training data.
# Use LM on this model and report the R-squared.
modelTrain <- lm(logmove ~ log(price) + brand, data= Ojtrain)
summary(modelTrain)
    #Multiple R-squared:  0.3931,	Adjusted R-squared:  0.393 

# Use predict(model, Ojtest) to predict log sales for the test set.
OJtest$predicted <- predict(modelTrain, OJtest)

# Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". How does it compare to the value in (a)?
cor(OJtest$predicted, OJtest$logmove)^2
    #[1] 0.3983027
    #It is approximately the same but slighty higher by .005


# Now let’s run better models.
# Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. Use LM to get regular R-squared. Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
modelTrain2 <- lm(logmove ~ brand*log(price)*feat, data= Ojtrain)
summary(modelTrain2)
    #Multiple R-squared:  0.5326,	Adjusted R-squared:  0.5324 
OJtest$predicted <- predict(modelTrain2, OJtest)
cor(OJtest$predicted, OJtest$logmove)^2
    #[1] 0.5465612
    #It is approximately the same but slighty higher by .014

# Now add in all the demographics. What is the regular R-squared on training data? What is the honest R-squared on the test set?
modelTrain3 <- lm(logmove ~ log(price)*brand*feat - 1 + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, data=oj)
modelTrain3 <- lm(logmove ~ brand*log(price)*feat, data= Ojtrain)
summary(modelTrain3)
    #Multiple R-squared:  0.9949,	Adjusted R-squared:  0.9949 
OJtest$predicted <- predict(modelTrain3, OJtest)
cor(OJtest$predicted, OJtest$logmove)^2
    #[1] 0.5943739
