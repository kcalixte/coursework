library(dplyr)
library(readr)



# load oj data
oj <- read.table('oj.csv', header=T, sep=',')


#Visualizing price.
###Make a plot of the distribution of prices.
ggplot(oj, aes(price)) + geom_bar() 

###Change the x-axis on this plot to use a logarithmic scale using scale_x_log10().
ggplot(oj, aes(price)) + geom_histogram() + scale_x_log10()

###Repeat i), faceted by brand.
ggplot(oj, aes(x = price)) + geom_bar() + facet_grid(. ~ brand)

##Repeat ii), faceted by brand.
ggplot(oj, aes(x = price)) + geom_bar() + scale_x_log10() + facet_grid(. ~ brand)

###What do these graphs tell you about the variation in price? Why do the log plots look different? Do you find them more/less informative?
########AAA Tropicana is more expensive than the other two brands but most people in the sample buy orange juice that is between 1 and 2 dollars.
########AAA They are less/more informative depending on if details are a priority or if a general  

#Visualizing the quantity/price relationship.
###Plot logmove (the log of quantity sold) vs. log price.
ggplot(oj, aes(x = log(price), y = logmove)) + geom_point()

###Color each point by brand. What do insights can you derive that were not apparent before?
ggplot(oj, aes(x = log(price), y = logmove, color = brand)) + geom_point()
######AAA dominicks is relatively cheap tropicana is relatively expensive. As price decreases quantity sold increases. 
######AAA Dominicks takes a bigger hit when they increase their price vs when tropicana does the same


#Estimating the relationship.
### Do a regression of logmove on log price. How well does the model fit? What is the elasticity (the coefficient on log price), and does it make sense? See here for some background on elasticity.
lm.fit = lm(logmove~log(price), data = oj)
plot(logmove~log(price), data=oj)
plot(oj$logmove, log(oj$price))
abline(lm.fit)

########AAA The model fits _____. The elasticity/coefficient on log price is -1.60131. It doesnt make sense to me because the elasticity is less than -1 which says that 
########AAA price and demand move in the same direction

### Now add in an intercept term for each brand (by adding brand to the regression formula). How do the results change? How should we interpret these coefficients?
lm.fit = lm(logmove~log(price)+brand, data = oj)
summary(lm.fit)
coef(lm.fit)
######AAA the log price coefficeint becomes more negative. The coeeficient for dominicks is not shown. Minute maid is .8701747  its between -1 and 1
######AAA which means demand is relatively inelastic, Tropicana is 1.5299428 which is higher than 1 meaning price and demand move in oppisite directions

###Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression formula. Note the estimate coefficients will "offset" the base estimates. What is the insights we get from this regression? What is the elasticity for each firm? Do the elasticities make sense?
lm.fit = lm(logmove~brand:log(price), data = oj)
#A(Intercept)   branddominicks:log(price) brandminute.maid:log(price)   brandtropicana:log(price) 
#A11.291720                   -3.920317                   -2.658435                   -2.130007 



#Impact of "featuring in store".

### Which brand is featured the most? Make a plot to show this.
ggplot(oj, aes(x = feat) ) + geom_histogram() + facet_grid(. ~ brand)
oj %>% filter(feat == 1) %>% group_by(brand) %>% ggplot(aes(x = brand) ) + geom_bar()

#How should we incorporate the "featured in store" variable into our regression? Start with an additive formulation (e.g. feature impacts sales, but not through price).
lm.fit = lm(logmove~brand:log(price) + feat, data = oj)
coef(lm.fit)
#(Intercept)                        feat   branddominicks:log(price) brandminute.maid:log(price)   brandtropicana:log(price) 
#10.6570513                   0.9227326                  -3.2046791                  -2.1765727                  -1.6572625 

#Now run a model where features can impact sales and price sensitivity.
lm.fit = lm(logmove~log(price)*feat, data = oj)
coef(lm.fit)
# (Intercept)      log(price)            feat log(price):feat 
# 9.6592637      -0.9582286       1.7143843      -0.9772859 

#Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
lm.fit = lm(logmove~brand:log(price)*feat, data = oj)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      10.53479    0.01868 563.919  < 2e-16 ***
#   feat                              1.33948    0.03188  42.020  < 2e-16 ***
#   branddominicks:log(price)        -2.97375    0.03220 -92.350  < 2e-16 ***
#   brandminute.maid:log(price)      -2.08461    0.02375 -87.772  < 2e-16 ***
#   brandtropicana:log(price)        -1.51291    0.01841 -82.191  < 2e-16 ***
#   branddominicks:log(price):feat   -0.96414    0.06492 -14.851  < 2e-16 ***
#   brandminute.maid:log(price):feat -0.32409    0.04661  -6.953 3.65e-12 ***
#   brandtropicana:log(price):feat   -0.73270    0.03966 -18.474  < 2e-16 ***
  
  
  #OR#
  lm.fit = lm(logmove~brand*log(price) + brand*feat, data = oj)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.9633 -0.4307 -0.0044  0.4207  3.2311 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 10.47934    0.02053 510.507  < 2e-16 ***
#   brandminute.maid             0.28824    0.04123   6.991 2.78e-12 ***
#   brandtropicana               0.90622    0.04630  19.574  < 2e-16 ***
#   log(price)                  -2.90339    0.03336 -87.024  < 2e-16 ***
#   feat                         0.87732    0.01699  51.639  < 2e-16 ***
#   brandminute.maid:log(price)  0.53408    0.05353   9.978  < 2e-16 ***
#   brandtropicana:log(price)    0.61191    0.05061  12.090  < 2e-16 ***
#   brandminute.maid:feat        0.24244    0.02386  10.161  < 2e-16 ***
#   brandtropicana:feat         -0.29564    0.02692 -10.982  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7011 on 28938 degrees of freedom
# Multiple R-squared:  0.5271,	Adjusted R-squared:  0.5269 
# F-statistic:  4031 on 8 and 28938 DF,  p-value: < 2.2e-16
  
  
  ggplot(oj, aes(x = log(price), y = logmove, color = as.factor(feat))) + geom_smooth(method = "lm") + facet_wrap(~ brand) + geom_point()
