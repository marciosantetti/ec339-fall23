

#== EC 339
#== Prof. Santetti 

#=============================================================================#
#                         SIMPLE LINEAR REGRESSION IN R                       #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files.' Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory.'

#================================================================================================#






###--- Loading the necessary packages for today:


library(tidyverse)
library(broom)          ## for manipulating regression objects.
library(wooldridge)     ## for Wooldrige's book data sets.


##---------------------------------------------------------


##--- In this script, we finally start doing Econometrics in practice.
## We'll use 'wooldridge' package (data from Wooldridge's book):


## The 'beauty' data set brings microeconometric data on wages, education, experience, gender, etc.

## Reference: Hamermesh, D.S. and J.E. Biddle (1994), "Beauty and the Labor Market,"
## American Economic Review 84, 1174-1194.


## To import it, as soon as the "wooldridge" is loaded:

data("beauty")

## And we transform it into a "tibble":

beauty <- beauty %>% 
  as_tibble()


## In your own time, make sure to run some statistical analyses (as done last week) for this data set. This
## way, you know your data better.


##--- Suppose we are interested in the relationship between an individual's educational attainment
## and their hourly wages.


## A good first step is to visualize this relationship:

beauty %>% 
  ggplot(aes(y = wage, x = educ)) +
  geom_point()


## Regression models in R are calculated with the 'lm()' function. lm = 'linear model.'

## The syntax is: lm(dependent ~ independent variable, data set).


## Thus, for our wage-education model, we have:


simple_reg1 <- lm(wage ~ educ, data = beauty)


## To access the regression results, we use the "summary()" function:


simple_reg1 %>% 
  summary()


## Another option is the "tidy()" function from the {broom} package:

simple_reg1 %>% 
  tidy()


## Q: How do we interpret these results?

## A:



## To draw the regression line generated by OLS,
## we simply add the "geom_smooth()" function to a scatter plot:


beauty %>% 
  ggplot(aes(y = wage, x = educ)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)





##--- Now, what about wages vs. years of experience in the labor market?


## First, visually...

beauty %>% 
  ggplot(aes(y = wage, x = exper)) +
  geom_point()


## Then, a model:


simple_reg2 <- lm(wage ~ exper, data = beauty)


simple_reg2 %>% 
  summary()



##---------------------------------------------------------


##--- Let us now work with an external data set:

## A subset of the data from Tennessee’s Project STAR (Student/Teacher Achievement Ratio).


star <- read_csv("star5_small.csv")


## And let us see the relationship between a student's total score (totalscore) and their math score (mathscore),
## fot classrooms with a school aide (aide)

## Here, aide equals 1 if the classroom has a school aide, and 0 if not. We need to filter the data first:



star_aide <- star %>% 
  filter(aide %in% 1)   # just selecting rows where aide = 1.


## Some visualization:


star_aide %>%
  ggplot(aes(y = totalscore, x = mathscore)) +
  geom_point()



## And a regression model:


simple_reg3 <- lm(totalscore ~ mathscore, data = star_aide)


simple_reg3 %>% 
  summary()


##---------------------------------------------------------

##--- regression residuals:


# We can easily extract the residual term from regression objects in R.

resid_reg3 <- simple_reg3 %>% 
  resid() %>% 
  as_tibble() %>% 
  rename(residuals = value)


resid_reg3 %>% 
  summarize(mean_residual = mean(residuals))        ## Is it zero, as theoretically assumed?


## We can add the residuals as a new column to the data set:


star_aide <- star_aide %>% 
  add_column(resid_reg3)



## What about the relationship between the independent variable (x) and the residual term (u)?


star_aide %>% 
  summarize(cov_x_u = cov(residuals, mathscore))    ## Is it zero, as theoretically assumed?



## And we can visualize the residual's distribution:


star_aide %>% 
  ggplot(aes(x = residuals)) +
  geom_histogram(color = "white")

# or


star_aide %>% 
  ggplot(aes(x = residuals)) +
  geom_density()


##---------------------------------------------------------



##--- Lastly, let us compute the slope and intercept coefficients from the previous regression by hand.

## Recall the formulas: slope (beta1) = Cov(x,y)/Var(x)
##                      intercept (beta0) = y.bar - x.bar * beta1, where x.bar and y.bar are the variables' means.


## We can use the "summarize()" function to do that:


star_aide %>% 
  summarize( mean_mathscore = mean(mathscore),
             
             mean_totalscore = mean(totalscore),
             
             covariance = cov(totalscore, mathscore),
             
             var_mathscore = var(mathscore),
             
             slope_coef = covariance / var_mathscore,
             
             intercept_coef = mean_totalscore - slope_coef * mean_mathscore)   ## Are these the same as before?



##---------------------------------------------------------







#--- Practice:

#== The 'food.csv' data set is available at the course GitHub page and on theSpring. It contains only
#== two variables: weekly food expenditures (in dollars), and weekly income (in hundreds of dollars).
#== Import this data set into your R environment, and play around with its variables. You
#== can do some basic statistical analysis, and then estimate a regression model, both with
#== the 'lm()' command and with manual calculations. The important thing is to make yourself 
#== comfortable with these concepts and the R practice. And even more importantly: enjoy your
#== practice time!
