Linear Regression
================
Matt Bartley
7/19/2020

## Background

This is a walkthrough of the HarvardX data science course on linear
regression. We start by examining a motivating example based on
Moneyball, a movie on the evolution of baseball statistics. Note the
following useful definitions:

1.  Plate appearance (PA): each time the batter has an opportunity to
    hit, it is called a plate appearance
2.  Bases on balls (BB): the pitcher fails to throw the ball through a
    predefined area considered to be hittable (the strike zone), so the
    batter is permitted to go to first base.
3.  Single: the batter hits the ball and gets to first base.
4.  Double (2B): the batter hits the ball and gets to second base.
5.  Triple (3B): the batter hits the ball and gets to third base.
6.  Home Run (HR): the batter hits the ball and goes all the way home
    and scores a run.

Note that a significant portion of the code is taken from the actual
course.  
<https://courses.edx.org/courses/course-v1:HarvardX+PH125.7x+2T2020/>

There a number of hypotheses we can examine through exploratory data
analysis and regression analysis. An obvious example, we can look at the
relationship between home runs and runs per game. Not surprisingly, we
see a corresponding linear relationship.

``` r
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
```

![](regression_files/figure-gfm/Bases%20on%20Balls%20or%20Stolen%20Bases-1.png)<!-- -->

``` r
NT <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, 
         R_per_game = R/G, 
         HR_per_game = HR / G, 
         W_per_game = W/G, 
         E_per_game = E/G,
         X3B_per_game = X3B/G,
         X2B_per_game = X2B/G)

NT  %>% ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
```

![](regression_files/figure-gfm/Baseball%20Questions-1.png)<!-- -->

``` r
NT %>% ggplot(aes(E_per_game, W_per_game)) +
  geom_point(alpha = 0.5)
```

![](regression_files/figure-gfm/Baseball%20Questions-2.png)<!-- -->

``` r
NT %>% ggplot(aes(X2B_per_game, X3B_per_game)) +
  geom_point(alpha = 0.5)
```

![](regression_files/figure-gfm/Baseball%20Questions-3.png)<!-- -->

## Correlation

A historical data set from Galton where he attempted to predict sons’
heights based on fathers’ heights will help us understand the concept of
correlation. Using typical summary statistics like mean and standard
deviation doesn’t inform us well on the relationship between the heights
of fathers and their sons, which is evident in the scatter plot.

``` r
data("GaltonFamilies")

set.seed(1983)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
    summarize(mean(father), sd(father), mean(son), sd(son))
```

    ## # A tibble: 1 x 4
    ##   `mean(father)` `sd(father)` `mean(son)` `sd(son)`
    ##            <dbl>        <dbl>       <dbl>     <dbl>
    ## 1           69.1         2.55        69.2      2.71

``` r
# scatterplot of father and son heights
galton_heights %>%
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5)
```

![](regression_files/figure-gfm/Galton%20heights%20data-1.png)<!-- -->

Instead we can calculate the correlation coefficient which helps us
understand this relationship. Note that we are computing this on a
random sample and so repeated sampling would result in different results
for our correlation estimate. We might think to use Monte Carlo
simulation to assess a possible confidence interval for our estimate of
the correlation. First we use a Q-Q plot to establish whether normality
is sufficient to build our interval. We can see that it isn’t.

``` r
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
```

    ## [1] 0.4334102

``` r
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
    summarize(r = cor(father, son))
R
```

    ## # A tibble: 1 x 1
    ##       r
    ##   <dbl>
    ## 1 0.463

``` r
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
```

![](regression_files/figure-gfm/correlation%20coefficient-1.png)<!-- -->

``` r
# expected value and standard error
mean(R)
```

    ## [1] 0.4291816

``` r
sd(R)
```

    ## [1] 0.1663783

``` r
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
    ggplot(aes(sample = R)) +
    stat_qq() +
    geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))
```

![](regression_files/figure-gfm/correlation%20coefficient-2.png)<!-- -->

``` r
NT <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, 
         R_per_game = R/G, 
         HR_per_game = HR / G, 
         W_per_game = W/G, 
         E_per_game = E/G,
         X3B_per_game = X3B/G,
         X2B_per_game = X2B/G)

NT %>% summarize(r = cor(R_per_game, AB_per_game)) %>% pull(r)
```

    ## [1] 0.6580976

``` r
NT %>% summarize(r = cor(W_per_game, E_per_game)) %>% pull(r)
```

    ## [1] -0.3396947

``` r
NT %>% summarize(r = cor(X2B_per_game, X3B_per_game)) %>% pull(r)
```

    ## [1] -0.01157404

Note that correlation is only useful in a specific context. There are
many possible patterns between pairs of data that can present with the
same correlation. Anscombe’s Quartet is one such example.

Remember our goal is to predict the son’s height based on the father’s
height. A general prediction would be to take the average height of the
sons. If however we want to predict the height of a given son, then we
have more information. We are predicting the son’s height given a
specific height for the father. In this case, the average height of sons
is not a good prediction.

We can stratify the height data and look at the relationship between
father and son heights for each stratum of father heights. If we further
plot the means of the sons heights we will notice that the ‘line of best
fit’ associated with the relationship between stratified father’s
heights and the conditional average height of their sons is equal to the
correlation between sons’ and fathers’ heights.

> Key idea: for every standard deviation increase above the average, our
> prediction grows by the correlation times the standard deviation above
> the predicted average

``` r
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
```

    ## [1] 8

``` r
sum(galton_heights$father == 72.5)
```

    ## [1] 1

``` r
# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
    filter(round(father) == 72) %>%
    summarize(avg = mean(son)) %>%
    pull(avg)
conditional_avg
```

    ## [1] 70.5

``` r
# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
    ggplot(aes(father_strata, son)) +
    geom_boxplot() +
    geom_point()
```

![](regression_files/figure-gfm/Regression%20Line-1.png)<!-- -->

``` r
# center of each boxplot
galton_heights %>%
    mutate(father = round(father)) %>%
    group_by(father) %>%
    summarize(son_conditional_avg = mean(son)) %>%
    ggplot(aes(father, son_conditional_avg)) +
    geom_point()
```

![](regression_files/figure-gfm/Regression%20Line-2.png)<!-- -->

``` r
# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = b, slope = m)
```

![](regression_files/figure-gfm/Regression%20Line-3.png)<!-- -->

Correlation is often motivated by the bivariate normal distribution
which is defined for pairs of random variables. If we believe the pairs
of random variables are independently normal, then the conditional
distribution of one variable given the other is also approximately
normal as well. Hence if we have father’s heights and son’s heights both
being normal, and conditionally on the father’s heights, the son’s
heights are approximately normal, then the bivariate distribution of
father and son heights is also normal.

To validate this assumption we assess whether the son’s heights are
approximately normal conditioning on the stratum of the father’s height.
Furthermore, if we validate that the data is approximately bivariate
then the conditional expectations is given by the regression line.

``` r
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
```

![](regression_files/figure-gfm/Bivariate%20Normal-1.png)<!-- -->

``` r
# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
```

``` r
0.5*3/2
```

    ## [1] 0.75

``` r
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
```

    ## Warning in set.seed(1989, sample.kind = "Rounding"): non-uniform 'Rounding'
    ## sampler used

``` r
female_heights <- GaltonFamilies%>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)


stats <- female_heights %>% summarize_each(funs(mean,sd))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
corr <- female_heights %>% summarize(r = cor(mother,daughter)) %>% pull(r)


# slope daughter from mother
slope <- corr * stats$daughter_sd / stats$mother_sd
slope
```

    ## [1] 0.3393856

``` r
stats$daughter_mean - stats$mother_mean * corr * stats$daughter_sd / stats$mother_sd 
```

    ## [1] 42.51701

``` r
corr**2
```

    ## [1] 0.1053132

``` r
stats$daughter_mean + slope*(60 - stats$mother_mean)
```

    ## [1] 62.88015
