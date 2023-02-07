ARU cardinal transmission
================
Sunny Tseng
2023-02-07

## Library

``` r
library(here)
library(tidyverse)
library(lme4)
library(betareg)
```

## Import data

``` r
data <- read_csv(here("cardinal_playback.csv"))
```

``` r
# have a brief check for the imported data. 
data
```

    ## # A tibble: 1,626 × 10
    ##    date       time   location beaufort ARU   plot_type direction dista…¹ song_…²
    ##    <date>     <time> <chr>       <dbl> <chr> <chr>     <chr>       <dbl>   <dbl>
    ##  1 2022-05-01 08:00  PB              0 PB    PB        N               0       1
    ##  2 2022-05-01 08:00  PB              0 PB    PB        N               0       2
    ##  3 2022-05-01 08:00  PB              0 PB    PB        N               0       3
    ##  4 2022-05-01 08:00  PB              0 PB    PB        N               0       4
    ##  5 2022-05-01 08:00  PB              0 PB    PB        N               0       6
    ##  6 2022-05-01 08:00  PB              0 PB    PB        N               0       7
    ##  7 2022-06-14 11:48  14_35           0 NEW   F         E              50       1
    ##  8 2022-06-14 11:48  14_35           0 NEW   F         E              50       2
    ##  9 2022-06-14 11:48  14_35           0 NEW   F         E              50       3
    ## 10 2022-06-14 11:48  14_35           0 NEW   F         E              50       4
    ## # … with 1,616 more rows, 1 more variable: confidence <dbl>, and abbreviated
    ## #   variable names ¹​distance, ²​song_exp

``` r
# filter out non-necessary data and standardize the object type.
# change the direction, as E & W should be marked as Side
data_clean <- data %>%
  filter(distance != 0) %>%
  mutate(beaufort = as_factor(beaufort),
         distance = as_factor(distance),
         song_exp = as_factor(song_exp)) %>% 
  mutate(direction = if_else(direction == "E" | direction == "W", "Side", direction)) %>%
  mutate(confidence = if_else(confidence == 0, 0.00000001, confidence))
data_clean
```

    ## # A tibble: 1,620 × 10
    ##    date       time   location beaufort ARU   plot_type direction dista…¹ song_…²
    ##    <date>     <time> <chr>    <fct>    <chr> <chr>     <chr>     <fct>   <fct>  
    ##  1 2022-06-14 11:48  14_35    0        NEW   F         Side      50      1      
    ##  2 2022-06-14 11:48  14_35    0        NEW   F         Side      50      2      
    ##  3 2022-06-14 11:48  14_35    0        NEW   F         Side      50      3      
    ##  4 2022-06-14 11:48  14_35    0        NEW   F         Side      50      4      
    ##  5 2022-06-14 11:48  14_35    0        NEW   F         Side      50      6      
    ##  6 2022-06-14 11:48  14_35    0        NEW   F         Side      50      7      
    ##  7 2022-06-14 11:48  14_35    0        NEW   F         Side      75      1      
    ##  8 2022-06-14 11:48  14_35    0        NEW   F         Side      75      2      
    ##  9 2022-06-14 11:48  14_35    0        NEW   F         Side      75      3      
    ## 10 2022-06-14 11:48  14_35    0        NEW   F         Side      75      4      
    ## # … with 1,610 more rows, 1 more variable: confidence <dbl>, and abbreviated
    ## #   variable names ¹​distance, ²​song_exp

## Data visualization

#### Email from Ken:

I was simply trying to run a boxplot to look at the data with the three
distances (50, 75 & 100) on the X axes, and Detection Confidence on the
Y axes, with the Forest, Control and Partial Cut treatments as separate
boxes. I was then trying to insert a line on top of each so I could see
how confidence decreased with distance, and also be able to see how the
forest treatment affected overall confidence at each distance. The
second ggplot was the closest I could come to what I was trying to
achieve, but it is a really weird way to have to get the visual! Maybe
distance has to be a factor for this to work instead of an integer? Just
not sure that would have the X axes spread out in a proper linear
fashion.

#### Reply:

The variable “distance” has to be either character or factor when making
the boxplot as it will give the ggplot the hint about how to put the
data on the x-axis. However, you will need to transfer the “distance”
back to numeric just for plotting out the regression line between the
confidence median versus distance.

``` r
data_clean_med <- data_clean %>% 
  group_by(distance, plot_type) %>%
  summarize(median_confidence = median(confidence)) %>%
  mutate(distance = as.numeric(distance)) 
# transforming the distance to numeric in order to fit the lm, 
# since 50, 75 and 100 are with the same interval so it's okay in this case. 
# another method (less flexible): 
# lm(median_confidence ~ distance %>% as.numeric(), data = .) %>% 
# coef()

aru_distance <- data_clean %>%
  ggplot() +
    geom_boxplot(aes(x = distance, y = confidence)) +
    geom_smooth(aes(x = distance, y = median_confidence), method = "lm", se = FALSE, data = data_clean_med) +
    facet_grid(rows = vars(plot_type)) +
    theme_bw()
    
aru_distance
```

![](aru_transmission_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Model formulation

#### Email from Ken:

The second part is running the model. I tried a general linear mixed
effect with comparing distance, direction, habitat (plot_type) and age
of ARU against detection confidence, while controlling for windspeed
during playback (beaufort) and replicate song types (song_exp). However,
the data is really skewed, so I am not convinced that the lmer is the
right model. I thought maybe a generalized linear model, but then not
sure which family this would be (binomial?). In any case, distance,
habitat and direction are all coming out as influential, and it looks
like the interaction between distance and habitat is also important.

#### Reply:

As mentioned in your text, “song_exp” is the replicates, so that we
don’t put it in the model. As suggested in this [online
discussion](https://www.researchgate.net/post/Which-type-of-regression-would-be-appropriate-when-the-dependent-variable-is-continuous-and-bounded-between-0-and-1),
a beta regression might be a good fit in the case when y variable ranges
from 0 to 1.

> The class of beta regression models, as introduced by Ferrari and
> Cribari-Neto (2004), is useful for modeling continuous variables y
> that assume values in the open standard unit interval (0, 1).

Then I would test out each of the factor one by one, then gradually add
significant variables, then test the interactions. Here are some
documents to help understanding Beta analysis in R:

- [Beta Regression in
  R](https://cran.r-project.org/web/packages/betareg/vignettes/betareg.pdf)
- [Extended beta regression in R: shaken, stirred, mixed, and
  partitioned](https://ro.uow.edu.au/buspapers/158/)

``` r
# test out each individual factor that might influence the confidence

## Factors
# ARU (2): NEW, OLD
# plot_type (3): F, C, P
# direction (3): Side, N, S
# distance (3): 50, 75, 100

## Replicates
# song_exp (7)


# here are the examples of fitting beta regression using single variable.
# I will need to study more about beta regression but the result interpretation
# is very similar to the linear model, where you look for significance value to 
# see whether the variable is contributing to the model.
conf_ARU <- betareg(confidence ~ ARU , data = data_clean) 
summary(conf_ARU)
```

    ## 
    ## Call:
    ## betareg(formula = confidence ~ ARU, data = data_clean)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5747 -1.5279  0.4875  0.7741  1.3439 
    ## 
    ## Coefficients (mean model with logit link):
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.69492    0.05305 -31.952   <2e-16 ***
    ## ARUOLD      -0.02235    0.05669  -0.394    0.693    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  0.88614    0.03653   24.25   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6498 on 3 Df
    ## Pseudo R-squared: 8.994e-05
    ## Number of iterations: 16 (BFGS) + 1 (Fisher scoring)

``` r
conf_ARU <- betareg(confidence ~ ARU , data = data_clean) 
summary(conf_ARU)
```

    ## 
    ## Call:
    ## betareg(formula = confidence ~ ARU, data = data_clean)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5747 -1.5279  0.4875  0.7741  1.3439 
    ## 
    ## Coefficients (mean model with logit link):
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.69492    0.05305 -31.952   <2e-16 ***
    ## ARUOLD      -0.02235    0.05669  -0.394    0.693    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  0.88614    0.03653   24.25   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6498 on 3 Df
    ## Pseudo R-squared: 8.994e-05
    ## Number of iterations: 16 (BFGS) + 1 (Fisher scoring)
