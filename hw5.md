Homework 5
================
Madison Stoms
November 4, 2019

``` r
library(tidyverse)

set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

Problem 1
---------

``` r
#function to fill missing values
fill_na = function(x) {
  
  if (is.numeric(x)) { 
    
    replace_na(x, mean(x, na.rm = TRUE))
    
    }
  
  else if (is.character(x)) { 
    
    replace_na(x, "virginica")
    
  }
  
}

#map function over iris_with_missing
iris_filled = map(iris_with_missing, fill_na)
```

Problem 2
---------

``` r
#function to read in files
read_in = function(x) {
  
  read.csv(str_c("data\\",  x))
  
}


#read in using map and tidy
trial = map_df(list.files("data"), read_in) %>%
  mutate(patient_id = 1:20,
         arm = c(rep("control", 10), rep("experimental", 10))
         ) %>%
  pivot_longer(cols = 1:8,
               names_to = "week", 
               names_prefix = "week_", 
               values_to = "measure")

#spaghetti plot
trial %>%
  ggplot(aes(x = week, y = measure, group = patient_id, color = arm)) +
  geom_line()
```

![](hw5_files/figure-markdown_github/unnamed-chunk-3-1.png)

At week 1, the groups preformed approximately equal, but separated as time progessed. All members of the experimental group preformed better than all members of the control group at week 8. Therefore, the intervention was effective in increasing the outcome of interest.

Problem 3
---------

``` r
#fix values
n = 30
b_0 = 2
sig_2 = 50
x = rnorm(n)

#define model and extract estimate and p-value
model_generate = function(n , b_0, b_1, sig_2, x ) {

  e = rnorm(n, sd = sig_2)
  y = b_0 + b_1 * x + e
  results = lm(y~x)
  broom::tidy(results)[2, c(2, 5)]
  
}

#generate 1000 models, test, and bind results
sim_results = function(n, b_0, b_1, sig_2, x) {
  
  rerun(1000, model_generate(n, b_0, b_1, sig_2, x)) %>%
  bind_rows()
  
}


#results for B0 = 0
sim_results(n, b_0, b_1 = 0, sig_2, x) %>%
  bind_rows() %>%
  head()
```

    ## # A tibble: 6 x 2
    ##   estimate p.value
    ##      <dbl>   <dbl>
    ## 1    14.3   0.207 
    ## 2    15.3   0.208 
    ## 3     3.08  0.720 
    ## 4    19.7   0.144 
    ## 5    20.6   0.0761
    ## 6   -12.1   0.391

``` r
#results for B0 in 1:6
results = 
  tibble(b_1 = 1:6) %>%
  mutate(
    output_lists = map(.x = b_1, 
                       ~rerun(1000, model_generate(
                         n, b_0, b_1 = .x, sig_2, x))),
    estimate_dfs = map(output_lists, bind_rows) 
  ) %>%
  select(-output_lists) %>%
  unnest(estimate_dfs) %>%
  as.data.frame() 

#power plot
results %>%
  mutate(rejected = ifelse(p.value < 0.05, 1, 0)) %>%
  group_by(b_1) %>%
  summarise(prop = mean(rejected)) %>%
  ggplot(aes(x = b_1, y = prop)) +
  geom_col(width = .5) +
  xlab("True Slope Parameter") +
  ylab("Proportion of Significant Tests") 
```

![](hw5_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
#average plot
sig = results %>%
  filter(p.value < 0.05) %>%
  group_by(b_1) %>%
  summarise(estimate_avg = mean(estimate)) 

results %>%
  group_by(b_1) %>%
  summarise(estimate_avg = mean(estimate)) %>%
  ggplot(aes(x = b_1, y = estimate_avg)) +
  geom_line(aes(color = "black")) +
  geom_line(data = sig,
            aes(color = "red")) +
  scale_color_manual(name = "Results", 
                     values = c("black", "red"),
                     labels = c("Total", "Rejected")) +
  xlab("Beta 1") +
  ylab("Average Estiamated Beta 1")
```

![](hw5_files/figure-markdown_github/unnamed-chunk-4-2.png)

The two lines are different because the rejected samples contain estimated values that were significantly greater than our null value of 0. Therefore, the average estimated value among rejected samples is larger than the average among all samples.
