Homework 6
================
Madison Stoms
November 4, 2019

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

Problem 1
---------

``` r
#input is a vector
fill_na = function(x) {
  
  if (is.numeric(x)) { 
    
    replace_na(x, mean(x, na.rm = TRUE))
    
    }
  
  else if (is.character(x)) { 
    
    replace_na(x, "virginica")
    
  }
  
}

iris_filled = map(iris_with_missing, fill_na)
```
