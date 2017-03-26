OPIM 5671 Assignment 3 Workings
================

-   [Import libraries](#import-libraries)
-   [Import data](#import-data)
-   [Feature engineering](#feature-engineering)
    -   [Month](#month)
-   [Partitions](#partitions)
-   [Visualizations](#visualizations)
    -   [Data Preparation](#data-preparation)
    -   [Trend/Seasonality](#trendseasonality)
    -   [Cross Correlation Function](#cross-correlation-function)
-   [Modelling](#modelling)
    -   [Lagged regression](#lagged-regression)
    -   [Modelling the residuals](#modelling-the-residuals)
-   [Input to SAS script](#input-to-sas-script)

Import libraries
================

``` r
library(magrittr)
```

Import data
===========

``` r
library(readr)
train.df <-
  read_csv(
    "./Data/train.csv",
    col_types = cols(
      chris = col_logical(),
      customers = col_double(),
      date = col_character(),
      easter = col_logical(),
      open = col_logical(),
      ph = col_logical(),
      prom = col_logical(),
      sch = col_logical()
    )
  )
summary(train.df)
```

    ##      date              sales             customers         open        
    ##  Length:942         Length:942         Min.   :   0.0   Mode :logical  
    ##  Class :character   Class :character   1st Qu.: 669.2   FALSE:156      
    ##  Mode  :character   Mode  :character   Median : 893.0   TRUE :786      
    ##                                        Mean   : 791.5   NA's :0        
    ##                                        3rd Qu.:1059.0                  
    ##                                        Max.   :1691.0                  
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:582       FALSE:783       FALSE:938       FALSE:936      
    ##  TRUE :360       TRUE :159       TRUE :4         TRUE :6        
    ##  NA's :0         NA's :0         NA's :0         NA's :0        
    ##                                                                 
    ##                                                                 
    ##      ph         
    ##  Mode :logical  
    ##  FALSE:928      
    ##  TRUE :14       
    ##  NA's :0        
    ##                 
    ## 

Fix the date column:

``` r
train.df %<>%
  dplyr::mutate(date = lubridate::ymd(date))
```

Fix the sales column:

``` r
train.df %<>%
  dplyr::mutate(sales = as.numeric(sales))
```

    ## Warning in eval(substitute(expr), envir, enclos): NAs introduced by
    ## coercion

``` r
summary(train.df)
```

    ##       date                sales         customers         open        
    ##  Min.   :2013-01-01   Min.   :    0   Min.   :   0.0   Mode :logical  
    ##  1st Qu.:2013-08-24   1st Qu.: 5882   1st Qu.: 669.2   FALSE:156      
    ##  Median :2014-04-16   Median : 7861   Median : 893.0   TRUE :786      
    ##  Mean   :2014-04-16   Mean   : 7305   Mean   : 791.5   NA's :0        
    ##  3rd Qu.:2014-12-07   3rd Qu.: 9700   3rd Qu.:1059.0                  
    ##  Max.   :2015-07-31   Max.   :18413   Max.   :1691.0                  
    ##                       NA's   :150                                     
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:582       FALSE:783       FALSE:938       FALSE:936      
    ##  TRUE :360       TRUE :159       TRUE :4         TRUE :6        
    ##  NA's :0         NA's :0         NA's :0         NA's :0        
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##      ph         
    ##  Mode :logical  
    ##  FALSE:928      
    ##  TRUE :14       
    ##  NA's :0        
    ##                 
    ##                 
    ## 

Feature engineering
===================

Month
-----

Add a month column (caret's `dummyVars` is broken):

``` r
tmp.df <- train.df %>%
  dplyr::mutate(month = as.factor(lubridate::month(date, label = T))) %>%
  dplyr::select(month)

months <- levels(tmp.df$month)

dummy.months <- purrr::map(months, function(m) {
  as.numeric(tmp.df == m)
}) %>%
  purrr::invoke(cbind, .) %>%
  as.data.frame() %>%
  magrittr::set_colnames(paste0("is", months))

train.df %<>%
  dplyr::bind_cols(dummy.months) %>%
  dplyr::select(-isNov, -isDec)
```

Partitions
==========

Create the test set:

``` r
test.df <- train.df %>%
  dplyr::slice(which(is.na(sales)))

summary(test.df)
```

    ##       date                sales       customers         open        
    ##  Min.   :2015-03-04   Min.   : NA   Min.   :   0.0   Mode :logical  
    ##  1st Qu.:2015-04-10   1st Qu.: NA   1st Qu.: 693.2   FALSE:26       
    ##  Median :2015-05-17   Median : NA   Median : 904.0   TRUE :124      
    ##  Mean   :2015-05-17   Mean   :NaN   Mean   : 792.1   NA's :0        
    ##  3rd Qu.:2015-06-23   3rd Qu.: NA   3rd Qu.:1061.2                  
    ##  Max.   :2015-07-31   Max.   : NA   Max.   :1414.0                  
    ##                       NA's   :150                                   
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:92        FALSE:126       FALSE:150       FALSE:148      
    ##  TRUE :58        TRUE :24        NA's :0         TRUE :2        
    ##  NA's :0         NA's :0                         NA's :0        
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##      ph              isJan       isFeb       isMar            isApr    
    ##  Mode :logical   Min.   :0   Min.   :0   Min.   :0.0000   Min.   :0.0  
    ##  FALSE:147       1st Qu.:0   1st Qu.:0   1st Qu.:0.0000   1st Qu.:0.0  
    ##  TRUE :3         Median :0   Median :0   Median :0.0000   Median :0.0  
    ##  NA's :0         Mean   :0   Mean   :0   Mean   :0.1867   Mean   :0.2  
    ##                  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0.0000   3rd Qu.:0.0  
    ##                  Max.   :0   Max.   :0   Max.   :1.0000   Max.   :1.0  
    ##                                                                        
    ##      isMay            isJun         isJul            isAug       isSep  
    ##  Min.   :0.0000   Min.   :0.0   Min.   :0.0000   Min.   :0   Min.   :0  
    ##  1st Qu.:0.0000   1st Qu.:0.0   1st Qu.:0.0000   1st Qu.:0   1st Qu.:0  
    ##  Median :0.0000   Median :0.0   Median :0.0000   Median :0   Median :0  
    ##  Mean   :0.2067   Mean   :0.2   Mean   :0.2067   Mean   :0   Mean   :0  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0   3rd Qu.:0.0000   3rd Qu.:0   3rd Qu.:0  
    ##  Max.   :1.0000   Max.   :1.0   Max.   :1.0000   Max.   :0   Max.   :0  
    ##                                                                         
    ##      isOct  
    ##  Min.   :0  
    ##  1st Qu.:0  
    ##  Median :0  
    ##  Mean   :0  
    ##  3rd Qu.:0  
    ##  Max.   :0  
    ## 

Set the size of the valivation set:

``` r
valid.size <- 150
```

Crate the validation set:

``` r
valid.df <- train.df %>%
  dplyr::setdiff(test.df) %>%
  tail(valid.size)

summary(valid.df)  
```

    ##       date                sales         customers       open        
    ##  Min.   :2014-10-05   Min.   :    0   Min.   :   0   Mode :logical  
    ##  1st Qu.:2014-11-11   1st Qu.: 6101   1st Qu.: 651   FALSE:25       
    ##  Median :2014-12-18   Median : 7908   Median : 874   TRUE :125      
    ##  Mean   :2014-12-18   Mean   : 7449   Mean   : 763   NA's :0        
    ##  3rd Qu.:2015-01-24   3rd Qu.:10049   3rd Qu.:1018                  
    ##  Max.   :2015-03-03   Max.   :16763   Max.   :1364                  
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:88        FALSE:128       FALSE:148       FALSE:150      
    ##  TRUE :62        TRUE :22        TRUE :2         NA's :0        
    ##  NA's :0         NA's :0         NA's :0                        
    ##                                                                 
    ##                                                                 
    ##      ph              isJan            isFeb            isMar     
    ##  Mode :logical   Min.   :0.0000   Min.   :0.0000   Min.   :0.00  
    ##  FALSE:149       1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00  
    ##  TRUE :1         Median :0.0000   Median :0.0000   Median :0.00  
    ##  NA's :0         Mean   :0.2067   Mean   :0.1867   Mean   :0.02  
    ##                  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00  
    ##                  Max.   :1.0000   Max.   :1.0000   Max.   :1.00  
    ##      isApr       isMay       isJun       isJul       isAug       isSep  
    ##  Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
    ##  1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
    ##  Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
    ##  Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
    ##  3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
    ##  Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
    ##      isOct     
    ##  Min.   :0.00  
    ##  1st Qu.:0.00  
    ##  Median :0.00  
    ##  Mean   :0.18  
    ##  3rd Qu.:0.00  
    ##  Max.   :1.00

Fix the test training set:

``` r
train.df %<>%
  dplyr::setdiff(valid.df) %>%
  dplyr::setdiff(test.df)

summary(train.df)
```

    ##       date                sales         customers         open        
    ##  Min.   :2013-01-01   Min.   :    0   Min.   :   0.0   Mode :logical  
    ##  1st Qu.:2013-06-10   1st Qu.: 5814   1st Qu.: 673.2   FALSE:105      
    ##  Median :2013-11-17   Median : 7856   Median : 893.0   TRUE :537      
    ##  Mean   :2013-11-17   Mean   : 7272   Mean   : 798.0   NA's :0        
    ##  3rd Qu.:2014-04-26   3rd Qu.: 9605   3rd Qu.:1070.8                  
    ##  Max.   :2014-10-04   Max.   :18413   Max.   :1691.0                  
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:402       FALSE:529       FALSE:640       FALSE:638      
    ##  TRUE :240       TRUE :113       TRUE :2         TRUE :4        
    ##  NA's :0         NA's :0         NA's :0         NA's :0        
    ##                                                                 
    ##                                                                 
    ##      ph              isJan             isFeb             isMar        
    ##  Mode :logical   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  FALSE:632       1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  TRUE :10        Median :0.00000   Median :0.00000   Median :0.00000  
    ##  NA's :0         Mean   :0.09657   Mean   :0.08723   Mean   :0.09657  
    ##                  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##                  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##      isApr             isMay             isJun             isJul        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.09346   Mean   :0.09657   Mean   :0.09346   Mean   :0.09657  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##      isAug             isSep             isOct        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.09657   Mean   :0.09346   Mean   :0.05452  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000

Visualizations
==============

Data Preparation
----------------

Consolidate the training and validation set:

``` r
consol.df <- train.df %>%
  dplyr::bind_rows(valid.df)

summary(consol.df)
```

    ##       date                sales         customers         open        
    ##  Min.   :2013-01-01   Min.   :    0   Min.   :   0.0   Mode :logical  
    ##  1st Qu.:2013-07-17   1st Qu.: 5882   1st Qu.: 658.2   FALSE:130      
    ##  Median :2014-01-31   Median : 7861   Median : 891.5   TRUE :662      
    ##  Mean   :2014-01-31   Mean   : 7305   Mean   : 791.4   NA's :0        
    ##  3rd Qu.:2014-08-17   3rd Qu.: 9700   3rd Qu.:1058.2                  
    ##  Max.   :2015-03-03   Max.   :18413   Max.   :1691.0                  
    ##     prom            sch            chris           easter       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:490       FALSE:657       FALSE:788       FALSE:788      
    ##  TRUE :302       TRUE :135       TRUE :4         TRUE :4        
    ##  NA's :0         NA's :0         NA's :0         NA's :0        
    ##                                                                 
    ##                                                                 
    ##      ph              isJan            isFeb            isMar        
    ##  Mode :logical   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  FALSE:781       1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  TRUE :11        Median :0.0000   Median :0.0000   Median :0.00000  
    ##  NA's :0         Mean   :0.1174   Mean   :0.1061   Mean   :0.08207  
    ##                  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
    ##                  Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
    ##      isApr             isMay             isJun             isJul        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.07576   Mean   :0.07828   Mean   :0.07576   Mean   :0.07828  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##      isAug             isSep             isOct        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.07828   Mean   :0.07576   Mean   :0.07828  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000

Convert the consolidated data frame into a time series object:

``` r
consol.ts <-
  purrr::map(consol.df[, -1], function(x)
  ts(zoo::zoo(x, order.by = consol.df$date))) %>%
  purrr::invoke(ts.intersect, .)
  
summary(consol.ts)
```

    ##      sales         customers           open             prom       
    ##  Min.   :    0   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.: 5882   1st Qu.: 658.2   1st Qu.:1.0000   1st Qu.:0.0000  
    ##  Median : 7861   Median : 891.5   Median :1.0000   Median :0.0000  
    ##  Mean   : 7305   Mean   : 791.4   Mean   :0.8359   Mean   :0.3813  
    ##  3rd Qu.: 9700   3rd Qu.:1058.2   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :18413   Max.   :1691.0   Max.   :1.0000   Max.   :1.0000  
    ##       sch             chris              easter               ph         
    ##  Min.   :0.0000   Min.   :0.000000   Min.   :0.000000   Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.000000   Median :0.000000   Median :0.00000  
    ##  Mean   :0.1705   Mean   :0.005051   Mean   :0.005051   Mean   :0.01389  
    ##  3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.000000   Max.   :1.000000   Max.   :1.00000  
    ##      isJan            isFeb            isMar             isApr        
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.0000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.1174   Mean   :0.1061   Mean   :0.08207   Mean   :0.07576  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000  
    ##      isMay             isJun             isJul             isAug        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.07828   Mean   :0.07576   Mean   :0.07828   Mean   :0.07828  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##      isSep             isOct        
    ##  Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000  
    ##  Mean   :0.07576   Mean   :0.07828  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000

Trend/Seasonality
-----------------

Plot sales:

``` r
plot(x = consol.df$date, y = consol.ts[, "sales"], type = "b")
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-10-1.png)

Sales by month:

``` r
consol.df %>%
  dplyr::mutate(month = lubridate::month(date, T),
                year = lubridate::year(date)) %>%
  dplyr::group_by(month, year) %>%
  dplyr::summarise(avg.sales = mean(sales)) %>%
  tidyr::spread(year, avg.sales)
```

    ## Source: local data frame [12 x 4]
    ## Groups: month [12]
    ## 
    ##    month   `2013`   `2014`   `2015`
    ## *  <ord>    <dbl>    <dbl>    <dbl>
    ## 1    Jan 6065.548 6472.839 6861.871
    ## 2    Feb 6386.786 6905.071 7108.107
    ## 3    Mar 6873.613 6591.258 7597.000
    ## 4    Apr 6579.367 7379.800       NA
    ## 5    May 6803.903 6988.065       NA
    ## 6    Jun 7059.033 7365.033       NA
    ## 7    Jul 8912.258 8623.710       NA
    ## 8    Aug 8334.387 8644.032       NA
    ## 9    Sep 6712.600 7708.467       NA
    ## 10   Oct 6702.806 7059.645       NA
    ## 11   Nov 7272.500 7640.200       NA
    ## 12   Dec 8126.710 8563.548       NA

Cross Correlation Function
--------------------------

Plot the ccf of sales against all the other variables:

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "customers"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "open"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "prom"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "sch"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "chris"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "easter"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
TSA::prewhiten(consol.ts[, "sales"], consol.ts[, "ph"])
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-18-1.png) \#\# Lagged plots

Plot of sales at various lags:

``` r
astsa::lag1.plot(consol.ts[,"sales"], 21)
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-19-1.png)

Modelling
=========

Lagged regression
-----------------

Prepare the data set:

``` r
alldata <-
  ts.intersect(
  consol.ts,
  saleslag7 = lag(consol.ts[, "sales"], -7),
  saleslag14 = lag(consol.ts[, "sales"], -14),
  saleslag1 = lag(consol.ts[, "open"],-4),
  openlag2 = lag(consol.ts[, "open"], -2),
  openlag5 = lag(consol.ts[, "open"], -5),
  promlag1 = lag(consol.ts[, "prom"], -1),
  schlag1 = lag(consol.ts[, "sch"], -1),
  easterlag1 = lag(consol.ts[, "easter"], -2)
  )
  
  dimnames(alldata)[[2]] <- dimnames(alldata)[[2]] %>%
  stringr::str_replace("consol.ts.(.*)", "\\1")
  
  n <- nrow(alldata)
  
  alldata.train <- ts(alldata[1:(n - valid.size),])
  alldata.valid <- ts(alldata[(n - valid.size + 1):n,])
```

Build the model:

``` r
fit <- lm(log(sales) ~ ., data = alldata.train)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = log(sales) ~ ., data = alldata.train)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.256255 -0.028120 -0.002953  0.027631  0.180882 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error   t value Pr(>|t|)    
    ## (Intercept) -2.077e+01  1.583e-02 -1311.430  < 2e-16 ***
    ## customers    1.129e-03  1.799e-05    62.754  < 2e-16 ***
    ## open         2.864e+01  1.668e-02  1717.653  < 2e-16 ***
    ## prom         1.210e-01  8.733e-03    13.855  < 2e-16 ***
    ## sch         -1.819e-02  9.473e-03    -1.920 0.055269 .  
    ## chris       -7.028e-02  4.175e-02    -1.683 0.092832 .  
    ## easter      -2.553e-02  3.203e-02    -0.797 0.425690    
    ## ph          -1.154e-01  2.202e-02    -5.240 2.22e-07 ***
    ## isJan       -4.431e-02  1.046e-02    -4.237 2.62e-05 ***
    ## isFeb       -5.916e-02  9.940e-03    -5.951 4.51e-09 ***
    ## isMar       -5.773e-02  9.727e-03    -5.935 4.96e-09 ***
    ## isApr       -6.605e-02  1.001e-02    -6.602 8.93e-11 ***
    ## isMay       -5.809e-02  9.752e-03    -5.957 4.38e-09 ***
    ## isJun       -5.517e-02  9.672e-03    -5.704 1.84e-08 ***
    ## isJul       -8.910e-02  1.039e-02    -8.574  < 2e-16 ***
    ## isAug       -1.110e-01  1.009e-02   -11.009  < 2e-16 ***
    ## isSep       -7.649e-02  9.668e-03    -7.912 1.22e-14 ***
    ## isOct       -7.972e-02  1.159e-02    -6.876 1.55e-11 ***
    ## saleslag7   -3.304e-06  1.035e-06    -3.191 0.001490 ** 
    ## saleslag14   4.025e-06  1.042e-06     3.862 0.000124 ***
    ## saleslag1    2.149e-02  6.116e-03     3.514 0.000474 ***
    ## openlag2     4.516e-02  6.760e-03     6.682 5.39e-11 ***
    ## openlag5     4.501e-02  6.304e-03     7.141 2.69e-12 ***
    ## promlag1    -2.992e-02  7.054e-03    -4.242 2.56e-05 ***
    ## schlag1      3.700e-02  8.815e-03     4.197 3.11e-05 ***
    ## easterlag1   5.044e-02  2.798e-02     1.803 0.071952 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05254 on 602 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:      1 
    ## F-statistic: 1.097e+06 on 25 and 602 DF,  p-value: < 2.2e-16

View the residuals:

``` r
astsa::acf2(residuals(fit))
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-22-1.png)

    ##         ACF  PACF
    ##  [1,]  0.12  0.12
    ##  [2,]  0.08  0.07
    ##  [3,]  0.02  0.00
    ##  [4,]  0.07  0.07
    ##  [5,]  0.00 -0.01
    ##  [6,]  0.03  0.02
    ##  [7,]  0.12  0.12
    ##  [8,]  0.01 -0.03
    ##  [9,]  0.00 -0.01
    ## [10,]  0.05  0.05
    ## [11,] -0.01 -0.04
    ## [12,] -0.07 -0.07
    ## [13,] -0.03 -0.01
    ## [14,]  0.12  0.12
    ## [15,] -0.02 -0.04
    ## [16,] -0.06 -0.07
    ## [17,] -0.06 -0.06
    ## [18,] -0.05 -0.04
    ## [19,] -0.09 -0.06
    ## [20,] -0.08 -0.05
    ## [21,]  0.09  0.11
    ## [22,] -0.03 -0.02
    ## [23,] -0.02 -0.01
    ## [24,] -0.01  0.00
    ## [25,]  0.02  0.03
    ## [26,]  0.01  0.05
    ## [27,] -0.02 -0.02
    ## [28,]  0.12  0.08
    ## [29,]  0.01 -0.01
    ## [30,]  0.05  0.05
    ## [31,]  0.04  0.03
    ## [32,]  0.02 -0.02
    ## [33,] -0.07 -0.06
    ## [34,] -0.06 -0.05
    ## [35,]  0.02 -0.04
    ## [36,] -0.01 -0.02

``` r
plot(fit)
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-23-1.png)![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-23-2.png)![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-23-3.png)![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-23-4.png) Make predictions on the validation set:

``` r
preds <- predict(fit, alldata.valid) %>%
  exp
  
tgt.cols <- c("date", "sales", "pred.sales", "err.squared")

rest.cols <- setdiff(names(valid.df), tgt.cols)

preds.df <- data.frame(valid.df) %>%
  dplyr::mutate(pred.sales = preds) %>%
  dplyr::mutate(err.squared = (sales - pred.sales) ^ 2) %>%
  dplyr::select(date, sales, pred.sales, err.squared,  match(rest.cols, names(valid.df))) %>%
  dplyr::rename(actual.sales = sales)
```

Compute the RMSE:

``` r
preds.df %>%
  dplyr::select(err.squared) %>%
  {sqrt(sum(.)/n)}
```

    ## [1] 284.89

If we manually update the predictions so that sales is 0 whenever the shop is closed...

``` r
preds.df %>%
  dplyr::mutate(pred.sales = dplyr::if_else(open == F, 0, pred.sales)) %>%
  dplyr::mutate(err.squared = (actual.sales - pred.sales) ^ 2) %>%
  dplyr::select(err.squared) %>%
  {
  sqrt(sum(.) / n)
  }
```

    ## [1] 284.89

Modelling the residuals
-----------------------

``` r
arima.fit <- arima(log(alldata.train[, "sales"]),
                   xreg = alldata.train[, -1],
                   order = c(1, 1, 1),
                   seasonal = list(order = c(0, 0, 1), period = 7))

preds <- predict(arima.fit, newxreg = alldata.valid[, -1])


arima.pred.df <- data.frame(valid.df) %>%
  dplyr::mutate(pred.sales = exp(preds$pred)) %>%
  dplyr::mutate(err.squared = (sales - pred.sales) ^ 2) %>%
  dplyr::select(date, sales, pred.sales, err.squared,  match(rest.cols, names(valid.df))) %>%
  dplyr::rename(actual.sales = sales)
```

``` r
astsa::acf2(arima.fit$residuals)
```

![](Assignment_3_workings_files/figure-markdown_github/unnamed-chunk-28-1.png)

    ##         ACF  PACF
    ##  [1,] -0.01 -0.01
    ##  [2,]  0.06  0.06
    ##  [3,] -0.02 -0.02
    ##  [4,]  0.06  0.06
    ##  [5,] -0.02 -0.01
    ##  [6,]  0.01  0.00
    ##  [7,]  0.01  0.01
    ##  [8,] -0.02 -0.02
    ##  [9,] -0.02 -0.02
    ## [10,]  0.05  0.05
    ## [11,] -0.02 -0.02
    ## [12,] -0.07 -0.07
    ## [13,] -0.03 -0.03
    ## [14,]  0.12  0.12
    ## [15,] -0.02 -0.02
    ## [16,] -0.06 -0.07
    ## [17,] -0.06 -0.06
    ## [18,] -0.04 -0.05
    ## [19,] -0.09 -0.08
    ## [20,] -0.08 -0.08
    ## [21,]  0.08  0.09
    ## [22,] -0.04 -0.02
    ## [23,] -0.03 -0.03
    ## [24,] -0.02 -0.03
    ## [25,]  0.02  0.01
    ## [26,]  0.01  0.05
    ## [27,] -0.04 -0.04
    ## [28,]  0.11  0.08
    ## [29,] -0.02 -0.01
    ## [30,]  0.03  0.03
    ## [31,]  0.03  0.02
    ## [32,]  0.01 -0.02
    ## [33,] -0.08 -0.06
    ## [34,] -0.06 -0.06
    ## [35,]  0.00 -0.04
    ## [36,] -0.02 -0.03

``` r
arima.pred.df %>%
  dplyr::select(err.squared) %>%
  {sqrt(sum(.)/n)}
```

    ## [1] 250.7524

Retrain model on training and validationset and predict on test set:

``` r
consol.df <- train.df %>%
  dplyr::bind_rows(valid.df) %>%
  dplyr::bind_rows(test.df)

consol.ts <-
  purrr::map(consol.df[,-1], function(x)
  ts(zoo::zoo(x, order.by = consol.df$date))) %>%
  purrr::invoke(ts.intersect, .)

alldata <-
  ts.intersect(
  consol.ts,
  saleslag7 = lag(consol.ts[, "sales"],-7),
  saleslag14 = lag(consol.ts[, "sales"],-14),
  saleslag1 = lag(consol.ts[, "open"], -4),
  openlag2 = lag(consol.ts[, "open"],-2),
  openlag5 = lag(consol.ts[, "open"],-5),
  promlag1 = lag(consol.ts[, "prom"],-1),
  schlag1 = lag(consol.ts[, "sch"],-1),
  easterlag1 = lag(consol.ts[, "easter"],-2)
  )
  
dimnames(alldata)[[2]] <- dimnames(alldata)[[2]] %>%
  stringr::str_replace("consol.ts.(.*)", "\\1")
  
n <- nrow(alldata)
  
alldata.train <- ts(alldata[1:(n - 150),])
alldata.test <- ts(alldata[(n - 150 + 1):n,])
  
model2.fit <- arima(
  log(alldata.train[, "sales"]),
  xreg = alldata.train[, -1],
  order = c(1, 1, 1),
  seasonal = list(order = c(0, 0, 1), period = 7)
  )
  
preds <- predict(model2.fit, newxreg = alldata.test[, -1])$pred %>%
  exp

summary(preds)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0    6630    7708    6838    8522    9858     143

Abandon this method because `predict` does not work with lagged predictors ¯\_(ツ)\_/¯ .

Input to SAS script
===================

Convert TRUE/FALSE to 0/1:

``` r
train.df %>%
  dplyr::mutate(
                open = as.numeric(open),
                prom = as.numeric(prom),
                sch = as.numeric(sch),
                chris = as.numeric(chris),
                easter = as.numeric(easter),
                ph = as.numeric(ph)) ->
  train.df
```

Save file:

``` r
write.csv(train.df, "./Data/train_mod.csv", row.names = F)
```
