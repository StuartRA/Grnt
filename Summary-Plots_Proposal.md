---
title: "Plots to be included in the JHSPH Graduate Exit Survey: Proposal"
date: "September 2, 2019"
output: 
  html_document:
    code_folding: hide
    keep_md: true
    df_print: paged
    theme: cosmo
    toc: yes
    toc_float: yes
  word_document:
    toc: yes
  pdf_document: default
  html:
    latex_engine: xelatex
header-includes: \usepackage{fontspec}
---




```r
library(ggplot2)
library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(gridExtra)
library(extrafont)
library(reshape2)
library(scales)
library(hash)
library(treemap)

# We source main_masters script to use get.tables function:
source("/Users/elenagoicoechea/Documents/School-Surveys/Exit-Surveys_2019/plot_funcs.R")
```

This proposal includes three main groups of charts (namely. 1. Simple Barchart, 2. Stacked Barchart, and 3. Treemap Chart), each one providing one more level of aggregation to the answer data.

All data included in these figures is taken from a spreadsheet with the full, raw results of the Graduate Exit Survey, AY 2018-2019 (attached).



```r
## set main args:
xlsxfilepath <- "/Users/elenagoicoechea/Documents/School-Surveys/De-ID_AY2018-19GraduateExitSurvey_5.31.19.xlsx"
colsets <- c(74:88,96:98,115:135,138:140,143:148,154:183,190,199,200,203:205,279,224:228,230:238,240:261)

## get matrices:
result <- get.tables(xlsxfilepath, colsets)

## build main matrix mapping:
dict <- build_dict(result)

#mtx_numlst <- c(1, 2, 3)

# pick matrices
mtx_lst <- c("Quality of overall curriculum", "Search for employment or training", "Quality of course content","Overall program quality", "Selection of online courses", "Would you select Johns Hopkins?")

colnames_lst <- list(c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Area', 'N', 'N/A - I did not receive advice on this', 'Not at all helpful',
                            'Not very helpful', 'Somewhat helpful', 'Very helpful', 'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('\rule{0pt}{2ex}', 'N', 'Definitely not', 'Probably not', 'Maybe',
                       'Probably', 'Definitely', 'pval'))


## for barcharts

#get dataframes:
dflst <- make_dflst(result, mtx_lst, colnames_lst, dict) 
df1 <- dflst[[1]]
df2 <- dflst[[2]]
df3 <- dflst[[3]]
df4 <- dflst[[4]]
df5 <- dflst[[5]]
df6 <- dflst[[6]]
```

### 1. Simple Barcharts

One simple chart aggregates a single answer option for a single given question, into different student groups. Useful for comparing student subpopulation answers to a selected question in more detail, and providing with concrete insights such as: 1) "Most students who considered their program's overall curriculum as "Poor" were MPH students", 2) "Virtually no DrPH student considered their curriculum as "Poor".

We can explore how students in different programs answered questions about "satisfaction" or "helpfulness", to compare the overall "climate" among the different groups (including against the whole school). For example, percentage of A = {"Poor" or "Not very helpful"} for several questions:


```r
# set params:
aspect_val <- 'Poor'
area_val <- 'Not at all helpful'
subttext = 'JHSPH Graduate Exit Survey, 2018-2019'

# build some simple barplots:
simplebarplot(df1, 'Aspect', aspect_val, subttext) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
simplebarplot(df2, 'Area', area_val, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
simplebarplot(df3, 'Aspect', aspect_val, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
simplebarplot(df4, 'Aspect', aspect_val, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

```r
simplebarplot(df5, 'Aspect', aspect_val, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-3-5.png)<!-- -->

We can try any other answer options - "Fair" and "Not very helpful" are shown below:


```r
# set params:
aspect_val2 <- 'Fair'
area_val2 <- 'Not very helpful'

# build some simple barplots:
simplebarplot(df1, 'Aspect', aspect_val2, subttext) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
simplebarplot(df2, 'Area', area_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
simplebarplot(df3, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
simplebarplot(df4, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

```r
simplebarplot(df5, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-4-5.png)<!-- -->

For "Good" and "Excellent":


```r
# set params:
aspect_val2 <- 'Good'
area_val2 <- 'Somewhat helpful'

# build some simple barplots:
simplebarplot(df1, 'Aspect', aspect_val2, subttext) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
simplebarplot(df2, 'Area', area_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
simplebarplot(df3, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
simplebarplot(df4, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
simplebarplot(df5, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-5-5.png)<!-- -->


```r
# set params:
aspect_val2 <- 'Excellent'
area_val2 <- 'Very helpful'

simplebarplot(df1, 'Aspect', aspect_val2, subttext) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
simplebarplot(df2, 'Area', area_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
simplebarplot(df3, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
simplebarplot(df4, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
simplebarplot(df5, 'Aspect', aspect_val2, subttext)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

### 2. Stacked Barcharts

Stacked barcharts still represnet a single question, but provide one more level of aggregation than simple charts since they include percentage answers for every answer option, for every student group:


```r
stackedbarplot(df1, 'Aspect', subttext )
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
stackedbarplot(df2, 'Area', subttext ) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
stackedbarplot(df3, 'Aspect', subttext ) 
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
stackedbarplot(df4, 'Aspect', subttext )
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```r
stackedbarplot(df6, '\rule{0pt}{2ex}', subttext )
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-7-5.png)<!-- -->

### 3. Treemap Charts

Treemaps add one more level of aggregation to student's answers, providing a visual summary of eacxh student group "mood" as reflected by their answers in the survey.

It takes a list of selected questions that have the same answer options and shows the relative answer proportions given by each group, all the given questions. Inner rectangle sizes are proportional to overall counts for each group.

A treemap example for selected questions [^2] with answer options = {'Poor', 'Fair', 'Good', 'Very good', 'Excellent'} is shown below: 

 [^2]: Questions included: "Quality of overall curriculum", "Quality of course content", "Relevance of course content", "Overall program quality", "Selection of online courses", "Your academic experience at Johns Hopkins", "Your student life experience at Johns Hopkins", "Your overall experience at Johns Hopkins""Breadth of courses offered", "Depth of courses offered", "Quality of academic advising".


```r
# list of selected tables to include in tree aggregation:
mtx_lst_tree <- c("Quality of overall curriculum", "Quality of course content", "Relevance of course content", "Overall program quality", "Selection of online courses", "Your academic experience at Johns Hopkins", "Your student life experience at Johns Hopkins", "Your overall experience at Johns Hopkins",
   "Breadth of courses offered", "Depth of courses offered", "Quality of academic advising")


x <- c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent','pval')
colnames_lst_tree <- rep(list(x), length(mtx_lst_tree))

#make dataframe:
dflst_tree <- make_dflst(result, mtx_lst_tree, colnames_lst_tree, dict)

# build treemap:
maketreemap(dflst_tree)
```

![](Summary-Plots_Proposal_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
