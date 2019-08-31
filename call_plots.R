# We source the plot_funcs script:
source("/Users/elenagoicoechea/Documents/School-Surveys/Exit-Surveys_2019/plot_funcs.R")

############################### Run ###########################################

## set args:
xlsxfilepath <- "/Users/elenagoicoechea/Documents/School-Surveys/De-ID_AY2018-19GraduateExitSurvey_5.31.19.xlsx"
colsets <- c(74:88,96:98,115:135,138:140,143:148,154:183,190,199,200,203:205,279,224:228,230:238,240:261)

## get matrices:
result <- get.tables(xlsxfilepath, colsets)

## build matrix mapping:
dict <- build_dict(result)

#mtx_numlst <- c(1, 2, 3)

# pick matrices
mtx_lst <- c("Quality of overall curriculum", "Search for employment or training", "Quality of course content",
             "Overall program quality", "Selection of online courses")
colnames_lst <- list(c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Area', 'N', 'N/A - I did not receive advice on this', 'Not at all helpful',
                            'Not very helpful', 'Somewhat helpful', 'Very helpful', 'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'),
                     c('Aspect', 'N', 'Poor', 'Fair', 'Good', 'Very good', 'Excellent',
                       'pval'))

# build dataframes:
dflst <- make_dflst(result, mtx_lst, colnames_lst, dict)

df1 <- dflst[[1]]
df2 <- dflst[[2]]
df3 <- dflst[[3]]
df4 <- dflst[[4]]
df5 <- dflst[[5]]

# ser params
aspect_val <- 'Poor'
area_val <- 'Not at all helpful'
subttext = 'HSPH Graduate Exit Survey, 2018-2019'

# build some simple barplots
simplebarplot(df1, 'Aspect', aspect_val, subttext) 
simplebarplot(df2, 'Area', area_val, subttext)
simplebarplot(df3, 'Aspect', aspect_val, subttext)
simplebarplot(df4, 'Aspect', aspect_val, subttext)
simplebarplot(df5, 'Aspect', aspect_val, subttext)

# build some stacked barplots
stackedbarplot(df1, 'Aspect', subttext )
stackedbarplot(df2, 'Area', subttext ) 
stackedbarplot(df3, 'Aspect', subttext ) 
stackedbarplot(df4, 'Aspect', subttext ) 



