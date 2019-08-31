
library(ggplot2)
library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(gridExtra)
library(extrafont)
library(reshape2)
library(scales)
library(hash)

# We source main_masters script to use get.tables function:
source("/Users/elenagoicoechea/Documents/School-Surveys/Exit-Surveys_2019/main_masters.R")

##################### Plotting Selected Aggregate Survey Results #########################################
#####################################  August 27, 2018 ###################################################

# 0 Build a Customized ggplot2 Theme

theme_surveys <- theme(line=element_line(size=0.5, lineend = 'round' ),
                   rect=element_rect(fill = "white", colour = "grey26"),
                   text=element_text(family = "Trebuchet MS", colour = "grey26" ),
                   aspect.ratio = 0.75,
                   plot.margin = margin(0.75,0.75,0.75,0.75, 'cm'),
                   plot.background = element_rect(fill = "white", colour = "lightcyan3",
                                                  size = 1.5),
                   
                   plot.title=element_text(size = rel(1.2), color="grey26", hjust = 0.75, 
                                           vjust = 1.15, face = "bold"),
                   plot.subtitle = element_text(size = rel(1.05), color="lightcyan4",
                                                hjust = 0.5, vjust = 0.5),
                   panel.grid.major.x = element_blank(),
                   panel.background = element_rect(colour = "grey80", fill = 'white'),
                   panel.grid.major = element_line(colour = "grey90", size=0.20),
                   
                   axis.text = element_text(size=12, colour = "grey50", vjust = 0, angle = 90),
                   axis.ticks.x = element_line(size=0.9, colour = "grey50"),
                   axis.ticks.y = element_line(size=0.9, colour = "grey50"),
                   axis.ticks.length = unit(0.25, "cm"),
                   axis.title.y = element_text(size = rel(1.05), angle = 90, face = "bold",
                                               vjust = 0),
                   axis.title.x = element_text(size = rel(1.05), angle = 0, face = "bold",
                                               vjust = -4),
                   
                   legend.justification = "center",
                   legend.box.background = element_rect(colour = "grey90"),
                   legend.box.margin = margin(0.5,0.5,0.5,0.5),
                   legend.text = element_text(size = rel(0.95)),
                   legend.title = element_text(size = rel(1.15), face = "bold",
                                               colour = "lightcyan4"),
                   legend.key = element_rect(fill = "grey96"),
                   strip.background = element_rect(colour = "grey80", fill = "grey96"),
                   strip.text.x = element_text(hjust=0.1, color='grey40',
                                               size = rel(1.05),  face = "bold"),
                   plot.caption = element_text(vjust=-2, size = rel(0.95)),
                   panel.spacing = unit(0.5, "cm"))


# 1 (Func) Map table topics to matrix indeces

build_dict <- function(res){
  dict <- hash() 
  for (i in 1:length(res)){
    if (length(res[[i]]) > 2) {
      if (res[[i]][[2]] != "" && res[[i]][[3]] != "") {
    if (has.key(res[[i]][[2]], dict)) {dict[[res[[i]][[3]]]] <- i} 
    else {dict[[res[[i]][[2]]]] <- i}
      }
    }
}
  return(dict)
}

# 2 (Func) Build a dataframe:
make_df <- function(res, mtx_numb, col_names) {
  resdf <- res[[mtx_numb]]
  resdf <- tail(resdf, -1) # remove first row (just formatting settings for tex tables)
  resdf<- resdf[-c(3),] # remove empty row
  colnames(resdf) <- col_names
  return(as.data.frame(resdf))
}

# 3 (Func) Build all dataframes:
make_dflst <- function(res, mtx_lst, colnames_lst, dict) {
  df_lst <- list()

  for (i in 1:length(mtx_lst)) {
    mtx_name <- paste0("BOLD", mtx_lst[i] )
    mtx_idx <- dict[[mtx_name]]
    dfm <- make_df(res, mtx_idx, colnames_lst[[i]])
    name <- paste('df:',i,sep='')
    df_lst[[name]] <- dfm
  }
  return(df_lst)
}

# 4 Plot results

## 4.1 (Func) Simple Barplot
simplebarplot <- function(df, xnm, ynm, subttext) {
   if (xnm == 'Area') {if (ynm == 'N/A - I did not receive advice on this') {fillcolor='#e5f4c2'}
     else if (ynm == 'Not at all helpful') {fillcolor='#b5e7aa'} else if (ynm == 'Not very helpful') {fillcolor='#64c5aa'} 
     else if (ynm == 'Somewhat helpful') {fillcolor='#2a9fcc'} else {fillcolor='#2466b0'}}
  else if (xnm == 'Aspect') {if (ynm == 'Poor') {fillcolor='#e5f4c2'}
    else if (ynm == 'Fair') {fillcolor='#b5e7aa'} else if (ynm == 'Good') {fillcolor='#64c5aa'} 
    else if (ynm == 'Very good') {fillcolor='#2a9fcc'} else {fillcolor='#2466b0'}
                                        }             
   title <- strsplit(as.character(df[1,1]), 'BOLD', fixed=FALSE)[[1]][2]
   title <- paste0(title , ' = ')
   title <- paste0(title , ynm)
   
   df_clean <- tail(df, -1)
   df_clean[,ynm] <- as.numeric(as.character(df_clean[,ynm]))
   school <- subset(df_clean, df_clean[,xnm]=='Schoolwide')
   schoolw <- as.numeric(as.character(school[,ynm]))
   dfs <- subset(df_clean, df_clean[,xnm] !='Schoolwide')
   X <- dfs[,xnm]
   Y <- dfs[,ynm]

    ggplot(data=dfs, aes(x=reorder(X, Y), y = Y)) +
      geom_bar(stat="identity", color = "lightcyan3", fill = fillcolor) +
      geom_hline(yintercept = schoolw, color='#032e5e', linetype = "dashed") +
      ggtitle(title) +
      labs(subtitle = subttext) +
      scale_y_continuous(breaks= pretty_breaks()) +
      xlab(xnm) + 
      ylab("%") +
      theme_surveys +         #ggthemes::theme_economist() +
      theme(axis.title.x=element_blank(), legend.title= element_blank()) +
      annotate("text", label = 'Schoolwide (%)',
               x = 2, y = (1.15*schoolw), size= 3.5, family = "Trebuchet MS",
               color='#032e5e')
    #ggsave(output_filepath)
}

## 4.2 (Func) Stacked Barplot
stackedbarplot <- function(df, xnm, subttext) {
  
  title <- strsplit(as.character(df[1,1]), 'BOLD', fixed=FALSE)[[1]][2]
  df_clean <- tail(df, -1)
  df_clean <- subset(df_clean, select = -c(N, pval))
  df_m <- melt(df_clean, id.var=xnm)
  df_m$value <- as.numeric(df_m$value)
 
  X <- df_m[,xnm]
  Y <- df_m$value 
  
  ggplot(df_m, aes(x=X, y = Y, fill = variable)) + 
    geom_bar(position="stack", stat="identity") +
    scale_y_continuous(breaks= pretty_breaks()) +
    scale_fill_brewer(palette="GnBu") +
    ggtitle(title) +
    labs(subtitle = subttext) +
    ylab("%") +
    theme_surveys +
    theme(axis.title.y = element_blank(), legend.title= element_blank(),
          axis.text = element_text(size=11, colour = "grey50", vjust = 0, angle = 0)) +
    coord_flip()
  
}


