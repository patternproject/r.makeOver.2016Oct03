## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
## Make over Monday Entry for Wk 39 
## However R will be used for basic data cleaning, visulization will be done in Tableau
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools)
pacman::p_load(dplyr,tidyr,scales,grid,stringr,rvest,readr)
pacman::p_load(purrr)

# http://stackoverflow.com/questions/33880211/ggplot2-fails-to-load
# install.packages("scales", dependencies = TRUE) 

# devtools::install_github("hadley/ggplot2")
pacman::p_load(ggplot2)

# for themes
# https://github.com/Bart6114/artyfarty
# devtools::install_github('bart6114/artyfarty')

# for combining multiple charts
# http://rmarkdown.rstudio.com/flexdashboard/
install.packages("flexdashboard", type = "source")

# tableau prefers xls

## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Helper Function ####
## `````````````````````````````````````````````

calculate_CAGR = function(end,start, end.y, start.y)
{
  period = end.y - start.y
  inv.period = 1/period
  return (((end / start)^inv.period)-1)
}


save_df = function(df,f.name,flag)
{
  if(flag)
  {
    # removing initial x added to the col names
    names(df) = gsub("x", "", names(df))
    
  }
  
  q.f.name = file.path("2. Data", f.name)
  write.csv(x=df, file=q.f.name,row.names=FALSE) 
  
}

# these fn are coming from: 
# D:\2. Bianca\1. Perso\36. Data Challenge\5. Code\v 01
# v22.r
dot_color <- function(x){
  ifelse(x == "Total","black",ifelse(x == "Female", f.color, m.color))
}

#http://sape.inf.usi.ch/quick-reference/ggplot2/shape
dot_shape <- function(x){
  ifelse(x > 0, 60,62) #right arrow, or left arrow
}

segment_color <- function(x){
  ifelse(x > 0, m.color,f.color) # male color, or female color
}

f.color = "#FF3F80"
m.color = "#3E50B4"
n.color = "#E3ECF7"

## for subtitles
# http://bayesball.blogspot.com/2016/03/adding-subtitle-to-ggplot2.html

library(grid)
library(gtable)

ggplot_with_subtitle <- function(gg, 
                                 label="", 
                                 fontfamily=NULL,
                                 fontsize=10,
                                 hjust=0, vjust=0, 
                                 bottom_margin=5.5,
                                 newpage=is.null(vp),
                                 vp=NULL,
                                 ...) {
  
  if (is.null(fontfamily)) {
    gpr <- gpar(fontsize=fontsize, ...)
  } else {
    gpr <- gpar(fontfamily=fontfamily, fontsize=fontsize, ...)
  }
  
  subtitle <- textGrob(label, x=unit(hjust, "npc"), y=unit(hjust, "npc"), 
                       hjust=hjust, vjust=vjust,
                       gp=gpr)
  
  data <- ggplot_build(gg)
  
  gt <- ggplot_gtable(data)
  gt <- gtable_add_rows(gt, grobHeight(subtitle), 2)
  gt <- gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
  gt <- gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)
  
  if (newpage) grid.newpage()
  
  if (is.null(vp)) {
    grid.draw(gt)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gt)
    upViewport()
  }
  
  invisible(data)
  
}


## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Read Data ####
## `````````````````````````````````````````````
setwd("d:/2. Bianca/1. Perso/14. MakeoverMonday/39. 2016 Sep 25")

## df.master ####
df.master = read.csv(
  "2. Data/Global Peach Index.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


## secondary data
## N/A

## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Manipulate Data ####
## `````````````````````````````````````````````

### fixing primary data set (df.master)

names(df.master) = tolower(names(df.master))

# master replica
df.1 = df.master

# removing redundant cols
if ((length(unique(df.1$item))) ==1)
{
  df.1 = 
  df.1 %>%
    select(-item)
}

if ((length(unique(df.1$metric))) ==1)
{
  df.1 = 
    df.1 %>%
    select(-metric)
}



## workhorse fn
# inputs: numeric vector
# outputs: data frame
# implementation: return df with 
# 1. mean
# 2. count of values above and below mean
fn_calc_prop = function(my.vector)
{
  #print("entering ---- ")
  #print(my.vector)
  #print(str(my.vector))
  
  # removing na
  my.vector = my.vector %>% na.omit()
  
  i.mean = my.vector %>% mean()
  #print(" mean")
  #print(i.mean)
  
  df.temp = data.frame(mean=i.mean,gt=0,lt=0)
  
  df.temp$gt = 
    as.data.frame(my.vector) %>%
    filter(. > i.mean) %>%
    summarise(gt = n()) 
  
  df.temp$lt = 
    as.data.frame(my.vector) %>%
    filter(. <= i.mean) %>%
    summarise(lt = n())
  
  
  return (df.temp)
}


df.summary =
 df.1 %>%
  # filter numeric cols
  keep(is.numeric) %>% 
  # for each numeric col
  # apply "fn_calc_prop" 
  map_df(fn_calc_prop)

# appending years cols back 
v.names = names(df.1)[-1] # except the first col containing country
df.summary$year = v.names
# removing initial x added to the col names
df.summary$year = gsub("x", "", df.summary$year)

# re-arranging cols for visibility
df.summary = 
  df.summary %>%
  select(year,mean,gt,lt)

# flattening out the list of gt and lt
# dont know why are list formed
df.summary$gt = unlist(df.summary$gt)
df.summary$lt = unlist(df.summary$lt)

# store df 
save_df(df.summary,"df.summary.csv",FALSE)


# http://lionel-.github.io/2015/10/08/using-purrr-with-dplyr/

cut_equal_sizes <- function(x, n = 3, ...) {
  ggplot2::cut_number(x, n, ...)
}

cut_equal_ranges <- function(x, n = 3, ...) {
  cut(x, n, include.lowest = TRUE, ...)
}

cut_categories <- function(x, n = 3) {
  # Record the name of the enclosed vector
  name <- names(x)
  
  # Create the new columns
  x$cat_n <- cut_equal_sizes(x[[1]], n)
  x$cat_r <- cut_equal_ranges(x[[1]], n)
  
  # Adjusting the names of the new columns
  names(x)[2:3] <- paste0(name, "_", n, names(x)[2:3])
  
  x
}


to.discretise.vars <- c(
  "mean"
)

df.summary.2 = 
  df.summary %>% 
  lmap_at(to.discretise.vars, cut_categories) 

# http://rforpublichealth.blogspot.com/2012/09/from-continuous-to-categorical.html
# df.summary$mean %>%
#   cut(breaks=c(100000,200000,300000,400000,500000),labels=c(1:4))

df.summary$cat = 
df.summary$mean %>%
  cut(breaks=c(100000,200000,300000,400000,500000),labels=c("1K+","2K+","3K+","4K+"))

# instead of messing with scales, transform the data
df.summary$mean.k <- df.summary$mean/1000

# proportion of countries above the mean
df.summary.3 =
  df.summary %>%
  #select(gt,lt) %>%
  mutate(my.sum = gt + lt, gt.mean = round((gt / my.sum),3)) %>%
  select(-my.sum)

## `````````````````````````````````````````````



## `````````````````````````````````````````````
#### Plot 1 ####
## `````````````````````````````````````````````

# http://www.colorhunter.com/palette/1921085
fat.casual.bbq = c("#716065", "#cb8052", "#f9ded0", "#f961a4","#57324e")

# filling in geom points
# http://stackoverflow.com/questions/15965870/fill-and-border-colour-in-geom-point-scale-colour-manual-in-ggplot


# http://stackoverflow.com/questions/25937000/ggplot2-error-discrete-value-supplied-to-continuous-scale
g.1 = ggplot(df.summary, aes(x = year, y = mean.k)) + 
  geom_point(aes(fill=cat),size = 2,shape=21) +
  scale_fill_manual(values=fat.casual.bbq) + 
  xlab("") + ylab("") + 
  theme_minimal() 

# making the x-label horizontal
g.3 = g.1 + theme(text = element_text(size = 10),
                  axis.text.x = element_text(angle = 90, hjust = 1))


g.3 = g.3 + ggtitle("World Peach Production")

palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
             "#525252", "#252525", "#000000") # = brewer.pal 'greys'
color.background = palette[2]
color.grid.major = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]
#theme_bw(base_size=9) 


g.3 = g.3 + theme(
  panel.background=element_rect(fill=color.background, color=color.background),
  plot.background=element_rect(fill=color.background, color=color.background),
  #panel.border=element_rect(color=color.background)
  plot.title=element_text(color=color.title, size=16, vjust=1.25, hjust=0),
  #plot.title = element_text(hjust=0, size=16)
  plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
  
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  legend.position=c(.15, .9),
  
  legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.title= element_text(size=0),
  legend.text=element_text(size=6),
  legend.key.size=unit(0.2, "cm"),
  legend.key.width=unit(0.5, "cm")

  
  
)

g.3

g.4 <- g.3 + annotate("text", x = 30, y = 200, label = "1999 >> mean production crosses 200K",
                    color="#7a7d7e", size=3, vjust=-1, fontface="bold")

g.5 <- g.4 + annotate("text", x = 35, y = 300, label = "2005 >> mean production crosses 300K",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

g.6 <- g.5 + annotate("text", x = 42, y = 390, label = "2011 >> mean production crosses 400K",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

g.6

# trying to reduce space between axis and plot
# but does not work

# # http://stackoverflow.com/questions/20220424/ggplot2-bar-plot-no-space-between-bottom-of-geom-and-x-axis-keep-space-above
# g.6 + 
#   coord_cartesian(xlim = c(1961,2012), ylim = c(0,450))
#   #scale_x_continuous(limits = c(1961,2012), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(0,420), expand = c(0, 0)) +
# 
# g.6 + 
#   geom_blank(aes(y=1.1*..count..), stat="count")  
  
## subtitle
# set the name of the current plot object to `gg`
gg <- g.6

# define the subtitle text
subtitle <- 
  "Rapid increase since 2005, taking only 6 years to cross another 100K"

p1 = ggplot_with_subtitle(gg, subtitle,
                     bottom_margin=20, lineheight=0.9)

p1

# add how world population has grown compared to 
# # https://www.r-bloggers.com/ourworldindata-an-r-data-package/
# growth of peach production
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Plot 2 ####
## `````````````````````````````````````````````
g.1 = ggplot(df.summary.3, aes(x = year, y = gt.mean)) + 
  geom_point(aes(fill=cat),size = 2,shape=21) +
  scale_fill_manual(values=fat.casual.bbq) + 
  xlab("") + ylab("") + 
  theme_minimal() 

# making the x-label horizontal
g.3 = g.1 + theme(text = element_text(size = 10),
                  axis.text.x = element_text(angle = 90, hjust = 1))


g.3 = g.3 + ggtitle("A Few Dominate the World Peach Production")

palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
             "#525252", "#252525", "#000000") # = brewer.pal 'greys'
color.background = palette[2]
color.grid.major = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]
#theme_bw(base_size=9) 


g.3 = g.3 + theme(
  panel.background=element_rect(fill=color.background, color=color.background),
  plot.background=element_rect(fill=color.background, color=color.background),
  #panel.border=element_rect(color=color.background)
  plot.title=element_text(color=color.title, size=16, vjust=1.25, hjust=0),
  #plot.title = element_text(hjust=0, size=16)
  plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
  
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  legend.position=c(.85, .8),
  
  legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.title= element_text(size=0),
  legend.text=element_text(size=6),
  legend.key.size=unit(0.2, "cm"),
  legend.key.width=unit(0.5, "cm")
  
  
  
)

g.3

# http://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
df.summary.3 %>%
   filter(gt.mean == max(gt.mean))
 
# max(df.summary.3$gt.mean)

g.4 <- g.3 + annotate("text", x = 28, y = 0.265, label = "1975 >> highest proportion, where mean production is 126K",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

#g.4

df.summary.3 %>%
  filter(gt.mean == min(gt.mean))


g.5 <- g.4 + annotate("text", x = 39, y = 0.08, label = "2010~12 >> lowest proportion, where mean production is ~400K",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

g.5

## subtitle
# set the name of the current plot object to `gg`
gg <- g.5

# define the subtitle text
subtitle <- 
  "The proportion of countries above the mean production for a given year, on Y-Axis"

p2 = ggplot_with_subtitle(gg, subtitle,
                     bottom_margin=20, lineheight=0.9)



## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Clean up ####
## `````````````````````````````````````````````
# rm(list=ls())