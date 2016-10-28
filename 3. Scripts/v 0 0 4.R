## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
## Make over Monday Entry for Wk 40 
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
# install.packages("flexdashboard", type = "source")

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

## for subtitles
#### ggplot_with_subtitle ####
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
setwd("d:/2. Bianca/1. Perso/14. MakeoverMonday/40. 2016 Oct 03")


## df.master ####
df.master = read.csv(
  "2. Data/Global Peace Index 2016.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


## secondary data
## SRC: https://www.theguardian.com/news/datablog/2011/oct/07/nobel-peace-prize-winners-list-2011#data
# List of Noble Prize Winners
df.secondary = read.csv(
  "2. Data/All nobel peace prize winners 2.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Pics of Noble Prize Winners:
# http://www.telegraph.co.uk/news/worldnews/10370215/Nobel-Peace-Prize-winners-from-1901-2014.html

## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Manipulate Data ####
## `````````````````````````````````````````````

### fixing primary data set (df.master)

names(df.master) = tolower(names(df.master))
names(df.secondary) = tolower(names(df.secondary))

# all country names to lower case
df.master = 
  df.master %>% 
  mutate(country = tolower(country))

df.secondary = 
  df.secondary %>% 
  mutate(country = tolower(country))


# master replica
df.1 = df.master
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Analysis 1 ####
## `````````````````````````````````````````````

## Idea
## our analysis is what happens to the ranking - post nobel prize
## ---

# df.1/df.master contains data for following years
i.year = 
  df.1$year %>%
  unique() 

# subset of df.secondary for years in df.1
df.secondary.year = 
  df.secondary %>%
  filter(year %in% i.year)

## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Analysis 2 ####
## `````````````````````````````````````````````

## Idea
## hw have nobel prize winners fared over years
## ---


# no of nobel prizes for each country
df.country.count = 
  df.secondary %>%
  select(year,winner,country) %>%
  na.omit() %>%
  mutate(country = tolower(country)) %>%
  group_by(country) %>%
  summarise(nbl.prize.cnt = n())
  
#   list of noble prize winners
# src: http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
c.nbl.cn.list = 
df.country.count %>%
  select(country) %>%
  unlist(use.names = FALSE)
  
# subset of df.1 containing only noble prize winners
df.nbl.data = 
  df.1 %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% c.nbl.cn.list)

## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Analysis 3 ####
## `````````````````````````````````````````````

## Idea
## compare mean and highest rank of two groups
## noble winners vs non winners
## ---


# diff from last year
df.diff = 
  df.1 %>%
  group_by(country) %>%  
  arrange(country) %>%
  # lower rank is better, hence reverse.diff has the inverted sign
  mutate(diff = rank - lag(rank), 
         reverse.diff = -diff) %>% 
  summarize(mean.diff = mean(diff,na.rm=TRUE), 
            highest.change = max(diff,na.rm=TRUE),
            r.mean.diff = mean(reverse.diff,na.rm=TRUE),
            r.highest.change = max(reverse.diff,na.rm=TRUE))


# adding noble prize winner flag to df.diff
df.diff.nbl.winner = 
  df.diff %>%
  # we do not need mean.diff and highest.change 
  # (as these do not consider that lower rank is better)
  select(-mean.diff,-highest.change) %>%
  mutate(nbl.winner = ifelse(country %in% c.nbl.cn.list,"yes","no"))


# adding count of noble prizes
df.secondary %>%
  


### Plot the data ###

# http://www.colorhunter.com/palette/1921085
fat.casual.bbq = c("#716065", "#cb8052", "#f9ded0", "#f961a4","#57324e")
col.1 = c("blue", "cyan4")
#col.2 = c("#e2b6cf","#cce8cc")
# https://coolors.co/export/png/cce8cc-f6efee-e2b6cf-e396df-e365c1
col.2 = c("#cce8cc","#e2b6cf")

  
g.1 = ggplot() + 
  theme_minimal() 

g.1 = g.1 +
  geom_point(data = df.diff.nbl.winner, 
             aes(x=r.mean.diff, y=r.highest.change, 
                 col=factor(nbl.winner), fill=factor(nbl.winner)),
             shape=21) +
  scale_colour_manual(name="Noble Prize Winners",
                      values=col.2) + 
  scale_fill_manual(name="Noble Prize Winners",
                    values=col.2) + 
  xlab("") + ylab("") 


g.2 = g.1 + ggtitle("Global Peace Index")

palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
             "#525252", "#252525", "#000000") # = brewer.pal 'greys'
color.background = "#f6efee" #palette[2]
color.grid.major = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]
#theme_bw(base_size=9) 


g.3 = g.2 + theme(
  panel.background=element_rect(fill=color.background, color=color.background),
  plot.background=element_rect(fill=color.background, color=color.background),
  plot.title=element_text(color=color.title, size=16, vjust=1.25, hjust=0),
  plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
  panel.grid.major=element_line(palette[3],size = 0.1),
  panel.grid.minor=element_line(palette[3],size = 0.05),
  
  # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  legend.position=c(.15, .88),
  
  legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.title= element_text(size=6),
  legend.text=element_text(size=6),
  legend.key.size=unit(0.3, "cm"),
  legend.key.width=unit(0.5, "cm"),
  
  axis.ticks=element_line(size = 0), #, colour = "red", linetype = "dotted")
  axis.ticks.length=unit(0, "cm"),	#length of tick marks (unit)
  #axis.ticks.margin=unit(0, "cm")	#space between tick mark and tick label (unit)
  axis.text=element_text(margin=(0))
)

g.4 = g.3  +
  labs(
    x = "Mean Value of Rank from 2008 ~ 16",
    y = "Highest Change in Rank from 2008 ~ 16")


## subtitle
# set the name of the current plot object to `gg`
gg <- g.4

# define the subtitle text
subtitle <- 
  "Rapid increase since 2005, taking only 6 years to cross another 100K"

p1 = ggplot_with_subtitle(gg, subtitle,
                          bottom_margin=20, lineheight=0.9)

p1


#### JUNKYARD ####

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