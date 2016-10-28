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

#### fn_get_rank ####
# fn to search rank
# INPUT: year (integer)
# OUTPUT: rank (integer)
# SRC: 
# http://adv-r.had.co.nz/Functions.html
# https://cran.r-project.org/web/packages/ArgumentCheck/vignettes/ArgumentChecking.html

# fn_get_rank = function (c.country = "finland", i.search.year = 2008) # for testing
fn_get_rank = function (c.country, i.search.year)
{
  print("entering fn")
  
  # if input args are missing
  # SRC: 
  # https://www.r-bloggers.com/programming-with-r-%E2%80%93-checking-function-arguments/
  
  if (missing(c.country))
    stop("Need to specify country for calculations.")
  
  if (missing(i.search.year))
    stop("Need to specify year for calculations.")
  
  if (!is.character(c.country))
    stop("Country should be character.")
  
  if (!is.numeric(i.search.year))
    stop("Year should be numeric.")
  
  # search the year in df.1 
  # if found return corresponding rank
  # else return NA
  i.flag =     
    df.1 %>%
    filter(year == i.search.year & country == c.country) %>%
    count() %>%
    unlist(use.names = FALSE) # to convert df into a vector **1
  
  # src: for **1
  # http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
  
  #str(i.flag)
  #print(i.flag)
  
  if (i.flag == 1)
  {
    return(
      df.1 %>%
        filter(year == i.search.year & country == c.country) %>%
        select(rank) %>%
        unlist(use.names = FALSE)
    )
  }
  else
  {
    return(NA)
  }
  
  
} # end fn_get_rank

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
## df.secondary ####
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

# remove white spaces
df.secondary$country =
  df.secondary %>%
  select(country) %>%
  #str_trim()
  unlist(use.names = FALSE) %>%
  # As of R 3.2.0 a new function was introduced for removing leading/trailing whitespace
  trimws() %>%
  str_trim()


# master replica
df.1 = df.master
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Analysis 1 ####
## `````````````````````````````````````````````

## ---
### IDEA
## ---
## our analysis is what happens to the ranking - post nobel prize
## ---

# df.1/df.master contains data for following years
i.year = 
  df.1$year %>%
  unique() 

# adding 2006 and 2007 to make it more comprehensive
i.year.2 = 2006:2016

# subset of df.secondary for years in df.1
df.nbl.win.1 = 
  df.secondary %>%
  filter(year %in% i.year) %>%
  #filter(year %in% i.year.2) %>%
  select(country, year) %>%
  # removing duplicate entries
  distinct() 

# Naming cols is necessary
# https://github.com/hadley/purrr/issues/203
names(df.nbl.win.1) = c("c.country", "i.search.year")

# extending the df to include year2 and year3, post winning the medal
df.nbl.win.2 = 
  df.nbl.win.1 %>%
  group_by(c.country) %>%
  mutate(year2 = i.search.year + 1, 
         year3 = i.search.year + 2) %>%
  ungroup()

# replace usa with united states of america
df.nbl.win.3 = 
  df.nbl.win.2 %>%
  mutate(c.country.2 = ifelse(c.country=="usa", "united states of america",c.country)) %>%
  select(-c.country) %>%
  # renaming the newly created col, for pmap
  rename(c.country=c.country.2) %>%
  select(c.country,i.search.year,year2,year3)


# mapping medal year
df.nbl.win.3$nbl.yr = 
  df.nbl.win.3 %>%
  select(c.country,i.search.year) %>%
  pmap(fn_get_rank) %>%
  unlist()
  
# mapping medal year + 1
df.nbl.win.4 = 
  df.nbl.win.3 %>%
  # rename cols for pmap
  rename(year1=i.search.year) %>% #new.name=old.name
  rename(i.search.year=year2) 
  
df.nbl.win.4$nbl.yr1 = 
  df.nbl.win.4 %>%
  select(c.country,i.search.year) %>%
  pmap(fn_get_rank) %>%
  unlist()

# mapping medal year + 2
df.nbl.win.4 = 
  df.nbl.win.4 %>%
  # rename cols for pmap
  rename(year2=i.search.year) %>%
  rename(i.search.year=year3) 
  

df.nbl.win.4$nbl.yr2 = 
  df.nbl.win.4 %>%
  select(c.country,i.search.year) %>%
  pmap(fn_get_rank) %>%
  unlist()

df.nbl.win.4 = 
  df.nbl.win.4 %>%
  # rename cols for pmap
  rename(year3=i.search.year)

df.nbl.win.4

# making copy
df.nbl = df.nbl.win.4

df.nbl = 
  df.nbl %>%
  rename(country=c.country)

# removing clutter
rm(df.nbl.win.1,df.nbl.win.2,df.nbl.win.3,df.nbl.win.4)

# for missing data
# Not available at:

# Alternate Rankings
# http://www.photius.com/rankings/global_peace_index_2007.html
# https://www.theguardian.com/news/datablog/2011/jan/17/global-peace-index

# decide not to use alternate rankings
# df.nbl %>%
#   mutate (nbl.yr=ifelse(country =="united states of america" & year1 == 2007,1,nbl.yr)) %>%
  

# not enough data for 2015 and 2016 winners - Tunisia
df.nbl = 
  df.nbl %>%
  #filter(!grepl(pat,year1))
  na.omit()





## ---
### STATUS
## ---
# In complete

## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Analysis 2 ####
## `````````````````````````````````````````````

## ---
### IDEA
## ---
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

## ---
### STATUS
## ---
# In complete


## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Analysis 3 ####
## `````````````````````````````````````````````

## ---
### IDEA
## ---
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


# ```````````````````
### Plot the data ###
# ```````````````````

## Color Scheme
# http://www.colorhunter.com/palette/1921085
#fat.casual.bbq = c("#716065", "#cb8052", "#f9ded0", "#f961a4","#57324e")
#col.1 = c("blue", "cyan4")
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

# main title
#g.2 = g.1 + ggtitle("Global Peace Index\n Comparing Noble Prize Winners with Non Winners")
main.title = "Global Peace Index"
sub.title = "Comparing Noble Prize Winners with Non Winners"
g.2 = g.1 + ggtitle(bquote(atop(.(main.title), atop(italic(.(sub.title)), ""))))


# theme values
palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
             "#525252", "#252525", "#000000") # = brewer.pal 'greys'
color.background =  "#FDFCFB" # "#FCFAFA" #white" # "#f6efee" #palette[2]
color.grid.major = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]
#theme_bw(base_size=9) 

# setting theme
g.3 = g.2 + theme(
  panel.background=element_rect(fill=color.background, color=color.background),
  plot.background=element_rect(fill=color.background, color=color.background),
  plot.title=element_text(color=color.title, size=16), #, vjust=1.25, hjust=0),
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
  
  axis.ticks=element_line(size = -1), #, colour = "red", linetype = "dotted")
  axis.ticks.length=unit(0, "cm"),	#length of tick marks (unit)
  #axis.ticks.margin=unit(0, "cm")	#space between tick mark and tick label (unit)
  axis.text=element_text(margin=(0),colour="#CAC4C3", face="bold")
)

# x and y labels
g.4 = g.3  +
  labs(
    x = "Mean of Delta in Rank Value from Previous Year, across 2008 ~ 2016",
    y = "Highest Change in Rank from 2008 ~ 2016")

# Label for Georgia
g.5 <- g.4 + annotate("text", x = 8.6, y = 27, label = "Georgia",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

# Label for Mongolia
g.6 <- g.5 + annotate("text", x = 6.6, y = 31.5, label = "Mongolia",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

# Label for Liberia
g.7 <- g.6 + annotate("text", x = 2.8, y = 16, label = "Liberia",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")


# Label for Iran
g.8 <- g.7 + annotate("text", x = 1.3, y = 24, label = "Iran",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")


# Label for Egypt
g.9 <- g.8 + annotate("text", x = -6.2, y = 16, label = "Egypt",
                       color="#7a7d7e", size=3, vjust=-1, fontface="bold")


# Label for Mexico
g.10 <- g.9 + annotate("text", x = -5.8, y = 2.8, label = "Mexico",
                      color="#7a7d7e", size=3, vjust=-1, fontface="bold")

# Label for Yemen
g.11 <- g.10 + annotate("text", x = -3.37, y = 1.8, label = "Yemen",
                       color="#7a7d7e", size=3, vjust=-1, fontface="bold")

# Label for Syria
g.12 <- g.11 + annotate("text", x = -8, y = 0, label = "Syria",
                        color="#7a7d7e", size=3, vjust=-1, fontface="bold")


g.12

# rectangle to highlight the crowded area
df.highlight = data.frame(
  xmin = -2.5,
  xmax = 2.5,
  ymin = 0,
  ymax = 15
)

df.dummy = 
  df.diff.nbl.winner %>%
  filter(r.mean.diff >= -2.5 & 
         r.mean.diff <= 2.5 &
         r.highest.change >= 0 &
         r.highest.change <= 15) %>%
  count()



g.13 = g.12 + 
  geom_rect(data=df.highlight, 
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=1/10) #,fill="#FDFCFB")


g.14 = g.13 + annotate("text", x = 4.6, y = 0.25, label = "104 out of 163 values are \ninside the shaded region",
                color="#7a7d7e", size=3, vjust=-1, fontface="bold")

g.14

## ---
### STATUS
## ---
# Completed
# Stored as: 
# g.14
# in:
# D:\2. Bianca\1. Perso\14. MakeoverMonday\40. 2016 Oct 03\4. Figures


## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Clean up ####
## `````````````````````````````````````````````
# rm(list=ls())
## `````````````````````````````````````````````