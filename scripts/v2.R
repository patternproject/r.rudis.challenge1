### Viz Challenge ####
# https://rud.is/b/2016/03/30/introducing-a-weekly-r-python-js-etc-vis-challenge/

### load libraries ####
# only install if not already done
# http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list.of.packages <- c("ggplot2", "showtext", "grid","ggalt","ggthemes","readxl","hrbrmisc","stringr","virdis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(ggalt)
library(ggthemes)
library(readxl)
library(dplyr)
library(hrbrmisc)
library(grid)
library(stringr)
library(virdis)

### download data ####
# get copies of the data locally

URL1 <- "http://www.faa.gov/uas/media/UAS_Sightings_report_21Aug-31Jan.xlsx"
URL2 <- "http://www.faa.gov/uas/media/UASEventsNov2014-Aug2015.xls"

fil1 <- basename(URL1)
fil2 <- basename(URL2)

if (!file.exists(fil1)) download.file(URL1, fil1)
if (!file.exists(fil2)) download.file(URL2, fil2)

### read data ####
# read it in

xl1 <- read_excel(fil1)
xl2 <- read_excel(fil2)

### manipulate data ####
# munge it a bit so we can play with it by various calendrical options

drones <- setNames(bind_rows(xl2[,1:3],
                             xl1[,c(1,3,4)]), 
                   c("ts", "city", "state"))
drones <- mutate(drones, 
                 year=format(ts, "%Y"), 
                 year_mon=format(ts, "%Y%m"), 
                 ymd=as.Date(ts), 
                 yw=format(ts, "%Y%V"))

# let's see them by week
by_week <- mutate(count(drones, yw), wk=as.Date(sprintf("%s1", yw), "%Y%U%u")-7)

# this looks like bad data but I didn't investigate it too much
by_week <- arrange(filter(by_week, wk>=as.Date("2014-11-10")), wk)

### visualize data ####
# plot

# gg <- ggplot(by_week, aes(wk, n))
# gg <- gg + geom_bar(stat="identity", fill="#937206")
# gg <- gg + annotate("text", by_week$wk[1], 49, label="# reports", 
#                     hjust=0, vjust=1, family="Cabin-Italic", size=3)
# gg <- gg + scale_x_date(expand=c(0,0))
# gg <- gg + scale_y_continuous(expand=c(0,0))
# gg <- gg + labs(y=NULL,
#                 title="Weekly U.S. UAS (drone) sightings",
#                 subtitle="As reported to the Federal Aviation Administration",
#                 caption="Data from: http://www.faa.gov/uas/law_enforcement/uas_sighting_reports/")
# #gg <- gg + theme_hrbrmstr(grid="Y", axis="X")
# gg <- gg + theme(axis.title.x=element_text(margin=margin(t=-6)))
# gg

# attempting a geom tile plot
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# http://docs.ggplot2.org/current/geom_tile.html
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# http://www.hitseq.com/ggplot/geom_tile.html
# http://rvisualization.com/wp-content/uploads/2015/05/ggplot2-tile.html

df.1 = by_week 

df.1$i.year <- as.numeric(df.1$yw %>% 
  substr(1,4))

df.1$i.week <- as.numeric(df.1$yw %>% 
  substr(5,6))

p1 <- ggplot(df.1, aes(x=i.year,y=i.week)) +
  #geom_tile(aes(fill = n), colour = "white") +
  geom_tile(aes(fill = n), colour = "white") +
  #scale_fill_gradient(low = "white", high = "steelblue")
  #scale_fill_gradient2(midpoint=50,mid="grey70",limits=c(0,100)) 
  #scale_fill_viridis()
  scale_fill_gradient2(low = '#67001f', mid = 'white', high = '#053061', midpoint = .5)


# http://rvisualization.com/wp-content/uploads/2015/05/ggplot2-tile.html
# https://github.com/ropensci/plotly/blob/master/tests/testthat/test-ggplot-heatmap.R

# p2 <- p1 +
#   theme(plot.background=element_rect(colour="grey93",fill="grey93"),
#         panel.background=element_rect(colour="grey93",fill="grey93"),
#         panel.grid.major=element_line(colour="cadetblue2",linetype=2),
#         axis.text=element_text(colour="navyblue",face="bold"),
#         axis.title=element_text(colour="slateblue4",face="bold"),
#         legend.position="none")

# p2 <- p1 +
#   theme(plot.background=element_rect(colour="lavenderblush2",fill="lavenderblush2"),
#         panel.background=element_rect(colour="lavenderblush2",fill="lavenderblush2"),
#         panel.grid.major=element_line(colour="lightcoral",linetype=2),
#         panel.grid.minor=element_blank(),
#         axis.text=element_text(colour="navyblue",face="bold"),
#         axis.title=element_text(colour="lightcoral",face="bold",size=20),
#         legend.position="none",
#         axis.ticks = element_blank()) + 
#         xlab("Week") + 
#         ylab("Year")


p2 <- p1 +
  theme(plot.background=element_rect(colour="white",fill="white"),
        panel.background=element_rect(colour="white",fill="white"),
        panel.grid.major.y=element_line(colour="lightcoral",linetype="dashed"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(colour="navyblue",face="bold"),
        axis.title=element_text(colour="lightcoral",face="bold",size=20),
        #legend.position="none",
        axis.ticks = element_blank())  
  
p2 <- p2 +
  xlab("Year") + 
  ylab("Week") +
  ggtitle(expression(atop(bold("Weekly U.S Drone Sightings"), atop(italic("As reported to the Federal Aviation Adminstration"), "")))) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# http://juliasilge.com/blog/You-Must-Allow-Me/
p3 <- p2 +
theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))


p3