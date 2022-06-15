# Data Visualization

library(tidyverse)
library(RColorBrewer)

ca <- read_csv("https://raw.githubusercontent.com/ScienceParkStudyGroup/r-lesson-based-on-ohi-data-training/gh-pages/data/ca.csv") 

p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors))

# Why does it show nothing?

p + geom_line()
p + geom_point()
p + geom_smooth()
p + geom_point() + 
  geom_smooth()
# check out https://r-graph-gallery.com/index.html or https://r-graph-gallery.com/ggplot2-package.html for more geoms


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors,
                          color=park_name))

p + geom_smooth()
p + geom_boxplot() # reduce visitors var by 1000


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors,
                          color=park_name,
                          size=visitors/1000))
p + geom_point(alpha=0.4) + 
  xlab('Visitors') +
  ylab('Year') +
  labs(color="Parks", size="Visitors (x1000)") +
  #scale_color_hue(h=c(0,360), c=200, l=50) # Remember to use scale_fill_hue for shapes
  #scale_color_manual(values = c('red', 'blue', 'green', 'purple', 'yellow', 'black', 'magenta', 'gray', 'green'))
  #scale_color_brewer(type="div", palette = 1, aesthetics = "color")
