# Data Visualization

library(tidyverse)
library(RColorBrewer)
library(maps)

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

  
# On to maps
map()
map('usa')
map('county')
map('state', regions="north dakota")
map('france')


# These functions draw a polygon representation of a map. They're nice, but we can't do much with them.
# Let's look inside:
map_data('county')
map_data('state', regions='north dakota')
midwest_counties <- subset(map_data('county'), region=="ohio"|
                          region=="indiana"|
                          region=="wisconsin"|
                          region=="illinois"|
                          region=="michigan")
                          #region=="north dakota")

# Now, let's fill them:
p <- ggplot(data=map_data('county'),
            mapping = aes(x=long, y=lat, group=group)) #Sets up the canvas

p + geom_polygon(fill='white', color='black') + # Recreates in ggplot so now we can use the full grammar
  coord_map(projection = 'sinusoidal') +
  xlab('The Longer Side')


# Let's make a choropleth map of poverty levels in the midwest
# This requires JOINING two datasets: our map polygons and data

View(midwest_counties)
View(midwest)

# JOINS require a "key" (a column) that's the same in both datasets. But we don't have that...
# So let's make one:

midwest$subregion <- tolower(midwest$county)
midwest_merged <- left_join(midwest_counties, midwest) #You can specify merge key or let R figure it out

View(midwest_merged)

# Now we can go back to ggplot:

p <- ggplot(data=midwest_merged, aes(x=long, y=lat, group=group, fill=percbelowpoverty))

p + 
  geom_polygon(color="gray50")
  #scale_fill_distiller(palette = "RdYlBu")