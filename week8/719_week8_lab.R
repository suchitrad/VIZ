x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11)
y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)



plot(x,y,type = "l")
install.packages("maps")
library(maps)

m = map("state")
str(m)
m$names
my.fname = file.choose()
load(my.fname)
colnames(shootings)
sort(shootings$State)
shootings$State =gsub("^\\s+|\\s+$","",shootings$State)
#^ start of th string
#$ end of string
#\\s+ " whitspaces

shootings$Total.Number.of.Victims


agg.data = aggregate(shootings$Total.Number.of.Victims,
          by = list(shootings$State),
          sum)

colnames(agg.data) = c("state","victims")
agg.data

#139 26 26

#255 192 203

my.pal = colorRampPalette(c(rgb(255,192,203,maxColorValue = 255),
                   rgb(139,26,26, maxColorValue = 255)))


num.cols = 10
#my.cols = rev(my.pal(num.cols))
my.cols = my.pal(num.cols)
pie(rep(1,num.cols), col= my.cols)

my.cols
my.cols[c(7,1,2,1,5,1,1,4,3,1,1,1,6,7)]

library(plotrix)
agg.data$scaled = round(rescale(agg.data$victims, c(1,10)),0)
agg.data$color = my.cols[agg.data$scaled]
agg.data

map("state")

state.order = match.map(database = "state",
                        regions = agg.data$state,
                        exact = FALSE,
                        warn = TRUE)
cbind(m$names, agg.data$state[state.order],agg.data$color[state.order])

#plot1
map("state", col= agg.data$color[state.order],
    fill = TRUE,
    resolution =0,
    lty=1,
    border = "tan")

map('county','new york')

head(us.cities)
num.cities = dim(us.cities)[1]
num.cities = length(us.cities$name)


my.cols = rep(rgb(1,.6,.2,.7), length(us.cities$name))
my.cols[us.cities$capital >0 ] = rgb(.2,.6,1,.8)

map("state")
#plot2
points(us.cities$long, us.cities$lat,
       col= my.cols, pch = 16,
       cex = rescale(us.cities$pop, c(.5,7)))

#plot3
boxplot(log10(us.cities$pop) ~ us.cities$capital)
