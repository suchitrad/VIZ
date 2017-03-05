#
##Suchitra D
#lab 7
# social network, JSON data, word cloud and scaling data

class.network.fname = file.choose()
class.data = read.csv(file = class.network.fname, header = TRUE, stringsAsFactors = FALSE)

colnames(class.data)
class(class.data)
class
library(igraph)
dim(class.data)
class.data[, c(3:42)]


m = as.matrix(class.data[, c(3:42)])
rownames(m)= class.data$Name
colnames(m)
rownames(m)
dim(m)
g = graph_from_adjacency_matrix(m)
g
plot(g, margin = -0.3)

vcount(g) #nodes, verices
ecount(g) #links,edges, ties

plot(degree(g))
deg.1 = degree(g)

par(mar = c(5,15,4,2))
barplot(sort(deg.1), horiz=T, las=2, border = NA, col= "darkgreen",
        main = "most links")


deg.2 = degree(g, mode ="in")

barplot(sort(deg.2), horiz=T, las=2, border = NA, col= "darkorange",
        main = "Popular")


deg.3 = degree(g, mode ="out")

barplot(sort(deg.3), horiz=T, las=2, border = NA, col= "darkorchid",
        main = "friendly")

#las= makes all axis text perpendicular 
#betweennes 



bet = betweenness(g)

par(mar = c(1,1,1,1))
plot.igraph(g, edge.arrow.size =0,
            edge.arrow.width =0,
            main = "IST 719- Tues  Wed", margin = -1
            )

plot(bet)

#plot1

my.size = bet  
par(mfrow = c(1,2))
#scaling
plot(sort(bet), main = "raw data", type = "l")
#plot(my.size^(1/5))
plot(sort(my.size^(1/5)), main = "sqrt scaled", type = "l")

par(mar = c(1,1,1,1), mfrow = c(1,1))
V(g)$size = bet

V(g)$size = 4 + my.size^(1/5)
plot.igraph(g, edge.arrow.size =0,
            edge.arrow.width =0,
            main = "IST 719- Tues  Wed"
)


par(mar = c(1,1,1,1), mfrow = c(1,1))
V(g)$size = bet

V(g)$size = 3  + (2 *my.size^(1/5))
plot.igraph(g, edge.arrow.size =0,
            edge.arrow.width =0,
            main = "IST 719- Tues  Wed", margin =-0.1
)

#plot2

V(g)$color = rgb(100,149, 237,
                  alpha = 160, maxColorValue = 255)

V(g)$color[class.data$Day == "t"] = rgb(165, 43, 42,
                                        alpha = 160, maxColorValue = 255)


V(g)$color[class.data$Day == "B"]= rgb (138, 43, 226,
                                        alpha = 160, maxColorValue = 255)


plot.igraph(g, edge.arrow.size =0,
            edge.arrow.width =0,
            main = "IST 719- Tues  Wed"
)

#use between 3 and 5 dimensions 

coords = layout_with_kk(g, dim = 3)
rglplot(g, layout = coords)
snapshot.fname = file.choose()

rgl.bringtotop()
rgl.snapshot(filename = "\\\\hd.ad.syr.edu\\03\\adf085\\Documents\\network.png", fmt = "png",top = TRUE )


?`rgl-package`

##### json
#suchitra D
#JSON 

library(jsonlite)
tweet.raw.fname = file.choose()

con  = file(tweet.raw.fname, open = "r")
tweets = stream_in(con)
close(con)
dim(tweets)
colnames(tweets)
tweets$text[1:3]
str(tweets)
tweets$user[1:3]

colnames(tweets$user)
table(tweets$user$lang)


plot(tweets$user$followers_count)

boxplot(tweets$user$followers_count)
hist(tweets$user$followers_count)

boxplot(log10(tweets$user$followers_count), ylab = "log10")



x = c(0,1,5,10,100,1000,10000)
y = log10(x)
plot(x,y,type= "s", ylab ="trasnformed log10")
log(x)
y

plot(sort(log10(tweets$user$followers_count)))

##########
library(stringr)
library(wordcloud)
library(plotrix)


tags = str_extract_all(tweets$text, "#\\S+", FALSE)
tags = tags[length(tags)>0]
tags = unlist(tags, use.names = FALSE)
tags = tolower(gsub("#|[[:punct:]]","", tags))
tags
length(tags)
tag.tab = table(tags)

myPlaFun = colorRampPalette(c("gold","orange","red"))

#plot3
wordcloud(names(tag.tab), as.numeric(tag.tab), scale = c(6,.75), min.freq = 1
          , max.words = Inf, random.order = FALSE, 
          random.color = FALSE, ordered.colors = TRUE,
          rot.per =0, colors = myPlaFun(length(tag.tab)))


#plot4
tag.df = data.frame(tag= names(tag.tab),
                    freq = as.numeric(tag.tab))


o = order(tag.df$freq, decreasing = TRUE)
tag.df = tag.df[o,]
tag.df$scale1 = rescale(tag.df$freq, c(0,1))
tag.df$scale2 = tag.df$scale1^2
tag.df$scale3 = tag.df$scale1^(1/2)
tag.df$scale4 = log10(tag.df$scale1 +1 )

plot(tag.df$scale1, type = "l", col = "yellow", lwd=5)
lines(tag.df$scale2, col= "red")
lines(tag.df$scale3, col = "blue")
lines(tag.df$scale4, col = "purple", lty = 2)

#plot5
wordcloud(tag.df$tag, tag.df$scale3 +1 , scale = c(2,.5), min.freq = 1
          , max.words = Inf, random.order = FALSE, 
          random.color = FALSE, ordered.colors = TRUE,
          rot.per =0, colors = myPlaFun(length(tag.tab)))



