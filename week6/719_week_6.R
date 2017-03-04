#
## Suchitra Deekshitula
## IST 719
#

# code to place 2 plots in context
x = 1:24
y = rnorm(length(x))
a = runif(n=length(x),min=2,max=9)
?runif

a = a + y^2
b = sample(c("D","W"),size = length(x), replace = T)


par(mfrow = c(2,1))
par(mar =c(2,4,4,2))

plot(x,y, type = "l")
par(mar =c(3,4,0,2))

barplot(a)

#matrix

m = matrix(c(1,1,3,
             1,4,3,
             2,2,3),
           nrow = 3, byrow = T)



layout(m)
?layout

par ( mar = c(0,4,4,2), bty = "n")

plot(x,y,type = "n", xaxt = "n", ylab = "", xlab = "",
     ylim = c(2*min(y), 2*max(y)))


lines(x,y,col= "purple",lty = 1, lwd = 2)
mtext(text = "Hourly Rate", side = 3, line = 2, cex = 1.3)
mtext(text = "Price", side = 2, line = 2)

par(mar = c(5,4,0,2))
barplot(a, names.arg = 1:length(x), col = "purple", border = NA)
mtext(text = "Volume", side = 2, line = 2)

par(mar = c(4,4,4,4), bty ="n")
boxplot(a ~b, col = "purple")


pie(table(b), col = c( "orange", "yellow"), border = NA)

#twitter

install.packages("circular","lubridate")
library(circular)
library(lubridate)

install.packages("wordcloud","plotrix")
library(wordcloud)
library(plotrix)

tweet.fname = choose.files()

tweets = read.delim(tweet.fname, quote ="\"", header = TRUE
                    , sep =",", stringsAsFactors = FALSE)

dim(tweets)
colnames(tweets)
table(tweets$media)


my.media = tweets$media
my.media[my.media == ""]= "text only"

table(my.media)
my.media = gsub(pattern = "photo\\|photo","photo",my.media)

# \\ signals a pipe ( special char)

barplot(table(my.media))

#Sun Aug 28 11:18:39 +0000 2016
tweets$created_at


my.date = as.POSIXct(strptime(tweets$created_at, "%a %b %d %H:%M:%S +0000 %Y"))
my.date

any(is.na(my.date))
max(my.date)
min(my.date)

difftime(max(my.date), min(my.date),units = "weeks")
difftime(max(my.date), min(my.date),units = "days")
difftime(max(my.date), min(my.date),units = "hours")

date.1 = format.Date(my.date, "%m-%d-%Y")

date.2 = as.POSIXct(strptime(date.1, "%m-%d-%Y"))
y.tab = table(date.2)
plot(y.tab)

plot(strptime(names(y.tab), "%Y-%m-%d"), as.numeric(y.tab), type = "l")




