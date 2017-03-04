#
#Author : Suchitra Deekshitula
#Purpose : Lab 3: Multi dim plots
#                 and focussing on questions

tmp = file.choose()

sales = read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)

str(sales)
colnames(sales)


# "X"            "sale.date"    "sales.rep"    "rep.sex"      "rep.region"  
# "rep.feedback" "wine"         "type"         "cost"         "unit.price"  
# "units.sold"   "recipt"       "expenses"     "year"        


#Q: what is the relationship between expenses and reciepts

sales$recipt[1:10]
sales$expenses[1:10]
cbind(sales$recipt[1:10], sales$expenses[1:10])

#example 1: 

plot(sales$recipt, sales$expenses,
     main = "Recipts and Expenses", bty = "n")


mtext(text= " what is the relationship between expenses and recipts?", side =3, line =0, adj = 0.5)



#side: which side(bottom:1, left:2, top:3 , right:4)
#line =1 : margin
#adj : justify: 0.5: centre
#0: left
#1: right


#example 2
View(sales)
mtext(text= " what is the relationship between expenses and recipts?", side =3, line =0, adj = 0)


#relationship between recipt and type

sales$recipt
sales$type[1:10]
unique(sales$type)
table(sales$type)

#bty : boxplot type
par(bty = "n")
#par(bty="l")
boxplot(sales$recipt ~ sales$type, col = c("skyblue", "pink"), names= c("red wine", "white wine"), main = " sales type vs recipts")
mtext(text= " what is the relationship between wine type  and recipts?", side =3, line =0, adj = 0.5, col = "blue")

?boxplot

#which region sells the most units
sales$rep.region
hist(sales$units.sold)

#shows a distribution
boxplot(sales$units.sold ~ sales$rep.region)

sum(sales$units.sold[sales$rep.region == "East"])
sum(sales$units.sold[sales$rep.region == "North"])
# tedious process


units.by.reg = aggregate ( sales$units.sold, 
                           by = list(sales$rep.region), 
                                     FUN =sum)

units.by.reg

barplot(units.by.reg[,2], names.arg = units.by.reg[,1])

#aliter

units.by.reg = aggregate ( sales$units.sold, 
                           by = list(region = sales$rep.region), 
                           FUN =sum)

units.by.reg

barplot(units.by.reg$x, names.arg = units.by.reg$region, main = " Wine sales by region")

units.by.reg.type = tapply(sales$units.sold, 
                            list(sales$rep.region,sales$type), sum)
class(units.by.reg.type)
class(units.by.reg)
rownames(units.by.reg.type)

#stacked bar plot
barplot(units.by.reg.type)

#Question : do different regions sell different ratios of red to white

#beside bar plot( grouped bar chart)
units.by.reg.type = tapply(sales$units.sold, 
                           list(sales$type,sales$rep.region), sum)

barplot(units.by.reg.type, beside = TRUE, col = c("maroon", "white"), legend.text = c("Red wine", "White wine") )
mtext(text ="Wine sales by region", line =2, adj = 0.5, cex = 2)

mtext(text ="Do different regions sell different ratios of red wine  to white wine", line =0.5, adj = 0)

#side: which side(bottom:1, left:2, top:3 , right:4)
#line =1 : margin
#adj : justify: 0.5: centre
#0: left
#1: right


?barplot

#Question: Are recipts growing over time for each region

colnames(sales)
hist(sales$year)

m = tapply(sales$recipt, list(sales$rep.region, sales$year), sum)

options(scipen = 999) # to avoid scientific notation on the y axis
range(m)
max(m)
x = as.numeric(colnames(m))

plot(x,m[1,], type = "l", col = "red", ylim =c(0,100000), lwd = 3, xlab = "Year", ylab = "Recipts")

mtext( text = " Recipts by region", side = 3, line = 1.5, cex = 1.75, adj=0.5)

lines(x, m[2,], col="blue", lwd = 3)
lines(x, m[3,], col="orange", lwd = 3)
lines(x, m[4,], col="pink", lwd = 3)
lines(x, m[5,], col="black", lwd = 3)

#legend()
#bottomleft , center , right, bottomright, topright, topleft, 

legend('bottomright', legend = rownames(m), lwd = 2, lty = 1, bty ="n", 
       col = c("red", "blue", "orange","pink","black"))



sales$sale.date
plot(sales$sale.date)
as.Date(sales$sale.date)

my.year = which(sales$year == 2014)
my.reg = which(sales$rep.region =="East")
tmp = sales[intersect(my.year, my.reg),]
dim(tmp)

#tmp = sales[sales$year == 2014, ]


my.date = as.Date(strptime(tmp$sale.date,"%m/%d/%Y"))
new.df = aggregate(tmp$recipt
          , by = list(date = my.date)
          , FUN =sum)
head(new.df)
plot(new.df$date, new.df$x, type ="l", main = "Sales recipts for East for 2014", xlab = "variance over time", ylab = "Sales recipts", col ="forestgreen")



