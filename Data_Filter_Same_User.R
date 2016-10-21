a
mydata <- read.csv("E:/okids/R/reading_history.csv")
lookup <- read.csv("E:/okids/R/article_category.csv")
base1 <- (merge(mydata, lookup, by = 'article_id'))
a = base1[with(base1, order(timestamp)), ]
len = length(a[,3])
now = 0
startIndex = 1416
previous <- a[startIndex+2,3]

for (index in startIndex:len) {
	now = a[index,3]
	if (now != previous){
		end = index
		break
		print("inside here")
	}
	previous = a[index+2,3]
}

plot(a[startIndex:end,2],a[startIndex:end,4])

