mydata <- read.csv("E:/okids/R/reading_history.csv")
lookup <- read.csv("E:/okids/R/article_category.csv")
base1 <- (merge(mydata, lookup, by = 'article_id'))
a = base1[with(base1, order(timestamp)), ]
len = length(a[,3])
timeInterval = 7*24*60*60
for (index in 1:len){
	if(a[index,2] > a[1,2]+timeInterval){
		b = a[1:index,]
		break
	}
}

b=b[with(b,order(user_id)),]
now = 0
startIndex = 1
previous <-b[startIndex+2,3]

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