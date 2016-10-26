dirData = "E:/okids/R/reading_history.csv"
dirLookup = "E:/okids/R/article_category.csv"
mydata <- read.csv(dirData)
lookup <- read.csv(dirLookup)
base1 <- (merge(mydata, lookup, by = 'article_id'))
listOfArticleId = as.matrix(unlist(sapply(base1, levels)))
len = length(base1[,3])
articleInInterval = base1[with(base1, order(timestamp)), ]
startIndexInterval = 1
dayFraction = 45/52
endIndexInterval = dayFraction*len

clickCounter = matrix(nrow = len, ncol = 12)
articleCounter = matrix (nrow = len, ncol =12)
articleCounter = as.data.frame(unique(articleInInterval [1:endIndexInterval,1]))
colnames(articleCounter) <- c("article_id")
articleCounter <- (merge(articleCounter , lookup, by = 'article_id'))
articleCounter <- table(articleCounter[,2])

articleCounterNow = matrix (nrow = len, ncol =12)
articleCounterNow = as.data.frame(unique(articleInInterval [endIndexInterval:len,1]))
colnames(articleCounterNow) <- c("article_id")
articleCounterNow <- (merge(articleCounterNow , lookup, by = 'article_id'))
articleCounterNow <- table(articleCounterNow[,2])
k=1
userLike = matrix(nrow = len, ncol = 15)
userLike  = as.data.frame(userLike)
colnames(userLike) <- c("User",listOfArticleId[1],listOfArticleId[2],listOfArticleId[3],listOfArticleId[4],listOfArticleId[5],listOfArticleId[6],listOfArticleId[7],listOfArticleId[8],listOfArticleId[9],listOfArticleId[10],listOfArticleId[11],listOfArticleId[12], "Interest", "Interest_2")
nowUser = 0
startIndex = 1

userDataInInterval = base1[with(base1, order(user_id)), ]
	
for (index in startIndexInterval:len) {
	nowUser = userDataInInterval[index,3]
	nextUser = userDataInInterval[index+1,3]
	if (nowUser != nextUser | index == len){
		end = index
		clickCounter = as.data.frame(userDataInInterval[startIndex:end,1])
		colnames(clickCounter ) <- c("article_id")
		clickCounter <- (merge(clickCounter , lookup, by = 'article_id'))
		clickCounter <- table(clickCounter [,2])
		userLike[k,1] = userDataInInterval[startIndex,3]
		max = 0
		listP = matrix(nrow = 12, ncol = 2)
		for (m in 2:13){
			pCatClick = clickCounter[m-1]/sum(clickCounter)
			pCat = articleCounter[m-1]/sum(articleCounter)
			sumArticle = sum(articleCounter)
			G = 10
			p0Cat = articleCounterNow [m-1]/sum(articleCounterNow )
			userLike[k,m] = 100* p0CatClick * ((sumArticle*pCatClick/pCat)+G)/(sumArticle+G)	
			listP[m-1,1] = userLike [k,m]
			listP[m-1,2] = listOfArticleId[m-1] 
			listP = listP[order(listP[,1],decreasing = TRUE),]
			interestLabel = listP[1,2]
			userLike[k,14] = interestLabel
		}
		listP[,1] = as.numeric(listP[,1])
		diff = as.numeric(listP[1,1]) - as.numeric(listP[2,1])
		if(abs(diff) < 0.3){
			userLike[k,15] = listP[2,2]
		} else {
			userLike[k,15] = "None"
		}
		k=k+1
		startIndex = end+1
	}		
}


output = na.omit(userLike)
output= output[with(output, order(User)),]
dirOutput ="E:/okids/R/output.csv"
write.csv(output,dirOutput)
dirStats ="E:/okids/R/statistik_interest.csv"
model2 <- table(userLike [,14])
write.csv(model2,dirStats)
dirStats2 ="E:/okids/R/statistik_interest2.csv"
model3 <- table(userLike [,15])
write.csv(model3,dirStats2)
