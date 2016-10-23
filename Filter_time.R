mydata <- read.csv("C:/Users/Oki.OKI-PC/Documents/newsRecomendation-master/reading_history.csv")
lookup <- read.csv("C:/Users/Oki.OKI-PC/Documents/newsRecomendation-master/article_category.csv")
base1 <- (merge(mydata, lookup, by = 'article_id'))
listOfArticleId = as.matrix(unlist(sapply(base1, levels)))
len = length(base1[,3])
dataInInterval = base1[with(base1, order(timestamp)), ]
timeInterval = 13*24*60*60
time = timeInterval
startIndexInterval = matrix(nrow = 4, ncol = 1)
endIndexInterval = matrix(nrow = 4, ncol = 1)
j=1
startIndexInterval[1] = 1
for (index in 1:len){
	if(dataInInterval[index,2] > dataInInterval[1,2]+time){
		time = time+timeInterval
		endIndexInterval[j] = index
		if (index < len-1){
			startIndexInterval[j+1] = index+1	
		}
		j=j+1
	}
}
endIndexInterval[j] = len
k=0
clickCounter = matrix(nrow = len, ncol = 12)
articleCounter = matrix (nrow = len, ncol =12)
userLike = matrix(nrow = len, ncol = 13)
userLike  = as.data.frame(userLike)
colnames(userLike) <- c("User",listOfArticleId[1],listOfArticleId[2],listOfArticleId[3],listOfArticleId[4],listOfArticleId[5],listOfArticleId[6],listOfArticleId[7],listOfArticleId[8],listOfArticleId[9],listOfArticleId[10],listOfArticleId[11],listOfArticleId[12])
uniqueArticle = 0
uniqueId = 0
for (i in 1:4){
	temp = dataInInterval[startIndexInterval[i]:endIndexInterval[i],]
	userDataInInterval=temp[with(temp,order(user_id)),]
	articleCounter = as.data.frame(unique(userDataInInterval[,1]))
	colnames(articleCounter) <- c("article_id")
	articleCounter <- (merge(articleCounter , lookup, by = 'article_id'))
	articleCounter <- table(articleCounter[,2])
	lengthUserData = length(userDataInInterval[,1]);
	nowUser = 0
	startIndex = 1
	nextUser <-userDataInInterval[startIndex+2,3]
	start = 1
	for (index in startIndex:lengthUserData) {
		nowUser = userDataInInterval[index,3]
		if(is.na(nextUser)){break}
		if (nowUser != nextUser){
			end = index
			uniqueArticle[k] = length(unique(userDataInInterval[startIndex:end,1]))
			clickCounter = as.data.frame(unique(userDataInInterval[startIndex:end,1]))
			colnames(clickCounter ) <- c("article_id")
			clickCounter <- (merge(clickCounter , lookup, by = 'article_id'))			
			clickCounter <- table(clickCounter [,2])
			uniqueId [k] = length(unique(userDataInInterval[startIndex:end,3]))
			userLike[k,1] = userDataInInterval[startIndex,3]
			for (m in 2:13){
				userLike[k,m] = (clickCounter[m-1]/sum(clickCounter))*(sum(articleCounter)/articleCounter[m-1])*0.5		
			}
			k=k+1
			startIndex = end+1
		}
		
		nextUser = userDataInInterval[index+2,3]
	}
}

output = na.omit(userLike)
output= output[with(output , order(User)), ]
library(xlsx)
dirExcel = "C:/Users/Oki.OKI-PC/Documents/newsRecomendation-master/output.xlsx"
dirCsv ="C:/Users/Oki.OKI-PC/Documents/newsRecomendation-master/output.csv"
write.xlsx(output,dirExcel)
write.csv(output,dirCsv)
