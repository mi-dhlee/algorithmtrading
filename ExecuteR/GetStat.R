library(rjson)
library(RJSONIO)

###################
##Get Statistic Data
###################
apiKey="your API Key"
userStatsId="your ID"
apiRkey="your api related value"
Cnt=24

URL=paste("http://kosis.kr/openapi/statisticsData.do?method=getList&apiKey=",
	apiKey,
	"=&format=json&jsonVD=Y&userStatsId=",
	userStatsId,
	"/301/DT_042Y001/2/1/",apiRkey,"&prdSe=M&newEstPrdCnt=",
	Cnt,sep="")
	
Data=fromJSON(paste(readLines(URL,encoding="UTF-8"), collapse=""))

DatList=lapply(Data,function(x) {
	aDat=matrix(cbind(x["PRD_DE"],x["DT"]),ncol=2)	
})

wdDat=do.call(rbind,DatList)
saveRDS(wdDat,"C:/Git/Rds/Stat/wd.rds")
