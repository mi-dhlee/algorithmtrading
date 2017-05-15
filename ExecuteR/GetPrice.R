rm(list=ls(all=TRUE))

library(rcom)

obj=comCreateObject("XA_Session.XASession")
objQuery.t8430=comCreateObject("XA_DataSet.XAQuery")
objQuery.t4201=comCreateObject("XA_DataSet.XAQuery")

#######################
##Log In
#######################
comInvoke(obj,"DisconnectServer")
comInvoke(obj,"ConnectServer","demo.etrade.co.kr",20001)

ID="your ID"
PW1="your pwd"

Login=comInvoke(obj,"Login",ID,PW1,"",0,"False")

repeat {
	if(comInvoke(obj,"GetClientIP")=="") {
		Sys.sleep(1)
	} else {
		break
	}
}

Sys.sleep(5)

#########################
##Get Code
#########################
comInvoke(objQuery.t8430,"LoadFromResFile","C:/eBEST/xingAPI/Res/t8430.res")
comInvoke(objQuery.t8430,"SetFieldData","t8430InBlock", "gubun", 0, 1) #0:전체/1:코스피/2:코스닥

repeat {
	comRqst=comInvoke(objQuery.t8430,"Request","False")

	if(comRqst<0) {
		Sys.sleep(2)
	} else {
		break
	}
}

Sys.sleep(2)

LoopEnd=comInvoke(objQuery.t8430,"GetBlockCount","t8430OutBlock")

repeat {
	LoopEnd=comInvoke(objQuery.t8430,"GetBlockCount","t8430OutBlock")

	if(LoopEnd=="" | LoopEnd==0) {
		Sys.sleep(2)

		LoopEnd=comInvoke(objQuery.t8430,"GetBlockCount","t8430OutBlock")
	} else {
		break
	}
}

Sys.sleep(3)

LoopEnd=as.numeric(LoopEnd)
LoopEnd=LoopEnd-1

CodeFunc=function(i) {
	Code=comInvoke(objQuery.t8430,"GetFieldData","t8430OutBlock", "shcode", i)

	repeat {
		if(Code=="") {
			Sys.sleep(1)

			Code=comInvoke(objQuery.t8430,"GetFieldData","t8430OutBlock", "shcode", i)
		} else {
			break
		}
	}

	return(Code)
}

Code=as.vector(unlist(lapply(0:LoopEnd,CodeFunc)))


#########################
##Get Recent Data : Monthly
#########################
PastPriceFunc=function(j) {
	Date=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "date",j+1)
	Open=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "open",j+1)
	High=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "high",j+1)
	Low=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "low",j+1)
	Close=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "close",j+1)
	Volume=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "jdiff_vol",j+1)
	Rate=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "rate",j+1)
	
	DoCnt=0

	repeat {
		DoCnt=DoCnt+1

		if(Date=="" | Open=="" | High=="" | Low=="" | Close=="") {
			if(DoCnt>=10) {
				break
			}

			Sys.sleep(1)

			Date=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "date",j+1)
			Open=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "open",j+1)
			High=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "high",j+1)
			Low=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "low",j+1)
			Close=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "close",j+1)
			Volume=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "jdiff_vol",j+1)
			Rate=comInvoke(objQuery.t4201,"GetFieldData","t4201OutBlock1", "rate",j+1)
		} else {
			break
		}
	}

	if(DoCnt>=10) {
		Data=matrix(rep(NA,7),nrow=1)		
	} else {	
		Data=cbind(Date,Open,High,Low,Close,Volume,Rate)
		Data=matrix(as.numeric(Data),nrow=1)
	}

	return(Data)
}

Cnt=500
Sdate=gsub("-","",Sys.Date()-Cnt)
SaveDataPath="C:/Git/Rds/"
SavedPriceList=list.files(paste(SaveDataPath,"Price/",sep=""))
SavedPriceList=gsub(".rds","",SavedPriceList)

CodeF=Code[sample(1:length(Code),50)]

for(i in c(1:length(CodeF))) {
	if(is.na(match(CodeF[i],SavedPriceList))==TRUE) {
		comInvoke(objQuery.t4201,"LoadFromResFile","C:/eBEST/xingAPI/Res/t4201.res")

		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "shcode", 0, CodeF[i]) #코드
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "gubun", 0, 2) #월:4
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "ncnt", 0, "")
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "qrycnt", 0, "")
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "tdgb", 0, "0")	
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "sdate", 0, Sdate)
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "edate", 0, "99999999")
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "cts_date", 0, "")
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "cts_time", 0, "")
		comInvoke(objQuery.t4201,"SetFieldData","t4201InBlock", "cts_daygb", 0, "")
		
		Sys.sleep(3)
		
		comInvoke(objQuery.t4201,"Request","False")
		
		Sys.sleep(2)
			
		LoopEnd=comInvoke(objQuery.t4201,"GetBlockCount","t4201OutBlock1")
		DoCnt=0

		repeat {
			DoCnt=DoCnt+1

			if(LoopEnd=="" | LoopEnd==0) {
				if(DoCnt>=10) {
					break
				}

				Sys.sleep(2)
				
				LoopEnd=comInvoke(objQuery.t4201,"GetBlockCount","t4201OutBlock1")
			} else {
				break
			}
		}

		if(DoCnt>=10) {
			message(sprintf("error!! %s will be passed!!",CodeF[i]))

			next
		} else {
			LoopEnd=as.numeric(LoopEnd)
			LoopEnd=LoopEnd-1
	
			PriceData=do.call(rbind,lapply(0:(LoopEnd-1),PastPriceFunc))
			PriceData=matrix(PriceData,ncol=7)
			colnames(PriceData)=c(CodeF[i],"Open","High","Low","Close","Volume","Rate")
		
			saveRDS(PriceData,paste(SaveDataPath,"Price/",CodeF[i],".rds",sep=""))
			message(sprintf("%d of %d [%s : done]",i,length(CodeF),CodeF[i]))	
		}
	} else {
		message(sprintf("[%s exist]",CodeF[i]))
	}
}
