rm(list=ls(all=TRUE))

library(rcom)
library(fPortfolio)

obj=comCreateObject("XA_Session.XASession")
objQuery.t0424=comCreateObject("XA_DataSet.XAQuery")
objQuery.CSPAT00600=comCreateObject("XA_DataSet.XAQuery")


#######################
##Log In
#######################
comInvoke(obj,"DisconnectServer")
comInvoke(obj,"ConnectServer","demo.etrade.co.kr",20001)

ID="your ID"
PW1="your pwd"
AccNo="your account number"
AccPW="your acoount pwd"

Login=comInvoke(obj,"Login",ID,PW1,"",0,"False")

repeat {
	if(comInvoke(obj,"GetClientIP")=="") {
		Sys.sleep(1)
	} else {
		break
	}
}

Sys.sleep(5)


#######################
##Get Account
#######################
comInvoke(objQuery.t0424,"LoadFromResFile","C:/eBEST/xingAPI/Res/t0424.res")
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "accno", 0, AccNo) 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "passwd", 0, AccPW) 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "prcgb", 0, "1") 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "chegb", 0, "2") 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "dangb", 0, "0") 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "charge", 0, "0") 
comInvoke(objQuery.t0424,"SetFieldData","t0424InBlock", "cts_expcode", 0, "") 

Sys.sleep(2)

DoCntA=0

repeat {
	comRqst=comInvoke(objQuery.t0424,"Request","FALSE")

	if(comRqst<0) {		
		Sys.sleep(2)

		comRqst=comInvoke(objQuery.t0424,"Request","FALSE")
	} else {
		break
	}
}

Sys.sleep(2)

repeat {
	IndivInvestPrice=comInvoke(objQuery.t0424,"GetFieldData","t0424OutBlock", "sunamt",0)
	IndivInvestPrice=as.numeric(IndivInvestPrice)

	if(is.na(IndivInvestPrice)==TRUE) {		
		Sys.sleep(2)

		IndivInvestPrice=comInvoke(objQuery.t0424,"GetFieldData","t0424OutBlock", "sunamt",0)
		IndivInvestPrice=as.numeric(IndivInvestPrice)
	} else {
		break
	}
}


#######################
##Ordering
#######################
OptW=readRDS("C:/Git/Rds/reEffWeight.rds")
nyClose=readRDS("C:/Git/Rds/nyClose.rds")

Tclose=nyClose[nrow(nyClose),names(OptW)]
StockInvPrice=as.numeric(OptW)*IndivInvestPrice
nVolume=ceiling(StockInvPrice/Tclose)
beZvol=which(nVolume==0)

if(is.null(beZvol)==TRUE) { 
	nVolume=nVolume[-beZvol] 
	Tclose=Tclose[-beZvol]]
}

for(i in c(1:length(nVolume))) {
	comInvoke(objQuery.CSPAT00600,"LoadFromResFile","C:/eBEST/xingAPI/Res/CSPAT00600.res")
	
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "AcntNo", 0, AccNo)
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "InptPwd", 0, AccPW)
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "IsuNo", 0, paste("A",names(nVolume),sep=""))
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "OrdPrc", 0, Tclose[i])
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "OrdQty", 0, nVolume[i])
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "BnsTpCode", 0, 2) #매수:2
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "BnsTpCode", 0, "00") #지정가
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "MgntrnCode", 0, "000")
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "LoanDt", 0, "")
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "OrdCndiTpCode", 0, "0")
	
	nSuccess=comInvoke(objQuery.CSPAT00600,"Request","FALSE")
	
	Sys.sleep(1)
	
	#reset
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "IsuNo", 0, "")
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "OrdPrc", 0, "")
	comInvoke(objQuery.CSPAT00600,"SetFieldData","CSPAT00600InBlock1", "OrdQty", 0, "")	
}

