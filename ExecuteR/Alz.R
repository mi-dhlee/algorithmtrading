rm(list=ls(all=TRUE))

library(fPortfolio)
library(e1071)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(parallel)


###################
##Reading Price
###################
TotalDataPath="C:/Git/Rds/"

rpList=list.files(paste(TotalDataPath,"Price/",sep=""))
tc=gsub(".rds","",rpList)

PriceList=as.list(rep(NA,length(tc)))

for(i in c(1:length(tc))) {
	prices=readRDS(paste(TotalDataPath,"Price/",rpList[i],sep=""))

	if("try-error" %in% class(prices)) {
		PriceList[[i]]=NA
	} else {
		timeM=as.character(prices[,tc[i]])
		rownames(prices)=timeM
		Preprices=prices[,2:ncol(prices)]
		prices=matrix(Preprices,ncol=6,dimnames=list(prices[,1],colnames(prices)[-1]))
		
		Dnames=order(rownames(prices))
		
		prices=matrix(prices[Dnames,],ncol=6,
			dimnames=list(rownames(prices)[Dnames],colnames(prices)))

		PriceList[[i]]=as.timeSeries(prices)		
	}
}


###################
##Cleansing Price
###################
n=length(PriceList)
Prices=as.list(rep(NA,n))

for(i in c(1:n)) {
	naMatch=PriceList[[i]][is.na(PriceList[[i]])]

	if(is.null(naMatch)==TRUE) {
		DataSet=PriceList[[i]]
		DataSet[is.na(DataSet)]=0

		DataSetDate=as.numeric(as.Date(rownames(DataSet)))
		DateCheck=which(diff(DataSetDate)>=21)

		if(identical(integer(0),DateCheck)==TRUE & length(DataSetDate)>=21) {		
			DSmat=matrix(DataSet,ncol=ncol(DataSet),
				dimnames=list(rownames(DataSet),colnames(DataSet)))
				
			dsr=as.numeric(DSmat[,"Rate"])
			A=which(dsr!=0); DoA=1;
			
			if(identical(integer(0),A)==FALSE) {
				if(length(A)==1) {
					if(A==1) {
						DSmat[1,"Rate"]=0
						
						DoA=0
					} else if(A==nrow(DSmat)) {
						B=as.numeric(DSmat[nrow(DSmat),"Rate"])
						
						DSmat[1:(nrow(DSmat)-1),"Rate"]=B
						DSmat[nrow(DSmat),"Rate"]=0
						
						DoA=0
					}
				}
				
				if(DoA==1) {
					for(j in c(1:length(A))) {
						if(j==1) {
							B=c(1:(A[j]-1))							
						} else if(j!=length(A)) {
							B=c((A[j-1]+1):(A[j]-1))							
						}
				
						DSmat[B,"Rate"]=DSmat[A[j],"Rate"]
					}
					
					DSmat[A,"Rate"]=0
				}
				
				for(j in c(1:4)) {
					DSmat[,j]=floor(DSmat[,j]*(1+DSmat[,"Rate"]/100))
				}
			}
			
			DataSet=as.timeSeries(DSmat[,1:5])
			
			Close=DataSet[,c("Close")]
			Volume=DataSet[,c("Volume")]

			mClose=mean(Close[(nrow(DataSet)-20):nrow(DataSet),])
			mVolume=mean(Volume[(nrow(DataSet)-20):nrow(DataSet),])

			if(mVolume>=10000 & mClose*mVolume>=10000*10000) {
				Prices[[i]]=list(name=tc[i],price=DataSet)			
			} else {
				Prices[[i]]=NA
			}
		}
	}
}

Prices=Prices[is.na(Prices)==FALSE]

nyName=do.call(c,lapply(Prices,function(x) { return(x$name) }))

nyOpen=do.call(cbind,lapply(Prices,function(x) { return(x$price[,"Open"]) }))
nyOpen=nyOpen[rowSums(is.na(nyOpen))==0,]
colnames(nyOpen)=nyName

nyHigh=do.call(cbind,lapply(Prices,function(x) { return(x$price[,"Close"]) }))
nyHigh=nyHigh[rowSums(is.na(nyHigh))==0,]
colnames(nyHigh)=nyName

nyLow=do.call(cbind,lapply(Prices,function(x) { return(x$price[,"Low"]) }))
nyLow=nyLow[rowSums(is.na(nyLow))==0,]
colnames(nyLow)=nyName

nyClose=do.call(cbind,lapply(Prices,function(x) { return(x$price[,"Close"]) }))
nyClose=nyClose[rowSums(is.na(nyClose))==0,]
colnames(nyClose)=nyName

nyVolume=do.call(cbind,lapply(Prices,function(x) { return(x$price[,"Volume"]) }))
nyVolume=nyVolume[rowSums(is.na(nyVolume))==0,]
colnames(nyVolume)=nyName

return=diff(log(nyClose))*100
return[1,]=0
return=return[,colSums(is.na(return))==0]

n=ncol(return)


###################
##Reading Stat
###################
rpList=list.files(paste(TotalDataPath,"Stat/",sep=""))

Sc=gsub(".rds","",rpList)
StatList=as.list(rep(NA,length(Sc)))

Rnames=rownames(return)
Bnames=strsplit(Rnames,"-")
BnVec=unlist(lapply(Bnames,function(x) { paste(c(x[1],x[2]),collapse="") }))

for(i in c(1:length(Sc))) {
	Stats=readRDS(paste(TotalDataPath,"Stat/",rpList[i],sep=""))
	
	Snames=Stats[,1]
	SnVec=rep(NA,length(Snames))
	
	for(j in c(1:length(Snames))) {
		A=which(BnVec==Snames[j])
		
		if(identical(integer(0),A)==FALSE) {
			SnVec[j]=Rnames[max(A)]
		}
	}
	
	Stats[,1]=SnVec
	Stats=matrix(Stats[rowSums(is.na(Stats))==0,],ncol=2)
	
	StDat=matrix(as.numeric(Stats[,2]),ncol=1,dimnames=list(Stats[,1],Sc[i]))
	StatList[[i]]=as.timeSeries(StDat)
}

Stats=do.call(cbind,StatList)
Stats=Stats[rowSums(is.na(Stats))==0,]


###################
##Data Set
###################
df=as.list(rep(NA,n))
dfTest=as.list(rep(NA,n))
dfVdn=as.list(rep(NA,n))

subStat=diff(log(Stats))*100

for(i in c(1:n)) {	
	#Technical Indicator
	Kst=as.timeSeries(KST(nyClose[,i],n=c(1,2),nROC=c(1,2),nSig=2)[,2])
	Roc=ROC(nyClose[,i])
	
	FastD=stoch(Roc,nFastK=5,nFastD=20,nSlowD=20,maType=list(list(EMA), list(EMA, wilder=TRUE), list(EMA)))
	StochK=FastD[,c("fastD")]
	StochD=EMA(StochK,n=5)
	Stoch=diff(StochK-StochD)	
	
	Obv=OBV(as.vector(nyClose[,i]),as.vector(nyVolume[,i]))
	Obv=as.timeSeries(matrix(Obv,ncol=1,dimnames=list(rownames(nyClose),"OBV")))
	
	#Stat
	sStatCov=as.timeSeries(matrix(dfpp[,2],ncol=1,dimnames=list(rownames(return),colnames(subStat))))
		
	#Making Example Data Frame
	Y=lag(return[,i],-1)

	dfpp=matrix(cbind(return[,i],subStat),ncol=2)
	
	for(j in c(2:nrow(dfpp))) {
		a=dfpp[j,2]; b=dfpp[j-1,2];
		
		if(is.na(b)==FALSE & is.na(a)==TRUE) {
			dfpp[j,2]=b
		}
	}
	
	dfPre=cbind(Y,sStatCov,Kst,Roc,Stoch,Obv)
	dfPre[nrow(dfPre),1]=0
	dfPre=dfPre[rowSums(is.na(dfPre))==0,]
	colnames(dfPre)=c(colnames(return)[i],colnames(sStatCov),"KST","ROC","STOCH","OBV")
	
	df[[i]]=dfPre
	dfTest[[i]]=dfPre[1:round(nrow(dfPre)*0.6),]
	dfVdn[[i]]=dfPre[(round(nrow(dfPre)*0.6)+1):nrow(dfPre),]		
} 


###################
##Modeling
###################
RstList=as.list(rep(NA,n))

for(i in c(1:n)) {
	dTI=dfTest[[i]]; dVI=dfVdn[[i]];
	
	#Generating Dependant
	cl=makeCluster(mc <- getOption("cl.cores", 4))

	ignore=clusterEvalQ(cl, {
		library(fPortfolio)
		library(e1071)
	})

	clusterExport(cl=cl, varlist=c(
		"dTI","dVI","return"
	))

	GsimFunc=function(j) {
		LTdat=rbind(dTI[j:nrow(dTI),],dVI[1:j,])
		
		LearnDat=as.timeSeries(matrix(LTdat[1:(nrow(LTdat)-1),],ncol=ncol(LTdat),
			dimnames=list(rownames(LTdat)[1:(nrow(LTdat)-1)],colnames(dTI))))
		colnames(LearnDat)[1]="Dependent"	
		
		TestDat=as.timeSeries(matrix(LTdat[nrow(LTdat),],ncol=ncol(LTdat),
			dimnames=list(rownames(LTdat)[nrow(LTdat)],colnames(dTI))))
		colnames(TestDat)[1]="Dependent"
		
		#Add New Feature
		FL=cbind(LearnDat[,1],return)
		FL=FL[rowSums(is.na(FL))==0,]
		
		sFL=lapply(2:ncol(FL),function(k) {
			subModel=summary(lm(FL[,1]~FL[,k]))
			
			if(subModel$coefficient[2,4]<=0.1) {
				return(k)
			} else {
				return(NA)
			}
		})
		
		sFL=unlist(sFL[is.na(sFL)==FALSE])
		
		if(is.null(sFL)==FALSE) {
			LearnDat=as.timeSeries(cbind(LearnDat,FL[,sFL]))
			TestDat=as.timeSeries(cbind(TestDat,return[rownames(TestDat),colnames(FL[,sFL])]))
		}
		
		#Start!
		#tuned=try(tune.svm(Dependent~.,data=LearnDat,gamma=10^(-2:0),cost=10^(0:2)),silent=T)

		#if("try-error" %in% class(tuned)) {
		#	GammaCost=matrix(c(0.01,10),nrow=1,ncol=2)
		#} else {
		#	GammaCost=cbind(tuned$best.parameters$gamma,tuned$best.parameters$cost)
		#}

		#Model=try(svm(Dependent~.,data=LearnDat,gamma=GammaCost[,1],cost=GammaCost[,2]),silent=T)
		Model=try(svm(Dependent~.,data=LearnDat),silent=T)
		
		if("try-error" %in% class(Model)) {
			SvmPred=as.timeSeries(matrix(0,nrow=1,ncol=1,
				dimnames=list(rownames(TestDat),"Pred")))
		} else {
			SvmPred=as.timeSeries(predict(Model,TestDat[,-1]))
			colnames(SvmPred)="Pred"			
		}
	
		SubRst=cbind(TestDat[,1],SvmPred)
		return(SubRst)
	}
	
	GsimList=parLapply(cl,1:nrow(dVI),GsimFunc)
	stopCluster(cl)
	
	GsimMat=do.call(rbind,GsimList)
	colnames(GsimMat)=c(colnames(df[[i]])[1],"Pred")
	
	RstList[[i]]=GsimMat
}

RstList=RstList[is.na(RstList)==FALSE]

RstHit=lapply(RstList,function(x) {
	aX=matrix(x[-which(x[,1]==0 | x[,2]==0),],ncol=2)		
	A=mean(ifelse(aX[,1]*aX[,2]>0,1,0))
	
	return(A)
})

#Check Performance
A=mean(unlist(RstHit))
print(A)

#Find Up Value
RstUp=lapply(RstList,function(x) {	
	A=as.numeric(x[nrow(x),2])
	
	if(A>0) { 
		return(colnames(x)[1])
	} else {
		return(NA)
	}
})

RstUp=unlist(RstUp[is.na(RstUp)==FALSE])


###################
##Resampled Efficiency
###################
nAA=nrow(dfTest[[1]])
RetAA=return[(nrow(return)-nAA+1):nrow(return),RstUp]

nSamp=100
ResMat=matrix(0,nrow=nSamp,ncol=ncol(RetAA),
	dimname=list(NULL,colnames(RetAA)))

for(i in c(1:nSamp)) {
	beRand=sort(sample(1:length(RstUp),min(5,length(RstUp))))
	RbR=RstUp[beRand]

	numAsset=length(beRand)
	minW=rep(0.01,numAsset)
	maxW=rep(1,numAsset)

	nAss=c(1:numAsset)
	constMin=paste("minW[", nAss, "]=", minW, sep="")
	constMax=paste("maxW[", nAss, "]=", maxW, sep="")
	constraints=c(constMin,constMax)
	
	MVP=minvariancePortfolio(RetAA[,RbR],spec=portfolioSpec(),constraints=constraints)
	gWmvp=getWeights(MVP)
	
	ResMat[i,names(gWmvp)]=gWmvp
}

reEffWeight=colMeans(ResMat)

saveRDS(reEffWeight,"C:/Git/Rds/reEffWeight.rds")
saveRDS(nyClose,"C:/Git/Rds/nyClose.rds")








