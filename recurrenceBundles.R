 

setwd('~/Dropbox/new.projects/recurrence-bundles')
a = read.table('low-satisfaction-dyad.txt',header=T,sep='\t',stringsAsFactors=FALSE)
colnames(a) = list('s','P','A')
pcodes = unique(a$P)
acodes = unique(a$A)

# the dichotomous codes
dichsP = matrix(0,nrow=dim(a)[1],ncol=length(pcodes))
dichsA = matrix(0,nrow=dim(a)[1],ncol=length(acodes))

# indices for the dichotomous codes
ixesP = unlist(lapply(a$P,function(x) {
  which(pcodes==x)
}))
ixesA = unlist(lapply(a$A,function(x) {
  which(acodes==x)
}))
plot(ixesP,ixesA,type='b',col=rgb(0,0,0,.5)) # state space

# add the 1 for the emotion
nSecs = 10
Alagged = matrix(0,nrow=nrow(dichsA)-nSecs+1,ncol=ncol(dichsA)*nSecs)
Plagged = matrix(0,nrow=nrow(dichsP)-nSecs+1,ncol=ncol(dichsP)*nSecs)
for (i in 1:length(ixesP)) {
  dichsP[i,ixesP[i]] = 1
  dichsA[i,ixesA[i]] = 1
  if (i >= nSecs) {
    Alagged[i-nSecs+1,] = as.vector(dichsA[(i-nSecs+1):i,])
    Plagged[i-nSecs+1,] = as.vector(dichsP[(i-nSecs+1):i,])
  }
}
lagged = cbind(Alagged,Plagged)
pcasol = princomp(lagged)


# get a rolling mean of 30s
library(caTools)
library(zoo)


allDat = cbind(dichsP,dichsA)






for (i in 1:dim(allDat)[2]) {
  allDat[,i] = allDat[,i] / mean(allDat[,i])
}




dat = runmean(allDat,k=100)
cc = 1-cor(t(dat))
dissim = as.dist(cc)
plot(hclust(dissim),col=rgb(allDat[,1],allDat[,2],allDat[,3]))

# get the bundles!
mypca = princomp(dat)

# seek bend
plot(mypca$sdev,type='b')

# plot the "path" in lower dimensional space
plot(mypca$scores[,1],mypca$scores[,2],type='b')

# see how emotions load
row.names(mypca$loadings) = c(pcodes,acodes)

plot(mypca$loadings[,1],mypca$loadings[,2],col='white')
text(mypca$loadings[,1],mypca$loadings[,2],c(pcodes,acodes))

plot(mypca$loadings[,3],mypca$loadings[,4],col='white')
text(mypca$loadings[,3],mypca$loadings[,4],c(pcodes,acodes))












