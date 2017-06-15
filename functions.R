windowEvents = function(dyad) {
  # the dichotomous codes
  a = dyad$a
  pcodes = dyad$pcodes
  acodes = dyad$acodes
  
  dichsP = matrix(0,nrow=dim(a)[1],ncol=length(pcodes))
  dichsA = matrix(0,nrow=dim(a)[1],ncol=length(acodes))
  
  # indices for the dichotomous codes
  ixesP = unlist(lapply(a$P,function(x) {
    which(pcodes==x)
  }))
  ixesA = unlist(lapply(a$A,function(x) {
    which(acodes==x)
  }))
  #plot(ixesP,ixesA,type='b',col=rgb(0,0,0,.5)) # state space
  
  # add the 1 for the emotion; creates lagged version of matrix... 
  #nSecs = 10
  #Alagged = matrix(0,nrow=nrow(dichsA)-nSecs+1,ncol=ncol(dichsA)*nSecs)
  #Plagged = matrix(0,nrow=nrow(dichsP)-nSecs+1,ncol=ncol(dichsP)*nSecs)
  #for (i in 1:length(ixesP)) {
  #  dichsP[i,ixesP[i]] = 1
  #  dichsA[i,ixesA[i]] = 1
  #  if (i >= nSecs) {
  #    Alagged[i-nSecs+1,] = as.vector(dichsA[(i-nSecs+1):i,])
  #    Plagged[i-nSecs+1,] = as.vector(dichsP[(i-nSecs+1):i,])
  #  }
  #}
  #lagged = cbind(Alagged,Plagged)
  #pcasol = princomp(lagged)
  
  # put 'em together
  allDat = cbind(dichsP,dichsA)
  
  # get new score based on inversion--mean
  for (i in 1:dim(allDat)[2]) {
    allDat[,i] = allDat[,i] / mean(allDat[,i])
  }
  
  # rolling mean of 60 seconds
  dat = runmean(allDat,k=60)  
  
  return(list(dat=dat,ixesP=ixesP,ixesA=ixesA))
}

loadFile = function(path,re.codes='SPAFF categories.csv') {
   # import sample dyad
   a = read.table(path,header=T,sep='\t',stringsAsFactors=FALSE,skip=1)
   # seconds into interaction, parent, adolescent
   colnames(a) = list('s','P.orig','A.orig')
   # a$P = gsub('P ','',a$P.orig) # remove designation (for overlap)
   # a$A = gsub('A ','',a$A.orig)
   
   # let's recode using SPAFF categories
   codes = read.csv(re.codes,header=T,stringsAsFactors=F)
   a$P = unlist(apply(a,1,function(x) {
     ix = which(tolower(codes$SPAFF.CODE)==tolower(x['P.orig']))
     if (length(ix)==0) {
       print(paste('WTF:',x['P.orig']))
     } else {
       return(codes$Category[ix])
     }
   }))
   a$A = unlist(apply(a,1,function(x) {
     ix = which(tolower(codes$SPAFF.CODE)==tolower(x['A.orig']))
     if (length(ix)==0) {
       print(paste('WTF:',x['A.orig']))
     } else {
       return(codes$Category[ix])
     }
   }))
   pcodes = unique(a$P) # unique SPAFF codes shown in this interaction
   acodes = unique(a$A)   
   return(list(pcodes=pcodes,acodes=acodes,a=a))
}