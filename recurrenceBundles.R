#
#  things to discuss... 
#  weird Part 03 in 12.txt... why?
#

# to get CSSR at command line: 
library(caTools)
library(zoo)
source('functions.R')

setwd('~/Dropbox/new.projects/recurrence-bundles') 

data.folder = '/Users/rickdale/Dropbox/new.projects/submitted\ or\ complete\ or\ just\ forgotten/main.emotion.analysis/chosen\ dyads/All'
#fls = list.files('data.files')
fls = list.files(data.folder)
resultsAll = c()

dyad.info = read.table('Key Grouping Variables.csv',header=T,stringsAsFactors=F,sep=',')

for (fl in fls) {
  
  print(fl)
  #fl = fls[1]
  dyad = loadFile(path=paste(data.folder,fl,sep='/'))
  windowedEvents = windowEvents(dyad)
  
  # reduce the data...
  #mypca = princomp(windowedEvents$dat)
  # seek bend
  #plot(mypca$sdev,type='b')
  # plot the "path" in lower dimensional space
  #plot(mypca$scores[,1],mypca$scores[,2],type='b')
  
  # note that windowedEvents generates the indices (used for column specs); we use them here
  # as the alphabet for CSSR
  write.table(file='alphabet_txt',t(unique(windowedEvents$ixesP)),row.names=F,col.names=F)
  data_file = paste(gsub('.txt','',fl),'_cssr',sep='')
  write.table(file=data_file,t(windowedEvents$ixesP),row.names=F,col.names=F)
  system(paste('./CSSR alphabet_txt ',data_file,' 2',sep=''),ignore.stdout = TRUE)
  
  results = read.table(paste(data_file,'_info',sep=''),sep=':')
  colnames(results) = list('key','value')
  # let's get some numbers from the CSSR results file
  to.store = data.frame(t(as.numeric(as.character(results$value[7:13]))))
  colnames(to.store) = as.character(results$key[7:13])
  to.store$len = length(windowedEvents$ixesP)
  to.store$id = as.numeric(unlist(strsplit(fl,'\\.'))[1])
  
  # let's align the dyad info file with this dyad and get their age, etc.
  info = dyad.info[which(dyad.info$ID==to.store$id),]
  
  resultsAll = rbind(resultsAll,cbind(to.store,info))
  system('mv *_cssr* results/')

}

plot(resultsAll$`Statistical Complexity`,resultsAll$`Number of Inferred States`)

cor.test(resultsAll$age,resultsAll$`Statistical Complexity`)
cor.test(resultsAll$meansat,resultsAll$`Statistical Complexity`)
cor.test(resultsAll$age,resultsAll$`Number of Inferred States`)

