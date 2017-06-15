# to get CSSR at command line: ./CSSR alphabet.txt datafile.txt 2
library(caTools)
library(zoo)
source('functions.R')

setwd('~/Dropbox/new.projects/recurrence-bundles') 
fls = list.files('data.files')
dyad = loadFile(path=paste('data.files',fls[2],sep='/'))
windowedEvents = windowEvents(dyad)



# reduce the data...
mypca = princomp(dat)

# seek bend
plot(mypca$sdev,type='b')

# plot the "path" in lower dimensional space
plot(mypca$scores[,1],mypca$scores[,2],type='b')

# see how emotions load
row.names(mypca$loadings) = c(pcodes,acodes)

write.table(file='alphabet_txt',t(unique(ixesP)),row.names=F,col.names=F)
write.table(file='data_txt',t(ixesP),row.names=F,col.names=F)










