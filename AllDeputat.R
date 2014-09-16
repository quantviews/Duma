library(plyr)
library(reshape2)

depAll.backup <- depAll

b <- lapply(depAll, ldply)
melt(x)


buff <- lapply(depAll, function(x){
  if (x$position =='Депутат ГД'){
  nm <- names(x)
  ll <- length(x$factions)
  nm <- c(nm[-length(nm)], paste0('faction',1:ll))
  b <- as.data.frame(x[nm[1:4]])
  bb <- sapply(x$factions, function(x) x['name'])
  b[,nm[5:(4+ll)]] <- bb
  #b <- do.call(rbind, x)
  return(b)}
  
})
bf <- ldply(buff,  data.frame)
parties <- levels(factor(as.vector(bf[,5:17])[!is.na(as.vector(bf[,5:17]))]))
n.changes <- apply(bf[,5:17], 1, function(x){
  x <- as.character(x)
  x <- x[!is.na(x)]
  length(unique(x) %in% parties)
})
bbb <- bf[which(n.changes==6),]
b <- as.data.frame(x)
lapply(x, d)
bf  <- do.call(rbind, buff)

party <- sapply(depList, function(x) x$factions[[length(x$factions)]][2])
#sapply(depList, function(x) x$factions[[length(x$factions)]]$id)
#str(aa)
party[sapply(party, is.null)] <- NA # преобразовать пустые элементы списка в NA 
party <- unlist(party) #преобразовать список в вектор 
name <- sapply(depList, function(x) x$name) #получить ФИО депутата
id <- sapply(depList, function(x) x$id) #получить и.д.
position <- sapply(depList, function(x) x$position) #должность депутата
depList <- data.frame(name, id, position, party) #объединить вектора в датафрейм 
depList <- subset(depList, position == 'Депутат ГД') # оставить только ГД, без Совета Федерации
depList$party <- factor(depList$party)
levels(depList$party) <- c('ЕР', "КПРФ", "ЛДПР", "СР")