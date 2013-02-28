# Многомерное шкалирование для анализа результатов голосования в ГД РФ за 2012 года 
require(reshape2)
require(ggplot2)
# Функция, траснформирующая количественные коды голосования в факторы 
SimplifyVotes <- function(result){
  res.q <- vector(length = length(result))
  res.q <- ifelse(result == 'за', 1, res.q)
  res.q <- ifelse(result == 'возд', -2, res.q)
  res.q <- ifelse(result == 'не голосовал', NA, res.q)
  res.q <- ifelse(result == 'против', -1, res.q)
  return(res.q)
}

#загрузить данные 
all.votes <- read.csv(file='data4export/votes2012.csv')
laws.2012 <- read.csv(file='data4export/laws2012.csv')
depDuma <- read.csv(file = 'data/DeputatDuma.csv', stringsAsFactors = FALSE)


all.votes$url <- as.factor(all.votes$url)
levels(all.votes$url)

result <- (all.votes$result)[1:100]

all.votes$res.q <- 0
all.votes$res.q <- SimplifyVotes(all.votes$result)

res <- by(all.votes[,c('id', 'result')], 
          INDICES= list(all.votes$result), table)
res <- table(all.votes[,c('id', 'result')])
res <- as.data.frame(res)
res <- merge(res, depDuma[,c('id', 'sname', 'party')], by = 'id', all.x= TRUE)

buff <- subset(res, result == 'против')
buff <- (buff[order(buff$Freq, decreasing = TRUE  ),])
top5 <- head(buff)
res.p <- ggplot(data = buff, aes(x= reorder(id, Freq), y = Freq, group = party))
res.p + geom_point(aes(colour = party))+
  scale_y_log10()+
  labs(x='', y = 'количество голосований', colour = '')+
  scale_colour_manual(name = '', values = c('ЕР' = '#4d6b8d', 'КПРФ' = '#bf0d0d', 
                                            'ЛДПР' = '#fad000', 'СР' = '#e6871d' ))+
  theme_classic()+
  geom_text(data = top5, aes(label = sname), hjust = 0.5, vjust = 0.5)

hist(res$за)

v.unique <- unique.data.frame(all.votes)
subset(v.unique, id == '99112236')
sum(all.votes$res.q)
rollcall <- dcast(data = v.unique[, c('id', 'url','res.q')], id ~ url, fun.aggregate= sum)
rollcall2 <- dcast(data = v.unique[, c( 'url','id', 'res.q')], url ~ id, fun.aggregate= sum)
row.names(rollcall2) <- rollcall2$url
rollcall2 <- rollcall2[,-1]
rollcall2 <- apply(rollcall2,2, as.factor)
yeas <- apply(rollcall2,2, function(x) length(x[x == '1']))
yeas <- yeas[order(yeas)]
plot(yeas)
nays <- apply(rollcall2,2, function(x) length(x[x == '-1']))
nays[order(nays)]

vozd <- apply(rollcall2,2, function(x) length(x[x == '-2']))
vozd[order(vozd)]
subset(rollcall, rollcall == 2)
aa <- as.matrix(rollcall)
aa <- apply(aa, 2, as.numeric)
aa[!(aa == 0 | aa == 1 | aa == -1)]
ab <- aa %in% c('-1', '0', '1')
subset(rollcall, id =='99112236')

row.names(rollcall) <- rollcall$id
rollcall <- rollcall[, -1]
rollcall <- as.matrix(rollcall)
rm(aa)
rollcall.dist <- dist(rollcall %*% t(rollcall))
aa<- as.matrix(rollcall.dist)
rollcall.mds <- as.data.frame(cmdscale(rollcall.dist,k=2))
names(rollcall.mds) <- c('x', 'y')
rollcall.mds$id <- row.names(rollcall)
sname <- strsplit(as.character(depDuma$name), ' ')
depDuma$sname <- sapply(sname, function(x){ paste0(x[[1]], ' ', substr(x[[2]],1,1),'.', substr(x[[3]],1,1),'.')})
rollcall.mds <- merge(rollcall.mds, depDuma, by = 'id')
summary(rollcall.mds)
rollcall.mds$x <- rollcall.mds$x- min(rollcall.mds$x)+1
plot(rollcall.mds$x)
rollcall.mds$y <- rollcall.mds$y-min(rollcall.mds$y)+1

p <- ggplot(data = subset(rollcall.mds), aes(x=x, y=y, group = party))+scale_size(range=c(2,2))
p+geom_jitter(aes(color = party))+ief.theme+geom_text(aes(label = sname), size = 4)
openGraph(width = 6, height = 6)
ggsave(file = 'graph/mds-2012.png', dpi = 300)

votes <- all.votes
votes.table <- dcast(data.frame(id=votes$id, url=votes$url, result=as.integer(factor(votes$result))), url~id)

votes.table[votes.table==2]
head(arrayInd(which(votes.table==2), dim(votes.table), .dimnames=dimnames(votes.table), useNames=T))

class(all.votes$url)
v.unique <- unique.data.frame(votes)
v.result[v.unique$result=="возд"]<- 0

v.unique <- unique.data.frame(votes)
v.unique$result <- v.unique$res.q
v.result<-v.unique$result
v.result[v.unique$result=="возд"]<- 0
v.result[v.unique$result=="против"]<- -1
v.result[v.unique$result=="не голосовал"]<- 0
v.result[v.unique$result=="за"]<- 1
votes.table <- acast(data.frame(number=v.unique$number, id=v.unique$id, url=v.unique$url, result=as.integer(v.result)), number+url~id)
res.prcomp<-prcomp(na.omit(votes.table))
plot3d(res.prcomp$rotation[,1], res.prcomp$rotation[,2], res.prcomp$rotation[,3])
plot3d(res.prcomp$rotation[,4], res.prcomp$rotation[,2], res.prcomp$rotation[,3])
plot3d(res.prcomp$x[,1], res.prcomp$x[,2], res.prcomp$x[,3])
summary(res.prcomp)
cand <- data.frame(id=v.unique$id, party=v.unique$party)
cand.table<-unique.data.frame(cand)
plot3d(res.prcomp$rotation[,4], res.prcomp$rotation[,2], res.prcomp$rotation[,3], col=c("red","green","blue","magenta")[factor(cand.table$party)])
arrayInd(which(votes.table==2), dim(votes.table), .dimnames=dimnames(votes.table), useNames=T)

rm(votes.table)