#198

require(XLConnect)

numbers <- z.tot[,'number']
numbers <- as.character(numbers)


for (i in 259:length(numbers)){
   number <- numbers[i]
   print(paste0(i, ' Законопроект №', number))
   url <- paste0('http://asozd2.duma.gov.ru/main.nsf/%28SpravkaNew%29?OpenAgent&RN=', number)
   #url <- 'http://asozd2.duma.gov.ru/main.nsf/%28SpravkaNew%29?OpenAgent&RN=196586-6&02'
   urldoc <- htmlTreeParse(url, useInternalNodes = T)
   #немного мороки с кодировкой win-1251
   doc <- getURL(url, .encoding="UTF-8")
   doc <- iconv(doc, "windows-1251", "UTF-8")
   doc <- sub("windows-1251", "UTF-8", doc)
   tables <-  readHTMLTable(doc) # прочитать таблицы
   if (length(tables) >0){
     table <- tables[[2]] # выбрать вторую по счет
     aa <- names(table)
     table <- apply(table,2,as.character) #преобразовать в текстовые строки 
     if (class(table) == 'matrix'){
       table <- table[table[,1] != '',] #если первая 
       buff <- rbind(table, aa)
       nn <- buff[,1]
       buff <- buff[,2] 
       names(buff) <- nn
       
       buff <- c(buff, number)
       names(buff)[length(buff)] <- 'number'
         
       buff <- as.data.frame(t(buff))
       ext <- nm[!(nm %in% names(buff))]
       ext1 <- as.data.frame(matrix(nrow =1, ncol = length(ext), dimnames = list(NULL, 
                                                                                 c(ext))))
       buff <- cbind(buff, ext1)
     
       asozd <- rbind(asozd, buff)
      }
   }
}

names(asozd)[!names(asozd) %in% names(buff)]
#названия колонок, соответствующие блокам паспортных данных 
nm <- c('number', 
   'Субъект права законодательной инициативы',
   'Форма законопроекта',
   'Ответственный комитет',
   'Комитеты-соисполнители',
   'Отрасль законодательства',
   'Тематический блок законопроектов',
   'Профильный комитет',
   'Заключение Правительства РФ на законопроект',
    "Принадлежность к примерной программе",    
   'Предмет ведения',
    'Вопрос ведения',    
        "Пакет документов при внесении",
        "Альтернативные законопроекты",
        'Законопроекты, входящие в один "пакет" с данным'
        )

#создать пустой датафрейм
asozd <- as.data.frame(matrix(nrow =1, ncol = length(nm), dimnames = list(NULL, 
                                                                       c(nm))))

asozd[,'Законопроекты, входящие в один "пакет" с данным'] <- NA
asozd[,'Законопроекты, входящие в один \"пакет\" с данным'] <- NA
i <-1


all.df.2008.back <- all.df.2008
z.tot2 <- z.tot
z.tot <- as.data.frame(apply(z.tot, 2, as.character))
aa <- merge(z.tot, asozd, by = "number")
write.table(aa, file = 'data4export/total+asozd.csv', fileEncoding = "UTF8", 
          row.names = FALSE, sep = ';')
write.table(aa, file = 'data4export/total+asozd-2013.csv', fileEncoding = "UTF8", 
            row.names = FALSE, sep = '$')
write.table(aa, file = 'data4export/total+asozd-2013-RosGazeta.csv', fileEncoding = "UTF8", 
            row.names = FALSE, sep = '$')

write.table(aa, file = 'data4export/total+asozd-2013-President.csv', fileEncoding = "UTF8", 
            row.names = FALSE, sep = '$')

wb <- loadWorkbook ( 'data4export/asozd-2013.xlsx' , create = TRUE )
sheet <- 'data'
createSheet(wb, sheet)
writeWorksheet ( wb , aa, sheet , startRow = 1 , startCol = 1 ,
                 header = TRUE )
saveWorkbook(wb)



urldoc <- htmlTreeParse(doc, useInternalNodes = T)
xpathSApply(urldoc, "//div[@Class = 'data-block']", xmlValue ) 

ReadGksURL <- function(url){
  doc <- getURL(url, .encoding="UTF-8")
  doc <- iconv(doc, "windows-1251", "UTF-8")
  doc <- sub("windows-1251", "UTF-8", doc)
  webpage <- readLines(tc <- textConnection(doc))
  close(tc)
  tables <- readHTMLTable(doc)
  tables <- readHTMLTable(doc, as.data.frame = TRUE,  error=function(...){})
  return(tables)
}



