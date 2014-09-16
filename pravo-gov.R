#получения и обработка данных Информационно-правовая система «Законодательство России»
# pravo.gov.ru
# Салихов Марсель (marcel.salikhov@gmail.com)


require(XML)
require(stringr)
require (RCurl)
require(tm)



# 1. Получение имен законопроектов и ссылок на них из меню "Расширенный поиск"
# 
#исходный url из расширенного поиска
url <- 'http://www.pravo.gov.ru/proxy/ips/?searchres=&bpas=cd00000&a3=102000492&a3type=1&a3value=%D4%E5%E4%E5%F0%E0%EB%FC%ED%FB%E9+%E7%E0%EA%EE%ED+&a6=&a6type=1&a6value=&a15=&a15type=1&a15value=&a7type=3&a7from=&a7to=&a7date=01.01.2013&a8=&a8type=2&a1=&a0=&a16=&a16type=1&a16value=&a17=&a17type=1&a17value=&a4=102000038&a4type=1&a4value=%C4%E5%E9%F1%F2%E2%F3%E5%F2+c+%E8%E7%EC%E5%ED%E5%ED%E8%FF%EC%E8+&textpres=&sort=-7&x=55&y=5'
url <- 'http://www.pravo.gov.ru/proxy/ips/?searchres=&bpas=cd00000&a3=102000492&a3type=1&a3value=&a6=&a6type=1&a6value=&a15=&a15type=1&a15value=&a7type=3&a7from=&a7to=&a7date=01.01.2013&a8=&a8type=2&a1=&a0=&a16=&a16type=1&a16value=&a17=&a17type=1&a17value=&a4=&a4type=1&a4value=&textpres=&sort=7&x=44&y=14'
url <-'http://www.pravo.gov.ru/proxy/ips/?searchres=&bpas=cd00000&a3=&a3type=1&a3value=&a6=&a6type=1&a6value=&a15=&a15type=1&a15value=&a7type=1&a7from=&a7to=&a7date=&a8=&a8type=2&a1=%CE+%EC%E5%F0%E0%F5+%E2%EE%E7%E4%E5%E9%F1%F2%E2%E8%FF+%ED%E0+%EB%E8%F6%2C+%EF%F0%E8%F7%E0%F1%F2%ED%FB%F5+%EA+%ED%E0%F0%F3%F8%E5%ED%E8%FF%EC+%EE%F1%ED%EE%E2%EE%EF%EE%EB%E0%E3%E0%FE%F9%E8%F5+%EF%F0%E0%E2+%E8+%F1%E2%EE%E1%EE%E4+%F7%E5%EB%EE%E2%E5%EA%E0%2C+%EF%F0%E0%E2+%E8+%F1%E2%EE%E1%EE%E4+%E3%F0%E0%E6%E4%E0%ED+%D0%EE%F1%F1%E8%E9%F1%EA%EE%E9+%D4%E5%E4%E5%F0%E0%F6%E8%E8&a0=&a16=&a16type=1&a16value=&a17=&a17type=1&a17value=&a4=&a4type=1&a4value=&textpres=&sort=7&x=72&y=7'
url <-'http://www.pravo.gov.ru/proxy/ips/?list_itself=&bpas=cd00000&a3=102000492&a3type=1&a3value=&a6=&a6type=1&a6value=&a15=&a15type=1&a15value=&a7type=3&a7from=&a7to=&a7date=01.01.2013&a8=&a8type=2&a1=&a0=&a16=&a16type=1&a16value=&a17=&a17type=1&a17value=&a4=&a4type=1&a4value=&textpres=&sort=7&x=44&y=14&lstsize=200&start=0&page=first'
# выбираем только левую панель (список законов)
url<- gsub('searchres', 'list_itself', url)
url <- paste0(url, '&page=first')

#url <- 'http://www.pravo.gov.ru/proxy/ips/?list_itself=&bpas=cd00000&a3=102000492&a3type=1&a3value=%D4%E5%E4%E5%F0%E0%EB%FC%ED%FB%E9+%E7%E0%EA%EE%ED&a6=&a6type=1&a6value=&a15=&a15type=1&a15value=&a7type=4&a7from=01.01.2013&a7to=31.12.2013&a7date=&a8=&a8type=2&a1=&a0=&a16=&a16type=1&a16value=&a17=&a17type=1&a17value=&a4=&a4type=1&a4value=&textpres=&sort=7&x=61&y=12&page=first'
urldoc <- htmlTreeParse(url, useInternalNodes = T)
#xpathSApply(urldoc, "//div[@class = 'l_link']", xmlValue ) 

# преобразование кодировки кириллицы
doc <- getURL(url, .encoding="UTF-8")
doc <- iconv(doc, "windows-1251", "UTF-8")
doc <- sub("windows-1251", "UTF-8", doc)
urldoc <- htmlTreeParse(doc, useInternalNodes = T)

names <- xpathSApply(urldoc, "//div[@class = 'l_link']", xmlValue ) #названия законов

text_links <- xpathSApply(urldoc, "//div[@class = 'l_link']/a", 
            function(x) xmlGetAttr(x, 'href')) #ccылки на тексты законов
text_links <- gsub('docbody', 'doc_itself', text_links)


#2. Выделить текст закона в исходной редакции 

#ссылка на текст закона (пример)
id <- sub("^.*nd=([0-9]+).*", "\\1", text_links[1]) #выделить номер закона id
url2 <- paste0('http://www.pravo.gov.ru/proxy/ips/?doc_itself=&&nd=', id, 'rdk=0&&page=1&rdk=0')
#параметр rdk =0 обеспечивает отображение исходную редакцию закона



doc <- getURL(url2, .encoding="UTF-8")
doc <- iconv(doc, "windows-1251", "UTF-8")
doc <- sub("windows-1251", "UTF-8", doc)
urldoc <- htmlTreeParse(doc, useInternalNodes = T)
#сам текст законa
text <- htmlToText(doc)
str_length(text)
words <- wordsCount(text) # подсчитываем количество слов в 


#3. Прочитать мета-информацию о законе по его id
url_meta <- paste0('http://www.pravo.gov.ru/proxy/ips/?doc_itself=&&vkart=card&nd=',
                   id, '&&page=1&rdk=0&link_id=0')

doc <- getURL(url_meta, .encoding="UTF-8")
doc <- iconv(doc, "windows-1251", "UTF-8")
doc <- sub("windows-1251", "UTF-8", doc)
urldoc <- htmlTreeParse(doc, useInternalNodes = T)

name <- xpathSApply(urldoc, "//div[@class = 'DC_header']", xmlValue ) 
publication <- xpathSApply(urldoc, "//div[@id = 'pb']", xmlValue ) 
id2 <- sub("^.*№ ([0-9]+).*", "\\1", publication) 

keywords <- xpathSApply(urldoc, "//div[@id = 'klsl']", xmlValue ) 
otrasl <- xpathSApply(urldoc, "//div[@id = 'rubr']", xmlValue ) 

nm <- (c('id', 'id2', 'name', 'text', 'publication', 'keywords',
         'otrasl', 'words'))
fz <- as.data.frame(matrix(nrow =0, ncol = length(nm), dimnames = list(NULL, 
                                                                          c(nm))))
fz.buff <- cbind(id, id2, name, text, publication, keywords, otrasl, words)
fz.buff <- as.data.frame(fz.buff)
fz <- rbind(fz, fz.buff)



