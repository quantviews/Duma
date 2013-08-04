# File-Name:       DumaFunctions.R           
# Purports:        Набор функций для получения данных по законопроектной деятельности ГосДумы РФ 
# Date:            2013-02-15                                
# Author:          Salikhov Marcel 
# Email:           marcel.salikhov@gmail.com
# Web-site:       quantviews.blogspot.com

require(RJSONIO)
require(RCurl)
require(XML)
require(plyr)
require(ggplot2)

#Функция возвращает ссылки на ссылки голосований по законопроекту, которые имеет значение number 
# number - номер законопроекта в vote.duma.gov.ru
#links - вектор ссылок на голосований по данному законопроекту 
GetVotesByNumber <- function(number){
  url <- paste('http://vote.duma.gov.ru/?convocation=AAAAAAA6&number=', number,'&sort=date_desc', sep ='')
  urldoc <- htmlTreeParse(url, useInternalNodes = T)
  #Если голосований по законопроекту больше 20, то они разделятеся на несколько страниц 
  pages <- xpathSApply(urldoc, "//a[contains(@href, 'page=')]", xmlAttrs ) 
  pages <- (unlist(pages)) #преобразуем список в вектор
  pages <- pages[names(pages) == 'href'] #оставляем только ссылку
  max.page <- 0 # по умолчанию страница с результатами только одна
  if (length(pages>0)) { # но если их больше 
    alist <- strsplit(pages, 'page=') #тогда определяем максимальную страницу                     
    max.page<-max(as.numeric(do.call(rbind, alist)[,2]))} #
  #получение ссылки и названия голосований для первой страницы 
  links <- xpathSApply(urldoc, "//a[contains(@href, 'vote')]", xmlAttrs )
  names <- xpathSApply(urldoc, "//a[contains(@href, 'vote')]", xmlValue ) 
  if (max.page>0) { #если страниц несколько 
    for (i in 2:max.page) { #то проходимся по ним в цикле 
        url <- paste0('http://vote.duma.gov.ru/?convocation=AAAAAAA6&number=', number,'&sort=date_desc&page=', i)
        urldoc <- htmlTreeParse(url, useInternalNodes = T)
        links.current <- xpathSApply(urldoc, "//a[contains(@href, 'vote')]", xmlAttrs )
        links <- c(links, links.current)
        names.current <- xpathSApply(urldoc, "//a[contains(@href, 'vote')]", xmlValue ) 
        names <- c(names, names.current)
      #cat('Страница', i, 'Количество ссылок', length(links.current), '\n')
      
    }
  }
  aa <- data.frame(links, names)
  aa$links <- as.character(aa$links) 
  return(aa)
  #получить ссылки Результаты голосования по законопроекту с number 
  # по порядку - первое - второе - третье чтение 
  #получить ссылки Результаты голосования по законопроекту с number
}


#Функция возвращает ссылки на ссылки голосований по законопроекту со з значение number
#Входные параметры: number - номер законопроекта в системе ГД как текстовая переменная
#Результат        : links - вектор с ссылками на голосования по трем чтения данного законопроекта 
# ПО ТРЕМ ЧТЕНИЯМ
# number - номер законопроекта в vote.duma.gov.ru
#links - вектор ссылок на голосований по данному законопроекту по трем чтениям
GetVotesByNumberChtenie <- function(number){
  current <- GetVotesByNumber(number) #получаем полный список голосования по законопроекту 
  c1 <- grep("(1 чтение)", current$names, fixed = TRUE) 
  if (length(c1) == 0) c1 <- grep("(первое чтение)", current$names, fixed = TRUE)
  if (length(c1) == 0) c1 <- grep("(за основу)", current$names, fixed = TRUE)
  c2 <- grep("(2 чтение)", current$names, fixed = TRUE)
  if (length(c2) == 0) c2 <- grep("(второе чтение)", current$names, fixed = TRUE)
  c3 <- grep("(3 чтение)", current$names, fixed = TRUE)
  if (length(c3) == 0) c3 <- grep("(третье чтение)", current$names, fixed = TRUE)
  links <- as.character(current$links[c(c1,c2,c3)]) #получаем ссылки голосований по чтениям
  links <- as.data.frame(links)
  links$chtenie <- c(rep(1, times = length(c1)), rep(2, times = length(c2)), rep(3, times = length(c3)))
  return(links)
}


# Функция, которая Получает результаты голосований по вопросу, который определяется ссылкой 
#Входные параметры: link - ссылка на голосование вида /vote/76815
#Результат        : df - датафрейм с результатами голосования по людюм и партиям 
# используемые изначально коды идентификации результатов госолования : 
# 2 = не голосовал
# 1 = против
#-1 = за 
# 0 = воздержался

GetVoteResult <- function(link){
  url <- paste('http://vote.duma.gov.ru', link, sep ='') # преобразовать ссылку
  # скачать данные из интернета
  deputiesData <- getURL(url, useragent="RCurl", referer="http://quantviews.blogspot.ru/")
  # выделить JSON объект из страницы html, который начинается deputiesData = 
  deputiesData <- strsplit(deputiesData, 'deputiesData = ')[[1]]
  #обрезать по началу объекту
  deputiesData <- deputiesData[2]
  deputiesData <- strsplit(deputiesData, ';\nactiveLetter')[[1]] #обрезать по окончанию объекта
  deputiesData <- deputiesData[1]
  vote <- fromJSON(deputiesData) #преобразовать JSON в списиок
  result <- sapply(vote, function(x) {x$result}) #результаты голосования по кодам
  names <- sapply(vote, function(x) {x$sortName}) # ФИО депутатов
  id <- sapply(vote, function(x) {x$url}) # ИД депутатов по ссылке
  id <- sapply(id, function(x) strsplit(x, 'deputy=')[[1]][2]) 
  names(id) <- ''
  vote.df <- data.frame(id, result) #объединить все в датафрейм
  deputiesDatavote.df <- merge(vote.df, subset(depDuma, select = - c(name, position)), by = 'id')
  vote.df$result <- as.factor(vote.df$result) #преобразовать в факторы
  summary(vote.df$result)
  # переименовать имена факторов 
  vote.df$result <- revalue(vote.df$result, c("-1"="за", "0"="возд", '2' = 'не голосовал', "1" = "против"))
  vote.df$result <- factor(vote.df$result, levels = c('за', 'возд', 'не голосовал', 'против'))
  #plot(vote.df$result)
  
  
  return(vote.df)
  
}


# Функция получение списка депутатов ГД из АИС Законопроект
GetDeputatList <- function(){
  url <- paste('http://api.duma.gov.ru/api/', api_key, '/deputies.json?current=1', sep = '')
  # получить JSON
  depList <- getURL(url, useragent="RCurl", referer="http://quantviews.blogspot.ru/", .encoding = "UTF-8")
  depList <- fromJSON(depList)
  # выделить последнюю партию депутата из списка партий данного депутата $fractions
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
  return(depList)
}

#Функция для получения списка законопроектов с сайта гд в форме текстового вектора 
#Входные параметры: url - номер законопроекта в системе ГД как текстовая переменная
#к примеру, http://www.duma.gov.ru/systems/law/?periodType=year&periodValue=2012&section=1&PAGEN_1=2#results
#pages - общее количество страниц с результатами. ставится ВРУЧНУЮ 
#Результат        :  - вектор с номерами законопроектов  


#Законопроекты, рассмотренные Государственной Думой (или в первом, или во втором, или в третьем чтении) в 2012 году, отсортированные по дате внесения в ГД (по убыванию)
GetNumberByURL <- function(url, pages){
  all.number <- ''
  
  for (i in 1:pages){
    url.c <- paste0(url, '&PAGEN_1=', i,'#results')
    doc <- htmlTreeParse(url.c, useInternalNodes = T)
    numbers <- xpathSApply(doc, "//div[@class = 'search-block-result']/h3",xmlValue ) #результат
    # последний 20 элементы другой класс "search-block-result last", поэтому его нужно присоединить отдельно
    numbers <- c(numbers, xpathSApply(doc, "//div[@class = 'search-block-result last']//h3",xmlValue )) #результат
    numbers <- sapply(numbers, function(x) {substring(x,1,8)})
    names(numbers) <- NULL
    all.number <- c(all.number, numbers)
  }
  return(all.number)
}

#Функция возвращения полного списка законов по URL 
# Проблема в том, что API возвращает ответ по запросу по законопроектам по страницам (pages)
# 1 - номер законопроекта 
# 2 - название 
#3 - комментарии (если есть)
#4 - дата внесения 
GetZakonByUrl <- function(url){
  #получить первую страницу, чтобы выделить основные параметры ответа 
  zakonList <- getURL(url, useragent="RCurl", referer="http://quantviews.blogspot.ru/", .encoding = "UTF-8")
  zakonList <- fromJSON(zakonList) 
  nm <- c('number', 'name','comments', 'introductionDate')
  #задаем структуру будущего датафрейма ответа 
  z.total <- as.data.frame(matrix(nrow =0, ncol = 4, dimnames = list(NULL, nm)))
  #оцениваем общее количество по запросов. По умолчанию API возврашает по 20 законопроектов на страницу 
  page_count <- ceiling(zakonList$count / 20) 
  #проходимся в цикле по странице и склеиваем их в один датафрейм   
  for (i in 1:page_count){
    url.current <- paste0(url, '&page=',i) #получить ответ для i-ой страницы 
    zakonList <- getURL(url.current, useragent="RCurl", referer="http://quantviews.blogspot.ru/", .encoding = "UTF-8")
    zakonList <- fromJSON(zakonList)
    zak <- sapply(zakonList$laws, function(x) x$number ) #номер законопроекта
    zak.name <- sapply(zakonList$laws, function(x) x$name ) #название законопроекта
    z.comments <- sapply(zakonList$laws, function(x) x$comments ) #комментарии
    z.comments[sapply(z.comments, is.null)] <- NA #преобразуем NULL элементы списка в NA
    z.comments <- unlist(z.comments) #это нужно для того, чтобы при преобразовании они не исчезли 
    z.introductionDate <- sapply(zakonList$laws, function(x) x$introductionDate) #дата внесения 
    z.introductionDate <- as.Date(z.introductionDate, formt = '%Y-%m-%d')
    z.df <- data.frame(zak, zak.name, z.comments, z.introductionDate) #преобразуем в текущий датафрейм
    names(z.df) <- nm #имена должны совпадать 
    z.total <- rbind(z.total, z.df) #присоединияем к общему 
  }
  z.total$number <- as.character(z.total$number)
  return(z.total)  #функция возвращает в ответ полный датафрейм с 4 столбцами
}

#Функция, которая возвращает все данные в форме списка
GetAllByUrl <- function(url){
  #получить первую страницу, чтобы выделить основные параметры ответа 
  zakonList <- getURL(url, useragent="RCurl", referer="http://quantviews.blogspot.ru/", .encoding = "UTF-8")
  zakonList <- fromJSON(zakonList) 
  page_count <- ceiling(zakonList$count / 20) 
  zakonList <- zakonList$laws
  #проходимся в цикле по странице  
  for (i in 2:page_count){
        url.current <- paste0(url, '&page=',i) #получить ответ для i-ой страницы 
        zakonBuff <- getURL(url.current, useragent="RCurl", referer="http://quantviews.blogspot.ru/", .encoding = "UTF-8")
        zakonBuff <- fromJSON(zakonBuff)
        zakonBuff <- zakonBuff$laws
        zakonList <- c(zakonList,zakonBuff)
    
  }
  
  return(zakonList)  #функция возвращает в ответ полный список
}

# Рисует результат голосования в трех чтениях по номеру законопроекта в системе АСОЗД
GraphResultByNumber <- function(number, title = ''){
  vv <- GetVotesByNumberChtenie(number) # получить вектор из трех элементов с ссылками на голосования (по трем чтениям)
  vv$chtenie <- as.character(vv$chtenie)
  #vv$chtenie[2] <- '2 (1)'
  #vv$chtenie[3] <- '2 (2)'
  nm <- c('id', 'result', 'party', 'chtenie')
  v.total <- as.data.frame(matrix(nrow =0, ncol = 4, dimnames = list(NULL, nm)))
  for (i in 1:nrow(vv)){
    res <- GetVoteResult(vv[i,1])
    res$chtenie <- as.factor(vv[i,2])
    v.total <- rbind(v.total, res)
  }
  v.total$chtenie <- revalue(v.total$chtenie, c("1"="первое", "2"="второе", '3' = 'третье'))
  v.title <- title #название законопроекта
  v.total <- merge(v.total, depDuma, by ='id')
  ll <- as.data.frame(table(v.total$chtenie, v.total$party, v.total$result))
  colnames(ll) <- c("chtenie",'party', 'result', 'count')
  p <- ggplot(data = v.total, aes(factor(party), fill = party))
  p<- p+geom_bar()+facet_grid(result ~ chtenie)+labs( x = '', y ='количество голосов', title = v.title)
  p<- p+ scale_fill_manual(name = '', values = c('ЕР' = '#4d6b8d', 'КПРФ' = '#bf0d0d', 'ЛДПР' = '#fad000', 'СР' = '#e6871d' ))+ief.theme+theme(legend.position="bottom")+
    geom_text(data = ll, aes(x = party, y = count, label = count), vjust = -0.1, size = 4)
}

#
GetVoteInfo <- function(url) {
  doc <- htmlTreeParse(url, useInternalNodes = T)
  name <- xpathSApply(doc, "//title", xmlValue ) #название законопроекта
  time <- xpathSApply(doc, "////div[@class = 'date-p']/span",xmlValue ) #время
  time <- strptime(time, format = '%d.%m.%Y %H:%M:%S') #преобразоввать формат 
  result <- xpathSApply(doc, "//div[@class = 'sp-head']/b",xmlValue ) #результат
  df <- data.frame(name, time, result)
  return(df)
  
}


# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


# ###--- EXAMPLES ---###
# # Example 1: url input
# input <- "http://www.google.co.uk/search?gcx=c&sourceid=chrome&ie=UTF-8&q=r+project#pq=%22hello+%3C+world%22&hl=en&cp=5&gs_id=3r&xhr=t&q=phd+comics&pf=p&sclient=psy-ab&source=hp&pbx=1&oq=phd+c&aq=0&aqi=g4&aql=&gs_sm=&gs_upl=&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb&fp=27ff09b2758eb4df&biw=1599&bih=904"
# txt <- htmlToText(input)
# txt
# 
# #r project - Google Search Web Images Videos Maps News Shopping Gmail More Translate Books Finance Scholar Blogs YouTube Calendar Photos Documents Sites Groups Reader Even more » Account Options Sign in Search settings Web History Advanced Search Results  1  -  10  of about  336,000,000  for  r project . Everything More Search Options Show options... Web The  R Project  for Statistical Computing R , also called GNU S, is a strongly functional language and environment to    statistically explore data sets, make many graphical displays of data from custom  ... www. r - project .org/  -  Cached  -  Similar [Trunc...]


wordsCount <- function(txt) {
#Функция, подсчитывающая количество слов в текстовой переменной 
  m <- str_match_all( txt, "\\S+" )  # Sequences of non-spaces
  length(m[[1]])
}


