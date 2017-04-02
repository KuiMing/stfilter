library(httr)
library(XML)
library(dplyr)
library(quantmod)
library(data.table)
library(quantmod)

#'@title Get daily record of stock from TWSE
#'
#'@description Get daily record of stock from TWSE
#'  \url{http://www.twse.com.tw/en/trading/exchange/STOCK_DAY/STOCK_DAY.php}
#'@details
#'
#'@param stock stock code
#'@param year_in year, ex:'2017'
#'@param month_in month, ex: '03'
#'@examples
#'TWSE_csv(stock='1215','2017','03')
#'


TWSE_csv <- function(stock="1215",year_in,month_in){
  url <- "http://www.twse.com.tw/en/trading/exchange/STOCK_DAY/STOCK_DAY.php"
  year <- as.numeric(year_in)
  month <- as.numeric(month_in)
  query_str <- paste0('myear=', year, '&mmon=', month,
                      '&STK_NO=', stock, '&login_btn=+Query+')
  res <- POST(url,
              add_headers(
                Connection= "keep-alive",
                `Content-Length`= 47,
                `Cache-Control`= "max-age=0",
                Accept= "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                Origin= "http://www.twse.com.tw",
                `Upgrade-Insecure-Requests`= 1,
                `User-Agent`= "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36",
                `Content-Type`= "application/x-www-form-urlencoded",
                `Accept-Encoding`= "gzip, deflate",
                `Accept-Language`= "zh-TW,zh;q=0.8,en-US;q=0.6,en;q=0.4,zh-CN;q=0.2"),
              body = query_str)
  x=year_in
  i=month_in

  url <- paste0("http://www.twse.com.tw/en/trading/exchange/STOCK_DAY/STOCK_DAY_print.php?genpage=genpage/Report",
                x, i, "/", x, i, "_F3_1_8_", stock, ".php&type=csv")
  # download.file(url,destfile = "tmp.csv")
  tables <- read.csv(url,header = F)
  tables <- tables[-1:-2,c(1:2,4:8)]
  tables[,2:7] <- sapply(2:7, function(i){
    as.numeric(unlist(gsub(',','',as.character(tables[,i]))))
  })
  tables <- mutate(tables,V1=gsub('/','-',V1)) %>%
    `colnames<-`(c("date","Volume","Open","High","Low","Close",'diff'))

  tables <- xts(tables[,2:7], as.Date(tables[,1])) %>%
    as.zoo()
  return(tables)
}

som <- function(x) {
  as.Date(format(x, "%Y-%m-1"))
}

long_date <- function(n=6){
  date <- c(som(som(Sys.Date())-1),
            som(Sys.Date()))
  year <- c(format(date[1],"%Y"),
            format(date[2],"%Y"))
  mon <- c(format(date[1],"%m"),
           format(date[2],"%m"))
  x <- try(TWSE_csv('1101', year[2], mon[2]),T)
  if (class(x)=='try-error'){
    date[2] <- date[1]
    date[1] <- som(date[2]-1)
    year <- c(format(date[1],"%Y"),
              format(date[2],"%Y"))
    mon <- c(format(date[1],"%m"),
             format(date[2],"%m"))
  }
  date=c()
  for (i in 1:2){
    x=TWSE_csv('1101', year[i], mon[i])
    x=index(x)
    date=c(date,gsub('-','/',x))
  }
  date=tail(date,n)

}

fund=function(date=format(Sys.time(),"%Y/%m/%d")){
  date=paste0(as.numeric(substr(date,1,4))-1911,substr(date,5,10))
  url = "http://www.twse.com.tw/ch/trading/fund/T86/T86.php"
  res=httr::POST(url,
                 httr::add_headers(
                   `Content-Type`= "application/x-www-form-urlencoded"
                 ),
                 body = paste0('download=html&qdate=',date,'&select2=ALLBUT0999&sorting=by_issue'))

  tables=httr::content(res,encoding = 'utf8') %>%
    rvest::html_table(fill = T)
  tables=tables[[2]]
  tables=tables[,c(1,5,8)]
  colnames(tables)=c('code','diff_f','diff_t')
  tables=mutate(tables,diff_f=as.numeric(gsub(',','',diff_f))) %>%
    mutate(diff_t=as.numeric(gsub(',','',diff_t)))
  return(tables)
}

MI_INDEX <- function(date=format(Sys.time(),"%Y%m%d")){
  url = 'http://www.twse.com.tw/en/trading/exchange/MI_INDEX/MI_INDEX.php'
  res=httr::POST(url,
                 httr::add_headers(
                   `Content-Type`= "application/x-www-form-urlencoded"
                 ),
                 body = paste0("download=&qdate=",date,"&selectType=ALLBUT0999"))
  tables=httr::content(res,encoding = 'utf8')

  tables <- htmlParse(tables) %>%
    readHTMLTable(stringsAsFactors = F, which = 2) %>%
    .[,c(1:2,5:8)]

  colnames(tables) <- c("code","volume",'open',
                        'high','low','close')

  for (i in 2:6){
    tables[,i] <- as.numeric(gsub(',','',tables[,i]))
  }
  return(tables)
}

otc_fund=function(date=format(Sys.time(),"%Y/%m/%d")){
  date=paste0(as.numeric(substr(date,1,4))-1911,substr(date,5,10))
  url <- paste0("http://www.tpex.org.tw/web/stock/3insti/daily_trade/3itrade_hedge_download.php?l=zh-tw&se=EW&t=D&d=",
                date,"&s=0,asc")

  otc_f <- fread(url,header = T,skip = 1,stringsAsFactors = F,data.table = F,
                 select = c(1,5,8),col.names = c('code','diff_f','diff_t'))

  otc_f$diff_f <- gsub(" ","",otc_f$diff_f) %>%
    gsub(",","",.) %>%
    as.numeric()
  otc_f$diff_t <- gsub(" ","",otc_f$diff_t) %>%
    gsub(",","",.) %>%
    as.numeric()
  otc_f$code <- gsub(" ","",otc_f$code)
  return(otc_f)
}

otc_daily <- function(date=format(Sys.time(),"%Y/%m/%d")){
  date=paste0(as.numeric(substr(date,1,4))-1911,substr(date,5,10))
  url <- paste0("http://www.tpex.org.tw/web/stock/aftertrading/daily_close_quotes/stk_quote_download.php?l=zh-tw&d=",
                date,"&s=0,asc,0")
  otc <- fread(url,header = T,skip = 1,stringsAsFactors = F,data.table = F,
               select = c(1,3,5:7,9),col.names = c('code','close','open','high','low','volume'))
  for (i in 2:6){
    otc[,i] <- as.numeric(gsub(',','',otc[,i]))
  }
  otc <- otc[,c('code','volume','open','high','low','close')]
  return(otc)
}

get_eps <- function(year,season,type){
  url <- "http://mops.twse.com.tw/mops/web/ajax_t163sb19"
  eps <- POST(url,
              body=paste0("encodeURIComponent=1&step=1&firstin=1&TYPEK=",
                          type,"&code=&year=", year,"&season=",season)) %>%
    content('text',encoding = 'utf8') %>%
    htmlParse(encoding = 'utf8') %>%
    readHTMLTable(stringsAsFactors = F)
  return(eps)
}
eps <- function(year,season){
  eps <- lapply(c('sii','otc'), function(i){
    x <- get_eps(year,season,i) %>%
      do.call(rbind,.)
    return(x)
  }) %>% do.call(rbind,.)

  eps <- eps[!is.na(eps[,6]),]
  eps[,4] <- as.numeric(eps[,4])
  eps <- eps[,c(1,4)]
  colnames(eps) <- c('code','eps')
  eps <- arrange(eps, eps)
  return(eps)
}

revenue <- function(year,mon,type){
  url <- paste0('http://mops.twse.com.tw/nas/t21/',
                type,'/t21sc03_',year,'_',mon,'_0.html')
  tab <- htmlParse(url,encoding = 'big5') %>%
    readHTMLTable(stringsAsFactors = F, which = 2, skip.rows = 1:4)
  tab <- tab[!is.na(tab[,11]), c(1,6)] %>%
    mutate(V6=as.numeric(gsub(',','',V6)))
  colnames(tab) <- c('code','rev')
  return(tab)
}

concen <- function(stock){
  url <- paste0("http://www.wantgoo.com/Stock/aStock/Chips?StockNo=",
                stock)
  concen <- GET(url) %>%
    content('text',encoding = 'utf8') %>%
    htmlParse(encoding = 'utf8') %>%
    readHTMLTable(colClasses = c("character", rep( "numeric",4)),
                  stringsAsFactors = F) %>% .[[1]]
  colnames(concen)[1] <- "Duration"
  concen$Duration[1] <- paste(concen$Duration[2],concen$Duration[1],sep="-")
  concen[1,2:5]=concen[1,2:5]-concen[3,2:5]
  concen=concen[1,]
  return(concen)
}

get_start_date <- function(stock){

  date <- c(som(som(Sys.Date())-1),
            som(Sys.Date()))
  year <- c(format(date[1],"%Y"),
            format(date[2],"%Y"))
  mon <- c(format(date[1],"%m"),
           format(date[2],"%m"))
  x <- try(TWSE_csv(stock$code, year[2], mon[2]),T)
  if (class(x)=='try-error'){
    date[2] <- date[1]
    date[1] <- som(date[2]-1)
    year <- c(format(date[1],"%Y"),
              format(date[2],"%Y"))
    mon <- c(format(date[1],"%m"),
             format(date[2],"%m"))
  }

  data=data.frame(date=NULL,Volume=NULL,Open=NULL,
                  High=NULL,Low=NULL,Close=NULL,diff=NULL)
  for (i in 1:2){
    x=TWSE_csv(stock, year[i], mon[i])
    ind=index(x)
    x <- as.data.frame(x)
    x$date <- ind
    data=rbind(data,x)
  }
  range <- data.frame(date=NULL, range=NULL)
  for (i in 1:(dim(data)[1]-2)){
    tmp <- data[i:(dim(data)[1]-1),]
    range=rbind(range,data.frame(date=tmp$date[1],
                                 range=(max(tmp$High)-min(tmp$Low))/head(tmp$Close,1)))
  }
  start=range$date[range$range<0.1][1]
  return(start)
}

f2w <- function(stocklist, today){
  stock <- stocklist
  wanted <- data.frame(code=stock$code)

  start <- sapply(stock$code, get_start_date) %>%
    as.Date()

  wanted$start <- start
  wanted$end <- gsub('/','-',today)
  wanted$close <- ""

  gs_auth()
  url='https://docs.google.com/spreadsheets/d/1_n9Ba3rzswv0L8gtETRXMFv-njcTTLKFLt3WozQCzJQ'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  List=gs_add_row(List, ws='wanted', input = wanted)
}


xls_ETL <- function(files){
  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "start","end",
            "marked","comment")
  x=read.xlsx(files,sheetIndex = 1)
  colnames(x) <- coln
  x$marked=""
  x$comment=""
  ratio <- x$買賣超[1:4]/x$買賣超[2:5]
  if (length(which(ratio>=1.5))>0){
    x$marked[1:max(which(ratio>=1.5))]=1:max(which(ratio>=1.5))
  }
  return(x)
}

new_stock <- function(stock,url){
  gs_auth()
  #url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  stock_list <- gs_read(List,ws='wanted')
  stock_list <- filter(stock_list,is.na(close))
  start <- stock_list$start[stock_list$code==stock] %>%
    as.Date() %>% format("%Y%m%d")
  end <- stock_list$end[stock_list$code==stock] %>%
    as.Date() %>% format("%Y%m%d")

  url=paste0('http://www.wantgoo.com/stock/',stock,
             '?searchType=stocks')
  res=GET(url)
  restr <- content(res,'text',encoding = 'utf8')
  res <- htmlParse(restr, encoding = 'utf8')
  x=xpathSApply(res,"//h3[@class='idx-name']",xmlValue)
  if (length(x)==0){
    List=gs_add_row(List,ws='recent',input=c(stock,"", "not exist"))
    return()
  }
  titlename=xpathSApply(res,"//h3[@class='idx-name']",xmlValue) %>%
    paste(.,'recent',start,end,sep = "_")

  newsheet <- gs_new(titlename,ws_title = "overbought")

  x=c(stock,newsheet$browser_url, newsheet$sheet_title)
  List=gs_add_row(List,ws='recent',input=x)

  command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_agentstat.sh',stock,start,end)
  system(command,ignore.stderr = T)

  x <- xls_ETL('overbought.xls')
  newsheet <- gs_edit_cells(newsheet,input =x)

  x <- xls_ETL('oversold.xls')
  newsheet <- gs_ws_new(newsheet,ws_title = 'oversold')
  newsheet <- gs_edit_cells(newsheet,ws='oversold',input = x)

}

#'@title Observe recent trend of chip
#'
#'@description Get trading records from wangoo and put them into google spread sheet.
#'Move files info specific folder in google drive.
#'@details
#'
#'@param url url of google drive folder
#'@param folder folder code, 'recent' for Frank;
#''fil_recent' for filtered stock.
#'@examples
#'url='https://docs.google.com/spreadsheets/d/1_n9Ba3rzswv0L8gtETRXMFv-njcTTLKFLt3WozQCzJQ'
#'ob_recent(url,'fil_recent')
#'
#'@seealso \link{daily_routine}


ob_recent <- function(url,folder='fil_recent'){
  #url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  old <- gs_read(List,ws='recent')
  stock=gs_read(List,ws='wanted')
  stock$close[which(!is.na(old$close))]='done'
  List <- gs_edit_cells(List,ws='wanted',input = stock)

  old <- filter(old,is.na(close))
  stock <- filter(stock,is.na(close))
  newstock <- setdiff(stock$code,old$code)
  daily <- gs_read(List,ws='daily')
  daily <- filter(daily, is.na(close))
  newstock <- setdiff(newstock,daily$code)
  if (length(newstock)>0){
    for (i in 1:length(newstock)){
      new_stock(newstock[i],url)
      if (i%%5==0){
        Sys.sleep(sample(120:180,1))
      }
      if (i%%300==0){
        Sys.sleep(600)
      }
    }
    old <- gs_read(List,ws='recent')
    ind <- which(old$code %in% newstock & is.na(old$close))

    write.csv(old[ind,],file = 'newstock.csv',fileEncoding = 'utf8',row.names = F)

    command <- paste('/Users/benjamin/anaconda/bin/python',
                     '/Users/benjamin/Github/stock_conspiracy/move_files.py',
                     folder)

    system(command,ignore.stderr = T)

    new_url <- read.csv('newstock.csv',stringsAsFactors = F,fileEncoding = 'utf8')

    ind <- which(old$code %in% new_url$code & is.na(old$close))

    old$url[ind] <- new_url$url

    List <- gs_edit_cells(List,ws='recent', input = old)

  }
}

main_everyday <- function(recent,date,url){
  list_url <- url
  stock <- recent$code
  newurl <- recent$url
  mark <- gs_url(newurl) %>%
    gs_read(ws=2) %>%
    filter(!is.na(marked))
  if (dim(mark)[1]>0){
    return()
  }
  mark <- gs_url(newurl) %>%
    gs_read(ws=1) %>%
    filter(!is.na(marked))
  if (dim(mark)[1]==0){
    return()
  }

  url=paste0('http://www.wantgoo.com/stock/',stock,
             '?searchType=stocks')
  res=GET(url)
  restr <- content(res,'text',encoding = 'utf8')
  res <- htmlParse(restr, encoding = 'utf8')
  titlename <- xpathSApply(res,"//h3[@class='idx-name']",xmlValue) %>%
    paste0('_daily_',date[1])



  newsheet <- gs_new(titlename,ws_title = mark$券商名稱[1])
  if (dim(mark)[1]>1){
    for (i in 2:dim(mark)[1]){
      newsheet <- gs_ws_new(newsheet,ws_title = mark$券商名稱[i])
    }
  }


  #url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(list_url, lookup = NULL, visibility = NULL, verbose = TRUE)
  List=gs_add_row(List,ws='daily',input=c(stock,newsheet$browser_url,titlename))


  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "date","comment")

  buyer=c()

  for (i in 1:5){
    command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_buyer.sh',stock,date[i],date[i])
    system(command)
    x=read_excel('buyer.xls')
    x=x[!is.na(x[,1]),]
    colnames(x)=coln
    buyer=rbind(buyer,x)
    #Sys.sleep(sample(20:30,1))
  }
  for (i in 1:dim(mark)[1]){
    output <- data.frame(matrix(0,nrow = length(date),ncol = length(coln)))
    colnames(output) <- coln
    output$date=date
    output[,1]=mark[i,1]
    output$comment=""
    x=buyer[buyer[,1]==as.character(mark[i,1]),]
    ind <- which(output$date %in% x$date)
    output[ind,]=x
    newsheet <- gs_edit_cells(newsheet,ws = i,input = output)
  }
  Sys.sleep(sample(90:120,1))
}

get_date <- function(){
  date <- som(Sys.Date())
  year <- format(date,"%Y")
  mon <- format(date,"%m")

  x=try(TWSE_csv('1101', year, mon),T)
  if (class(x)=='try-error'){
    date <- som(som(Sys.Date())-1)
    year <- format(date,"%Y")
    mon <- format(date,"%m")
    x=try(TWSE_csv('1101', year, mon),T)
  }
  x=index(x)
  date=gsub('-','',x)
  today=format(Sys.Date(),"%Y%m%d")
  date=tail(date,1)
  if (date != today){
    return()
  }
  return(date)
}

main_daily <- function(daily,date){
  stock <- daily$code
  newurl <- daily$url
  sheet <- gs_url(newurl)
  ws <- gs_ws_ls(sheet)
  for (i in 1:length(ws)){
    oldsheet = gs_read(sheet, ws=i)
    if (tail(oldsheet$date,1)==date){
      if (i==length(ws)){
        return()
      }
      next
    }
  }


  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "date","comment")

  command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_buyer.sh',stock,date,date)
  system(command)
  buyer=read_excel('buyer.xls')
  buyer=buyer[!is.na(buyer[,1]),]
  colnames(buyer)=coln


  for (i in 1:length(ws)){
    output <- buyer[buyer[,1]==ws[i],]
    if (length(output$date)==0){
      output=buyer[1,]
      output$date=date
      output[1,1]=ws[i]
      output[1,2:7]=0
      output[1,9]=""
    }
    sheet <- gs_add_row(sheet,ws = i,input = output)
  }

}

#'@title Observe daily trading records of specific broker
#'
#'@description Observe daily trading records of specific broker
#'@details
#' 1. Stop crawling records and rename google spread sheets which were marked in "close" column.
#'
#' 2. Renew existed files.
#'
#' 3. Get trading records for new selected stock.
#'@param url url of google drive folder
#'@param folder folder code, 'daily' for Frank;
#''fil_daily' for filtered stock.
#'@examples
#'url='https://docs.google.com/spreadsheets/d/1_n9Ba3rzswv0L8gtETRXMFv-njcTTLKFLt3WozQCzJQ'
#'daily_routine(url,'fil_daily')
#'
#'@seealso \link{ob_recent}


daily_routine <- function(url,folder='fil_daily'){
  gs_auth()
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)

  daily <- gs_read(List,ws='daily')

  close_stock <- filter(daily, !is.na(close)) %>%
    filter(is.na(done))
  if (dim(close_stock)[1]>0){
    for (i in 1:dim(close_stock)[1]){
      renames <- gs_url(close_stock$url[i])
      titles <- gsub("-","",close_stock$close[i]) %>%
        paste(renames$sheet_title,.,sep = "_")
      gs_rename(renames,titles)
      close_stock$title[i] <- titles
    }
    close_stock$done <- 'done'
    daily[!is.na(daily$close),] <- close_stock
    List <- gs_edit_cells(List,ws='daily', input = daily)
  }

  date <- get_date()
  daily=daily[is.na(daily$close), ]
  if (!is.na(daily$code) && !is.null(date)){
    for (i in 1:dim(daily)[1]){
      main_daily(daily[i,],date)
      if (i%%5==0){
        Sys.sleep(sample(120:180,1))
      }
    }
  }


  stock=gs_read(List,ws='recent')
  recent <- filter(stock,is.na(close))

  date=long_date(5) %>% gsub('/','',.)
  if (dim(recent)[1]>0){
    for (i in 1:dim(recent)[1]){
      main_everyday(recent[i,],date,url)
    }
    old <- gs_read(List,ws='daily')
    ind <- which(old$code %in% recent$code & is.na(old$done))
    write.csv(old[ind,],file = 'newstock.csv',fileEncoding = 'utf8',row.names = F)
    command=paste('/Users/benjamin/anaconda/bin/python',
                  '/Users/benjamin/Github/stock_conspiracy/move_files.py',
                  folder)
    system(command,ignore.stderr = T)

    new_url <- read.csv('newstock.csv',stringsAsFactors = F,fileEncoding = 'utf8')
    ind <- which(old$code %in% new_url$code & is.na(old$done))
    old$url[ind] <- new_url$url
    List <- gs_edit_cells(List,ws='daily', input = old)
    recent <- gs_read(List, ws='recent')
    recent$close <- "done"
    List <- gs_edit_cells(List,ws='recent', input = recent)
  }
}


