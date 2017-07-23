

library(stfilter)
get_CCI=function(stock,today){
  x=stock
  day=as.Date(today)
  ym=c(som(som(som(day))-1)-1,
       som(som(day)-1),
       som(day))
  year <- sapply(ym, function(x){
    format(x,"%Y")
  })
  mons <- sapply(ym, function(x){
    format(x,"%m")
  })
  test=try(TWSE_csv(x,year[3],mons[3]),T)
  if (class(test)=="try-error"){
    y <- rbind(OTC_csv(x,year[1],mons[1]),
               OTC_csv(x,year[2],mons[2]),
               OTC_csv(x,year[3],mons[3]))
  }else {
    y <- rbind(TWSE_csv(x,year[1],mons[1]),
               TWSE_csv(x,year[2],mons[2]),
               TWSE_csv(x,year[3],mons[3]))
  }
  y=y[!is.na(Cl(y))] %>% as.xts()

  return(cbind(CCI(HLC(y),n=14),SMA(Cl(y),20),Cl(y)))

}
url='https://docs.google.com/spreadsheets/d/1_n9Ba3rzswv0L8gtETRXMFv-njcTTLKFLt3WozQCzJQ'
List=gs_url(url)

date <- long_date()
today=tail(date,1)

f_t <- rbind(fund(today),otc_fund(today)) %>%
  filter(nchar(code)==4) %>%
  filter(diff_f>0) %>%
  filter((diff_t+diff_f)/diff_f>0.8)
f_t=arrange(f_t,desc(diff_f)) %>%
  head(30)
OHLCV_daily <- rbind(MI_INDEX(today), otc_daily(today))

CCIt=  lapply(f_t$code, function(x){
  y=get_CCI(x,today)
  ind=which(index(y)==as.Date(today))
  # Sys.sleep(1)
  output=data.frame(code=x,cci=y$cci[as.Date(today)],
                    lcci=as.numeric(y$cci[ind-1]))
}) %>% do.call(rbind,.)

stock=filter(CCIt,cci>100) %>%
  filter(lcci<100) %>%
  merge(f_t,"code") %>%
  merge(OHLCV_daily,"code")
if (dim(stock)[1]>0){
  stock$date=today
  stock$link=url=paste0('http://www.wantgoo.com/stock/astock/three?StockNo=',
                        stock$code,'&dtSpan=40')

  stock=stock[,c('date','code','cci','lcci',
                 'diff_f','diff_t','close','link')]

  List=gs_add_row(List,ws='CCI',input = stock)
}


## Validation




url='https://docs.google.com/spreadsheets/d/1_n9Ba3rzswv0L8gtETRXMFv-njcTTLKFLt3WozQCzJQ'
List=gs_url(url)

mon=c('01','02','03','04','06','07')
td=lapply(mon, function(i){
  TWSE_csv('1101','2017',i)
}) %>% do.call(rbind,.) %>% as.zoo() %>%
  index()

for (i in 1:length(td)){
  date <- long_date(n=30,Date = td[i])
  ind=which(date==td[i])
  date=date[(ind-5):ind]
  today=tail(date,1)

  f_t <- rbind(fund(today),otc_fund(today)) %>%
    filter(nchar(code)==4) %>%
    filter(diff_f>0) %>%
    filter((diff_t+diff_f)/diff_f>0.8)
  f_t=arrange(f_t,desc(diff_f)) %>%
    head(30)
  OHLCV_daily <- rbind(MI_INDEX(today), otc_daily(today))

  CCIt=  lapply(f_t$code, function(x){
    y=get_CCI(x,today)
    ind=which(index(y)==as.Date(today))
    # Sys.sleep(1)
    output=data.frame(code=x,cci=y$cci[as.Date(today)],
                      lcci=as.numeric(y$cci[ind-1]))
  }) %>% do.call(rbind,.)

  stock=filter(CCIt,cci>100) %>%
    filter(lcci<100) %>%
    merge(f_t,"code") %>%
    merge(OHLCV_daily,"code")
  if (dim(stock)[1]>0){
    stock$date=today
    stock$link=url=paste0('http://www.wantgoo.com/stock/astock/three?StockNo=',
                          stock$code,'&dtSpan=40')

    ind=which(td %in% as.Date(today))+1
    stock$in_date=as.character(td[ind])
    stock=stock[,c('date','code','in_date','cci','lcci',
                   'diff_f','diff_t','close','link')]

    List=gs_add_row(List,ws='CCI',input = stock)
  }

  Sys.sleep(5)
}

newlist=gs_read(List,ws='CCI')

validation=function(newlist){
  newlist$in_price=0
  newlist$out_date60=""
  newlist$out_price60=0
  newlist$out_date20=""
  newlist$out_price20=0
  newlist$out_date_CCI0=""
  newlist$out_price_CCI0=0
  newlist$out_date_CCI100=""
  newlist$out_price_CCI100=0
  newlist$CCI0=0
  newlist$CCI100=0
  mon=data.frame(year=c('2016','2017','2017','2017','2017','2017','2017','2017'),
                 mon=c('12','01','02','03','04','05','06','07'))


  for (i in 1:dim(newlist)[1]){
    test=try(TWSE_csv(newlist$code[i],mon$year[1],
                      mon$mon[1]),T)
    if (class(test)=="try-error"){
      dt=lapply(1:7, function(x){
        OTC_csv(newlist$code[i],mon$year[x],
                mon$mon[x])
      }) %>% do.call(rbind,.) %>% as.xts()
    }else {
      dt=lapply(1:7, function(x){
        TWSE_csv(newlist$code[i],mon$year[x],
                 mon$mon[x])
      }) %>% do.call(rbind,.) %>% as.xts()
    }
    dt=dt[!is.na(Cl(dt))]
    newlist$in_price[i]=Cl(dt)[newlist$in_date[i]]
    ind=paste0(newlist$in_date[i],'::2017-07')
    dt$ma20=SMA(Cl(dt),20)
    dt$diff20=Cl(dt)-dt$ma20
    dt$ma60=SMA(Cl(dt),60)
    dt$diff60=Cl(dt)-dt$ma60
    dt$CCI=CCI(HLC(dt),14)

    dt=dt[ind]
    ind=which(dt$diff20<0)
    if (length(ind)>0){
      newlist$out_price20[i]=as.numeric(Cl(dt[ind[1]]))
      newlist$out_date20[i]=as.character(index(Cl(dt[ind[1]])))
      newlist$ma20[i]=as.numeric(dt$ma20[ind[1]])
    }else {
      newlist$out_price20[i]=as.numeric(tail(Cl(dt),1))
      newlist$out_date20[i]=as.character(index(tail(Cl(dt),1)))
      newlist$ma20[i]=as.numeric(tail(dt$ma20,1))
    }

    ind=which(dt$diff60<0)
    if (length(ind)>0){
      newlist$out_price60[i]=as.numeric(Cl(dt[ind[1]]))
      newlist$out_date60[i]=as.character(index(Cl(dt[ind[1]])))
      newlist$ma60[i]=as.numeric(dt$ma60[ind[1]])
    }else {
      newlist$out_price60[i]=as.numeric(tail(Cl(dt),1))
      newlist$out_date60[i]=as.character(index(tail(Cl(dt),1)))
      newlist$ma60[i]=as.numeric(tail(dt$ma60,1))
    }
    ind=which(dt$CCI<0)
    if (length(ind)>0){
      newlist$out_price_CCI0[i]=as.numeric(Cl(dt[ind[1]]))
      newlist$out_date_CCI0[i]=as.character(index(Cl(dt[ind[1]])))
      newlist$CCI0[i]=as.numeric(dt$CCI[ind[1]])
    }else {
      newlist$out_price_CCI0[i]=as.numeric(tail(Cl(dt),1))
      newlist$out_date_CCI0[i]=as.character(index(tail(Cl(dt),1)))
      newlist$CCI0[i]=as.numeric(tail(dt$CCI,1))
    }

    ind=which(dt$CCI < -100)
    if (length(ind)>0){
      newlist$out_price_CCI100[i]=as.numeric(Cl(dt[ind[1]]))
      newlist$out_date_CCI100[i]=as.character(index(Cl(dt[ind[1]])))
      newlist$CCI100[i]=as.numeric(dt$CCI[ind[1]])
    }else {
      newlist$out_price_CCI100[i]=as.numeric(tail(Cl(dt),1))
      newlist$out_date_CCI100[i]=as.character(index(tail(Cl(dt),1)))
      newlist$CCI100[i]=as.numeric(tail(dt$CCI,1))
    }
  }
  newlist=newlist %>%
    mutate(profit20=(out_price20-in_price)*1000) %>%
    mutate(profit60=(out_price60-in_price)*1000) %>%
    mutate(profitCCI0=(out_price_CCI0-in_price)*1000) %>%
    mutate(profitCCI100=(out_price_CCI100-in_price)*1000)
  return(newlist)
}

for (i in 1:dim(newlist)[1]){
  x=validation(newlist[i,])
  List=gs_add_row(List,ws='vali_CCI',input=x)
  Sys.sleep(20)
}




List=gs_edit_cells(List,ws="vali_CCI",input = x)





