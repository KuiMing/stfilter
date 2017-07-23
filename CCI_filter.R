

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


