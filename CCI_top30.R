# 漲幅最大30檔+CCI

library(stfilter)

res=GET('http://pchome.megatime.com.tw/rank/sto0/ock03.html',
        add_headers(
          Referer= 'http://pchome.megatime.com.tw/rank/sto0/ock03.html'

        )) %>%
  content('text',encoding = 'utf8') %>%
  htmlParse(encoding = 'utf8') %>%
  readHTMLTable(stringsAsFactors = F,which=1)


code=stringr::str_match_all(res$股票,"[0-9]{4}") %>%
  unlist()
date <- long_date()
today=tail(date,1)

CCItt=c()

for (i in 1:3){
  CCIt=  lapply(code[(1+(i-1)*10):(i*10)], function(x){
    y=get_CCI(x,today)
    ind=which(index(y)==as.Date(today))
    # Sys.sleep(5)
    output=data.frame(code=x,cci=y$cci[as.Date(today)],
                      lcci=as.numeric(y$cci[ind-1]))
  }) %>% do.call(rbind,.)

  CCItt=rbind(CCItt,CCIt)
  Sys.sleep(300)
}

f_t <- rbind(fund(today),otc_fund(today)) %>%
  filter(nchar(code)==4) %>%
  filter(diff_f>0)

OHLCV_daily <- rbind(MI_INDEX(today), otc_daily(today))


stock=filter(CCItt,cci>100) %>%
  filter(lcci<100) %>%
  merge(f_t,"code") %>%
  merge(OHLCV_daily,"code")
