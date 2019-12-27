library(tidyverse)
library(rvest)
library(RSelenium)
library(data.table)
library(lubridate)
library(httr)
library(tcltk)
library(jsonlite)
library(fs)
library(plotly)
library(readxl)

setwd("D:/1drive/OneDrive - Wargaming.net/wowp_work/wowp_analysis")
setwd("D:/1drive/OneDrive - Wargaming.net/wowp_work/wowp_analysis/new_spa")

# home
setwd('D:/wg_1drv/OneDrive - Wargaming.net/files/r_sel/')
shell.exec('sel_auto.bat')
setwd("D:/wg_1drv/OneDrive - Wargaming.net/2.Pagon_Work")  

# company
setwd('D:/1drive/OneDrive - Wargaming.net/files/r_sel/')
shell.exec('sel_auto.bat')
setwd("D:/1drive/OneDrive - Wargaming.net/2.Pagon_Work")  

##############################
# parser
# itemlist SELL get function
##############################

get.itembay.sell.poe = function(x){
  
  mywait <- function() {
    tt <- tktoplevel()
    tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
            side='bottom')
    tkbind(tt,'<Key>', function()tkdestroy(tt) )
    
    tkwait.window(tt)
  }
  
  rsel = remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445L,
                      browserName = 'chrome')
  rsel$open()
  openbck = 'http://trade.itembay.com/item/sell/itemSellList?iGameSeq=1551' 
  rsel$navigate(openbck)
  
  mywait()
  print('my wait terminated')
  
  itembay = NULL
  for(i in seq(x)){
    #i = 2
    url.json1 = 'http://trade.itembay.com/item/api/sell/getSearchItemSellList?iGameSeq=1551&iGameServerSeq=0&tiItemType=3&chkTranPossible=true&chkTranComplete=true&tiCreditLevelNormal=0&tiCreditLevelBronze=1&tiCreditLevelSilver=3&tiCreditLevelGold=5&tiCreditLevelVip=6&chkReward=false&chkExcellent=false&chkDuplication=false&chkDirectMessage=false&iMinPrice=&iMaxPrice=&biMinQuantity=&biMaxQuantity=&vcSearchTxt1=&vcSearchTxt2=&vcOrderBy=OrderByItem&iPageNo='
    url.json2 = '&iLimit=48&tiPaging=1&bPriceOrder=0&bPremiumList=0'
    url.json = paste0(url.json1,i,url.json2)
    
    con = GET(url.json)
    tt = content(con, 'text')
    tt = gsub("(;|\n|_callback|jQuery1707377572341505474_1508264183038)", "", tt)
    tt = gsub("\\(", "[", tt)
    tt = gsub("\\)", "]", tt)
    
    temp = fromJSON(tt)
    temp.x = temp$data$list
    
    itembay = bind_rows(itembay, temp.x)
    
    progress = round(i/x*100,2)
    print(paste0(i,' pages completed / ',progress,'%'))
    
    html = rsel$getPageSource()[[1]]
    parsed = read_html(html)
    
    chk = parsed %>% 
      html_node('#divPaging > span.btn_page_next') %>% 
      html_attr('style')
    
    if(!is.na(chk)){
      print(paste0('stopped in ', i,' page'))
      break
    } 
    
    btn.next = rsel$findElement('css', '#divPaging > span.btn_page_next')
    btn.next$clickElement()
    Sys.sleep(2)
  }
  
  raw = itembay 
  head = names(raw) %>% 
    tolower()  
  names(raw) = head
  raw = raw %>% 
    select(-baseprice, -vcimagepatharray, -vccomment, -vcimagepath, -vcimageurl)
  
  raw = raw %>% 
    mutate(tranprice = str_remove_all(tranprice, '\\,') %>% as.integer())  %>% 
    mutate_at(vars(creditcss, credittextlevel, vcgamename, vcservername, cpremium ), as.factor) %>%
    mutate(dtpremiumenddate = str_remove(dtpremiumenddate, '\\.\\d{1,3}') %>% ymd_hms)
  
  td = today() %>% 
    str_remove_all(., '-')
  file.sell = paste0('poe_itembay_list_', td, '.csv')
  write.csv(raw, file.sell, fileEncoding = 'euc-kr', row.names = F)
  
  print(paste0(file.sell, ' : file saved'))
  return(itembay)
  print('itembay data frame returned')
}

itembay4 = get.itembay.sell.poe(1500)

# data processing
raw = itembay4 
head = names(raw) %>% 
  tolower()  
names(raw) = head
raw = raw %>% 
  select(-baseprice, -vcimagepatharray, -vccomment, -vcimagepath, -vcimageurl)

raw = raw %>% 
  mutate(tranprice = str_remove_all(tranprice, '\\,') %>% as.integer())  %>% 
  mutate_at(vars(creditcss, credittextlevel, vcgamename, vcservername, cpremium ), as.factor) %>%
  mutate(dtpremiumenddate = str_remove(dtpremiumenddate, '\\.\\d{1,3}') %>% ymd_hms)

td = today() %>% 
  str_remove_all(., '-')
file.sell = paste0('poe_itembay_list_', td, '.csv')
write.csv(raw, file.sell, fileEncoding = 'euc-kr', row.names = F)


##############################
# parser
# itemlist BUY get function
##############################

get.itembay.buy.poe = function(x){
  
  mywait <- function() {
    tt <- tktoplevel()
    tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
            side='bottom')
    tkbind(tt,'<Key>', function()tkdestroy(tt) )
    
    tkwait.window(tt)
  }
  
  rsel = remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445L,
                      browserName = 'chrome')
  rsel$open()
  openbck = 'http://trade.itembay.com/item/buy/itemBuyList?iGameSeq=1551'
  rsel$navigate(openbck)
  
  mywait()
  print('my wait terminated')
  
  itembay = NULL
  for(i in seq(x)){
    #i = 2
    url.json1 = 'http://trade.itembay.com/item/api/buy/getSearchItemBuyList?iGameSeq=1551&iGameServerSeq=0&tiItemType=3&chkTranPossible=true&chkTranComplete=true&tiCreditLevelNormal=0&tiCreditLevelBronze=1&tiCreditLevelSilver=3&tiCreditLevelGold=5&tiCreditLevelVip=6&chkSellPossible=false&chkExcellent=false&chkDuplication=false&iMinPrice=&iMaxPrice=&biMinQuantity=&biMaxQuantity=&vcSearchTxt1=&vcSearchTxt2=&vcOrderBy=OrderByItem&iPageNo='
    url.json2 = '&iLimit=48&tiPaging=1&bPriceOrder=0&bPremiumList=0'
    url.json = paste0(url.json1,i,url.json2)
    
    con = GET(url.json)
    tt = content(con, 'text')
    tt = gsub("(;|\n|_callback|jQuery1707377572341505474_1508264183038)", "", tt)
    tt = gsub("\\(", "[", tt)
    tt = gsub("\\)", "]", tt)
    
    temp = fromJSON(tt)
    temp.x = temp$data$list
    
    itembay = bind_rows(itembay, temp.x)
    
    progress = round(i/x*100,2)
    print(paste0(i,' pages completed / ',progress,'%'))
    
    #rsel$navigate('http://trade.itembay.com/item/buy/itemBuyList?iGameSeq=1551')
    
    html = rsel$getPageSource()[[1]]
    parsed = read_html(html)
    
    chk = parsed %>% 
      html_node('#divPaging > span.btn_page_next') %>% 
      html_attr('style')
    
    if(!is.na(chk)){
      print(paste0('stopped in ', i,' page'))
      break
    } 
    btn.next = rsel$findElement('css', '#divPaging > span.btn_page_next')
    btn.next$clickElement()



    Sys.sleep(2)
  }
  
  raw2 = itembay
  head = names(raw2) %>% 
    tolower()  
  names(raw2) = head
  raw2 = raw2 %>% 
    select(-baseprice, -vcimagepatharray, -vccomment, -vcimagepath, -vcimageurl)
  
  raw2 = raw2 %>% 
    mutate(tranprice = str_remove_all(tranprice, '\\,') %>% as.integer())  %>% 
    mutate_at(vars(creditcss, credittextlevel, vcgamename, vcservername, cpremium ), as.factor) %>%
    mutate(dtpremiumenddate = str_remove(dtpremiumenddate, '\\.\\d{1,3}') %>% ymd_hms)
  
  td = today() %>% 
    str_remove_all(., '-')
  file.buy = paste0('poe_itembay_list_buy_', td, '.csv')
  write.csv(raw2, file.buy, fileEncoding = 'euc-kr', row.names = F)
  print(paste0(file.buy, ' : file saved'))
  return(itembay)
  print('itembay returned')
}
itembay.buy = get.itembay.buy.poe(300)

# data processing
raw2 = itembay.buy
head = names(raw2) %>% 
  tolower()  
names(raw2) = head
raw2 = raw2 %>% 
  select(-baseprice, -vcimagepatharray, -vccomment, -vcimagepath, -vcimageurl)

raw2 = raw2 %>% 
  mutate(tranprice = str_remove_all(tranprice, '\\,') %>% as.integer())  %>% 
  mutate_at(vars(creditcss, credittextlevel, vcgamename, vcservername, cpremium ), as.factor) %>%
  mutate(dtpremiumenddate = str_remove(dtpremiumenddate, '\\.\\d{1,3}') %>% ymd_hms)

td = today() %>% 
  str_remove_all(., '-')
file.buy = paste0('poe_itembay_list_buy_', td, '.csv')
write.csv(raw2, file.buy, fileEncoding = 'euc-kr', row.names = F)


######################
# wassup.gg
######################


get.wassup.poe = function(){

  rsel = remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445L,
                      browserName = 'chrome')
  url.gg = 'https://poe.wassup.gg/currency_price'
  rsel$open()
  rsel$navigate(url.gg)
  
  html = rsel$getPageSource()[[1]]
  parsed = read_html(html)
  
  name = parsed %>% 
    html_node('#currency_tbody') %>% 
    html_nodes('tr') %>% 
    html_node('td:nth-child(1)') %>% 
    html_text() %>% 
    as.tibble %>% 
    rename(name = value)
  
  chaos = parsed %>% 
    html_node('#currency_tbody') %>% 
    html_nodes('tr') %>% 
    html_node('td:nth-child(2)') %>% 
    html_text() %>% 
    as.tibble %>% 
    separate(value, c('choas.orb', 'a'), sep = '\\s') %>% 
    select(choas.orb) %>% 
    mutate(choas.orb = as.numeric(choas.orb))
  
  today = rep(today(), nrow(name)) %>% 
    as.tibble %>% 
    rename(date = value)
    
  
  wassup = bind_cols(today, name, chaos)
  
  wassup = wassup %>% 
    mutate(name = as.factor(name))
  
  td = today() %>% 
    str_remove_all(., '-')
  file.wassup = paste0('poe_wassup_', td, '.csv')
  write.csv(wassup, file.wassup, fileEncoding = 'euc-kr', row.names = F)
  print(paste0(file.wassup, ' : file saved'))
  print('wassup returned')
  return(wassup)
  
}

wassup = get.wassup.poe()







######################
# PoE INVEN
######################


get.inven.poe = function(x){
  
  today = today() 
  rsel = remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445L,
                      browserName = 'chrome')
  rsel$open()
  
  url.base = 'http://www.inven.co.kr/board/poe/5448?sort=PID&p='
  poe.inv = NULL
  for(i in seq(x)){
    #i = 1
    
    url = paste0(url.base, i)
    rsel$navigate(url)
    
    html = rsel$getPageSource()[[1]]
    parsed = read_html(html)
    
    text = parsed %>% 
      html_node('#powerbbsBody > table > tbody > tr > td > div > table > tbody > tr > td > table > tbody > tr:nth-child(3) > td > form > table > tbody') %>% 
      html_nodes('td.bbsSubject') %>% 
      html_text() %>%
      as.tibble %>% 
      separate(value, c('a', 'text'), sep = ']') %>%
      select(text) %>% 
      mutate(cmt = str_extract(text, '\\[\\d{1,}')) %>% 
      mutate(cmt = ifelse(is.na(cmt), 0, cmt)) %>% 
      mutate(cmt = str_remove(cmt, '\\[') %>% as.integer)
    
    url = parsed %>% 
      html_node('#powerbbsBody > table > tbody > tr > td > div > table > tbody > tr > td > table > tbody > tr:nth-child(3) > td > form > table > tbody') %>% 
      html_nodes('td.bbsSubject > a') %>% 
      html_attr('href') %>% 
      as.tibble %>% 
      rename(url = value)
    
    date = parsed %>% 
      html_node('#powerbbsBody > table > tbody > tr > td > div > table > tbody > tr > td > table > tbody > tr:nth-child(3) > td > form > table > tbody') %>% 
      html_nodes('td.date') %>% 
      html_text() %>%
      as.tibble %>% 
      rename(date = value) %>% 
      mutate(date = ifelse(str_detect(date, '\\d{2}:\\d{2}'), paste(today, date),
                           paste0('2019-',date,'00:00'))) %>% 
      mutate(date = ymd_hm(date))
    
    pv = parsed %>% 
      html_node('#powerbbsBody > table > tbody > tr > td > div > table > tbody > tr > td > table > tbody > tr:nth-child(3) > td > form > table > tbody') %>% 
      html_nodes('td.hit') %>% 
      html_text() %>%
      as.tibble %>% 
      rename(pv = value) %>% 
      mutate(pv = as.integer(pv))
    
    temp = bind_cols(text, date, pv, url) %>% 
      slice(-(1:4))
    
    poe.inv = bind_rows(poe.inv, temp)
    
    progress = round(i/x*100,2)
    print(paste0(i,' page completed :', progress, '%'))
    Sys.sleep(2)
    
  }
  
  return(poe.inv)
}

poe = get.inven.poe(400)

cnt.inv = poe %>% 
  mutate(days = str_extract(date, '\\d{4}-\\d{2}-\\d{2}') %>% ymd()) %>% 
  group_by(days) %>% 
  summarise(post = n(), cmt = sum(cmt)) %>%
  gather(type, value, -days)

td = today() %>% 
  str_remove_all(., '-')
file.wassup = paste0('poe_inven_list', td, '.csv')
write.csv(poe, file.wassup, fileEncoding = 'euc-kr', row.names = F)

######################
# Game trix
######################

date.latest = today()-2
game = '스타크래프트'
game = '패스 오브 엑자일'
game = '로스트아크'

# 패스 오브 엑자일  스타크래프트  스타크래프트2  오버워치
# 리그 오브 레전드 던전앤파이터 로스트아크

get.gametrix.poe = function(date.latest, game){
  
  rsel = remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445L,
                      browserName = 'chrome')
  rsel$open()
  openbck = 'https://gtssl.gametrics.com/member/Login.aspx'
  rsel$navigate(openbck)
  
  # login
  id = rsel$findElement(using = 'id', value = 'txt_user_id')
  pw = rsel$findElement(using = 'id', value = 'txt_user_pass')
  login = rsel$findElement('xpath', '//*[@id="loginBtn"]')
  
  id$setElementAttribute('value', 'wargaming1')
  pw$setElementAttribute('value', 'wgbest')

  login$clickElement()
  
  # load serach page
  search.bck = 'http://www.gametrics.com/gameinfo/GameInfo01.aspx'
  rsel$navigate(search.bck)
  
  gamename = rsel$findElement('id', 'txt_programname')
  date.end = rsel$findElement('id', 'txt_enddate')
  date.start = rsel$findElement('id', 'txt_startdate')
  

  date.1st = date.latest - 372
  
  gamename$setElementAttribute('value', game)
  date.end$setElementAttribute('value', date.latest)
  date.start$setElementAttribute('value', date.1st)
  
  # btn.search = rsel$findElement('css', '#searchBtn')
  btn.search = rsel$findElement('xpath', '//*[@id="searchBtn"]')
  # btn.search$click()
  # btn.search$doubleclick()
  btn.search$clickElement()
      
  # get reseult
  igr.total = NULL
  limit = 1487 - 372
  
  for(i in seq(4)){
    #i = 2

    html = rsel$getPageSource()[[1]]
    parsed = read_html(html)
    
  
    date = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(1)') %>% 
      html_text() %>% 
      as.tibble %>% 
      slice(-(1:3)) %>% 
      rename(date = value) %>% 
      mutate(date = ymd(date))
    
    if(nrow(date) == 0){
      break;
    }
    
    rank.total = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(2)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-(1:2)) %>% 
      rename(rank.total = value)
    
    rank.genre = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(3)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-(1:2)) %>% 
      rename(rank.genre = value)
    
    share.total = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(4)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-(1:2)) %>% 
      rename(share.total = value)
    
    share.genre = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(5)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(share.genre = value)
    
    time.total.hour = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(6)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(time.total.hour = value)
    
    time.igr.min = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(7)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(time.igr.min = value)
    
    time.stay.min = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(8)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(time.stay.min = value)
    
    cnt.igr = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(9)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(cnt.igr = value)
    
    cnt.usage = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(10)') %>% 
      html_text()%>% 
      as.tibble %>% 
      slice(-1) %>% 
      rename(cnt.usage = value)
  
    accu = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(11)') %>% 
      html_text()%>% 
      as.tibble %>% 
      rename(accu = value)
    
    pccu = parsed %>% 
      html_node('#view > table > tbody') %>% 
      html_nodes('td:nth-child(12)') %>% 
      html_text()%>% 
      as.tibble %>% 
      rename(pccu = value)

    temp = bind_cols(date, rank.total, rank.genre, share.total, share.genre, 
                     time.total.hour, time.igr.min, time.stay.min, cnt.igr,
                     cnt.usage, accu, pccu)
    
    igr.total = bind_rows(igr.total, temp)
    
    if(limit == 0){
      break;
    }
    
    if(limit < 372){
      backup = date.1st
      date.1st = backup - limit
      date.latest = backup
      limit = 0
    } else {
      backup = date.1st
      date.1st = backup - 372
      date.latest = backup
      limit = limit - 372
    }
    
    gamename = rsel$findElement('id', 'txt_programname')
    date.end = rsel$findElement('id', 'txt_enddate')
    date.start = rsel$findElement('id', 'txt_startdate')
    
    gamename$setElementAttribute('value', game)
    date.end$setElementAttribute('value', date.latest)
    date.start$setElementAttribute('value', date.1st)
    
    btn.search2 = rsel$findElement('css', '#searchBtn')
    btn.search2$clickElement()
    
    print(paste0(i, ' phase completed'))
    Sys.sleep(2)
  
  } #for(i in seq(4)){
  
  # igr.total = igr.total %>% unique() %>% 
  #   arrange(date)
   
  igr.final = igr.total%>%
    mutate_if(str_detect(., '\\%'), funs(str_remove(.,'\\%'))) %>% 
    mutate_if(str_detect(., '\\,'), funs(str_remove_all(., '\\,'))) %>% 
    mutate_at(vars(2:12), as.numeric)
  
  game = rep(game, nrow(igr.final)) %>% 
    as.tibble %>% 
    rename(game = value)
  
  igr.final = bind_cols(game, igr.final)
  
  return(igr.final)
  #
}
test %>% 
  mutate(date = ymd(date))
test = get.gametrix.poe(today()-2, '스타크래프트')
test = get.gametrix.poe(today()-2, '패스 오브 엑자일')
test = get.gametrix.poe(today()-2, '로스트아크') 


test = ymd(test)
test %>% 
  ggplot() +
  geom_line(aes(x = date, y = pccu))

######################
# analysis
######################
setwd("D:/wg_1drv/OneDrive - Wargaming.net/2.Pagon_Work")  
setwd("D:/1drive/OneDrive - Wargaming.net/2.Pagon_Work")  

# wassup

list = dir_ls(glob = 'poe_wassup*2019*.csv')
total.wassup = NULL
for(i in seq(length(list))){
  #i = 1
  file = list[i]
  temp = read.csv(file, fileEncoding = 'euc-kr', stringsAsFactors = F) %>% 
    as.tibble %>% 
    mutate(date = ymd(date)) %>% 
    mutate(name = as.factor(name))
  total.wassup = bind_rows(total.wassup, temp)
  progress = round( i / length(list) *100,2)
  print(paste0(i, ' file completed : ', progress, '%'))
}

total.wassup %>% 
  filter(choas.orb >= 10) %>% 
  ggplot() +
  geom_line(aes(x = date, y = choas.orb, group = name, color = name)) +
  geom_point(aes(x = date, y = choas.orb, group = name, color = name)) 

str(total.wassup)



# merge SELL files

list = dir_ls(glob = '*list_2019*.csv')

total.item.sell = NULL
for(i in seq(length(list))){
  #i = 1
  file = list[i]
  temp = read.csv(file, fileEncoding = 'euc-kr', stringsAsFactors = F)
  date = file %>% 
    str_extract(., '\\d{8}') %>% 
    ymd() %>% 
    rep(., nrow(temp)) %>% 
    as.tibble %>% 
    rename(date = value)
  
  temp = bind_cols(date, temp)
  total.item.sell = bind_rows(total.item.sell, temp)
  progress = round( i / length(list) *100,2)
  print(paste0(i, ' file completed : ', progress, '%'))
}

dtotal.item.sell = total.item.sell %>% 
  filter(str_detect(vcsubject, '엑잘') | str_detect(vcsubject, '엑자일')) %>%
  filter(!str_detect(vcsubject, '카오')) %>% 
  select(date, vcsubject, vcservername ,iprice, 
         biwantquantity, biminquantity, bimaxquantity) %>%  
  mutate(price = round(iprice/biwantquantity, 0)) %>% 
  mutate(vcservername = ifelse(vcservername == '4개월 리그', 
                               'Legion리그[한국]', vcservername)) %>% 
  rename(server = vcservername) %>% 
  mutate(server = as.factor(server)) %>% 
  filter(price != 0)

str(total.item)

# merge BUY files

list = dir_ls(glob = '*list_*buy*2019*.csv')

total.item.buy = NULL
for(i in seq(length(list))){
  #i = 1
  file = list[i]
  temp = read.csv(file, fileEncoding = 'euc-kr', stringsAsFactors = F)
  date = file %>% 
    str_extract(., '\\d{8}') %>% 
    ymd() %>% 
    rep(., nrow(temp)) %>% 
    as.tibble %>% 
    rename(date = value)
  
  temp = bind_cols(date, temp)
  total.item.buy = bind_rows(total.item.buy, temp)
  progress = round( i / length(list) *100,2)
  print(paste0(i, ' file completed : ', progress, '%'))
}

total.item.buy = total.item.buy %>% 
  filter(str_detect(vcsubject, '엑잘') | str_detect(vcsubject, '엑자일')) %>%
  filter(!str_detect(vcsubject, '카오')) %>% 
  select(date, vcsubject, vcservername ,iprice, 
         biwantquantity, biminquantity, bimaxquantity) %>%  
  mutate(price = round(iprice/biwantquantity, 0)) %>% 
  mutate(vcservername = ifelse(vcservername == '4개월 리그', 
                               'Legion리그[한국]', vcservername)) %>% 
  rename(server = vcservername) %>% 
  mutate(server = as.factor(server)) %>% 
  filter(price != 0)



## AVG price 

avg.sell = total.item.sell %>% 
  filter(!(server %in% c('하드코어', '기타'))) %>% 
  group_by(date, server) %>% 
  summarise(avg.price = mean(price)) 

a.sell = (avg.sell %>% 
       ggplot() +
       geom_line(aes(x = date, y = avg.price, group  = server, color = server)) +
       geom_point(aes(x = date, y = avg.price, group  = server, color = server)) +
       ggtitle('Itembay Exile AVG Price (Offer from Vender)')) +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             legend.position = 'none')

a.sell = (avg.sell %>% 
            ggplot() +
            geom_line(aes(x = date, y = avg.price, group  = server, color = server)) +
            geom_point(aes(x = date, y = avg.price, group  = server, color = server)) +
            ggtitle('Itembay Exile AVG Price (Offer from Vender)'))


ggplotly(a.sell)


avg.buy = total.item.buy %>% 
  filter(!(server %in% c('하드코어', '기타'))) %>% 
  group_by(date, server) %>% 
  summarise(avg.price = mean(price)) 
  
a.buy = (avg.buy %>% 
       ggplot() +
       geom_line(aes(x = date, y = avg.price, group  = server, color = server)) +
       geom_point(aes(x = date, y = avg.price, group  = server, color = server)) +
       ggtitle('Itembay Exile AVG Price (Offer from Customer)'))

ggplotly(a.buy)

## Seller's post count 

post.sell = total.item.sell %>% 
  filter(!(server %in% c('하드코어', '기타'))) %>% 
  group_by(date, server) %>% 
  summarise(post = n())

post.buy = total.item.buy %>% 
  filter(!(server %in% c('하드코어', '기타'))) %>% 
  group_by(date, server) %>% 
  summarise(post = n())

b.sell = (post.sell %>% 
       ggplot() +
       geom_line(aes(x = date, y = post, group = server, color = server)) +
       geom_point(aes(x = date, y = post, group = server, color = server)) +
       ggtitle('Itembay Exile Trade Post Count (Vender Side)'))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none')

ggplotly(b.sell)

b.buy = (post.buy %>% 
            ggplot() +
            geom_line(aes(x = date, y = post, group = server, color = server)) +
            geom_point(aes(x = date, y = post, group = server, color = server)) +
            ggtitle('Itembay Exile Trade Post Count (Vender Side)'))

ggplotly(b.buy)

## IGR

igr = read_html(dir_ls(glob = 'poe*.xls'))

date = igr %>% 
  html_node('body > div > table') %>% 
  html_nodes('td:nth-child(1)') %>% 
  html_text() %>% 
  as.tibble %>% 
  rename(date = value) %>% 
  mutate(date = ymd(date))
pccu = igr %>% 
  html_node('body > div > table') %>% 
  html_nodes('td:nth-child(12)') %>% 
  html_text() %>% 
  as.tibble %>% 
  rename(igr.pccu = value)

poe_igr = bind_cols(date, pccu) %>% 
  mutate(igr.pccu = str_remove_all(igr.pccu, ',')) %>% 
  mutate(igr.pccu = as.integer(igr.pccu))
date.min = min(total.item.sell$date)
date.max = max(total.item.sell$date)

str(poe_igr)

igr.graph = poe_igr %>% 
  filter(date >= date.min & date <= date.max) %>% 
  ggplot() +
  geom_line(aes(x = date, y = igr.pccu)) +
  ggtitle('PoE IGR PCCU')  +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none')

ggplotly(igr.graph)

## inven

date.min = ymd('2019-07-29')
date.max = ymd('2019-08-27')
inv = cnt.inv %>% 
  filter(days >= date.min & days <= date.max) %>% 
  rename(date = days) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, color = type)) +
  geom_point(aes(x = date, y = value, color = type)) +
  ggtitle('PoE INVEN Post Count') + 
  theme(legend.position = 'bottom')+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none')

inv = cnt.inv %>% 
  filter(days >= date.min & days <= date.max) %>% 
  rename(date = days) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, color = type)) +
  geom_point(aes(x = date, y = value, color = type)) +
  labs(x = "TY [°C]", y = "Txxx") +
  ggtitle('PoE INVEN Post Count')

  
ggplotly(inv)

## disply in one 

x1 = a.sell +
  theme(legend.position = 'none')
x2 = b.sell +
  theme(legend.position = 'bottom')

(x = gridExtra::grid.arrange(igr.graph, inv, x1, x2, nrow = 4))


