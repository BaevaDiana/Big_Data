# загрузка библиотеки
install.packages("rvest")
library(rvest)

# cоздание документа html из url
# уровень жизни стран мира по годам
url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

# функция выбирает все элементы таблицы на странице по селектору 'table'
nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

# преобразования HTML-таблицы в датафрейм
df_21<-html_table(nodes_21[[2]])%>%as.data.frame()
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

# присвоения имен строкам датафрейма 
rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

# выбор столбцов в датафрейме с оценками общего качетсва жизни
df_21<-df_21[, 3:11]
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

# выбор стран согласно варианту
# русификация - Испания, Италия, Румыния, Греция, Дания
v<-c('Spain', 'Italy', 'Romania', 'Greece', 'Denmark')

# оценка индекса качества жизни
s<-'Quality of Life Index'
QLI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(QLI)<-v

years<-2014:2021
mn<-min(QLI, na.rm=TRUE)
mx<-max(QLI, na.rm=TRUE)
plot(
  years,
  QLI$'Romania',
  xlab='Года',
  ylab='Индекс качества жизни',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса качества жизни',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, QLI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, QLI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, QLI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, QLI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'bottomright', 
  cex=0.5, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка индекс покупательной способности
s<-'Purchasing Power Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)<-v

years<-2014:2021
mn<-min(PPI, na.rm=TRUE)
mx<-max(PPI, na.rm=TRUE)
plot(
  years,
  PPI$'Romania',
  xlab='Года',
  ylab='Индекс покупательной способности',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса покупательной способности',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PPI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, PPI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, PPI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, PPI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.5, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка индекса безопасности
s<-'Safety Index'
SI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(SI)<-v

years<-2014:2021
mn<-min(SI, na.rm=TRUE)
mx<-max(SI, na.rm=TRUE)
plot(
  years,
  SI$'Romania',
  xlab='Года',
  ylab='Индекс безопасности',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса безопасности',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, SI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, SI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, SI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, SI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка медицинского обслуживания
s<-'Health Care Index'
HCI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(HCI)<-v

years<-2014:2021
mn<-min(HCI, na.rm=TRUE)
mx<-max(HCI, na.rm=TRUE)
plot(
  years,
  HCI$'Romania',
  xlab='Года',
  ylab='Индекс медицинского обслуживания',
  ylim=c(mn-13,mx+13),
  main='Оценка индекс медицинского обслуживания ',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, HCI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, HCI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, HCI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, HCI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.5, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка индекса прожиточного минимума
s<-'Cost of Living Index'
CLI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(CLI)<-v

years<-2014:2021
mn<-min(CLI, na.rm=TRUE)
mx<-max(CLI, na.rm=TRUE)
plot(
  years,
  CLI$'Romania',
  xlab='Года',
  ylab='Индекс прожиточного минимума',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса прожиточного минимума',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, CLI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, CLI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, CLI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, CLI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка отношения цены на жилье к доходу
s<-'Property Price to Income Ratio'
PPIR<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPIR)<-v

years<-2014:2021
mn<-min(PPIR, na.rm=TRUE)
mx<-max(PPIR, na.rm=TRUE)
plot(
  years,
  PPIR$'Romania',
  xlab='Года',
  ylab='Отношение цены на жилье к доходу',
  ylim=c(mn-13,mx+13),
  main='Оценка отношения цены на жилье к доходу',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PPIR$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка индекс времени движения на дороге
s<-'Traffic Commute Time Index'
TCTI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(TCTI)<-v

years<-2014:2021
mn<-min(TCTI, na.rm=TRUE)
mx<-max(TCTI, na.rm=TRUE)
plot(
  years,
  TCTI$'Romania',
  xlab='Года',
  ylab='Индекс времени движения на дороге',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса времени движения на дороге',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, TCTI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка индекса загрязнения
s<-'Pollution Index'
PI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PI)<-v

years<-2014:2021
mn<-min(PI, na.rm=TRUE)
mx<-max(PI, na.rm=TRUE)
plot(
  years,
  PI$'Romania',
  xlab='Года',
  ylab='Индекс загрязнения',
  ylim=c(mn-13,mx+13),
  main='Оценка индекса загрязнения',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, PI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, PI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, PI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'topright', 
  cex=0.5, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

# оценка климатического индекса
# за 2014 и 2015 года данные отсутствуют
s<-'Climate Index'
CI<-as.data.frame(
  rbind(
    #df_14[v, s],
    #df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2016:2021
)
colnames(CI)<-v

years<-2016:2021
mn<-min(CI, na.rm=TRUE)
mx<-max(CI, na.rm=TRUE)
plot(
  years,
  CI$'Romania',
  xlab='Года',
  ylab='Климатический индекс',
  ylim=c(mn-13,mx+13),
  main='Оценка климатического индекса',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, CI$'Spain', type='b', col='green', lty=1, pch=1, lwd=2)
lines(years, CI$'Italy', type='b', col='red', lty=1, pch=1, lwd=2)
lines(years, CI$'Greece', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(years, CI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend(
  'bottomright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)


# cоздание документа html из url
url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')

# извлекает все элементы HTML-кода, соответствующие заданному селектору в переменной selector_name
selector_name<-'a.post-list-item-title-link'
fnames<-html_nodes(url, selector_name)%>%html_text()%>%as.vector()

# извлекает все элементы HTML-кода, соответствующие заданному селектору в переменной selector_name
selector_name<-'address.post-list-item-info'
fnames2<-html_nodes(url, selector_name)%>%html_text()%>%as.vector()

# извлекает все элементы HTML-кода, соответствующие заданному селектору в переменной selector_name
selector_name<-'.post-list-item-title-link'
fnames_addr<-html_nodes(url, selector_name)%>%html_attr('href')
fnames_addr2<-fnames_addr

# создание датафрейма
d<-data.frame(fnames[1:40], fnames2, fnames_addr2[1:40])
# именование столбцов
colnames(d)<-c('Название музея', 'Адрес', 'Ссылка на фото')
View(d)

