# импорт данных 
df_d<-read.csv('C:/Users/Дианочка/Desktop/Обработка больших данных/4 ЛР/ss_wom_duo.csv',sep=";", header=T, fileEncoding="cp1251")
df_t<-read.csv('C:/Users/Дианочка/Desktop/Обработка больших данных/4 ЛР/ss_wom_team.csv',sep=";", header=T, fileEncoding="cp1251")

stat_places_d <- sapply(df_d[,-1], sum)
stat_places_t <- sapply(df_t[,-1], sum)

par(mfrow=c(1,2))
barplot(stat_places_d, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Женщины, дуэты (за все время)")
barplot(stat_places_t, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Женщины. команды (за все время)")

first_d <- df_d[,c(1:2)][df_d$X1 > 0, ]
first_t <- df_t[,c(1:2)][df_t$X1 > 0, ]

pie(first_d$X1, labels=first_d$X1, col=rainbow(length(first_d$X1)), main = "Количество золотых медалей (женщины-дуэты)\nза все время")
legend(-1.02, -0.1, first_d$Год, cex = 0.7, fill=rainbow(length(first_d$Год)))

pie(first_t$X1, labels=first_t$X1, col=rainbow(length(first_t$X1)), main = "Количество золотых медалей (женщины-команды)\nза все время")
legend(-1.02, -0.1, first_t$Год, cex = 0.7, fill=rainbow(length(first_t$Год)))

prize_d <- data.frame(Год=df_d$Год, Призовых=rowSums(df_d[, 2:4]))
prize_t <- data.frame(Год=df_t$Год, Призовых=rowSums(df_t[, 2:4]))

par(mfrow=c(1,1))
plot(prize_t, type="b", pch=19, col="navyblue", xaxt="n", ylim=c(0,7), main="Призовые места России по синхронному плаванию  за 20 лет")
lines(prize_d, type="o", pch=19,  col="navyblue")
lines(prize_t, type="o", pch=19, col="hotpink")
legend(min(df_t$Год), 7, c("Женщины-дуэты", "Женщины-команды"), fill=c("navyblue", "hotpink"))
axis(side=1, at=prize_t$Год)



events_gold <- read.csv('C:/Users/Дианочка/Desktop/Обработка больших данных/4 ЛР/gold.csv',sep=";", header=T, fileEncoding="cp1251")

plot(events_gold$Год, events_gold$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,50), xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних олимпиад")
lines(events_gold$Год, events_gold$Китай, type="o", pch=19, col="#1aafd0")
lines(events_gold$Год, events_gold$Япония, type="o", pch=19, col="#6a67ce")
lines(events_gold$Год, events_gold$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_gold$Год, events_gold$Россия, type="o", pch=19, col="gray70")
lines(events_gold$Год, events_gold$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_gold$Год, events_gold$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_gold$Год)
legend(max(events_gold$Год) - 1.5, 53, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))


events_prizes <- read.csv('C:/Users/Дианочка/Desktop/Обработка больших данных/4 ЛР/priz.csv',sep=";", header=T, fileEncoding="cp1251")

plot(events_prizes$Год, events_prizes$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,130), xlab="Год", ylab="Медалей", main="Призовые медали за 6 последних олимпиад")
lines(events_prizes$Год, events_prizes$КИТАЙ, type="o", pch=19, col="#1aafd0")
lines(events_prizes$Год, events_prizes$Япония, type="o", pch=19, col="#6a67ce")
lines(events_prizes$Год, events_prizes$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_prizes$Год, events_prizes$Россия, type="o", pch=19, col="gray70")
lines(events_prizes$Год, events_prizes$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_prizes$Год, events_prizes$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_prizes$Год)
legend(max(events_prizes$Год) - 1, 137, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))

#выделим призовые места за последние 6 лет
prize_6_d <- tail(prize_d, 6)
prize_6_t <- tail(prize_t, 6)

par(mfrow=c(1,3))
plot(prize_6_t, type="b", pch=19, col="navyblue", xaxt="n", ylim=c(0,7), main="Призовые места России по синхронному плаванию \nза последние 6 ОИ")
lines(prize_6_t, type="o", pch=11, col="hotpink")
lines(prize_6_d, type="o", pch=11, col="hotpink")
legend(min(prize_6_t$Год), 7.2, cex=0.7 ,c("Женщины-дуэты", "Женщины-команды"), fill=c("navyblue", "hotpink"))
axis(side=1, at=prize_t$Год)

prize_grouped = data.frame(Призовых_М=prize_6_d$Призовых, Призовых_Ж=prize_6_t$Призовых)
barplot(height=t(as.matrix(prize_grouped)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=prize_6_d$Год, col=c("navyblue", "hotpink"), main="Количество призовых мест России\nпо синхронному плаванию за последние 6 ОИ")

prize_6_sum <- sapply(prize_grouped, sum)
pie(prize_6_sum, labels=c(prize_6_sum["Призовых_Ж"], prize_6_sum["Призовых_Ж"]), col=c("navyblue", "hotpink"), main="Всего призовых мест у женщин-дуэтов и женщин-команд  из России\nпо синхронному плаванию за последние 6 ОИ")

