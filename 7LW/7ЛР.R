# Задание 1.

# импорт данных (все спортсмены по всем видам спорта)
df <- read.table("C:/Users/Дианочка/Desktop/Обработка больших данных/7LW/athlete_events.csv", sep=",", header=TRUE)
View(df)

v<-c("Name", "Sex", "Weight", "Sport")
df1<-df[which(df[, "Sport"]=="Synchronized Swimming"), v]

# удаление строчек с пустым значением поля "Вес"
df1<-df1[-which(is.na(df1[, "Weight"])),]
# удаление повторяющихся строчек
df1<-unique(df1)
# данные после нормализации
View(df1)
# значения столбца "Вес" становятся вектором
x<-df1[1:nrow(df1), "Weight"]

# Одномерные статистические тесты.

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x)

# Графический способ.

# гистограмма с линией плотности
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
hist(x, freq=FALSE, col="gray")
lines(x2, fun, col=2, lwd=2)

# квантильно-квантильный график
qqnorm(x)
qqline(x, col=4, lwd=2)

# Тест Стьюдента.
t.test(x, mu=50, conf.int=TRUE)

# Тест Уилкоксона.
wilcox.test(x, mu=mean(x), conf.int=TRUE)


#Задание 2.

df2<-df[which(df[, "Sport"]%in%c("Synchronized Swimming", "Swimming")), v]
df2<-df2[-which(is.na(df2[, "Weight"])),]
df2<-df2[which(df2[, "Sex"]=="F"),]
df2$Sport <- factor(df2$Sport)
df2$Sport <- droplevels(df2$Sport)
x4<-df2[1:nrow(df2), "Weight"]

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x4)

# квантильно-квантильный график
qqnorm(x4)
qqline(x4, col=4, lwd=2)

# проверка равенство дисперсий
# Тест Флингера-Киллина.
fligner.test(df2$Weight~df2$Sport, df2)

# проверка на отсутствие разницы в среднестатистическом значении 
t.test(df2$Weight~df2$Sport)
t.test(df2$Weight~df2$Sport, paired=FALSE, var.equal=TRUE)




