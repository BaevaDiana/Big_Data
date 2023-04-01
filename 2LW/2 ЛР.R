
# загрузка данных в R
df <- read.csv("C:/Users/Дианочка/Desktop/Обработка больших данных/2LR/Любимые фильмы.csv", sep=";", header=T, fileEncoding="cp1251")

# 1 задание - вычислить max, min, mean по каждому столбцу

# функции,вычисляющие требуемые значения
max_col <- function(data){
  sapply(data, max, na.rm = TRUE)
} 
min_col <- function(data) sapply(data,min,na.rm = TRUE)
mean_col <- function(data) sapply(data,mean,na.rm = TRUE)


# вызов функции
max_col(df[3:length(df-1)])
min_col(df[3:length(df-1)])
mean_col(df[3:12])

# 2 задание - подсчитать количество людей, отдавших предпочтение >7 и <3 (составить вектор)

# создание векторов
count_7 <- integer(10)
count_3 <- integer(10)


print("Количество людей с предпочтениями >7:") 
count_7<- colSums(df[3:12] >7,na.rm = TRUE);count_7
print("Количество людей с предпочтениями <3:")
count_3 <- colSums(df[3:12] <3,na.rm = TRUE);count_3


# 3 задание - вывести рейтинг фильмов в списке по убыванию

# создание вектора для рейтинга
rating <- integer(10)

# сортируем по убыванию средние значения оценок
rating<-sort(sapply(df[3:12], mean, na.rm = TRUE),decreasing = TRUE);rating

# 4 задание - построить столбчатую диаграмму оценок (можно сделать разными способами)

barplot(count_7,main = " Оценки больше 7",xlab="Фильмы",ylab="Количество",names = names(df[3:12]),cex.names = 0.8, col=rainbow(1))
barplot(count_3,main = " Оценки меньше 3",xlab="Фильмы",ylab="Количество",names = names(df[3:12]),cex.names = 0.8, col=rainbow(1))










