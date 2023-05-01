#-------------------------------------------------------
# Часть 1.

# чтение данных
branch.01 <- read.table("C:/Users/Дианочка/Desktop/Обработка больших данных/6LW/Филиалы.dat", header = TRUE,";", dec=',', fileEncoding="CP1251")

# нормализация данных
branch.02 <-branch.01[,-c(1,4,5,7,8)]

branch.02[,1:4]<-sapply(branch.02[,1:4],as.numeric)
branch.01[,c(2,3,6,9,10)]<-sapply(branch.01[,c(2,3,6,9,10)],as.numeric)
typeof(branch.02[2,4])

branch.02 <- scale(branch.02[,1:5], center = TRUE, scale = TRUE)

maxs <- apply(branch.02[,1:5], 2, max)
mins <- apply(branch.02[,1:5], 2, min)

branch.02 <- scale(branch.02, center = mins, scale = maxs - mins)

branch.03<-data.frame(branch.01[,c(1,4,5,7,8)],branch.02)

# создание матрицы попарных расстояний (по умолчанию - Евклидово расстояние)
dist.branch <- dist(branch.03 [,6:10])

# проводим кластерный анализ, 
# результаты записываются в список clust.branch
# hclust ожидает матрицу расстояния, а не исходные данные.
clust.branch<- hclust(dist.branch, "ward.D")

# просмотр краткой сводки результатов анализа
clust.branch

# построение дендрограммы
plot(clust.branch, labels = branch.01$НАЗВАНИЕ)
rect.hclust(clust.branch, k = 4, border="red")
abline(h = 1.3, col = "blue", lwd='3') # h - horizontal line, col - color
k = 4

# построение диаграммы "Каменная осыпь"
plot(1:19, clust.branch$height, type='b',xlab="Номер компоненты",ylab = "Собственное значение") 


# разделим Филиалы на 4 кластера
# вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.branch, k) 

# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups

dend <- as.dendrogram(clust.branch)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)


# вывод филиалов соответсвенно сформированным кластерам 
branch.01[groups==1, 1]
branch.01[groups==2, 1]
branch.01[groups==3, 1]
branch.01[groups==4, 1]

# для каждой группы определяем средние значения характеристик и строим датафрейм

# в 1-ом кластере
g1<-colMeans(branch.01[groups==1, c(2,3,6,9,10)])
# во 2-ом кластере
g2<-colMeans(branch.01[groups==2, c(2,3,6,9,10)])
# в 3-ем кластере
g3<-colMeans(branch.01[groups==3, c(2,3,6,9,10)])
# во 4-ом кластере
g4<-colMeans(branch.01[groups==4, c(2,3,6,9,10)])

# для общего графика
# в 1-ом кластере
g11<-colMeans(branch.03[groups==1,6:10])
# во 2-ом кластере
g12<-colMeans(branch.03[groups==2, 6:10])
# в 3-ем кластере
g13<-colMeans(branch.03[groups==3, 6:10])
# в 4-ом кластере
g14<-colMeans(branch.03[groups==4, 6:10])

# делаем дата фрейм из векторов групп кластеров
df<-data.frame(g1,g2,g3,g4); df
df1<-t(df); df1

df11<-data.frame(g11,g12,g13,g14); df11
df12<-t(df11); df12

barplot(as.matrix(df12), col=c("magenta","red","green","pink"))
legend("topright",
       legend = rownames(df12),
       fill = c("magenta","red","green","pink"),
       border = "black",
       cex=0.8)


barplot(df1[,1], ylim=range(pretty(c(0,max(df1[,1])))), 
        main="Площадь", 
        col=c("magenta","red","green","pink"))
legend("topright",
       legend = rownames(df1),
       fill = c("magenta","red","green","pink"),
       border = "black",
       cex=0.8)

barplot(df1[,2], ylim=range(pretty(c(0,max(df1[,2])))), 
        main="ПРОХОДИМОСТЬ", 
        col=c("magenta","red","green","pink"),legend=rownames(df1))

barplot(df1[,3], ylim=range(pretty(c(0,max(df1[,3])))), 
        main="МЕТРО", 
        col=c("magenta","red","green","pink"),legend=rownames(df1))

barplot(df1[,4], ylim=range(pretty(c(0,max(df1[,4])))), 
        main="ЦЕНЫ", 
        col=c("magenta","red","green","pink"),legend=rownames(df1))

barplot(df1[,5], ylim=range(pretty(c(0,max(df1[,5])))), 
        main="ПРОДАЖИ", 
        col=c("magenta","red","green","pink"),legend=rownames(df1))


library (lattice)
library("scatterplot3d")

df <-read.table("C:/Users/Дианочка/Desktop/Обработка больших данных/6LW/Филиалы.dat", header = TRUE,";", dec=',', fileEncoding="CP1251")

df[,c(2,3,6,9,10)]<-sapply(df[,c(2,3,6,9,10)],as.numeric)

my_data <- df
my_data["Group"]<-groups

# вывод графика рассеяния с минимальным количеством параметров с выделением имени
xyplot(ПРОДАЖИ ~ ПРОХОДИМ,group = Group, data = my_data,auto.key = TRUE,pch = 20,cex = 1.5)

# боксплот, отражающий характеристики классов типов 
boxplot(ПРОДАЖИ ~ Group, data =my_data, xlab="КЛАСТЕРЫ",ylab = "ПРОДАЖИ", frame = FALSE, col=c("magenta","red","green","pink"))

# построение трехмерного графика наших классов
cloud(ПРОДАЖИ~ЦЕНЫ*МЕТРО, group = Group, data = my_data, auto.key = TRUE,pch = 20,cex = 1.5) 

#-------------------------------------------------------
# Часть 2
install.packages("vctrs")

library(klaR)

my_data$Group<- c(as.factor(groups))

naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data) 
naive_df$tables 
naive_df$tables$ПРОДАЖИ
naive_df


# построение графиков по Бпйесу
opar=par() 
opar
layout(matrix(c(1,2),2,2, byrow = TRUE)) 
plot(naive_df,lwd = 2, legendplot=TRUE,cex=0.1)
#восстановление
par=opar


# классификация Decision Tree
set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(my_data)


library(party)
myFormula <- Group ~ ПРОХОДИМ + ЦЕНЫ + ПРОДАЖИ
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
table(predict(df_ctree),trainData$Group)
predict(df_ctree)
plot(predict(df_ctree))
plot(df_ctree)


# алгоритм Random Forest 
library(randomForest) 
rf <- randomForest(Group ~ .,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Group)
print(rf)








