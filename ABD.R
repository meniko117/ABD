library (data.table)
library (igraph)
library("visNetwork")
library(ggplot2)
library(ggpubr)


#account_info_dt <- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistaa', sep = ";", header= TRUE,  quote="", fill=TRUE)

# загружаем все файлы
account_columns_dt1<- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistaa', sep = ";", header= TRUE,  quote="", fill=TRUE, select = c(1,2,6))
account_columns_dt2<- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistab', sep = ";", header= TRUE,  quote="", fill=TRUE, select = c(1,2,6))
account_columns_dt3<- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistac', sep = ";", header= TRUE,  quote="", fill=TRUE, select = c(1,2,6))
account_columns_dt4<- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistad', sep = ";", header= TRUE,  quote="", fill=TRUE, select = c(1,2,6))
account_columns_dt5<- fread ('C:/Users/msmirnov/Documents/проект АБД/reorg/kartschetistae', sep = ";", header= TRUE,  quote="", fill=TRUE, select = c(1,2,6))


# строим граф на всех данных из БД


account_info_dt<- rbind(account_columns_dt1, account_columns_dt2, account_columns_dt3, account_columns_dt4, account_columns_dt5,use.names=FALSE)

rm(account_columns_dt1, account_columns_dt2, account_columns_dt3, account_columns_dt4, account_columns_dt5)
gc()

# ищем ключ "потомков"
descendant<- account_info_dt$kartschetist.karta %in% account_info_dt$kartschetist.id

account_info_dt<- cbind(account_info_dt, descendant)

account_info_descendant<- subset(account_info_dt, account_info_dt$descendant== TRUE)

actors <- data.frame(name=unique(c(account_info_descendant$kartschetist.id, account_info_descendant$kartschetist.karta)))



relations <- data.frame(from=account_info_descendant$kartschetist.id,
                        to=account_info_descendant$kartschetist.karta)

graph_all <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

# список вершин для вывода на UI Shiny
vertex_length<- c("22800743", "25459155", "24397841", '26097804')




# самый длинный путь
get_diameter(graph_all)




# кол-во изолированных графов
sum(round(ver_df$Freq / (ver_df$tt+1))) 


hist3<-ggplot(data = vertex_graph_distr) + geom_bar(aes(x = vertex_graph_distr$tt, y = vertex_graph_distr$Freq), stat = "identity")+ labs(x = "кол-во граней в графе", y = "Кол-во графов") + scale_y_continuous(labels = scales::comma, limits=c(0, 6700000))

hist4<-ggplot(data = vertex_graph_distr) + geom_bar(aes(x = vertex_graph_distr$tt, y = vertex_graph_distr$Freq), stat = "identity")+  ylim(c(0, 500000)) + labs(x = "кол-во граней в графе", y = "Кол-во графов") + scale_y_continuous(labels = scales::comma, limits=c(0, 500000))

hist5<-ggplot(data = vertex_graph_distr) + geom_bar(aes(x = vertex_graph_distr$tt, y = vertex_graph_distr$Freq), stat = "identity")+  ylim(c(0, 10000)) + labs(x = "кол-во граней в графе", y = "Кол-во графов")


figure<- ggarrange(hist3, hist4, hist5,
                   labels = c("A", "B", 'C'),
                   ncol = 1, nrow = 3)





# список вершин, у которых 2 родителя
vertex_2_in <- V(graph_all)[degree(graph_all, v = V(graph_all), mode = "in") == 135]$name

# кол-во родительских элементов для каждой вершины в графе
vertex_in<- as.data.frame(table(degree(graph_all, v = V(graph_all), mode = "in")))
vertex_out<-as.data.frame(table(degree(graph_all, v = V(graph_all), mode = "out")))


# список вершин графа, куда входит заданная вершина
list_vetrtex_graph_fun <- function (x) {subcomponent (graph_all, x, mode = "all")$name}

list_vetrtex_graph_fun ('637' )


# построить подграф, в который входит заданная вершина
visIgraph( induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, list_vetrtex_graph_fun("3684"), mode = "all")$name))


subset(account_info_dt, account_info_dt$kartschetist.karta=="20062113")
subset(account_info_dt, account_info_dt$kartschetist.id=="20062113")

# для построения графа с конкретным количеством вершин



system.time({
  tt<-as.data.frame(local_scan(graph_all,
                               k = 5, mode = 'all'))
})

# сводная
as.data.frame(table(tt))

# выводим все вершины, куда входит заданная



row.names( subset(tt, tt[,1]==6))

vertex_graph_distr<- as.data.frame(table(tt))

ver_df<-as.numeric(vertex_graph_distr)

ver_df$tt<-as.numeric(levels(vertex_graph_distr$tt ))
ver_df$Freq<-as.numeric(vertex_graph_distr$Freq)
















#
# прочее
#




account_info<- as.data.frame(account_info_dt)

#кол-во дублиированных номеров
nrow(account_info[duplicated(account_info$kartschetist.karta),])

# сколько раз встречается номер для не уникальных номеров
subset(table(account_info$kartschetist.karta), table(account_info$kartschetist.karta)>1)

subset(account_info, account_info$kartschetist.karta== 27294620)


# количество разлинчых НП владещих одним счетом
library(dplyr)
acc_summary<- account_info %>% 
  group_by(kartschetist.nmsch) %>% 
  summarize(n_unique = length(unique(kartschetist.np)))

acc_summary_subset<-subset(acc_summary, acc_summary$n_unique >2)

# макс кол-во владельцев счета
max(acc_summary_subset$n_unique)


# сортировка в порядке убывания кол-ва НП - владельцев счета
acc_summary_subset[order(-acc_summary_subset$n_unique), ]

# в рамках одного банка один и тот же счет меняет владельцев, но нет записей для потомков и номер nmmsg одинаковый
tt<-subset(account_info, account_info$kartschetist.nmsch== '40101810400000010007')
tt1<- subset(tt, tt$kartschetist.ko==21291)

length(unique(subset(account_info, account_info$kartschetist.nmsch== '40101810400000010007')$kartschetist.np))


# ищем ключ "потомков"
descendant<- account_info$kartschetist.karta %in% account_info$kartschetist.id

account_info<- cbind(account_info, descendant)

account_info_descendant<- subset(account_info, account_info$descendant== TRUE)

# выборка существенных полей для таблицы с "потомками"
account_info_descendant_key<- account_info_descendant [ , c(1:6, 17, 22)]

get_link_fun <- function (x) { subset (account_info, account_info$kartschetist.id == x)[ , c(1:6, 17, 22)]}

#get_link_fun_DT <- function (x) { subset (account_info, account_info$kartschetist.id == x)[ , c(1:6, 17, 22)]}

get_link_fun('20153388')

# получаем таблицу связей с потомками для последующего соденинения с таблицей содержащих родительские эелементы
# (считает 500 связей за 20 мин)



system.time({ 
descendant_link<-do.call(rbind,lapply (account_info_descendant_key$kartschetist.karta[ c(1:15000)], function (x) {get_link_fun(x)}))

})

# соединяем табилцу родителей и потомков
account_info_descendant_key_subset <- cbind( account_info_descendant_key [c(1:15000),], descendant_link)

# переименовываем колонки для потомков
colnames(account_info_descendant_key_subset)[9:16] <- c("id", "karta", "nmmsg", 'datemsg', 'nmsch', 'dataotrksch', 'ko', 'np')


# account_info_descendant_key_subset[11,] <- account_info_descendant_key_subset[10,]
# 
# account_info_descendant_key_subset[11, c(1, 9)] <- c('2608', "20301254")
# 




# строим граф

actors <- data.frame(name=unique(c(account_info_descendant_key_subset$kartschetist.id, account_info_descendant_key_subset$id)))



relations <- data.frame(from=account_info_descendant_key_subset$kartschetist.id,
                        to=account_info_descendant_key_subset$id)

g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)


plot(g, edge.label = account_info_descendant_key_subset$dataotrksch)


library("visNetwork")
visIgraph(g)



visNetwork(V(g), E(g))

Data <- toVisNetworkData(g)
visNetwork(g)





get_paths_by_length (g,2)






















V(graph_all)[degree(graph_all, v = V(graph_all), mode = "all") == 1]



vertex_in_all<- as.data.frame(table(degree(graph_all, v = V(graph_all), mode = "all")))

hist1<-ggplot(data = vertex_in_all) + geom_bar(aes(x = vertex_in_all$Var1, y = vertex_in_all$Freq), stat = "identity")+ labs(x = "кол-во вершин предков", y = "Кол-во вершин") + scale_y_continuous(labels = scales::comma, limits=c(0, 8500000))

hist2<-ggplot(data = vertex_in_all) + geom_bar(aes(x = vertex_in_all$Var1, y = vertex_in_all$Freq), stat = "identity")+  ylim(c(0, 100)) + labs(x = "кол-во вершин предков", y = "Кол-во вершин")

library(ggpubr)
figure<- ggarrange(hist1, hist2,
                   labels = c("A", "B"),
                   ncol = 1, nrow = 2)










account_info_dt_2_in_subset<- subset(account_info_dt, account_info_dt$kartschetist.karta %in% vertex_2_in)

library(dplyr)
vert_2_in <- account_info_dt_2_in_subset %>% 
  group_by(kartschetist.nmsch) %>% 
  summarize(n_unique = length(unique(kartschetist.np)))

vert_2_in[order(-vert_2_in$n_unique), ]

subset(account_info_dt, account_info_dt$kartschetist.nmsch== '40702810200000001026')[ , c(1:6, 17, 22)]



# перечень всех входящих вершин в заданную
subcomponent (graph_all, "25459155", mode = "all")

# самый длинны путь (диаметр графа)
diamter_dt<-subset (account_info_dt, account_info_dt$kartschetist.id %in% get_diameter(graph_all)$name)[ , c(1:6, 17, 22)]

# подграф на основе списка вершин
g2 <- induced.subgraph(graph=graph_all,vids=get_diameter(graph_all)$name)

plot(g2, edge.label = diamter_dt$kartschetist.dataotrksch)

g3 <- induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, "2474914", mode = "all")$name)

g3_edges<-subset (account_info_dt, account_info_dt$kartschetist.id %in% g3)

plot(g3)
visIgraph(g3)





g3 <- induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, "58670", mode = "all")$name)
plot(g3)

row.names( subset(tt, tt[,1]==1))
vertex_length<- c("22800743", "25459155", "24397841", '26097804')

get_diameter(graph_all)





# перечень вершин для построения подграфов
lapply(V(graph_all)$name[1:5],list_vetrtex_graph_fun)[[3]]

# для кажой вершины строим подграф и считаем диаметр
diameter(induced.subgraph(graph=graph_all,vids=lapply(V(graph_all)$name[1:5],list_vetrtex_graph_fun)[[3]]))

length(V(g5))

g5<-induced.subgraph(graph=graph_all,vids=lapply(V(graph_all)$name[1:5],list_vetrtex_graph_fun)[[3]])

#кол-во вершин входящих в подграф
system.time({
vertecies_number_list<-do.call(rbind, lapply(lapply(V(graph_all)[1:1000],list_vetrtex_graph_fun), length))
})

tt<-local_scan(induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, induced_graph_fun ("3684"), mode = "all")$name),
               k = 4, mode = 'all')





subcomponent (graph_all, x, mode = "all")$name
as.data.frame(table(subcomponent (graph_all, V(graph_all)[1:4], mode = "all")$name))



visIgraph( induced_graph_fun ("3684"))

visIgraph( induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, induced_graph_fun ("6342120"), mode = "all")$name))




subset(account_info_dt, account_info_dt$kartschetist.karta== 3220708)[ , c(1:6, 17, 22)]

# слияние 2-х счетов в 1
subset (account_info_dt, account_info_dt$kartschetist.karta %in% c(20438640, 21020437, 663213 ))[ , c(1:6, 17, 22)]
subset(account_info_dt, account_info_dt$kartschetist.id== 20438640)[ , c(1:6, 17, 22)]

print(g, e=TRUE, v=TRUE)

gsize(graph_all)


graph_all %>%
  vapply(gsize, 0) %>%
  hist()