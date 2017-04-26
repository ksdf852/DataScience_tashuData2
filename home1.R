#�ʿ��� Library ����
library(ggplot2)
library(ggmap)

#.csv ������ �о� ���̱�
tashu = read.csv(file='tashu.csv',encoding = 'UTF-8')
station <- read.csv("station.csv")

#�ʿ��� ������ ����
rent_station <-data.frame(as.numeric(tashu$RENT_STATION))
return_station <-data.frame(as.numeric(tashu$RETURN_STATION))
station_info <- as.numeric(station$'��ȣ')

#table�� �������� �󵵼� ���� �� ����
rent_station_table <-table(rent_station)
return_station_table <-table(return_station)

rent_station_index <-order(rent_station_table,decreasing=TRUE)
return_station_index <-order(return_station_table,decreasing=TRUE)

rent_station_sort <-sort(rent_station_table,decreasing=TRUE)
return_station_sort <-sort(return_station_table,decreasing=TRUE)

#rent_station�� �󵵼��� return_station�� �󵵼��� ����
total_station_table=rent_station_table
count=1
for(i in return_station_index){
  total_station_table[i]=total_station_table[i]+return_station_sort[count]
  count=count+1
  print(i)
}

#���� ������ �󵵼��� ����
total_station_index <- order(total_station_table,decreasing = TRUE)
total_station_table_sort <- sort(total_station_table,decreasing = TRUE)

#---------------------TOP10 ����------------------------------
top_10 <-total_station_index[c(1:10)]
top_10
station_num <-station_info[top_10[c(1:10)]]
Freq <- total_station_table_sort[c(1:10)]
result <- cbind(station_num,Freq)
result <- as.data.frame(result)

result

#-------------------------------------------------------------

#-------------------TOP_10������ ����׷���--------------------------------------------------
dia_bar <- ggplot(result, aes(x=factor(station_num), y=Freq, fill=factor(station_num))) +
  geom_bar(stat='identity') + scale_y_continuous(name="Frequncy", labels = scales::comma)

dia_bar

#---------------------------------------------------------------------------------------------

#------------------TOP_10������ GoogleMap-------------------------------------------------------------------------
station_location <- subset(station, select=c("��ȣ", "����","�浵"))
colnames(station_location) <- c("��ȣ", "����","�浵")

top10_station_location <- merge(result, station_location,by=0)
top10_station_location

map_point <- ggmap(get_googlemap("Daejon", maptype="roadmap", zoom=13))+geom_point(data=top10_station_location, 
                                                      aes(�浵,����,size=Freq),alpha=0.4,colour="red" )
map_point
#-----------------------------------------------------------------------------------------------------------------
#----------------------- TOP20��� ------------------------------------------------------
rent_return <- table(tashu$RENT_STATION, tashu$RETURN_STATION)
rent_return

rent_return = as.data.frame(rent_return)
rent_return
top20_rent_return <- head(rent_return[with(rent_return, order(-Freq)),], 20)
colnames(top20_rent_return) <- c("rent", "return", "Freq")
top20_rent_return
#----------------------------------------------------------------------------------------
#----------------------- TOP20��� �׷��� ---------------------------------------------------
ggplot(top20_rent_return, aes(x=factor(rent), y=factor(return), size=Freq))+
  geom_point() + xlab("rent") + ylab("return")