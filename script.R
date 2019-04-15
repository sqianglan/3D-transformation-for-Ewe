rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
#library(readr)
#df<- read_delim("ETr919_1R_de_25x_trial_vantage.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#read file from csv file

#df<- read_table("../data_from_Ewe/LB4489_1L_mb3_25x_Vantage_Bud cells.csv", sep = ';', dec = ',', header = T)

df<- read_excel("../data_from_Ewe/LB4489_1L_mb3_25x_Vantage_Bud cells.xls")
coord <- data.frame(df[,1], df[,5], df[,9],df[,16]) # select the relavent data from the data sheet
#strip the char in the Object ID into two columns.
coord_after <- coord %>% separate(Surpass.Object, sep = ' ', into = c('ONE','TWO','Object','ID')) %>% select(-c("ONE","TWO"))
colnames(coord_after) <-c("X","Y","Z","Object","ID")

#filter the row with GM130 which is the end_point of the arrow later, and name the rows with the ID number
end_point <- coord_after %>% filter(Object == 'GM130') %>% remove_rownames %>% column_to_rownames(var="ID") %>% select(X, Y, Z, end.X = X, end.Y = Y, end.Z = Z) 

#filter the row with Cells which is the start_point of the arrow later, and name the rows with the ID number
start_point <- coord_after %>% filter(Object =='nuclei') %>% remove_rownames %>% column_to_rownames(var="ID")%>% select(X, Y, Z,start.X = X, start.Y = Y, start.Z = Z)

# merge the row based ont the ID number, 
coord_final <- merge(start_point, end_point, by=0)

write.csv(coord_final, file = "../data_from_Ewe/LB4489_1L_mb3_25x_Vantage_Bud cells_substracted.csv")

#Transformation the axis and set the start point as (0,0,0)


coord_transformed <- coord_final %>% mutate(X =coord_final$end.X-start.X) %>% mutate(Y = coord_final$end.Y-start.Y) %>%mutate(Z=coord_final$end.Z-start.Z) %>% mutate(r = sqrt(X^2+Y^2+Z^2)) %>% mutate(t= asin(Y/r)) %>% mutate(degree=360*t/(2*pi)) %>% mutate(t_adj = case_when(X<0 ~ pi-degree ))

#plot based on coordinates
plot(NA, xlim=c(-10,10), ylim=c(10,-10), xlab="X", ylab="Y")
with(coord_transformed, mapply("arrows", 0, 0, X,Y) )

#####plot the arrow with angel
#plot(NA, xlim=c(0,250), ylim=c(250,0), xlab="X", ylab="Y")
plot(coord_transformed$start.X, coord_transformed$start.Y, pch=19, cex=1,  xlim=c(0,250), ylim=c(250,0), xlab="X", ylab="Y",col="Blue")
length=10
with(coord_transformed, mapply("arrows", start.X, start.Y, x1=start.X+length*cos(t_adj), y1=start.Y-length*sin(t_adj), length=0.05), col='pink')


#plot with ggplot2
polar <- ggplot(coord_transformed, aes(x = degree_adj,value = r))
polar <- polar + coord_polar(theta='x', start =0, direction = -1)
polar + geom_segment(aes(y=0, xend= degree_adj, yend=r),arrow=arrow(length=unit(0.3,"cm")))+scale_x_continuous("", limits=c(0,360),
                                                                                                               breaks=seq(0,360-1,by=45),)



#library(grid)
#p <- ggplot(coord_transformed, aes(x = X, y = Y))
#p <- p + geom_segment arrow = arrow(length = unit(0.1,"cm")))


