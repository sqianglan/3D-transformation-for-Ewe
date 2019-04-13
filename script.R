rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
#library(readr)
#df<- read_delim("ETr919_1R_de_25x_trial_vantage.csv", ";", escape_double = FALSE, trim_ws = TRUE)

df<- read.table("ETr919_1R_de_25x_trial_vantage.csv", sep = ';', dec = ',', header = T)
coord <- data.frame(df$X, df$Y, df$Z,df$Surpass.Object) # select the relavent data from the data sheet
#strip the char in the Object ID into two columns.
coord_after <- coord %>% separate(df.Surpass.Object, sep = ' ', into = c('Object','ID'))

#filter the row with GM130 which is the end_point of the arrow later, and name the rows with the ID number
end_point <- coord_after %>% filter(Object == 'GM130') %>% remove_rownames %>% column_to_rownames(var="ID") %>% select(df.X, df.Y, df.Z, end.X = df.X, end.Y = df.Y, end.Z = df.Z) 

#filter the row with Cells which is the start_point of the arrow later, and name the rows with the ID number
start_point <- coord_after %>% filter(Object =='Cells') %>% remove_rownames %>% column_to_rownames(var="ID")%>% select(df.X, df.Y, start.X = df.X, start.Y = df.Y, start.Z = df.Z)

# merge the row based ont the ID number, 
coord_final <- merge(start_point, end_point, by=0)

#Transformation the axis and set the start point as (0,0,0)


coord_transformed <- coord_final %>% mutate(X =coord_final$end.X-start.X) %>% mutate(Y = coord_final$end.Y-start.Y) %>%mutate(Z=coord_final$end.Z-start.Z) %>% mutate(r = sqrt(X^2+Y^2+Z^2)) %>% mutate(t= asin(Y/r)) %>% mutate(degree=360*t/(2*pi)) %>% mutate(t_adj = case_when(X<0 | Z<0 ~ pi-degree ))

#plot based on coordinates
plot(NA, xlim=c(-6,6), ylim=c(6,-6), xlab="X", ylab="Y")
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


