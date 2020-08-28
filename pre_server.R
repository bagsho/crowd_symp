require(ggplot2) 
library(dplyr)

day<-data.frame(1:14)
sympton<-c("Ates","Kuru Oksuruk" ,"Bogaz Agrisi" ,"Yutkunma Zorlugu","Bas Agrisi","Ishal","Koku Alamama")
df<-merge(day,sympton)
names(df)<-c("day","sympton")
fre<-c(0,0,1,2,33,52,65,64,69,59,53,39,23,19,0,0,0,4,20,34,46,49,61,55,51,42,33,11,0,0,1,9,29,33,48,61,49,45,31,11,5,4,0,0,3,4,35,49,53,70,61,58,52,31,13,11,0,1,2,8,22,39,44,41,22,11,8,4,7,3,0,2,9,25,55,66,42,19,11,5,2,3,1,0,0,0,15,22,36,47,39,42,40,43,33,35,22,12)
df<-cbind(df,fre)
df<-rbind(df%>%mutate(comp=0),df%>%mutate(comp=1,fre=ifelse(fre>0,100-fre,0)))
selection<-sympton
df_selected<-df %>% filter(sympton %in% selection)%>%
		        filter(sympton %in% selection & fre>0) %>%
			  mutate(fre=ifelse(comp==1,100,fre))
p <- ggplot(df_selected, aes(x=day,y = fre,fill =as.factor(comp),group=sympton)) 
p <- p + geom_col(data=df_selected%>%filter(comp==1),width =1,col="grey")+ylim(0,100)
p <- p + scale_fill_manual(values = alpha("white",.9))
p <- p + geom_col(data=df_selected%>%filter(comp==0),width =1,fill = "black")+ylim(0,100)
p <- p + theme_classic()
p <- p + scale_x_continuous(breaks=seq(1, 14, 1))
p <- p + expand_limits(x=c(1,14))
p <- p + geom_vline(xintercept = 5,col="red")
p <- p + facet_wrap( ~ sympton, ncol=1, strip.position = "right",drop=TRUE)
p <- p + theme( 
  		    panel.border = element_blank(),
  		    panel.grid.major = element_blank(),
  		    panel.grid.minor = element_blank(),		    
		    legend.position="none",
		    strip.background=element_blank(),
		    axis.title.y = element_blank(),
		    axis.ticks = element_blank(),
		    axis.text.y = element_blank(),
   		    axis.line.x=element_line(size = 0.1, colour = "grey"),
		    axis.line.y= element_blank(),
		    strip.text.y = element_text(angle = 0, color='blue4')
		   )
p