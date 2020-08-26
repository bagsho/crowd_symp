library(shiny)
library(dplyr)
require(ggplot2) 

#ui

ui <- fluidPage(
  titlePanel("Enfeksiyon Gününden İtibaren Günlere Göre Sempton Görülme Oranları"),
  sidebarLayout(
    	sidebarPanel(

		checkboxGroupInput("checkGroup", label = h3("Sempton"), 
    			choices = list("Ateş" = 1, "Kuru Öksürük" = 2, "Boğaz Ağrısı" = 3,
				"Yutkunma Zorluğu"=4, "Baş Ağrısı"=5, "İshal"=6, "Koku Alamama"=7),
    			selected = c(1,2))
		
  	),
    	mainPanel(
      	plotOutput(outputId = "distPlot")
    	)
  )
)

#server

server <- function(input, output) {
		output$distPlot <- renderPlot({exprs(input)})   			
}

exprs<- function(input) {

#data.frame'in olusturulmasi

##gun ve sympton olusturuluyor.
day<-data.frame(1:14)
sympton<-c("Ateş","Kuru Öksürük" ,"Boğaz Ağrısı" ,"Yutkunma Zorluğu","Baş Ağrısı","İshal","Koku Alamama")

##gun ve sempton birlestiriliyor.
df<-merge(day,sympton)
names(df)<-c("day","sympton")

##frekans tablosu olusturuluyor.
fre<-c(
0,0,1,2,33,52,65,64,49,33,22,11,6,1,
0,0,0,4,20,34,46,49,61,55,51,42,33,28,
0,0,1,9,29,33,48,61,49,34,11,0,0,0,
0,0,0,4,35,49,53,70,61,58,52,31,13,0,
0,0,0,4,45,55,44,41,22,11,8,0,0,0,
0,0,0,9,55,66,42,19,11,0,0,0,0,0,
0,2,6,17,36,47,50,35,25,15,5,5,0,0)

##gun ve sempton datasi ile birlestiriliyor.
df<-cbind(df,fre)

##arka plan icin ayni sayida satir eklenip, tamamlayici anlamina gelen comp degiskeni olusturuluyor.
df<-rbind(df%>%mutate(comp=0),df%>%mutate(comp=1,fre=ifelse(fre>0,100-fre,0)))

#data ui'dan gelen sempton isimlerine gore filtreleniyor.

##ui'dan gelen sempton isimleri selection degiskenine atiliyor.
selection<-sympton[as.integer(input$checkGroup)]

##sempton isimlerine gore filtreleme islemi yapiliyor. bu arada comp=1 olanlarin fre'leri ataniyor.
df_selected<-df %>% filter(sympton %in% selection)%>%
		        filter(sympton %in% selection & fre>0) %>%
			  mutate(fre=ifelse(comp==1,100,fre))

##grafik ayarlari yapiliyor.
p <- ggplot(df_selected, aes(x=day,y = fre,fill =as.factor(comp),group=sympton)) 
p <- p + geom_col(data=df_selected%>%filter(comp==1),width =1,col="grey")+ylim(0,100)+xlim(0,14)
p <- p + scale_fill_manual(values = alpha("white",.9))
p <- p + geom_col(data=df_selected%>%filter(comp==0),width =1,fill = "black")
p <- p + theme_classic()
p <- p + scale_x_continuous(breaks=seq(1,14, 1))
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
}

shinyApp(ui = ui, server = server)