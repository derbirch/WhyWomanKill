##########################################################################


#十年分省地图


##########################################################################
install.packages('maptools')
install.packages('mapproj')
library(plotly)
library(maptools)
library(ggplot2)
library(plyr)
library(mapproj)
library(viridis)
View(china_data)
View(province)
View(data)
setwd("~/homework/WhyWomanKill")

#q <- getBaiduMap('全国', width=600, height=600, zoom=18, scale = 2, messaging=FALSE)
#ggmap(q)

#province<-read.csv(file = 'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\DATA\\location\\province.csv',header =TRUE,sep=",")

china_map <-readShapePoly('~/homework/WhyWomanKill/DATA/maps/bou2_4p.shp') # 读取地图空间数据
china_map <- fortify(china_map) #转化为数据框
tb<-read.csv(file = '~/homework/WhyWomanKill/DATA/10年各省案件总数.csv',header =T,sep=',')

data <- tb
data <- data %>%
  arrange(amount) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste(
    "城市: ", name, "\n", 
    "案件数量: ", amount, sep="")
  )
class(data$mytext)

# Make the map (static)
pl <- data %>%
  ggplot() +
  geom_polygon(data = china_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(x=long, y=lat, size=amount, color=amount, text=mytext, alpha=amount) ) +
  scale_size_continuous(range=c(1,15)) +
  scale_color_viridis(option="inferno", trans="log" ) +
  scale_alpha_continuous(trans="log") +
  theme_void() +
  coord_map() +
  #coord_quickmap()+
  theme(legend.position = "none")

pl <- ggplotly(pl, tooltip="text")
pl


###############################################################################
remotes::install_github('JohnCoene/echarts4r.maps')
library(echarts4r)
library(echarts4r.maps)
data<-read.csv(file = '~/homework/WhyWomanKill/DATA/分省案件数量2009-2011.csv',header =T,sep=',',dec = ".",quote='')
json <- jsonlite::read_json('~/homework/WhyWomanKill/DATA/china.json')

data %>% 
  group_by(year) %>% 
  e_charts(name,timeline = TRUE) %>% 
  e_title("2009-2019年各省案件数量图") %>%
  e_map_register("China", json) %>%
  e_map(amount,map="China") %>% 
  e_timeline_opts(
    autoPlay = TRUE,
    axis_type = "category",
    playInterval = 1500,
    bottom = 0
  ) %>% 
  e_visual_map(amount)%>% 
  e_tooltip(formatter = htmlwidgets::JS("
  function(params){
        return('省份： ' + params.name+ 
                       '</strong><br />案件数量： ' + params.value)
      }"))%>%
  e_theme("shine")
