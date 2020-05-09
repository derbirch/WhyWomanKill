
library(echarts4r)

df<-data.frame(性别=c('男','女'),数量=c(671,3073))

df %>% 
  head() %>% 
  dplyr::mutate(model = c('男','女')) %>% 
  e_charts(model) %>% 
  e_pie(数量, roseType = "radius") %>%
  e_tooltip(trigger = "item")%>%
  e_title('原告性别比')%>%
  e_theme("dark")



liquid <- data.frame(val = c(0.6, 0.5, 0.4))

liquid %>% 
  e_charts() %>% 
  e_liquid(val) %>%
  e_theme("shine")