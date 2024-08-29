# dhaka areea 
url = "https://en.wikipedia.org/wiki/Neighbourhoods_in_Dhaka_Metropolitan_Area"
library(rvest)
places =read_html(url) %>% 
  html_nodes("li") %>% 
  html_text()

areas = c(places[50:115])
areas
dhaka = data.frame(no = 1:length(areas),areas)
write.csv(dhaka,file="dhaka.csv")
##




url = "https://geokeo.com/database/town/bd/1/"
vb =read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()
vb[[1]]

paste0("https://geokeo.com/database/town/bd/",2,"/")


####### list

li = list()
nm = 1:22
for(i in 1:22){
  url = paste0("https://geokeo.com/database/town/bd/",i,"/")
  li[[i]]= read_html(url) %>% 
    html_nodes("table") %>% 
    html_table()
}



############# beset waay 


ll = data.frame()
nm = 1:22
for(i in 1:22){
  url = paste0("https://geokeo.com/database/town/bd/",i,"/")
   gh= read_html(url) %>% 
    html_nodes("table") %>% 
    html_table()
   ll=rbind(ll,gh[[1]])
}
names(ll)
address = ll %>% 
  select("Name","Country","Latitude","Longitude")
address  
write.csv(address,file="address.csv")
