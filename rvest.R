library(rvest)
url = "https://bn.wikipedia.org/wiki/%E0%A6%B9%E0%A7%81%E0%A6%AE%E0%A6%BE%E0%A6%AF%E0%A6%BC%E0%A7%82%E0%A6%A8_%E0%A6%86%E0%A6%B9%E0%A6%AE%E0%A7%87%E0%A6%A6%E0%A7%87%E0%A6%B0_%E0%A6%9A%E0%A6%B2%E0%A6%9A%E0%A7%8D%E0%A6%9A%E0%A6%BF%E0%A6%A4%E0%A7%8D%E0%A6%B0_%E0%A6%93_%E0%A6%A8%E0%A6%BE%E0%A6%9F%E0%A6%95%E0%A7%87%E0%A6%B0_%E0%A6%A4%E0%A6%BE%E0%A6%B2%E0%A6%BF%E0%A6%95%E0%A6%BE"
vb = read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()
vb[[2]]$বছর
table(vb[[2]]$বছর)


hist(rnegbin(10000,10,4.5),col="green")
hist(rpois(10000,4.5),col="red",add=TRUE)
