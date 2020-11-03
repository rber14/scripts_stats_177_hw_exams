library(rvest)
library(xml2)
library(dplyr)

html = html('https://www.cargurus.com/Cars/inventorylisting/viewDetailsFilterViewInventoryListing.action?bodyTypeGroup=bg6')

html2 = html('https://www.cargurus.com/Cars/inventorylisting/viewDetailsFilterViewInventoryListing.action?bodyTypeGroup=bg6#resultsPage=1')

# getting the link/img
info = html %>% html_nodes('img') %>% html_attr('src') 
info2 = html2 %>% html_nodes('img') %>% html_attr('src') 


# put them into an R data Frame
info = data.frame(info)
info2 = data.frame(info2)

typeof(info)
View(info)
View(info2)

#take last row out not an image we want
info <- info[-nrow(info),]

info2 <- info2[-nrow(info2),]

new_data_set = c(list(info), list(info2))

#view data frame
new_data_set  = data.frame(new_data_set)
View(new_data_set)
write.table(new_data_set, file = '/Users/robertocampos/Desktop/data_car.csv', row.names = FALSE, dec = FALSE)
write.csv(new_data_set)

?rbind

