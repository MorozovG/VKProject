library("RCurl")
library("jsonlite")
library("stringr")


#sample.int(100000, size = 100, replace = T)
#temp <- getURL("https://api.vk.com/method/friends.get?user_id=6196")
#fromJSON(temp)
#getURL("https://api.vk.com/method/users.getSubscriptions?user_id=6196")
#getURL("https://api.vk.com/method/users.getFollowers?user_id=6196")

url <- "https://api.vk.com/method/friends.get?user_id="
id_num <- sample.int(100000, size = 3, replace = T)


get_data <- function(id) {
        url_get <- str_c(url, id)
        resp <- getURL(url_get)
        respdata <- c(respdata, fromJSON(resp)$response)
        Sys.sleep(0.5)
        respdata
}

get_number <- function(id) {
        url_get <- str_c(url, id)
        resp <- getURL(url_get)
        Sys.sleep(0.5)
        num_friend <- length(fromJSON(resp)$response)
        
}

temp <- sapply(id_num, get_data)
untemp <- unlist(temp)
num_temp <- table(untemp)
num_temp <- as.data.frame(num_temp)
num_temp1 <- num_temp[order(-num_temp$Freq),]
result <- sapply(as.numeric(as.character(num_temp1$untemp[1:300])), get_number)
result1 <- cbind(as.numeric(as.character(num_temp1$untemp[1:300])), result)
resultfin <- result1[order(-result1[,2]),]
