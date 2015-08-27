library("RCurl") # Библиотека для генерации запросов к API
library("jsonlite") # Библиотека для обработки JSON
library("stringr") # Библиотека для работы с текстом


# Шаблон запроса подписок аккаунта
url <- "https://api.vk.com/method/users.getSubscriptions?user_id=" 

# Шаблон запроса подписчиков аккаунта
url2 <- "https://api.vk.com/method/users.getFollowers?user_id="

# Генерируем id аккаунта с replace (выбираем из 2.8 млн. id 700 аккаунтов с повторениями)
id_num <- sample.int(280000000, size = 300, replace = T)


# Функция для получения id подписок для сгенерированных аккаунтов
get_id_account <- function(id) {
        url_get <- str_c(url, id) # Составляем запрос к API
        resp <- getURL(url_get) # Запрашиваем и получаем ответ
        Sys.sleep(0.5) # Задержка для обхода ограничения на кол-во запросов в сек
        # Обрабатываем ответ и получаем id подписок
        fromJSON(resp)$response$users$items 
}


# Функция для получения кол-ва подписчиков аккаунта
get_count <- function(id) {
        url_get <- str_c(url2, id)
        resp <- getURL(url_get)
        Sys.sleep(0.5)
        fromJSON(resp)$response$count
}

# Получаем, при помощи функции get_id_account, id подписок для аккаунтов в виде list
account_id <- sapply(id_num, get_id_account)

# Преобразовываем list в data.frame
account_id <- unlist(account_id)

# Подсчитываем кол-во попаданий для каждого id полученных аккаунтов
account_id <- table(account_id)

# Преобразовываем в data.frame
account_id <- as.data.frame(account_id)

# Сортируем в убывающем порядке
account_id <- account_id[order(-account_id$Freq),]

# Получаем, при помощи функции get_count, кол-во подписчиков
result <- sapply(as.numeric(as.character(account_id$account_id[1:300])), get_count)

# Объединяем полученные данные о кол-ве подписчиков с id аккаунтов
result <- cbind(as.numeric(as.character(account_id$account_id[1:300])), result)

# Сортируем в убывающем порядке
result <- result[order(-result[,2]),]

result <- data.frame(id = result[,1], count_of_followers = result[, 2])

write.csv(result, "result_subscribers.csv")
