library("RCurl") # Библиотека для генерации запросов к API
library("jsonlite") # Библиотека для обработки JSON
library("stringr") # Библиотека для работы с текстом


# Шаблон запроса подписок аккаунта
url <- "https://api.vk.com/method/users.getSubscriptions?user_id=" 

# Шаблон запроса членов паблика
url2 <- "https://api.vk.com/method/groups.getMembers?group_id="

# Генерируем id аккаунта с replace (выбираем из 2.8 млн. id 700 аккаунтов с повторениями)
id_num <- sample.int(280000000, size = 700, replace = T)


# Функция для получения id подписок для сгенерированных аккаунтов
get_id_public <- function(id) {
        url_get <- str_c(url, id) # Составляем запрос к API
        resp <- getURL(url_get) # Запрашиваем и получаем ответ
        Sys.sleep(0.5) # Задержка для обхода ограничения на кол-во запросов в сек
        # Обрабатываем ответ и получаем id подписок
        fromJSON(resp)$response$groups$items 
}


# Функция для получения кол-ва подписчиков паблика
get_count <- function(id) {
        url_get <- str_c(url2, id)
        resp <- getURL(url_get)
        Sys.sleep(0.5)
        fromJSON(resp)$response$count
}

# Получаем, при помощи функции get_id_public, id подписок для аккаунтов в виде list
public_id <- sapply(id_num, get_id_public)

# Преобразовываем list в data.frame
public_id <- unlist(public_id)

# Подсчитываем кол-во попаданий для каждого id полученных пабликов
public_id <- table(public_id)

# Преобразовываем в data.frame
public_id <- as.data.frame(public_id)

# Сортируем в убывающем порядке
public_id <- public_id[order(-public_id$Freq),]

# Получаем, при помощи функции get_count, кол-во подписчиков
result <- sapply(as.numeric(as.character(public_id$public_id[1:300])), get_count)

# Объединяем полученные данные о кол-ве подписчиков с id пабликов
result <- cbind(as.numeric(as.character(public_id$public_id[1:300])), result)

# Сортируем в убывающем порядке
result <- result[order(-result[,2]),]

