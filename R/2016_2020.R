library(xml2)
library(rvest)
library(tidyverse)

url <- "https://www.fmprc.gov.cn/web/fyrbt_673021/jzhsl_673025/default.shtml"


url_to_text <- function(x) {
  
  txt <- x %>% 
    read_html() %>%
    html_nodes("[class='content']") %>%
    html_nodes("p") %>%
    html_text()
  
  txt <- str_trim(txt)
  txt <- txt[txt != ""]

  return(txt)
}



url_to_title <- function(x) {
  
  title <- x %>%
    read_html() %>%
    html_nodes("[class='rebox_news']") %>%
    html_nodes("a") %>%
    html_text()
  
  return(title)
}

url_to_web <- function(x) {
  
  web <- x %>%
    read_html() %>%
    html_nodes("[class='rebox_news']") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  web <- str_remove(web, "^./")
  web <- paste0("https://www.fmprc.gov.cn/web/fyrbt_673021/jzhsl_673025/", web)
  
  return(web)
}


url_to_web(url)

url_to_title(url)

url_to_text("https://www.fmprc.gov.cn/web/fyrbt_673021/jzhsl_673025/t1798529.shtml")


# From list urls to webpage urls

all_titles <- c()

for (i in 1:length(page_url)) {
  
  all_titles <- c(all_titles, url_to_title(page_url[i]))
  
}

all_web_urls <- c()

for (i in 1:length(page_url)) {
  
  all_web_urls <- c(all_web_urls, url_to_web(page_url[i]))
  
}

all_texts <- list()

for (i in 1:length(all_web_urls)) {
  
  all_texts[[i]] <- url_to_text(all_web_urls[i])
  print(i)
}


all_texts_2 <- list()

for (i in 1:length(all_texts)) {
  
  all_texts_2[[i]] <- all_texts[[i]][all_texts[[i]] != ""]
  
}

saveRDS(all_texts, "./rdata/all_texts.rds")
saveRDS(all_titles, "./rdata/all_titles.rds")
saveRDS(all_web_urls, "./rdata/all_web_urls.rds")

# Remove non-date webpage

vec <- !is.na(str_extract(all_titles, "^[0-9]{4}年[0-9]{1,2}月[0-9]{1,2}日"))

all_titles <- all_titles[vec]
all_web_urls <- all_web_urls[vec]
all_texts <- all_texts[vec]

rm(vec)

# Write files

all_file_names <- str_extract(all_titles, "^[0-9]{4}年[0-9]{1,2}月[0-9]{1,2}日") %>%
  str_remove("日") %>%
  str_replace_all("(年|月)", "_")

library(data.table)

for (i in 1:length(all_texts)) {
  
  fwrite(all_texts[i], paste0("./Press_Text/", all_file_names[i], ".txt"))
  
}







