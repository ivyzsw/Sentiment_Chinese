
ans_idx_2020 <- list()

for (i in 1:length(all_texts)) {
  
  ans_idx_2020[[i]] <- get_ans_idx(all_texts[[i]])
  
}

all_texts[[639]]
get_ans_idx(all_texts[[639]])
get_qst_idx(all_texts[[639]])

# cleaning

for (i in 1:length(all_texts[[23]])) {
  
  all_texts[[23]][i] <- str_replace(all_texts[[23]][i], "记者提问", "记者")
}

all_web_urls <- all_web_urls[c(1:638, 640:999)]
all_titles <- all_titles[c(1:638, 640:999)]
all_texts[[639]] <- NULL

# compute

all_texts_list <- list()

for (i in 1:length(all_texts)) {
  all_texts_list[[i]] <- idx_to_text(all_texts[[i]], ans_idx_2020[[i]])
}

# read old data and merge

all_titles_old <- readRDS("./rdata/all_titles_old.rds")
all_texts_list_old <- readRDS("./rdata/all_texts_old_list.rds")
all_web_urls_old <- readRDS("./rdata/all_web_urls_old.rds")

texts <- c(all_texts_list, all_texts_list_old)
titles <- c(all_titles, all_titles_old)
urls <- c(all_web_urls, all_web_urls_old)

# check QA length

q_len <- c()
a_len <- c()
k <- 0

for (i in 1:length(texts)) {
  for (j in 1:length(texts[[i]])) {
    k <- k + 1
    q_len[k] <- length(texts[[i]][[j]][[1]])
  }
}

k <- 0

for (i in 1:length(texts)) {
  for (j in 1:length(texts[[i]])) {
    k <- k + 1
    a_len[k] <- length(texts[[i]][[j]][[2]])
  }
}

# check data order

titles[2500]
texts[[2500]]
urls[2500]

# 追问

day_idx <- c()
q_idx <- c()
q_list <- list()
k <- 0

for (i in 1:length(texts)){
  for (j in 1:length(texts[[i]])){
    if(any(str_detect(texts[[i]][[j]][[2]], "(^追问|记者追问|再次?追问：)"))) {
      k <- k + 1
      day_idx[k] <- i
      q_idx[k] <- j
      q_list[[k]] <- texts[[i]][[j]][[2]][str_detect(texts[[i]][[j]][[2]], 
                                      "(^追问|记者追问|再次?追问：)")]
    }
  }
}

for (i in 1:length(q_list)) {
  q_list[[i]] <- paste0(q_list[[i]], collapse = "")
}

texts_cp <- texts

for (i in 1:length(q_list)) {
  texts_cp[[day_idx[i]]][[q_idx[i]]][[1]] <- paste0(texts_cp[[day_idx[i]]][[q_idx[i]]][[1]],
                                                    q_list[[i]], collapse = "")
}

texts_cp[[42]][[2]][[1]]
texts_cp[[day_idx[59]]][[q_idx[59]]][[1]]

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "(^追问|记者追问|再次?追问：)"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "(^追问|记者追问|再次?追问：)")]
    }
  }
}

a <- 0

for (i in 1:length(texts)){
  for (j in 1:length(texts[[i]])){
    a <- a + length(texts[[i]][[j]][[2]])
  }
}

a <- 0

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    a <- a + length(texts_cp[[i]][[j]][[2]])
  }
}


k <- 0
a <- c()
for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    k <- k + 1
    a[k] <- length(texts_cp[[i]][[j]][[2]])
    }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if (length(texts_cp[[i]][[j]][[2]]) == 0) {
      print(j)
    }
  }
}

texts_cp[[2327]][[1]]


texts_cp[[1270]][[16]][[1]] <- "问：我们对会谈都保持乐观，即使只有一线希望，还是希望六方会谈能在本月底举行。如果出现最糟糕的情况，会谈没有举行，朝鲜方面被发现有新的核设备，在这种情况下，中国在外交层面将如果面对这样一种情况？追问：我们中间还存在一种悲观情绪，请问你认为最糟糕的情况是什么？中方将会怎么做？"
texts_cp[[1270]][[16]][[2]] <- c("你提问的第一句话，要保持乐观的态度。我觉得你这个话已经为你后半部分的提问做了很好的注解。如果你希望我一定要讲些什么话的话，我想说，半岛无核化的目标是各方的共识，我们期望各方都能够坚定信念，克服困难，能够举行第四轮六方会谈，能够使这个解决进程逐步地、扎实地向前推进。",
                                 "我不希望发生最糟糕情况，我希望大家能够根据第三轮六方会谈中各方达成的共识，能够克服困难，扎实前进。")

texts_cp[[1270]][[16]]

str_detect(answers, "再见") %>% sum()
str_detect(answers, "主持例行记者") %>% sum()
str_detect(answers, "(?!主持)例行记者会") %>% sum()
str_detect(answers, "主持例行记者会") %>% sum()
str_detect(answers, "主持例行记者.*会。$") %>% sum()

answers[str_detect(answers, "见！")]
answers[str_detect(answers, "^.{0,5}如果没有.{0,2}问题.{0,25}$")]
answers[str_detect(answers, "^（?记者：")]
answers[str_detect(answers, "：$")]
answers[str_detect(answers, "^.{0,10}谢谢大家([。！]|出席)")]
answers[str_detect(answers, "false")]
answers[str_detect(answers, "，$")]


for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "，$"))) {
      if (max(which(str_detect(texts_cp[[i]][[j]][[2]], "，$") == TRUE)) >= 
          length(texts_cp[[i]][[j]][[2]])) {
        print(c(i,j))
      }
    }
  }
}

texts_cp[[1129]][[8]][[2]] <- texts_cp[[1129]][[8]][[2]][1:2]
texts_cp[[2515]][[1]][[2]] <- texts_cp[[2515]][[1]][[2]][1]
texts_cp[[2565]][[2]][[2]] <- texts_cp[[2565]][[2]][[2]][1:2]
texts_cp[[2605]][[3]][[2]] <- texts_cp[[2605]][[3]][[2]][1]
texts_cp[[2667]][[2]][[2]] <- texts_cp[[2667]][[2]][[2]][1:4]
texts_cp[[2726]][[1]][[2]] <- texts_cp[[2726]][[1]][[2]][1:2]
texts_cp[[2727]] <- NULL
texts_cp[[1608]][[3]][[2]][4] <- "关于你的第二个问题，有关具体情况请你向有关部门去了解，我在前面的阐述中已经回答了我们在处理此类问题上的原则。"

dates <- dates[c(1:2726, 2728:2769)]
titles <- titles[c(1:2726, 2728:2769)]
urls <- urls[c(1:2726, 2728:2769)]

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "记者问"))) {
      print(c(i,j))
      }
    }
}

texts_cp[[2579]][[8]]

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "，$"))) {
      a <- which(str_detect(texts_cp[[i]][[j]][[2]], "，$") == TRUE)[1]
      texts_cp[[i]][[j]][[2]][a] <- paste0(texts_cp[[i]][[j]][[2]][a], texts_cp[[i]][[j]][[2]][a+1])
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][-(a+1)]
    }
  }
}


texts_cp <- texts

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "主持例行记者.*会。$"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "主持例行记者.*会。$")]
    }
  }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "再见"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "再见")]
    }
  }
}

answers[str_detect(answers, "见！")]
answers[str_detect(answers, "^.{0,5}如果没有.{0,2}问题.{0,25}$")]
answers[str_detect(answers, "^（?记者：")]
answers[str_detect(answers, "^.{0,10}谢谢大家([。！]|出席)")]
answers[str_detect(answers, "记者问")]

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "false"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "false")]
    }
  }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "^.{0,10}谢谢大家([。！]|出席)"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "^.{0,10}谢谢大家([。！]|出席)")]
    }
  }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "见！"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "见！")]
    }
  }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "^.{0,5}如果没有.{0,2}问题.{0,25}$"))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "^.{0,5}如果没有.{0,2}问题.{0,25}$")]
    }
  }
}

for (i in 1:length(texts_cp)){
  for (j in 1:length(texts_cp[[i]])){
    if(any(str_detect(texts_cp[[i]][[j]][[2]], "^（?记者："))) {
      texts_cp[[i]][[j]][[2]] <- texts_cp[[i]][[j]][[2]][!str_detect(texts_cp[[i]][[j]][[2]], "^（?记者：")]
    }
  }
}


for (i in 1:length(texts_cp)){
    if(texts_cp[[i]][[1]][[1]] == "问：") {
      texts_cp[[i]][[1]] <- NULL 
    }
}

for (i in 1:length(texts_cp)){
    if (length(texts_cp[[i]][[1]][[2]]) == 0) {
      texts_cp[[i]][[1]] <- NULL
    }
}

texts_cp[[1794]]


# build metadata

urls[1001]

dates <- str_extract(titles, "[0-9]{4}年[0-9]{1,2}月[0-9]{1,2}日")

str_extract(dates, "[0-9]{4}") %>% table()
str_extract(dates, "[0-9]{1,2}月") %>% str_remove("月") %>% table()
str_extract(dates, "[0-9]{1,2}日") %>% str_remove("日") %>% table()

metadata <- tibble(
  Year = str_extract(dates, "[0-9]{4}"),
  Month = str_extract(dates, "[0-9]{1,2}月") %>% str_remove("月"),
  Day = str_extract(dates, "[0-9]{1,2}日") %>% str_remove("日"),
  URL = urls
)

metadata$Date <- as.Date(ISOdate(metadata$Year, metadata$Month, metadata$Day))

metadata$URL[1800]
texts[[1800]]

# write txt files

library(data.table)

for (i in 1:nrow(metadata)) {
  for (j in 1:length(texts[[i]])) {
    
    writeLines(c(texts[[i]][[j]][[1]],
                 texts[[i]][[j]][[2]]), 
               paste0("./QA_Text/", metadata$Date[i], "_", j, ".txt"))
  }
}

writeLines(c(texts[[1900]][[2]][[1]],
             texts[[1900]][[2]][[2]]), "test.txt")

saveRDS(texts, "./rdata/texts.rds")
saveRDS(metadata, "./rdata/metadata.rds")







