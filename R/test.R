

# split QA

a <- str_detect(all_texts[[1]], "\\？$")
str_detect(all_texts[[500]], "^问：|记者：")



a == b

all_texts[[1]][60]
a[60]

test <- all_texts[[1]]

which(str_detect(test, "记者：") %in% TRUE)



get_qst_idx(test)



a <- get_ans_idx(test)
a <- get_ans_idx(all_texts[[500]])

b <- idx_to_text(all_texts[[500]], a)


x <- get_qst_idx(test)

# Test QA length

get_qst_idx(all_texts[[1]])

for (i in 1:length(all_texts)) {
  
  x <- get_qst_idx(all_texts[[i]])
  
  print(i)
  
}

# Modify text
all_texts[[145]][34]

all_texts[[145]][34] <- str_replace_all(all_texts[[145]][34], "（记者.*。）", "")
all_texts[[145]][34] <- str_replace_all(all_texts[[145]][34], "（记者点头）", "")

all_titles[247]
all_texts[[247]][8]
all_texts[[247]][8] <- str_replace_all(all_texts[[247]][8], "（记者.*。）", "")

all_titles[293]
all_texts[[293]][16]
all_texts[[293]][16] <- str_replace_all(all_texts[[293]][16], "（记者.*。）", "")

all_titles[335]
length(all_texts[[335]])
all_texts[[335]][28]
all_texts[[335]] <- all_texts[[335]][!str_detect(all_texts[[335]], "记者：我只是想知道更多的信息，我可以等到明天")]

all_titles[377]
all_texts[[377]][13]
all_texts[[377]][13] <- str_replace_all(all_texts[[377]][13], "（记者.*。）", "")

all_titles[808]
all_texts[[808]][7]
all_texts[[808]][7] <- str_replace_all(all_texts[[808]][7], "（.*记者.*。.*）", "")

all_titles[968]
all_web_urls[968]
txt <- str_split(all_texts[[968]][14], "[:blank:]")[[1]] %>%
  `[`(str_split(all_texts[[968]][14], "[:blank:]")[[1]] != "")
length(all_texts[[968]])
all_texts[[968]] <- all_texts[[968]][!str_detect(all_texts[[968]], "有13名朝鲜人从海外的")]
all_texts[[968]] <- append(all_texts[[968]], txt, after = 13)

# Remove other lines

x <- c()
for (i in 1:length(all_texts)) {
  x[i] <- sum(str_detect(all_texts[[i]], "\\*"))
}
sum(x)
y <- c()
for (i in 1:length(all_texts)) {
  y[i] <- sum(str_detect(all_texts[[i]], "^\\*.*\\*$"))
}
sum(y)
for (i in 1:length(all_texts)) {
  all_texts[[i]] <- all_texts[[i]][!str_detect(all_texts[[i]], "^\\*.*\\*$")]
}





