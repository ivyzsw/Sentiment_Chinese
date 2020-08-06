
library(tidyverse)

answers <- c()

for (i in 1:length(texts)){
  for (j in 1:length(texts[[i]])){
    answers <- append(answers, texts[[i]][[j]][[2]])
  }
}


answers <- answers[!str_detect(answers, "有记者问及")]

# random sampling
training <- sample(answers, 2500, replace = FALSE)

library(openxlsx)

write.xlsx(tibble(Score = "", Text = training), "training.xlsx")
write(training, "training.txt")

