
get_qst_idx <- function(txt) {
  
  idx <- which(str_detect(txt, "^问：|记者：") %in% TRUE)
  diff <- append(idx[2:length(idx)], length(txt) + 1) - idx
  
  if (1 %in% diff) {
    print(idx)
    warning(immediate. = TRUE, "QA length 1. Please Check.")
  }
  return(idx)
}



get_ans_idx <- function(txt) {
  
  q_idx <- get_qst_idx(txt)
  j <- 0
  start_idx <- 1
  
  if (q_idx[1] == 1) {
    l <- vector("list", length(q_idx))
  } else {
    l <- vector("list", length(q_idx) + 1)
    l[[1]] <- vector("list", 2)
    l[[1]][[1]] <- 0
    l[[1]][[2]] <- 1:(q_idx[1] - 1)
    j <- j + 1
    start_idx <- q_idx[1]
  }
  
  
  for (i in start_idx:length(txt)) {
    
    if (i %in% q_idx) {
      j <- j + 1
      l[[j]] <- vector("list", 2)
      l[[j]][[1]] <- i
    } else {
      l[[j]][[2]] <- append(l[[j]][[2]], i)
    }
  }
  return(l)
}


# Index to text

idx_to_text <- function(text, idx) {
  
  l <- vector("list", length(idx))
  
  for (i in 1:length(idx)) {
    
    l[[i]] <- vector("list", 2)
    
    if (idx[[i]][[1]] == 0) {
      l[[i]][[1]] <- "问：" 
    } else {
      l[[i]][[1]] <- text[idx[[i]][[1]]]
    }
    
    l[[i]][[2]] <- text[idx[[i]][[2]]]
    
  }
  return(l)
} 





