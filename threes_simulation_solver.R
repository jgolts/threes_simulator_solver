library(tidyverse)
library(furrr)
library(progressr)

plan(multisession)

set.seed(157)

exp_memo <- new.env(hash = T)
uniq_memo <- new.env(hash = T)

gen_cartes <- function(k) {
  as.matrix(expand.grid(rep(list(1:6), k)))
}

gen_uniq <- function(roll) {
  
  roll_concat <- paste(sort(roll), collapse = "-")
  
  if (exists(roll_concat, envir = uniq_memo)) {
    return(uniq_memo[[roll_concat]])
  }
  
  n <- length(roll)
  all_combos <- list()
  
  for (k in 1:n) {
    combo <- combn(roll, k, simplify = F)
    
    all_combos <- c(all_combos, combo)
  }
  
  combo_concat <- sapply(all_combos, function(x) paste(sort(x), collapse = "-"))
  
  uniq <- tibble(key = combo_concat, subset = all_combos) %>% 
    distinct(key, .keep_all = T)
  
  return(uniq)
  
}

gen_min_set <- function(roll, n_saved, sum_saved){
  uniq <- gen_uniq(roll)
  min_score <- Inf
  
  for (j in 1:nrow(uniq)) {
    subs <- uniq$subset[[j]]
    saved_now <- sum(subs)
    k <- length(subs)
    
    future <- gen_expected(n_saved + k, sum_saved + saved_now)
    
    if (future < min_score) {
      min_score <- future
      min_subs <- subs
    }
  }
  
  return(list(score = min_score, subset = min_subs))
}

gen_expected <- function(n_saved, sum_saved){
  
  if (n_saved == 5) {
    return(sum_saved)
  }
  
  exp_concat <- paste (n_saved, sum_saved, sep = ":")
  if (exists(exp_concat, envir = exp_memo)) {
    return(exp_memo[[exp_concat]])
  }
  
  k <- 5 - n_saved
  cartes <- gen_cartes(k)
  
  expected <- with_progress({
    p <- progressor(steps = nrow(cartes))
    
    scores <- future_map_dbl(
      1:nrow(cartes),
      function(i) {
        roll <- as.integer(cartes[i, ])
        min_set <- gen_min_set(roll, n_saved, sum_saved)
        p()
        return(min_set$score)
      },
      .options = furrr_options(seed = TRUE)
    )
    
    mean(scores)
  })
  
  exp_memo[[exp_concat]] <- expected
  return(expected)
}

simulate_game <- function(strategy = c("best", "random", "worst")) {
  n_saved <- 0
  sum_saved <- 0
  remaining <- 5
  
  while (n_saved < 5) {
    roll <- sample(1:6, remaining, replace = T)
    
    uniq <- gen_uniq(roll)
    
    if (strategy == "best") {
      min_set <- gen_min_set(roll, n_saved, sum_saved)
      saved <- min_set$subset
    } 
    else if (strategy == "random") {
      i <- sample(1:nrow(uniq), 1)
      saved <- uniq$subset[[i]]
    } 
    else if (strategy == "worst") {
      worst_score <- Inf
      for (j in 1:nrow(uniq)) {
        subs <- uniq$subset[[j]]
        future <- gen_expected(n_saved + length(subs), sum_saved + sum(subs))
        
        if (future > worst_score) {
          worst_score <- future
          saved <- subs
        }
      }
    }
    
    n_saved <- n_saved + length(saved)
    sum_saved <- sum_saved + sum(saved)
    remaining <- 5 - n_saved
  }
  
  return(sum_saved)
}

simulate <- function(n = 1000) {
  scores <- tibble(
    best = replicate(n, simulate_game("best")),
    random = replicate(n, simulate_game("random")),
    worst = replicate(n, simulate_game("worst"))
  )
  
  return(scores)
}

gen_expected(0, 0)

scores <- simulate(100)