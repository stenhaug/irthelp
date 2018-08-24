stan_fit <- function(x){
   x <- as.data.frame(x)
   x[, 1] <- ifelse(x[, 1] == "reference", 1, 2)
   gr <- x[, 1]
   resp <- x[, -1]
   J <- nrow(resp)
   K <- ncol(resp)
   y <- list()
   jj <- list()
   kk <- list()
   for (i in 1:ncol(resp)) {
      resp[, i] -> y[[i]]
      1:nrow(resp) -> jj[[i]]
      rep(i, nrow(resp)) -> kk[[i]]
   }
   do.call("c", y) -> y
   do.call("c", jj) -> jj
   do.call("c", kk) -> kk

   L <- list(J = J, K = K, N = length(y), jj = jj, kk = kk, y = y, g = gr)

   fit <- stan(file = "stan/irt_ml.stan", data = L)
   fit
}

bayes_factor <- function(fit, i){
   LRO.utilities::savage_dickey(
      rstan::extract(fit)$beta[,,2][,i] - rstan::extract(fit)$beta[,,1][,i],
      y[,2] - y[,1],
      Q = 0
   )
}

bayes_factor_df <- function(fit, items){
   data_frame(
      item_original = items, # items is a bit tricky because we want to renumber
      item_new = 1:length(items),
      bf = 1:length(items) %>% map_dbl(~ bayes_factor(fit, .)$BF10)
   ) %>%
      dplyr::select(item_original, item_new, bf)
}

get_anchors <- function(fit_bf, threshold){
   fit_bf %>%
      filter(bf > threshold) %>%
      pull(item_original)
}

just_invariant_items <- function(resp, fit_bf, threshold){
   invariant_items <- fit_bf %>% filter(bf >= threshold) %>% pull(item_original)

   if(length(invariant_items) == 0){return(NA)}

   resp[ , c(1, invariant_items + 1)]
}
