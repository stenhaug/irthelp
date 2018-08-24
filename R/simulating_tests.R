b_offset_mixture <- function(p, nitems){
   gen_one <- function(p){
      ifelse(runif(1) < p, rnorm(1, 0.6, 0.1), rnorm(1, 0, 0.1))
   }
   rerun(nitems, gen_one(p)) %>% unlist()
}

sim_biased_test <- function(d, n, p_f, th_f_mean, n_j, b_offset){
   # students in each group
   n_r <- round(n * p_f)
   n_f <- n - n_r

   # abilities
   th_r <- rnorm(n_r, mean = 0, sd = 1) # note if i change this, need to change get_true_dif
   th_f <- rnorm(n_f, mean = th_f_mean, sd = 1)

   # reference group constants
   b <- rnorm(n_j, mean = 0, sd = 0.15)
   a <- rep(1, n_j)
   c <- rep(0, n_j)

   # matrices to put in the IRT equation
   th_mat <- matrix(c(th_r, th_f), n, n_j, byrow = FALSE)
   b_mat <- matrix(b, n, n_j, byrow = TRUE)

   b_offset_mat <-
      rbind(
         matrix(0, n_r, n_j, byrow = TRUE),
         matrix(b_offset, n_f, n_j, byrow = TRUE)
      )

   a_mat <- matrix(a, n, n_j, byrow = TRUE)
   c_mat <- matrix(c, n, n_j, byrow = TRUE)

   # probability matrix
   prob_mat <- c_mat + (1 - c_mat) /
      (1 + exp(-d*a_mat * (th_mat - b_mat - b_offset_mat)))

   # 1- responses (first half are reference, second half are focal)
   group <- data_frame(
      group = c(rep("reference", n_r), rep("focal", n_f))
   )

   answers <- as_tibble(
      ifelse(prob_mat > matrix(runif(n_j*n), n, n_j), 1, 0)
   )

   resp <- bind_cols(group, answers)

   # 2- student info
   student <- data_frame(group = group$group, theta = c(th_r, th_f))

   # 3- item info
   item <- data_frame(b, b_offset, a, c)

   # output these 3
   list(resp = resp, student = student, item = item)
}
