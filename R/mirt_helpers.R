model_output <- function(model){
   tmp <-
      coef(model) %>%
      purrr::map(~ .[-length(.)]) %>%
      purrr::map(~ do.call(rbind, .))

   data_frame(
      var = c(paste0("item", 1:nrow(tmp$focal)), "mean_ability", "var_ability"),
      reference = c(-tmp$reference[, "d"], coef(model)$reference$GroupPars[1], coef(model)$reference$GroupPars[2]),
      focal = c(-tmp$focal[, "d"], coef(model)$focal$GroupPars[1], coef(model)$focal$GroupPars[2])
   ) %>%
      mutate(difference = focal - reference)
}

rasch_all_as_anchors <- function(resp){
   mirt::multipleGroup(
      data = resp[, -1],
      model = 1,
      itemtype = "Rasch",
      group = forcats::fct_rev(as.character(resp[[1]])),
      invariance = c(names(resp)[-1], "free_means")
   )
}

rasch_with_anchors <- function(resp, anchors){
   mirt::multipleGroup(
      data = resp[ , -1],
      model = 1,
      itemtype = "Rasch",
      group = forcats::fct_rev(as.character(resp[[1]])),
      invariance = c(paste0("V", anchors), "free_means")
   )
}

get_achieve_gap <- function(model_output){
   filter(model_output, var == "mean_ability")$focal
}
