library("EasyABC")

prior <- list(c("unif", -10, 10))

model <- function(x) {
  set.seed(x[1])
  
  var1 <- 1.0 / 100.0
  var2 <- 1.0

  if(runif(1) < 0.5) {
    rnorm(1, x[2], var1)
  } else {
    rnorm(1, x[2], var2)
  }
}

observed <- 0

setwd("output/easyABC/")
result <- ABC_sequential("Lenormand", model=model, use_seed=TRUE, prior=prior,
  alpha = 0.1, p_acc_min = 0.01, nb_simul=5000, summary_stat_target=observed, n_cluster=1, verbose=TRUE)
setwd("../..")

for (i in 1:length(result$intermediary)) {
  write.table(
    result$intermediary[[i]]$posterior[,3],
    paste("output/easyABC/lenormand2012_5000_0.1_0.01_",i - 1,"_1.csv", sep=""),
    row.names=FALSE, col.names=FALSE)
}


setwd("output/easyABC/")
result <- ABC_sequential("Beaumont", model=model, use_seed=TRUE, prior=prior,
  tolerance_tab = c(2, 1.5, 1.0, 0.5, 0.1, 0.05, 0.01), nb_simul=5000, summary_stat_target=observed, n_cluster=4, verbose=TRUE)
setwd("../..")

for (i in 1:length(result$intermediary)) {
  write.table(
    result$intermediary[[i]]$posterior[,3],
    paste("output/easyABC/beaumont2009_5000_2.0_0.01_",i - 1,"_1.csv", sep=""),
    row.names=FALSE, col.names=FALSE)
}