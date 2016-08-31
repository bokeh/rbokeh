context("boxplot")

test_that("boxplot with muchs points is faster", {
  xdat <- data.frame(
    group=rep('bg', 20000),
    score=rnorm(20000),
    stringsAsFactors=FALSE)
  idx <- sample(nrow(xdat), 50)
  xdat$group[idx] <- 'fg'
  xdat$score[idx] <- rnorm(length(idx), 0.75)

  system.time(p <- figure(xdat) %>% ly_boxplot(x=group, y=score))
  ## w/o do.call
  ##    user  system elapsed
  ##   0.139   0.004   0.146
  ##
  ## w/ do.call


})
