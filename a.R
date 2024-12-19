runtimeVsrating = aggregate(basics_ratings$averageRating, list(basics_ratings$runtimeMinutes), FUN=mean)
colnames(runtimeVsrating)[1] = "runtimeMinutes"
colnames(runtimeVsrating)[2] = "meanRating"
plot(runtimeVsrating$runtimeMinutes, runtimeVsrating$meanRating, xlab = "Runtime Minutes", ylab = "Aveeraged Rating Over Different Runtimes")
runtimeVsrating$runtimeMinutes = sapply(runtimeVsrating$runtimeMinutes, as.numeric)
runtimeVsrating$meanRating = sapply(runtimeVsrating$meanRating, as.numeric)

limit_data = runtimeVsrating[runtimeVsrating$runtimeMinutes<400,]
plot(limit_data$runtimeMinutes,limit_data$meanRating, xlab = "Runtime Minutes", ylab = "Aveeraged Rating Over Different Runtimes")
linear_fit = lm(limit_data$meanRating ~ limit_data$runtimeMinutes)
abline(linear_fit,lw =2, col = "blue")

