consistency_rate = function(dt) {
    table(dt)
    y = sort(table(dt), decreasing = TRUE)
    consistency_rate = y[1] / length(dt)
    result = c(as.integer(names(consistency_rate)), consistency_rate[[1]])
    return(result)
}
