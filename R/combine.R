## Helper function that adjusts monster stats as they would be in combining
adjust_stats = function(x) {
    stopifnot("Monster" %in% class(x))
    return(x$stats * c(0, 0.5, 1, 1.5, 2)[x$statgains])
}

## Helper function that orders monster stats as they would be in combining
order_stats = function(x) {
    stopifnot("Monster" %in% class(x))
    adjusted_stats = adjust_stats(x)
    sorted_stats = sort(adjusted_stats, decreasing = TRUE)
    for ( i in 1:5 ) {
        if ( sorted_stats[i] == sorted_stats[i + 1] ) {
            baseline1 = x$baselines[names(sorted_stats)[i]]
            baseline2 = x$baselines[names(sorted_stats)[i+1]]
            if ( baseline2 > baseline1 ) {
                new_order = 1:6
                new_order[c(i, i+1)] = new_order[c(i+1, i)]
                sorted_stats = sorted_stats[new_order]
            }
        }
    }
    return(sorted_stats)
}
