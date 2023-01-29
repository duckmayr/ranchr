## Helper function that adjusts monster stats as they would be in combining
adjust_stats = function(x) {
    stopifnot("Monster" %in% class(x))
    res = x$stats * c(0, 0.5, 1, 1.5, 2)[x$statgains]
    nms = names(res)
    res = stats::setNames(round(pmin(999, res)), nms)
    return(res)
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

#' See how two Monsters will combine
#'
#' @param e1 A \code{\link{Monster}} object
#' @param e2 A \code{\link{Monster}} object
#'
#' @return An object of class "MonsterCombination"; a list with elements
#' \describe{
#'     \item{parent1_stats}{The ordered, adjusted stats of \code{e1}}
#'     \item{parent2_stats}{The ordered, adjusted stats of \code{e2}}
#'     \item{parent_stat_orders}{A data frame listing the parents' stat orders}
#'     \item{baby_stat_orders}{A data frame listing potential babies' stat orders}
#'     \item{best_match}{The predicted best outcome from combining}
#' }
#'
#' @export
`+.Monster` = function(e1, e2) {
    breeds = c(e1$main, e2$main, e1$sub, e2$sub)
    possible = with(ranchr::monsterlist, main %in% breeds & sub %in% breeds)
    possibles = ranchr::monsterlist$monster[possible]
    sorted_stats1 = order_stats(e1)
    sorted_stats2 = order_stats(e2)
    possible_baby_orderings = sapply(possibles, function(x) {
        names(sort(ranchr::monsters[[x]]$stats, decreasing = TRUE))
    })
    ## TODO: Should this be lexicographic or weighted in some way?
    matching_stats = apply(possible_baby_orderings, 2, function(y) {
        sum(y == names(sorted_stats1) & y == names(sorted_stats2))
    })
    best_match = names(matching_stats)[which.max(matching_stats)]
    abbreviations = c(
        life = "L", power = "P", intelligence = "I",
        skill = "Sk", speed = "Sp", defense = "D"
    )
    parent_stat_orders = data.frame(
        Monster = c(e1$monster, e2$monster),
        Main = c(e1$main, e2$main),
        Sub = c(e1$sub, e2$sub),
        "Stat Order" = c(
            paste(abbreviations[names(sorted_stats1)], collapse = " "),
            paste(abbreviations[names(sorted_stats2)], collapse = " ")
        ),
        check.names = FALSE
    )
    baby_stat_orders = data.frame(
        Monster = possibles,
        Main = sapply(possibles, function(x) ranchr::monsters[[x]]$main),
        Sub = sapply(possibles, function(x) ranchr::monsters[[x]]$sub),
        "Stat Order" = apply(possible_baby_orderings, 2, function(x) {
            paste(abbreviations[x], collapse = " ")
        }),
        Matches = matching_stats,
        check.names = FALSE
    )
    return(
        structure(
            .Data = list(
                parent1_stats = sorted_stats1,
                parent2_stats = sorted_stats2,
                parent_stat_orders = parent_stat_orders,
                baby_stat_orders = baby_stat_orders,
                best_match = best_match
            ),
            class = c("MonsterCombination", "list")
        )
    )
}
