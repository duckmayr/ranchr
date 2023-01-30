#' Engineer a Monster combination
#'
#' @param parent A \code{\link{Monster}} object
#' @param child A \code{\link{Monster}} object
#'
#' @return An object of class "MonsterComboEngineering"; a list with elements
#' \describe{
#'     \item{given_parent}{The \code{\link{Monster}} given in \code{parent}}
#'     \item{given_child}{The \code{\link{Monster}} given in \code{child}}
#'     \item{stat_orders}{A data frame listing potential parents' stat orders}
#' }
#'
#' @export
engineer = function(parent, child) {
    sort_ = function(x) sort(x, decreasing = TRUE)
    missing_parent = missing(parent)
    missing_child  = missing(child)
    if ( missing_parent & missing_child ) {
        stop("At least one of 'parent' or 'child' must be given", call. = FALSE)
    } else if ( missing_parent ) {
        stop("Engineering from child only not yet implemented", call. = FALSE)
    } else if ( missing_child ) {
        stop("Engineering from parent only not yet implemented", call. = FALSE)
    } else {
        child_breeds = c(child$main, child$sub)
        parent1_breeds = c(parent$main, parent$sub)
        shares_breed = any(parent1_breeds %in% child_breeds)
        purebreed_child = child$main == child$sub
        if ( !(shares_breed | purebreed_child) ) {
            stop("Cannot generate this child with this parent")
        }
        parent1_stats = names(order_stats(parent))
        child_stats = names(sort_(child$baselines))
        matches = parent1_stats[parent1_stats == child_stats]
        if ( length(matches) == 0 ) {
            stop("This parent and child have 0 stat order matches")
        }
        if ( parent$monster == child$monster ) {
            needed = child$sub
        } else {
            needed = setdiff(child_breeds, parent1_breeds)
        }
        possible = with(ranchr::monsterlist, main %in% needed | sub %in% needed)
        possibles = ranchr::monsterlist$monster[possible]
        possible_orderings = sapply(possibles, function(x) {
            m = ranchr::monsters[[x]]
            sorted_gains = sort_(m$statgains)
            for ( i in 1:5 ) {
                if ( sorted_gains[i] == sorted_gains[i + 1] ) {
                    baseline1 = m$baselines[names(sorted_gains)[i]]
                    baseline2 = m$baselines[names(sorted_gains)[i+1]]
                    if ( baseline2 > baseline1 ) {
                        new_order = 1:6
                        new_order[c(i, i+1)] = new_order[c(i+1, i)]
                        sorted_gains = sorted_gains[new_order]
                    }
                }
            }
            return(names(sorted_gains))
        })
        matching_stats = apply(possible_orderings, 2, function(y) {
            sum(y == parent1_stats & y == child_stats)
        })
        abbreviations = c(
            life = "L", power = "P", intelligence = "I",
            skill = "Sk", speed = "Sp", defense = "D"
        )
        stat_orders = data.frame(
            Monster = possibles,
            Main = sapply(possibles, function(x) ranchr::monsters[[x]]$main),
            Sub = sapply(possibles, function(x) ranchr::monsters[[x]]$sub),
            "Stat Order" = apply(possible_orderings, 2, function(x) {
                paste(abbreviations[x], collapse = " ")
            }),
            Matches = matching_stats,
            check.names = FALSE
        )
        stat_orders = stat_orders[order(stat_orders$Matches), ]
        bad_match = with(stat_orders, Monster == parent$monster | Matches == 0)
        stat_orders = stat_orders[!bad_match, ]
    }
    return(
        structure(
            .Data = list(
                given_parent = parent,
                given_child  = child,
                stat_orders  = stat_orders
            ),
            class = c("MonsterComboEngineering", "list")
        )
    )
}
