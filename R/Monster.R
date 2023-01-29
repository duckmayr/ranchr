#' Create a Monster object
#'
#' @param name A character vector of length one; the monster's given name
#' @param monster A character vector of length one; the monster's breed name;
#'     if \code{""} (the default), this will be determined by the \code{main}
#'     and \code{sub} arguments
#' @param main A character vector of length one; the monster's main breed;
#'     if \code{""} (the default), this will be determined by the \code{monster}
#'     argument
#' @param sub A character vector of length one; the monster's sub breed;
#'     if \code{""} (the default), this will be determined by the \code{monster}
#'     argument
#' @param stats A numeric vector of length six; the monster's current stats;
#'     if \code{NULL} (the default), the baseline stats of the monster breed
#'     will be used; the vector should either be in the order
#'     life, power, intelligence, skill, speed, defense, or else the elements
#'     should have those names
#'
#' @return An object of class "Monster"; a list with elements
#' \describe{
#'     \item{name}{The monster's given name}
#'     \item{monster}{The monster's breed name}
#'     \item{main}{The monster's main breed}
#'     \item{sub}{The monster's sub breed}
#'     \item{stats}{The monster's current stats}
#'     \item{baselines}{The monster's breed's baseline stats}
#'     \item{statgains}{The monster's breed's statgain information}
#'     \item{lifespan}{The monster's breed's base lifespan (in weeks)}
#' }
#'
#' @export
Monster = function(name = "", monster = "", main = "", sub = "", stats = NULL) {
    ## Identify the type of monster we're dealing with & get the template
    if ( sub == "" ) {
        sub = main
    }
    if ( monster != "" ) {
        if ( !(monster %in% ranchr::monsterlist$monster) ) {
            stop("ranchr does not have data on ", monster)
        }
    } else {
        condition1 = any(ranchr::monsterlist$main == main)
        condition2 = any(ranchr::monsterlist$sub == sub)
        if ( !(condition1 & condition2) ) {
            stop("ranchr does not have data on a ", main, "/", sub)
        }
        monster = ranchr::monsterlist$monster[which(condition1 & condition2)]
    }
    template = ranchr::monsters[[monster]]
    if ( main == "" ) {
        main = template$main
    }
    if ( sub == "" ) {
        sub = template$sub
    }
    if ( main != template$main | sub != template$sub ) {
        warning(
            "Your main/sub specification doesn't match the monster breed ",
            "you specified; assuming the breed name given is what you want"
        )
    }
    ## Sanity checks on stats
    if ( !is.null(stats) ) {
        if ( length(stats) != 6 ) {
            stop("stats should be of length 6")
        }
        nms = c("life", "power", "intelligence", "skill", "speed", "defense")
        if ( is.null(names(stats)) ) {
            names(stats) = nms
        } else {
            if ( !setequal(names(stats), nms) ) {
                warning(
                    "stats was not properly named; operating under the ",
                    "assumption it's in the following order:\n",
                    paste(nms, collapse = ", ")
                )
                names(stats) = nms
            }
            stats = stats[nms]
        }
    }
    ## Set the name and stats and return
    template$name = name
    if ( !is.null(stats) ) {
        template$stats = stats
    }
    return(template)
}
