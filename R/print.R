## Helper function to center text (pad w spaces on both sides) in certain width
center_text = function(x, width) {
    text_length = nchar(x)
    left_pad = floor((width - text_length) / 2)
    right_pad = width - text_length - left_pad
    return(paste0(strrep(" ", left_pad), x, strrep(" ", right_pad)))
}

## Helper function to convert a matrix to a table
matrix_as_table = function(x, align, maxcolwidth) {
    if ( missing(align) ) { align = paste0("l", strrep("r", ncol(x)-1)) }
    widths  = pmax(nchar(colnames(x)), apply(x, 2, function(y) max(nchar(y))))
    if ( !missing(maxcolwidth) ) {
        for ( j in 1:ncol(x) ) {
            if ( widths[j] > maxcolwidth ) {
                x[, j] = abbreviate(x[, j], maxcolwidth-1, dot = TRUE)
            }
        }
    }
    header  = paste(mapply(center_text, colnames(x), widths), collapse = "   ")
    hline   = strrep("-", nchar(header))
    align   = strsplit(align, "")[[1]]
    align   = ifelse(align == "l", "-", ifelse(align == "r", "", align))
    tblbody = apply(sapply(1:ncol(x), function(j) {
        if ( align[j] == "c" ) {
            return(center_text(x[ , j], widths[j]))
        }
        return(sprintf(paste0("%", align[j], widths[j], "s"), x[ , j]))
    }), 1, paste, collapse = "   ")
    return(c(header, hline, tblbody))
}

## Helper function to print such a table to the console
print_matrix_table = function(matrix_table, width) {
    if ( missing(width) ) {
        cat(matrix_table, sep = "\n")
    } else{
        cat(center_text(matrix_table, width), sep = "\n")
    }
    invisible(matrix_table)
}

#' @export
print.Monster = function(x, ...) {
    w = getOption("width")
    breed  = paste0("(", x$main, " / ", x$sub, ")")
    fixed  = adjust_stats(x)
    order_ = paste(tools::toTitleCase(names(order_stats(x))), collapse = ", ")
    gains  = x$statgains
    stat_table = cbind(
        Stat = tools::toTitleCase(names(x$stats)),
        Current = x$stats,
        Gain = sprintf("%d (x %0.2f)", gains, c(0, 0.5, 1, 1.5, 2)[gains]),
        Adjusted = fixed,
        Baseline = x$baselines
    )
    cat("\n")
    if ( x$name != "" ) {
        cat(center_text(x$name, w), "\n\n", sep = "")
    }
    cat(center_text(x$monster, w), "\n", sep = "")
    cat(center_text(breed, w), "\n\n", sep = "")
    print_matrix_table(matrix_as_table(stat_table), w)
    cat("\n\n")
    cat(center_text(paste0("Stat order:", strrep(" ", 37)), w), "\n", sep = "")
    cat(center_text(order_, w), "\n\n", sep = "")
    invisible(x)
}

#' @export
print.MonsterCombination = function(x, ...) {
    w = getOption("width")
    header = paste(x$parent_stat_orders$Monster, collapse = " with ")
    header = paste0("    Combining ", header)
    cat("\n", header, "\n\n", sep = "")
    abbreviations = c(
        life = "L", power = "P", intelligence = "I",
        skill = "Sk", speed = "Sp", defense = "D"
    )
    hdr2 = paste(
        x$parent_stat_orders$Main,
        x$parent_stat_orders$Sub,
        sep = " / "
    )
    col_lengths = nchar(hdr2)
    hdr1 = sapply(1:2, function(i) {
        center_text(x$parent_stat_orders$Monster[i], col_lengths[i])
    })
    hdr1 = paste(hdr1, collapse = "    ")
    hdr2 = paste(hdr2, collapse = "    ")
    cat(center_text("Parents' Adjusted & Ordered Stats", w), "\n", sep = "")
    cat(center_text(hdr1, w), "\n", sep = "")
    cat(center_text(hdr2, w), "\n", sep = "")
    cat(center_text(strrep("-", nchar(hdr1) + 2), w), "\n", sep = "")
    col1 = center_text(sprintf(
        "%-3s %3d",
        paste0(abbreviations[names(x$parent1_stats)], ":"),
        x$parent1_stats
    ), col_lengths[1])
    col2 = center_text(sprintf(
        "%-3s %3d",
        paste0(abbreviations[names(x$parent2_stats)], ":"),
        x$parent2_stats
    ), col_lengths[2])
    table_body = paste(col1, col2, sep = "    ")
    cat(center_text(table_body, w), sep = "\n")
    cat("\n")
    sayings = c(
        "This one's all up to you",
        "The prospect is unsure",
        "This combination doesn't look so good, I can't recommend it",
        "The prospect is fine... It will probably work out",
        "The prospect of this combination is good. You can look forward to it",
        NA,
        paste(
            "The prospect of this combination is great.",
            "It can't go wrong unless something weird happens"
        )
    )
    matches = sum(names(x$parent1_stats) == names(x$parent2_stats))
    explainer = paste0("(That means there are ", matches, " matching stats)")
    if ( x$parent_stat_orders$Monster[1] == x$parent_stat_orders$Monster[2] ) {
        matches = 0
        explainer = "(That's because the parents are the same exact breed)"
    }
    saying = strwrap(sayings[matches + 1], w - 16)
    saying = paste(saying, collapse = paste0("\n", strrep(" ", 16)))
    cat("    Dadge says: ", saying, "\n\n", sep = "")
    explainer = strwrap(explainer, w - 4)
    explainer = paste(explainer, collapse = paste0("\n", strrep(" ", 4)))
    cat("    ", explainer, "\n\n", sep = "")
    dat = x$baby_stat_orders
    dat = dat[order(dat$Matches, decreasing = TRUE), ]
    dat = matrix_as_table(dat, align = "lllcc", maxcolwidth = 13)
    cat(center_text("Possible Outcomes", w), "\n", sep = "")
    print_matrix_table(dat, w)
    cat("\n")
    invisible(x)
}

#' @export
print.MonsterComboEngineering = function(x, ...) {
    w = getOption("width")
    h = paste(
        "Combining", x$given_parent$monster,
        "to get a", x$given_child$monster
    )
    cat("\n    ", h, "\n\n", sep = "")
    dat = x$stat_orders[order(x$stat_orders$Matches, decreasing = TRUE), ]
    dat = matrix_as_table(dat, align = "lllcc", maxcolwidth = 13)
    cat(center_text("Possible Second Parents", w), "\n", sep = "")
    print_matrix_table(dat, w)
    cat("\n")
    invisible(x)
}
