## Helper function to center text (pad w spaces on both sides) in certain width
center_text = function(x, width) {
    text_length = nchar(x)
    left_pad = floor((width - text_length) / 2)
    right_pad = width - text_length - left_pad
    return(paste0(strrep(" ", left_pad), x, strrep(" ", right_pad)))
}

#' @export
print.Monster = function(x, ...) {
    w = getOption("width")
    breed  = paste0("(", x$main, " / ", x$sub, ")")
    header = "    Stat       Current      Gain      Adjusted   Baseline"
    hline  = "---------------------------------------------------------"
    fixed  = adjust_stats(x)
    order_ = paste(tools::toTitleCase(names(order_stats(x))), collapse = ", ")
    gains  = x$statgains
    gains  = sprintf("%d (x %0.2f)", gains, c(0, 0.5, 1, 1.5, 2)[gains])
    gains  = stats::setNames(gains, names(x$stats))
    cat("\n")
    if ( x$name != "" ) {
        cat(center_text(x$name, w), "\n\n", sep = "")
    }
    cat(center_text(x$monster, w), "\n", sep = "")
    cat(center_text(breed, w), "\n\n", sep = "")
    cat(center_text(header, w), "\n", sep = "")
    cat(center_text(hline, w), "\n", sep = "")
    for ( stat in names(x$stats) ) {
        Stat = tools::toTitleCase(stat)
        this_row = paste0(Stat, strrep(" ", 12 - nchar(stat)), "   ")
        this_row = paste0(this_row, sprintf("%7d", x$stats[stat]), "   ")
        this_row = paste0(this_row, sprintf("%s", gains[stat]), "   ")
        this_row = paste0(this_row, sprintf("%8d", fixed[stat]), "   ")
        this_row = paste0(this_row, sprintf("%8d", x$baselines[stat]))
        cat(center_text(this_row, w), "\n", sep = "")
    }
    cat("\n")
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
    stat_width = 13 + 3 + 7
    remaining_width = w - stat_width - 2
    dat = x$baby_stat_orders
    dat = dat[order(dat$Matches, decreasing = TRUE), ]
    header = colnames(dat)
    breed_col_widths = apply(dat[ , 1:3], 2, function(x) max(nchar(x)))
    if ( sum(breed_col_widths) + 9 > remaining_width ) {
        excess = sum(breed_col_widths) + 9 - remaining_width
        to_trim = ceiling(excess / 2)
        breed_col_widths[2:3] = breed_col_widths[2:3] - to_trim
        abb = function(x, ...) abbreviate(names.arg = x, ..., dot = TRUE)
        header[2] = abb(header[2], minlength = breed_col_widths[2]-1)
        header[3] = abb(header[3], minlength = breed_col_widths[3]-1)
        dat$Main  = abb(dat$Main,  minlength = breed_col_widths[2]-1)
        dat$Sub   = abb(dat$Sub,   minlength = breed_col_widths[3]-1)
    }
    total_table_width = sum(breed_col_widths) + 9 + stat_width
    widths = c(breed_col_widths, 13, 7)
    header = sapply(1:5, function(i) center_text(header[i], widths[i]))
    header = paste(header, collapse = "   ")
    table_body = apply(dat, 1, function(r) {
        paste(
            paste0(r[1], strrep(" ", max(0, widths[1] - nchar(r[1])))),
            paste0(r[2], strrep(" ", max(0, widths[2] - nchar(r[2])))),
            paste0(r[3], strrep(" ", max(0, widths[3] - nchar(r[3])))),
            r[4],
            center_text(r[5], 7),
            sep = "   "
        )
    })
    centered_title = center_text("Possible Outcomes", total_table_width)
    cat(center_text(centered_title, w), "\n", sep = "")
    cat(center_text(header, w), "\n", sep = "")
    cat(center_text(strrep("-", nchar(header) + 2), w), "\n", sep = "")
    cat(center_text(table_body, w), sep = "\n")
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
    stat_width = 13 + 3 + 7
    remaining_width = w - stat_width - 2
    dat = x$stat_orders
    dat = dat[order(dat$Matches, decreasing = TRUE), ]
    header = colnames(dat)
    breed_col_widths = apply(dat[ , 1:3], 2, function(x) max(nchar(x)))
    if ( sum(breed_col_widths) + 9 > remaining_width ) {
        excess = sum(breed_col_widths) + 9 - remaining_width
        to_trim = ceiling(excess / 2)
        breed_col_widths[2:3] = breed_col_widths[2:3] - to_trim
        abb = function(x, ...) abbreviate(names.arg = x, ..., dot = TRUE)
        header[2] = abb(header[2], minlength = breed_col_widths[2]-1)
        header[3] = abb(header[3], minlength = breed_col_widths[3]-1)
        dat$Main  = abb(dat$Main,  minlength = breed_col_widths[2]-1)
        dat$Sub   = abb(dat$Sub,   minlength = breed_col_widths[3]-1)
    }
    total_table_width = sum(breed_col_widths) + 9 + stat_width
    widths = c(breed_col_widths, 13, 7)
    header = sapply(1:5, function(i) center_text(header[i], widths[i]))
    header = paste(header, collapse = "   ")
    table_body = apply(dat, 1, function(r) {
        paste(
            paste0(r[1], strrep(" ", max(0, widths[1] - nchar(r[1])))),
            paste0(r[2], strrep(" ", max(0, widths[2] - nchar(r[2])))),
            paste0(r[3], strrep(" ", max(0, widths[3] - nchar(r[3])))),
            r[4],
            center_text(r[5], 7),
            sep = "   "
        )
    })
    centered_title = center_text("Possible Second Parents", total_table_width)
    cat(center_text(centered_title, w), "\n", sep = "")
    cat(center_text(header, w), "\n", sep = "")
    cat(center_text(strrep("-", nchar(header) + 2), w), "\n", sep = "")
    cat(center_text(table_body, w), sep = "\n")
    cat("\n")
    invisible(x)
}
