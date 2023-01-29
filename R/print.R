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
    header = "    Stat     | Current | Adjusted | Baseline"
    hline  = "-------------+---------+----------+---------"
    fixed  = adjust_stats(x)
    order_ = paste(tools::toTitleCase(names(order_stats(x))), collapse = ", ")
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
        this_row = paste0(Stat, strrep(" ", 12 - nchar(stat)), " | ")
        this_row = paste0(this_row, sprintf("%7d", x$stats[stat]), " | ")
        this_row = paste0(this_row, sprintf("%8d", fixed[stat]), " | ")
        this_row = paste0(this_row, sprintf("%8d", x$baselines[stat]))
        cat(center_text(this_row, w), "\n", sep = "")
    }
    cat("\n")
    cat(center_text(paste0("Stat order:", strrep(" ", 37)), w), "\n", sep = "")
    cat(center_text(order_, w), "\n\n", sep = "")
    invisible(x)
}
