fmt.taxa <-
function(taxa) {
    if(!inherits(taxa, "flora")) stop("Taxa is not of class flora.")
    family <- unlist(lapply(taxa, `[`, "family"))
    name <- unlist(lapply(taxa, `[`, "name"))
    author <- unlist(lapply(taxa, `[`, "author"))
    accepted <- unlist(lapply(taxa, `[`, "accepted"))
    accepted.name <- unlist(lapply(taxa, `[`, "accepted.name"))
    found <- unlist(lapply(taxa, `[`, "found"))
    suggestion <- unlist(lapply(taxa, `[`, "suggestion"))
    out <- data.frame(
            family = family,
            name = name,
            author = author,
            accepted = accepted,
            accepted.name = accepted.name,
            found = found,
            suggestion = suggestion,
            stringsAsFactors = FALSE
           )
    rownames(out) <- NULL
    out
}
