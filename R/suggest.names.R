suggest.names <-
function(spp) {
    spp <- fixCase(spp)
    first.letter <- strsplit(spp, "")[[1]][1]
    species.first.letter <- species[grep(paste("^", first.letter, sep = ""), species)]
    l1 <- length(as.character(spp))
    l2 <- length(as.character(species.first.letter))
    out <- .C("levenshtein", as.character(spp), as.character(species.first.letter), 
        l1, l2, ans = integer(max(l1, l2)), PACKAGE = "flora")
    # distance <- levenshteinSim(spp, species.first.letter)
    distance <- 1 - (out$ans/pmax(nchar(spp), 
        nchar(species.first.letter)))
    max.dist <- max(distance, na.rm = TRUE)
    if (max.dist >= 0.75) {
        species.first.letter[distance == max(distance, na.rm = TRUE)][1]
    } else {
        NA
    }
}
