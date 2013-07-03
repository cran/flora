taxon.citation <-
function(spp) {
    res <- get.taxon.info(spp, pbar = FALSE)
    if (!res[[1]]$processed) {cat("The taxon couldn't be processed at this time. 
        Please check your internet connection or try again later\n"); return(NA)}
    if (res[[1]]$found) {
        status <- res[[1]]$accepted
        if (status) {
            cat("Accepted name.\n")
        } else {
            cat("Synonym of", paste(res[[1]]$accepted.name, ".\n", sep = ""))
        }
        res[[1]]$citation
    } else {
        cat("Not found. Did you mean", paste(res[[1]]$suggestion, "?", sep = ""), 
            "\n")
        NA
    }
}