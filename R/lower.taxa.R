lower.taxa <-
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
        tax.rank <- res[[1]]$tax.rank
        if (tax.rank == "family or tribe") {
            lt <- get.taxon.info(res[[1]]$lower.taxa, pbar = FALSE)
            lt <- c(na.omit(unlist(lapply(lt, function(x) x$lower.taxa))))
            names(lt) <- NULL
        } else {
            if (tax.rank == "genus") {
                lt <- c(na.omit(res[[1]]$lower.taxa))
            } else {
                lt <- NULL
            }   
        }
    } else {
        cat("Not found. Did you mean", paste(res[[1]]$suggestion, "?", sep = ""), 
            "\n")
        return(NA)
    }
    lt
}