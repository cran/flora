get.taxon.info <-
function(taxa, verbose = FALSE, pbar = TRUE) {
    if (inherits(taxa, "flora")) {
        processed.taxa <- unlist(lapply(taxa, `[[`, "processed"))
        if (all(processed.taxa)) invisible(taxa)
        taxa.list <- names(taxa)[!processed.taxa]
        counter <- sum(processed.taxa)
        out <- taxa
    } else {
        if (class(taxa) == "character") {
            taxa <- gsub("^\\s+|\\s+$", "", taxa)
            taxa.list <- taxa[taxa != ""]
            taxa.list <- sapply(taxa.list, fixCase)
            names(taxa.list) <- NULL
            out <- vector("list", length(taxa.list))
            names(out) <- taxa.list
            class(out) <- c("list", "flora")
            for (i in seq_along(out)) out[[i]] <- list(processed = FALSE)
            counter <- 0 
            if (length(taxa.list) == 1) pbar = FALSE
        } else {
            stop("Unrecognised taxa type.")
        }
    }
    curl <- getCurlHandle()
    failures <- 0
    if (pbar) 
        pb <- txtProgressBar(min = 0, max = length(taxa), style = 3)
    for (spp in taxa.list) {
        if (failures >= 5) {warning("Looks like Flora do Brasil is down. Please try again later."); return(out)}
        counter <- counter + 1
        if (pbar) 
            setTxtProgressBar(pb, counter)
        splitted.spp <- strsplit(spp, " ")
        accepted <- NA
        if (length(splitted.spp[[1]]) == 1) {
            if (length(grep("ceae", spp)) == 1) {
                tax.rank <- "family or tribe"
            } else {
                tax.rank <- "genus"
            }
        } else {
            tax.rank <- "species"
        }
        spp.id <- ids[species == spp]
        if ((length(spp.id) == 0) || (length(spp.id) > 1)) {
            if (length(spp.id) == 0) {
                sgst = suggest.names(spp)
                fnd = FALSE
            } else {
                sgst = "duplicated entry check on flora"
                fnd = TRUE
            }
            out[[spp]] <- list(
                tax.rank = NA,
                id = NA,
                accepted = NA,
                accepted.name = NA,
                family = NA,
                name = spp, 
                author = NA,
                lower.taxa = NA,
                synonyms = NA,
                found = fnd, 
                suggestion = sgst, 
                citation = NA,
                processed = TRUE)
            next
        }
        res <- tryCatch(getURL(paste(flora.url, spp.id, sep = ""), curl = curl, .opts = list(timeout = 10, 
            maxredirs = 2, verbose = verbose)), error = function(e) return("timeout"))
        if (res[1] == "erro" | res[1] == "timeout") {
            res <- tryCatch(getURL(paste(flora.url, spp.id, sep = ""), curl = curl, 
                .opts = list(timeout = 10, maxredirs = 2, verbose = verbose)), error = function(e) return("timeout"))
            if (res[1] == "erro") {
                out[[spp]] <- list(
                tax.rank = NA,
                id = NA,
                accepted = NA,
                accepted.name = NA,
                family = NA,
                name = spp, 
                author = NA,
                lower.taxa = NA,
                synonyms = NA,
                found = FALSE, 
                suggestion = NA, 
                citation = NA,
                processed = FALSE)
                next
            }
            if (res[1] == "timeout") {
                failures <- failures + 1
                next
            } 
        }
        res <- fromJSON(res)
        family <- gsub("^\\s+|\\s+$", "", xmlValue(getNodeSet(htmlParse(res$hierarquia), 
            "//div[@class = 'taxon']/text()")[[1]]))
        not.accepted.regex <- "[Ss]in\u00F4nimo|n\u00E3o|rejeitado|ileg\u00EDtimo|Variante|mal"
        accepted.regex <- "^Nome aceito$|^Nome correto$|aceito.*desconhecido|aceito.*correto|^Ainda desconhecido$"
        if (length(grep(not.accepted.regex, paste(res$statusQualificador, collapse = " "))) > 
            0) {
            accepted <- FALSE
        } else {
            if (length(grep(accepted.regex, paste(res$statusQualificador, collapse = " "))) > 
                0) {
                accepted <- TRUE
            } else {
                accepted <- NA
            }
        }
        if (accepted && !is.na(accepted)) {
            if (!is.null(res$temComoSinonimo)) {
                synonyms.ids <- unlist(getNodeSet(htmlParse(res$temComoSinonimo), 
                  "//@onclick"))
                synonyms.ids <- gsub("carregaTaxonGrupo\\(|\\)", "", synonyms.ids)
                synonyms <- species[ids %in% synonyms.ids]
            } else {
                synonyms <- NA
            }
            accepted.name <- spp
        } else {
            if (!is.null(res$ehSinonimo)) {
                accepted.name.ids <- unlist(getNodeSet(htmlParse(res$ehSinonimo), 
                  "//@onclick"))
                accepted.name.ids <- gsub("carregaTaxonGrupo\\(|\\)", "", accepted.name.ids)
                accepted.name <- species[ids %in% accepted.name.ids]
                accepted <- FALSE
            } else {
                accepted.name <- NA
            }
            if (!is.null(res$temComoSinonimo)) {
                synonyms.ids <- unlist(getNodeSet(htmlParse(res$temComoSinonimo), 
                  "//@onclick"))
                synonyms.ids <- gsub("carregaTaxonGrupo\\(|\\)", "", synonyms.ids)
                synonyms <- species[ids %in% synonyms.ids]
            } else {
                synonyms <- NA
            }
        }

        if (length(accepted.name) > 1) {
            accepted.name <- paste(accepted.name, collapse = " | ")
        }
        author <- getNodeSet(htmlParse(res$nome), "//div[contains(@class,'Autor')]/text()")
        author <- xmlValue(author[[1]])
        author <- gsub("^\\s+|\\s+$", "", author)
        if (tax.rank == "species") {
            lower.taxa <- NA
        }
        if (tax.rank == "genus") {
            if (length(res$filhosEspecie) > 0 && res$filhosEspecie != "") {
                lower.taxa <- unlist(lapply(getNodeSet(htmlParse(res$filhosEspecie), 
                  "//div[@class = 'taxon']/i/text()"), xmlValue))
            } else {
                lower.taxa <- NA
            }
        }

        if (tax.rank == "family or tribe") {
            if (length(res$filhosGenero) > 0 && res$filhosGenero != "") {
                lower.taxa <- unlist(lapply(getNodeSet(htmlParse(res$filhosGenero), 
                  "//div[@class = 'taxon']/i/text()"), xmlValue))
            } else {
                lower.taxa <- NA
            }
        }
        citation <- gsub("<(.|\n)*?>", "", res$citacao)
        out[[spp]] <- list(
            tax.rank = tax.rank, 
            id = spp.id, 
            accepted = accepted, 
            accepted.name = accepted.name,
            family = family, 
            name = spp, 
            author = author, 
            lower.taxa = lower.taxa, 
            synonyms = synonyms, 
            found = TRUE, 
            suggestion = NA,
            citation = citation, 
            processed = TRUE)
    }
    if (pbar) 
        close(pb)
    if (!all(unlist(lapply(out, `[[`, "processed")))) warning("Some entries were not processed at this time. Please try again later.")
    invisible(out)
}
