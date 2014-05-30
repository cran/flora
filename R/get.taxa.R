#' Get plant taxonomical and distribution data
#' 
#' This function collects taxonomic information and distribution from the
#' Brazilian Flora Checklist. Synonyms and misspelled names are resolved 
#' automatically. Results can be combined with life form, habitat, vernacular
#' name, and occurrence data.
#' 
#' @param taxa a character vector containing one or more taxa, without authors 
#'   see \code{\link{remove.authors}} if you have a list with authorities
#' @param replace.synonyms should the function automatically replace synonyms?
#' @param life.form include the life form of the taxon?
#' @param habitat include the habitat of the taxon?
#' @param vernacular include vernacular names and localities?
#' @param states include occurrence data?
#' @param establishment include the establishment type (native, cultivated or 
#'   naturalized)?
#' @param drop character vector with names of columns with taxonomic
#'   information to be removed from the returned data frame. Available names: 
#'   "id", "scientific.name", "accepted.name", "family", "genus",
#'   "specific.epiteth", "infra.epiteth", "taxon.rank", "authorship",
#'   "taxon.status", "name.status", and "search.str".
#' @details The returned data frame will contain a variable number of rows and 
#'   columns depending on how the function was called. For instance, since there
#'   might be more than one vernacular name for each taxon, some rows
#'   will be duplicated if \code{vernacular} is set to \code{TRUE}. All misspelled taxa
#'   are automatically corrected if the function can come up with a reasonable
#'   guess for the name.
#' @return a data frame
#' @export
#' @examples 
#' data(plants)
#' get.taxa(plants)
#' get.taxa(plants, life.form = TRUE, establishment = TRUE)
get.taxa <-
  function(taxa, 
           replace.synonyms = TRUE,  
           life.form = FALSE, 
           habitat = FALSE, 
           vernacular = FALSE, 
           states = FALSE, 
           establishment = FALSE,
           drop = c("authorship", 
                    "genus", "specific.epiteth", 
                    "infra.epiteth", "name.status"))  {
    original.search <- taxa
    obs <- rep(NA, length(taxa))
    taxa <- trim(taxa)
    found <- is.element(taxa, all.taxa$search.str)
    if (any(!found)) {
      for (i in which(!found)) {
        taxon <- taxa[i]
        uncertain <- regmatches(taxon, regexpr("[a|c]f+\\.", taxon))
        if (length(uncertain) != 0L) taxon <- gsub("[a|c]f+\\.", "", taxon)
        ident <- regmatches(taxon, regexpr("\\s+sp\\.+\\w*", taxon))
        if (length(ident) != 0L) taxon <- unlist(strsplit(taxon, " "))[1]
        if (is.element(taxon, all.taxa$search.str)) {
          taxa[i] <- taxon
          found[i] <- TRUE
        } else {
          suggestion <- suggest.names(taxon)
          if (!is.na(suggestion)) {
            taxa[i] <- suggestion
            found[i] <- TRUE
            obs[i] <- "was misspelled"
          } else {
            obs[i] <- "not found"
          }
        }
      }
    }
    res <- data.frame(matrix(NA, ncol = ncol(all.taxa), nrow = length(taxa)))
    names(res)  <- names(all.taxa)
    res[found, ] <- all.taxa[match(taxa[found], all.taxa$search.str), ]
    if (replace.synonyms) {
      synonyms <- res[!is.na(res$taxon.status) & res$taxon.status == "synonym", ]
      rel <- relationships[relationships$related.id %in% synonyms$id, ]
      accepted <- merge(all.taxa, rel[, c("id", "related.id")], by = "id", sort = FALSE)
      accepted <- accepted[!is.na(accepted$taxon.status) & accepted$taxon.status == "accepted", ]
      accepted <- accepted[!duplicated(accepted$related.id), ]
      accepted <- accepted[order(match(accepted$related.id, res$id)), ]
      #corrected <- which(res$id %in% accepted$related.id)
      corrected <- match(accepted$related.id, res$id)
      not.corrected <- which(res$id %in% synonyms$id)
      not.corrected <- not.corrected[which(!not.corrected %in% corrected)]
      #return(list(a = res[corrected, ]$id, b = accepted$related.id))
      res[corrected, ] <- accepted[, -13]
      previous.obs.corrected <- which(!is.na(obs[corrected]))
      empty.obs.corrected <- which(is.na(obs[corrected]))
      previous.obs.not <- which(!is.na(obs[not.corrected]))
      empty.obs.not <- which(is.na(obs[not.corrected]))
      obs[corrected][previous.obs.corrected] <- paste(obs[corrected][previous.obs.corrected], "replaced synonym", sep = "|")
      obs[corrected][empty.obs.corrected] <- "replaced synonym"
      obs[not.corrected][previous.obs.not] <- paste(obs[not.corrected][previous.obs.not], "not replaced synonym", sep = "|")
      obs[not.corrected][empty.obs.not] <- "not replaced synonym"
    }
    if (is.null(drop)) {
      res <- data.frame(res, original.search, obs, stringsAsFactors = FALSE)
    } else {
      res <- data.frame(res[, !names(res) %in% drop], original.search, obs, stringsAsFactors = FALSE)
    }
    if (life.form) {
      res <- left_join(res, species.profiles[, c("id", "life.form")], by = "id")
    }
    if (habitat) {
      res <- left_join(res, species.profiles[, c("id", "habitat")], by = "id")
    }
    if (vernacular) {
      res <- left_join(res, vernacular.names[, c("id", "vernacular.name", "locality")], by = "id")
    }
    if (states) {
      res <- left_join(res, distribution[,c("id", "occurrence")], by = "id")
    }
    if (establishment) {
      res <- left_join(res, distribution[,c("id", "establishment")], by = "id")
    }
    res
  }