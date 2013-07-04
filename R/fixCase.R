fixCase <-
function(x) {
    s <- paste(tolower(strsplit(x, " ")[[1]]), collapse = " ")
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
