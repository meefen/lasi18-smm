
collect_annotations <- function(tag = '', token = '') {
  
  EnsurePackage("httr")
  EnsurePackage("jsonlite")
  
  token <- paste("Bearer", token)
  
  # Search by tag
  annotations <- NULL
  batch = 200; i = 0
  repeat{
    url_search = paste0("https://hypothes.is/api/search?limit=", batch,
                        "&offset=", i, "&tag=", tag)
    req <- httr::GET(url_search,
                     add_headers("Authorization" = token))
    json <- httr::content(req, as = "text")
    tmp <- fromJSON(json, simplifyDataFrame = TRUE, flatten = TRUE)$rows
    if(is.null(nrow(tmp))) {
      break
    }
    annotations <- rbind(annotations, tmp)
    i <- i + batch
  }
  
  return(annotations)
}