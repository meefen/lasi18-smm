
analyze_text_with_watsonNLU <- function(text, username_NLU, password_NLU) {
  
  url_NLU = "https://gateway.watsonplatform.net/natural-language-understanding/api"
  version_NLU = "?version=2017-02-27"
  
  text <- URLencode(text)
  
  POST(url=paste(
    url_NLU,
    "/v1/analyze",
    version_NLU,
    "&text=", text,
    "&features=keywords,entities",
    "&entities.emotion=true",
    "&entities.sentiment=true",
    "&keywords.emotion=true",
    "&keywords.sentiment=true",
    sep=""),
    authenticate(username_NLU, password_NLU),
    add_headers("Content-Type"="application/json")
  )
}