readDescriptionSDML <- function(x)
{
  if(is.null(x)) return(NULL)

  browser()
  
  list(
       title      = xmlValue(x[["title"]]),
       source     = xmlValue(x[["source"]]),
       date       = xmlValue(x[["date"]]),
       version    = xmlValue(x[["version"]]),
       comment    = xmlValue(x[["comment"]]),
       creator    = xmlValue(x[["creator"]]),
       SDMLclass  = xmlValue(x[["class"]]),
       properties = readProperties(x[["properties"]])
       )
}
