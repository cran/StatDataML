readDescriptionSDML <- function(x)
{
    title <- xmlValue(x[["title"]][[1]])
    source <- xmlValue(x[["source"]][[1]])
    date <- xmlValue(x[["date"]][[1]])
    version <- xmlValue(x[["version"]][[1]])
    comment <- xmlValue(x[["comment"]][[1]])
    creator <- xmlValue(x[["creator"]][[1]])
    class <- xmlValue(x[["class"]][[1]])
    ret <- list(title=title, source=source, date=date, version=version,
                comment=comment, creator=creator, SDMLclass=class)
    ret
}
