readDescriptionSDML <- function(x)
{
    title <- x$children[["title"]]$children$text$value
    source <- x$children[["source"]]$children$text$value
    date <- x$children[["date"]]$children$text$value
    version <- x$children[["version"]]$children$text$value
    comment <- x$children[["comment"]]$children$text$value
    creator <- x$children[["creator"]]$children$text$value
    class <- x$children[["class"]]$children$text$value
    ret <- list(title=title, source=source, date=date, version=version,
                comment=comment, creator=creator, SDMLclass=class)
    ret
}
