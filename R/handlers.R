handlersSDML <- function() {
    list(
         textdata = function(x, ...){

             sep <- ifelse("sep" %in% names(xmlAttrs(x)),
                           xmlAttrs(x)["sep"], " \n")
             sep <- paste("[", sep, "]+", sep="")

             mode <- ifelse("mode" %in% names(xmlAttrs(x)),
                           xmlAttrs(x)["mode"], "")

             list(value=unlist(strsplit(xmlValue(x[[1]]), sep)),
                  mode=mode)
         })
}

