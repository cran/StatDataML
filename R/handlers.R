handlersSDML <-
    list(
         textdata = function(x, ...){

             sep <- ifelse("sep" %in% names(x$attributes),
                           x$attributes["sep"], " \n")
             sep <- paste("[", sep, "]\\+", sep="")

             mode <- ifelse("mode" %in% names(x$attributes),
                           x$attributes["mode"], "")

             list(value=unlist(strsplit(x$children$text$value, sep)),
                  mode=mode)
         })
