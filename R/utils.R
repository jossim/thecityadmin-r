require(plyr)
# Returns the items in y that are not also in x.
diffy <- function(x, y) {
    diff = NULL
    
    for(i in y) if(!i %in% x) diff = c(diff, i)
    
    return(diff)
}

# Binds colunms that are named with the names in the names vector to the passed
# in data frame with values provided by default.
multi.cbind <- function(df, names, default = NA) {
    if(length(names) > 0) {
        new.df = data.frame()
        for(i in 1:length(names)){
            if(ncol(new.df) < 1)
                new.df = data.frame(default)
            else
                new.df = cbind(new.df, default)
        }
        colnames(new.df) = names
        df = cbind(df, new.df)
    }
    return(df)
}

# Takes in a data frame & a list of items to be added, row wise, to the data 
# frame. If the list has different column names than the data frame, then the
# combind data frame will include all the named columns.
build.frame <- function(df, items, index = 1) {    
    for(i in index:length(items)) {
        tmp = data.frame(matrix(unlist(items[[i]]), 
                                nrow = 1, 
                                ncol = length(unlist(items[[i]])))
        )
        
        colnames(tmp) = names(items[[i]])
        tmp = multi.cbind(tmp, diffy(names(tmp), names(df)))
        df = multi.cbind(df, diffy(names(df), names(tmp)))
        df = rbind.fill(df, tmp)
    }
    return(df)
}

replace.empty <- function(i, rep = NA) {
    if(is.null(i)) {
        return(rep)
    }
    else if (length(i) == 0) {
        return(rep)
    }
    else if(class(i) == "character" && nchar(i) == 0) {
        return(rep)
    }
    else {
        rep = i
    } 

    return(i)
}

flatten.list <- function(lst) {
    new.list = list()
    lst.names = names(lst)
    lst = lapply(lst, function(x) replace.empty(x))
    
    for(i in 1:length(lst)) {
        #print(class(lst[[i]]))
        if(class(lst[[i]]) == "list") {
            #browser()
            names(lst[[i]]) = paste(lst.names[i], names(lst[[i]]), sep = ".")
            new.list = append(new.list, flatten.list(lst[[i]]))
        }
        else {
            new.list[[lst.names[i]]] = lst[[i]]
        }
    }
    return(new.list)
}
