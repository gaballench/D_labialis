# iterative cor.test function
# Formals are x, y, data, and ...
# `x` is a vector containing either indexes or names for the independent variable in the correlation
# `y` is a vector containing either indexes or names for the dependent vcariables in the correlation
# `data` is the dataset where x and y are located. If data are not in either matrix or data frame convert the to such structure before using the function.
# ... are further arguments to be passed for the cor.test function. See ?cor.test for further information

iter.cor.test <- function(x, y, data, ...) {
    if((class(x) == "numeric" | class(y) == "numeric") & (typeof(x) == "double" | typeof(y) == "double")) {
        warning("Either `x` or `y` are numeric but not integers. Coercion to integer will take only the entire part of the indexes (i.e., 1.6 becomes 1)")
    }
    output <- list()
    length(output) <- length(x) * length(y)
    iter <- 0
    for(i in seq_along(x)) {
        for(j in seq_along(y)) {
            iter <- iter + 1
            output[[iter]] <- cor.test(x = data[, x[i]], y = data[, y[j]], ...)
            names(output)[iter] <- paste(as.character(x[i]), "vs", as.character(y[j]), sep = "_")
        }
    }
    return(output)
}
