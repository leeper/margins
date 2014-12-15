at_builder <- 
function(data,
         terms,
         at = NULL, 
         atmeans = FALSE, 
         ...){
    if(!is.null(at) && length(at) > 0) {
        if(any(!names(at) %in% names(data)))
            stop("Unrecognized variable name in 'at'")
        e <- expand.grid(at)
        e <- split(e, unique(e))
        data_out <- lapply(e, function(z) {
            out <- as.data.frame(model.matrix(object = terms, data = data))
            out <- `[<-`(out, , names(z), value = z)
            if(atmeans) {
                for(i in names(out)[!names(out) %in% names(at)]){
                    out[,i] <- mean(out[,i])
                }
            }
            out
        })
    } else {
        data <- model.matrix(object = terms, data = data)
        if(atmeans) {
            data_out <- list(as.data.frame(t(colMeans(data))))
        } else {
            data_out <- list(as.data.frame(data))
        }
    }
    data_out
}
