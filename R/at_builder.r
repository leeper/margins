at_builder <- 
function(data,
         terms,
         at = NULL, 
         atmeans = FALSE, # get rid of `atmeans` argument and pass arguments to aggregate
         ...){
    if(!is.null(at) && length(at) > 0) {
        if(any(!names(at) %in% names(data)))
            stop("Unrecognized variable name in 'at'")
        e <- expand.grid(at)
        e <- split(e, unique(e))
        data_out <- lapply(e, function(z) {
            # build new data for predict
            dat <- data
            dat <- `[<-`(dat, , names(z), value = z)
            # build new model.matrix
            out <- as.data.frame(model.matrix(object = terms, data = dat)) # this will fail with factors
            out <- `[<-`(out, , names(z), value = z)
            if(atmeans) {
                for(i in names(dat)[!names(dat) %in% names(at)]){
                    out[,i] <- mean(out[,i])
                } # haven't check this
                for(i in names(out)[!names(out) %in% names(at)]){
                    dat[,i] <- mean(dat[,i])
                }
            }
            structure(list(data = dat, mm = out), Variables = unlist(z))
            
        })
        return(setNames(data_out, names(e)))
    } else {
        out <- model.matrix(object = terms, data = data)
        if(atmeans) {
            dat <- aggregate(. ~ 1, data = data, FUN = mean) # can use different funcs and args here (`median`, `quantile`, etc.)
            out <- aggregate(. ~ 1, data = out, FUN = mean)
        } else {
            dat <- data
            out <- as.data.frame(out)
        }
        data_out <- list(data = dat, mm = out)
        return(structure(list(`1` = data_out), Variables = NULL))
    }
}
