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
            if(atmeans) {
                for(i in names(data)[!names(data) %in% names(at)]){
                    if (is.numeric(data[,i])) {
                        data[,i] <- mean(data[,i], na.rm=TRUE)
                    }
                }
            }
            mf1 <- model.frame(formula(terms), data = data)
            data <- `[<-`(data, , names(z), value = z)
            mf2 <- model.frame(formula(terms), data = data)
            for (nm in names(mf2)) {
                if (is.factor(mf2[[nm]])) {
                    mf2[[nm]] <- factor(mf2[[nm]], levels=levels(mf1[[nm]]))
                }
            }
            out <- as.data.frame(model.matrix(object = terms, data = mf2))
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
