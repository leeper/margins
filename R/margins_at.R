margins_at <-
function(x, 
         at = NULL,
         atmeans = FALSE,
         ...){

    # NEED TO SETUP THE BELOW TO WORK WITH MULTIPLE `at` VALUES
    # set `at`
    mm <- as.data.frame(model.matrix(x)) # data
    if(atmeans) {
        # replace data with means of data
        tmp <- as.list(colMeans(mm))
        # insert `at` values
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            # expand possible combinations of `at` values
            tmp <- expand.grid(append(tmp, at))
        }
        # replace `mm` with updated data
        mm <- as.data.frame(tmp)
        rm(tmp)
    } else {
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            # overwrite `at` values
            mm[ , names(at)] <- at
        }
    }
    
    # method dispatch
    margins(x, newdata = mm, ...)
}
