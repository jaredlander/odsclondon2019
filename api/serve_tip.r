simple_mod <- readr::read_rds('artifacts/simple_mod.rds')

#' Return a predicted score
#' @param bill
#' @get /tip
function(bill)
{
    predict(simple_mod, newdata=data.frame(total_bill=as.numeric(bill)))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
    myData <- iris
    title <- "All Species"
    
    # Filter if the species was specified
    if (!missing(spec)){
        title <- paste0("Only the '", spec, "' Species")
        myData <- subset(iris, Species == spec)
    }
    
    plot(myData$Sepal.Length, myData$Petal.Length,
         main=title, xlab="Sepal Length", ylab="Petal Length")
}
