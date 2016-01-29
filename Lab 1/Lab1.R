max.function <- function(a){
    return (function(x){a - x})
}
func = max.function(.5)
curve (func, xlim=c(0,1), main='Area Curve')