CalcLmt <-
function(size, pct, alpha=0.05){
    n <- length(size)
    low <- rep(NA, n)
    upp <- rep(NA, n)
    for (i in 1:n) {
        k <- floor(size[i]*pct)
        pval <- pbinom(k, size=size[i], prob=pct, lower.tail=TRUE)
        while (pval>=alpha & k>0) {
            k <- k-1
            pval <- pbinom(k, size=size[i], prob=pct, lower.tail=TRUE)
        }
        if (pval<alpha) {low[i] <- k+1}
          else {low[i] <- 0}
        k <- ceiling(size[i]*pct)
        pval <- pbinom(k, size=size[i], prob=pct, lower.tail=FALSE)
        while (pval>=alpha & k<size[i]) {
            k <- k+1
            pval <- pbinom(k, size=size[i], prob=pct, lower.tail=FALSE)
        }
        if (pval<alpha) {upp[i] <- k}
        else {upp[i] <- size[i]}
    }
    lowpct <- low/size
    upppct <- upp/size
    if (pct>0.5) {
        outlow <- 1-upppct
        outupp <- 1-lowpct
        out <- data.frame(size=size, lower=low, upper=upp, outlow=outlow, outupp=outupp)
    }
      else {out <- data.frame(size=size, lower=low, upper=upp, lowpct=lowpct, upppct=upppct)}
    out
}
fig1 <-
function(){
    #windows(width=10, height=8)
    plot(T1crit$size, T1crit$size-T1crit$upper, xlim=c(0, 500), ylim=c(0, 35),
         xlab="Dataset Size", ylab="# Above Cut Point",
         type="l", col="blue")
    lines(T1crit$size, T1crit$size-T1crit$lower, lty=1, col="blue")

    lines(T2crit$size, T2crit$size-T2crit$upper, lty=1, col="red")
    lines(T2crit$size, T2crit$size-T2crit$lower, lty=1, col="red")  

    legend(1, 35, lty=c(1,1), col=c("blue","red"), 
            legend=c("Upper and Lower Bounds for Tier 1 Cut Points","Upper and Lower Bounds for Tier 2 Cut Points"))
}
fig2 <-
function(){
    #windows(width=10, height=8)
    plot(T1crit$size, T1crit$outupp*100, xlim=c(0, 500), ylim=c(0, 15),
         xlab="Dataset Size", ylab="% Above Cut Point",
         type="l", col="blue")
    lines(T1crit$size, T1crit$outlow*100, lty=1, col="blue")
    abline(h=5, lty=2, col="blue")
    lines(T2crit$size, T2crit$outupp*100, lty=1, col="red")
    lines(T2crit$size, T2crit$outlow*100, lty=1, col="red")  
    abline(h=1, lty=2, col="red")
    legend(200, 15, lty=c(1,1), col=c("blue","red"), 
            legend=c("Upper and Lower Bounds for Tier 1 Cut Points","Upper and Lower Bounds for Tier 2 Cut Points"))
}
