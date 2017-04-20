#N samples of size n from population vector, pop, stored in Nxn matrix samples      
samples<-function(pop, n, N){
       samples<-matrix(c(rep(0, n*N)), nrow=N, ncol=n)
       for (i in 1:N) for (j in 1:n) {samples[i,j]<-sample(pop,1)}
       return(samples)}
        
#sample means for samples in rows of matrix "samples" 
sampmeans<-function(samples){
        means<-rep(0, length(samples[, 1]))
        for (i in 1:length(samples[,1])){
            means[i]<-mean(samples[i,])}
            return(means)}
        
     
           
samphist<-function(pop, n, N){
        means<-sampmeans(samples(pop, n, N))
        m<-mean(means)
        s<-sd(means)
        h<-hist(means, breaks=min(30, N/10))
        x<-seq(min(means), max(means), 0.01)
        y<-dnorm(x, m, s)*diff(h$mids[1:2])*length(means)
        M<-1.1*max(h$counts)
        hist(means, xlab='Sample Means', main=paste('Distribution of Sample Means, n=',n), ylim=c(0, M),ylab='Density' , breaks=min(30, N/10), axes=F)
        axis(1, at=h$breaks, labels=h$breaks)
        lines(x, y, lwd=2, col='red')
        }
        
