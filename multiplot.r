
Eplot<-function(){
    x<-c()
    y<-c()
    plot(x,y, xlim=c(-10,10), ylim=c(-10,10), xlab='', ylab='',axes=F)
    text(0,1, "Upload Data and/or", cex=1.5, col='blue', font=2)
    text(0,-1, "Select Columns", cex=1.5, col='blue', font=2)
                     }
                     
ERRplot<-function(){
    x<-c()
    y<-c()
    plot(x,y, xlim=c(-10,10), ylim=c(-10,10), xlab='', ylab='',axes=F)
    text(0,0, "The variables differ in length.", cex=1.5, col='red', font=2)
                     } 


                          
nicetable<-function(G, H, xname, yname){
         x<-c()
         y<-c()
         T<-table(G,H)
         n<-length(levels(G))
         m<-length(levels(H))
         plot(x, y, axes=F, xlim=c(-1, m+1), ylim=c(-1, n+2),
                xlab='', ylab='')
         VTP<-seq(1,m,1)
         HTP<-seq(n,1, -1)
         VLP<-seq(0.5, m+0.5, 1)
         HLP<-seq(0.5, n+0.5, 1)
         for (i in 1:length(VLP)){
              segments(VLP[i], 0.5, VLP[i], n+0.5, lwd=2)
                           }
         for (j in 1:length(HLP)){
              segments(0.5, HLP[j], m+0.5, HLP[j], lwd=2)
                           }
         for (k in 1:m){
               for (l in 1:n){
                  text(VTP[k], HTP[l], paste0(T[l,k]), 
                            col='purple',font=2, cex=1.5)
                             }
                        }
        for (J in 1:m){
             text(VTP[J], -0.1, levels(H)[J], col='red', font=2, cex=1.5)
                      }
        for (K in 1:n){
             text(-0.05, HTP[K], levels(G)[K], col='blue', font=2, cex=1.5)
                      }
        text((m+1)/2, n+1.5, 
             paste0('Contigency Table for ',xname,' and ',yname), 
              col='darkgreen', font=2,
                        cex=1.75)
        text((m+1)/2-(m+1)/10.75, n+1, expression(chi^2),
                  col='darkgreen', font=2,cex=1.25
            )
        text((m+1)/2+(m+1)/15, n+0.91, 
            paste0('=',round(chisq.test(T)[[1]],2),'; p=',
                       round(chisq.test(T)[[3]],2)),
             col='darkgreen', font=2,
                        cex=1.25
            )  
        text(-0.75, (n+1.5)/2, xname, col='blue', font=2, cex=1.75,srt=90) 
        text((m+1)/2, -0.75, yname, col='red', font=2, cex=1.75)              
                             }
                             
nicetable2<-function(G, H, xname, yname, I){
         x<-c()
         y<-c()
         T<-table(G,H)
         n<-length(levels(G))
         m<-length(levels(H))
         ypts<-rep(1:n, m)
          xpts<-c()
          for (j in 1:m){
              xpts<-c(xpts, rep(j,n))
                        }
         plot(x, y, axes=F, xlim=c(-1, m+1), ylim=c(-1, n+2),
                xlab='', ylab='')
         VTP<-seq(1,m,1)
         HTP<-seq(n,1, -1)
         VLP<-seq(0.5, m+0.5, 1)
         HLP<-seq(0.5, n+0.5, 1)
         for (i in 1:length(VLP)){
              segments(VLP[i], 0.5, VLP[i], n+0.5, lwd=2)
                           }
         for (j in 1:length(HLP)){
              segments(0.5, HLP[j], m+0.5, HLP[j], lwd=2)
                           }
         for (k in 1:m){
               for (l in 1:n){
                  text(VTP[k], HTP[l], paste0(T[l,k]), 
                            col='purple',font=2, cex=1.5)
                             }
                        }
        for (i2 in 1:length(I)){
             text(xpts[I[i2]], ypts[I[i2]], 
                   paste0(T[n+1-ypts[I[i2]],xpts[I[i2]]]), 
                   col='green', font=2, cex=1.5)
        for (J in 1:m){
             text(VTP[J], -0.1, levels(H)[J], col='red', font=2, cex=1.5)
                      }
        for (K in 1:n){
             text(-0.05, HTP[K], levels(G)[K], col='blue', font=2, cex=1.5)
                      }
        text((m+1)/2, n+1.5, 
             paste0('Contigency Table for ',xname,' and ',yname), 
                      col='darkgreen', font=2,
                        cex=1.75)
        text((m+1)/2-(m+1)/10.75, n+1, expression(chi^2),
                  col='darkgreen', font=2,cex=1.25
            )
        text((m+1)/2+(m+1)/15, n+0.91, 
            paste0('=',round(chisq.test(T)[[1]],2),'; p=',
                       round(chisq.test(T)[[3]],2)),
             col='darkgreen', font=2,
                        cex=1.25
            )
        text(-0.75, (n+1.5)/2, xname, col='blue', font=2, cex=1.75, srt=90) 
        text((m+1)/2,-0.75,yname, col='red', font=2, cex=1.75)              
                             }
                             
                             }
                           

regline<-function(x,y){
           m_x<-mean(x)
           m_y<-mean(y)
           sd_x<-sd(x)
           sd_y<-sd(y)
           r<-cor(x,y)
           return(function(t){r*(sd_y/sd_x)*(t-m_x)+m_y})
                       }              
                                
         
ClkNearPt<-function(Q, x, y){
             I<-1
             D<-EDist(c(x[1], y[1]), c(Q[[1]], Q[[2]]))
             for (i in 2:length(x)){  
                   if (EDist(c(x[i], y[i]), c(Q[[1]], Q[[2]]))<D)
                         {I<-i
                          D<-EDist(c(x[i], y[i]), c(Q[[1]], Q[[2]]))
                          }
                    else {}
                                    }
            return(I)
                              }
                              
ClkNearPt2<-function(Q, x, y){
             I<-1
             D<-UDist(c(x[1], y[1]), c(Q[[1]], Q[[2]]))
             for (i in 2:length(x)){  
                   if (UDist(c(x[i], y[i]), c(Q[[1]], Q[[2]]))<D)
                         {I<-i
                          D<-UDist(c(x[i], y[i]), c(Q[[1]], Q[[2]]))
                          }
                    else {}
                                    }
            return(I)
                              }
                              

                                
FindColInd<-function(ColName, D){
           I<-0
           for (i in 1:length(names(D))){
                 if(ColName==names(D)[i])
                       {I<-i}
                  else {} 
                                         }                            
           return(I)
                                }
  
SubFr1<-function(D, G, GI, H, HI, PI){
     n<-length(levels(G))
     m<-length(levels(H))
     yt<-rep(1:n, m)
     xt<-c()
        for (j in 1:m){
        xt<-c(xt, rep(j,n))
                      }
    It<-n+1-yt[PI]
    Jt<-xt[PI]
    return(subset(D,
         D[, GI]==levels(G)[It] & D[, HI]==levels(H)[Jt]))
                                     }


MultiPlot<-function(x, y, xname, yname){
    if (is.numeric(x) & is.numeric(y))
        {plot(x, y, pch=19, col='red', xlab=xname, ylab=yname)
        title(main = list(
        paste0('Scatterplot with Regression, y on x, r= ',round(cor(x,y),2)),
                cex = 1.5, col = "darkred", font = 2))
        t<-seq(min(x), max(x), 0.01) 
          z<-regline(x,y)(t)
          lines(t,z , col='magenta', lwd=2)
             }
        else if (is.factor(x) & is.numeric(y))
             {K<-length(levels(x))
              xnum<-rep(0, length(x))
              for (i in 1:length(x)){
                  for (j in 1:K){
                       if (x[i]==levels(x)[j])
                          {xnum[i]<-j}
                       else {}
                                 }
                           }
              R<-(max(y)-min(y))/5
              plot(xnum, y, pch=19, col='red', axes=F,xlab=xname,ylab=yname,
                 xlim=c(0, K+1), ylim=c(min(y)-R, max(y)+R))
              ticks1<-seq(0, K+1,1)
              labs1<-c('',levels(x),'')
              ticks2<-seq(min(y)-R, max(y)+R, R)
              labs2<-c('',round(ticks2[2:7], 1),'')
              axis(1, at=ticks1, labels=labs1, pos=min(y)-R)
              axis(2, at=ticks2, labels=labs2, pos=0)
                 }
        else if (is.numeric(x) & is.factor(y))
             {K<-length(levels(y))
              ynum<-rep(0, length(y))
              for (i in 1:length(y)){
                  for (j in 1:K){
                       if (y[i]==levels(y)[j])
                          {ynum[i]<-j}
                       else {}
                                 }
                                    }
              R<-(max(x)-min(x))/5
              plot(x, ynum, pch=19, col='red', axes=F, xlab=xname,ylab=yname,
                 ylim=c(0, K+1), xlim=c(min(x)-R, max(x)+R))
              ticks2<-seq(0, K+1,1)
              labs2<-c('',levels(y),'')
              ticks1<-seq(min(x)-R, max(x)+R, R)
              labs1<-c('', round(ticks1[2:7], 1), '')
              axis(1, at=ticks1, labels=labs1, pos=0)
              axis(2, at=ticks2, labels=labs2, pos=min(x)-R)
                 }
        else if (is.factor(x) & is.factor(y))
              {nicetable(x,y, xname, yname)}
        else {}
        if (is.numeric(x) | is.numeric(y))
             {return('N')}
        else {return('C')}
                          }

MultiPlotPts<-function(x, y, xname, yname, I){
    if (is.numeric(x) & is.numeric(y))
        {plot(x, y, pch=19, col='red', xlab=xname, ylab=yname)
        title(main = list(
        paste0('Scatterplot with Regression, y on x, r= ',round(cor(x,y),2)),
                cex = 1.5, col = "darkred", font = 2))
        t<-seq(min(x), max(x), 0.01) 
          z<-regline(x,y)(t)
          lines(t,z , col='magenta', lwd=2)
        for (i in I){
                 points(x[i], y[i], pch=19, col='blue')
                          }
             }
        else if (is.factor(x) & is.numeric(y))
             {K<-length(levels(x))
              xnum<-rep(0, length(x))
              for (i in 1:length(x)){
                  for (j in 1:K){
                       if (x[i]==levels(x)[j])
                          {xnum[i]<-j}
                       else {}
                                 }
                           }
              R<-(max(y)-min(y))/5
              plot(xnum, y, pch=19, col='red', axes=F,xlab=xname,ylab=yname,
                 xlim=c(0, K+1), ylim=c(min(y)-R, max(y)+R))
              ticks1<-seq(0, K+1,1)
              labs1<-c('',levels(x),'')
              ticks2<-seq(min(y)-R, max(y)+R, R)
              labs2<-c('',round(ticks2[2:7], 1),'')
              axis(1, at=ticks1, labels=labs1, pos=min(y)-R)
              axis(2, at=ticks2, labels=labs2, pos=0)
              for (i in I){
                 points(xnum[i], y[i], pch=19, col='blue')
                          }
                 }
        else if (is.numeric(x) & is.factor(y))
             {K<-length(levels(y))
              ynum<-rep(0, length(y))
              for (i in 1:length(y)){
                  for (j in 1:K){
                       if (y[i]==levels(y)[j])
                          {ynum[i]<-j}
                       else {}
                                 }
                                    }
              R<-(max(x)-min(x))/5
              plot(x, ynum, pch=19, col='red', axes=F,xlab=xname,ylab=yname,
                 ylim=c(0, K+1), xlim=c(min(x)-R, max(x)+R))
              ticks2<-seq(0, K+1,1)
              labs2<-c('',levels(y),'')
              ticks1<-seq(min(x)-R, max(x)+R, R)
              labs1<-c('', round(ticks1[2:7], 1), '')
              axis(1, at=ticks1, labels=labs1, pos=0)
              axis(2, at=ticks2, labels=labs2, pos=min(x)-R)
              for (i in I){
                 points(x[i], ynum[i], pch=19, col='blue')
                          }
                 }
        else if (is.factor(x) & is.factor(y))
              {nicetable2(x,y, xname, yname, I)}
        else {}
                          }





               