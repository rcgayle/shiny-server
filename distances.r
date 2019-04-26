#L^p distance between P and Q in R^n


LPDist<-function(P, Q, p){
           if (length(P)!=length(Q)){
               return('P and Q have differing lengths.')}
           else {
              n<-length(P)
              S<-rep(0,n)
              for (i in 1:n){
                 P[i]<-as.numeric(P[i])
                 Q[i]<-as.numeric(Q[i])
                 S[i]<-(abs(P[i]-Q[i]))^p
                            }
                 }
              return((sum(S)^(1/p)))
                     }
                     
#Uniform distance between P and Q in R^n

UDist<-function(P, Q){
           if (length(P)!=length(Q)){
               return('P and Q have differing lengths.')}
           else {
              n<-length(P)
              S<-rep(0,n)
              for (i in 1:n){
                 P[i]<-as.numeric(P[i])
                 Q[i]<-as.numeric(Q[i])
                 S[i]<-abs(P[i]-Q[i])
                            }
                 }
              return(max(S))
                     }

#Euclidean distance between P and Q in R^n
                     
EDist<-function(P, Q){
            if (length(P)!=length(Q)){
               return('P and Q have differing lengths.')}
            else{
              return(LPDist(P, Q, 2))
                     }}
                     
#L^1 distance between P and Q in R^n
                     
L1Dist<-function(P, Q){
            if (length(P)!=length(Q)){
               return('P and Q have differing lengths.')}
            else{
              return(LPDist(P, Q, 1))
                     }
                     }
                     
LPNorm<-function(P,p){
          O<-rep(0, length(P))
          return(LPDist(P, O,p))
                     }
                     
ENorm<-function(P){
          return(LPNorm(P,2))
                  }
                  
L1Norm<-function(P){
          return(LPNorm(P,1))
                  }
                  
UNorm<-function(P){
          O<-rep(0, length(P))
          return(UDist(P,O))
                   }