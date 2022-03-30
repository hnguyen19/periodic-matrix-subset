library(popbio)


mim <- subset(monkeyflower, species == "cardinalis" &
                site == "Carlon" & year != "pooled", select = c(4:19))
## convert data frame to list of matrices using split
mim1 <-split(mim, 2000:2002) # Projection matrix from 3 years
mim2 <-lapply(mim1, matrix, nrow=4, byrow=TRUE)
vr1 <- pfister.plot(mim2)
vr1
## PLOT using labels
plot(vr1$cv, vr1$elas, xlab="CV", ylab="Elasticity", log="xy", type='n')
# Split matrix elements into transitions representing F (fertility),
# S (survival), G (growth), and R (retrogression).
# Fertility on top row, survival on diagonal, growth is above diagonal
# and retrogression below diagonal.
rownames(vr1)
y2 <- expression(S[11],G[21],G[31],G[41],
                 F[12],S[22],G[32],G[42],
                 F[13],R[23],S[33],G[43],
                 F[14],R[34],S[44])
text(vr1$cv, vr1$elas, y2)
### add trend line
abline(lm(log10(vr1$elas)~log10(vr1$cv)), col="red")
## include Spearman's rank correlation
a <- cor.test(vr1$cv, vr1$elas, method="spearman")
a
text(10, .0015, substitute(rho == x, list(x=round(a$estimate,2))), col="blue")


How to organize Marsden population projection: 
  Solution 1: realize multiple scenarios to get multiple projection matrices --> does not make practical sense because the goal is to find ways to decrease population growth.  
  Solution 2: realize one projection matrix per block --> use arithmetic means, not emmeans, disregard response variable distribution
  Solution 3: per each list of em means, a list of se is available (all empirically measured vitals and some literature-based lists), how to incorporate these to calculate lambda var? 
    

  # code from https://pdixon.stat.iastate.edu/stat534/R/matrix.r
  v <- matrix(0, nrow=9, ncol=9)
  
  v
  
  ## where did the values in v come from?
  v[2,2] <- 0.0036
  
  # 6th row/col is a32
  v[6,6] <- 0.0032
  
  # 4th row/col is a12 = fecundity
  v[4,4] <- 0.27
  
  # 7th row/col is a13, also = fecundity
  v[7,7] <- 0.27
  
  # the two fecundities have correl=1, so cov = 0.27
  # that's a correlation between the 4'th and 7'th values
  v[7,4] <- v[4,7] <- 0.27
  
  
  a <- rbind(c(0, 2.6, 2.6), c(0.2, 0, 0), c(0, 0.57, 0))
  
  ev <- eigen(a)
  
  Re(ev$vectors[,1])
  v <- Re(ev$vectors[,1])
  v/sum(v)

