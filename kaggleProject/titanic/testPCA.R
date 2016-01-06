library(HSAUR)
# look at heptathlon data
heptathlon

# correlations
round(cor(heptathlon[,-8]),2)   # ignoring "score" 
# covariance
round(cov(heptathlon[,-8]),2)

# PCA
# scale=T bases the PCA on the correlation matrix
hep.PC.cor = prcomp(heptathlon[,-8], scale=TRUE)
hep.PC.cov = prcomp(heptathlon[,-8], scale=FALSE)

# PC scores per competitor
hep.scores.cor = predict(hep.PC.cor)
hep.scores.cov = predict(hep.PC.cov)

# Plot of PC1 vs PC2
par(mfrow = c(2, 1))
plot(hep.scores.cov[,1],hep.scores.cov[,2],
     xlab="PC 1",ylab="PC 2", pch=NA, main="Covariance")
text(hep.scores.cov[,1],hep.scores.cov[,2],labels=1:25) 

plot(hep.scores.cor[,1],hep.scores.cor[,2],
     xlab="PC 1",ylab="PC 2", pch=NA, main="Correlation")
text(hep.scores.cor[,1],hep.scores.cor[,2],labels=1:25) 