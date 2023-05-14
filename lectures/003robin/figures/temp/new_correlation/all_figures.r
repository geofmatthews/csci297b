######## CORRELATION PLOT (LAKE DATA; ALK/PH)
lakes <- read.csv("lakes.csv", T)
attach(lakes)

################################
# CORRELATION PLOT
################################
cor.pearson <- cor.test(alk, log10(chl))
cor.spearman <- cor.test(alk, log10(chl), method="spearman")
cor.kendall  <- cor.test(alk, log10(chl), method="kendall")

op=par(mfrow=c(1,1))
pdf (file="correlation1.pdf", width=9, height=6)
plot(alk, log10(chl), 
     xlab="Alkalinity (mg/L)", 
     ylab=(expression("Chlorophyll " ("log10 " * mu * "g/L"))),
     pch=21, bg="skyblue", cex=2)

legend(x="topleft",
   c("Pearson's r = 0.469", 
     "Spearman's rho = 0.547",
     "Kendall's tau = 0.382",
     "P-values, all <0.0001"),   bty="n")
dev.off()

################################
# REGRESSION PLOT
################################
alkchl.lm = lm(log10(chl) ~alk)
alk.sort = sort(unique(alk))
pred.chl = predict(alkchl.lm, 
    newdata = data.frame(alk = alk.sort), int="pred")

op=par(mfrow=c(1,1))
pdf (file="regression1.pdf", width=9, height=6)
plot(alk, log10(chl), 
     xlab="Alkalinity (mg/L)", 
     ylab=(expression("Chlorophyll " ("log10 " * mu * "g/L"))),
     pch=21, bg="skyblue", cex=2)
abline(alkchl.lm, col="red", lty=2, lwd=2)
lines(alk.sort, pred.chl[,2], lty=2) #lower CI
lines(alk.sort, pred.chl[,3], lty=2) #upper CI

legend(x="topright", 
   c(paste("Log10 Chl =", round(alkchl.lm$coef[2],4), 
     "* Alk + ", round(alkchl.lm$coeff[1],4)),
    paste("Adj.R-squared =", round(summary(alkchl.lm)$adj.r.squared, 4)),
    paste("P-value <0.0001")),
    bty="n", cex=1) 
dev.off()

op=par(mfrow=c(1,1))
pdf (file="regression2.pdf", width=9, height=6)
plot(alk, log10(chl), 
     xlab="Alkalinity (mg/L)", 
     ylab=(expression("Chlorophyll " ("log10 " * mu * "g/L"))),
     pch=21, bg="skyblue", cex=2)
abline(alkchl.lm, col="red", lty=2, lwd=2)
abline(v=median(alk), col="blue", lty=1, lwd=2)

lines(alk.sort, pred.chl[,2], lty=2) #lower CI
lines(alk.sort, pred.chl[,3], lty=2) #upper CI

legend(x="topright", 
   c(paste("Log10 Chl =", round(alkchl.lm$coef[2],4), 
     "* Alk + ", round(alkchl.lm$coeff[1],4)),
    paste("Adj.R-squared =", round(summary(alkchl.lm)$adj.r.squared, 4)),
    paste("P-value <0.0001")),
    bty="n", cex=1) 
legend(x="topleft", c("med alk=", "28.7 mg/L"), text.col="blue", bty="n")

arrows(x0=-10, y0=-0.45, x1=28.7, y1=-0.45, col="navyblue", lwd=3, length=0.2, angle=30)
arrows(x0=-10, y0=1.65, x1=28.7, y1=1.65, col="navyblue", lwd=3, length=0.2, angle=30)

legend(x="bottomright", expression("95 pct CI = 0.3-45 " * mu* "g/L Chl   "), bty="n", text.col="red")
dev.off()

