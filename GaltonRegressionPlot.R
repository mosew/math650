z <- read.table("galton.txt")
plot(z$V1,z$V2,ylab="Son's Height",xlab="Father's Height", main="Galton's Regression")
galton.lm=lm(z$V2 ~ z$V1)
abline(galton.lm$coeff["(Intercept)"],galton.lm$coeff["z$V1"])