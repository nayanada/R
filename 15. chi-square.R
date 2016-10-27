
# Chi-square goodness of fit
observed <- c(23, 12, 25)
chisq.test(observed)

# chi-square test of indepdendence
observed1 <- matrix(c(40,10,10,90,40,10), nrow=2, ncol=3, byrow = T)
chisq.test(observed1)
