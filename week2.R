dd <- read.csv("~/Dropbox/Occupancy Modelling//yrwa_detection.csv")[,-1]

# naive occupancy for each survey
colSums(dd)/nrow(dd) # 1 = 0.147, #2 = .128, #3 = 0.136

# naive occupancy for all surveys combined
sum(colSums(dd))/(nrow(dd)*3) # .137

# table of 0s, 1s, 2s, and 3s

dd$rsum <- rowSums(dd)
tabsum0 <- table(dd$rsum)
psum0 <- tabsum0/sum(tabsum0) ## answer

# table of 1s, 2s, and 3s

tabsum <- table(factor(dd$rsum, level=0:3))[-1]
psum <- prop.table(tabsum)

# expected histories

ehist <- function(p) 


x95 <- rbinom(1000, size=3, prob=.95)
x05 <- rbinom(1000, size=3, prob=.05)
x50 <- rbinom(1000, size=3, prob=.50)
x80 <- rbinom(1000, size=3, prob=.80)
x65 <- rbinom(1000, size=3, prob=.70) # best match is .70
ehist(x95)
ehist(x05)
ehist(x50)
ehist(x70)

# loop to find minimized difference

diff <- function(p) {
  xp <- rbinom(1000, size=3, prob=p)
  sum(abs(psum - prop.table(table(factor(xp, level=0:3))[-1])))
}

ps <- seq(.001, 1, length.out=1000)
diffs <- sapply(ps, diff)

pd <- data.frame(ps, diffs)
p <- pd$ps[which.min(pd$diffs)]

# probability 000s are false negative
p000 <- (1-p)^3 # 

#adjusted probability of occupancy
psi <- sum(colSums(dd))/(nrow(dd)*3) # 0.37






