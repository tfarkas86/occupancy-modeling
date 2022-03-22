yrwa <- read.csv("~/Dropbox/Occupancy Modelling/yrwa_detection.csv")[,-1]
fosp <- read.csv("~/Dropbox/Occupancy Modelling/fosp_detection.csv")[,-1]
brcr <- read.csv("~/Dropbox/Occupancy Modelling/brcr_detection.csv")[,-1]

# naive p for each survey
yrwa_ns <- colSums(yrwa)/nrow(yrwa)
fosp_ns <- colSums(fosp)/nrow(fosp)
brcr_ns <- colSums(brcr)/nrow(brcr)

yrwa_avg <- mean(yrwa_ns) # 0.137
fosp_avg <- mean(fosp_ns) # 0.135
brcr_avg <- mean(brcr_ns) # 0.120

# naive p using summarized data

yrwa_sum <- sum(ifelse(rowSums(yrwa) > 0, 1, 0))/nrow(yrwa) # 0.201
fosp_sum <- sum(ifelse(rowSums(fosp) > 0, 1, 0))/nrow(fosp) # 0.174
brcr_sum <- sum(ifelse(rowSums(brcr) > 0, 1, 0))/nrow(brcr) # 0.190

# p using all histories 

yrwa_sp <- mean(rowSums(yrwa)/ncol(yrwa)) # 0.137
fosp_sp <- mean(rowSums(fosp)/ncol(fosp)) # 0.135
brcr_sp <- mean(rowSums(brcr)/ncol(brcr)) # 0.120

# p using non-zero histories

yrwa_sp2 <- mean(rowSums(yrwa[rowSums(yrwa) >0,])/ncol(yrwa)) # 0.680
fosp_sp2 <- mean(rowSums(fosp[rowSums(fosp) >0,])/ncol(fosp)) # 0.776
brcr_sp2 <- mean(rowSums(brcr[rowSums(brcr) >0,])/ncol(brcr)) # 0.633

# unmarked with yellow warblers

yrwa_un <- unmarkedFrameOccu(yrwa)
plot(yrwa_un)
yrwa.occ <- occu(~ 1 ~ 1, data=yrwa_un)

fosp_un <- unmarkedFrameOccu(fosp)
plot(fosp_un)
fosp.occ <- occu(~ 1 ~ 1, data=fosp_un)

brcr_un <- unmarkedFrameOccu(brcr)
plot(brcr_un)
brcr.occ <- occu(~ 1 ~ 1, data=brcr_un)

unlogit <- function(logit) {
  return((exp(-logit)+1)^-1)
}

# point estimates

yrwa_psi <- unlogit(coef(yrwa.occ)[1]) # 0.210
yrwa_pee <- unlogit(coef(yrwa.occ)[2]) # 0.651
fosp_psi <- unlogit(coef(fosp.occ)[1]) # 0.176
fosp_pee <- unlogit(coef(fosp.occ)[2]) # 0.766
brcr_psi <- unlogit(coef(brcr.occ)[1]) # 0.204
brcr_pee <- unlogit(coef(brcr.occ)[2]) # 0.590

# backTranform with confidence intervals

confint(backTransform(yrwa.occ, type="state"))
confint(backTransform(yrwa.occ, type="det"))
confint(backTransform(fosp.occ, type="state"))
confint(backTransform(fosp.occ, type="det"))
confint(backTransform(brcr.occ, type="state"))
confint(backTransform(brcr.occ, type="det"))


