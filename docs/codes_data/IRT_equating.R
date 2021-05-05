
install.packages("SNSequate")
library("SNSequate")

data("KB36")
str(KB36)

# Parameters of Form X
parm.x <- KB36$KBformX_par

# Parameters of Form Y
parm.y <- KB36$KBformY_par

# Combine the parameters from both forms
parm.xy <- as.data.frame(cbind(parm.y, parm.x))

# Parameters for common items
common.items <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)

# Equate the two forms
irt.link(parm.xy, common.items, model = "3PL", icc = "logistic", D = 1.7)

install.packages("equateIRT")
library("equateIRT")

# Parameters of form X - guessing, difficulty, discrimination
kbx <- cbind(KB36$KBformX_par[,3], KB36$KBformX_par[,2], KB36$KBformX_par[,1])

# Parameters of form Y - guessing, difficulty, discrimination
kby <- cbind(KB36$KBformY_par[,3],KB36$KBformY_par[,2], KB36$KBformY_par[,1])

# Rename the items differently: I1 to I36 in Form X; I37 to I60 in Form Y
row.names(kbx) <- paste0("I", 1:36)
row.names(kby) <- paste0("I", c(37,38, 3,39,40, 6,41,42, 9,43,44, 12,45,46, 15,47,48, 18,49,50, 21,51,52,24,53,54, 27,55,56, 30,57,58, 33,59,60, 36))

# Combine the parameters in a list
datakb <- list(kbx, kby)
datakb

# Coef is the file that includes the item parameters
# ltmparam asks whether the latent trait parameterization is used for difficulty parameters
# lparam asks whether the logistic parameterization is used for guessing parameters
mod <- modIRT(coef = datakb,ltparam = FALSE, lparam = FALSE, display = FALSE)

# Estimate equating constants using each method
# mods: an object returned from modIRT
# which: which forms to be equated
# method: equating method
eq_meanmean <- direc(mods = mod, which = c(1, 2), method = "mean-mean")
eq_meansigma <- direc(mods = mod, which = c(1, 2), method = "mean-sigma")
eq_SL <- direc(mods = mod, which = c(1, 2), method = "Stocking-Lord")
eq_H <- direc(mods = mod, which = c(1, 2), method = "Haebara")

# See the results
summary(eq_meanmean)
summary(eq_meansigma)
summary(eq_SL)
summary(eq_H)


install.packages("mirt")
library("mirt")

# Response data
kbx_data <- KB36$KBformX
kby_data <- KB36$KBformY

# Estimate the item parameters using 3PL
modx <- mirt(kbx_data, model = 1, itemtype = "3PL", verbose = FALSE)
mody <- mirt(kby_data, model = 1, itemtype = "3PL", verbose = FALSE)

# Parameters of Form X
parm.x <- coef(modx, IRTpars = TRUE, simplify = TRUE)$items[, c(1, 2, 3)]

# Parameters of Form Y
parm.y <- coef(mody, IRTpars = TRUE, simplify = TRUE)$items[, c(1, 2, 3)]

# Rename the guessing parameter properly
colnames(parm.x)[3] <- "c"
colnames(parm.y)[3] <- "c"

# Combine the parameters from both forms
parm.xy <- as.data.frame(cbind(parm.y, parm.x))

# Parameters for common items
common.items <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)

# Equate the two forms
irt.link(parm.xy, common.items, model = "3PL", icc = "logistic", D = 1.7)



