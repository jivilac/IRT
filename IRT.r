# Install package and load library
library(MCMCpack)

# Load the Data
SupCourt2000 <- read.csv(file.choose())

# Calculate the posterior distributions for each member across 1,000 simulated ideal points ((5500-500)/5)
sc_IRT_mcmc <- MCMCirt1d(SupCourt2000,
                              theta.constraints = list(Scalia = "+"), # anchor the scale
                              mcmc = 5500, # 5500 total iterations
                              burnin = 500, # discard first 500 iterations
                              thin = 5, # discard each 5th
                              store.item = TRUE) # store parameters for posterior calcs

posterior1d <- t(apply(sc_IRT_mcmc, 1, scale))
justices <- posterior1d[ , 1:nrow(SupCourt2000)]

# Store mean values for posterior plots
idealpt_means <- colMeans(justices)

## Plot the posterior distributions from the MCMC sims
plot(density(justices), 
     main = "US Supreme Court: 2000 Term",
     xlab = "Ideology (Liberal - Conservative)",
     ylab = "Posterior Density", xlim=c(-2,3), 
     ylim = c(-0.05,4), 
     type = "n")
abline(h = 0)
text(idealpt_means[1], 1.65, "Rehnquist", font = 2)
text(idealpt_means[2], 1.4, "Stevens", font = 2)
text(idealpt_means[3], 2.2, "O'Connor", font = 2)
text(idealpt_means[4], 1.1, "Scalia", font = 2)
text(idealpt_means[5], 2.8, "Kennedy", font = 2)
text(idealpt_means[6], 1.8, "Souter", font = 2)
text(idealpt_means[7], 1.8, "Thomas", font = 2)
text(idealpt_means[8], 1.675, "Ginsburg", font = 2)
text(idealpt_means[9], 1.5, "Breyer", font = 2)
for (i in 1:9) {
lines(density(justices[,i]), lty = i, lwd = 2, col = i)
}

# Can store and inspect the numeric summaries if so desired
#votes <- posterior1d[ , (nrow(SupCourt2000) + 1):ncol(posterior1d)]
#diff.parameters <- votes[ ,seq(1,ncol(votes), by=2)]
#diff.means <- colMeans(diff.parameters)
#disc.parameters <- votes[ ,seq(2,ncol(votes), by=2)]
#disc.means <- colMeans(disc.parameters)

# The higher the value of the disc parameter, the better job it does discriminating between individuals 
# on the latent dimension - absolute values only matter, because negative and positive values imply 
# liberal or conservative given the dimension we are looking at - but the disc paramater is useful 
# in looking at its magnitude - higher liberal values are relative to the baseline of "scalia" that
# we set earlier - thus higher negative means more liberal, essentially - BUT again, the key is the
# magnitude of the value



#
# Now using the PSCL package for full point plot

library(pscl)

sc_rc <- rollcall(SupCourt2000[ ,2:44], 
                  legis.names=SupCourt2000[ ,1])

sc_IRT_pscl <- ideal(sc_rc,
                     d = 1,
                     maxiter = 5500, 
                     thin = 5, 
                     burnin = 500, 
                     store.item = TRUE)

plot(sc_IRT_pscl, 
     conf.int = 0.95)




## Let's try IRT on some legislative data
# Load the data for the 108 House
hou <- readKH(file.choose())

irt_house <- ideal(hou,
                   dropList=list(codes = "notinLegis", 
                              legisMin = 25),
                   d = 2, 
                   maxiter = 5500, 
                   thin = 5, 
                   burnin = 500,
                   #impute = TRUE, # uncomment to engage multiple imputation; controversial, so proceed with caution
                   store.item = TRUE) 

# plot the output
d1 <- (-1*irt_house$xbar[,1])
d2 <- (-1*irt_house$xbar[,2])

plot(d1, d2, 
     type = "n")
points(d1[hou$legis.data$party == "R"],
       d2[hou$legis.data$party == "R"], 
       col = amerika::amerika_palette(n = 100, 
                                      name = "Republican", 
                                      type = "continuous"), pch = 19)
points(d1[hou$legis.data$party == "D"], 
       d2[hou$legis.data$party == "D"],
       col = amerika::amerika_palette(n = 100, 
                                      name = "Democrat", 
                                      type = "continuous"), pch = 19)
points(d1[hou$legis.data$party == "Indep"], 
       d2[hou$legis.data$party == "Indep"],
       col = "green", pch = 19)



## Finally, let's compare IRT to W-NOMINATE using 89th Senate roll call data (less polarized than today)
sen <- readKH(file.choose())

# Fit an IRT model
irt_sen <- ideal(sen, 
                 d = 2, 
                 maxiter = 5500, 
                 thin = 5, 
                 burnin = 500, 
                 store.item = TRUE)

# Actor-based constraints for post-hoc identification
irt_const <- postProcess(irt_sen,
                         constraints = list(MONDALE = c(-0.774, -0.188), # liberal
                                            TALMADGE = c(0.266, 0.794), # conservative 
                                            SIMPSON = c(0.971, -0.238))) # moderate

# Store mean ideal points from IRT (based on posterior simulations)
ideal1 <- irt_const$xbar[,1] # d1
ideal2 <- irt_const$xbar[,2] # d2

# Store some other useful stuff
code <- sen$legis.data$icpsrLegis
party <- sen$legis.data$partyCode
state <- sen$legis.data$icpsrState

# Fit and store ideal points form W-NOMINATE for comparison
library(wnominate)

wnom_sen <- wnominate(sen, 
                      dims = 2, 
                      polarity = c(2,2)) 

# Store ideal points from NOMINATE version
wnom1 <- wnom_sen$legislators$coord1D # d1
wnom2 <- wnom_sen$legislators$coord2D # d2


# Now, plot the two side-by-side, by party
par(mfrow = c(1,2)) 
par(mar = c(5.5,4,4,2) + 0.1) # make the margins of the plot pane a little bigger (by .5 at the bottom for axis labels)
plot(ideal1, ideal2, 
     main =" ", 
     xlab =" ", 
     ylab =" ",
     xlim = c(-1.5,2.0), 
     ylim = c(-1.5,2.5), asp = 1, type = "n")

# Title and axis labels (manually)
mtext("The 89th US Senate: IRT", side=3, line=1.5, cex=1.6, font=2)
mtext("First Dimension (Liberal - Conservative)", side=1, line=2.25, cex=1.4)
mtext("S = S. Democrat, N = N. Democrat, R = Republican", side=1, line=3.5, cex=1.4)
mtext("Second Dimension (Race)",side=2, line=2.5, cex=1.4)

# S. Democrats
points(ideal1[party == 100 & state >= 40 & state <= 51],
       ideal2[party == 100 & state >= 40 & state <= 51], 
       pch="S", col="gray50", font=2)
points(ideal1[party == 100 & state == 53], 
       ideal2[party == 100 & state == 53],
       pch="S", col="gray50", font=2)
points(ideal1[party == 100 & state == 54], 
       ideal2[party == 100 & state == 54],
       pch="S", col="gray50", font=2)

# N. Democrats
points(ideal1[party == 100 & (state < 40 | state > 54)], 
       ideal2[party == 100 & (state < 40 | state > 54)], 
       pch="N", col="gray67", font=2)
points(ideal1[party == 100 & state == 52], 
       ideal2[party == 100 & state == 52], 
       pch="N", col="gray67", font=2)

# Republicans
points(ideal1[party==200], 
       ideal2[party==200], 
       pch="R", col="gray33", font=2)


#
# W-NOMINATE
par(mar = c(5.5,4,4,2) + 0.1)
plot(wnom1, wnom2, 
     main = " ", 
     xlab = " ", 
     ylab = " ",
     xlim = c(-1.5,2.0), 
     ylim = c(-1.5,2.5), asp = 1, type = "n")

# Title and axis labels
mtext("The 89th US Senate: W-NOMINATE", side=3, line=1.50, cex=1.6, font=2)
mtext("First Dimension (Liberal - Conservative)",side=1, line=2.25, cex=1.4)
mtext("S = S. Democrat, N = N. Democrat, R = Republican", side=1, line=3.5, cex=1.4)
mtext("Second Dimension (Race)", side=2, line=2.5, cex=1.4)

# SD's
points(wnom1[party == 100 & state >= 40 & state <= 51],
       wnom2[party == 100 & state >= 40 & state <= 51], 
       pch="S", col="gray50", font=2)
points(wnom1[party == 100 & state == 53], 
       wnom2[party == 100 & state == 53],
       pch="S", col="gray50", font=2)
points(wnom1[party == 100 & state == 54], 
       wnom2[party == 100 & state == 54],
       pch="S", col="gray50", font=2)

# ND's
points(wnom1[party == 100 & (state < 40 | state > 54)], 
       wnom2[party == 100 & (state < 40 | state > 54)], pch="N", col="gray67", font=2)
points(wnom1[party == 100 & state == 52], 
       wnom2[party == 100 & state == 52], 
       pch="N", col="gray67", font=2)

# R's
points(wnom1[party==200], 
       wnom2[party==200], 
       pch="R", col="gray33", font=2)

# consider adding ablines for h and v for demo purposes
