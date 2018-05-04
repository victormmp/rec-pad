require(graphics)

# Let's use some example data from the R libraries: USArrests
# Have a quick look at the data
head(USArrests)
summary(USArrests)
# You should see that the measured variables are on
# different scales: Assault varies from 45 to 337, but
# all other variables only from 1 to 90. 
# If we use raw values, it's possible that the absolutely, 
# but not proportionately greater variance of Assault
# might affect our analysis. 
# You should scale data whenever your variables differ 
# by orders of magnitude, as they do here.
# Transcription data usually varies over orders of 
# magnitude, so scaling is appropriate.

# We'll run PCA with prcomp (SVD-based), with and without 
# scaling to unit variance
unscaled = prcomp(USArrests)
scaled = prcomp(USArrests, scale = TRUE)

# Let's plot the results as a screeplot
par(mfrow=c(2,1))
plot(unscaled)
plot(scaled)
# You should see both that the Y-axis values are much reduced,
# and that the relationships between eigenvalues are different,
# when scaling is used.
# In particular the first PC is not so dominant when the data 
# is scaled to unit variance.

# If you look at your prcomp output, you'll see it reports
# Standard deviations and Rotation. The screeplot plots variances,
# but remember that sqrt(Variance) = StDev. So, recovering the
# variances in the screeplot is easy:
summary(unscaled)
unscaled$var = unscaled$sdev * unscaled$sdev
summary(scaled)
scaled$var = scaled$sdev * scaled$sdev
unscaled$var
scaled$var

# It's similarly easy to get the proportion of variance reported
# in the summary
unscaled$var/sum(unscaled$var)
scaled$var/sum(scaled$var)

# Looking at the biplots is also informative
par(mfrow=c(1,2))
biplot(unscaled)
biplot(scaled)

# For the unscaled data, the variable with greatest influence 
# in the loading for PC1 is Assault. 
# It is directed almost entirely along PC1, and 
# almost entirely accounts for PC1, as can be seen from the 
# Rotation (loadings) matrix in the summary.

unscaled

# We noted that the difference in magnitude of the Assault data 
# relative to everything else might skew our results, and indeed
# it has.
# Looking at the scaled data PCA biplot, and the summary:

scaled

# we can see that all four variables contribute much more evenly
# to PC1, but that PC2 is influenced more strongly by UrbanPop and
# Murder.
# You can see this clearly if you plot PC3 in a biplot:

# PC3 vs PC1
biplot(scaled, choices=c(1,3))

# PC3 vs PC2
biplot(scaled, choices=c(2,3))

# However, the variance explained by PCs 3 and 4 is pretty low.
# Nevertheless, a plot of PC4 against PC3 might suggest an interesting
# social study:

biplot(scaled, choices=c(3,4))

x <- USArrests