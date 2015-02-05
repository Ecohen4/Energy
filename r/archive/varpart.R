install.packages("vegan")
library(vegan)

data(mite)
data(mite.env)
data(mite.pcnm)

## See detailed documentation:
## Not run: 
vegandocs("partition")
## End(Not run)

# Two explanatory matrices -- Hellinger-transform Y
# Formula shortcut "~ ." means: use all variables in 'data'.
mod <- varpart(mite, ~ ., mite.pcnm, data=mite.env, transfo="hel")
mod
showvarparts(2)
plot(mod)

### try varpart() on my data ###

###  


# Alternative way of to conduct this partitioning
# Change the data frame with factors into numeric model matrix
mm <- model.matrix(~ SubsDens + WatrCont + Substrate + Shrub + Topo, mite.env)[,-1]
mod <- varpart(decostand(mite, "hel"), mm, mite.pcnm)
# Test fraction [a] using RDA:
rda.result <- rda(decostand(mite, "hell"), mm, mite.pcnm)
anova(rda.result, step=200, perm.max=200)


# standardization methods in vegan package
data(varespec)
sptrans <- decostand(varespec, "max")
apply(sptrans, 2, max)
sptrans <- wisconsin(varespec)

## Chi-square: PCA similar but not identical to CA.
## Use wcmdscale for weighted analysis and identical results.
sptrans <- decostand(varespec, "chi.square")
plot(procrustes(rda(sptrans), cca(varespec)))