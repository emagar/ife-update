## # Este es un mixture model que pesqué del internet:

## mixmodel = function() {
##     for( i in 1 : N ) {
##         y[i] ~ dnorm(mu[i], tau)
##         mu[i] <- lambda[T[i]]
##         T[i] ~ dcat(pi[]) }
##     pi[1:2] ~ ddirch(alpha[])
##     theta ~ dnorm(0.0, 1.0E-6)%_%I(0.0, )
##     lambda[1] ~ dnorm(0.0, 1.0E-6)
##     lambda[2] <- lambda[1] + theta
##     tau ~ dgamma(0.001,0.001)
##     sigma <- 1 / sqrt(tau)
## }

## # applied notation: two components
## mixmodel = function() {
##     for( i in 1 : N ) {
##         swing[i] ~ dnorm(mu[i], tau)           # individual swings are normally distributed
##         mu[i] <- mean[component[i]]            # with i group's-specific means;
##         component[i] ~ dcat(mix_proportions[]) # this predicts i's component: 
##     }
##     # PRIORS
##     mix_proportions[1:2] ~ ddirch(a[])         # a[] is a data vector of 1, 1.5, 2 for Woldenberg 1
##     mean[1] ~ dnorm(0.0, 1.0E-4)               # group means are
##     mean[2] <- mean[1] + gap                   # separated by a gap 
##     gap ~ dnorm(0.0, 1.0E-4)%_%I(0.0, )        # gap is normally distributed
##     tau ~ dgamma(0.01,0.01)
##     sigma <- 1 / sqrt(tau)
## }

# Intento de traducirlo al modelo vectorizado del IFE:

ife.model.items.mix = "model {
	for (n in 1:n.obs) {
		y[n] ~ dbern (pi[n])
		probit(pi[n]) <- beta[vote[n]]*theta[member[n]] - alpha[vote[n]]
	}
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
	# Beta (discrimination) --- votes sorted such that 1-2 are north-south, rest uninformative
        for(j in 3:n.item){ 
            beta [j] ~ dnorm(0, 0.1)
        }
        beta [1] ~ dnorm( 4, 4)
        beta [2] ~ dnorm(-4, 4)
	# ideal points
	for(i in 1:n.member) {
        theta[i] ~ dnorm(mu[i],1) 
        mu[i] <- promedios[component[i]]
        component[i] ~ dcat(mix_proportions[])
	}
  # three mixture components (party contingents)
  mix_proportions[1:3] ~ ddirch(a[])     # a[] is a data vector with three elements
	gaps[1] ~ dnorm(0, 1.0E-0)T(0,)
	gaps[2] ~ dnorm(0, 1.0E-0)T(0,)
	promedios[1] ~ dnorm(0.0, 1.0E-0)
	promedios[2] <- promedios[1] + gaps[1]
	promedios[3] <- promedios[2] + gaps[2]
}"




