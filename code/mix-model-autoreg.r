####################################################
## Yearly mixture model with autoregressive       ##
## components for years with no identifying votes ##
####################################################

# move anchors to first period for identification, use this model
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

# remainder periods use this model... needs mixture code
ife.model.autoreg-mix = "model {
	for (n in 1:n.obs) {
		y[n] ~ dbern (pi[n])
		probit(pi[n]) <- beta[vote[n]]*theta[member[n]] - alpha[vote[n]]
	}
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }
	# Beta (discrimination)
	for (j in 1:n.item) { beta[j] ~ dnorm(0, 0.1) }
	# ideal points
	for(i in 1:n.member)  { theta[i] ~ dnorm( mean.theta[i], precision.theta[i] ) }
}"


