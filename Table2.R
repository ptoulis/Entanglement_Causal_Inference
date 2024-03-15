#' 03/2024. "Estimating causal effects when treatments are entangled by a network of units" 
#' Code to replicate Table 2.
#' 
#' Instructions: Each line of Table 2 corresponds to a different specification of JOB_ID. 
#' This code structure was chosen to parallelize the on a cluster in batches of 50.. 
#'  JOB_ID: 1-50 => Line 1 of Table 2
#'.         50-100 => Line 2
#'.         100-150 => Line 3,   
#'.        etc. 
#'. 
#'. Set JOB_ID to reproduce the desired line of Table 2. Then run main_sim().
#'
#' Comments/questions: Panos Toulis (panos.toulis@chicagobooth.edu)
rm(list=ls())


JOB_ID = 200  # JOB_ID in 300
NREPS = 25  # e.g., 25

NUM_CORES = 300
N = 300  # number of units.
# Simulation parameters
GROUND_TRUTH = 4
TEST_val = GROUND_TRUTH # change this for Type-II error

# Given parameter space, it will map JOB ID to appropriate parameter setting.
# Full experiments: e.g., Space = (0, 3, 5) , sigma = (0, 0.2)
# job.id = (i-1)*K *L0 + (j-1)*K + r
get_sim_pars = function(job.id) {
  all_confounding_effect = c(0, 1, 3)
  all_specification = c(0, 0.1)
  L0 = length(all_specification)
  L1 = length(all_confounding_effect)
  
  NUM_SETTINGS = L0*L1
  K = floor(NUM_CORES/NUM_SETTINGS)
  
  r = (job.id %% K)
  job.id = (job.id - r)/K # job.id = (i-1)*L0 + (j-1)
  j = (job.id %% L0) + 1
  job.id = (job.id - (j-1))/L0 # job.id = (i-1)
  i = min(job.id + 1, L1)
  c(all_confounding_effect[i], all_specification[j])
}

CHECK_get_sim_pars = function() {
  jobs = seq(1:NUM_CORES)
  y = t(sapply(jobs, function(j) get_sim_pars(j)))
  y = cbind(jobs, y)
  plot(jobs, y[,2], type="b")
}

# Simulation parameters
PAR = get_sim_pars(JOB_ID) # par_1 = confounding effect. par_2 = misspecification effect.

# other parameters (don't change) -- see model above.
TRUE_mu = 2.1
TRUE_a = 4
TRUE_theta = c(TRUE_mu, TRUE_a)
bernoulli.p = 0.25 ; 

# Jan 2024 - Final revision code.
# rm(list=ls())
library(dplyr)
library(httr)
library(nnet)

#' Model: N = 200 units
#'  D ~ Bernoulli(0.3) -- samples original seeds.
#'  G ~ graph(mu + a*|x_i - x_j)
#'  Z = D + (1-D)*(G * D + u < 0) -- treatment on the non-seeds
#'  Y = -b * exp(-dX)*D + 10* Z + e
#' 
#' Observed data: (Y, Z, D, X)
#' 
#' There is confounding between (dX, Z) because of treatment entanglement Z = f(G), conditional on D.
#' A non-entangled model will model Z ~ X but this would be wrong.
expit = function(u) exp(u) / (1 + exp(u))

#' Samples graph given P_ij : dyadic probabilities.
sample_G = function(P) {
  diag(P) = 0
  stopifnot(nrow(P)==N, ncol(P)==N)
  G = matrix(rbinom(N^2, size=1, prob=P), nrow=N) # G = new connections
  diag(G)=0
  G[lower.tri(G)] = 0  # make G symmetric.
  G = G + t(G) #
  return(G)
}

set.seed(24)
X = rbeta(N, shape1 = 0.4, shape2 = 0.6) # covariates
xi = matrix(X, nrow=N, ncol=N) 
xj = t(xi)

dX = (sqrt(xi*xj)) # X_i * X_j
#dX = abs(xi - xj) # X_i * X_j
# Misspecification: PAR[2] = cor(obs X, unobs X)
# stopifnot(PAR[2] >= 0 & PAR[2] <= 1)
# RE = -PAR[2]*dX + sqrt(1-PAR[2]^2)*matrix( rnorm(N^2, sd=sd(dX)), nrow=N, ncol=N) # rnorm(N, sd=PAR[2])
RE = PAR[2] * abs(xi-xj)

# Sample ground-truth network.
PROB = expit(-TRUE_mu - TRUE_a*dX + RE) # link probabilities
GRAPH = sample_G(PROB)
# Done with initialization. 

# Initialization
# Random seed.
random_seed = as.integer(as.numeric(Sys.time())/10000) * JOB_ID
print(paste("> JOB_ID = ", JOB_ID, " Random seed=", random_seed, "PAR = (", paste(PAR, collapse = ", "), ")"))
# Use the random seed
set.seed(random_seed)

# Sample treatemnt Z given network=g, seed treatment=d
gen_Z = function(g, d) {
  U = rlogis(N)
  as.vector(2*d + (1-d)*(U < g %*% d - 3) ) # treated individually or friend treated, {0,1,2}
}

# Simulate data
sim_data = function() {
  
  # GRAPH = sample_G(PROB)
  D = rbinom(N, size=1, prob=bernoulli.p) # seed treatment
  Z = gen_Z(GRAPH, D) # treated individually or friend treated.
  
  r = PAR[1] # r=0 no confounding, r=1 medium, r=2 high
  C = -r*exp(-r*dX) %*% D # confounding
  Y0 = C + 0.5*rnorm(N)
  Y1 = Y0 + GROUND_TRUTH #
  Y2 = Y1 + rnorm(N)
  Y = Y0*(Z==0) + Y1*(Z==1) + Y2*(Z==2)
  # observed data.
  return(data.frame(Y=Y, Z=Z, D=D, X=X, C=C, Y0=Y0) )
}

# CHECK function for sim_data()
CHECK_sim_data = function(vis=F) {
  
  par(mfrow=c(3, 3))
  
  print(paste(">> Parameters = ", paste(PAR, collapse = ", ")))
  
  print(">> X distribution") 
  print(summary(X))
  print(paste("** G: Symmetric? ", all(rowSums(GRAPH) == colSums(GRAPH)), " edges=", sum(GRAPH), "Density=", sum(GRAPH)/(N*(N-1)/2)) )
  
  d = sim_data()
  print(">> Z distribution")
  print(table(d$Z))
  deg = rowSums(GRAPH)
  print(">> Degree distribution")
  print(table(deg))
  # boxplot(X ~ deg)
  print(paste(">> avg(Z[D==0]) = ", round(mean(d$Z[d$D==0]),2), " exposed non-seeds - should be more than 15%"))
  library(igraph)
  if(vis) {
    g = graph_from_adjacency_matrix(GRAPH, mode = "undirected")
    print(head(which(d$D==1)))
    # plot(g, vertex.size=10, vertex.color=d$Z+1)
  }
  
  ## Outcomes
  hist(X, breaks=50)
  plot(Y ~ X, d, pch=20, col=Z+1)
  boxplot(Y ~ Z, d, pch=20)
  fit = lm(Y ~ Z + X, d)
  print(confint(fit))
  print(paste("True effect = ", GROUND_TRUTH))
  
  boxplot(X ~ Z, data=d, main="all")
  boxplot(C ~ Z, data=d, main="all")
  boxplot(X ~ Z, data=d[d$D==0,], main="conditional (D=0)")
  boxplot(C ~ Z, data=d[d$D==0,], main="conditional (D=0)")
  boxplot(Y ~ Z, data=d[d$D==0,], main="conditional (D=0)")
  # ps = ent_aware_model(d, method="oracle")
  # print(summary(ps[d$D==0,]))
  
  dat = sim_data()
  ps = matrix(runif(3*N), nrow=N)
  dat_ps = add_ps(ps, dat, ngroups=10)
  print(paste("unif PS estimator = ", round(estimator(dat_ps), 2)))
  
  ps= naive_model(dat)
  dat_ps = add_ps(ps, dat, ngroups=10)
  print(paste("naive PS estimator = ", round(estimator(dat_ps), 2)))
  
  se = ps_randtest_covers(ps, dat, H0_effect = GROUND_TRUTH, return.se = T)
  
  ps = ent_aware_model(dat, method="oracle")
  dat_ps = add_ps(ps, dat, ngroups=10)
  
  print(paste("oracle PS estimator = ", round(estimator(dat_ps), 2)))
  est = CHECK_Naive()
  hist(est, breaks=40)
  print(paste("Estimate reject ", mean(est < GROUND_TRUTH-2*se | est > GROUND_TRUTH+2*se)) )
}

#' Calculate MLE of network model.
#' 
mle = function(dat) {
  t0 = proc.time()[3]
  print(">> Running mle...")
  # MLE for network model given Z_obs
  D = dat$D # original seed treatment
  I = which(D==0) # Learn network model conditional on D=seeds.
  Z_I = dat$Z[I] # treatments for units who are not seeds. Only those inform the likelihood
  
  lik = function(theta) {
    theta = exp(theta) # parameters are positive
    theta = as.numeric(theta)
    stopifnot(all(theta >= 0))
    stopifnot(length(theta)==2)
    P_ij = expit(-theta[1]-theta[2]*dX)
    ll = mean(replicate(20, {
      g = sample_G(P_ij)
      V = g %*% D - 3
      u_I = plogis(V[I])
      exp(mean(dbinom(Z_I, size=1, prob=u_I, log = T)))
    }))
    return(ll)
  }
  
  # initialize from a good place. 
  # NOTE: Evaluate the methodology free of numerical issues.  
  out = optim(par=0.95*log(1e-6 + TRUE_theta) + 0.05*rnorm(2), fn = lik, method="BFGS", control=list(fnscale=-1))
  print(paste("MLE finished in ", round(proc.time()[3] - t0,2), "seconds") )
  exp(out$par)
}


# Naive model
naive_model = function(dat) {

  #fit = multinom(Z ~ X, dat, trace=FALSE)
  fit = multinom(Z ~ X + D, dat, trace=FALSE)
  
  #fit = glm(Z ~X , dat, family="poisson")
  ps = fitted(fit)
  return(ps)
}

# method = hat, true, oracle 
# hat= use MLE on possibly misspecified model
# true = use true parameter under possibly misspecified model
# oracle = use correct model
ent_aware_model = function(dat, method="hat", num.rand=500) {
  # optimize for a,b. Then sim to calculate true ps.
  P_ij = NA
  if(method=="hat") {
    theta = mle(dat)
    P_ij = expit(-theta[1] - theta[2]*dX)
  } else if(method=="oracle") {
    P_ij = PROB
  } else if(method=="true") {
    ps = matrix(0, nrow=N, ncol=3)
    ps[,3] = dat$D
    ps[,2] = (1-dat$D)*plogis( as.vector(GRAPH %*% dat$D) - 3)
    ps[,1] = 1-ps[,2]-ps[,3]
    colnames(ps) = seq(0, ncol(ps)-1)
    return(ps)
  } else {
    stop(paste("Method ", method, "is not recognized."))
  }
  
  Z_sampl = NA
  if(method=="oracle") {
    # oracle
    Z_sampl = replicate(num.rand, {
      gen_Z(GRAPH, dat$D) # treated individually or friend treated.
    })
  } else {
    Z_sampl = replicate(num.rand, {
      g = sample_G(P_ij)
      gen_Z(g, dat$D) # treated individually or friend treated.
    })
  }
  
  # Z_sampl = N x num.rand
  ps = t(apply(Z_sampl, 1, function(row) {
    c(mean(row==0), mean(row==1), mean(row==2))
  }))
  
  stopifnot(all(ps[dat$D==0, 3] == 0))
  stopifnot(all(ps[dat$D==1, 3] == 1))
  colnames(ps) = seq(0, ncol(ps)-1)
  # ps = N x 3
  return(ps)
}

add_ps = function(ps, dat, ngroups=10) {
  stopifnot(ncol(ps)==3) # ps0, ps1, ps2
  stopifnot(nrow(ps)==nrow(dat)) #
  m = length(unique(ps[dat$D==0,1]))
  ngroups = min(ngroups, m); # print(paste("ngroups=", ngroups))
  dat = dat %>% mutate(ps0=ps[,1], ps1=ps[,2], ps2=ps[,3], class = kmeans(ps[,1:2], centers=ngroups)$cluster) %>% 
                filter(Z %in% c(0,1)) %>% select(Y,Z,class, ps0, ps1, ps2)
  return(dat) # [class, psK, Y, Z]. ps1 = P(Z=K|X)
}

# subclassification
# Estimators for treatment effect Y(1)-Y(0)
# TODO:speedup
estimator = function(dat.ps) {
  
  # dat = [class, Y, Z]
  all_groups = unique(dat.ps$class)
  
  Z = dat.ps$Z
  Y = dat.ps$Y
  s_groups = sapply(all_groups, function(g) {
    Ig = (dat.ps$class==g)
    ng = sum(Ig)
    n1 = sum(Ig*Z)
    n0 = ng - n1
    if(n0*n1==0) {
      return(c(0, 0))
    } else {
      return( c(ng, (sum(Ig * Y*Z) / n1 - sum(Ig *Y*(1-Z))/ n0 ) ) )
    }
  })
  # s_groups = 2 x num_groups, row 1 = cluster size, row 2= estimate
  weighted.mean(s_groups[2,], w = s_groups[1,])
}

estimator2 = function(ps, dat) {
  
  stopifnot(ncol(ps)==3) # ps0, ps1, ps2
  stopifnot(nrow(ps)==nrow(dat)) #
  I = which(dat$Z %in% c(0,1))
  stopifnot(all(dat$D[I]==0))
  
  dat = dat %>% select(Y, Z, X) %>% filter(Z %in% c(0,1))
  
  dat$ps1 = ps[I,2]/(ps[I,1]+ps[I,2])
  
  dat$ps1[dat$ps1==1] = 1-1e-5
  dat$ps1[dat$ps1==0] = 1e-5
  
  dat$ps0 = 1-dat$ps1
  
  with(dat, sum(Y*Z/ps1) - sum(Y*(1-Z)/ps0)) / nrow(dat) # IPW
}


randomization_pval = function(tobs, tvals, two.sided=FALSE) {
  # tvals are additional draws
  N = length(tvals)
  p1 = (1/(N+1)) *(sum(tvals > tobs) + runif(1)*(1+sum(abs(tvals - tobs) < 1e-15)))
  if(!two.sided) {
    return(p1)
  } else {
    p2 = 2*min(p1, 1-p1)
    return(p2)
  }
}

# Test Y(0) = Y(1), Y(exposed non-seed) = Y(control non-seed)
# PS-based approximate randomization test.
# 1. Subclassify on supplied propensity scores (PS). 
# 2. Perform randomization test for the sharp null Y(1) = Y(0) + true_effect.
ps_randtest_covers = function(ps, dat, H0_effect, ngroups=10, num.rand=500, vis=F, return.se=FALSE) {
  
  # ps = N x 3 - matrix of propensity scores for 0,1,2
  dat.ps = add_ps(ps, dat, ngroups=ngroups) # [class, ps_k, Y, Z], Z in {0,1}
  stopifnot(all(dat.ps$Z %in% c(0,1)))
  
  tstat = function(df_1) {
    estimator(df_1)
  }
  
  T_obs = tstat(dat.ps)  # obs test statistic
  n = nrow(dat.ps)
  dat.ps$ps1= dat.ps$ps1 / (dat.ps$ps1 + dat.ps$ps0) #   ps_I = ps[I,2]/(ps[I,1] + ps[I,2])
  
  randomize = function() {
    df_r = dat.ps
    df_r$Z = rbinom(n, size=1, prob=dat.ps$ps1)
    df_r$Y = dat.ps$Y + (df_r$Z - dat.ps$Z)*H0_effect # impute outcomes
    return(df_r)
  }
  
  T_vals = replicate(num.rand, {
    df_r = randomize()
    tstat(df_r)
  })
  
  if(return.se) {
    return(sd(T_vals))
  }
  
  pval = randomization_pval(T_obs, T_vals, two.sided = T)
  
  if(vis) {
    hist(T_vals, breaks=40, main=paste("tobs=", round(T_obs,2), "pval=", round(pval,2)))
    abline(v=T_obs, col="red", lwd=2)
  }
  
  return(pval)
}

# should be unbiased for GROUND_TRUTH
CHECK_estimator = function() {
  est = c()
  for(i in 1:100) {
    dat = sim_data()
    ps = ent_aware_model(dat, method="oracle")
    est = c(est, estimator(ps, dat, ngroups=14))
    hist(est, breaks=50)
    abline(v=GROUND_TRUTH, col="red")
    print(summary(est))
  }
}


CHECK_Oracle = function() {
  rej = c()
  for(i in 1:100) {
    dat = sim_data()
    ps = ent_aware_model(dat, method="oracle")
    pval =  ps_randtest_covers(ps, dat, H0_effect = TEST_val, v=T, num.rand = 500)
    print(pval)
    rej = c(rej, pval < 0.05)
    print(paste("i=", i, " / 100 rejection=", round(100*mean(rej), 2), "%") )
  }
}

CHECK_True = function() {
  rej = c()
  for(i in 1:1000) {
    dat = sim_data()
    ps = ent_aware_model(dat, method="true")
    print(paste(" num=", length(table(GRAPH %*% dat$D)), " -- OK.."))
    print(paste("Estimator = ", round(estimator(add_ps(ps, dat)), 2)))
  }
}

CHECK_Naive = function(do.test=FALSE) {
  rej = c()
  est = c()
  
  for(i in 1:300) {
    dat = sim_data()
    ps = naive_model(dat)
    # ps_true = ent_aware_model(dat, "oracle")
    if(do.test) {
      pval =  ps_randtest_covers(ps, dat, H0_effect = TEST_val, v=T)
      rej = c(rej, pval < 0.05)
      print(paste("i=", i, " / 100 rejection=", round(100*mean(rej), 2), "%") )
    }
    
    dat_ps = add_ps(ps, dat)
    est = c(est, estimator(dat_ps))
  }
  
  return(est)
}

# Main simulation
main_sim = function(nreps=NREPS, print_every=3) {
  
  # M = results
  cols = c("naive", "entangled", "true", "type")
  # type=0 (coverage) type=1 (bias)
  M = matrix(0, nrow=0, ncol=length(cols))
  colnames(M) = cols
  t0 = proc.time()[3]
  T_name = random_seed
  
  save_results = function(M_results) {
    Results = list()
    Results$par_1 = PAR[1]
    Results$par_2 = PAR[2]
    Results$nsampl = nrow(M_results)/2 # because we save (type=0) and (type=1) for each repetition
    Results$M = M_results
    save(Results, file=sprintf("output/results_%d_%d.rda", JOB_ID, T_name))
  }
  
  for(irep in 1:nreps) {
    dat = sim_data()
    ps1 = naive_model(dat)
    ps2 = ent_aware_model(dat, method = "hat")
    ps3 = ent_aware_model(dat, method = "true")
    NGROUPS = 10
    rej1 = (ps_randtest_covers(ps1, dat, H0_effect = TEST_val, ngroups = NGROUPS, vis = F) < 0.05)
    rej2 = (ps_randtest_covers(ps2, dat, H0_effect = TEST_val, ngroups = NGROUPS, vis = F) < 0.05)
    # use 14 groups for true PS. Shouldn't affect results though.
    rej3 = (ps_randtest_covers(ps3, dat, H0_effect = TEST_val, ngroups = NGROUPS+4, vis = F) < 0.05)
    
    est1 = estimator(add_ps(ps1, dat, ngroups = NGROUPS))
    est2 = estimator(add_ps(ps2, dat, ngroups = NGROUPS))
    est3 = estimator(add_ps(ps3, dat, ngroups = NGROUPS+4))
    
    M = rbind(M, c(rej1, rej2, rej3, 0))
    M = rbind(M, c(est1- GROUND_TRUTH, est2-GROUND_TRUTH, est3-GROUND_TRUTH, 1))
    
    if(irep %% print_every==0) {
      print(paste("** Simulation: Job=", JOB_ID, ", Confounding = ", PAR[1], "/ misspecification = ", PAR[2] , " ** ") )
      print(paste("irep=", irep, " / ", nreps, " time/irep = ", round((proc.time()[3]-t0)/irep,2)," Rejection = "))
      M0 = M[which(M[,"type"]==0), ]
      
      print( round(100*colMeans(M0, na.rm = T), 3) ) 
      # save_results(M)
    }
  }
  
  t1= proc.time()[3]
  print(t1-t0)
}
