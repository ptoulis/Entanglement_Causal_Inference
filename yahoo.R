#' 03/2024. "Estimating causal effects when treatments are entangled by a network of units" 
#' Code to replicate Table 3.
#' 
#' Instructions: See README_entanglement.pdf for details.
#'
#' Comments/questions: Panos Toulis (panos.toulis@chicagobooth.edu)
#' 
#' Note: Before running this code, you should run yahoo_preprocess.R to create the necessary processed datasets.
#' 
rm(list=ls())
options(digits=5)
library(dplyr)

#' TODO: Change this folder pointing to your local installation of the Yahoo Go datasets.
DATA_FOLDER = "~/Datasets/Yahoo/"

#' Builds the communication network between users in the US during Week 2. 
#' 
#' For every pairwise communication between users, we flag whether the users communicating are seeds, and whether they are treated.
#' (treatment = message by a seed.)
#' 
#' @param seeds Vector of user ids that we define as seeds.
#' @param all_users Vector of user ids that is the superset of all users in our analysis (seeds \in all_users)
Build_CommNetwork_Week2 = function(seeds, all_users) {
  #
  print("Loading comm network in Week 2...")
  load("~/Datasets/Yahoo/dataset/im_08_14.rda") # communications in Week 2.
  # 2. Communications.
  cleanup_im = function(im_obj) {
    print("Cleaning up IM object...")
    im_obj$ego_is_seed = as.numeric(im_obj$egoid %in% seeds)
    im_obj$alter_is_seed = as.numeric(im_obj$alterid %in% seeds)
    im_obj$ego_treat = im_obj$alter_is_seed * (1-im_obj$ego_is_seed)
    im_obj$alter_treat = im_obj$ego_is_seed * (1-im_obj$alter_is_seed)
    
    # (a) take only US
    im_obj = im_obj %>% filter(egoid %in% all_users, alterid %in% all_users)    # comm network in the US, Week 2.
    # (b) remove self-messages
    im_obj = subset(im_obj, egoid != alterid) # remove self-messages (what is this?)
    
    # Keep all communications.
    return(im_obj)
  }
  
  # Week 2 comms.
  sum(im2$count) # 758 million total messages.
  im_week2 = cleanup_im(im2)
  sum(im_week2$count) # 325,309 messages in US -- 892,908
  
  im_ij = im_week2 # 
  im_ij$activity_code=NULL
  return(im_ij)
}

make_hist = function(y, lab) {
  par(mar = rep(6, 4))  # Increase the bottom margin
  hist(y, main="", xlab="", cex.lab=3.5, cex.main=3.5, cex.axis=2, ylab="", breaks=25)
  mtext(lab, side=1, line=4, cex=3)
}

#' Generates the (X, Z, Y) for data analysis.
Process = function() {

  # Country 238 = US
  user_file = paste(DATA_FOLDER,"dataset/simple/user.rda", sep="")
  go_file = paste(DATA_FOLDER,"dataset/simple/go.rda", sep="")
  
  load(user_file)  # user data
  load(go_file)  # go data
  
  # user = 
  # userid priorgo hasreg gender ageyr country
  
  nrow(user) # 27.5 million users
  any(duplicated(user$userid)) # should be FALSE
  mean(user$priorgo==0) # 98.5% have not used GO
  mean(user$gender==-1) # 9% have no gender information.
  mean(user$ageyr==-1) # 99% have no age information.
  
  # TODO(ptoulis): Could make GO => user profile. 
  
  # GO usage for month of October/2007
  # date userid firstgo devid pctpv gopv fppv mailpv srchpv wthrpv newspv finpv sportspv flickrpv
  
  length(unique(go$userid)) # 384k users of GO. ~ 1.5% of all users.
  
  # 1. Create Users
  print("> Creating user = individual from the US, has full information, < 70 year old.")
  user = user %>% filter(country==238,  ageyr > -1, gender > -1, ageyr <= 70) # US
  nrow(user)  # 27.5 million -> 13.02 million
  mean(user$priorgo==0) # 98.3% have not used GO. Similar.
  summary(user)    # 60% male, ~30yr old.
  
  make_hist(user$ageyr,"age")
  all_users =  unique(user$userid)
  print(paste(">> There are ", length(all_users), "users."))  # all_users = 13 million with full info in the US
  
  # focus on US users
  mean(unique(go$userid) %in% all_users) # 58.3% of GO data users are in USER subset.
  go = go %>% filter(userid %in% all_users) # 1.28milion entries in subpop.
  nrow(go)
  
  # 2. Create Seeds
  stopifnot(all(go %>% group_by(userid) %>% summarize(used_go=sum(firstgo)) %>% pull(used_go))) # Check in GO data frame all users adopted go.
  a  = go %>% group_by(userid) %>% summarize(first=date[which(firstgo==1)]) %>% pull(first)
  
  adoption_day = go %>% group_by(userid) %>% summarize(first=date[which(firstgo==1)]) %>% pull(first)
  make_hist(adoption_day,"Day of adoption")

  seeds = go %>% filter(date <= 7, firstgo==1) %>% pull(userid) # seeds = adopters in Week 1.  
  any(duplicated(seeds)) # Should be FALSE
  print(paste(">> There are ", length(seeds), "seeds.")) # ~ 115k seeds in the US
  
  im_ij = NA
  fname_im = paste(DATA_FOLDER, "dataset/simple/im_ij.rda", sep="")
  
  if(file.exists(fname_im)) {
    print(paste("Loading ", fname_im))
    load(fname_im)
  } else {
    im_ij = Build_CommNetwork_Week2(seeds, all_users)
    sum(im_ij$count) # 243 million messages
    ego_gender = left_join(im_ij, user, c("egoid"="userid")) %>% select(gender)
    alter_gender = left_join(im_ij, user, c("alterid"="userid")) %>% select(gender)
    ego_ageyr = left_join(im_ij, user, c("egoid"="userid")) %>% select(ageyr)
    alter_ageyr = left_join(im_ij, user, c("alterid"="userid")) %>% select(ageyr)
    
    im_ij$ego_ageyr = ego_ageyr$ageyr
    im_ij$alter_ageyr = alter_ageyr$ageyr
    im_ij$ego_gender = ego_gender$gender
    im_ij$alter_gender = alter_gender$gender
    
    im_ij = im_ij %>% mutate(age_diff = abs(ego_ageyr - alter_ageyr), 
                             close_age = (abs(ego_ageyr - alter_ageyr) < 5),
                             same_gender= (ego_gender==alter_gender) )

    im_ij = im_ij %>% mutate(hash=log(5)*(egoid+alterid) + log(59)*abs(alterid-egoid))
    a = im_ij %>% group_by(hash) %>% mutate(left=egoid < alterid, right= egoid > alterid, both=(sum(left)>0)*(sum(right)>0)) %>% mutate(ok=left*both + (1-both)) #   + (1-(sum(left)>0)*(sum(right)>0)))
    im_ij = im_ij[a$ok==1,]
    save(im_ij, file=fname_im)
  }
  
  # user = users in US with "full info"
  # userid priorgo hasreg gender ageyr country
  
  # go = GO usage for USERS.
  # date userid firstgo devid pctpv gopv fppv mailpv srchpv wthrpv newspv finpv sportspv flickrpv
  
  head(im_ij)
  user_com = im_ij %>% group_by(egoid) %>% summarize(total=sum(count))
  make_hist(log(1+user_com$total), "Log number of messages")
  #   egoid  alterid count ego_is_seed alter_is_seed ego_treat alter_treat ego_ageyr alter_ageyr ego_gender alter_gender age_diff close_age same_gender       hash

  nrow(im_ij) # 15.5 million comm entries -> 10.5 million (after removing duplicates)
  sum(im_ij$count) # 243 million messages -> 178m
  
  # Sanity checks
  all(im_ij$ego_gender*im_ij$alter_gender + (1-im_ij$ego_gender)*(1-im_ij$alter_gender) == im_ij$same_gender)
  user %>% filter(userid %in% c(3, 18381866)) # Close age=TRUE, Same_gender=FALSE
  all(im_ij$egoid %in% all_users)
  all(im_ij$alterid %in% all_users)
  
  # whether units were treated as egos
  print("> Creating im_treat = [userid, num_treat] = num of messages on a user from a seed.")
  im_ego_treat = im_ij %>% group_by(egoid) %>% summarize(ntreat=sum(ego_treat))
  # whether units were treated as alters
  im_alter_treat = im_ij %>% group_by(alterid) %>% summarize(ntreat=sum(alter_treat))

  # A user was treated 7 times.
  im_ego_treat$egoid[which(im_ego_treat$ntreat==7)]
  im_ij %>% filter(egoid==16701932) %>% pull(alter_is_seed) %>% sum # Should be 10.
  im_treat = full_join(im_ego_treat, im_alter_treat, c("egoid"="alterid"))
  im_treat[is.na(im_treat)] = 0
  im_treat = im_treat %>% mutate(userid=egoid, ntreat=ntreat.x + ntreat.y) %>% select(-c(egoid, ntreat.x, ntreat.y))
  # im_treat$ntreat = as.numeric(im_treat$ntreat > 0)
  # userid ntreat
  # <int>  <dbl>
  #   3      0
  #   41      0
  #   50      0
  #   171      0
  #...
  # im_treat = users who communicated in Week 2. ntreat=were they treated or not.
  head(im_treat, 20) # 264 was treated,
  im_ij %>% filter(egoid==264) # was treated as ego
  stopifnot(4219324 %in% seeds)
  stopifnot(!any(duplicated(im_treat$userid)))
  print(paste(">> A total of ", nrow(im_treat), " users communicated in Weeks > 2.")) # 3.5 million users.
  all(im_treat$userid %in% all_users)
  # im_treat contains all users who communicated at least once.
  
# Define, X, Z, Y
  # X
  print(paste("> Defining units = users (not seed) who communicated with someone in Week 2"))
  units  = setdiff(unique(im_treat$userid), seeds) # units = those who communicated at least once in week 2.
  print(paste(">> There are ", length(units), " units."))  # 3.54 mil units.
  print(paste("> Creating X."))
  X = user %>% filter(userid %in% units) %>% select(userid, gender, ageyr)  # 3.5m users.
  stopifnot(nrow(X) == length(units))
  any(X$userid %in% seeds) # should be FALSE
  # Z
  print(paste("> Creating Z."))
  Z = left_join(X, im_treat, c("userid"="userid"))
  stopifnot(sum(is.na(Z))==0)
  Z = Z$ntreat
  table(Z)
  # Z
  # 0             1       2       3       4       5       6       7      10 
  # 3514577   12424    8963     801      58      11       4       1       1 
  
  # Y
  print("> Define adopters.")
  # TODO(ptoulis): We could explore various definitions of this. 
  adopters = go %>% filter(date > 7, firstgo==1, userid %in% units) # Adopted in Week 2 or later.
  print(paste(">> There are ", nrow(adopters), "adopters."))  # 9,581 = adopters in the US
  Y = rep(0, nrow(X)) # Create outcome vector
  Y[X$userid %in% adopters$userid] = 1   # sum(Y) = 9,581
  
  save(X, file="X.rda")
  save(Y, file="Y.rda")
  save(Z, file="Z.rda")
  
  # Full data frame.
  D = data.frame(y=Y, z=Z, age=X$ageyr, sex=X$gender, userid=X$userid)
  
  print("> Ego Creating xx,xo,ox,oo variables (communications between same gender, or close age).")
  ego_d = im_ij %>% group_by(egoid) %>% summarise(xx=sum(count*same_gender * close_age), 
                                                  xo=sum(count*same_gender * (1-close_age)), 
                                                  ox=sum(count*(1-same_gender) * close_age),
                                                  oo=sum(count*(1-same_gender) * (1-close_age)), 
                                                  total=sum(count))
  summary(ego_d)
  # > head(ego_d)
  # # A tibble: 6 × 5
  #   egoid    xx    xo    ox    oo
  #    <int> <int> <dbl> <dbl> <dbl>
  
  # >   summary(ego_d)
  # egoid                xx               xo               ox               oo        
  # Min.   :       3   Min.   :     0   Min.   :     0   Min.   :     0   Min.   :     0  
  # 1st Qu.: 6258586   1st Qu.:     0   1st Qu.:     0   1st Qu.:     0   1st Qu.:     0  
  # Median :12817798   Median :     0   Median :     0   Median :     1   Median :     1  
  # Mean   :13101642   Mean   :    22   Mean   :    24   Mean   :    37   Mean   :    42  
  # 3rd Qu.:19762610   3rd Qu.:     3   3rd Qu.:     4   3rd Qu.:    14   3rd Qu.:    19  
  # Max.   :27499976   Max.   :124599   Max.   :482049   Max.   :124459   Max.   :122844  
  
  # Clear ordering. More communications happens across genders and within similar age.
  
  ego_d[which.max(ego_d$xx),]
  im_ij %>% filter(egoid==16607375) %>% pull(count) %>% sum # 125,000 messages! 
  # Look into unit=172
  im_ij %>% filter(egoid==172)
  
  D2 = left_join(D, ego_d, by=c("userid"="egoid"))
  print("> Alter Creating xx,xo,ox,oo variables (communications between same gender, or close age).")
  alter_d = im_ij %>% group_by(alterid) %>% summarise(xx=sum(count*same_gender * close_age), 
                                                      xo=sum(count*same_gender * (1-close_age)), 
                                                      ox=sum(count*(1-same_gender) * close_age),
                                                      oo=sum(count*(1-same_gender) * (1-close_age)), 
                                                      total=sum(count))
  D3 = left_join(D2, alter_d, by=c("userid"="alterid"))
  head(D3)
  
  # y z age sex userid xx.x xo.x ox.x oo.x total.x xx.y xo.y ox.y oo.y total.y
  
  D3[is.na(D3)] = 0
  
  D4 = D3  %>% mutate(all_xx = xx.x + xx.y,
                      all_xo = xo.x + xo.y,
                      all_ox = ox.x + ox.y,
                      all_oo = oo.x + oo.y, 
                      total = total.x + total.y)
  D4 = D4 %>% select(y, z, age, sex, userid, all_xx, all_xo, all_ox, all_oo, total)
  yahoo_all = D4
  
  # Final dataset:
  yahoo_all$z = as.numeric(yahoo_all$z > 0)   ## 
  #   y z age sex userid all_xx all_xo all_ox all_oo
  
  save(yahoo_all, file="yahoo_all.rda")
}

# Analysis here. 

# Randomization test for global null.
Global_null = function() {
  
  load("yahoo_all.rda")
  any(duplicated(yahoo_all$userid))
  fit = glm(y ~ z + age + sex, data=yahoo_all, family="binomial")
  # glm(formula = y ~ z + age + sex, family = "binomial", data = yahoo_all)
  
  # 
  # Coefficients:
  #   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -7.022857   0.035119  -200.0   <2e-16 ***
  #   z            0.570566   0.044008    13.0   <2e-16 ***
  #   age          0.023547   0.000907    26.0   <2e-16 ***
  #   sex          0.569045   0.022708    25.1   <2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # (Dispersion parameter for binomial family taken to be 1)
  # 
  # Null deviance: 132407  on 3536839  degrees of freedom
  # Residual deviance: 130999  on 3536836  degrees of freedom
  # AIC: 131007
  #
  # With binarized treatment
  # > summary(fit)
  # 
  # Call:
  #   glm(formula = y ~ z + age + sex, family = "binomial", data = m)
  # 
  # Coefficients:
  #   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -6.9925052  0.0373071 -187.43   <2e-16 ***
  #   zTRUE        1.0400976  0.0838797   12.40   <2e-16 ***
  #   age          0.0210257  0.0009939   21.16   <2e-16 ***
  #   sex          0.6706132  0.0247149   27.13   <2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # (Dispersion parameter for binomial family taken to be 1)
  # 
  # Null deviance: 119655  on 3104947  degrees of freedom
  # Residual deviance: 118179  on 3104944  degrees of freedom
  # AIC: 118187
  # 
  # Number of Fisher Scoring iterations: 9
  # 
  fit0 = glm(y ~ 1, data=yahoo_all, family="binomial")
  Tobs = AIC(fit0) - AIC(fit) # =1401
  Tvals= c()
  for(i in 1:20) {
    fit_i = glm(sample(y) ~ z + age + sex, data=yahoo_all, family="binomial")
    # Tval ~= +-4
    Tvals = c(Tvals, AIC(fit0) - AIC(fit_i))
    hist(Tvals, breaks=20, main=round(Tobs, 2))
  }
}

#' Randomization-based analysis of results.
#' 
Rand_Analysis = function(data, use_entanglement=TRUE, level0=1, level1=2, H0_effect=0, trim=0.05, nrand=250) {
  H0_effect = round(H0_effect, 5)
  yahoo_all=data
  stopifnot(c("y", "z", "age", "sex") %in% colnames(yahoo_all))
  yahoo_all$z = (yahoo_all$z > 0) + (yahoo_all$z > 1) # make binary
  
  # yahoo_all$z = factor((yahoo_all$z > 0) + (yahoo_all$z > 1), labels=c("B", "A", "C"), ordered=T)
  # Model
  # fit = glm(y ~ z + age + sex, data=yahoo_all, family="binomial")
  # summary(fit)
  # Call:
  #   glm(formula = y ~ as.factor(z) + age + sex, family = "binomial", 
  #       data = yahoo_all)
  # 
  # Coefficients:
  #   Estimate Std. Error  z value Pr(>|z|)    
  # (Intercept)   -7.0233340  0.0351226 -199.966  < 2e-16 ***
  #   as.factor(z)1  0.7712587  0.1187774    6.493  8.4e-11 ***
  #   as.factor(z)2  1.2176050  0.1082897   11.244  < 2e-16 ***
  #   age            0.0235034  0.0009075   25.900  < 2e-16 ***
  #   sex            0.5698962  0.0227117   25.093  < 2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # (Dispersion parameter for binomial family taken to be 1)
  # 
  # Null deviance: 132407  on 3536839  degrees of freedom
  # Residual deviance: 130993  on 3536835  degrees of freedom
  # AIC: 131003
  # 
  m = NA
  if(use_entanglement) {
     W = yahoo_all[,6:9]
     A = matrix(rowSums(W), nrow=nrow(W), ncol=4, byrow=F)
     W = round(10*W/A) # communication profile: normalized
     
     m = yahoo_all
     m[,6:9] = W 
     m = m %>% select(y, z, age, sex, all_xx, all_xo, all_ox, all_oo) %>% 
               group_by(age, sex, all_xx, all_xo, all_ox, all_oo) %>% 
               mutate(ps=sum(z)/n(), class=paste(age,sex, all_xx, all_xo, all_ox, all_oo, sep="_"), num=n()) 
     
     # > head(m)
     # y     z   age   sex all_xx all_xo all_ox all_oo      ps class            num
     # <dbl> <int> <int> <int>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <chr>        <int>

     
  } else {
    m = yahoo_all %>% group_by(age, sex) %>% mutate(ps=sum(z)/n(), class=paste(age, sex, sep="_"), num=n())
  }
  
  m0 = m
  m = m0 # start here
  L = quantile(m$ps, trim)
  R = quantile(m$ps, 1-trim)
  m = m %>%  filter(ps > L, ps < R)
  nrow(m) # 3.1m ~= removes ~0.4 million out of 3.5 million.
          # with entanglement = 2.9m ~= removes ~0.6 million out of 3.5 million.
  length(unique(m$class)) # 93 without, 2486 with entanglement
  
  # a = m %>% group_by(class) %>% summarize(ps=ps[1], age=age[1])
  # par(mar=rep(6, 4))
  # plot(a$age, a$ps, pch=20, cex=1.5, ylab="propensity score", xlab="age", cex.lab=2.1)
  print(">> Conditioning the test.")
  yahoo_all = yahoo_all %>% filter(z %in% c(level0, level1))

  
  # [m] has propensity score column (ps). 
  tstat = function(df) {
    stopifnot("class" %in% colnames(df))
    df1 = df %>% group_by(class) %>% summarize(ATE=mean(y[z==level1]) - mean(y[z==level0]), nk=n(), nkt=sum(z))
    100*weighted.mean(df1$ATE, w=df1$nk, na.rm=T)  #
  }
  
  T_obs = tstat(m)
  T_vals = c()
  
  for(irep in 1:nrand) {
    # df = m %>% group_by(class) %>% mutate(z=sample(z))
    
    df_r = m %>% group_by(class) %>% mutate(z=sample(z))
    df_r$y = df_r$y + H0_effect*(df_r$z - m$z) # impute missing outcomes
    T_vals = c(T_vals, tstat(df_r))
    p = mean(T_vals > T_obs) 
    pval = 2*min(p, 1-p)
    if(irep%%3==0) {
      hist(T_vals, cex.lab=1.5, xlab="randomization distribution", 
           main=paste("H0:", H0_effect, ", T_obs=", round(T_obs, 3), ", pval=", round(pval, 4), " (levels=", level0, "-", level1, ", nrand=", irep, "/", nrand, ", sigma=", round(sd(T_vals),2), ")"), 
           breaks=sqrt(irep))
      abline(v=T_obs, col="red", lwd=2)
    }
  }

  return(list(rand=T_vals, T_obs=T_obs, pval=pval))
}


#' Run this first.
Table3_standard = function(nreps=1000) {
  # No entanglement, Test Y(0)=Y(1) and Y(1)=Y(2)
  load("yahoo_all.rda")
  
  out = Rand_Analysis(yahoo_all, FALSE, level0=0, level1=1, nrand=nreps) # Tobs=0.326, pval=0, se_rand=0.05
  0.326 - qnorm(0.025)*0.05
  0.326 - qnorm(1-0.025)*0.05
  Rand_Analysis(yahoo_all,FALSE, level0=0, level1=1, H0_effect = 0.228/100, nrand =nreps) # left endpoint Y(0)=Y(1)
  Rand_Analysis(yahoo_all,FALSE, level0=0, level1=1, H0_effect = 0.424/100, nrand=nreps)  # right  Y(0)=Y(1)
  
  # Y(1)=Y(2)
  out = Rand_Analysis(yahoo_all,FALSE, level0=1, level1=2, nrand=nreps) # Tobs=0.325, pval=0, se_rand=0.08
  right = out$T_obs - quantile(out$rand, 0.025) # 0.80716 
  left = out$T_obs - quantile(out$rand, 0.975) # 0.51447 
  Rand_Analysis(yahoo_all,FALSE, level0=1, level1=2, H0_effect = 0.178/100, nrand = nreps) # left endpoint
  Rand_Analysis(yahoo_all,FALSE, level0=1, level1=2, H0_effect = 0.476/100, nrand=nreps)
}

Table3_entanglement = function(nreps=1000) {
  
  # ENTANGLEMENT=TRUE
  # With entanglement, Test Y(0)=Y(1) and Y(1)=Y(2)
  load("yahoo_all.rda")
  out = Rand_Analysis(yahoo_all, TRUE, level0=0, level1=1, nrand=nreps) # Tobs = 0.682, se_rand=0.07
  save(out, file=sprintf("Table3__entangled_0_1_nreps=%d.rda", nreps))
  right = out$T_obs - quantile(out$rand, 0.025) # 0.80716 
  left = out$T_obs - quantile(out$rand, 0.975) # 0.51447 
  
  # [1] "Y(0)=Y(1). Effect =  0.00511 pval= 0.027"
  # [1] "Y(0)=Y(1). Effect =  0.0080 pval= 0.063"
  # 
  out = Rand_Analysis(yahoo_all, TRUE, level0=0, level1=1, H0_effect = left/100, nrand = nreps) # left endpoint Y(0)=Y(1)
  print(paste("Y(0)=Y(1). Effect = ", left/100, "pval=", out$pval))
  out = Rand_Analysis(yahoo_all, TRUE, level0=0, level1=1, H0_effect = 0.82/100, nrand=nreps)  # right  Y(0)=Y(1)
  print(paste("Y(0)=Y(1). Effect = ", right/100, "pval=", out$pval))
  
  # Y(1)=Y(2)
  out = Rand_Analysis(yahoo_all, TRUE, level0=1, level1=2, nrand=nreps) # Tobs = -0.21, 0.065, se_rand=0.11
  save(out, file=sprintf("Table3__entangled_1_2_nreps=%d.rda", nreps))
  
  right = out$T_obs - quantile(out$rand, 0.025) # -0.45708 
  left = out$T_obs - quantile(out$rand, 0.975) # -0.0023
  
  out = Rand_Analysis(yahoo_all, TRUE, level0=1, level1=2, H0_effect = -0.44/100, nrand = nreps) # left endpoint Y(0)=Y(1)
  print(paste("Y(1)=Y(2). Effect = ", left/100, "pval=", out$pval))
  out = Rand_Analysis(yahoo_all, TRUE, level0=1, level1=2, H0_effect = 0.006/100, nrand=nreps)  # right  Y(0)=Y(1)
  print(paste("Y(1)=Y(2). Effect = ", right/100, "pval=", out$pval))
  
}



Placebo_study = function(num.rand=200) {
  
  load("yahoo_all.rda")
  # yahoo_all$z = (yahoo_all$z > 0) + (yahoo_all$z > 1)
  yahoo_all$z_factor = factor((yahoo_all$z > 0) + (yahoo_all$z > 1)) # 0, 1, 2
  # Specify "1" as the baseline level
  yahoo_all$z_factor  = relevel(yahoo_all$z_factor , ref = "1")
  
  expit = function(u) exp(u)/(1+exp(u))
  N = nrow(yahoo_all)
  # Should give 5% rejection rate.
  Results = matrix(0, nrow=0, ncol=5)
  colnames(Results) = c("setting", "tau", "test", "method", "reject")
   # setting 0 : Y = tau*(Z>0) , 1: Y=tau*(Z>2)
   # tau = [0, 4]
  # test 0:  Y(0)=Y(1) and 1: Y(1)=Y(2)
  # method 0=glm, 1=FRT
  # reject = (0,1)
  for(irep in 1:NREPS) {
    d = yahoo_all
    
    # Sample parameters
    tau = round(runif(1, max=4), 3)
    setting = rbinom(1, size=1, prob=0.5) # 0=model 0 , 1 =model 1
    
    if(setting==0) {
      pr = expit(-5.8 -0.0143*abs(d$age-30) + tau*(d$z > 0) )
      d$y = rbinom(N, size=1, prob=pr)
    } else {
      pr = expit(-5.8 -0.0143*abs(d$age-30) + tau*(d$z > 2) )
      d$y = rbinom(N, size=1, prob=pr)
    }
    
    fit = lm(y ~ z_factor + d$age  +sex, data=d)
    ci = confint(fit)[c(2, 3), ] # first line tests Y(0)=Y(1)
    
    contains_0 = function(w) {
      stopifnot(length(w)==2 & w[2] >= w[1] )
      as.numeric(0 >= w[1] & 0 <= w[2])
    }
    # test=0 reject if ci[1,] does not cover true tau
    rej = 1 - contains_0(ci[1,]) # need to negate
    Results = rbind(Results, c(setting, tau, test=0, method=0, reject=rej))
    # test=1 Check if 0 in interval
    rej = 1 - contains_0(ci[2,])
    Results = rbind(Results, c(setting, tau, test=1, method=0, reject=rej))
    
    ## FRT
    out = Rand_Analysis(d, use_entanglement = FALSE, level0=0, level1=1, nrand=num.rand)
    rej = as.numeric(out$pval < 0.05)
    Results = rbind(Results, c(setting, tau, test=0, method=1, reject=rej))# testing y(0)=y(1)
    
    out = Rand_Analysis(d, use_entanglement = FALSE, level0=1, level1=2, nrand=num.rand)
    rej = as.numeric(out$pval < 0.05)
    Results = rbind(Results, c(setting, tau, test=1, method=1, reject=rej)) # testing y(1)=y(2)
    
    print(Results)
    
    Results = as.data.frame(Results)
    fname = sprintf("output/results_%d.rda", JOB_ID)
    save(Results, file=fname)
    
  }
  
}
