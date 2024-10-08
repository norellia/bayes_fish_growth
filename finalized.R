# Run cleaning first to set up the data, this script handles all of the JAGS runs
# Dataframes from cleaning: cleaned_wtpa = all, wtpa = measured

# This script creates and runs 3 JAGS models, one with just the measured length at ages, one with all the length at ages, and a third with a subset of ages 1-5.  
# Loosely based off Ortiz de Zárate and Babcock (2016).  

# Load Packages
library(R2jags)
library(ggmcmc)


##### All Measured Data ######

# Create model.file for JAGS
# I looped through 282 observations to find a von Bertalanffy curve that would fit the measured data
# The priors were selected based off the general range of White Perch growth curves
write("model
      {
      for(i in 1:282){
        PredL[i] <- Linf * (1 - exp(-K*(Age[i] - Age0)))
        logPredL[i] <- log(PredL[i])
        Length[i] ~ dlnorm(logPredL[i], tau)
			  logObsL[i] <-log(Length[i]) 	     # log transfomration of observed value
			  resid[i] <- logObsL[i]-logPredL[i]  # residuals
		 	  Rep[i] ~ dlnorm(logPredL[i], tau) # replicated data set
		 	  logRep[i] <-log(Rep[i]) # replicated data set
		 	  Prob[i] <- step(logRep[i] - logObsL[i])  # Probability replicated data>real data
        sresid2[i]<-(logObsL[i]-logPredL[i])*(logObsL[i]-logPredL[i])*tau   # stand. residuals squared
        rep.sresid2[i]<-(logRep[i]-logPredL[i])*(logRep[i]-logPredL[i])*tau  # replicated sresid squared
        sstest[i] <- (logObsL[i]-logMean)*(logObsL[i]-logMean)*tau 
      }
      #priors specification
      K ~ dunif(0,2)
      Age0 ~ dunif(-10,0)
      Linf ~ dunif(200,400)
      tau ~ dgamma(0.001,0.001)
      
      #Derived parameters
      logMean <-mean(logObsL[])
      mean.prob<-mean(Prob[])
      chi.square.obs<-sum(sresid2[])
      chi.square.rep<-sum(rep.sresid2[])
      sstot <-sum(sstest[])
      p.value<-step(chi.square.obs-chi.square.rep)
      r.squared <- 1-(chi.square.obs/sstot)
      r.squared2 <-1-1/(tau*sd(logObsL[])*sd(logObsL[]))
      
      }", file = "vonBwtp.txt")

# Set initial values for JAGS
init1 <- list(tau=1,K=0.6)
init2 <- list(tau=0.5,K=0.4)

# Create a copy of wtpa (might remove this)
wtp <- wtpa

# Run JAGS model with 400,000 iterations, 50,000 burn in, and 100 thin
wtpjags=jags(wtp,list(init1,init2),model.file="vonBwtp.txt",
             parameters.to.save=c("K","Linf","tau","Age0","p.value","r.squared","r.squared2",
                                  "PredL","logPredL"),
             n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

# Create and look at JAGS output
res1 <- wtpjags$BUGSoutput
head(res1$summary)
round(res1$summary[c("Age0", "K","Linf","p.value","r.squared","r.squared2"),c("mean","sd","2.5%","50%","97.5%","Rhat","n.eff")],3)

# Plot the glm line of the mean with confidence intervals over a scatter plot of the data 
df <- data.frame(res1$summary[paste0("PredL[",1:282,"]"),c("mean","2.5%","50%","97.5%")])
names(df) <- c("mean","lci","median","uci")
df <- cbind(df,wtp)
ggplot(df, aes(x=Age, y=Length, ymin=lci, ymax=uci))+
  geom_point() +
  geom_ribbon(alpha=0.3) +
  theme_bw() +
  geom_line(aes(x=Age,y=mean)) +
  ylab("Length")

# Print the DIC and pD
res1$DIC
res1$pD

# Do a second run and save just the vonB parameters to make the gelman plots 
wtpjags2=jags(wtp,list(init1,init2),model.file="vonBwtp.txt",
             parameters.to.save=c("K","Linf","Age0"),
             n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

mc1 <- as.mcmc(wtpjags2)
gelman.plot(mc1)

##### All Measured Data and Back-Calculations #####
write("model
      {
      for(i in 1:1368){
        PredL[i] <- Linf * (1 - exp(-K*(Age[i] - Age0)))
        logPredL[i] <- log(PredL[i])
        Length[i] ~ dlnorm(logPredL[i], tau)
        			  logObsL[i] <-log(Length[i]) 	     # log transfomration of observed value
			  resid[i] <- logObsL[i]-logPredL[i]  # residuals
		 	  Rep[i] ~ dlnorm(logPredL[i], tau) # replicated data set
		 	  logRep[i] <-log(Rep[i]) # replicated data set
		 	  Prob[i] <- step(logRep[i] - logObsL[i])  # Probability replicated data>real data
        sresid2[i]<-(logObsL[i]-logPredL[i])*(logObsL[i]-logPredL[i])*tau   # stand. residuals squared
        rep.sresid2[i]<-(logRep[i]-logPredL[i])*(logRep[i]-logPredL[i])*tau  # replicated sresid squared
        sstest[i] <- (logObsL[i]-logMean)*(logObsL[i]-logMean)*tau 
      }
      #priors specification
      K ~ dunif(0,2)
      Age0 ~ dunif(-10,0)
      Linf ~ dunif(200,400)
      tau ~ dgamma(0.001,0.001)
      
      #Derived parameters
      logMean <-mean(logObsL[])
      mean.prob<-mean(Prob[])
      chi.square.obs<-sum(sresid2[])
      chi.square.rep<-sum(rep.sresid2[])
      sstot <-sum(sstest[])
      p.value<-step(chi.square.obs-chi.square.rep)
      r.squared <- 1-(chi.square.obs/sstot)
      r.squared2 <-1-1/(tau*sd(logObsL[])*sd(logObsL[]))
      }", file = "vonBwtp2.txt")

wtp <- cleaned_wtpa

wtpjags3=jags(wtp,list(init1,init2),model.file="vonBwtp2.txt",
             parameters.to.save=c("K","Linf","tau", "Age0","r.squared", "p.value","r.squared2",
                                  "PredL","logPredL"),
             n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

res2=wtpjags3$BUGSoutput
res2$summary

round(res2$summary[c("Age0", "K","Linf","p.value","r.squared","r.squared2"),c("mean","sd","2.5%","50%","97.5%","Rhat","n.eff")],5)

df2<-data.frame(res2$summary[paste0("PredL[",1:1368,"]"),c("mean","2.5%","50%","97.5%")])
names(df2)<-c("mean","lci","median","uci")
df2<-cbind(df2,wtp)
ggplot(df2,aes(x=Age,y=Length,ymin=lci,ymax=uci))+geom_point()+geom_ribbon(alpha=0.3)+theme_bw()+geom_line(aes(x=Age,y=mean))+ylab("Length")

res2$DIC
res2$pD

wtpjags4=jags(wtp,list(init1,init2),model.file="vonBwtp2.txt",
              parameters.to.save=c("K","Linf", "Age0"),
              n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

mc2 <- as.mcmc(wtpjags4)
gelman.plot(mc2)

##### Subset #####
# The final model used a subsample of ages 1-5 where each age had 100 samples where the measured length at ages were supplemented by however many back-calculated ages were needed to reach 100 samples.   
df_3 = 0
for(i in 1:5){
  #only get less than or equal to age i
  s <- filter(cleaned_wtpa, Age <= i)
  s <- filter(s, (i-1) < Age)
  #only get = to age i 
  s.b <- filter(s, Type == "Back-Calc")
  s.a <- filter(s, Type == "Measured")
  x = 100 - nrow(s.a)
  s.b <- sample_n(s.b, x)
  df_3 <- rbind(df_3, s.a, s.b)
}
df_3 <- df_3[-1,]

df_3$round <- ceiling(df_3$Age)
ggplot(df_3, aes(round, fill = Type)) +
  geom_histogram(bins = 5, color = "black") + scale_fill_manual(values=c("white", "black"))+ggtitle("Histogram of Fish Age") + ylab("Count") + theme_classic()


write("model
      {
      for(i in 1:500){
        PredL[i] <- Linf * (1 - exp(-K*(Age[i] - Age0)))
        logPredL[i] <- log(PredL[i])
        Length[i] ~ dlnorm(logPredL[i], tau)
        logObsL[i] <-log(Length[i]) 	     # log transfomration of observed value
			  resid[i] <- logObsL[i]-logPredL[i]  # residuals
		 	  Rep[i] ~ dlnorm(logPredL[i], tau) # replicated data set
		 	  logRep[i] <-log(Rep[i]) # replicated data set
		 	  Prob[i] <- step(logRep[i] - logObsL[i])  # Probability replicated data>real data
        sresid2[i]<-(logObsL[i]-logPredL[i])*(logObsL[i]-logPredL[i])*tau   # stand. residuals squared
        rep.sresid2[i]<-(logRep[i]-logPredL[i])*(logRep[i]-logPredL[i])*tau  # replicated sresid squared
        sstest[i] <- (logObsL[i]-logMean)*(logObsL[i]-logMean)*tau 
      }
      #priors specification
      K ~ dunif(0,2)
      Age0 ~ dunif(-10,0)
      Linf ~ dunif(200,400)
      tau ~ dgamma(0.001,0.001)
      
      #Derived parameters
      logMean <-mean(logObsL[])
      mean.prob<-mean(Prob[])
      chi.square.obs<-sum(sresid2[])
      chi.square.rep<-sum(rep.sresid2[])
      sstot <-sum(sstest[])
      p.value<-step(chi.square.obs-chi.square.rep)
      r.squared <- 1-(chi.square.obs/sstot)
      r.squared2 <-1-1/(tau*sd(logObsL[])*sd(logObsL[]))
      
      }", file = "vonBwtp3.txt")

wtp <- df_3

wtpjags5=jags(wtp,list(init1,init2),model.file="vonBwtp3.txt",
              parameters.to.save=c("K","Linf","tau", "Age0","r.squared", "p.value","r.squared2",
                                   "PredL","logPredL"),
              n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

res3=wtpjags5$BUGSoutput
res3$summary

round(res3$summary[c("Age0", "K","Linf","p.value","r.squared","r.squared2"),c("mean","sd","2.5%","50%","97.5%","Rhat","n.eff")],5)


df3<-data.frame(res3$summary[paste0("PredL[",1:500,"]"),c("mean","2.5%","50%","97.5%")])
names(df3)<-c("mean","lci","median","uci")
df3<-cbind(df3,wtp)
ggplot(df3,aes(x=Age,y=Length,ymin=lci,ymax=uci))+geom_point()+geom_ribbon(alpha=0.3)+theme_bw()+geom_line(aes(x=Age,y=mean))+ylab("Length")

res3$DIC
res3$pD

wtpjags6=jags(wtp,list(init1,init2),model.file="vonBwtp3.txt",
              parameters.to.save=c("K","Linf", "Age0"),
              n.chains=2,n.iter=400000,n.burnin=50000,n.thin=100)

mc3 <- as.mcmc(wtpjags6)
gelman.plot(mc3)

par(mfrow = c(2,2))
densplot(mc2)   
densplot(mc1)   
densplot(mc3)   

ggplot(cleaned_wtpa,aes(x=Age,y=Length))+geom_point()+theme_bw()+geom_line(data = df, aes(x=Age,y=mean))+geom_line(data = df2, aes(x=Age,y=mean), color = "blue")+geom_line(data = df3, aes(x=Age,y=mean), color = "red")+ylab("Length")

