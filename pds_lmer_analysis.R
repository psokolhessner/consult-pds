data_location = '~/Downloads/';
filename = 'PainDetectionStudy5soccerplayerstreatment.csv';
csvpath <- file.path(data_location, filename)

pdsdata <- read.csv(csvpath) # load the data

# Relevant columns:
#   id (subject ID, N = 154, 1:154)
#   believe (the DV; belief from 1:7)
#   target_id (the ID of the person in the video, N = 40, 1:40)
#   T_race_dummy (0 = black target, 1 = white target)
#   T_veracity_dummy (0 = real, 1 = fake)

pdsdata$T_race_dummy1 = pdsdata$T_race_dummy*2 - 1; # -1 = black, 1 = white
pdsdata$T_veracity_dummy1 = pdsdata$T_veracity_dummy*2-1; # -1 = real, 1 = fake

library(lme4)
library(lmerTest)

mean_believe = list();
mean_believe$blackreal = mean(pdsdata$believe[(pdsdata$T_race_dummy1==-1) & (pdsdata$T_veracity_dummy1==-1)])
mean_believe$whitereal = mean(pdsdata$believe[(pdsdata$T_race_dummy1==1) & (pdsdata$T_veracity_dummy1==-1)])
mean_believe$blackfake = mean(pdsdata$believe[(pdsdata$T_race_dummy1==-1) & (pdsdata$T_veracity_dummy1==1)])
mean_believe$whitefake = mean(pdsdata$believe[(pdsdata$T_race_dummy1==1) & (pdsdata$T_veracity_dummy1==1)])


ffxmodel = lm(believe ~ 1 + T_race_dummy1*T_veracity_dummy1, data = pdsdata)
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      3.85292    0.02331 165.323   <2e-16 ***
# T_race_dummy1                    0.04058    0.02331   1.741   0.0817 .  
# T_veracity_dummy1               -0.24675    0.02331 -10.588   <2e-16 ***
# T_race_dummy1:T_veracity_dummy1 -0.25390    0.02331 -10.894   <2e-16 ***

# Race: No significant main effect
# Veracity: significant negative effect
# Race x Veracity: significant negative interaction

rfxmodel_all = lmer(believe ~ 1 + T_race_dummy1*T_veracity_dummy1 + (1 + T_race_dummy1*T_veracity_dummy1| id),
                data = pdsdata)
#                                    Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                        3.85292    0.05370  153.09242  71.748   <2e-16 ***
# T_race_dummy1                      0.04058    0.02202 4202.00917   1.843   0.0654 .  
# T_veracity_dummy1                 -0.24675    0.02199 5680.95758 -11.221   <2e-16 ***
# T_race_dummy1:T_veracity_dummy1   -0.25390    0.02210 3203.18052 -11.488   <2e-16 ***

# Singular fit, but can't figure out why, all estimates seem OK (incl. RFX).

rfxmodel_bothintercepts = lmer(believe ~ 1 + T_race_dummy1*T_veracity_dummy1 + (1 | id) + (1 | target_id),
                               data = pdsdata)
#                                  Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      3.85292    0.19555 41.41030  19.703   <2e-16 ***
# T_race_dummy1                    0.04058    0.18875 35.99994   0.215    0.831    
# T_veracity_dummy1               -0.24675    0.18875 35.99994  -1.307    0.199    
# T_race_dummy1:T_veracity_dummy1 -0.25390    0.18875 35.99994  -1.345    0.187 

# No significant effects with both intercepts being random

rfxmodel_subjectintercept = lmer(believe ~ 1 + T_race_dummy1*T_veracity_dummy1 + (1 | id),
                                 data = pdsdata)
#                                    Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                        3.85292    0.05371  153.00000  71.739   <2e-16 ***
# T_race_dummy1                      0.04058    0.02199 6003.00000   1.846    0.065 .  
# T_veracity_dummy1                 -0.24675    0.02199 6003.00000 -11.222   <2e-16 ***
# T_race_dummy1:T_veracity_dummy1   -0.25390    0.02199 6003.00000 -11.547   <2e-16 ***

# Normal pattern. 

rfxmodel_targetintercept = lmer(believe ~ 1 + T_race_dummy1*T_veracity_dummy1 + (1 | target_id),
                                data = pdsdata)
#                                  Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      3.85292    0.18875 36.00000  20.412   <2e-16 ***
# T_race_dummy1                    0.04058    0.18875 36.00000   0.215    0.831    
# T_veracity_dummy1               -0.24675    0.18875 36.00000  -1.307    0.199    
# T_race_dummy1:T_veracity_dummy1 -0.25390    0.18875 36.00000  -1.345    0.187    

# No effects.


# Do target-level RFX not work b/c the targets are being treated SO DIFFERENTLY? 
# Try separate regressions on the basis of T race

rfxmodel_targetintercept_blackTonly = lmer(believe ~ 1 + T_veracity_dummy1 + (1 | target_id),
                                           data = pdsdata[pdsdata$T_race_dummy1==-1,])
#                    Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)        3.812338   0.273502 17.999998  13.939 4.37e-11 ***
# T_veracity_dummy1  0.007143   0.273502 17.999998   0.026    0.979    

rfxmodel_targetintercept_whiteTonly = lmer(believe ~ 1 + T_veracity_dummy1 + (1 | target_id),
                                           data = pdsdata[pdsdata$T_race_dummy1==1,])
#                     Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)         3.8935     0.2602 18.0000  14.963 1.34e-11 ***
# T_veracity_dummy1  -0.5006     0.2602 18.0000  -1.924   0.0703 .  

# Doesn't illuminate much. Means are similar (which is expected).
# While T_veracity is different, it's not significant in either


# I think the issue comes down to subdivision. The RFX of subjects are 'ok' because each subject
# sees lots of different kinds of stimuli, so RFX of subjects doesn't operate ON the thing you're
# measuring (the 40 videos each person sees). So RFX of subject is tolerated.
#
# RFX of TARGET is not - because between race and veracity, 4 groups are created w/ 10 targets in each.
# Allowing those 10 targets to vary directly saps the variance that the race/veracity terms are otherwise
# picking up. Put another way, RFX of target ID is operating directly on the thing that has your effects - 
# the different players. It's too much statistically for this dataset to handle. RFX of targets is not
# possible in practice in this dataset. 