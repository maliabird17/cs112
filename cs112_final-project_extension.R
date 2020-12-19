### extension part 1: genetic matching for male ###
# creating vector of covariates
male.X <- cbind(maledata$Weekswrk, maledata$Hrswrk, maledata$Nonwhite, maledata$Hs, maledata$Assoc,
                maledata$Bach, maledata$Grad, maledata$Exper, maledata$Exper_sq, maledata$Marries,
                maledata$Metro, maledata$Midwest, maledata$South, maledata$West, maledata$Agri, maledata$Mine,
                maledata$Construct, maledata$Manu, maledata$Trans, maledata$Trade, maledata$Finance,
                maledata$Busi, maledata$Enter, maledata$Prof, maledata$Public, maledata$Service, maledata$Office,
                maledata$Natconst, maledata$Prodlabor)

# doing genetic matching to determine optimal weights of the covariates
invisible(capture.output(genout.male <- GenMatch(Tr = MaleQueerLogical, X = male.X,
                                                 estimand = "ATT", unif.seed = 123, int.seed = 123, M=1, pop.size = 15, max.generations = 15,
                                                 wait.generations = 5)))

###   Perform the matching and estimation of treatment effect with queer group as treatment group
# Matching covariates with weight from GenMatch
mout.male <- Match(Y = maledata$LogIncome, Tr = MaleQueerLogical, X = male.X, M = 1,
                    estimand = "ATT", Weight.matrix = genout.male)

# Adding Logical version of MaleQueer to female dataset
maledata$MaleQueerLogical <- MaleQueerLogical
# check the balance from matching
mb.male <- MatchBalance(MaleQueerLogical ~ Weekswrk + Hrswrk +Nonwhite +  Hs + Assoc + Bach +
                          Grad + Exper + Exper_sq + Married + Metro + Midwest + South + West + Agri + Mine + Construct +
                          Manu + Trans + Trade + Finance + Busi + Enter + Prof + Public + Service + Office + Natconst +
                          Prodlabor, data = maledata, match.out = mout.male, nboots = 500)

#call the estimated treatment effect
mout.male$est
# getting p-value of EST
summary(mout.male)

### genetic matching for female ###
# creating vector of covariates
female.X <- cbind(femaledata$Weekswrk, femaledata$Hrswrk, femaledata$Nonwhite, femaledata$Hs, femaledata$Assoc,
                  femaledata$Bach, femaledata$Grad, femaledata$Exper, femaledata$Exper_sq, femaledata$Marries,
                  femaledata$Metro, femaledata$Midwest, femaledata$South, femaledata$West, femaledata$Agri, femaledata$Mine,
                  femaledata$Construct, femaledata$Manu, femaledata$Trans, femaledata$Trade, femaledata$Finance,
                  femaledata$Busi, femaledata$Enter, femaledata$Prof, femaledata$Public, femaledata$Service, femaledata$Office,
                  femaledata$Natconst, femaledata$Prodlabor)

# doing genetic matching to determine optimal weights of the covariates
invisible(capture.output(genout.female <- GenMatch(Tr = FemaleQueerLogical, X = female.X,
                                                   estimand = "ATT", unif.seed = 123, int.seed = 123, M=1, pop.size = 15, 
                                                   max.generations = 15, wait.generations = 5)))

###   Perform the matching and estimation of treatment effect with queer group as treatment group
# Matching covariates with weight from GenMatch
mout.female <- Match (Y = femaledata$LogIncome, Tr = FemaleQueerLogical, X = female.X, M = 1,
                      estimand = "ATT", Weight.matrix = genout.female)

# Adding Logical version of FemaleQueer to female dataset
femaledata$FemaleQueerLogical <- FemaleQueerLogical
# check the balance from matching
mb.female <- MatchBalance(FemaleQueerLogical ~ Weekswrk + Hrswrk +Nonwhite +  Hs + Assoc + Bach +
                            Grad + Exper + Exper_sq + Married + Metro + Midwest + South + West + Agri + Mine + Construct +
                            Manu + Trans + Trade + Finance + Busi + Enter + Prof + Public + Service + Office + Natconst +
                            Prodlabor, data = femaledata, match.out = mout.female, nboots = 500)

#call the estimated treatment effect
mout.female$est
# getting p-value of EST
summary(mout.female)



#### extension part 2: quantile treatment effects ###
# install package needed for QTE calculations
install.packages('qte')
library(qte)

# female
# outcome variable: LogIncome; treatment variable: FemaleQueerLogical
# 5% - 95% => 90% confidence interval
# increments of 0.05 in tau
female_quantile_effect <- ci.qtet(LogIncome ~ FemaleQueerLogical, data = femaledata,
                                  probs=seq(0.05,0.95,0.05), se=T, iters=10)
# returns QTE for each tau
summary(female_quantile_effect)
# returns plot with tau and QTE
ggqte(female_quantile_effect)

# male
# same as above
male_quantile_effect <- ci.qtet(LogIncome ~ MaleQueerLogical, data = maledata,
                                probs=seq(0.05,0.95,0.05), se=T, iters=10)
summary(male_quantile_effect)
ggqte(male_quantile_effect)

