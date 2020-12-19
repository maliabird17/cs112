#loading cobalt function for balance plots
library(cobalt)
library(gridExtra)


### Male propensity score balance plots 
prop_match_plot1m  <- bal.plot(pmatchmale, 'Grad', 
                         formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                           Nonwhite +  Hs + Assoc + Bach + Grad + 
                           Exper + Exper_sq + Married + Metro + Midwest + South +
                           West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                           Busi + Enter + Prof + Public + Service +
                           Office + Natconst + Prodlabor,data = maledata, which = "both",
                         sample.names = c("Unmatched", "Matched"))

prop_match_plot2m  <- bal.plot(pmatchmale, 'Prof', 
                         formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                           Nonwhite +  Hs + Assoc + Bach + Grad + 
                           Exper + Exper_sq + Married + Metro + Midwest + South +
                           West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                           Busi + Enter + Prof + Public + Service +
                           Office + Natconst + Prodlabor,data = maledata, which = "both",
                         sample.names = c("Unmatched", "Matched"))

prop_match_plot3m  <- bal.plot(pmatchmale, 'Trade', 
                         formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                           Nonwhite +  Hs + Assoc + Bach + Grad + 
                           Exper + Exper_sq + Married + Metro + Midwest + South +
                           West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                           Busi + Enter + Prof + Public + Service +
                           Office + Natconst + Prodlabor,data = maledata, which = "both",
                         sample.names = c("Unmatched", "Matched"))

prop_match_plot4m  <- bal.plot(pmatchmale, 'Exper', 
                               formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = maledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

grid.arrange(prop_match_plot1m , prop_match_plot2m , prop_match_plot3m , prop_match_plot4m ,  nrow = 2)


### Female propensity score balance plots 
prop_match_plot1f  <- bal.plot(pmatchfemale, 'Grad', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor, data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

prop_match_plot2f  <- bal.plot(pmatchfemale, 'Prof', 
                         formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                           Nonwhite +  Hs + Assoc + Bach + Grad + 
                           Exper + Exper_sq + Married + Metro + Midwest + South +
                           West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                           Busi + Enter + Prof + Public + Service +
                           Office + Natconst + Prodlabor,data = femaledata, which = "both",
                         sample.names = c("Unmatched", "Matched"))

prop_match_plot3f  <- bal.plot(pmatchfemale, 'Trade', 
                         formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                           Nonwhite +  Hs + Assoc + Bach + Grad + 
                           Exper + Exper_sq + Married + Metro + Midwest + South +
                           West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                           Busi + Enter + Prof + Public + Service +
                           Office + Natconst + Prodlabor,data = femaledata, which = "both",
                         sample.names = c("Unmatched", "Matched"))

prop_match_plot4f  <- bal.plot(pmatchfemale, 'Exper', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

grid.arrange(prop_match_plot1f , prop_match_plot2f , prop_match_plot3f , prop_match_plot4f ,  nrow = 2)


### Male genetic matching balance plots 
gen_match_plot1m  <- bal.plot(mout.male, 'Grad', 
                               formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = maledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot2m  <- bal.plot(mout.male, 'Prof', 
                               formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = maledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot3m  <- bal.plot(mout.male, 'Trade', 
                               formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = maledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot4m  <- bal.plot(mout.male, 'Exper', 
                               formula = MaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = maledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

grid.arrange(prop_match_plot1m , prop_match_plot2m , prop_match_plot3m , prop_match_plot4m ,  nrow = 2)



### Female genetic matching balance plots 
gen_match_plot1f  <- bal.plot(mout.female, 'Grad', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor, data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot2f  <- bal.plot(mout.female, 'Prof', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot3f  <- bal.plot(mout.female, 'Trade', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

gen_match_plot4f  <- bal.plot(mout.female, 'Exper', 
                               formula = FemaleQueerLogical ~ Weekswrk + Hrswrk + 
                                 Nonwhite +  Hs + Assoc + Bach + Grad + 
                                 Exper + Exper_sq + Married + Metro + Midwest + South +
                                 West + Agri + Mine + Construct + Manu + Trans + Trade + Finance +
                                 Busi + Enter + Prof + Public + Service +
                                 Office + Natconst + Prodlabor,data = femaledata, which = "both",
                               sample.names = c("Unmatched", "Matched"))

grid.arrange(gen_match_plot1f , gen_match_plot2f , gen_match_plot3f , gen_match_plot4f ,  nrow = 2)

