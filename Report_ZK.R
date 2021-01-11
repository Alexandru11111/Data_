library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse)
library(gridExtra)
library(lm.beta)

require(mediation)
library(car)
# ## Custom functions	

# We will use these custom functions to get bootstrapped confidence intervals.	


# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

# function to obtain adjusted R^2	
# source: https://www.statmethods.net/advstats/bootstrapping.html (partially modified)	
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	


# Computing the booststrap BCa (bias-corrected and accelerated) bootstrap confidence intervals by Elfron (1987)	
# This is useful if there is bias or skew in the residuals.	

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	



View(Data_utan_bortfall_250_deltagare)
summary(Data_utan_bortfall_250_deltagare)
describe(Data_utan_bortfall_250_deltagare)

is.factor(Data_utan_bortfall_250_deltagare$Group) 

#Must be done 
Data_utan_bortfall_250_deltagare$Group <- ordered(Data_utan_bortfall_250_deltagare$Group,
                                                  levels = c("1", "2", "3"))

levels(Data_utan_bortfall_250_deltagare$Group)



Mod_1 = lm(Communal ~ Kön + gillameditation, data = Data_utan_bortfall_250_deltagare)
summary(Mod_1) 
AIC(Mod_1)

mod_grp = lm(Communal ~ Group, data =Data_utan_bortfall_250_deltagare)
summary(mod_grp)
AIC(mod_grp)

Anova()


res.man <- manova(cbind(Kön, Group) ~ Communal, data = Data_utan_bortfall_250_deltagare) #is this correct? No only 1 DF should be 2 
summary(res.man)
summary.aov(res.man)

res.man2 <- manova(cbind(Communal, Kön) ~ Group, data = Data_utan_bortfall_250_deltagare) #I think this is correct 
summary(res.man2)
summary.aov(res.man2)


one.way <- aov(Communal ~ Group, data = Data_utan_bortfall_250_deltagare)

summary(one.way)
?aov

Data_utan_bortfall_250_deltagare %>%
set.seed(180)
dplyr::sample_n(Data_utan_bortfall_250_deltagare, 10)



Data_utan_bortfall_250_deltagare$Group <- ordered(Data_utan_bortfall_250_deltagare$Group,
                      levels = c("1", "2", "3"))

levels(Data_utan_bortfall_250_deltagare$Group)


install.packages("ggpubr")
install.packages("devtools")
require("ggpubr")
library("ggpubr")
library(car)

#This is to just show the results in visuals. 
ggboxplot(Data_utan_bortfall_250_deltagare, x = "Group", y = "Communal", 
         color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         order = c("1", "2", "3"),
         ylab = "Communal", xlab = "Group") # Here you can see that grp1 treatment is showing higher communal golas then grp 3



my_data <- na.omit(Data_utan_bortfall_250_deltagare)

 mean_plot <- ggline(Data_utan_bortfall_250_deltagare, x = "Group", y = "Communal", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3"),
       ylab = "Communal", xlab = "Group") 
 
 
 boxplot(Communal ~ Group, data = Data_utan_bortfall_250_deltagare,
         xlab = "Group", ylab = "Communal",
         frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07")) 
 
 levels(Data_utan_bortfall_250_deltagare$Group)
 
 aov1 <- aov(Communal ~ Group, data = Data_utan_bortfall_250_deltagare) # This works How to calculate η2? Look this up.
 summary(aov1)
 etaSquared(aov1) # why does this not work?=
EtaSq(aov1, type = 2, anova = FALSE)
eta_sq(aov1)

TukeyHSD(aov1) # performing multiple pairwise-comparison between the means of groups. You see the sig is on grp 3vs1

                                                  "Skip until line 259!"

"An Analysis of Covariance, ANCOVA, since the idea is that i now also include SCS meassures (self-compassion)" 

require("car")
library(car)
library(psych)
library(ggplot2)

leveneTest(aov1) #checking assumptions homogenity of variance 

describeBy(Data_utan_bortfall_250_deltagare$Communal, Data_utan_bortfall_250_deltagare$Group) 

ggplot(Data_utan_bortfall_250_deltagare,aes(y=Communal, x=Group, fill=Group))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8) # sad :P 


aov2 <- aov(Communal ~ Group, data = Data_utan_bortfall_250_deltagare) # type 3 anova, the correct anova to run 
Anova(aov2, type="III") # typ 3 error 
summary.lm(aov2)

"The best design for your prob is a 2x2 factor design"
#Go for the 2 x 2 factor design insted of lm if you dont grasp it # Mby make it bigger with gender? In LM Gener was a sig ????

MANCOVA1<- aov(Communal ~ Group + Self-compassion + Group:Self-compssion, data=dataset)" THISwont run since you dont have the variable, this is for future anaysis"

"Does this work to make 2 dependent variable + adding gender, ask zoltan"

Y <- cbind(Communal,Self-compassion)
fit <- manova(Y ~ Group*Sex)



#for plotting check https://www.statmethods.net/stats/anova.html


"mediationanalysis on LM"
install.packages("mediation")



Mod_1 = lm(Communal ~ Kön + gillameditation + Group, data = Data_utan_bortfall_250_deltagare)
summary(Mod_1) #0.14 
AIC(Mod_1)

Mod_2 = lm(Communal ~ Kön + gillameditation + Self-compassion, data = Data_utan_bortfall_250_deltagare) "This dont work, dont have the variable self-compassion"

#https://cran.r-project.org/web/packages/mediation/vignettes/mediation.pdf
# Use mediat function to estmiate ACME and ADE with function med.fit and med.out
med.fit <- lm(Communal ~ Kön + Group + gillameditation + self-compassion, data = Data_utan_bortfall_250_deltagare) "This dont work, dont have the variable self-compassion"
med.out <- glm(self-compassion ~ Kön + Group + gillameditation + Communal, data = Data_utan_bortfall_250_deltagare) "This dont work, dont have the variable self-compassion"

med.out <- mediate(med.fit, out.fit, treat = "Group", mediatior = "Self-compassion", robustSE = TRUE, sims = 1000)
summary(med.out)


#Group-level treatment and individual-level mediator 
med.fit <- glmer(Communal ~ Self-compassion + ålder + g + pared + (1|SCH_ID),
                 + family = binomial(link = "logit"), data = student
                 
                 
                 "Mediation using Lavaan"
                 
                 install.packages("lavaan")
                 library(lavaan)
                 library(tidyverse)
                 library(knitr)
                 library(lavaan)
                 library(psych)
                 library(MBESS)
                 #With the test.dataset
                 thirst_dat <- "data/mackinnon_2008_t3.1.csv"%>% read_csv()
                 
                 
                 #"here is want Group to be the diffrent treatments, can it be written like this? 
                 #http://web.pdx.edu/~newsomj/semclass/ho_mediation.pdf How to test mediation structural equation model SEM. 
                 #https://www.statisticshowto.com/mediator-variable/ 
                 Mod_med1 = lm(Self-compassion ~ Group, data = Data_utan_bortfall_250_deltagare)
                 Mod_med2 = lm(Communal ~ Self-compassion + Group, data = Data_utan_bortfall_250_deltagare)
                 
                 
                 
                 
                 library(dagitty)
                 
dag_6.3 <- dagitty( "dag {
G -> K
K -> C
G -> C 
}")
plot(dag_6.3)
                
                
                modz.m1 <- ' Kön ~ Group
                             Communal ~ Kön + Group'
                 
                 
 
                fit <- sem(modz.m1, data= Data_utan_bortfall_250_deltagare)    # why does this not working anymore?  
                 
                 
                 summary(fit) #non std coeff
                 summary(fit, standardized=T, fit.measures = T, rsq = T)
                 
                 
               #Leta efter indirekt, direkt och total effekt  
                 
                 modz.m2 <- ' Kön ~ a*gillameditation
                               Communal ~ b*Kön + c*gillameditation 
                               
                 indirect := a*b               
                 direct := c
                 total := c* (a*b)'
                 
        fit.2 <- sem(modz.m2, data = Data_utan_bortfall_250_deltagare)          
                 
                 
                 
                 n
summary(fit.2, standardized=T, fit.measures = T, rsq = T) # b and c is significant, no mediation effect 
                 
                 
                 
                 
                 
                 SK#resampling method: precentile bootstrapping


                 
modz.m3 <- ' Kön ~ a*gillameditation
                               Communal ~ b*Kön + c*gillameditation 
                               
                 indirect := a*b               
                 direct := c
                 total := c* (a*b)'

set.seed(230)


   fitmod3 <- sem(modz.m2, data=Data_utan_bortfall_250_deltagare, se="bootstrap", bootstrap=200)              
                 
                 
     parameterEstimates(fitmod2, ci=TRUE, level=0.95, boot.ci.type="perc")            
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 "Notes from meeting - 01-11"
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 