# Data_


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
#Load the data



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




res.man2 <- manova(cbind(Communal, Kön) ~ Group, data = Data_utan_bortfall_250_deltagare)
summary(res.man2)
summary.aov(res.man2)




install.packages("ggpubr")
install.packages("devtools")
require("ggpubr")
library("ggpubr")
library(car)

ggboxplot(Data_utan_bortfall_250_deltagare, x = "Group", y = "Communal", 
         color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         order = c("1", "2", "3"),
         ylab = "Communal", xlab = "Group")




 mean_plot <- ggline(Data_utan_bortfall_250_deltagare, x = "Group", y = "Communal", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3"),
       ylab = "Communal", xlab = "Group") 



 aov1 <- aov(Communal ~ Group, data = Data_utan_bortfall_250_deltagare) # This works How to calculate η2?
 summary(aov1)
 etaSquared(aov1) # why does this not work?=
EtaSq(aov1, type = 2, anova = FALSE)
eta_sq(aov1)

TukeyHSD(aov1)

"Skip until line 259!"

"An Analysis of Covariance, ANCOVA, since the idea is that i now also include SCS meassures (self-compassion)


                  library(lavaan)
                 library(tidyverse)
                 library(knitr)
                 library(lavaan)
                 library(psych)
                 library(MBESS)


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
                 
                 




