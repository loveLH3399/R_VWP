### Example program Visual World Paradigm
### Based on Ivanova-Sullivan et al. (2022)
### Bulgarian and Russian
### Two objects shown: either same gender or different genders
### Children asked to look at target object either to the left or to the right
### Nouns are preceded by adjective which indicates the gender
### Correspondence between gender adjectives/nouns is either transparent or opaque
### Dependent variable = looks at the target object at the time when the adjective and the noun are shown
### Data given for 3 participants and 26 items (id)


# Load data and have a look at them
# Can be done via Import Dataset (right top)

library(readxl)
sample <- read_excel("sample.xlsx")
View(sample)
summary(sample)


# Make a file dataALL that is an R data frame and that only includes the important variables
dataALL <- data.frame(sample[,c("participant","id","TargetLooks","SEG3_onset","type","condition","exp")])
colnames(dataALL)
summary(dataALL)


# id is a nominal/categorical variable, but not read as a continuous variable; change this
dataALL $ id<- as.factor(dataALL $ id)
summary(dataALL)


#Filter only Exp data
data<- dplyr::filter(dataALL,  exp =="exp")  


# Install and activate GGPLOT
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggeffects")) install.packages("ggeffect")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggpubr")) install.packages("ggpubr")
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(ggpubr)


# Show a smoothed line plot for TargetLooks
ggplot(data, aes(y = TargetLooks, x = SEG3_onset, color = type, fill = type)) +
  # lines for proportions
  geom_smooth(span = .4, alpha = .4, method = "gam") +
  # add vertical line
  geom_vline(xintercept = 0, linetype="dotted", color = "darkgrey", linewidth=.75) +
  annotate("text", x=0, y=0.1, label="italic(SEG3_onset)", parse=TRUE, angle=90, colour="gray28", size=3)+
  # add vertical line
  # add text
  # separate panels for each condition
  facet_grid(~condition)


# Window selection between onset of adjective (-880 ms) and onset of silence (531 ms)
data1<- dplyr::filter(data, SEG3_onset >-880 & SEG3_onset  <531)  
write.csv(data1,"selected_data.csv")


# Make a new plot
ggplot(data1, aes(y = TargetLooks, x = SEG3_onset, color = type, fill = type)) +
  # lines for proportions
  geom_smooth(span = .4, alpha = .4, method = "gam") +
  # add vertical line
  geom_vline(xintercept = 0, linetype="dotted", color = "darkgrey", size=.75) +
  annotate("text", x=0, y=0.8, label="italic(Noun_onset)", parse=TRUE, angle=90, colour="gray28", size=3)+
  geom_vline(xintercept = 531, linetype="dotted", color = "darkgrey", size=.75) +
  annotate("text", x=531, y=0.8, label="italic(Silence_onset)", parse=TRUE, angle=90, colour="gray28", size=3)+
  geom_vline(xintercept = -882, linetype="dotted", color = "darkgrey", size=.75) +
  annotate("text", x=-882, y=0.8, label="italic(ADJ-Onset)", parse=TRUE, angle=90, colour="gray28", size=3)+
  # add vertical line
  # add text
  # separate panels for each condition
  facet_grid(~condition)


# Run linear mixed effects (lme) analysis and provide a nice table
if (!require("lme4")) install.packages("lme4")
if (!require("sjPlot")) install.packages("sjPlot")
library(lme4)
library(sjPlot)

# To run lme, important to work with centered variables!
data1$SEG3_onset_c <- scale(data1$SEG3_onset,scale=FALSE)[,1]
model1 <- lmer(TargetLooks ~ SEG3_onset_c* type*condition+ 1+ (1| participant)+ (1|id), data = data1, REML = T,
               contrasts = list(type = "contr.sum", condition = "contr.sum"))
tab_model(model1, show.stat = T) 
plot_model(model1, type = "pred", terms = c("SEG3_onset_c ", "type ", "condition "))

