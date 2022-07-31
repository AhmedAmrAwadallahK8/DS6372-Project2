#library(dplyr)
#library(magrittr)
#library(tidyverse)
#library(ggplot2)
library(aplore3)


#Change working directory to this source file directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Load my custom functions
source("Utility.r")

?glow_bonemed
?glow500
str(glow_bonemed)

#sub_id: Identification Code (1-n)
#site_id: Study Site (1-6J)
#phy_id: Physician ID code (128 unique codes)
#priorfrac: History of Prior Fracture (no, Yes)
#age: age at enrollment (Years)
#height: height at enrollment (centimeter)
#weight: Weight at enrollment (kg)
#bmi: body mass index (kg/m^2)
#premeno: Menopause before age 45 (no, Yes)
#momfrac: Mother had hip fracture (no, yes)
#armassist: Arms are needed to stand frm chair (no, yes)
#smoke: form or current smoker (no, yes)
#raterisk: self-reported risk of frac (Less than others of same, same as others of the same age, greater than others of the same age)
#fracscore: Fracture Risk Score (Composite Risk Score)
#bonemed: bone medications at enroll (no, yes)
#bonemed_fu: bone meds at follow up (no, yes)
#bonetreat: bone med both at enroll and follow (no, yes)
#fracture: any fracture in first year (no, yes) TARGET



#Na Check
percent_na_plot(glow_bonemed)
# No NA values nice

#sub_id: There shouldnt be any relationship here but checking regardless
cts_categ_compare(glow_bonemed, "sub_id", "fracture")
#There is non random behavior but we should NOT use this variable

#site_id: Same as before there shouldn't be any relationship
cts_categ_compare(glow_bonemed, "site_id", "fracture")
#Difference in medians

#phy_id:
cts_categ_compare(glow_bonemed, "phy_id", "fracture")
#No significant difference in behavior

#priorfrac:
categ_categ_compare(glow_bonemed, "priorfrac", "fracture")
#Some difference

#age
cts_categ_compare(glow_bonemed, "age", "fracture")
#Some Difference

#weight
cts_categ_compare(glow_bonemed, "weight", "fracture")
#No significant visual difference

#height
cts_categ_compare(glow_bonemed, "height", "fracture")
#No significant visual difference

#bmi
cts_categ_compare(glow_bonemed, "bmi", "fracture")
#No significant visual difference

#premeno
categ_categ_compare(glow_bonemed, "premeno", "fracture")
#No significant visual difference

#momfrac
categ_categ_compare(glow_bonemed, "momfrac", "fracture")
#Some visual difference

#armassist
categ_categ_compare(glow_bonemed, "armassist", "fracture")
#Some visual difference

#smoke
categ_categ_compare(glow_bonemed, "smoke", "fracture")
#slight visual difference

#raterisk
categ_categ_compare(glow_bonemed, "raterisk", "fracture")
#Decent visual trend

#fracscore
cts_categ_compare(glow_bonemed, "fracscore", "fracture")
#Moderate visual Difference

#bonemed
categ_categ_compare(glow_bonemed, "bonemed", "fracture")
#Moderate visual difference

#bonemed_fu
categ_categ_compare(glow_bonemed, "bonemed_fu", "fracture")
#Moderate visual difference

#bonetreat
categ_categ_compare(glow_bonemed, "bonetreat", "fracture")
#Moderate Visual difference

#Variables to Consider
  #priorfrac
  #age
  #momfrac
  #armassist
  #smoke: minor
  #raterisk
  #fracscore
  #Probably correlated
    #bonemed
    #bonemed_fu
    #bonetreat

#Very Slight Correlation
  #height

#Target Variable
  #fracture

#Check if dataset is balanced
glow_bonemed %>% ggplot(aes(x=fracture)) + geom_bar()

#dataset is imbalanced

cts_categ_compare(glow_bonemed, "bmi", "bonemed")

#QDA/LDA Analysis
str(glow_bonemed)
cts_vars_and_target = c("phy_id", "age", "weight", "height", "bmi", "fracscore", "fracture")
glow_bonemed_cts_df = glow_bonemed %>% select(cts_vars_and_target)
str(glow_bonemed_cts_df)

ggpairs(glow_bonemed_cts_df, columns=1:6, aes(colour=fracture))

ggpairs(glow_bonemed_cts_df, columns=2:5, aes(colour=fracture))
#Doesnt appear like any variable as a clean linear distinction bounday. Going to consider QDA as my model of choice
#ggpairs(glow_bonemed,columns = c("momfrac", "armassist", "smoke", "raterisk", "fracscore", "bonemed", "bonemed_fu", "bonetreat"),
#        columnLabels = c("momfrac", "armassist", "smoke", "raterisk", "fracscore", "bonemed", "bonemed_fu", "bonetreat"),aes(colour=fracture))



library(GGally)
variablesToSelect = c("priorfrac", "age", "momfrac", "armassist", "smoke", "raterisk", "fracscore", "bonemed", "bonemed_fu", "bonetreat", "height", "fracture")

glow_bonemed_subset = glow_bonemed %>% select(variablesToSelect)


#ggpairs(glow_bonemed,columns = c("momfrac", "armassist", "smoke", "raterisk", "fracscore", "bonemed", "bonemed_fu", "bonetreat"),
#        columnLabels = c("momfrac", "armassist", "smoke", "raterisk", "fracscore", "bonemed", "bonemed_fu", "bonetreat"),aes(colour=fracture))


pair_plot = ggpairs(glow_bonemed_subset, columns=1:11)
str(pair_plot[1,1])
ggpair_subset(pair_plot, 1, 2)

test = glow_bonemed %>% ggplot(aes(x=fracture)) + geom_bar()
str(test)
test
multiplot(pair_plot[1,1])
