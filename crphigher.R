
library(janitor)
crp = read_csv('mc.csv')

crp = crp %>% clean_names()

crp %>% 
  mutate()

crp=crp %>% mutate(ASCVD=case_when(
  sex=="M"~ascvdm(age=age,numTC=t_cholestrol,hdl=hdl,numBP=sbp,smokc=0,diab=1,rxbp=0),
  sex=="F"~ascvdf(age=age,numTC=t_cholestrol,hdl=hdl,numBP=sbp,smokc=0,diab=1,rxbp=0))) 

glimpse(crp)

library(magrittr)

crp %$% cor.test(average_cimt_mm,hs_crp_mg_l)

crp %$% cor.test(average_cimt_mm,ASCVD)

skim(crp)

devtools::install_github("PHP2560-Statistical-Programming-R/r-framingham")
  

library(frisk)

gg=calc_card_10_one(age=45, gender="F", cholesterol=89,bmi=173,
                 hdl=89, sbp=65, is_sbp_under_treatment=TRUE,
                 smoking_status=F, diabetes_status=T
)

df <- data.frame(age=sample(30:70,100,rep=TRUE),
                 gender=sample(c("M","F"),100,rep=TRUE),
                 bmi=sample(16:48,100, rep = TRUE),
                 hdl=sample(10:100,100,rep=TRUE),
                 chl=sample(100:400,100,rep=TRUE),
                 sbp=sample(90:200,100,rep=TRUE),
                 isSbpTreated=sample(c(TRUE,FALSE),100,rep=TRUE),
                 smoking=sample(c(TRUE,FALSE),100,rep=TRUE),
                 diabetes=sample(c(TRUE,FALSE),100,rep=TRUE)
)

##create df from crp
df= tibble(
  age=crp$age,gender=crp$sex,hdl=crp$hdl,chl=crp$t_cholestrol,
  sbp=crp$sbp,isSbpTreated=FALSE,smoking=FALSE,diabetes=TRUE)
)
# call frisk function for ddf

ddf=calc_card_10(df, age="age", gender="gender", cholesterol="chl",
             hdl="hdl", sbp="sbp", is_sbp_under_treatment="isSbpTreated",
             smoking_status="smoking", diabetes_status="diabetes")

##ddf 

ddf= ddf %>% mutate(risk=as.numeric(risk)) %>% rename(risk1=risk) %>% 
  
  rename(risk=points) %>% 
  mutate(pointser= case_when(
    risk>= 1&risk<= 4 ~1,
    
    risk >= 5 & risk <= 6~2,
    risk == 7~3,
    risk == 8~4,
    risk == 9~5,
    risk == 10~6,
    risk == 11~8,
    risk == 12~10,
    risk == 13~12,
    risk == 14~16,
    risk == 15~20,
    risk == 16 ~25,
    risk>=17 ~30)) 


###
crp$framigham_score= as.numeric(ddf$pointser)

crp %$% cor.test(hs_crp_mg_l,framigham_score)
