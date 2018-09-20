points.men <- function(age, totchol, smoker, HDLChol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age == 1) points = points - 9
  else if(age == 2) points = points - 4
  else if(age == 4) points = points + 3
  else if(age == 5) points = points + 6
  else if(age == 6) points = points + 8
  else if(age == 7) points = points + 10
  else if(age == 8) points = points + 11
  else if(age == 9) points = points + 12
  else if(age == 10) points = points + 13
  
  # Total cholesterol points
  if(age == 1 | age == 2)
  {
    # Age 20–39 years
    if(totchol == 2) points = points + 4
    else if(totchol == 3) points = points + 7
    else if(totchol == 4) points = points + 9
    else if(totchol == 5) points = points + 11
  } else if(age == 3 | age == 4)
  {
    # Age 40–49 years
    if(totchol == 2) points = points + 3
    else if(totchol == 3) points = points + 5
    else if(totchol == 4) points = points + 6
    else if(totchol == 5) points = points + 8
  } else if(age == 5 | age == 6)
  {
    # Age 50–59 years
    if(totchol == 2) points = points + 2
    else if(totchol == 3) points = points + 3
    else if(totchol == 4) points = points + 4
    else if(totchol == 5) points = points + 5
  } else if(age == 7 | age == 8)
  {
    # Age 60–69 years
    if(totchol == 2) points = points + 1
    else if(totchol == 3) points = points + 1
    else if(totchol == 4) points = points + 2
    else if(totchol == 5) points = points + 3
  } else if(age == 9 | age == 10)
  {
    # Age 70–79 years
    if(totchol == 4) points = points + 1
    else if(totchol == 5) points = points + 1
  } 
  
  # Cigarette smoker points
  if(smoker=="smoker")
  {
    if(age == 1 | age == 2) points = points + 8
    else if(age == 3 | age == 4) points = points + 5
    else if(age == 5 | age == 6) points = points + 3
    else if(age == 7 | age == 8) points = points + 1
    else if(age == 9 | age == 10) points = points + 1
  }
  
  # HDL cholesterol points
  if(HDLChol == 1) points = points - 1
  else if(HDLChol == 3) points = points + 1
  else if(HDLChol == 4) points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP == 2) points = points + 1
    else if(SBP == 3) points = points + 2
    else if(SBP == 4) points = points + 2
    else if(SBP == 5) points = points + 3
  } else if(treated == 'No')
  {
    if(SBP == 3) points = points + 1
    else if(SBP == 4) points = points + 1
    else if(SBP == 5) points = points + 2
  }
  
  return(points)
}

points.women <- function(age, totchol, smoker, HDLChol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age == 1) points = points - 7
  else if(age == 2) points = points - 3
  else if(age == 4) points = points + 3
  else if(age == 5) points = points + 6
  else if(age == 6) points = points + 8
  else if(age == 7) points = points + 10
  else if(age == 8) points = points + 12
  else if(age == 9) points = points + 14
  else if(age == 10) points = points + 16
  
  # Total cholesterol points
  if(age == 1 | age == 2)
  {
    # Age 20–39 years
    if(totchol == 2) points = points + 4
    else if(totchol == 3) points = points + 8
    else if(totchol == 4) points = points + 11
    else if(totchol == 5) points = points + 13
  } else if(age == 3 | age == 4)
  {
    # Age 40–49 years
    if(totchol == 2) points = points + 3
    else if(totchol == 3) points = points + 6
    else if(totchol == 4) points = points + 8
    else if(totchol == 5) points = points + 10
  } else if(age == 5 | age == 6)
  {
    # Age 50–59 years
    if(totchol == 2) points = points + 2
    else if(totchol == 3) points = points + 4
    else if(totchol == 4) points = points + 5
    else if(totchol == 5) points = points + 7
  } else if(age == 7 | age == 8)
  {
    # Age 60–69 years
    if(totchol == 2) points = points + 1
    else if(totchol == 3) points = points + 2
    else if(totchol == 4) points = points + 3
    else if(totchol == 5) points = points + 4
  } else if(age == 9 | age == 10)
  {
    # Age 70–79 years
    if(totchol == 2) points = points + 1
    else if(totchol == 3) points = points + 1
    else if(totchol == 4) points = points + 2
    else if(totchol == 5) points = points + 2
  } 
  
  # Cigarette smoker points
  if(smoker == 1)
  {
    if(age == 1 | age == 2) points = points + 9
    else if(age == 3 | age == 4) points = points + 7
    else if(age == 5 | age == 6) points = points + 4
    else if(age == 7 | age == 8) points = points + 2
    else if(age == 9 | age == 10) points = points + 1
  }
  
  # HDL cholesterol points
  if(HDLChol == 1) points = points - 1
  else if(HDLChol == 3) points = points + 1
  else if(HDLChol == 4) points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP == 2) points = points + 3
    else if(SBP == 3) points = points + 4
    else if(SBP == 4) points = points + 5
    else if(SBP == 5) points = points + 6
  } else if(treated == 'No')
  {
    if(SBP == 2) points = points + 1
    else if(SBP == 3) points = points + 2
    else if(SBP == 4) points = points + 3
    else if(SBP == 5) points = points + 4
  }
  
  return(points)
}

crp %>%  mutate( age1 = case_when(
  age <35 ~ 1,
  age<40~ 2,
  age<45~ 3,
  age<50~ 4,
  age<55~ 5,
  age<60~ 6,
  age<65~ 7,
  age<70~ 8,
  age<75~ 9,
  TRUE ~ 10),
  sbp1 = case_when(
    sbp <120 ~ 1,
    sbp <130 ~ 2,
    sbp <140 ~ 3,
    sbp <160 ~ 4,
    TRUE ~ 5),
  tchol1 = case_when(
    t_cholestrol <160 ~ 1,
    t_cholestrol <200 ~ 2,
    t_cholestrol <240 ~ 3,
    t_cholestrol <280 ~ 4,
    TRUE~ 5),
  hdl1 = case_when(
    hdl <40 ~ 4,
    hdl <50 ~ 3,
    hdl <60 ~ 2,
    TRUE ~ 1)) %>% 
  mutate( points1 = case_when(
    sex=="M" ~ points.men(age=age1,totchol = tchol1,smoker =0,HDLChol = hdl1,treated = 'No',SBP = sbp1),
    sex=="F" ~ points.women(age=age1,totchol = tchol1,smoker =0,HDLChol = hdl1,treated = 'No',SBP = sbp1)
  )) %>% View()
  
  
  
  
  points.men(age=2,totchol=2,SBP=3,smoker="smoker",,hdl1=3,smoker=1,treated='No')
  

  
  ##############
  
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
  