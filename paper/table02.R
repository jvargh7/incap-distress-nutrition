


vars <- c("gtatole*gtchatoleexposurestatus","byear","pccs6775","scale(moht_imputed)","moscho_imputed",
          "eduyr","gtadrelstatyn","pccs1618","score_change_ge300","urbano_rural2018")

output_b = data.frame(sex = character(),
                      iv = character(),
                      OR = character())
# LOGIT -----------
for (v in vars){
  formula_v <- paste0("gtadseverementaldistress ~ ",v,"+ (1|d_id_unim)")
  print(v)
  logit_b_f <- glmer(as.formula(formula_v),
                     family = binomial(),
                     # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                     # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                     control=glmerControl(optimizer ="nloptwrap2"),
                     data = paper_srq20 %>% 
                       dplyr::filter(chsex=="female",!is.na(score_change_ge300)))
  print("F done")
  logit_b_m <- glmer(as.formula(formula_v),
                     family = binomial(),
                     # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                     # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                     control=glmerControl(optimizer ="nloptwrap2"),
                     data = paper_srq20 %>% 
                       dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
  print("M done")
  formula_p <- paste0("gtadseverementaldistress ~ ",v,"+ chsex + (1|d_id_unim)")
  logit_b_p <- glmer(as.formula(formula_p),
                     family = binomial(),
                     glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                     # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                     # control=glmerControl(optimizer ="nloptwrap2"),
                     data = paper_srq20 %>% 
                       dplyr::filter(!is.na(score_change_ge300)))
  print("P done")
  output_b = bind_rows(output_b,
                       clean_glm_result(logit_b_f,link = "glmer logit") %>% 
                         mutate(sex = "Female"),
                       clean_glm_result(logit_b_m,link = "glmer logit") %>% 
                         mutate(sex = "Male"),
                       clean_glm_result(logit_b_p,link = "glmer logit") %>% 
                         mutate(sex = "Pooled")
  )
  
}


output_b %>% 
  dplyr::filter(!iv %in% c("chsexfemale","gtatole","gtchatoleexposurestatuspartial","gtchatoleexposurestatusfull")) %>% 
  dplyr::select(sex,iv,OR) %>% 
  mutate(iv = case_when(iv == "gtatole" ~ "ATOLE VILLAGE",
                        iv == "byear" ~ "BIRTH YEAR - 1962",
                        iv == "chsexfemale" ~ "SEX (= FEMALE)",
                        iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL",
                        iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL",
                        iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                        iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                        iv == "pccs6775" ~ "CHILD SOCIAL CLASS (z-scores)",
                        iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (IMPUTED, relative z-scores)",
                        iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (IMPUTED)",
                        iv == "score_change_ge300yes" ~ "(HOLMS RAHE STRESS >= 300)",
                        iv == "scale(gtadeduyr1618)" ~ "COMPLETED YEARS OF SCHOOLING (z-scores)",
                        iv %in% c("gtadeduyr1618","eduyr") ~ "COMPLETED YEARS OF SCHOOLING",
                        iv == "pccs1618" ~ "ADULT SOCIAL CLASS (z-scores)",
                        iv == "gtadrelstatyn" ~ "MARITAL STATUS (Married or In Relationship)",
                        iv == "urbano_rural2018" ~ "RURAL 2018",
                        TRUE ~ NA_character_
  )) %>% 
  pivot_wider(names_from="sex",values_from = "OR") %>% 
  knitr::kable(format="markdown")


# ZERO-INFLATED POISSON ---------

output_b = data.frame(sex = character(),
                      iv = character(),
                      OR = character())

for (v in vars){
  formula_v <- paste0("adsrq ~ ",v,"+ (1|d_id_unim)")
  zinfglmm_pois_b_f <- glmmTMB(as.formula(formula_v),
                               ziformula = ~1,
                               family="poisson",
                               data=paper_srq20 %>% 
                                 dplyr::filter(chsex=="female",!is.na(score_change_ge300)))
  zinfglmm_pois_b_m <- glmmTMB(as.formula(formula_v),
                               ziformula = ~1,
                               family="poisson",
                               data=paper_srq20 %>% 
                                 dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
  
  formula_p <- paste0("adsrq ~ ",v,"+ chsex + (1|d_id_unim)")
  zinfglmm_pois_b_p <- glmmTMB(as.formula(formula_p),
                               ziformula = ~1,
                               family="poisson",
                               data=paper_srq20 %>% 
                                 dplyr::filter(!is.na(score_change_ge300)))
  
  output_b = bind_rows(output_b,
                       clean_glm_result(zinfglmm_pois_b_f,link = "zinf glmmTMB log") %>% 
                         mutate(sex = "Female"),
                       clean_glm_result(zinfglmm_pois_b_m,link = "zinf glmmTMB log") %>% 
                         mutate(sex = "Male"),
                       clean_glm_result(zinfglmm_pois_b_p,link = "zinf glmmTMB log") %>% 
                         mutate(sex = "Pooled")
  )
  
}


output_b %>% 
  dplyr::filter(!iv %in% c("chsexfemale","gtatole","gtchatoleexposurestatuspartial","gtchatoleexposurestatusfull")) %>% 
  dplyr::select(sex,iv,RR) %>% 
  mutate(iv = case_when(iv == "gtatole" ~ "ATOLE VILLAGE",
                        iv == "byear" ~ "BIRTH YEAR - 1962",
                        iv == "chsexfemale" ~ "SEX (= FEMALE)",
                        iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL",
                        iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL",
                        iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                        iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                        iv == "pccs6775" ~ "CHILD SOCIAL CLASS (z-scores)",
                        iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (IMPUTED, relative z-scores)",
                        iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (IMPUTED)",
                        iv == "score_change_ge300yes" ~ "(HOLMS RAHE STRESS >= 300)",
                        iv == "scale(gtadeduyr1618)" ~ "COMPLETED YEARS OF SCHOOLING (z-scores)",
                        iv %in% c("gtadeduyr1618","eduyr") ~ "COMPLETED YEARS OF SCHOOLING",
                        iv == "pccs1618" ~ "ADULT SOCIAL CLASS (z-scores)",
                        iv == "gtadrelstatyn" ~ "MARITAL STATUS (Married or In Relationship)",
                        iv == "urbano_rural2018" ~ "RURAL 2018",
                        TRUE ~ NA_character_
  )) %>% 
  pivot_wider(names_from="sex",values_from = "RR") %>% 
  knitr::kable(format="markdown")


