
# MODEL 1 --------------

formula_v1 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + byear + pccs6775  + moscho_imputed + scale(moht_imputed)  + (1|d_id_unim)"
formula_p1 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775  + moscho_imputed + scale(moht_imputed)  + (1|d_id_unim)"

logit_m1_f <- glmer(as.formula(formula_v1),
                    family = binomial(),
                    # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="female",!is.na(score_change_ge300),!is.na(gtadeduyr1618)))
logit_m1_m <- glmer(as.formula(formula_v1),
                    family = binomial(),
                    # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="male",!is.na(score_change_ge300),!is.na(gtadeduyr1618)))
logit_m1_p <- glmer(as.formula(formula_p1),
                    family = binomial(),
                    glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    # control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(!is.na(score_change_ge300),!is.na(gtadeduyr1618)))

bind_rows(clean_glm_result(logit_m1_f,link = "glmer logit") %>% 
            mutate(sex = "Female"),
          clean_glm_result(logit_m1_m,link = "glmer logit") %>% 
            mutate(sex = "Male"),
          clean_glm_result(logit_m1_p,link = "glmer logit") %>% 
            mutate(sex = "Pooled"),
          
) %>% 
  dplyr::select(sex,iv,OR) %>% 
  mutate(iv = case_when(iv == "gtatole" ~ "ATOLE (PERIOD = NONE)",
                        iv == "byear" ~ "BIRTH YEAR - 1962",
                        iv == "chsexfemale" ~ "SEX (= FEMALE)",
                        iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                        iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                        iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                        iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                        iv == "pccs6775" ~ "CHILD SOCIAL CLASS (z-scores)",
                        iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (STATA IMPUTED, relative z-scores)",
                        iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (STATA IMPUTED)",
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


# MODEL 2: + Adult SES ------------

formula_v2 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + (1|d_id_unim)"
formula_p2 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + (1|d_id_unim)"

logit_m2_f <- glmer(as.formula(formula_v2),
                    family = binomial(),
                    # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="female",!is.na(score_change_ge300)))
logit_m2_m <- glmer(as.formula(formula_v2),
                    family = binomial(),
                    glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    # control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
logit_m2_p <- glmer(as.formula(formula_p2),
                    family = binomial(),
                    glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    # control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(!is.na(score_change_ge300)))

bind_rows(clean_glm_result(logit_m2_f,link = "glmer logit") %>% 
            mutate(sex = "Female"),
          clean_glm_result(logit_m2_m,link = "glmer logit") %>% 
            mutate(sex = "Male"),
          clean_glm_result(logit_m2_p,link = "glmer logit") %>% 
            mutate(sex = "Pooled"),
          
) %>% 
  dplyr::select(sex,iv,OR) %>% 
  mutate(iv = case_when(iv == "gtatole" ~ "ATOLE (PERIOD = NONE)",
                        iv == "byear" ~ "BIRTH YEAR - 1962",
                        iv == "chsexfemale" ~ "SEX (= FEMALE)",
                        iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                        iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                        iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                        iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                        iv == "pccs6775" ~ "CHILD SOCIAL CLASS (z-scores)",
                        iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (STATA IMPUTED, relative z-scores)",
                        iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (STATA IMPUTED)",
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


# MODEL 3: + HRSTRESS >= 300 ------------

formula_v3 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618  + gtadrelstatyn + score_change_ge300 + urbano_rural2018 + (1|d_id_unim)"

formula_p3 = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + score_change_ge300 + urbano_rural2018 + (1|d_id_unim)"

logit_m3_f <- glmer(as.formula(formula_v3),
                    family = binomial(),
                    # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="female",!is.na(score_change_ge300)))
logit_m3_m <- glmer(as.formula(formula_v3),
                    family = binomial(),
                    glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    # control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
logit_m3_p <- glmer(as.formula(formula_p3),
                    family = binomial(),
                    # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                    # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                    control=glmerControl(optimizer ="nloptwrap2"),
                    data = paper_srq20 %>% 
                      dplyr::filter(!is.na(score_change_ge300)))

bind_rows(clean_glm_result(logit_m3_f,link = "glmer logit") %>% 
            mutate(sex = "Female"),
          clean_glm_result(logit_m3_m,link = "glmer logit") %>% 
            mutate(sex = "Male"),
          clean_glm_result(logit_m3_p,link = "glmer logit") %>% 
            mutate(sex = "Pooled"),
          
) %>% 
  dplyr::select(sex,iv,OR) %>% 
  mutate(iv = case_when(iv == "gtatole" ~ "ATOLE (PERIOD = NONE)",
                        iv == "byear" ~ "BIRTH YEAR - 1962",
                        iv == "chsexfemale" ~ "SEX (= FEMALE)",
                        iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                        iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                        iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                        iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                        iv == "pccs6775" ~ "CHILD SOCIAL CLASS (z-scores)",
                        iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (STATA IMPUTED, relative z-scores)",
                        iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (STATA IMPUTED)",
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

