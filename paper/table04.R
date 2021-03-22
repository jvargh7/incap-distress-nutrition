
# MODEL 1 -------------
formula_pois_v1 <- "adsrq~gtatole*gtchatoleexposurestatus + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + (1|d_id_unim)"
formula_pois_p1 <- "adsrq~gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + (1|d_id_unim)"

zinfglmm_pois_f1 <- glmmTMB(as.formula(formula_pois_v1),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="female",!is.na(score_change_ge300),!is.na(gtadeduyr1618)))

zinfglmm_pois_m1 <- glmmTMB(as.formula(formula_pois_v1),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="male",!is.na(score_change_ge300),!is.na(gtadeduyr1618)))
zinfglmm_pois_p1 <- glmmTMB(as.formula(formula_pois_p1),
                            ziformula = ~1+chsex,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(!is.na(score_change_ge300),!is.na(gtadeduyr1618)))


bind_rows(clean_glm_result(zinfglmm_pois_f1,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Female"),
          clean_glm_result(zinfglmm_pois_m1,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Male"),
          clean_glm_result(zinfglmm_pois_p1,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Pooled")
) %>% 
  dplyr::filter(type !="Zero Inflation") %>%  
  dplyr::select(sex,iv,RR) %>% 
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
  pivot_wider(names_from="sex",values_from = "RR") %>% 
  knitr::kable(format="markdown")


# MODEL 2: + Adult SES --------------

formula_pois_v2 <- "adsrq~gtatole*gtchatoleexposurestatus + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + (1|d_id_unim)"
formula_pois_p2 <- "adsrq~gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + (1|d_id_unim)"

zinfglmm_pois_f2 <- glmmTMB(as.formula(formula_pois_v2),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="female",!is.na(score_change_ge300)))

zinfglmm_pois_m2 <- glmmTMB(as.formula(formula_pois_v2),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
zinfglmm_pois_p2 <- glmmTMB(as.formula(formula_pois_p2),
                            ziformula = ~1+chsex,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(!is.na(score_change_ge300)))


bind_rows(clean_glm_result(zinfglmm_pois_f2,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Female"),
          clean_glm_result(zinfglmm_pois_m2,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Male"),
          clean_glm_result(zinfglmm_pois_p2,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Pooled")
) %>% 
  dplyr::filter(type !="Zero Inflation") %>% 
  dplyr::select(sex,iv,RR) %>% 
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
  pivot_wider(names_from="sex",values_from = "RR") %>% 
  knitr::kable(format="markdown")

# MODEL 3 : + HRSTRESS >= 300 ----------

formula_pois_v3 <- "adsrq~gtatole*gtchatoleexposurestatus + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + score_change_ge300 + (1|d_id_unim)"
formula_pois_p3 <- "adsrq~gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + score_change_ge300 + (1|d_id_unim)"

zinfglmm_pois_f3 <- glmmTMB(as.formula(formula_pois_v3),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="female",!is.na(score_change_ge300)))

zinfglmm_pois_m3 <- glmmTMB(as.formula(formula_pois_v3),
                            ziformula = ~1,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(chsex=="male",!is.na(score_change_ge300)))
zinfglmm_pois_p3 <- glmmTMB(as.formula(formula_pois_p3),
                            ziformula = ~1+chsex,
                            family="poisson",
                            data=paper_srq20 %>% 
                              dplyr::filter(!is.na(score_change_ge300)))


bind_rows(clean_glm_result(zinfglmm_pois_f3,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Female"),
          clean_glm_result(zinfglmm_pois_m3,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Male"),
          clean_glm_result(zinfglmm_pois_p3,link = "zinf glmmTMB log") %>% 
            mutate(sex = "Pooled")
) %>% 
  dplyr::filter(type !="Zero Inflation") %>% 
  dplyr::select(sex,iv,RR) %>% 
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
  pivot_wider(names_from="sex",values_from = "RR") %>% 
  knitr::kable(format="markdown")



