formula_p6_fe = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + gtvillage + chsex + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + scale(gtadeduyr1618) + pccs1618 + gtadrelstatyn + score_change_ge300 + ademployment"

formula_p4_fe = "gtadseverementaldistress ~ gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775  + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + urbano_rural2018 + gtadrelstatyn + score_change_ge300"

# formula_pois_p3 <- "adsrq~gtatole*gtchatoleexposurestatus + chsex + byear + pccs6775 + moscho_imputed + scale(moht_imputed) + eduyr + pccs1618 + gtadrelstatyn + urbano_rural2018 + score_change_ge300 + (1|d_id_unim)"

logit_m6_fe_p <- glm(as.formula(formula_p6_fe),
                     family = binomial,
                     # id = d_id_unim,
                     # corstr = "unstructured",
                     
                     # glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                     # control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                     # control=glmerControl(optimizer ="nloptwrap2"),
                     data = paper_srq20 %>% 
                       dplyr::filter(!is.na(score_change_ge300),!is.na(gtadeduyr1618)))
summary(logit_m6_fe_p)

anova(logit_m4_fe_p,logit_m6_fe_p,test="Chisq")