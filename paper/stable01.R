
library(lavaan)

# MODEL -----------
path_model_p2 <- '
eduyr ~ byear + pccs6775 + scale_moht_imputed + moscho_imputed + gtatole + exposureF + exposureP + gtatole:exposureF + gtatole:exposureP + chsex
pccs1618 ~ byear + pccs6775 + scale_moht_imputed + moscho_imputed + gtatole + exposureF + exposureP + gtatole:exposureF + gtatole:exposureP + chsex + eduyr + urbano_rural2018 + gtadrelstatyn
urbano_rural2018 ~ byear + pccs6775 + scale_moht_imputed + moscho_imputed + gtatole + exposureF + exposureP + gtatole:exposureF + gtatole:exposureP + chsex + eduyr
gtadrelstatyn ~ byear + pccs6775 + scale_moht_imputed + moscho_imputed + gtatole + exposureF + exposureP + gtatole:exposureF + gtatole:exposureP + chsex + eduyr
'

# FIT ------------

path_fit <-  sem(path_model_p2,
                 data = paper_srq20 %>% 
                   dplyr::filter(!is.na(score_change_ge300),!is.na(gtadeduyr1618)) %>% 
                   mutate(scale_gtadeduyr1618 = scale(gtadeduyr1618),
                          scale_moht_imputed = scale(moht_imputed),
                          exposureF = case_when(gtchatoleexposurestatus == "full" ~ 1,
                                                TRUE ~ 0),
                          exposureP = case_when(gtchatoleexposurestatus == "partial" ~ 1,
                                                TRUE ~ 0),
                          
                   ),
                 std.ov = FALSE,
                 cluster = "d_id_unim",
                 estimator = "mlr",
                 # group="chsex",
                 # Using FIML for "imputation"
                 missing = "fiml")

summary(path_fit)

# RESULT ----------
path_fit %>% 
  broom::tidy(.) %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  dplyr::filter(op == "~") %>% 
  separate(term,c("dv","iv")," ~ ") %>% 
  mutate(out = paste0(estimate,"(",conf.low,", ",conf.high,")")) %>% 
  dplyr::select(dv,iv,out) %>% 
  pivot_wider(names_from="dv",values_from = "out") %>% 
  knitr::kable()