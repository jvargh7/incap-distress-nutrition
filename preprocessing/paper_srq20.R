
jh_data <- read_dta(paste0(path_incap_rally_box,"/SRQ-20 Paper/John Hoddinott - HAZ24m_shocks_Majo_migration_paper.dta")) %>% 
  dplyr::select(id_uni,starts_with("haz24"))

ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs.RDS"))
source(paste0(path_incap_repo,"/structural/early_life.R"))
source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))
variables_ann <- read_dta(paste0(path_incap_rally_box,"/SRQ-20 Paper/variables_ann.dta"))

dd_variables_ann <- read_dta(paste0(path_incap_rally_box,"/SRQ-20 Paper/DD_variables_ann.dta"))

paper_srq20 <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site %in% c("guatemala")) %>% 
  dplyr::select(pin,gtatole,gtchatoleexposurestatus,gtchatoleexposuretiming,
                chhazwho0, chhazwho24, chwhzwho0, chwhzwho24,
                
                adsrq, gtchbyear,chsex,chsoclass,gtadstresslifeevents2018,
                gtadladdercommunity2018,gtadladdereconomic2018,
                gtadfaith2018,gtadsleepbehav2018,gtadseleff2018,
                gtademsupp2018,gtadhope2018,gtadmp2018,
                gtadphysicaldisability2018,
                gtadnumadchildexp2018,gtadadchildexp2018,
                
                gtadeduyr1618,gtadwealthindex2018,gtadwealthindex2016,
                ademployment,
                adrelstat,gtadnumchildren2018,gtadeduyr1618) %>% 
  mutate(site_pin = pin - 20000000) %>% 
  
  mutate(gtadwealthindex2018 = case_when(!is.na(gtadwealthindex2018) ~ gtadwealthindex2018,
                                         TRUE ~ gtadwealthindex2016)) %>% 
  
  mutate(
    byear = gtchbyear-62,
    
    gtadwealthcat2018 = cut(gtadwealthindex2018,
                            breaks = quantile(gtadwealthindex2018,probs=seq(0,1,by=0.2),na.rm = TRUE),
                            include.lowest=TRUE,
                            labels=c("lowest","2","3","4","highest")),
    gtadseverementaldistress = case_when(chsex=="female" & adsrq >=7 ~ 1,
                                         chsex == "male" & adsrq >= 7 ~ 1,
                                         is.na(adsrq) ~ NA_real_,
                                         TRUE ~ 0),
    # gtadseverementaldistress = case_when(chsex=="female" & adsrq >=8 ~ 1,
    #                                      chsex == "male" & adsrq >= 6 ~ 1,
    #                                      is.na(adsrq) ~ NA_real_,
    #                                      TRUE ~ 0),
    gtadrelstat2cat2018 = case_when(adrelstat %in% c("in a relationship","married") ~ 1,
                                    adrelstat %in% c("single") ~ 0,
                                    TRUE ~ NA_real_)
  ) %>% 
  left_join(ur %>% 
              dplyr::select(iduni,urbano_rural2018),
            by = c("site_pin" = "iduni")) %>% 
  dplyr::mutate(score_change = case_when(gtadstresslifeevents2018 == 0 ~ "Score = 0",
                                         gtadstresslifeevents2018 >= 300 ~ "80% Chance: Score>=300",
                                         gtadstresslifeevents2018 >=150 ~ "50% Chance: 150 <= Score <300",
                                         gtadstresslifeevents2018 <150 ~ "Low Chance: Score < 150",
                                         TRUE ~ NA_character_ ),
                score_change_ge300 = case_when(
                  gtadstresslifeevents2018 >= 300 ~ 1,
                  gtadstresslifeevents2018 < 300 ~ 0,
                  TRUE ~ NA_real_ )
                
  ) %>% 
  mutate(score_change = fct_relevel(score_change,"Low Chance: Score < 150"),
         score_change = fct_relevel(score_change,"Score = 0"),
         score_change_ge300 = factor(score_change_ge300, levels=c(0,1),labels=c("no","yes"))
  ) %>%
  
  
  mutate(gtadseverementaldistress = factor(gtadseverementaldistress,levels=c(0,1),labels=c("no","yes")),
         gtadrelstat2cat2018 = factor(gtadrelstat2cat2018,levels=c(1,0),
                                      labels=c("married or in a relationship","single"),ordered = TRUE),
         gtchatoleexposurestatus = factor(gtchatoleexposurestatus,levels=c("none","partial","full")),
         gtchatoleexposurestatusnumeric = case_when(gtchatoleexposurestatus == "none" ~ 0,
                                                    gtchatoleexposurestatus == "partial" ~ 1,
                                                    gtchatoleexposurestatus == "full" ~ 2,
                                                    TRUE ~ NA_real_
         ),
         gtadrelstatyn = case_when(adrelstat %in% c("in a relationship","married") ~ 1,
                                   adrelstat %in% c("single") ~ 0,
                                   TRUE ~ NA_real_)
  ) %>% 
  left_join(jh_data,
            by=c("site_pin"="id_uni")) %>% 
  left_join(variables_ann,
            by=c("site_pin" = "iduni")) %>% 
  left_join(dd_variables_ann,
            by=c("site_pin" = "iduni")
  ) %>% 
  left_join(incap_early_life %>% 
              dplyr::select(id_uni,moscho,moht,moage,
                            moscho_sib, moht_sib,
                            moscho_imputed,moht_imputed,moage_imputed),
            by = c("site_pin"="id_uni")) %>% 
  left_join(ses_masters %>% 
              dplyr::select(id_uni,pcall6775_1,pcall2016_1,pcall2018_1,comun,fechan),
            by = c("site_pin" = "id_uni")) %>% 
  mutate(pcall1618_1 = case_when(!is.na(pcall2018_1) ~ gtadwealthindex2018,
                                 TRUE ~ pcall2016_1)) %>%
  mutate(pcall1618_1 = scale(pcall1618_1)) %>% 
  rename(pccs6775 = pcall6775_1,
         pccs1618 = pcall1618_1) %>% 
  mutate(gtvillage = factor(comun,levels=c(3,6,8,14),labels=c("FR_ES","AT_CO","FR_SD","AT_SJ"))) %>% 
  left_join(read_dta(paste0(path_local_working,"/Processed Data/Stata datasets/gtm_structural.dta")) %>% 
              dplyr::select(site_pin,d_id_unim),
            by=c("site_pin")) %>% 
  
  mutate(taskinstruction = scale(se08) %>% as.numeric(),
         homeenvironment = scale(se22) %>% as.numeric())
# %>% 
#   dplyr::filter(urbano_rural2018 %in% c(0,1))


# Adding labels --------------

attr(paper_srq20$score_change,"label") <- "Holms Rahe Stressful Life Events (4 category)"
attr(paper_srq20$score_change_ge300,"label") <- "Holms Rahe Stressful Life Events >=300 (binary)"
attr(paper_srq20$byear,"label") <- "Birth Year - 1962"
attr(paper_srq20$chsoclass,"label") <- "Social class in childhood"
attr(paper_srq20$gtadwealthcat2018,"label") <- "Wealth category from gtadwealthindex2018"
attr(paper_srq20$gtadseverementaldistress,"label") <- "SRQ >=7 in Females and SRQ >=7 in Males"
attr(paper_srq20$gtadrelstat2cat2018,"label") <- "Relationship status (binary) from adrelstat"
attr(paper_srq20$gtchatoleexposurestatusnumeric,"label") <- "Atole exposure status (numeric) from gtchatoleexposurestatus"
attr(paper_srq20$pccs6775,"label") <- "Cross-sectional wealth index (binary, PCA) in 1967-75 census"
attr(paper_srq20$pccs1618,"label") <- "Cross-sectional wealth index (binary, PCA) in 2017-18 (else 2015-16 for 3 obs)"
attr(paper_srq20$taskinstruction,"label") <- "Task Instruction (se08) standardized"
attr(paper_srq20$homeenvironment,"label") <- "Home Environment (se22) standardized"
attr(paper_srq20$gtvillage,"label") <- "Village Fixed Effect"


# Saving dataset -----------

paper_srq20 %>% 
  dplyr::filter(!is.na(adsrq)) %>%
  labelled::copy_labels(paper_srq20,.) %>%
  write_dta(.,paste0(path_incap_rally_box,"/SRQ-20 paper/paper_srq20.dta"),version=12)

paper_srq20 %>% 
  dplyr::filter(!is.na(adsrq)) %>%
  labelled::copy_labels(paper_srq20,.) %>%
  saveRDS(.,paste0(path_incap_rally_box,"/SRQ-20 paper/paper_srq20.RDS"))


# Quick descriptives -----------
paper_srq20 %>% 
  dplyr::filter(!is.na(adsrq)) %>%
  labelled::copy_labels(paper_srq20,.) %>%
  dplyr::select(chsex, gtadseverementaldistress,
                chsoclass,gtchbyear,chhazwho24,chwhzwho24,
                
                pccs6775,
                score_change_ge300,
                moht,moht_imputed,
                moscho, moscho_imputed,
                ademployment,
                gtadnumchildren2018,gtadeduyr1618,gtadwealthcat2018,
                gtadrelstat2cat2018,gtadstresslifeevents2018,
                gtadseleff2018,gtademsupp2018) %>% 
  compareGroups(chsex ~ .,data=.) %>% 
  createTable(show.n=TRUE)

paper_srq20 %>% 
  dplyr::filter(!is.na(adsrq)) %>% 
  glm(data=.,gtadseverementaldistress ~ chsex+ gtatole*haz24estWHO,family=binomial()) %>% 
  summary(.)

