
# Path to dataset ----------
paper_srq20 <- readRDS(paste0(path_incap_rally_box,"/SRQ-20 paper/paper_srq20.RDS"))

paper_srq20 <- paper_srq20 %>% 
  left_join(read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/ses/ses_masters_apr06.sas7bdat")) %>% 
                                           dplyr::select(id_uni,pcall6775),by=c("site_pin"="id_uni")) %>% 
  dplyr::filter(urbano_rural2018 %in% c(0,1)) %>% 
  mutate(eduyr = gtadeduyr1618 - median(gtadeduyr1618))


# Outcome distribution --------

paper_srq20 %>% 
  ggplot(data=.,aes(x=adsrq)) +
  geom_histogram() +
  theme_bw() +
  xlab("SRQ-20") +
  facet_wrap(~chsex)

# Table generation -----------

tab_df = paper_srq20 %>% 
  mutate(atolefull = case_when(gtatole==1 & gtchatoleexposurestatus == "full" ~ 1,
                               gtatole == 0 & gtchatoleexposurestatus == "full" ~ 0,
                               TRUE ~ NA_real_),
         atolepartial = case_when(gtatole==1 & gtchatoleexposurestatus == "partial" ~ 1,
                                  gtatole == 0 & gtchatoleexposurestatus == "partial" ~ 0,
                                  TRUE ~ NA_real_),
         atolenone = case_when(gtatole==1 & gtchatoleexposurestatus == "none" ~ 1,
                               gtatole == 0 & gtchatoleexposurestatus == "none" ~ 0,
                               TRUE ~ NA_real_),
  ) %>% 
  dplyr::filter(!is.na(score_change_ge300)) %>% 
  dplyr::select(
    chsex,
    adsrq,
    gtadseverementaldistress,
    
    gtatole, gtchatoleexposurestatus,
    gtchbyear, pccs6775,
    
    moht,
    moht_sib,
    moht_imputed,
    moscho, 
    moscho_sib,
    moscho_imputed,
    
    score_change_ge300, gtadeduyr1618,
    pccs1618,gtadrelstatyn,
    
    atolefull,atolepartial,atolenone,
    urbano_rural2018
  )

tab1f <-  compareGroups(gtadseverementaldistress ~.-chsex,
                        data=tab_df %>% 
                          dplyr::filter(chsex =="female") ,
                        method = c(2,
                                   
                                   3,3,
                                   2,1,
                                   
                                   1,1,1,
                                   2,2,2,
                                   
                                   3,2,1,3,
                                   
                                   3,3,3,3,3)) %>% 
  createTable(show.n=TRUE,sd.type = 2,q.type = c(2,2))


tab1m <-  compareGroups(gtadseverementaldistress ~.-chsex,
                        data=tab_df %>% 
                          dplyr::filter(chsex =="male") ,
                        method = c(2,
                                   
                                   3,3,
                                   2,1,
                                   
                                   1,1,1,
                                   2,2,2,
                                   
                                   3,2,1,3,
                                   
                                   3,3,3,3,3)) %>% 
  createTable(show.n=TRUE,sd.type = 2,q.type = c(2,2))

tab1p <-  compareGroups(gtadseverementaldistress ~.-chsex,
                        data=tab_df,
                        method = c(2,
                                   
                                   3,3,
                                   2,1,
                                   
                                   1,1,1,
                                   2,2,2,
                                   
                                   3,2,1,3,
                                   
                                   3,3,3,3,3)) %>% 
  createTable(show.n=TRUE,sd.type = 2,q.type = c(2,2))

tab1f %>% 
  export2xls(.,file=paste0(path_incap_rally_box,"/SRQ-20 paper/tables/tab1f.xlsx"))

tab1m %>% 
  export2xls(.,file=paste0(path_incap_rally_box,"/SRQ-20 paper/tables/tab1m.xlsx"))

tab1p %>% 
  export2xls(.,file=paste0(path_incap_rally_box,"/SRQ-20 paper/tables/tab1p.xlsx"))












