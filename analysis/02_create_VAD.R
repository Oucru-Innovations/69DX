# Extract summary data for each type
# Author: Trinh Dong Huu Khanh

# Load data ----
rm(list=ls())
library(tidytable)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_dir <- '../data'
load(file.path(data_dir, 'imported', 'dx69_imported_data.Rdata'))

hist[, usubjid:=ifelse(usubjid=='003-12-1-074', '003-12-0-074', usubjid)]

# Demographics

# Baseline information ----
## Demographics ----

birthdata$usubjid <-paste0('003-', birthdata$usubjid)
bsl_demo <- demographic |>
  mutate(sdiagnos2 = sdiagnos,
         sdiagnos=
           sdiagnos |>
           tolower() |>
           gsub('sot xuat huyet','', x=_) |>
           gsub('sxhd', '', x=_) |>
           gsub('sxh', '', x=_) |>
           strsplit(',\\s?')) |>
  # select(-doh) |>
  left_join(select(birthdata, usubjid, doh2=doh, coh:noi_o), by='usubjid') |>
  transmute(
    usubjid = usubjid,
    sex = case_match(sex, 'M' ~ 'Male', "F" ~ 'Female'),
    sdiagnos2 = sdiagnos2,
    age_exact = 16 + difftime(
      anytime::anydate(dss),
      ifelse(`nam_sinh__2006:_dob` > 2024, 
             as.Date(`nam_sinh__2006:_dob`, origin='1899-12-30') |> {
               function(x){
                 lubridate::year(x) <- lubridate::year(x) + 16
                 x
               }
             }(),
             lubridate::make_date(year=`nam_sinh__2006:_dob`+16,1,1)),
      units='days'
    )/365.25,
    age = 2024 - ifelse(`nam_sinh__2006:_dob` > 2024, 
                        lubridate::year(as.Date(`nam_sinh__2006:_dob`, origin='1899-12-30')),
                        `nam_sinh__2006:_dob`),
    address = 
      ifelse(grepl('Ho Chi Minh', noi_o, fixed=TRUE), 'Ho Chi Minh City', noi_o) |>
      forcats::fct_infreq() |>
      forcats::fct_lump_n(n=4),
    day_sick = ds2,
    date_start = anytime::anydate(dss),
    date_admit = anytime::anydate(ifelse(is.na(doh), doh2, doh)),
    date_enrol = anytime::anydate(dresearch),
    date_icu_in = anytime::anydate(dicu),
    date_icu_out = anytime::anydate(doicu),
    date_discharge = anytime::anydate(douth),
    nday_icu = difftime(date_icu_out, date_icu_in, units='d') |> as.numeric(),
    nday_hos = difftime(date_discharge, date_admit, units='d') |> as.numeric(),
    
    diag_warning = sapply(
      sdiagnos,
      \(x) any(grepl('canh bao', x))
    ),
    diag_heart = sapply(
      sdiagnos,
      \(x) any(grepl('lien nhi', x))
    ),
    diag_shock = sapply(
      sdiagnos,
      \(x) case_when(
        any(grepl('tai soc', x)) ~ 'DSS + Reshock',
        any(grepl('soc', x)) ~ 'DSS + No reshock',
        TRUE ~ 'No shock'
      )
    ),
    diag_bleed = sapply(
      sdiagnos,
      function(x) case_when(
        any((grepl('XH nang', x) | grepl('xuat huyet nang', x))) ~  'Severe',
        any((grepl('XH', x) | grepl('xuat huyet', x))) ~ 'Non-severe',
        TRUE ~ 'No haemorrhage'
      )),
    diag_liver = sapply(
      sdiagnos, 
      \(x) case_when(
        any(grepl('gan nang', x)) ~ 'Acute liver failure',
        any(grepl('gan', x)) ~ 'Damage + No ALF',
        TRUE ~ 'No liver damage',
      )),
    diag_kidney =
      sapply(
        sdiagnos,
        \(x) case_when(
          any(grepl('than cap', x)) ~ 'Acute renal failure',
          any( grepl('than', x) &
                 !grepl('than hu', x) &
                 !grepl('than man', x)) ~ 'Damaged without ARF',
          TRUE ~ 'No renal damage'
        )
      ),
    diag_brain = sapply(
      sdiagnos,
      \(x) any(grepl('nao', x) & !grepl('tai bien', x))),
    diag_multi = rowSums(cbind(diag_liver!='No liver damage',
                               diag_heart,
                               diag_kidney !='No renal damage', 
                               diag_brain))
  )  

## Preadmission information ----
pre_admit <-
  medhist |>
  # filter(pretreatment == 'Y') |>
  transmute(
    usubjid = usubjid,
    pretreat = case_when(
      pretreatment%in%'N'&bvbnd ~ 'From another ward at HTD',
      pretreatment%in%'Y'&!bvbnd ~ 'Direct from another healthcare centre',
      pretreatment%in%'Y'&bvbnd ~ 'From another healthcare centre via another ward at HTD',
      .default='Direct admission',
    ) |> factor(
      levels = c(
        'Direct admisision',
        'From another ward at HTD',
        'Direct from another healthcare centre' ,
        'From another healthcare centre via another ward at HTD'
      )
    ),
    # pretreat = pretreatment %in% 'Y',
    # pretreat_htd = ifelse(pretreatment %in% 'Y', bvbnd, NA),
    day_pretreat = as.numeric(pretreatmenttime)/24,
    date_pretreat = anytime::anydate(pretreatmentdtc),
    pretreat_infusion = infusion == 'Y',
    pretreat_infusion_amount = infuseamount,
    pretreat_infusion_polyamount = polyamount,
    pretreat_antibiotic = antibiotic == 'Y',
    pretreat_reason_shock = 
      shocklong | reshock | grepl('soc', tolower(othorganspec)),
    pretreat_reason_hemorrhage = 
      hemorrhage | grepl('(ra huyet) | (chay mau)', tolower(othorganspec)),
    pretreat_reason_organ = 
      organdamage  | grepl('(gan)|(than)|(nao)|(ast)|(alt)', tolower(othorganspec)),
    pretreat_reason_other = 
      othorgan & 
      !grepl('soc', tolower(othorganspec)) &
      !grepl('(ra huyet) | (chay mau)', tolower(othorganspec)) &
      !grepl('(gan)|(than)|(nao)|(ast)|(alt)', tolower(othorganspec))
  ) |>
  left_join(select(bsl_demo, usubjid, date_enrol, date_admit)) |>
  mutate(
    day_pretreat =
      ifelse(
        pretreat == 'From another ward at HTD',
        as.numeric(difftime(date_enrol, 
                            date_admit, units='d')),
      ifelse(is.na(day_pretreat),
             as.numeric(difftime(date_enrol,
                                 date_pretreat, units='d')),
             day_pretreat))
  )


## Comorbidity ----
comorbid <-
  hist %>%
  mutate(
    usubjid=usubjid,
    past_denv = sxhd %in% 'Y' | !is.na(sxhdamount) | !is.na(sxhdage) , #sometimes, they did not fill all the fields
    across(highpress:thalasssemia, ~.x%in%'Y',.names = 'cm_{col}'),
    cm_any = 
      cbind(mutate(., across(highpress:thalasssemia, ~.x%in%'Y'), .keep = 'none'),
            oth=is.na(oth)) |>
      apply(1, any),
    pregnant = case_when(
      (postpartum == 'M') %in% TRUE ~ NA,
      !is.na(pregnant) & pregnant == 'Y' ~ TRUE,
      .default = FALSE
    ),
    postpartum = case_when(
      (postpartum == 'M') %in% TRUE ~ NA,
      !is.na(postpartum) & postpartum == 'Y' ~ TRUE,
      .default = FALSE
    ),
    cm_kidney_disease = 
      ckd == 'Y' | kidney == 'Y' | ns == 'Y' |
      grepl('hctc', tolower(oth)) | grepl('than hu', tolower(oth)),
    cm_anemia_thalassemia = 
      anemia == 'Y' | 
      thalasssemia == 'Y' |
      grepl('thieu mau', x=tolower(oth)) |
      grepl('thala', x=tolower(oth)),
    cm_cardio_disease = 
      chestpain == 'Y' |
      myocardialinfarction == 'Y' |
      heartfail1 == 'Y' | 
      heartfail3 == 'Y' | 
      pvd == 'Y' | 
      cvd == 'Y' | 
      highpress == 'Y' |
      grepl('huyet ap', tolower(oth)) |
      grepl('\\sha\\s', tolower(oth)) |
      grepl('\\stim', tolower(oth)) | 
      grepl('thieu mau', tolower(oth)) |
      grepl('thong lien that', tolower(oth)),
    cm_pulmonary_disease = cld == 'Y' |  srf == 'Y' | grepl('phoi', x=tolower(oth)),
    cm_liver_disease =
      ld == 'Y' | lhf == 'Y' |
      grepl('gan', x=tolower(oth)),
    cm_diabetes = 
      diabetes %in% 'Y' | sd %in% 'Y' |
      grepl('duong', x=tolower(oth)),
    cm_cancer = soc == 'Y' | othc == 'Y' | 
      grepl('ung thu', tolower(oth)) | grepl('K ', oth),
    cm_immusuppression = hiv == 'Y' | aids == 'Y',
    cm_covid = grepl('covid', tolower(oth)) & grepl('/',tolower(oth)) & !grepl('mui', tolower(oth)),
  ) %>%
  mutate(.,
         cm_other = !sxhconfirm & 
           !past_denv &!pregnant & !postpartum,
           rowSums(select(., starts_with(('cm_'))))==0) |>
         # cm_other = !sxhconfirm & 
         #   (antiinflame %in% 'Y' | 
         #   hemiplegia %in% 'Y' |
         #   ctd %in% 'Y' | su %in% 'Y' | 
         #   dementia %in% 'Y' |
         #   surgery %in% 'Y')|
         #   (!is.na(oth) & rowSums(select(., starts_with(('cm_'))))==0)) |>
  select(usubjid, past_denv,
         pregnant, postpartum,
         starts_with('cm_'))

## Baseline clinical ----
bsl_clin <-
  cli |>
  transmute(
    usubjid = usubjid,
    clin_sbp = as.numeric(sbp),
    bmi = 
      ifelse(is.na(bmi), weight/(height^2), bmi),
    
  )

## Past laboratory ----

pre_lab <-
  precli_precligrid |>
  transmute(
    usubjid = usubjid,
    
    # dengue-specific
    ns1 = ns1 == 'Y',
    igm = igmdengue == 'Y',
    igg = iggdengue == 'Y',
    
    # blood information, only mentioned ones were included
    bld_hct_max = hct,
    bld_hct_min = hct,
    bld_plt = plt,
    # remove TQ by checking if number > 20 and is a round number
    bld_tqinr = ifelse(tqinr > 20 & tqinr * 10 %% 10 == 0, NA_real_, tqinr), 
    bld_fibrinogen = fibrinogen,
    
    # liver
    liv_ast = ast,
    liv_alt = alt,
    liv_ggt = ggt,
    liv_bil_tot = bilirubintp,
    liv_albumin = albumin,
    
    # kidney
    kid_cre = creatinin,
    kid_ure = ure,
    
    # effusiton
    eff_cardio = FALSE,
    eff_pleuro = pleuraleffusion %in% 'Y',
    eff_perito = peritonealeffusion %in% 'Y'
  ) |>
  left_join(
    cli |>
      transmute(
        usubjid=usubjid,
        tmp_clin_eff_perito = peritonealeffusion %in% 'Y',
        tmp_clin_eff_cardio = pericardialeffusion %in% 'Y',
        tmp_clin_eff_pleuro = pleuraleffusion %in% 'Y'
      )
  ) |>
  mutate(
    eff_cardio = tmp_clin_eff_cardio,
    eff_pleuro = eff_pleuro | tmp_clin_eff_pleuro,
    eff_perito = eff_perito | tmp_clin_eff_perito
  ) |>
  select(!starts_with('tmp_')) |>
  group_by(usubjid) |>
  summarise(
    # across(where(is.logical), 
    #        ~ case_match(
    #          any(.x), 
    #          TRUE ~ 'Positive', 
    #          FALSE ~ 'Negative',
    #          NA ~ 'Unknown') |>
    #          factor(levels=c('Positive', 'Negative', 'Unknown'))) ,
    across(c(ns1:igg, starts_with('eff_')), any),
    across(c(bld_hct_min, bld_plt, bld_fibrinogen, liv_albumin), min, na.rm=TRUE),
    across(c(bld_hct_max, bld_tqinr, liv_ast:liv_bil_tot, kid_cre, kid_ure), max, na.rm=TRUE)
  ) |>
  mutate(across(everything() & !usubjid, ~ifelse(is.infinite(.x), NA, .x)))
  

# Follow-up information ----

## Follow-up clinical ----

fu_clin <- 
  list(clia, clib, clic) |>
  lapply(mutate,
         usubjid=usubjid, 
         # clin_temp=ends_with('temp'),
         clin_pulse=pulse,
         clin_sbp = sbp,
         clin_gcs=gcs,
         clin_jaundice = jaundice,
         clin_enlargeliver = enlargeliver,
         clin_enlargespleen = enlargespleen,
         clin_pleuraleffusion = pleuraleffusion,
         clin_peritonealeffusion = peritonealeffusion,
         clin_bleeding  = bleeding,
         clin_bleeding_injectsite= bruise,
         clin_bleeding_upper = hematemesis,
         clin_bleeding_lower = hemorrhoid,
         clin_bleeding_uria = hematuria,
         clin_bleeding_gum = bleedinggum,
         clin_bleeding_nose = bleedingnose,
         clin_bleeding_vagina = vaginalbleeding,
         clin_brain_restless = restless,
         clin_brain_convulsion = convulsion,
         clin_brain_focal = fnm,
         clin_urine = ifelse(
           urinena %in% TRUE, 0,
           str_extract(urine,'\\d+') |> as.numeric() 
         )
  ) |>
  lapply(select, usubjid, clin_temp=ends_with('temp'),
         starts_with('clin_')) |>
  do.call(rbind, args=_ ) |>
  summarise(
    # across(clin_temp:clin_gcs, ~ list(.x)),
    # across(c(clin_temp:clin_gcs,clin_urine), 
    #        ~ if (length(na.omit(.x)))
    #          paste(min(.x, na.rm=T), max(.x, na.rm=T), sep=', ') else 
    #            NA_character_
    # ),
    across(c(clin_sbp:clin_gcs,clin_urine), min),
    across(clin_temp:clin_pulse, max),
    across(clin_jaundice:clin_brain_focal, 
           ~ any(if(is.character(.x)) .x%in%'Y' else .x)),
    .by=usubjid)


## Follow-up laboratory ----

fu_lab <-
  list(preclia, preclib, preclic) |>
  lapply(
    transmute,
    usubjid = usubjid,
    
    # dengue-specific
    ns1 = ns1 == 'Y',
    igm = igmdengue == 'Y',
    igg = iggdengue == 'Y',
    
    # blood information, only mentioned ones were included
    bld_hct_min = hct,
    bld_hct_max = hct,
    bld_plt = plt,
    # remove TQ by checking if number > 20 and is a round number
    bld_tqinr = ifelse(tqinr > 20 & tqinr * 10 %% 10 == 0, NA_real_, tqinr), 
    bld_fibrinogen = fibrinogen,
    
    # liver
    liv_ast = ast,
    liv_alt = alt,
    liv_ggt = ggt,
    liv_bil_tot = bilirubintp,
    liv_albumin = albumin,
    
    # kidney
    kid_cre = creatinin,
    kid_ure = ure,
    
    # effusion
    eff_cardio = echocardiogrameffusion %in% 'Y',
    eff_pleuro = pleuraleffusion %in% 'Y',
    eff_perito = if (exists('peritonealeffusionammount')) !is.na(peritonealeffusionammount) else FALSE
  ) |>
  do.call(rbind, args=_ ) |>
  group_by(usubjid) |>
  summarise(
    # across(where(is.logical), 
    #        ~ case_match(
    #          any(.x), 
    #          TRUE ~ 'Positive', 
    #          FALSE ~ 'Negative',
    #          NA ~ 'Unknown') |>
    #          factor(levels=c('Positive', 'Negative', 'Unknown'))) ,
    across(c(ns1:igg, starts_with('eff_')), any),
    across(c(bld_hct_min, bld_plt, bld_fibrinogen, liv_albumin), min, na.rm=TRUE),
    across(c(bld_hct_max, bld_tqinr, liv_ast:liv_bil_tot, kid_cre, kid_ure), max, na.rm=TRUE)
  ) |>
  mutate(across(everything() & !usubjid, ~ifelse(is.infinite(.x), NA, .x)))

## Treatment ----
### Vasopressor ----
vaso_tx <- 
  list(treata, treatb, treatc) |>
  lapply(
    select,
    usubjid = usubjid,
    
    dopamin,
    dobutamin,
    noradrenalin,
    adrenalin,
    albumin,
    crrt,
    
    es, frozenplasma, cp, platelet
    
  ) |>
  do.call(rbind, args=_) |>
  summarise(
    across(dopamin:crrt, ~ any(.x %in% 'Y')),
    across(es:platelet, ~ sum(.x, na.rm = T)),
    .by=usubjid
  ) |>
  rename_with(~ paste0('vaso_', .x),
              .cols=dopamin:platelet
  ) %>%
  mutate(., vaso_vaso = (select(., -usubjid) |> rowSums())>0) |>
  select(usubjid, vaso_vaso, everything())

### Liver ----
liver_tx <- 
  list(treata, treatb, treatc) |>
  lapply(
    select,
    usubjid = usubjid,
    
    metronidazole,
    rifaximin,
    # tpe,  nac
    
  ) |>
  do.call(rbind, args=_) |>
  summarise(
    across(metronidazole:rifaximin, ~ any(.x)),
    # across(tpe:nac, ~ any(.x %in% 'Y')),
    .by=usubjid
  ) |>
  rename_with(~ paste0('liver_', .x),
              .cols=!contains('usubjid')
  ) %>%
  mutate(., liver_liver = (select(., -usubjid) |> rowSums())>0) |>
  select(usubjid, liver_liver, everything())

### Kidney ----
kidney_tx <-
  list(treata, treatb, treatc) |>
  lapply(
    select,
    usubjid = subjid,
    
    diuretic
    
  ) |>
  do.call(rbind, args=_) |>
  summarise(kidney_diuretic = any(diuretic%in%'Y'), .by=usubjid) |>
  left_join(transmute(comp,usubjid=subjid, kindey_ventilate=kdnkq%in%'Y'))%>%
  mutate(., kidney_kidney = (select(., -usubjid) |> rowSums())>0) |>
  select(usubjid,kidney_kidney, everything())

# Discharge information ----

## Discharge dx ----

dis_diag <- 
  comp |>
  mutate(
    type_shock = isdshock %in% 'Y',
    type_shock_reshock = 
      ifelse(isdshock %in% 'Y',
             isdreshock %in% 'Y'|!is.na(isdreshockspec), 
             NA),
    type_hemorrhage = ishemorrhage %in% 'Y',
    type_liver = iscirrhois %in% 'Y',
    type_liver_cirrhosis = 
      ifelse(iscirrhois %in% 'Y',
             cirrhosisalf %in% 'Y',
             NA),
    type_kidney = iskidneydamage %in% 'Y',
    type_kidney_aki =  # no AKI value
      ifelse(iskidneydamage %in% 'Y',
             kdcrrt %in% 'Y' | kdnkq %in% 'Y',
             NA),
    type_heart  = isheartfailure %in% 'Y',
    type_brain = isbi %in% 'Y'
  ) %>%
  mutate(., type_multi = rowSums(select(., 
                                        c(type_brain,type_liver,type_kidney,
                                          type_heart)))) |>
  select(usubjid=usubjid,
         starts_with('type'))


## Discharge treatment ----

dis_outc <-
  comp |>
  mutate(
    outc_shock = isdshock %in% 'Y',
    outc_reshock = ifelse(isdreshock %in% 'Y', isdreshock %in% 'Y',NA),
    outc_N_reshock = ifelse(isdreshock %in% 'Y', replace_na(as.character(isdreshockspec),'Unknown'), NA_character_),
    outc_poly = ifelse(isdreshock %in% 'Y', ispoly %in% 'Y',NA),
    outc_Alb = ifelse(isdreshock %in% 'Y', isalbumin %in% 'Y',NA),
    
    outc_hemorrhage = ishemorrhage %in% 'Y',
    outc_erythrocyte_sedimentation = 
      ifelse(ishemorrhage %in% 'Y', replace_na(es,0), NA_real_),
    outc_fresh_frozen_plasma =
      ifelse(ishemorrhage %in% 'Y', replace_na(frozenplasma,0), NA_real_),
    outc_cryoprecipitate = 
      ifelse(ishemorrhage %in% 'Y', replace_na(cp, 0), NA_real_),
    outc_platelet = 
      ifelse(ishemorrhage %in% 'Y', replace_na(platelet, 0), NA_real_),
    
    outc_liver = iscirrhois %in% 'Y',
    outc_jaundice = ifelse(iscirrhois %in% 'Y', cirrhosisjaundice %in% 'Y', NA),
    outc_alf = ifelse(iscirrhois %in% 'Y', cirrhosisalf %in% 'Y', NA),
    outc_liverbrain = ifelse(iscirrhois %in% 'Y', cirrhosishe %in% 'Y', NA),
    outc_nac = ifelse(iscirrhois %in% 'Y', cirrhosisnac %in% 'Y', NA),
    outc_tpe = ifelse(iscirrhois %in% 'Y', cirrhosistpe %in% 'Y', NA),
    
    outc_kidney = iskidneydamage %in% 'Y',
    outc_crrt = ifelse(iskidneydamage %in% 'Y', kdcrrt %in% 'Y', NA),
    
    outc_brain = isbi %in% 'Y',
    outc_convulsion = ifelse(isbi %in% 'Y', biconvulsion %in% 'Y', NA),
    outc_focal = ifelse(isbi %in% 'Y', bifnm %in% 'Y', NA),
    
    outc_heart = isheartfailure %in% 'Y',
    outc_troponinI = ifelse(isheartfailure %in% 'Y', 
                            
                            case_when(
                              hftroponin == '<10' ~ 'Lower than 10',
                              as.numeric(hftroponin) < 10 ~ 'Lower than 10',
                              as.numeric(hftroponin) < 100 ~ '10 - 100',
                              as.numeric(hftroponin) > 100 ~ 'Higher than 100',
                              .default = NA_character_
                            ),
                            NA_character_) |>
      factor(levels=  c('Lower than 10', '10 - 100', 'Higher than 100')),
    outc_probnp = ifelse(isheartfailure %in% 'Y', hfprobnp, NA_real_),
    
    outc_canola  = ricanola %in% 'Y',
    outc_hfnc    = rihfnc %in% 'Y',
    outc_niv     = riniv %in% 'Y',
    outc_machine = rimachine %in% 'Y',
    
    outc_pneumonia = pneumonia,
    outc_sepsis = sepsis,
    outc_urinary_infection = uti,
    outc_overinfusion = grepl('qua tai', tolower(oth)),
    
    outc2_healthy = strongres,
    outc2_sequelae = ifelse(
      sequelaeres,
      case_match(sequelaespec,
                 'yeu 1/2 nguoi' ~ 'Hemiplegia',
                 'Xo gan' ~ 'Cirrhosis',
                 .default = 'Unknown sequela'
      ),
      'No sequelae'
    ),
    outc2_transfer = ifelse(
      hfres,
      case_match(
        hfspec,
        "say thai tien trien(dau ha vi, xuat huyet am dao)" ~ 'Miscarriage warning',
        "suy than, chuyen BV Nguyen Tri Phuong" ~ 'Acute renal failure',
        "kham san TD say" ~ 'Miscarriage warning',
        .default = 'Unknown reason'
      )
      ,
      'Not transferred'
    ),
    outc2_death = deathres
  ) |>
  select(usubjid = usubjid, starts_with('outc'))


# Type-specific analysis ----

## Shock ----

shock <- 
  comp |>
  filter(isdshock %in% 'Y') |>
  transmute(
    usubjid = usubjid,
    n_reshock = ifelse(isdreshock %in% 'Y', 
                   replace_na(as.character(isdreshockspec),
                              'Unknown'),
                   NA_character_),
    # hct = hct,
    # plt = platelet,
    
    # treatment
    tx_colloids = rowSums(cbind(0, poly1, poly2, poly3), na.rm = TRUE) > 0,
    tx_albumin = rowSums(cbind(0, albumin1, albumin2, albumin3, albumin4), na.rm = TRUE) > 0,
    
    # Respiratory tx
    tx_canola  = ricanola %in% 'Y',
    tx_hfnc    = rihfnc %in% 'Y',
    tx_niv     = riniv %in% 'Y',
    tx_machine = rimachine %in% 'Y',
  ) |>
  left_join(
    select(fu_lab, usubjid, starts_with('eff_'))
  ) |>
  left_join(
    list(pre_lab, fu_lab) |>
      lapply(select, usubjid, bld_hct_max, bld_plt) |>
      do.call(rbind, args=_) |>
      group_by(usubjid) |>
      summarise(
        across(where(is.logical) & !usubjid, any),
        across(where(is.numeric) & !usubjid, min, na.rm=TRUE))
  ) |>
  left_join(select(vaso_tx, usubjid, tx_vaso=vaso_vaso)) |>
  mutate(
    # across(where(is.logical), 
    #        ~ case_match(
    #          any(.x), 
    #          TRUE ~ 'Positive', 
    #          FALSE ~ 'Negative',
    #          NA ~ 'Unknown') |>
    #          factor(levels=c('Positive', 'Negative', 'Unknown'))),
    across(where(
      function(x) is.factor(x) & 
           identical(levels(x), 
                        c("Positive", "Negative", "Unknown"))
    ),
    ~ replace_na(.x, 'Unknown')) 
  ) |> unique()


## Liver ----

liver <- 
  comp |>
  filter(iscirrhois%in%'Y') |>
  transmute(
    usubjid=usubjid,
    clin_jaundice = cirrhosisjaundice %in% 'Y',
    clin_alf = cirrhosisalf %in% 'Y',
    clin_encephalopathy = cirrhosishe %in% 'Y' | !is.na(cirrhosishespec),
    
    # treatment
    tx_nac = cirrhosisnac %in% 'Y',
    tx_tpe = cirrhosistpe %in% 'Y'
  ) |>
  left_join(
    select(fu_clin, usubjid, clin_enlargeliver, clin_enlargespleen)
  ) |>
  left_join(
    list(pre_lab, fu_lab) |>
      lapply(select, usubjid, bld_tqinr, liv_albumin, liv_bil_tot, liv_ast, liv_alt) |>
      do.call(rbind, args=_) |>
      group_by(usubjid) |>
      summarise(
        across(where(is.logical) & !usubjid, any),
        across(where(is.numeric) & !usubjid, max, na.rm=TRUE))
  ) |>
  left_join(select(liver_tx, usubjid, tx_he = liver_liver)) |>
  # mutate(tx_he= ifelse(clin_encephalopathy, tx_he, NA)) |>
  unique()
  

## Hemorrhage ----

bleeding <-
  comp |>
  filter(ishemorrhage %in% 'Y') |>
  transmute(
    usubjid=usubjid,
    site_brain = grepl('nao', tolower(hhothspec)) | grepl('mang cung', hhothspec),
    site_nose = hhnose,
    site_mouth = hhmouth | grepl('rang', hhothspec),
    site_digest_tract = hhudt | hhldt,
    site_inject_site = hhsting | grepl('(chich)|(tiem)|(HADM)', hhothspec), 
    site_urinary_tract = hhut,
    site_vaginal = hhvagina,
    site_other = hhskin | (hhoth | !is.na(hhothspec))
      & (!grepl('(chich)|(tiem)|(rang)|(HADM)', hhothspec) & 
         !grepl('nao', tolower(hhothspec)) & !grepl('mang cung', hhothspec)),
    .hhothspec=hhothspec,
    
    # treatment
    tx_packed_rbc = replace_na(es,0),
    tx_fresh_frozen_plasma = replace_na(frozenplasma,0),
    tx_cryoprecipitate = replace_na(cp, 0),
    tx_platelet =  replace_na(platelet, 0),
    tx_nasal_package = frontmeche | rearmeche | gumpress,
    tx_endoscopic_intervention = eh
  ) |>
  left_join(
    list(pre_lab, fu_lab) |>
      lapply(select, usubjid, bld_hct_min, bld_plt, bld_fibrinogen, bld_tqinr) |>
      do.call(rbind, args=_) |>
      group_by(usubjid) |>
      summarise(
        across(where(is.numeric) & !usubjid & !bld_tqinr, min, na.rm=TRUE),
        across(bld_tqinr, max,  na.rm=TRUE)
      )
  )

## Heart ----

heart <-
  comp |>
  filter(isheartfailure %in% 'Y') |>
  transmute(
    usubjid=usubjid,
    troponinI = case_when(
      hftroponin == '<10' ~ 'Lower than 10',
      as.numeric(hftroponin) < 10 ~ 'Lower than 10',
      as.numeric(hftroponin) < 100 ~ '10 - 100',
      as.numeric(hftroponin) > 100 ~ 'Higher than 100',
      .default = 'Unknown'
    ) |>
      factor(levels=  c('Lower than 10', '10 - 100', 'Higher than 100', 'Unknown')),
    abnormal_ecg = !is.na(hfecg)
  )

## Brain ----

brain <-
  comp |>
  filter(isbi %in% 'Y') |>
  transmute(
    usubjid=usubjid,
    convulsion = biconvulsion %in% 'Y',
    fnp = bifnm %in% 'Y'
  )

## Kidney ----

kidney <-
  comp |>
  filter(iskidneydamage %in% 'Y') |>
  transmute(
    usubjid=usubjid,
    tx_crrt =  kdcrrt %in% 'Y',
    tx_intubation = kdnkq %in% 'Y'
  ) |>
  left_join(
    list(bsl_clin, fu_clin) |>
      lapply(select, usubjid, clin_sbp) |>
      do.call(rbind, args=_) |>
      group_by(usubjid) |>
      summarise(
        across(where(is.numeric) & !usubjid, min, na.rm=TRUE),
      )
  ) |>
  left_join(
    list(pre_lab, fu_lab) |>
      lapply(select, usubjid, kid_ure, kid_cre) |>
      do.call(rbind, args=_) |>
      group_by(usubjid) |>
      summarise(
        across(where(is.numeric) & !usubjid, min, na.rm=TRUE)
      )
  )
    
# Save ---
save(list=ls(), file='../data/vad/69DX_vad.Rdata')
