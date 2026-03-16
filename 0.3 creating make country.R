source(here::here("anc testing/0.1 extracting pjnz functions.R"))

# ---- make country creation ----
# obtain full pjnz list
pjnz_list = list.files(pattern = "\\.pjnz$", full.names = TRUE,recursive = T,ignore.case = T)

# combine multi regional files together and remove from general list
pjnz_ethiopia = pjnz_list[grepl("Ethiopia",pjnz_list)]
pjnz_list = pjnz_list[!grepl("Ethiopia",pjnz_list)]
pjnz_kenya = pjnz_list[grepl("Kenya",pjnz_list)]
pjnz_list = pjnz_list[!grepl("Kenya",pjnz_list)]
pjnz_zimbabwe = pjnz_list[grepl("Zimbabwe",pjnz_list)]
pjnz_list = pjnz_list[!grepl("Zimbabwe",pjnz_list)]

# make a series of lists to then be able to perform further analysis with extraction functions
pjnz_list = as.list(pjnz_list)
pjnz_list_cnt = pjnz_list
pjnz_list[["Ethiopia"]] = as.list(pjnz_ethiopia)
pjnz_list[["Kenya"]] = as.list(pjnz_kenya)
pjnz_list[["Zimbabwe"]] = as.list(pjnz_zimbabwe)
# make country for single national files
make_country = list()
for (i in 1:length(pjnz_list_cnt)) {
  cnt <- as.character(first90::read_country(pjnz_list_cnt[[i]]))
  make_country[[cnt]] = make_country_FUN(pjnz_list_cnt[[i]])
 
}

# make country for regional files
make_country$Ethiopia = make_country_FUN(pjnz_list[["Ethiopia"]])
make_country$Kenya = make_country_FUN(pjnz_list[["Kenya"]])
make_country$Zimbabwe = make_country_FUN(pjnz_list[["Zimbabwe"]])
# clear surveys to allow for national surveys
make_country$Ethiopia$survey_hts = make_country$`Cape Verde`$survey_hts 
make_country$Kenya$survey_hts = make_country$`Cape Verde`$survey_hts
make_country$Zimbabwe$survey_hts = make_country$`Cape Verde`$survey_hts

#sort make country so it is in alphabetical order
make_country <- make_country[order(names(make_country))]


# ---- creating program data for South africa(may be depricated for 2025 files) ----

#data is retrived from thembisa 4.8
#table B1 pg 168
total <- c(
  1484963, 1615201, 1745439, 2174750, 2417331, 2746824, 3427448,
  7614416, 10376224, 9633233, 9847819, 8208788, 9514084, 12202185,
  13347568, 12784658, 13596451, 17203267, 12938499, 15371573, 16091837
)
names(total) = 2003:2023
#table B3 hiv prevalance for HIV tests
postive_percent <- c(
  25.24, 26.97, 25.34, 24.73, 24.46, 15.71, 14.34,
  9.10, 8.08, 7.16, 6.11, 4.86, 3.86, 3.20, 2.77
)/100
percent_years <- c(
  "2005", "2006", "2007", "2008", "2009",
  "2011", "2013", "2016", "2017", "2018",
  "2019", "2020", "2021", "2022", "2023"
)
names(postive_percent) = as.numeric(percent_years)

postive_tests = total[names(postive_percent)] * postive_percent

#table B4 pg 171
df <- data.frame(
  Year = c("2021", "2022", "2023", "2024"),
  Males_15_24 = c(6.8, 7.1, 7.6, 8.0)/100,
  Females_15_24_excl_ANC = c(23.4, 25.7, 23.6, 23.5)/100,
  Ages_25_49_excl_ANC = c(50.1, 50.4, 52.1, 51.6)/100,
  Ages_50_plus = c(12.2, 10.9, 11.4, 11.9)/100,
  First_ANC_tests = c(7.5, 5.9, 5.3, 4.9)/100
)
#table b5 pg 171
df_pos <- data.frame(
  Year = c("2021", "2022", "2023", "2024"),
  Males_15_24 = c(2.78, 1.75, 1.38, 1.22)/100,
  Females_15_24_excl_ANC = c(2.49, 1.86, 1.55, 1.37)/100,
  Ages_25_49_excl_ANC = c(5.00, 4.33, 3.87, 3.38)/100,
  Ages_50_plus = c(2.93, 2.80, 2.58, 2.43)/100,
  First_ANC_tests = c(7.76, 6.70, 5.78, 5.08)/100
)

df$non_anc = NULL 


df_tests = df[1:3,2:6] * total[c("2021","2022","2023")]
df_tests$year = c("2021","2022","2023")
df_tests = df_tests[,c(6,1:5)]
df_tests$VCT = rowSums(df_tests[,2:5])


df_pos_tests = df_tests[,2:6]*df_pos[1:3,2:6]
df_pos_tests$year = c("2021","2022","2023")
df_pos_tests = df_pos_tests[,c(6,1:5)]
df_pos_tests$VCT = rowSums(df_pos_tests[,2:5])


prgm_dat_sa = make_country$`South Africa`$prgm_dat
prgm_dat_sa[1:21,2] = 2003:2023
prgm_dat_sa$country = "South Africa"
prgm_dat_sa$sex = "both"
prgm_dat_sa$agegr = "15-99"
prgm_dat_sa$tot = total
prgm_dat_sa$totpos[prgm_dat_sa$year %in% names(postive_tests[1:12])] = (postive_tests[1:12])
prgm_dat_sa$vct[19:21] = df_tests$VCT
prgm_dat_sa$anc[19:21] = df_tests$First_ANC_tests

prgm_dat_sa$vctpos[19:21] = df_pos_tests$VCT
prgm_dat_sa$ancpos[19:21] = df_pos_tests$First_ANC_tests

prgm_dat_sa$tot[19:21] = prgm_dat_sa$vct[19:21] + prgm_dat_sa$anc[19:21]
prgm_dat_sa$totpos[19:21] = prgm_dat_sa$vctpos[19:21] + prgm_dat_sa$ancpos[19:21]

make_country$`South Africa`$prgm_dat = prgm_dat_sa
# ---- align program data in congo to spectrum ----#

tot_congo <- t(matrix(c(
  45990, 49455, 51540, 23341, 89216, 130514,
  1963,  1697,  2106,  3178,  4347,  11547,
  15568, 10552, 16507, 22266, 36689,  65578,
  1244,  1034,  1454,  2540,  3315,  10061
), nrow = 4, byrow = TRUE))

make_country$Congo$prgm_dat[4:9,5:8] = tot_congo
make_country$Congo$prgm_dat[1:3,5:8] = NA

# ---- error correction in programme data(may be depricated for 2025 files) ----
#Rwanda
make_country$Rwanda$prgm_dat$sex[make_country$Rwanda$prgm_dat$sex == "male"] = "female"
make_country$Rwanda$prgm_dat$sex[make_country$Rwanda$prgm_dat$sex == "both"] = "male"
#only makes sense if it was male = female and both = male

#burundi
#also has errors for pre 2010, correct test values(?) but no posives so remvoed
make_country$Burundi$prgm_dat = make_country$Burundi$prgm_dat[-(1:7),] 

#cape verde missings
#make_country$`Cape Verde`$prgm_dat

#niger same as Rwanda
make_country$Niger$prgm_dat$sex[make_country$Niger$prgm_dat$sex == "male"] = "female"
make_country$Niger$prgm_dat$sex[make_country$Niger$prgm_dat$sex == "both"] = "male"
#same structure

#Gabon same as Rwanda
make_country$Gabon$prgm_dat$sex[make_country$Gabon$prgm_dat$sex == "male"] = "female"
make_country$Gabon$prgm_dat$sex[make_country$Gabon$prgm_dat$sex == "both"] = "male"

# ---- adjust programme data for ANC to use the PMTCT module values(may be depricated for 2025 files) ----

path_anc = here::here("anc testing")

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <- readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))

for (i in 1:length(make_country)) {

  cnt = names(make_country)[i]
  print(cnt)
  anc_tests = pmtct_list[[cnt]]$anc_test

  if(dim(make_country[[cnt]]$prgm_dat)[1]==0){
    print("empty prgdat")
    print(cnt)
    next
  }
  if(cnt %in% "South Africa"){
    next
  }
  if(cnt %in% "Swaziland"){
    anc_tests = pmtct_list[["eSwatini"]]$anc_test
    
  }
  
  anc_tests_tot = anc_tests[c("anc_tested","anc_known_neg"),]
  anc_tests_tot["anc_known_neg",is.na(anc_tests_tot["anc_known_neg",])] = 0
  anc_tests_tot = colSums(anc_tests_tot)

  anc_tests_pos = anc_tests[c("anc_tested_pos"),]
  anc_tests_tot[anc_tests_tot == 0] = NA
  anc_tests_pos[anc_tests_pos == 0] = NA

  anctests = rbind(anc_tests_tot,anc_tests_pos)
  prg_dat = make_country[[cnt]]$prgm_dat

  years = as.numeric(names(anc_tests_tot[!is.na(anc_tests_tot)]))
  if(length(years)>length(prg_dat$year)){
    prgm_dat2 = prg_dat
    prg_dat[1:length(years),"year"] = years
    prg_dat[,"sex"] = "both"

    prg_dat[1:dim(prg_dat)[1],c("country","sex","agegr","tot","totpos",
                                "vct","vctpos","anc","ancpos")] = NA

    prg_dat <- full_join(prg_dat[,c("year","sex")], prgm_dat2, by = "year")

    prg_dat[,] = prg_dat[,c("country","year","sex.y","agegr","tot","totpos",
                            "vct","vctpos","anc","ancpos")]
    names(prg_dat) = c("country","year","sex","agegr","tot","totpos",
                       "vct","vctpos","anc","ancpos")
    prg_dat$sex = "both"


  }else{
    prg_dat = prg_dat[prg_dat$year %in% years,]
  }


  prg_dat$anc = unlist(anctests[1,!is.na(anctests[1,])])[as.character(prg_dat$year)]
  prg_dat$ancpos = unlist(anctests[2,!is.na(anctests[1,])])[as.character(prg_dat$year)]

  prg_dat[prg_dat$sex %in% "male",9:10] = NA

  prgm_dat = make_country[[cnt]]$prgm_dat

  prg_dat2 <- full_join(prg_dat[,c("year","sex","anc","ancpos")],
                        prgm_dat[,c("country","year","sex","agegr","tot","totpos","vct","vctpos")],
                        by = c("year","sex")) %>%
    arrange(year)

  prg_dat3 = prg_dat2[,c("country","year","sex","agegr","tot","totpos",
                         "vct","vctpos","anc","ancpos")]

  names(prg_dat3) = c("country","year",
                      "sex","agegr","tot","totpos",
                      "vct","vctpos","anc","ancpos")
  prg_dat3$country = cnt
  prg_dat3$agegr = "15-99"

  make_country[[cnt]]$prgm_dat = prg_dat3
}



# fixing vct stratification for just gabon, and swaziland
make_country$Swaziland$prgm_dat$vct = make_country$Swaziland$prgm_dat$tot-make_country$Swaziland$prgm_dat$anc
make_country$Swaziland$prgm_dat$vctpos = make_country$Swaziland$prgm_dat$totpos-make_country$Swaziland$prgm_dat$ancpos

#benin, adjust the anc to be NA for 2011-2013, total and ANC dont line up
make_country$Benin$prgm_dat$ancpos[1:3] = NA
make_country$Benin$prgm_dat$anc[1:3] = NA
make_country$Benin$prgm_dat$totpos[1:3] = NA
make_country$Benin$prgm_dat$tot[1:3] = NA

#adjust so VCT present for gabon and guniea 
make_country$Gabon$prgm_dat$vct = make_country$Gabon$prgm_dat$tot-
  ifelse(is.na(make_country$Gabon$prgm_dat$anc),0,make_country$Gabon$prgm_dat$anc)
make_country$Gabon$prgm_dat$vctpos = make_country$Gabon$prgm_dat$totpos-
  ifelse(is.na(make_country$Gabon$prgm_dat$ancpos),0,make_country$Gabon$prgm_dat$ancpos)

#guinea bissau missing 2023
make_country$`Guinea-Bissau`$prgm_dat$vctpos[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023] =  
  make_country$`Guinea-Bissau`$prgm_dat$totpos[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023] - 
  make_country$`Guinea-Bissau`$prgm_dat$ancpos[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023]

make_country$`Guinea-Bissau`$prgm_dat$vct[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023] =  
  make_country$`Guinea-Bissau`$prgm_dat$tot[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023] - 
  make_country$`Guinea-Bissau`$prgm_dat$anc[make_country$`Guinea-Bissau`$prgm_dat$year %in% 2023]




# ---- fixing survey discrepancies in the 2024 spectrum files(using master survey from first90)(may be depricated for 2025 files) ----

# cote divore: removed 2005 15-24 male positve, new 2025 file still missing it
# burkina faso: new 2025 file
# senegal: missing half the 205,2010,1017 dhs( no positives at all)
# gambia: missing agre strat in 2018 mics
# cabo verde: missing 2005
# uganda: gained 2020 but lost 2016 male strat??
# mozambique: added 2021, removed all others(2004,2008,2018)
# lesotho: removed positves and 15-49(also fake(?), counts for phia(NA vs 		 2126))
# Zimbabwe
# Kenya
# Ethiopia

# loads a master survey file with extracted survey documents
load(paste0(here::here("surveys/survey_hts_master.rda")))
surveys_africa = subset(survey_hts_master, country %in% c(names(make_country),
                                                          "Cabo Verde",
                                                          "Central African Republic",
                                                          "Tanzania",
                                                          "Sao Tome and Principe",
                                                          "Namibia",
                                                          "Cote d'Ivoire",
                                                          "Cameroon"))

age_group =  c('15-24','25-34','35-49','15-49')
#cote Divore
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Cote d'Ivoire")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")

# adds new surveys
cote = read.csv("surveys/CotedIvoire_Shiny90_Enquete_2025-01-29.csv")
names(cote) = names(survey_hts_master)[c(2,1,3,6,5,4,7,8,9,10,11)]
cote$outcome = "evertest"                                         
cote$est = cote$est/100
cote$ci_l = cote$ci_l/100
cote$ci_u = cote$ci_u/100
cote$se = cote$se/100
surv2 = subset(cote,country %in% cnt2)
surv2 = subset(surv2,agegr %in% age_group)
surv2 = subset(surv2,outcome %in% "evertest")
surv2 = subset(surv2,year %in% 2021)


#combine 2 survey components
make_country$`Côte d'Ivoire`$survey_hts = rbind(surv1,surv2)
make_country$`Côte d'Ivoire`$survey_hts$country = "Cote-d'Ivoire"
make_country$`Côte d'Ivoire`$cnt = "Côte d'Ivoire"



#adjust South Sudan gender
make_country$`South Sudan`$survey_hts$sex == "female"

#Burkina Faso
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Burkina Faso")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
# adds new surveys
burkina = read.csv("surveys/BurkinaFaso_BF2021DHS_2025-01-28.csv")
names(burkina) = names(survey_hts_master)[c(2,1,3,6,5,4,7,8,9,10,11)]
burkina$outcome = "evertest"                                         
burkina$est = burkina$est/100
burkina$ci_l = burkina$ci_l/100
burkina$ci_u = burkina$ci_u/100
burkina$se = burkina$se/100


surv2 = subset(burkina,country %in% cnt2)
surv2 = subset(surv2,agegr %in% age_group)
surv2 = subset(surv2,outcome %in% "evertest")
surv2 = subset(surv2,year %in% 2021)

#combine 2 survey components
make_country$`Burkina Faso`$survey_hts = rbind(surv1,surv2)

#senegal
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Senegal")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$Senegal$survey_hts = surv1

#gambia
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Gambia")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$Gambia$survey_hts = surv1

#`Cape Verde`
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Cabo Verde")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$`Cape Verde`$survey_hts = surv1
make_country$`Cape Verde`$survey_hts$country = "Cape Verde"

#uganda
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Uganda")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
surv2 = subset(make_country$Uganda$survey_hts, year >2017)
make_country$Uganda$survey_hts = rbind(surv1,surv2)

#Madagascar
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Madagascar")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
surv2 = subset(make_country$Madagascar$survey_hts, year >2017)
make_country$Madagascar$survey_hts = rbind(surv1,surv2)


#Mozambique
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Mozambique")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
surv2 = subset(make_country$Mozambique$survey_hts, year >2017)
# current Ci_u is 0, swap it for approximatiom of Ci_u using se
surv2[c(surv2$sex == "male" & surv2$agegr == "35-49" & surv2$hivstatus == "positive") ,"ci_u"] = surv2[c(surv2$sex == "male" & surv2$agegr == "35-49" & surv2$hivstatus == "positive") ,"est"] + surv2[c(surv2$sex == "male" & surv2$agegr == "35-49" & surv2$hivstatus == "positive") ,"se"]*1.96 
make_country$Mozambique$survey_hts = rbind(surv1,surv2)


#lesotho
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Lesotho")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
surv2 = subset(make_country$Lesotho$survey_hts, year >2017)
make_country$Lesotho$survey_hts = rbind(surv1,surv2)
# set counts for PHIA to NA( unlikely to be 2126 for all categories)
make_country$Lesotho$survey_hts$counts[143:154] = NA


#Zimbabwe
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Zimbabwe")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$Zimbabwe$survey_hts = surv1

#Kenya
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Kenya")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$Kenya$survey_hts = surv1

#Ethiopia
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "Ethiopia")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$Ethiopia$survey_hts = surv1

#south africa
cnt2 = unique(surveys_africa$country)[which(unique(surveys_africa$country) == "South Africa")]
# returns the missing survey
surv1 = subset(surveys_africa,country %in% cnt2)
surv1 = subset(surv1,agegr %in% age_group)
surv1 = subset(surv1,outcome %in% "evertest")
make_country$`South Africa`$survey_hts = surv1

for (i in 1:length(make_country)) {
  survey_hts1 = make_country[[i]]$survey_hts
  print(names(make_country)[i])
  #print(summary(survey_hts1[,c("est","ci_u","ci_l")]))
}
#correct Niger upper ci
make_country$Niger$survey_hts$ci_u[c(43,48)] = 1.00
# further corrections (based on master file)
make_country$Niger$survey_hts[(make_country$Niger$survey_hts$year == 2006 & 
                                 make_country$Niger$survey_hts$sex == "male"&
                                 make_country$Niger$survey_hts$agegr == "15-24"&
                                 make_country$Niger$survey_hts$hivstatus == "positive"),
                              7:10] = c(0.00500000, 0.20916501, 0.000000000, 0.4099559)

make_country$Niger$survey_hts[(make_country$Niger$survey_hts$year == 2006 & 
                                 make_country$Niger$survey_hts$sex == "female"&
                                 make_country$Niger$survey_hts$agegr == "35-49"&
                                 make_country$Niger$survey_hts$hivstatus == "positive"),
                              7:10] = c(0.00500000, 0.07268548, 0.000000000, 0.14246091)


make_country$Niger$survey_hts[(make_country$Niger$survey_hts$year == 2012 & 
                                 make_country$Niger$survey_hts$sex == "male"&
                                 make_country$Niger$survey_hts$agegr == "25-34"&
                                 make_country$Niger$survey_hts$hivstatus == "positive"),
                              7:10] = c(0.00500000, 0.17320508, 0.000000000, 0.3394757)



#correct malawi Upper ci
make_country$Malawi$survey_hts[172,]$ci_u = 1.00

# correct Botstana upper CI
#0.962853+0.006586*1.96
make_country$Botswana$survey_hts$ci_u[(make_country$Botswana$survey_hts$year %in% 2021 & make_country$Botswana$survey_hts$agegr %in% "35-49" & make_country$Botswana$survey_hts$sex %in% "male" & make_country$Botswana$survey_hts$hivstatus %in% "all")] = 0.962853+0.006586*1.96

# Rename Swaziland
make_country[["Swaziland"]]$cnt = "eSwatini"
make_country[["eSwatini"]] = make_country$Swaziland
make_country$Swaziland = NULL

# remove Swaziland, swapped for eSwatini
make_country = make_country[!(names(make_country) %in% ("Swaziland"))]
# 
# # remove cabo verde and mauriania, no ANC data
make_country = make_country[!(names(make_country) %in% c("Mauritania","Cape Verde"))]

# ---- saving ----
saveRDS(object = make_country, file = here::here('anc testing/data/make_country_presimul.rds'))


