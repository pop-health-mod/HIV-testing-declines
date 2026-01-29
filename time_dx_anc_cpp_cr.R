Rcpp::cppFunction('List time_to_dx_cr_cpp(List s_fp,
                  IntegerVector year,
                  std::string age,
                  std::string sex,
                  std::string test_ever,
                  double dt) {
                  
                  int n_year = year.size();
                  int ind_age;
                  int ind_sex;
                  int ind_th;
                  int ind_anc_th;
                  
                  List ss = s_fp["ss"];
                  int proj_start = ss["proj_start"];
                  NumericVector infections = s_fp["infections"];
                  NumericVector diagn_rate = s_fp["diagn_rate"];
                  NumericVector anc_diagn_rate = s_fp["anc_diagn_rate"];
                  NumericVector cd4_init = s_fp["cd4_initdist"];
                  // age index
                  if (age == "15-24") {
                  ind_age = 0;
                  } else if (age == "25-34") {
                  ind_age = 3;
                  } else if (age == "35-49") {
                  ind_age = 5;
                  } else {
                  ind_age = 8;
                  }
                  // sex index
                  if (sex == "male") {
                  ind_sex = 0 ;
                  } else {
                  ind_sex = 1;
                  }
                  // ever tested index
                  if (test_ever == "never") {
                  ind_th = 0;
                  ind_anc_th = 0;
                  } else if (test_ever == "ever"){
                  ind_th = 1;
                  ind_anc_th = 0;
                  } else if (test_ever == "ever_anc"){
                  ind_th = 1;
                  ind_anc_th = 1;
                  
                  }
                  
                  // average number of years in a CD4 category
                  NumericVector fpcd4 = s_fp["cd4_prog"];
                  IntegerVector ind_cd4 = IntegerVector::create(
                  (6 * 9 * ind_sex) + (6 * ind_age) + 0,
                  (6 * 9 * ind_sex) + (6 * ind_age) + 1,
                  (6 * 9 * ind_sex) + (6 * ind_age) + 2,
                  (6 * 9 * ind_sex) + (6 * ind_age) + 3,
                  (6 * 9 * ind_sex) + (6 * ind_age) + 4,
                  (6 * 9 * ind_sex) + (6 * ind_age) + 5);
                  NumericVector cd4_prg = fpcd4[ind_cd4];
                  
                  // death rates by CD4 category
                  NumericVector fpcd4mort = s_fp["cd4_mort"];
                  IntegerVector ind_nmd15 = IntegerVector::create(
                  (7 * 9 * ind_sex) + (7 * 0) + 0,
                  (7 * 9 * ind_sex) + (7 * 0) + 1,
                  (7 * 9 * ind_sex) + (7 * 0) + 2,
                  (7 * 9 * ind_sex) + (7 * 0) + 3,
                  (7 * 9 * ind_sex) + (7 * 0) + 4,
                  (7 * 9 * ind_sex) + (7 * 0) + 5,
                  (7 * 9 * ind_sex) + (7 * 0) + 6);
                  NumericVector cd4_mort15 = fpcd4mort[ind_nmd15];
                  IntegerVector ind_nmd25 = IntegerVector::create(
                  (7 * 9 * ind_sex) + (7 * 3) + 0,
                  (7 * 9 * ind_sex) + (7 * 3) + 1,
                  (7 * 9 * ind_sex) + (7 * 3) + 2,
                  (7 * 9 * ind_sex) + (7 * 3) + 3,
                  (7 * 9 * ind_sex) + (7 * 3) + 4,
                  (7 * 9 * ind_sex) + (7 * 3) + 5,
                  (7 * 9 * ind_sex) + (7 * 3) + 6);
                  NumericVector cd4_mort25 = fpcd4mort[ind_nmd25];
                  IntegerVector ind_nmd35 = IntegerVector::create(
                  (7 * 9 * ind_sex) + (7 * 5) + 0,
                  (7 * 9 * ind_sex) + (7 * 5) + 1,
                  (7 * 9 * ind_sex) + (7 * 5) + 2,
                  (7 * 9 * ind_sex) + (7 * 5) + 3,
                  (7 * 9 * ind_sex) + (7 * 5) + 4,
                  (7 * 9 * ind_sex) + (7 * 5) + 5,
                  (7 * 9 * ind_sex) + (7 * 5) + 6);
                  NumericVector cd4_mort35 = fpcd4mort[ind_nmd35];
                  IntegerVector ind_nmd50 = IntegerVector::create(
                  (7 * 9 * ind_sex) + (7 * 8) + 0,
                  (7 * 9 * ind_sex) + (7 * 8) + 1,
                  (7 * 9 * ind_sex) + (7 * 8) + 2,
                  (7 * 9 * ind_sex) + (7 * 8) + 3,
                  (7 * 9 * ind_sex) + (7 * 8) + 4,
                  (7 * 9 * ind_sex) + (7 * 8) + 5,
                  (7 * 9 * ind_sex) + (7 * 8) + 6);
                  NumericVector cd4_mort50 = fpcd4mort[ind_nmd50];
                  // initial cd4 cell count distribution
                  double cd4_init_1 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 0];
                  double cd4_init_2 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 1];
                  double cd4_init_3 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 2];
                  double cd4_init_4 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 3];
                  double cd4_init_5 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 4];
                  double cd4_init_6 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 5];
                  double cd4_init_7 = cd4_init[(7 * 9 * ind_sex) + (7 * ind_age) + 6];
                  // for probability of being Dx or dead before certain time (expressed in dt)
                  int tind6mo = nearbyint(0.5 / dt);                  
                  int tind1yr = nearbyint(1 / dt);
                  int tind2yr = nearbyint(2 / dt);
                  int tind5yr = nearbyint(5 / dt);
                  
                  // to store the results
                  NumericVector time_dx_avg(n_year);
                  NumericVector time_dx_med(n_year);
                  NumericVector prb6mo(n_year);
                  NumericVector prb1yr(n_year);
                  NumericVector prb2yr(n_year);
                  NumericVector prb5yr(n_year);
                  NumericVector prb500cd4(n_year);
                  NumericVector prb350cd4(n_year);
                  NumericVector propdx(n_year);
                  
                  // we calculate for each year using a loop
                  for (int n = 0; n < n_year; ++n) {
                  int ind_yr = year[n] - proj_start;
                  
                  // what is the average age of incident HIV infection
                  NumericVector age_15 = NumericVector::create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
                  NumericVector inf_15 = infections[seq(66 * 2 * ind_yr + (ind_sex * 66), 66 * 2 * ind_yr + (ind_sex * 66) + 9)];
                  NumericVector nw15 = age_15 * inf_15;
                  double n15 = sum(nw15);
                  double avg_inf15 = 14 + n15 / sum(inf_15);
                  NumericVector age_25 = NumericVector::create(11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
                  NumericVector inf_25 = infections[seq(66 * 2 * ind_yr + (ind_sex * 66) + 10, 66 * 2 * ind_yr + (ind_sex * 66) + 19)];
                  NumericVector nw25 = age_25 * inf_25;
                  double n25 = sum(nw25);
                  double avg_inf25 = 14 + n25 / sum(inf_25);
                  NumericVector age_35 = NumericVector::create(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35);
                  NumericVector inf_35 = infections[seq(66 * 2 * ind_yr + (ind_sex * 66) + 20, 66 * 2 * ind_yr + (ind_sex * 66) + 34)];
                  NumericVector nw35 = age_35 * inf_35;
                  double n35 = sum(nw35);
                  double avg_inf35 = 14 + n35 / sum(inf_35);
                  
                  // testing rates by CD4 category index for DIAGNrate
                  IntegerVector ind_nm15 = IntegerVector::create(
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 0,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 1,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 2,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 3,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 4,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 5,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + 6);
                  
                  // testing rates by CD4 category, index for ANC
                  IntegerVector ind_nm15_anc = IntegerVector::create(
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 0,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 1,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 2,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 3,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 4,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 5,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + 6);
                  
                  // use the index to find testing rates for anc and non-anc testing, male anc diagn is 0
                  NumericVector nmt15 = diagn_rate[ind_nm15] + anc_diagn_rate[ind_nm15_anc];
                  
                  IntegerVector ind_nm25 = IntegerVector::create(
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 0,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 1,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 2,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 3,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 4,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 5,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 3) + 6);
                  
                  IntegerVector ind_nm25_anc = IntegerVector::create(
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 0,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 1,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 2,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 3,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 4,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 5,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 3) + 6);
                  
                  NumericVector nmt25 = diagn_rate[ind_nm25] + anc_diagn_rate[ind_nm25_anc];
                  
                  IntegerVector ind_nm35 = IntegerVector::create(
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 0,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 1,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 2,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 3,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 4,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 5,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 5) + 6);
                  
                  IntegerVector ind_nm35_anc = IntegerVector::create(
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 0,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 1,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 2,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 3,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 4,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 5,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 5) + 6);
                  
                  NumericVector nmt35 = diagn_rate[ind_nm35] + anc_diagn_rate[ind_nm35_anc];
                  
                  IntegerVector ind_nm50 = IntegerVector::create(
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 0,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 1,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 2,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 3,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 4,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 5,
                  (7 * 9 * 2 * 4 * ind_yr) + (7 * 9 * 2 * ind_th) + (7 * 9 * ind_sex) + (7 * 8) + 6);
                  
                  IntegerVector ind_nm50_anc = IntegerVector::create(
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 0,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 1,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 2,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 3,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 4,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 5,
                  (7 * 9 * 2 * 5 * ind_yr) + (7 * 9 * 2 * ind_anc_th) + (7 * 9 * ind_sex) + (7 * 8) + 6);
                  
                  NumericVector nmt50 = diagn_rate[ind_nm50] + anc_diagn_rate[ind_nm50_anc];
                  
                  
                  // Creating vector for time and testing rates
                  IntegerVector time_int_ = seq(1, 50 / dt);
                  NumericVector time_int = as<NumericVector>(time_int_) * dt;
                  int vec_l = time_int.size();
                  NumericVector time_t(vec_l);
                  NumericVector test_rate_t(vec_l * 7);
                  NumericVector mort_rate_t(vec_l * 7);
                  
                  if (age == "15-24") {
                  // int ind15 = std::round((25 - avg_inf15) / dt);
                  // int ind25 = std::round(ind15 + 10 / dt);
                  // int ind35 = std::round(ind25 + 15 / dt);
                  int ind15 = floor((25 - avg_inf15) / dt + 0.5);
                  int ind25 = floor(ind15 + 10 / dt + 0.5);
                  int ind35 = floor(ind25 + 15 / dt + 0.5);
                  for (int j = 0; j < vec_l; ++j) {
                  IntegerVector ind_j = seq(j * 7 + 0, j * 7 + 6);
                  if (j < ind15) {
                  test_rate_t[ind_j] = nmt15;
                  mort_rate_t[ind_j] = cd4_mort15;
                  } else if (j >= ind15 && j < ind25) {
                  test_rate_t[ind_j] = nmt25;
                  mort_rate_t[ind_j] = cd4_mort25;
                  } else if (j >= ind25 && j < ind35) {
                  test_rate_t[ind_j] = nmt35;
                  mort_rate_t[ind_j] = cd4_mort35;
                  } else {
                  test_rate_t[ind_j] = nmt50;
                  mort_rate_t[ind_j] = cd4_mort50;
                  }}
                  } else if (age == "25-34") {
                  // int ind25 = std::round((35 - avg_inf25) / dt);
                  // int ind35 = std::round(ind25 + 15 / dt);
                  int ind25 = floor((35 - avg_inf25) / dt + 0.5);
                  int ind35 = floor(ind25 + 15 / dt + 0.5);
                  for (int j = 0; j < vec_l; ++j) {
                  IntegerVector ind_j = seq(j * 7 + 0, j * 7 + 6);
                  if (j < ind25) {
                  test_rate_t[ind_j] = nmt25;
                  mort_rate_t[ind_j] = cd4_mort25;
                  } else if (j >= ind25 && j < ind35) {
                  test_rate_t[ind_j] = nmt35;
                  mort_rate_t[ind_j] = cd4_mort35;
                  } else {
                  test_rate_t[ind_j] = nmt50;
                  mort_rate_t[ind_j] = cd4_mort50;
                  }}
                  } else if (age == "35-49") {
                  // int ind35 = std::round((50 - avg_inf35) / dt);
                  int ind35 = floor((50 - avg_inf35) / dt + 0.5);
                  for (int j = 0; j < vec_l; ++j) {
                  IntegerVector ind_j = seq(j * 7 + 0, j * 7 + 6);
                  if (j < ind35) {
                  test_rate_t[ind_j] = nmt35;
                  mort_rate_t[ind_j] = cd4_mort35;
                  } else {
                  test_rate_t[ind_j] = nmt50;
                  mort_rate_t[ind_j] = cd4_mort50;
                  }}
                  } else {
                  for (int j = 0; j < vec_l; ++j) {
                  IntegerVector ind_j = seq(j * 7 + 0, j * 7 + 6);
                  test_rate_t[ind_j] = nmt50;
                  mort_rate_t[ind_j] = cd4_mort50;
                  }}
                  // we initialize the model for competing risk
                  NumericVector X(7 * vec_l);
                  NumericVector nb_dx(7 * vec_l);
                  NumericVector person_time(vec_l);
                  NumericVector nb_death(vec_l);
                  NumericVector nb_dx_tot(vec_l);
                  NumericVector tot_pop(vec_l);
                  X[0] = cd4_init_1;
                  X[1] = cd4_init_2;
                  X[2] = cd4_init_3;
                  X[3] = cd4_init_4;
                  X[4] = cd4_init_5;
                  X[5] = cd4_init_6;
                  X[6] = cd4_init_7;
                  // loop for competing risk of cd4 progression, HIV-death, and diagnosis
                  for (int i = 1; i < vec_l; ++i) {
                  int t_i = i - 1;
                  IntegerVector ind_t = seq(i * 7 + 0, i * 7 + 6);
                  
                  X[i * 7 + 0] = X[t_i * 7 + 0] + dt * ( - (cd4_prg[0] + mort_rate_t[i * 7 + 0] + test_rate_t[i * 7 + 0]) * X[t_i * 7 + 0]);
                  X[i * 7 + 1] = X[t_i * 7 + 1] + dt * (cd4_prg[0] * X[t_i * 7 + 0] - 
                  (cd4_prg[1] + mort_rate_t[i * 7 + 1] + test_rate_t[i * 7 + 1]) * X[t_i * 7 + 1]);
                  X[i * 7 + 2] = X[t_i * 7 + 2] + dt * (cd4_prg[1] * X[t_i * 7 + 1] - 
                  (cd4_prg[2] + mort_rate_t[i * 7 + 2] + test_rate_t[i * 7 + 2]) * X[t_i * 7 + 2]);
                  X[i * 7 + 3] = X[t_i * 7 + 3] + dt * (cd4_prg[2] * X[t_i * 7 + 2] - 
                  (cd4_prg[3] + mort_rate_t[i * 7 + 3] + test_rate_t[i * 7 + 3]) * X[t_i * 7 + 3]);
                  X[i * 7 + 4] = X[t_i * 7 + 4] + dt * (cd4_prg[3] * X[t_i * 7 + 3] - 
                  (cd4_prg[4] + mort_rate_t[i * 7 + 4] + test_rate_t[i * 7 + 4]) * X[t_i * 7 + 4]);
                  X[i * 7 + 5] = X[t_i * 7 + 5] + dt * (cd4_prg[4] * X[t_i * 7 + 4] - 
                  (cd4_prg[5] + mort_rate_t[i * 7 + 5] + test_rate_t[i * 7 + 5]) * X[t_i * 7 + 5]);
                  X[i * 7 + 6] = X[t_i * 7 + 6] + dt * (cd4_prg[5] * X[t_i * 7 + 5] - 
                  (mort_rate_t[i * 7 + 6] + test_rate_t[i * 7 + 6]) * X[t_i * 7 + 6]);
                  
                  NumericVector nb_death_z(7);
                  NumericVector nb_dx_z(7);
                  double tot_py_z = 0;
                  IntegerVector ind_t0 = seq(t_i * 7 + 0, t_i * 7 + 6);
                  for (int z = 0; z < 7; ++z) {
                  int ind_tz = ind_t0[z];
                  nb_dx[ind_tz + 7] = (X[ind_tz] * test_rate_t[ind_tz + 7]) * dt;
                  nb_dx_z[z] = (X[ind_tz] * test_rate_t[ind_tz + 7]) * dt;
                  nb_death_z[z] = (X[ind_tz] * mort_rate_t[ind_tz + 7]) * dt;
                  tot_py_z += X[ind_tz + 7];
                  }
                  nb_death[i] = sum(nb_death_z);
                  nb_dx_tot[i] = sum(nb_dx_z);
                  person_time[i - 1] = (tot_py_z + (nb_death[i] + nb_dx_tot[i]) / 2) * dt;
                  NumericVector tot_pop_i = X[ind_t];
                  tot_pop[i] = sum(tot_pop_i);
                  }
                  
                  // outcomes are stored here for each year
                  time_dx_avg[n] = sum(person_time);
                  // finding which.min()
                  NumericVector med_abs = abs(tot_pop - 0.5);
                  NumericVector::iterator which_min_id = std::min_element(med_abs.begin(), med_abs.end());
                  int median_id = which_min_id - med_abs.begin();
                  time_dx_med[n] = time_int[median_id];
                  NumericVector prb6mo_n = nb_dx_tot[seq(0, tind6mo - 1)];
                  NumericVector prb1yr_n = nb_dx_tot[seq(0, tind1yr - 1)];
                  NumericVector prb2yr_n = nb_dx_tot[seq(0, tind2yr - 1)];
                  NumericVector prb5yr_n = nb_dx_tot[seq(0, tind5yr - 1)];
                  prb6mo[n] = sum(prb6mo_n);
                  prb1yr[n] = sum(prb1yr_n);
                  prb2yr[n] = sum(prb2yr_n);
                  prb5yr[n] = sum(prb5yr_n);
                  IntegerVector ind_cd4_500 = seq(0, vec_l - 1) * 7;
                  IntegerVector ind_cd4_350 = ind_cd4_500 + 1;
                  NumericVector nb_dx_500 = nb_dx[ind_cd4_500];
                  NumericVector nb_dx_350 = nb_dx[ind_cd4_350];
                  prb500cd4[n] = sum(nb_dx_500);
                  prb350cd4[n] = sum(nb_dx_350) + sum(nb_dx_500);
                  propdx[n] = sum(nb_dx);
                  }
                  
                  DataFrame val = DataFrame::create(
                  Named("year") = year,
                  Named("age") = age,
                  Named("sex") = sex,
                  Named("time_dx_avg") = time_dx_avg,
                  Named("time_dx_med") = time_dx_med,
                  Named("prb6mo") = prb6mo,
                  Named("prb1yr") = prb1yr,
                  Named("prb2yr") = prb2yr,
                  Named("prb5yr") = prb5yr,
                  Named("prb500cd4") = prb500cd4,
                  Named("prb350cd4") = prb350cd4,
                  Named("propdx") = propdx);
                  
                  return(val);
                  }')
