library(Amelia)
library(haven)

# 1. Load Stata data
dat <- read_dta(".dta") |> haven::zap_labels()

# 2. Run Amelia with one imputation (m = 1)
a_out <- amelia(
  dat,
  m = 1,                       # single completed dataset
  ts = "year",                 # time variable 
  cs = "h_pid",                # cross-sectional unit ID 
  bounds = matrix(c(which(names(dat)=="Wh"), 0, Inf), nrow = 1),
  noms = c("sex","married","week","law",
           "compsize","h_eco4","educ","region","f_educ","m_educ"),
  polytime = 2,                # include quadratic time trend

)

# 3. Extract the completed dataset
imp1 <- a_out$imputations[[1]]

# 4. Save back to Stata
write_dta(imp1, ".dta")
