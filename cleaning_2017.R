## ---------------------------------------------------------------------------------------------------------------------
df2017 <- read.csv("cps_2017.csv", sep=";")


## ----selecting variables manually-------------------------------------------------------------------------------------
df2017 <- df2017 %>% 
  dplyr::select(
        "year",
        "age",
        "female",
        "wbho",
        "wbhao",
        "wbhaom",
        "forborn",
        "citizen",
        "citstat",
        "arrived",
        "peinusyr",
        "penatvty",
        "pemntvty",
        "pefntvty",
        "hprsmort",
        "married",
        "marstat",
        "wkslyr",
        "clslyr",
        "ftptlyr",
        "wksrec",
        "wrk",
        "lkwrklyr",
        "lkwrk_wk",
        "lfstat",
        "empl",
        "unem",
        "nilf",
        #"cert",
        "certgov",
        "selfemp",
        "selfinc",
        "pubsect",
        "pubfed",
        "pubst",
        "publoc",
        "schenrl",
        "schhs",
        "schcol",
        "schft",
        "schpt",
        "state",
        "suburb",
        "rural",
        "educ92",
        "weeks",
        "uhours",
        "hours",
        "fulltimely",
        "fulltimelw",
        "incp_all",
        "hrearn",
        "hrwage"
  )


## ----delete columns with more than 70% of missing values--------------------------------------------------------------
high_pct_miss_2017 <- as.data.frame(
    subset(
        miss_var_summary(df2017), # Identify features, number of NA and % of NA of all the df
        pct_miss > 70)) #Identify those features with a % of NA higher than 70%

high_pct_miss_2017


## ----delete rows with missing values----------------------------------------------------------------------------------
high_pct_miss_2017 <- high_pct_miss_2017$variable

df2017 <- df2017 %>% 
  dplyr::select(-one_of(high_pct_miss_2017))


## ----impute blank values with NA--------------------------------------------------------------------------------------
df2017[df2017 == ""] <- NA


## ----filter over 16 years old and below 65 years old------------------------------------------------------------------
df2017 <- df2017 %>% 
  filter(age >= 16 & age <= 65)


## ----NA imputation----------------------------------------------------------------------------------------------------
# drop rows from lfstat that are NA
df2017 <-df2017[!is.na(df2017$lfstat),]

# Impute all 0-1 columns replacing NA with 2 and filling arrived with "Born in US". 2 means "Not Applicable"

df2017 <- df2017 %>% 
  mutate(arrived = ifelse(citstat == "Born in US", "Born in US", arrived),
        peinusyr = ifelse(citstat == "Born in US", "Born in US", arrived),
        selfemp = ifelse(is.na(selfemp), 2, selfemp),
        selfinc = ifelse(is.na(selfinc), 2, selfinc),
        pubsect = ifelse(is.na(pubsect), 2, pubsect),
        pubfed = ifelse(is.na(pubfed), 2, pubfed),
        pubst = ifelse(is.na(pubst), 2, pubst),
        publoc = ifelse(is.na(publoc), 2, publoc), 
        weeks = ifelse(is.na(weeks), 0, weeks),
        uhours = ifelse(is.na(uhours), 0, uhours),
        hours = ifelse(is.na(hours), 0, hours),
        fulltimely = ifelse(is.na(fulltimely), 0, fulltimely),
        fulltimelw = ifelse(is.na(fulltimelw), 0, fulltimelw),
        wkslyr = ifelse(is.na(wkslyr), 0, wkslyr),
        clslyr = ifelse(is.na(clslyr), 2, clslyr),
        hprsmort = ifelse(is.na(hprsmort), 2, hprsmort),
        hrearn = ifelse(is.na(hrearn), 0, hrearn),
        hrwage = ifelse(is.na(hrwage), 0, hrwage))



## ----check if there are negative values (-9, -99) in the numeric columns----------------------------------------------
df2017 %>% 
  dplyr::select(where(~ any(. < 0)))


## ----delete rows with negative values---------------------------------------------------------------------------------
# Dim before deleting negative values
dim(df2017)

df2017 <- df2017 %>%
  filter(incp_all >= 0 & hrearn >= 0)

# Dim after deleting negative values
dim(df2017)


## ----create age groups------------------------------------------------------------------------------------------------
# Age groups
df2017 <- df2017 %>% mutate(
  age_group = cut(age, 
                  breaks=c(0, 24, 44, 64, 100), 
                  labels = c("<25", 
                              "25-44",
                              "45-64",
                              ">=65")))

df2017$age_group <- as.character(df2017$age_group)

table(df2017$age_group)


## ----select numeric variables as a vector-----------------------------------------------------------------------------
# Select numeric variables as a vector
num_vars_2017 <- df2017 %>%
    # select() %>% # exclude ID variable
    select_if(is.numeric) %>%
    names()

num_vars_2017


## ----select categorical variables as a vector-------------------------------------------------------------------------
cat_vars_2017 <- df2017 %>%
   # select() %>% # exclude ID variable
   select_if(is.character) %>%
  names()

cat_vars_2017


## ----dummify categorical variables------------------------------------------------------------------------------------
# Create dummies:
df_dummified_2017 <- dummy_cols(df2017, 
                 select_columns = cat_vars_2017 )

# Delete old columns:
df_dummified_2017 <- df_dummified_2017 %>% 
  dplyr::select(-all_of(cat_vars_2017))

# Clean variables
df_dummified_2017 <- clean_names(df_dummified_2017)


## ----avoid unem being deleted when excluding near zero variance variables---------------------------------------------
zerovar_vars_2017 <- nearZeroVar(df_dummified_2017 %>%
  dplyr::select(!unem), names = TRUE)


## ----delete near zero variance columns in dummified df----------------------------------------------------------------
df_dummified_2017 <- df_dummified_2017 %>% 
  dplyr::select(-one_of(zerovar_vars_2017))


## ----make unem a factor, and prepare its levels for svm---------------------------------------------------------------
df_dummified_2017$unem <- as.factor(df_dummified_2017$unem)

levels(df_dummified_2017$unem) <- make.names(levels(df_dummified_2017$unem))
