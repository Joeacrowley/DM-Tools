#######  Standard DM toolkit ################################################### 


####### Set up... ##############################################################

library(haven)
library(tidyverse)
library(clean)
library(labelled)
library(Hmisc) # weighted quantiles

# Load BSA teaching data
df <- read_sav('/Users/joecrowley/R/Data/BSA Teaching Data/spss/spss25/bsa2019_poverty_open.sav', user_na = T)


####### Variable summary #######################################################

names(df)
df %>% glimpse
look_for(df)


####### Example descriptive code ################################################

freq(as_factor(df$RSex, levels = "both"))

# function map freq()
freqm <- function(data, vars) { 
  map(vars, function(x) {
    result <- list(data %>% pull(x) %>% freq())
    names(result)[1] <- paste(x, attributes(data %>% pull(x))$label)
    return(result)
    })
}

bsa_vars <- c("libauth", "leftrigh", "welfare2") # vector of var names
freqm(df, bsa_vars)

# tidyr fct_count for factor vectors
fct_count(as_factor(df$RSex), prop = T, sort = T)

# fct_drop to remove empty levels
fct_count(fct_drop(as_factor(df$RSex)))

df %>% count(RAgeCat)

# count() for all of a vector of variables
count_df <- function(df, vars) {
  map(vars, 
      ~df %>%
        mutate_at(.x, ~as_factor(.x, levels = "both")) %>%
        count(.data[[.x]]) %>% 
        mutate(var = .x) %>% 
        rename(cat = .x) %>% 
        relocate(var)) %>%
    bind_rows
}

df %>% count_df(c("RAgeCat", "RSex", "HEdQual3"))


# Quick cross-tab for haven style objects
crs <- function(data, var1, var2){
  fct_cross(as_factor(data %>% pull(var1)), as_factor(data %>% pull(var2))) %>% fct_count(., prop = T) %>%
    separate(f, c("f1","f2"), ":") %>% 
    mutate(p = paste0(round(p * 100, 1), "%"), n = as.character(n)) %>%
    pivot_longer(cols = c(n,p), names_to = "Statistic", values_to = "Values") %>%
    pivot_wider(names_from = f2, values_from = Values) %>% 
    arrange(f1, desc(Statistic))
}

df %>% crs(var1 = "RSex", var2 = "RAgeCat")


####### Topics covered #########################################################

# - Working with numeric variables
# - Recoding variables based on others
# - Factor recoding
# - Recoding SPSS data to R style data
# - Filtering data
# - Categorising numeric data
# - Oooh, coalesce()



####### Working with numeric data ##############################################


# Mean of single variables
df <- df %>% mutate(libauth_av = mean(libauth, na.rm = T), 
                    leftrigh_av = mean(leftrigh, na.rm = T))

df %>% count(libauth_av)
df %>% count(leftrigh_av)


# Mean of multiple variables at once (each mean is separate)
# A purrr-style formula
df <- df %>% mutate(across(c(welfare2:libauth), ~mean(.x, na.rm = T), .names = "{.col}_mean"))
df %>% count(libauth_mean)
df %>% count(leftrigh_mean)
df %>% count(welfare2_mean)


# Calculate multiple statistics per variable
# Requires a named list of functions
df %>% select(welfare2) %>% 
  mutate(across(all_of("welfare2"), 
                list(var_mean = mean, sd = sd), # First part is name for stat column, second is the statistic function
                .names = "{.col}_{.fn}"))


# Can select variables to be edited based on variable type as well. 
df %>% mutate(across(where(is.double) & !c(Sserial), round))


# And based on variable location. 
df %>% mutate(across(1, round))


# Mean across multiple variables

# DOES NOT WORK - this takes the average only of the first variable
df <- df %>% mutate(av_ati = mean(libauth, leftrigh, na.rm = T))
df %>% count(libauth, leftrigh, av_ati)

# Mean of both variables for whole sample
df <- df %>% mutate(av_ati = mean(c(libauth, leftrigh), na.rm = T))
df %>% count(av_ati)

# Mean of both variables allowing tidy select - use c_across
df %>% mutate(av_ati2 = mean(c_across(all_of(c("libauth", "leftrigh"))), na.rm = T)) %>% count(av_ati2)

####### Row or group-wise operations ###########################################

# Row-wise variable summaries [SLOW]
df %>% 
  rowwise() %>%
  mutate(av_ati2 = mean(c_across(all_of(c("libauth", "leftrigh"))), na.rm = T)) %>% 
  count(av_ati2)


# Group wise variable summaries
df %>% 
  group_by(RSex) %>%
  mutate(av_ati2 = mean(c_across(all_of(c("libauth", "leftrigh"))), na.rm = T)) %>% 
  count(av_ati2) # Remember, without ungroup output is still grouped... 

# Quicker row-wise functions... 
?rowSums

df <- df %>% mutate(
  av_ati3 = rowMeans(across(c(libauth, leftrigh)), na.rm = T)
)

df %>% select(av_ati3, libauth, leftrigh) %>% 
  sample_n(50) %>% print(n=1000)

set.seed(123)  # for reproducibility

num_df <- data.frame(
  var1 = sample(1:100, 50, replace = T),
  var2 = sample(1:45, 50, replace = T),
  var3 = sample(1:20, 50, replace = T),
  var4 = sample(1:75, 50, replace = T),
  var5 = sample(1:50, 50, replace = T)
)

num_df <- num_df %>% mutate(
  
  # All columns
  row_total = rowSums(across(everything()), na.rm = T), 
  
  # Identify column numbers
  rows_2_4 = rowSums(across(c(2,4)), na.rm = T), 
  
  # Identfy variable with ends_with
  ends_with_3_5 = rowSums(across(ends_with(c("3","5")))),
  
  # With character vector
  `1_and_2` = rowSums(across(all_of(c("var1", "var2"))))
  
)

num_df %>% print()

####### Recoding variables based on other variables ############################

# Mostly uses case_when

# First, group up age

df %>% count(RAgeCat)

df <- df %>% mutate(age2 = case_when(
  RAgeCat %in% c(1,2,3) ~ 1, 
  RAgeCat %in% c(4,5,6,7) ~ 2, 
  TRUE ~ NA), 
  age2 = factor(age2, labels = c("18 - 44", "45 +"))
  )

df %>% count(RAgeCat, age2)

# Recoding drawing on several variables

# Identify left-wing Tories. 

df %>% count_df(c("PartyId2", "leftrigh")) %>% print(n=100)

df <- df %>% mutate(left_wing_tory = case_when(
  PartyId2 == 1 & zap_missing(leftrigh) < 2.5 ~ 1, 
  PartyId2 == 1 & zap_missing(leftrigh) >= 2.5 ~ 2, 
  PartyId2 %in% 2:8 ~ 3), 
  left_wing_tory = factor(left_wing_tory, 
                          levels = 1:3, 
                          labels = c(
                            "Left wing Tory", 
                            "Tory", 
                            "Other parties"
                          ))
  )

df %>% count(left_wing_tory)

# Recoding drawing on several variables using tidy-select

# Identify people with extreme responses on BSA attitude scales
df %>% count_df(vars = c("libauth", "leftrigh", "welfare2")) %>% print(n=100)
freq(df$libauth, nmax=100, sort.count = F)

vars <- c("libauth", "leftrigh", "welfare2")
df <- df %>% mutate(extremes = case_when(
  
  # Values above 5 or below zero are NA
  if_any(all_of(vars), ~. < 0 | . > 5) ~ NA, 
  
    # Cases with any value above 4 or below two are 1
    if_any(all_of(vars), ~. > 4 & . <= 5) | 
    if_any(all_of(vars), ~ . < 2 & . >= 0) 
    ~ 1, 
  
  # Remaining values zero
  TRUE ~ 0)
  
  )

# extremes defined as reporting values greater than 4 or less than 2 on any score
df %>% count(extremes) 

df %>% 
  group_by(extremes) %>% 
  summarise(across(all_of(vars), list(min = min, max = max))) %>% 
  pivot_longer(cols = -extremes)


####### Factor recode #########################################################

# Example - opions about benefits, simplifying dole2
attributes(df$dole2)$label

# factorise opinions about benefits. 
df <- df %>% mutate(dole2 = as_factor(Dole))

df %>% count(dole2)
levels(df$dole2)

# Collapse levels of a factor
df <- df %>% 
  mutate(dole3 = fct_collapse(dole2, 
      "Neither" = c("(Neither)", 
                    "BOTH: UNEMPLOYMENT BENEFIT CAUSES HARDSHIP BUT CAN'T BE HIGHER OR THERE WOULD BE NO INCENTIVE TO WORK", 
                    "BOTH: UNEMPLOYMENT BENEFIT CAUSES HARDSHIP TO SOME, WHILE OTHERS DO WELL OUT OF IT", 
                    "About right/in between",                                                                               
                    "Other answer (WRITE IN)"),
      "DK/REF" = c("Don`t know", "Refusal")
      ))

df %>% count(dole3)

# Recode factor levels
df <- df %>% 
  mutate(dole3 = fct_recode(dole3, 
     "Benefits for unemployed people are too low and cause hardship" = "...benefits for unemployed people are too low and cause hardship", 
     "Benefits for unemployed people are too high and discourage them from finding jobs" = "or, benefits for unemployed people are too high and discourage them from finding jobs"
     ))

df %>% count(dole3)

# fct_recode with an external vector

df %>% count(SMNews)

# Define levels to recode...
to_recode <- c(Often = "Several times a day", Often = "Every day", Often = "Several times a week", 
               `Not often` = "Never", `Not often` = "Less often than once a month", `Not often` = "Once a month")

df <- df %>% mutate(SMNews_f = as_factor(SMNews),
                    SMNews_f = fct_recode(SMNews_f, !!!to_recode))

df %>% count(SMNews_f, SMNews)

################### Manage missing data in factors #############################

attributes(df$Spend1)
df %>% count(Spend1)

# Remove user NA tagged labels with zap_missing()
df <- df %>% mutate(Spend1_f = as_factor(zap_missing(Spend1)))
df %>% count(Spend1_f)
var_label(df$Spend1_f)

# Make NA a valid level
df <- df %>% mutate(Spend1_f = fct_na_value_to_level(Spend1_f, level = "No priority known")) 
df %>% count(Spend1_f)

# Move levels to NA
df <- df %>% mutate(Spend1_f = fct_na_level_to_value(Spend1_f, extra_levels = "No priority known"))
df %>% count(Spend1_f)

# Or, setting levels to NA can be done in fct_recode
df %>% count(Married)

df %>% mutate(Married = fct_recode(as_factor(Married), NULL = "No information")) %>% 
  count(Married)

################### Less used factor edits ######################################

# Collapse 'other' factor levels & reorder factors...

# Other level to provide summary level for unused levels. 
df <- df %>% mutate(married2 = fct_collapse(as_factor(Married), 
    "No information" = "No information", 
    "Past marriage or married" = c("Married/living as married", "Separated/divorced", "Widowed"), 
    other_level = "Never married"
    ), 
    married2 = fct_relevel(married2, "Never married") # fct_relevel to control factor level order
    )

df %>% count(married2, Married)

# Combine factor levels 
fct_cross(as_factor(df$RSex), as_factor(df$RAgeCat)) %>% fct_count(., prop = T) 

df <- df %>% mutate(age_sex = fct_cross(as_factor(RSex), as_factor(RAgeCat)))
df %>% count(age_sex)


# drop unused levels - fct_drop
df$TaxSpend %>% as_factor() %>% fct_count()

df$TaxSpend %>% as_factor() %>% fct_drop() %>% fct_count()


####### Recode labelled numeric data ###########################################





####### Filter data  ###########################################################

df %>% filter(PartyId2 == 1) %>% count(PartyId2)

df %>% filter(if_any(ends_with("Rsex"), ~ . > 1)) # Women only

df %>% filter(if_all(RSex:RAgeCat, ~ . > 1)) %>% # Womeny older than 24
  count(RSex, RAgeCat)


####### Group numeric data  ####################################################

attributes(df$NatFrEst) # No. of people claiming benefits who give false info
df %>% count(NatFrEst) %>% print(n=100)
hist(zap_missing(df$NatFrEst))


# ntile()
# Rough buckets, ignores ties (same value can end up in differet buckets)
# resulting groups more or less perfectly quintiles - because does not respect ties.

ntile(zap_missing(df$NatFrEst), n = 5) %>% freq # No labels - hard to understand

# Function to add labels to ntile breaks
ntile_lab <- function(data, var, breaks = 5) { 
  
  var_to_use <- data %>% pull(var)
  
  vec <- ntile(zap_missing(var_to_use), n = breaks)
  
  breaks <- map(na.omit(unique(vec)),
                ~paste0(
                  min(zap_missing(var_to_use)[vec == .x],na.rm = T), " - ",
                  max(zap_missing(var_to_use)[vec == .x],na.rm = T)
                )
  )
  vec_unique <-  as.vector(na.omit(unique(vec)))
  names(vec_unique) <-  breaks
  vec_unique
  
  result <- vec %>% set_value_labels(.labels = vec_unique)
  
  return(result)
  
  }

# Test ntile_lab
ntile_lab(data = df, var = "NatFrEst") %>% as_factor() %>% fct_count()

# Now with 10 breaks
ntile_lab(data = df, var = "NatFrEst", breaks = 10) %>% as_factor() %>% fct_count()

# cut() from base R

# Square bracket in output indicates the groups are capped on that side, 
# e.g. here the right hand side includes ten in the first group
cut(df$NatFrEst, breaks = seq(from = 0, to = 100, by = 10)) %>% fct_count() 

# Controlling where groups end and start is hard... 
# If we use the following breaks: 
seq(from = 0, to = 100, by = 10)

# Here cut() implements those breaks so the results run from 0 up to 9, then 10 to 19 
# Problematically, 100 is NA
# That is because bands close on the left (the rightmost value is excluded)
df <- df %>% mutate(ben_cheat = cut(NatFrEst, breaks = seq(from = 0, to = 100, by = 10), 
                                    include.lowest = F, 
                                    right = F))
df %>% count(ben_cheat, NatFrEst) %>% print(n=100)

# Now we set right = T... 
# Now breaks are 1-10, 11-20, 21-30
# 100 is included
# Exclues zero, because include.lowest = F
df <- df %>% mutate(ben_cheat = cut(NatFrEst, breaks = seq(from = 0, to = 100, by = 10), 
                                    include.lowest = F, 
                                    right = T))
df %>% count(ben_cheat, NatFrEst) %>% print(n=100)

# Now also set include.lowest = T
# This is probably best, breaks are 0-10, 11-20, 21-30 and 100 + 0 are included
df <- df %>% mutate(ben_cheat = cut(NatFrEst, breaks = seq(from = 0, to = 100, by = 10), 
                                    include.lowest = T, 
                                    right = T))
df %>% count(ben_cheat, NatFrEst) %>% print(n=100)


# With decimalised data one would need to take even more care... 
df %>% count(libauth) %>% print(n=1000)


# Actually the same settings as above again seem best
df <- df %>% mutate(libauth_brack = cut(libauth, seq(1,5,by = 1), include.lowest = T, right = T))
df %>% count(libauth_brack, libauth) %>% print(n=1000)


# tidyr versions to group numeric data... 

# cut_width() breaks vector into chunks of a given width, here 5.
cut_width(sample(1:100, size = 100, replace = T), width = 5) %>% fct_count() %>% print(n=21)

cut_width(zap_missing(df$NatFrEst), width = 7.5) %>% fct_count()

# cut_interval, n groups of equal range
cut_interval(zap_missing(df$NatFrEst), length = 10) %>% fct_count

# cut_number - n groups with approx equal observations in each
cut_number(zap_missing(df$NatFrEst), n = 5) %>% fct_count # equal ish for sure.


# Quantiles... returns a numeric vector of cut points...

# max val is 999, it includes user NA 
quantile(df$NatFrEst)

# With zap_missing, the na_rm argument is required. 
quantile(zap_missing(df$NatFrEst), na.rm=T)

# Can be matched with cut()
# right = T in the cut() function gives the best result, although % not perfectly 25%. 
df <- df %>% mutate(NatFrEst_quarts = 
                      cut(NatFrEst, 
                          breaks = quantile(zap_missing(df$NatFrEst), na.rm=T), 
                          include.lowest = T, 
                          right = T))

fct_count(na.omit(df$NatFrEst_quarts), prop = T)

quantile(df$NatFrEst, probs = 0.1) # Can return a single value if you needed a 10% cut point... 
quantile(df$NatFrEst, probs = seq(0,1,0.1)) # Or very detailed sequences

# Now, exclude missing and return value at which we cross 90th percentile
quantile(zap_missing(df$NatFrEst), probs = 0.9,na.rm =T)

# Cut people at the 90th percentile for highest perceptions of benefit cheating
cut_val <- quantile(zap_missing(df$NatFrEst), probs = 0.9,na.rm=T)
cut_val

df <- df %>% mutate(NatFrEst_top_10 = case_when(
  is.na(zap_missing(NatFrEst)) ~ NA, 
  NatFrEst > cut_val ~ 1, 
  TRUE ~ 0), 
  NatFrEst_top_10 = factor(NatFrEst_top_10, levels = c(0,1), labels = c("Less than 75%", "More than 75%")))

df %>% count(NatFrEst_top_10) %>% group_by(!is.na(NatFrEst_top_10)) %>% mutate(p = n / sum(n))

# Or more simply...
df %>% mutate(NatFrEst_90 = cut(NatFrEst, 
                                breaks = c(0,cut_val,100), 
                                include.lowest = T, 
                                right = T)) %>% 
  count(NatFrEst_90) 


# Weighted quantiles 
wtd.quantile(df$NatFrEst, weights = df$WtFactor) # Again, returns a numeric vector... 

# right = T in the cut() function gives the best result, although % not perfectly 25%. 
df <- df %>% mutate(NatFrEst_quarts_wt = 
                      cut(NatFrEst, 
                          breaks = wtd.quantile(zap_missing(df$NatFrEst), na.rm=T), 
                          include.lowest = T, 
                          right = T))

df %>% count(NatFrEst_quarts_wt)
df %>% count(NatFrEst_quarts,NatFrEst_quarts_wt) # They match, weights are weak I guess.


# Look at weights with quantiles... 
quantile(df$WtFactor, probs = seq(0,1,0.025)) %>% matrix(nrow = 8,ncol = 5)
quantile(df$WtFactor, probs = seq(0,1,0.025)) %>% plot

####### Checking edits that effect numeric variables ###########################

# Take a random sample...
df %>% count(NatFrEst_quarts_wt, NatFrEst) %>% 
  sample_n(50) %>% print(n=50)

# Look at min and max values
df %>% group_by(NatFrEst_quarts_wt) %>% 
  summarise(min = min(NatFrEst, na.rm = T), 
            max = max(NatFrEst, na.rm = T))

############## Renaming ########################################################

# One variable at a time
df %>% names
df %>% rename(sex = RSex, `Weight variable` = WtFactor, serial_number = Sserial) %>% 
  names

# General edits...
df %>% rename_all(tolower) %>% names


# Using tidy-select to identify variables, and apply a function
df %>% select(libauth, leftrigh, welfare2) %>% glimpse

df %>% select(libauth, leftrigh, welfare2) %>% 
  rename_with(~paste0(.x, "_f"), .cols = everything()) %>% 
  names()

# rename with an external vector of names

# Uses a named vector...
lookup <- c(`Left-right scale` = "leftrigh", 
            `Libertarian-authoritarian scale` = "libauth", 
            `Welfarism scale` = "welfare2", 
            dole = "Dole"
            )

rename(df, all_of(lookup)) %>% names





############## The labelled package ############################################

# What is useful? 
#  -  the data dictionary tool... look_for()
#  -  recode skipmeal with labelled()

####### labelled() - look_for() ####### 

# labelled() makes many see look good functions
look_for(df)
look_for(df, details = FALSE)
look_for(df, "paptype")
look_for(df, c("paptype", "RAgeCat"))

# generate_dictionary() == look_for()

# results as dataframe
# all levels of variables compressed into single row. 
look_for(df, c("paptype", "RAgeCat")) %>%
  convert_list_columns_to_character()

# results as LONG dataframe
# i.e. each level of variables gets its own row
look_for(df, c("paptype", "RAgeCat")) %>%
  lookfor_to_long_format() %>%
  convert_list_columns_to_character()


####### labelled() - Create labelled data ####### 

v <- labelled(
  c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
  c(yes = 1, no = 3, "don't know" = 8, refused = 9)
)

v

#  dplyr style for data frame
df2 <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
  set_variable_labels(s1 = "Sex", s2 = "Question") %>%
  set_value_labels(s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))

df2$s2

####### labelled() - Basic labelled data edits  ####### 

# Edit a single label
val_label(v, 8)
val_label(v, 8) <- "DK"
v

# val_labels removes unspecified labels
val_labels(v)
val_labels(v) <- c("Don't know" = 8)
v

# Using NULL with val_labels removes all labels and haven-labelled class
val_labels(v) <- NULL
v

# We can also remove value labels with...
remove_val_labels(v)

# Drop all or only one label...
df2 <- df2 %>% set_value_labels(s1 = NULL) # All of a variable's labels
df2 <- df2 %>% remove_value_labels(s2 = 2) # removing one value label
df2$s1
df2$s2

# Drop multiple labels at once...
df2 <- df2 %>% remove_value_labels(s2 = 8:9)
df2$s2

 # Within mutate, here dropping user_na and keeping variable labels. 
df2 %>% mutate(s2 = remove_labels(s2, keep_var_label = T, user_na_to_na = T)) 
  

# set_value_labels() has the same role as val_labels, over writes all values
df2 <- df2 %>% set_value_labels(s2 = c(Yes = 1, "Don't know" = 8, Unknown = 9))
df2$s2

# Whereas add_value_labels updates an existing variable (like val_label)
df2 <- df2 %>% add_value_labels(s2 = c(No = 2))
df2$s2

# Reattaching labels restores haven-labelled class
val_labels(v) <- c(yes = 1, no = 3, refused = 9, "don't know" = 8)
v # By default labels appear in the order added (8 here is after 9)

####### labelled() - Editing multipled labelled variables in a dataframe  ####### 

# Editing multiple columns at once... 
test_df <- data.frame(v1 = 1:3, v2 = c(2, 3, 1), v3 = 3:1)
test_df 

val_label(test_df, 1) <- "yes"
val_label(test_df[, c("v1", "v3")], 2) <- "maybe"
val_label(test_df[, c("v2", "v3")], 3) <- "no"
val_labels(test_df )

test_df2 <- data.frame(v1 = 1:3, v2 = c(2, 3, 1), v3 = 3:1)
val_labels(test_df2) <- list(v1 = c(no = 3, yes = 1), v2 = c(a = 1, b = 2, c = 3))
val_labels(test_df2)

####### labelled() - Sort labelled data()  ####### 

# To sort them by their numeric values...
sort_val_labels(v)
sort_val_labels(v, decreasing = T)
# Or their labels...
sort_val_labels(v, according_to = "l")

# Sorting in a dataframe... dplyr style
test_df2 <- test_df2 %>% mutate(v1a = sort_val_labels(v1))
test_df2$v1a # labelling order reversed
test_df2 %>% count(v1, v1a) # but values the same 

####### labelled() - Handle missing data  ####### 

# Tag missing values
na_values(test_df$v1) <- (2)
na_values(test_df$v1)
test_df$v1

na_range(test_df$v2) <-  c(2,3)
na_values(test_df$v2)
test_df$v2

# Missing tag to NA
test_df$v2 <- user_na_to_na(test_df$v2)
test_df

# Drop missing tag
test_df$v1 <- remove_user_na(test_df$v1)
test_df$v1
test_df

test_df$v1
test_df <- test_df %>% mutate(v1 = set_na_values(v1, c(2,3)))
test_df$v1

test_df <- test_df %>% mutate(v1 = remove_user_na(v1))
test_df$v1

# Drop labelled or unlabelled values
v <- labelled(c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA), c(yes = 1, maybe = 2, no = 3))
v
nolabel_to_na(v) # 9 is now NA, because it has no label

size <- labelled(c(1.88, 1.62, 1.78, 99, 1.91), c("not measured" = 99))
size
val_labels_to_na(size)

# There are also the haven function zap_missing() and zap_labelled()

df %>% count(RAgeCat)
df %>% count(zap_missing(RAgeCat)) # Remove user_na only, convert to NA. 
df %>% count(zap_labels(RAgeCat)) # Remove all labels, user_na to NA
df %>% count(zap_labels(RAgeCat, user_na = T)) # Retain user_na as valid value



#######  labelled() - Version of recode() ####### 
# Extension to method from dplyr for labelled numeric data... 

# Recoding with the labelled package
x <- labelled(1:3, c(yes = 1, no = 2))
x
dplyr::recode(x,`3` = 2L, .combine_value_labels = T)

# Where combined values have a label, these are combined... 
x <- labelled(1:3, c(yes = 1, no = 2, maybe = 3))
x
dplyr::recode(x,`3` = 2L, .combine_value_labels = T)

# Grouping agree / disagree style scale...
x <- labelled(
  1:4,
  c(
    "strongly agree" = 1,
    "agree" = 2,
    "disagree" = 3,
    "strongly disagree" = 4
  )
)

x

# Here, merge groups labels, .sep allows user defined break.
dplyr::recode(
  x,
  `1` = 1L,
  `2` = 1L,
  `3` = 2L,
  `4` = 2L,
  .combine_value_labels = TRUE, .sep = " or "
)

# Here recode drops value labels...
dplyr::recode(
  x,
  `1` = 1L,
  `2` = 1L,
  `3` = 2L,
  `4` = 2L,
  .keep_value_labels = F,
)

# Create a test data frame for tidyr style recoding
test_df <- test_df %>% set_value_labels(v1 = c(Yes = 1, No = 2, Maybe = 3))

# Application within mutate()
test_df$v1
test_df <- test_df %>% 
  mutate(v1a = recode(v1, 
                      `1` = 1L, 
                      `2` = 1L, 
                      .combine_value_labels = TRUE)
         )

test_df$v1a
test_df %>% count(as_factor(v1), as_factor(v1a)) # Review result

####### labelled() - Convert labelled data to factors  ####### 
                               
# Application of to_factor() - convert labelled data to factors
v <- labelled(c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA), c(yes = 1, no = 3, maybe = 2))

# the labelled() to_factor() function seems similar to as_factor()
v %>% to_factor() %>% levels()
v %>% as_factor() %>% levels()

v
to_factor(v) # labels as levels
to_factor(v, levels = "v") # values not labels
to_factor(v, levels ="p") # both

# Missing data
to_factor(v, nolabel_to_na = T) # unalabelled values to NA

# Factor order
to_factor(v, sort_levels = "n") %>% levels() # Order is the order labels were defined in
to_factor(v, sort_levels = "v") %>% levels() # Order by values 
to_factor(v, sort_levels = "l") %>% levels() # Order by labels 

# Back to labelled (perhaps for easy edits?)
f <- to_factor(v)
to_labelled(f) # Note values now 1:4, not 1,2, 3 and 9


###### labelled() - Applied examples ##################

# Example of skipmeals - simplify into two groups...
df %>% count(skipmeal)

df <- df %>% 
  mutate(skipmeal2 = case_when(
    skipmeal %in% 1:4 ~ 1, 
    skipmeal %in% 5:7 ~ 2)) %>% 
  set_value_labels(skipmeal2 = c("Less often than once a week" = 1, 
                                 "Once a week or more" = 2))

df %>% count(skipmeal2, skipmeal)
df %>% count(to_factor(skipmeal2))

# Edit labels - then drop user missing vals, for libertarian-authoritatian scale

df %>% count(libauth)
val_labels(df$libauth)

df <- df %>% 
  mutate(libauth2 = add_value_labels(libauth, c("Libertarian" = 1, "Authoritarian" = 5)), 
         libauth2 = user_na_to_na(libauth2))
         
val_labels(df$libauth2)
df %>% count(libauth2)


# Combine levels with recode... has to be done exactly right...

# Review data
freq(as_factor(df$incdiffs, levels = "both")) 
val_labels(df$incdiffs)

# This code is sort of ok, but does not combine labels
# Here the results are 'agree' and 'strongly agree' both become 'strongly agree'.
# All other values are combined into 9L - 'Not answered'. 

df <- df %>% 
  mutate(incdiffs2 = recode(incdiffs, 
                      `2` = 1L, 
                      `1` = 1L, 
                      `3` = 3L,
                      `4` = 4L,
                      `5` = 4L, 
                      .default = 9L, 
                      # .combine_value_labels = T
                      ))

df$incdiffs2
df %>% count(as_factor(incdiffs2), as_factor(incdiffs))

# This code won't run, the issue is that df$incdiffs contains user-defined NA values
attributes(df$incdiffs) # values -1, 8 and 9 are user tagged NA. 
class(df$incdiffs)

df <- df %>% 
  mutate(incdiffs2 = recode(incdiffs, 
                            `2` = 1L, 
                            `1` = 1L, 
                            `3` = 2L,
                            `4` = 3L,
                            `5` = 3L,
                            .combine_value_labels = T
  ))


# Once we remove user defined NA, the class changes
user_na_to_na(df$incdiffs) %>% class # no longer has haven_labelled_spss

# In tidyr style, collapsing variable labels...
df <- df %>% 
  mutate(incdiffs2 = user_na_to_na(incdiffs),
         incdiffs2 = recode(incdiffs2, 
                            `1` = 1L, 
                            `2` = 1L,
                            `3` = 2L,
                            `4` = 3L,
                            `5` = 3L,
                            .combine_value_labels = T)) 

df %>% pull(incdiffs2) %>% to_factor(levels = "p") %>% fct_count()


####### Combining lots of variables ###########################################
# From help: 
# Given a set of vectors, coalesce() finds the first non-missing value at each 
# position. It's inspired by the SQL COALESCE function which does the same thing
# for SQL NULLs.


# BSA is not great for this... so creating a test dataset
coalesce_data <- 
  data_frame(
  v1 = sample(c(1:10, NA), 100, replace = T), 
  v2 = sample(c(1:10, NA), 100, replace = T),
  v3 = sample(c(1:10, NA), 100, replace = T)
)

coalesce_data 

# Replace NA with the first non-missing value, going in order...
coalesce_data %>% 
  mutate(
    non_miss = coalesce(v1,v2,v3),
    non_miss_reverse = coalesce(v3,v2,v1)
  ) %>% 
  print(n=30)

# tidy-select sort of approach
coalesce_data %>% 
  mutate(
    non_miss = reduce(across(everything()), coalesce)
    ) %>% 
  print(n=30)


####### tidy_select ############################################################
# Operations across multiple variables...

df %>% select(contains("pove")) %>% names
df %>% select(starts_with("s")) %>% names
df %>% select(ends_with("s")) %>% names
df %>% select(contains(c("welfare", "leftrigh", "libauth")))
df %>% select(contains("pov") & !starts_with("much"))

# Valid values range from 1 - 5
df %>% count_df(c("leftrigh", "libauth", "welfare2")) %>% print(n=150)

val_labels(df[c("leftrigh", "libauth", "welfare2")])

# First remove user-defined NA... 
df <- 
  df %>% 
  mutate(across(c(leftrigh, libauth, welfare2), 
                ~user_na_to_na(.x), 
                .names = "{.col}_2"), 
         across(ends_with("_2"), 
                ~case_when(.x <= 2.5 ~ 1, 
                           .x < 3.5 ~ 2, 
                           .x >= 3.5 ~ 3)))

df %>% count(leftrigh_2, leftrigh) %>% print(n=100)

# Not an ideal, as these variable really require different value labels...
df <- df %>% 
  mutate(leftrigh_2 = factor(leftrigh_2, 
                             levels = c(1,2,3), 
                             labels = c("Leftwing", "Middling", "Rightwing")
                             )
         )

df %>% count(leftrigh_2) 
df %>% count(leftrigh_2, leftrigh) %>% print(n=50)


# Function to generate labelled_spss vectors with missing values defined
generate_var <- function(n = 1000) {
  labelled_spss(
    sample(
      c(1:5, 8, 9, NA),
      size = n,
      replace = TRUE,
      prob = c(0.15, 0.2, 0.25, 0.2, 0.1, 0.05, 0.03, 0.02)
    ),
    labels = c(
      "Strongly agree" = 1,
      "Agree" = 2,
      "Neither agree nor disagree" = 3,
      "Disagree" = 4,
      "Strongly disagree" = 5,
      "Don't know" = 8,
      "Refused" = 9
    ),
    na_values = c(8, 9) # <- user-defined missing values
  )
}

# Create dummy data set
set.seed(123)
dummy_data <- tibble(
  q1 = generate_var(),
  q2 = generate_var(),
  q3 = generate_var(),
  q4 = generate_var(),
  q5 = generate_var()
)

# View data
look_for(dummy_data)

# Using across to apply edits across multiple variables... 

# Using case_when() and factor()
dummy_data <- 
  dummy_data %>% 
  mutate(across(everything(), 
                ~factor(
                  case_when(.x <= 2 ~ 1, 
                           .x == 3 ~ 2, 
                           .x >= 4 & .x < 8 ~ 3), 
                  levels = c(1,2,3), 
                  labels = c("Agree","Neither","Disagree")),
                .names = "{.col}_2"
                )
         ) 

dummy_data %>% count(q1, q1_2)

# Using recode from labelled()
dummy_data <- 
  dummy_data %>% 
  mutate(across(!contains("_2"), 
                ~recode(user_na_to_na(.x),
                  `1` = 1L, 
                  `2` = 1L, 
                  `3` = 2L, 
                  `4` = 3L,
                  `5` = 3L, 
                  .combine_value_labels = T
                  ),
                .names = "{.col}_2"
  ))
   

dummy_data %>% count(q1, q1_2)

# using fct_recode from forcats...
dummy_data <- 
  dummy_data %>% 
    mutate(across(!contains("_2"), 
                  ~fct_recode(to_factor(.x), 
         "Agree" = "Strongly agree", 
         "Agree" = "Agree", 
         "Disagree" = "Disagree", 
         "Disagree" = "Strongly disagree"
                    ), 
         .names = "{.col}_2")
         )

# The result has left the user_NA values in place
# add user_na_to_na = TRUE to to_factor() to remove the.  
dummy_data %>% count(q1_2)
dummy_data %>% count(q1, q1_2)














