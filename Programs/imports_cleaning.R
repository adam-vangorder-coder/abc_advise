
#load packages
library(tidyverse)
library(readxl)
library(janitor)
library(arsenal)

###Set up drives----------------------------------------------------------------
if (Sys.getenv("USERNAME") == "YOUR USERNAME HERE"){
  # CHANGE ME ONLY IF YOU DONT USE THE STANDARD IDEA LOCATION
  drive <- "C:\\Users\\YOUR PATH HERE\\abc_advise"
} else {
  # DONT CHANGE ANYTHING IF THIS IS YOUR PATH
  (drive <- sprintf("C:\\Users\\%s\\Documents\\ABC Advise\\abc_advise\\", Sys.getenv("USERNAME"))) 
}

(raw_data <- sprintf("%sRaw Data\\", drive))
(edited_data <- sprintf("%sEdited Data\\", drive))
(final_data <- sprintf("%sFinal Data\\", drive))
(mappings <- sprintf("%sMappings\\", drive))

###raw data---------------------------------------------------------------------

imp_current_raw <- read_excel(sprintf("%sIDS0182_Imports_Current.xlsx", raw_data), skip = 1) |> 
  clean_names()

str(imp_current_raw)
head(imp_current_raw)
summary(imp_current_raw)

imp_historic_raw <- read_excel(sprintf("%sIDS0182_Imports_Historical.xlsx", raw_data), skip = 1) |> 
  clean_names()

str(imp_historic_raw)
head(imp_historic_raw)
summary(imp_historic_raw)

###clean ----------------------------------------------------------------

aggregate_codes <- c("MGOODS", "MGENMR", "MNMGLD", "MNPET", "MPET")

imp_current_aggs <- imp_current_raw |>
  filter(enduse_code %in% aggregate_codes)

imp_current <- imp_current_raw |> 
  filter(!enduse_code %in% aggregate_codes)
  
#should print TRUE to console - checks that we didnt lose any data
nrow(imp_current) + nrow(imp_current_aggs) == nrow(imp_current_raw)

imp_current1 <- imp_current |> 
  mutate(
    aggregate_level = case_when(
      nchar(enduse_code) == 2 ~ "One-Digit", 
      nchar(enduse_code) == 3 ~ "Two-Digit", 
      nchar(enduse_code) == 4 ~ "Three-Digit",
      nchar(enduse_code) == 6 ~ "Five-Digit",
      ),
    one_digit_code = str_extract(enduse_code, regex("^..")),
    two_digit_code = str_extract(enduse_code, regex("^M..")), #leaves NAs in one-digit rows
    three_digit_code = str_extract(enduse_code, regex("^....")), #leaves NAs in one- and two-digit rows
    five_digit_code = str_extract(enduse_code, regex("^......")), #leaves NAs in one-, two-, and three-digit rows
  )

#break out by agg level
current_one_digit<- imp_current1 |> 
  filter(aggregate_level == "One-Digit")

current_two_digit <- imp_current1 |> 
  filter(aggregate_level == "Two-Digit")

current_three_digit <- imp_current1 |> 
  filter(aggregate_level == "Three-Digit")

current_five_digit <- imp_current1 |> 
  filter(aggregate_level == "Five-Digit")


#check, should return TRUE to console
nrow(current_one_digit) + nrow(current_two_digit) + nrow(current_three_digit) + nrow(current_five_digit) == nrow(imp_current1)


