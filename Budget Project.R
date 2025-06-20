# Install required packages
install.packages(c("tidyverse","GGally","gridExtra","esquisse","caret","rvest","ggplot2","googledrive","ggthemes","googlesheets4"))

# Load required packages
library(tidyverse)
library(GGally)
library(esquisse)
library(gridExtra)
library(caret)
library(readr)
library(dplyr)
library(rvest)
library(car)
library(ggplot2)
library(zoo)
library(stringr)
library(data.table)
library(RColorBrewer)
library(googledrive)
library(googlesheets4)

# Google drive authorization
# Make sure to allow all access
drive_auth()

# Specify the Google Sheet URL
# Paste google sheet sharing link in quotations below
sheet_url <- ""

# No scientific notation
options(scipen = 9999)


### SECU DEBIT ###

# Folder of budget documents
# pattern = folder name from google drive
secuFolder <- drive_find(pattern = "SECU DEBIT", type = "folder")
# List all the secuFiles in the secuFolder
secuFiles <- drive_ls(as_id(secuFolder$id))
# File IDs and names
secuIDs <- secuFiles$id
secuNames <- secuFiles$name
# Empty list to store the data frames
secuDF <- list()
# Loop through the file IDs
for (i in seq_along(secuIDs)) {
  secuID <- secuIDs[i]
  secuName <- secuNames[i]
  # Read the raw content of the file
  secuContent <- drive_read_raw(as_id(secuID))
  # Convert the raw content to a character vector
  secuContent <- rawToChar(secuContent)
  # Read the character vector as a data frame
  secuExample <- read.csv(text = secuContent) 
  secuExample <- secuExample %>%
    select(Process.Dates, Description, Credit.Amount, Debit.Amount) %>%
    rename("processDate" = "Process.Dates",
           "description" = "Description",
           "moneyIn" = "Credit.Amount",
           "moneyOut" = "Debit.Amount")  
  ######################################################################################
  # Remove row names from the data frame
  rownames(secuExample) <- NULL
  # Store the data frame in the list
  secuDF[[i]] <- secuExample
}
# Combine all the data frames into one
debitSECU <- do.call(rbind, secuDF)
# Convert the combined data frame to an object in the global environment
assign("debitSECU", debitSECU, envir = .GlobalEnv)
# Convert date into proper date format
debitSECU <- debitSECU %>%
  # Filter out Venmo transactions from SECU so you don't double count later on
  filter(!str_detect(description,"VENMO")) %>%
  # Change date format & add fields needed for unioning data frames later on
  mutate(processDate = as.Date(processDate, format = "%m/%d/%Y"),
         venmoTransID = NA,
         transactionType = "SECU Debit")


### APPLE CREDIT ###

# Folder of budget documents
# pattern = folder name from google drive
appleFolder <- drive_find(pattern = "APPLE CREDIT", type = "folder")
# List all the appleFiles in the appleFolder
appleFiles <- drive_ls(as_id(appleFolder$id))
# File IDs and names
appleIDs <- appleFiles$id
appleNames <- appleFiles$name
# Empty list to store the data frames
appleDF <- list()
# Loop through the file IDs
for (i in seq_along(appleIDs)) {
  appleID <- appleIDs[i]
  appleName <- appleNames[i]
  # Read the raw content of the file
  appleContent <- drive_read_raw(as_id(appleID))
  # Convert the raw content to a character vector
  appleContent <- rawToChar(appleContent)
  # Read the character vector as a data frame
  appleExample <- read.csv(text = appleContent) 
  appleExample <- appleExample %>%
    filter(Type != "Payment") %>%
    select(Clearing.Date,Description,Amount..USD.) %>%
    rename("processDate" = "Clearing.Date",
           "description" = "Description",
           "moneyOut" = "Amount..USD.") %>%
    mutate(moneyIn = NA)  
  ######################################################################################
  # Remove row names from the data frame
  rownames(appleExample) <- NULL
  # Store the data frame in the list
  appleDF[[i]] <- appleExample
}
# Combine all the data frames into one
creditAPPLE <- do.call(rbind, appleDF)
# Convert the combined data frame to an object in the global environment
assign("creditAPPLE", creditAPPLE, envir = .GlobalEnv)
# Convert date into proper date format
creditAPPLE <- creditAPPLE %>%
  mutate(processDate = as.Date(processDate, format = "%m/%d/%Y"),
         venmoTransID = NA,
         transactionType = "APPLE Credit")



### VENMO ###

# Folder of budget documents
venmoFolder <- drive_find(pattern = "VENMO", type = "folder")
# List all the venmoFiles in the folder
venmoFiles <- drive_ls(as_id(venmoFolder$id))
# File IDs and names
venmoIDs <- venmoFiles$id
venmoNames <- venmoFiles$name
# Empty list to store the data frames
venmoDF <- list()
# Loop through the file IDs
for (i in seq_along(venmoIDs)) {
  venmoID <- venmoIDs[i]
  venmoName <- venmoNames[i]
  # Read the raw content of the file
  venmoContent <- drive_read_raw(as_id(venmoID))
  # Convert the raw content to a character vector
  venmoContent <- rawToChar(venmoContent)
  # Read the character vector as a data frame
  venmoExample <- read.csv(text = venmoContent)
  # Remove the first row and make row 2 the new header row
  venmoExample <- venmoExample[-1, ]
  colnames(venmoExample) <- venmoExample[1, ]
  venmoExample <- venmoExample[-1, ]
  venmoExample <- venmoExample[-1, -1]
  # Remove rows where ID is empty (blank)
  venmoExample <- venmoExample[venmoExample$ID != "", ]
  ######################################################################################
  # Store the data frame in the list with the file name as the key
  venmoDF[[i]] <- venmoExample
}
# Combine all the data frames into one
debitVENMO <- do.call(rbind, venmoDF)
# Convert the combined data frame to an object in the global environment
assign("debitVENMO", debitVENMO, envir = .GlobalEnv)
# Create the "moneyTo" field by removing symbols and converting to numeric
debitVENMO$amountTotal <- as.numeric(gsub("[$+-]", "", debitVENMO$`Amount (total)`))
# Multiply by -1 for values originally preceded by "-"
debitVENMO <- debitVENMO %>%
  rename("totalAmount" = `Amount (total)`) %>%
  filter(Type != 'Instant Transfer' & Type != 'Standard Transfer') %>%
  mutate(
    amountTotal = case_when(
      str_detect(totalAmount,"[-]") == TRUE ~ amountTotal * -1,
      TRUE ~ amountTotal),
    moneyIn = case_when(
      amountTotal >= 0 ~ amountTotal,
      TRUE ~ NA),
    moneyOut = case_when(
      amountTotal < 0 ~ amountTotal,
      TRUE ~ NA),
    moneyTo = case_when(
      account == From & Type == "Charge" ~ From,
      TRUE ~ To),
    moneyFrom = case_when(
      moneyTo == From ~ To,
      TRUE ~ From),
    processDate = as.Date(Datetime),
    description = Note,
    moneyOut = moneyOut * -1,
    transactionType = "VENMO") %>%
  rename("venmoTransID" = "ID") %>%
  select(account,transactionType,venmoTransID,processDate,description,moneyFrom,moneyTo,moneyIn,moneyOut)


### CLASSIFY BUSINESS IN TRANSACTIONS ###

### COMBINED DEBIT & CREDIT TRANSACTIONS ###
transactions <- bind_rows(debitSECU,creditAPPLE) %>%
  filter(!str_detect(description,"PAYMENT - THANK YOU"))

# Classify purchases
transactions <- transactions %>%
  mutate(
    moneyTo = case_when(
      str_detect(description,"LA RANCHERITA") & !is.na(moneyOut) ~ "La Rancherita",
      str_detect(description,"TROPICAL SMOOTHIE") & !is.na(moneyOut) ~ "Tropical Smoothie Cafe",
      str_detect(description,"O2 Fitness") & !is.na(moneyOut) ~ "O2 Fitness",
      str_detect(description,"YouTube TV") & !is.na(moneyOut) ~ "YouTube TV",
      str_detect(description,"JPMorgan Chase") & !is.na(moneyOut) ~ "JPMorgan Chase & Co",
      str_detect(description,"COURTYARD BY MARRI") & !is.na(moneyOut) ~ "Courtyard by Marriott",
      str_detect(description,"THE RUSTIC") & !is.na(moneyOut) ~ "The Rustic",
      str_detect(description,"CORNER BAKERY") & !is.na(moneyOut) ~ "Corner Bakery Cafe",
      str_detect(description,"COWTOWN COLISEUM") & !is.na(moneyOut) ~ "Cowtown Coliseum",
      str_detect(description,"7-ELEVEN") & !is.na(moneyOut) ~ "7-Eleven",
      str_detect(description,"CHICK-FIL-A") & !is.na(moneyOut) ~ "Chick-fil-A",
      str_detect(description,"Arboretum Operat") & !is.na(moneyOut) ~ "Apartments at The Arboretum",
      str_detect(description,"Oddfellows") & !is.na(moneyOut) ~ "Oddfellows",
      str_detect(description,"TRUCK YARD DALLAS") & !is.na(moneyOut) ~ "Texas Truck Yard Dallas",
      str_detect(description,"WHOLEFDS") & !is.na(moneyOut) ~ "Whole Foods Market",
      str_detect(description,"NOODLES &") & !is.na(moneyOut) ~ "Noodles & Company",
      str_detect(description,"WAL-MAR") & !is.na(moneyOut) ~ "Walmart",
      str_detect(description,"THE LAKES GOLF") & !is.na(moneyOut) ~ "Lakes Country Club",
      str_detect(description,"GOODBERRY'S") & !is.na(moneyOut) ~ "Goodberry's Frozen Custard",
      str_detect(description,"PROVISSOUTHPORT") & !is.na(moneyOut) ~ "Provision Company",
      str_detect(description,"CHIPOTLE") & !is.na(moneyOut) ~ "Chipotle Mexican Grill",
      str_detect(description,"CIRCLE K") & !is.na(moneyOut) ~ "Circle K",
      str_detect(description,"Blue Surf Caf") & !is.na(moneyOut) ~ "Blue Surf Café",
      str_detect(description,"GRINDERS CAFFE") & !is.na(moneyOut) ~ "Grinders Caffè",
      str_detect(description,"EXXON") & !is.na(moneyOut) ~ "ExxonMobil",
      str_detect(description,"Acorns") & !is.na(moneyOut) ~ "Acorns",
      str_detect(description,"COOK OUT") & !is.na(moneyOut) ~ "Cook Out",
      str_detect(description,"MILK ROAD COFFEE") & !is.na(moneyOut) ~ "Milk Road Coffee",
      str_detect(description,"Disney Plus") & !is.na(moneyOut) ~ "Disney+",
      str_detect(description,"BULL CITY CIDER") & !is.na(moneyOut) ~ "Bull City Ciderworks",
      str_detect(description,"THE COMMONS") & !is.na(moneyOut) ~ "Duke University Dining",
      str_detect(description,"APPLECARD GSBANK PAYMENT") & !is.na(moneyOut) ~ "Credit Card Payment",
      str_detect(description,"Brew Coffee") & !is.na(moneyOut) ~ "BREW Coffee Bar",
      str_detect(description,"APPLE COM/BILL") & !is.na(moneyOut) ~ "Apple",
      str_detect(description,"MACPAW") & !is.na(moneyOut) ~ "CleanMyMac X",
      str_detect(description,"SCRATCH KITCH") & !is.na(moneyOut) ~ "Scratch Kitchen & Taproom",
      str_detect(description,"NIKE COM") & !is.na(moneyOut) ~ "Nike",
      str_detect(description,"MOE'S SW GRILL") & !is.na(moneyOut) ~ "Moe's Southwest Grill",
      str_detect(description,"Boricua Soul") & !is.na(moneyOut) ~ "Boricua Soul",
      str_detect(description,"PARTS UNKNOWN") & !is.na(moneyOut) ~ "Parts Unknown", 
      str_detect(description,"BP#") & !is.na(moneyOut) ~ "British Petroleum (BP)",
      str_detect(description,"NEW HANOVER COUNTYWILMINGTON") & !is.na(moneyOut) ~ "New Hanover Parking",
      str_detect(description,"MURPHY EXPRESS") & !is.na(moneyOut) ~ "Murphy USA",
      str_detect(description,"BOJANGLES") & !is.na(moneyOut) ~ "Bojangles",
      str_detect(description,"LOS TRES MAGUEYES") & !is.na(moneyOut) ~ "Los Tres Magueyes",
      str_detect(description,"JUBALA") & !is.na(moneyOut) ~ "Jubala Coffee",
      str_detect(description,"Tobacco Road") & !is.na(moneyOut) ~ "Tobacco Road Sports Cafe",
      str_detect(description,"M Sushi") & !is.na(moneyOut) ~ "M Sushi",
      str_detect(description,"ACH Debit PAYPAL") & !is.na(moneyOut) ~ "Starbucks",
      str_detect(description,"ABERCROMBIE") & !is.na(moneyOut) ~ "Abercrombie & Fitch",
      str_detect(description,"SECU ") & !is.na(moneyOut) ~ "State Employees Credit Union",
      str_detect(description,"Internet Transfer CREDIT TO  SV") & !is.na(moneyOut) ~ "State Employees Credit Union",
      str_detect(description,"WALGREENS") & !is.na(moneyOut) ~ "Walgreens",
      str_detect(description,"CRISP SALAD") & !is.na(moneyOut) ~ "Crisp",
      str_detect(description,"MADEWELL") & !is.na(moneyOut) ~ "Madewell",
      str_detect(description,"MADEWELL") & !is.na(moneyOut) ~ "Madewell",
      str_detect(description,"MORRISVILLE PAR") & !is.na(moneyOut) ~ "Unknown",
      str_detect(description," TARGET ") & !is.na(moneyOut) ~ "Target Corporation",
      str_detect(description,"MAX COM") & !is.na(moneyOut) ~ "HBO Max",
      str_detect(description,"AMZN Mktp") & !is.na(moneyOut) ~ "Amazon.com",
      str_detect(description,"Internet Transfer CREDIT TO  CC") & !is.na(moneyOut) ~ "Credit Card Payment",
      str_detect(description,"STARBUCKS") & !is.na(moneyOut) ~ "Starbucks",
      str_detect(description,"FRONTIER AI") & !is.na(moneyOut) ~ "Frontier Airlines",
      str_detect(description,"UBER") & !is.na(moneyOut) ~ "Uber Technologies Inc",
      str_detect(description,"KOE WETZEL'S RIOT ROOM") & !is.na(moneyOut) ~ "Koe Wetzel's Riot Room",
      str_detect(description,"SECOND RODEO") & !is.na(moneyOut) ~ "Second Rodeo Brewing",
      str_detect(description,"HG SPLY CO") & !is.na(moneyOut) ~ "HG Sply Co.",
      str_detect(description,"Chelseas Corner") & !is.na(moneyOut) ~ "Chelseas Corner",
      str_detect(description,"TEXAS BB CONCESSIONS") & !is.na(moneyOut) ~ "Texas Rangers Concessions",
      str_detect(description,"CAFE DURO") & !is.na(moneyOut) ~ "Cafe Duro",
      str_detect(description,"DFW CNBC") & !is.na(moneyOut) ~ "CNBC Store",
      str_detect(description,"PNH DFW BWW") & !is.na(moneyOut) ~ "Buffalo Wild Wings",
      str_detect(description,"TGI FRIDAYS") & !is.na(moneyOut) ~ "TGI Fridays",
      str_detect(description,"ATLANTIC GARDENING") & !is.na(moneyOut) ~ "Atlantic Gardening",
      str_detect(description,"HUMMINGBIRD") & !is.na(moneyOut) ~ "Hummingbird Restaurant",
      str_detect(description,"TRADER JOE") & !is.na(moneyOut) ~ "Trader Joe's",
      str_detect(description,"TARGET ") & !is.na(moneyOut) ~ "Target Corporation",
      str_detect(description,"CHEWY.COM") & !is.na(moneyOut) ~ "Chewy",
      str_detect(description,"BILLFISH GRILL") & !is.na(moneyOut) ~ "BILLFISH Grill & Bar",
      str_detect(description,"SALT MARSH COTTAGE") & !is.na(moneyOut) ~ "Salt Marsh Cottage",
      str_detect(description,"BEST DAY EVER A DOG") & !is.na(moneyOut) ~ "Best Day Ever Dog Boutique",
      str_detect(description,"SOCIAL COFFEE") & !is.na(moneyOut) ~ "Social Coffee and Supply Co.",
      str_detect(description,"Spectrum") & !is.na(moneyOut) ~ "Spectrum",
      str_detect(description,"NEUSE SPORT SHOP") & !is.na(moneyOut) ~ "Neuse Sport Shop",
      str_detect(description,"FOOD LION") & !is.na(moneyOut) ~ "Food Lion",
      str_detect(description,"DORCAS MINISTRIES") & !is.na(moneyOut) ~ "Dorcas Ministries",
      str_detect(description,"THE VILLAGE MARKET") & !is.na(moneyOut) ~ "The Village Market",
      str_detect(description,"HARRIS TEETER") & !is.na(moneyOut) ~ "Harris Teeter",
      str_detect(description,"PAYOUTS-NETWORK") & !is.na(moneyOut) ~ "Payouts Network Inc.",
      str_detect(description,"Interest Charge on Purchases") & !is.na(moneyOut) ~ "State Employees Credit Union",
      str_detect(description,"Interest Charge on Cash Advances") & !is.na(moneyOut) ~ "State Employees Credit Union",
      str_detect(description,"Trnsfr From Overdraft XFER FROM  SV") & !is.na(moneyIn) ~ paste0(account,"-Checking"),
      str_detect(description,"Internet Transfer DEBIT FROM SV") & !is.na(moneyIn) ~ paste0(account,"-Checking"),
      str_detect(description,"GOOGLE STORAGE") & !is.na(moneyOut) ~ "Google",
      str_detect(description,"PORT CITY JAVA") & !is.na(moneyOut) ~ "Port City Java",
      str_detect(description,"PT'S OLDE FASHIONED GR") & !is.na(moneyOut) ~ "P.T.'s Olde Fashioned Grille",
      str_detect(description,"VAN LEEUWEN ICE CR") & !is.na(moneyOut) ~ "Van Leeuwen Ice Cream",
      str_detect(description,"FADES AND SHAVES B") & !is.na(moneyOut) ~ "Fades and Shaves by Marsha",
      str_detect(description,"BREW COFFEE BAR") & !is.na(moneyOut) ~ "BREW Coffee Bar",
      str_detect(description,"FIDDLE TREE FARM") & !is.na(moneyOut) ~ "Fiddle Tree Farm",
      str_detect(description,"SEA PAWS") & !is.na(moneyOut) ~ "Sea Paws",
      str_detect(description,"LUCKY TREE") & !is.na(moneyOut) ~ "Lucky Tree",
      str_detect(description,"PAX & BENEFICIA") & !is.na(moneyOut) ~ "Pax & Beneficia Coffee",
      str_detect(description,"TYLER KINGSTON") & !is.na(moneyOut) ~ "Tyler Kingston Mercantile",
      str_detect(description,"HUDSONST1738 2400 JOHN BRANTLEY BLVD RALEIGH") & !is.na(moneyOut) ~ "Starbucks",
      str_detect(description,"HARRIS TE") & !is.na(moneyOut) ~ "Harris Teeter",
      str_detect(description,"COLDSTONE CREAMERY") & !is.na(moneyOut) ~ "Cold Stone Creamery",
      str_detect(description,"MARCO'S PIZZA") & !is.na(moneyOut) ~ "Marco's Pizza",
      str_detect(description,"BRU'S PUBLIC HOUSE") & !is.na(moneyOut) ~ "Bru's Public House",
      str_detect(description,"AMAZON") & !is.na(moneyOut) ~ "Amazon.com",
      str_detect(description,"TARGET") & !is.na(moneyOut) ~ "Target Corporation",
      str_detect(description,"SHEETZ") & !is.na(moneyOut) ~ "Sheetz",
      str_detect(description,"MOE'S SOUTHWEST GRILL") & !is.na(moneyOut) ~ "Moe's Southwest Grill",
    )
  )

# Classify purchases
transactions <- transactions %>%
  mutate(
    moneyFrom = case_when(
      # External Check
      str_detect(description,"Member Deposit") & !is.na(moneyIn) ~ "External Check",
      # Transfers
      str_detect(description,"Internet Transfer DEBIT FROM SV") & !is.na(moneyIn) ~ paste0(account,"-Savings"),
      # Apple card cash back
      str_detect(description,"VISA Money Transfer") & str_detect(description,"APPLE CASH") & !is.na(moneyIn) ~ "Apple",
      # Dividend earned from SECU
      str_detect(description,"Dividend Earned") & !is.na(moneyIn) ~ "State Employees Credit Union"
    ),
    # Classify pull from Savings to Debit as negative to make sense of summing later on
    moneyIn = case_when(
      str_detect(description,"Internet Transfer DEBIT FROM SV") & !is.na(moneyIn) ~ moneyIn * -1,
      TRUE ~ moneyIn)) %>%
  select(account,transactionType,venmoTransID,processDate,description,moneyFrom,moneyTo,moneyIn,moneyOut)

# Union Venmo transactions with SECU transactions
transactions <- bind_rows(transactions,debitVENMO)

# ### VALIDATION ###
# # Identify unknown payment received
# transactions %>%
#   filter(!is.na(moneyIn) & is.na(moneyFrom))
# # Identify unknown payment sent
# transactions %>%
#   filter(!is.na(moneyOut) & is.na(moneyTo))


### CATEGORIZE TRANSACTIONS ###

# Categorize transactions
transactions <- transactions %>%
  mutate(
    toCategory = case_when(
      # Consumables
      moneyTo == 'La Rancherita' ~ 'Consumables',
      moneyTo == 'Tropical Smoothie Cafe' ~ 'Consumables',
      moneyTo == 'The Rustic' ~ 'Consumables',
      moneyTo == 'Corner Bakery Cafe' ~ 'Consumables',
      moneyTo == 'Oddfellows' ~ 'Consumables',
      moneyTo == 'Chick-fil-A' ~ 'Consumables',
      moneyTo == 'Texas Truck Yard Dallas' ~ 'Consumables',
      moneyTo == 'Whole Foods Market' ~ 'Consumables',
      moneyTo == 'Noodles & Company' ~ 'Consumables',
      moneyTo == 'Provision Company' ~ 'Consumables',
      moneyTo == 'Chipotle Mexican Grill' ~ 'Consumables',
      moneyTo == 'Blue Surf Café' ~ 'Consumables',
      moneyTo == 'Bojangles' ~ 'Consumables',
      moneyTo == 'Grinders Caffè' ~ 'Consumables',
      moneyTo == 'Cook Out' ~ 'Consumables',
      moneyTo == 'Milk Road Coffee' ~ 'Consumables',
      moneyTo == 'Bull City Ciderworks' ~ 'Consumables',
      moneyTo == 'Duke University Dining' ~ 'Consumables',
      moneyTo == 'BREW Coffee Bar' ~ 'Consumables',
      moneyTo == 'Jubala Coffee' ~ 'Consumables',
      moneyTo == 'Scratch Kitchen & Taproom' ~ 'Consumables',
      moneyTo == 'Los Tres Magueyes' ~ 'Consumables',
      moneyTo == 'M Sushi' ~ 'Consumables',
      moneyTo == 'Boricua Soul' ~ 'Consumables',
      moneyTo == 'Tobacco Road Sports Cafe' ~ 'Consumables',
      moneyTo == 'Crisp' ~ 'Consumables',
      moneyTo == "Moe's Southwest Grill" ~ 'Consumables',
      moneyTo == "Goodberry's Frozen Custard" ~ 'Consumables',
      moneyTo == "Starbucks" ~ 'Consumables',
      moneyTo == "Koe Wetzel's Riot Room" ~ 'Consumables',
      moneyTo == "Second Rodeo Brewing" ~ 'Consumables',
      moneyTo == "HG Sply Co." ~ 'Consumables',
      moneyTo == "Chelseas Corner" ~ 'Consumables',
      moneyTo == "Texas Rangers Concessions" ~ 'Consumables',
      moneyTo == "Cafe Duro" ~ 'Consumables',
      moneyTo == "Buffalo Wild Wings" ~ 'Consumables',
      moneyTo == "TGI Fridays" ~ 'Consumables',
      moneyTo == "Hummingbird Restaurant" ~ 'Consumables',
      moneyTo == "Trader Joe's" ~ 'Consumables',
      moneyTo == "BILLFISH Grill & Bar" ~ 'Consumables',
      moneyTo == "Social Coffee and Supply Co." ~ 'Consumables',
      moneyTo == "Food Lion" ~ 'Consumables',
      moneyTo == "The Village Market" ~ 'Consumables',
      moneyTo == "Harris Teeter" ~ 'Consumables',
      moneyTo == 'Port City Java' ~ 'Consumables',
      moneyTo == "P.T.'s Olde Fashioned Grille" ~ 'Consumables',
      moneyTo == 'Van Leeuwen Ice Cream' ~ 'Consumables',
      moneyTo == 'Lucky Tree' ~ 'Consumables',
      moneyTo == 'Pax & Beneficia Coffee' ~ 'Consumables',
      moneyTo == 'Cold Stone Creamery' ~ 'Consumables',
      moneyTo == "Marco's Pizza" ~ 'Consumables',
      moneyTo == "Bru's Public House" ~ 'Consumables',
      moneyTo == "Moe's Southwest Grill" ~ 'Consumables',
      # Entertainment
      moneyTo == 'YouTube TV' ~ 'Entertainment', 
      moneyTo == 'Cowtown Coliseum' ~ 'Entertainment',
      moneyTo == 'Apple' ~ 'Entertainment',
      moneyTo == 'Lakes Country Club' ~ 'Entertainment',
      moneyTo == 'Disney+' ~ 'Entertainment',
      moneyTo == 'HBO Max' ~ 'Entertainment',
      # Lifestyle
      moneyTo == 'Parts Unknown' ~ 'Lifestyle',
      moneyTo == 'O2 Fitness' ~ 'Lifestyle',
      moneyTo == 'Nike' ~ 'Lifestyle',
      moneyTo == 'Madewell' ~ 'Lifestyle',
      str_detect(description,"MORRISVILLE PAR") & !is.na(moneyOut) ~ 'Lifestyle',
      moneyTo == "Atlantic Gardening" ~ 'Lifestyle',
      moneyTo == "Neuse Sport Shop" ~ 'Lifestyle',
      moneyTo == "Dorcas Ministries" ~ 'Lifestyle',
      moneyFrom == 'Madewell' ~ 'Lifestyle',
      moneyTo == 'Fades and Shaves by Marsha' ~ 'Lifestyle',
      moneyTo == 'Fiddle Tree Farm' ~ 'Lifestyle',
      moneyTo == 'Abercrombie & Fitch' ~ 'Lifestyle',
      # Transportation & Travel
      moneyTo == 'JPMorgan Chase & Co' ~ 'Transportation & Travel',
      moneyTo == 'Courtyard by Marriott' ~ 'Transportation & Travel',
      moneyTo == '7-Eleven' ~ 'Transportation & Travel',
      moneyTo == 'Murphy USA' ~ 'Transportation & Travel',
      moneyTo == 'Circle K' ~ 'Transportation & Travel',
      moneyTo == 'British Petroleum (BP)' ~ 'Transportation & Travel',
      moneyTo == 'ExxonMobil' ~ 'Transportation & Travel',
      moneyTo == 'New Hanover Parking' ~ 'Transportation & Travel',
      moneyTo == "Frontier Airlines" ~ 'Transportation & Travel',
      moneyTo == "Uber Technologies Inc" ~ 'Transportation & Travel',
      moneyTo == "Sheetz" ~ 'Transportation & Travel',
      # Utilities, Services & Charges
      moneyTo == 'State Employees Credit Union' & str_detect(description,"SECU Foundation") ~ 'Utilities, Services & Charges',
      moneyTo == 'Acorns' & str_detect(description,"Subscription") ~ 'Utilities, Services & Charges',
      str_detect(description,"Interest Charge on Purchases") & moneyTo == 'State Employees Credit Union' ~ 'Utilities, Services & Charges',
      str_detect(description,"Interest Charge on Cash Advances") & moneyTo == 'State Employees Credit Union' ~ 'Utilities, Services & Charges',
      moneyTo == 'Google' & str_detect(description,"GOOGLE STORAGE") ~ 'Utilities, Services & Charges',
      moneyTo == "Spectrum" ~ 'Utilities, Services & Charges',
      moneyTo == 'CleanMyMac X' ~ 'Utilities, Services & Charges',
      # Savings & Investments
      moneyTo == 'State Employees Credit Union' & str_detect(description,"CREDIT TO  SV") ~ 'Savings & Investments',
      moneyTo == 'Acorns' & str_detect(description,"Acorns Invest") ~ 'Savings & Investments',
      # Housing
      moneyTo == 'Apartments at The Arboretum' ~ 'Housing',
      # General Merchandise
      moneyTo == 'Walmart' ~ 'General Merchandise',
      moneyTo == 'Target Corporation' ~ 'General Merchandise',
      moneyTo == 'Amazon.com' ~ 'General Merchandise',
      moneyTo == "CNBC Store" ~ 'General Merchandise',
      moneyTo == "Salt Marsh Cottage" ~ 'General Merchandise',
      moneyTo == 'Tyler Kingston Mercantile' ~ 'General Merchandise',
      # Health & Medical
      moneyTo == 'Walgreens' ~ 'Health & Medical',
      # Debt
      moneyTo == 'Credit Card Payment' ~ 'Debt',
      moneyTo == "Payouts Network Inc." ~ 'Debt',
      # Pets
      moneyTo == "Chewy" ~ 'Pets',
      moneyTo == "Best Day Ever Dog Boutique" ~ 'Pets',
      moneyTo == 'Sea Paws' ~ 'Pets',
      # Income & Gifts
      moneyFrom == 'State Employees Credit Union' & str_detect(description,"Dividend Earned") ~ 'Income & Gifts',
      moneyFrom == 'Apple' ~ 'Income & Gifts',
      moneyFrom == 'External Check' ~ 'Income & Gifts',
      moneyFrom == 'Duke University' ~ 'Income & Gifts',
      moneyFrom == 'Dilworth Coffee' ~ 'Income & Gifts'
    )
  )


### SUB CATEGORIES FOR TRANSACTIONS ###

# Categorize transactions
transactions <- transactions %>%
  mutate(
    toSubCategory = case_when(
      # Consumables
      moneyTo == 'La Rancherita' ~ 'Dining & Drinks',
      moneyTo == 'Tropical Smoothie Cafe' ~ 'Dining & Drinks',
      moneyTo == 'The Rustic' ~ 'Dining & Drinks',
      moneyTo == 'Corner Bakery Cafe' ~ 'Dining & Drinks',
      moneyTo == 'Oddfellows' ~ 'Dining & Drinks',
      moneyTo == 'Chick-fil-A' ~ 'Dining & Drinks',
      moneyTo == 'Texas Truck Yard Dallas' ~ 'Dining & Drinks',
      moneyTo == 'Whole Foods Market' ~ 'Groceries',
      moneyTo == 'Noodles & Company' ~ 'Dining & Drinks',
      moneyTo == 'Provision Company' ~ 'Dining & Drinks',
      moneyTo == 'Chipotle Mexican Grill' ~ 'Dining & Drinks',
      moneyTo == 'Blue Surf Café' ~ 'Dining & Drinks',
      moneyTo == 'Bojangles' ~ 'Dining & Drinks',
      moneyTo == 'Grinders Caffè' ~ 'Dining & Drinks',
      moneyTo == 'Cook Out' ~ 'Dining & Drinks',
      moneyTo == 'Milk Road Coffee' ~ 'Dining & Drinks',
      moneyTo == 'Bull City Ciderworks' ~ 'Dining & Drinks',
      moneyTo == 'Duke University Dining' ~ 'Dining & Drinks',
      moneyTo == 'BREW Coffee Bar' ~ 'Dining & Drinks',
      moneyTo == 'Jubala Coffee' ~ 'Dining & Drinks',
      moneyTo == 'Scratch Kitchen & Taproom' ~ 'Dining & Drinks',
      moneyTo == 'Los Tres Magueyes' ~ 'Dining & Drinks',
      moneyTo == 'M Sushi' ~ 'Dining & Drinks',
      moneyTo == 'Boricua Soul' ~ 'Dining & Drinks',
      moneyTo == 'Tobacco Road Sports Cafe' ~ 'Dining & Drinks',
      moneyTo == 'Crisp' ~ 'Dining & Drinks',
      moneyTo == "Moe's Southwest Grill" ~ 'Dining & Drinks',
      moneyTo == "Goodberry's Frozen Custard" ~ 'Dining & Drinks',
      moneyTo == "Starbucks" ~ 'Dining & Drinks',
      moneyTo == "Koe Wetzel's Riot Room" ~ 'Dining & Drinks',
      moneyTo == "Second Rodeo Brewing" ~ 'Dining & Drinks',
      moneyTo == "HG Sply Co." ~ 'Dining & Drinks',
      moneyTo == "Chelseas Corner" ~ 'Dining & Drinks',
      moneyTo == "Texas Rangers Concessions" ~ 'Dining & Drinks',
      moneyTo == "Cafe Duro" ~ 'Dining & Drinks',
      moneyTo == "Buffalo Wild Wings" ~ 'Dining & Drinks',
      moneyTo == "TGI Fridays" ~ 'Dining & Drinks',
      moneyTo == "Hummingbird Restaurant" ~ 'Dining & Drinks',
      moneyTo == "Trader Joe's" ~ 'Groceries',
      moneyTo == "BILLFISH Grill & Bar" ~ 'Dining & Drinks',
      moneyTo == "Social Coffee and Supply Co." ~ 'Dining & Drinks',
      moneyTo == "Food Lion" ~ 'Groceries',
      moneyTo == "The Village Market" ~ 'Dining & Drinks',
      moneyTo == "Harris Teeter" ~ 'Groceries',
      moneyTo == 'Port City Java' ~ 'Dining & Drinks',
      moneyTo == "P.T.'s Olde Fashioned Grille" ~ 'Dining & Drinks',
      moneyTo == 'Van Leeuwen Ice Cream' ~ 'Dining & Drinks',
      moneyTo == 'Lucky Tree' ~ 'Dining & Drinks',
      moneyTo == 'Pax & Beneficia Coffee' ~ 'Dining & Drinks',
      moneyTo == 'Cold Stone Creamery' ~ 'Dining & Drinks',
      moneyTo == "Marco's Pizza" ~ 'Dining & Drinks',
      moneyTo == "Bru's Public House" ~ 'Dining & Drinks',
      moneyTo == "Moe's Southwest Grill" ~ 'Dining & Drinks',
      # Entertainment
      moneyTo == 'YouTube TV' ~ 'Streaming & Music', 
      moneyTo == 'Cowtown Coliseum' ~ 'Social & Activities',
      moneyTo == 'Apple' ~ 'Streaming & Music',
      moneyTo == 'Lakes Country Club' ~ 'Social & Activities',
      moneyTo == 'Disney+' ~ 'Streaming & Music',
      moneyTo == 'HBO Max' ~ 'Streaming & Music',
      # Lifestyle
      moneyTo == 'Parts Unknown' ~ 'Shopping',
      moneyTo == 'O2 Fitness' ~ 'Personal Care & Fitness',
      moneyTo == 'Nike' ~ 'Shopping',
      moneyTo == 'Madewell' ~ 'Shopping',
      str_detect(description,"MORRISVILLE PAR") & !is.na(moneyOut) ~ 'Shopping',
      moneyTo == "Atlantic Gardening" ~ 'Shopping',
      moneyTo == "Neuse Sport Shop" ~ 'Shopping',
      moneyTo == "Dorcas Ministries" ~ 'Shopping',
      moneyFrom == 'Madewell' ~ 'Shopping',
      moneyTo == 'Fades and Shaves by Marsha' ~ 'Personal Care & Fitness',
      moneyTo == 'Fiddle Tree Farm' ~ 'Personal Care & Fitness',
      moneyTo == 'Abercrombie & Fitch' ~ 'Shopping',
      # Transportation & Travel
      moneyTo == 'JPMorgan Chase & Co' ~ 'Car Payment & Insurance',
      moneyTo == 'Courtyard by Marriott' ~ 'Travel Expenses',
      moneyTo == '7-Eleven' ~ 'Fuel & Auto Maintenance',
      moneyTo == 'Murphy USA' ~ 'Fuel & Auto Maintenance',
      moneyTo == 'Circle K' ~ 'Fuel & Auto Maintenance',
      moneyTo == 'British Petroleum (BP)' ~ 'Fuel & Auto Maintenance',
      moneyTo == 'ExxonMobil' ~ 'Fuel & Auto Maintenance',
      moneyTo == 'New Hanover Parking' ~ 'Tolls, Parking & Other',
      moneyTo == "Frontier Airlines" ~ 'Travel Expenses',
      moneyTo == "Uber Technologies Inc" ~ 'Tolls, Parking & Other',
      moneyTo == "Sheetz" ~ 'Fuel & Auto Maintenance',
      # Utilities, Services & Charges
      
      # Savings & Investments
      
      # Housing
      moneyTo == 'Apartments at The Arboretum' ~ 'Rent',
      # General Merchandise
      
      # Health & Medical
      
      # Debt
      
      # Pets
      
      # Income & Gifts
      
      # Else main category
      TRUE ~ toCategory
    )
  )


# Replace NA values in moneyIn and moneyOut fields with 0
transactions$moneyIn[is.na(transactions$moneyIn)] <- 0
transactions$moneyOut[is.na(transactions$moneyOut)] <- 0

# ### VALIDATION ###
# # Identify unknown payment vendors
# transactions %>%
#   filter(!is.na(moneyOut) & !is.na(moneyTo) & is.na(toCategory))
# # Identify unknown payment from
# transactions %>%
#   filter(!is.na(moneyIn) & !is.na(moneyFrom) & is.na(toCategory))


### WRITE DATA TO GOOGLE SHEET ###

# Write the data frame to the Google Sheet, replacing the entire sheet
write_sheet(transactions, sheet_url, sheet = "moneySpent")






















