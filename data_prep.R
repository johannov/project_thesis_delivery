#### Data processing for project ####



library(tidyverse)
library(countrycode)
library(zoo)

# Importing "End-use prices: Energy prices in national currency per toe" from IEA (2021)
prices <- read_csv("./data/END_TOE-2021-1-EN-20220628T112037.csv")

# Extracting household prices for natural gas and selected substitutes
substitutes <- c("Natural gas", "Light fuel oil", "Electricity")

prices <- subset(prices, Product %in% substitutes)
prices <- subset(prices, Sector == "Households")
prices <- subset(prices, Flow == "Total price (nat. cur./toe NCV)")

# Extracting annual values
prices <- subset(prices, !grepl("Q", Time))

prices$id <- paste(prices$IEA_LOCATION, prices$TIME)

prices <- pivot_wider(prices, id, names_from = IEA_PRODUCT, values_from = Value)

# Importing "CPI: 01-12 - All items, Index" from OECD.Stat, base year 2015
cpi <- read_csv("./data/PRICES_CPI_20102022111335885.csv")
cpi <- subset(cpi, Subject == "CPI: 01-12 - All items")
cpi$id <- paste(cpi$LOCATION, cpi$TIME)
names(cpi)[names(cpi)=="Value"] <- "cpi"

# Importing "Households and NPISHs Final consumption expenditure (current LCU)" from The World Bank
income <- read_csv("./data/082329d9-97ff-4b96-8b2a-ffa7b8f59517_Data.csv", na = c("", "NA", ".."))
income <- pivot_longer(income, cols = !c("Series Name", "Series Code", "Country Name", "Country Code"), names_to = "Time", values_to = "income")
income$Time <-  substr(income$Time, 1, 4)
income$Time <- as.numeric(income$Time)
income$id <- paste(income$`Country Code`, income$Time)

# Importing "Population, total" from The World Bank
pop <- read_csv("./data/bca880c3-507f-45db-9eb2-77dcf288a655_Data.csv", na = c("", "NA", ".."))
pop <- pivot_longer(pop, cols = !c("Series Name", "Series Code", "Country Name", "Country Code"), names_to = "Time", values_to = "pop")
pop$Time = substr(pop$Time, 1, 4)
pop$Time <- as.numeric(pop$Time)
pop$id <- paste(pop$`Country Code`, pop$Time)

# Importing "World energy balances, Natural Gas, Residential" from IEA / OECD iLibrary
demand <- read_csv("./data/WBAL_23102022174555611.csv")
demand$id <- paste(demand$COUNTRY, demand$TIME)
names(demand)[names(demand)=="Value"] <- "demand"

# Importing "Cooling and heating degree days by country - annual data" from Eurostat
hcdd <- read_csv("./data/nrg_chdd_a_linear.csv")
hcdd$geo <- countrycode(hcdd$geo, "eurostat", "iso3c")
hcdd$id <- paste(hcdd$geo, hcdd$TIME_PERIOD)
hcdd <- pivot_wider(hcdd, id_cols = "id", values_from = "OBS_VALUE", names_from = "indic_nrg")

# Joining imported data
rm(df)
df <- merge(cpi[c("cpi", "id")], prices, by = "id", all = TRUE)
df <- merge(df, pop[c("pop", "id")], by = "id", all = TRUE)
df <- merge(df, income[c("income", "id")], by = "id", all = TRUE)
df <- merge(df, demand[c("demand", "id")], by = "id", all.x = TRUE)
df <- merge(df, hcdd[c("HDD", "id")], by = "id", all.x = TRUE)


df$Country <- substr(df$id, 1, 3)
df$Time <- substr(df$id, 5, 8)
df$Time <- as.numeric(df$Time)


# Standardizing variable names
nom_tot <- dplyr::rename(df,
                         "time" = "Time",
                         "country" = "Country",
                         "price_ng"= "NATGAS",
                         "price_lfo" = "LFO",
                         "price_el" = "ELECTR",
                         "hdd" = "HDD")
rm(df)

# Calculating per capita values
nom_pc <- nom_tot
pc_var <- c("income", "demand")
nom_pc[pc_var] <- nom_pc[pc_var] / nom_pc$pop
nom_pc$demand <- nom_pc$demand * 1000000 # converting to kgoe

# Deflating monetary values
real_pc <- nom_pc
monetary_var <- c("income", "price_ng", "price_el", "price_lfo")
real_pc[monetary_var] <- real_pc[monetary_var] / ( real_pc$cpi * 0.01 )

# Extracting EU + EFTA + UK
eu <-  c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK", "IS", "LI", "NO", "CH")
eu <- countrycode(eu, "eurostat", "iso3c")
real_pc_eu <- subset(real_pc, country %in% eu)

# Dropping insignificant demand observations
lcudata <- subset(real_pc_eu, !is.na(demand) & demand > 0.01)

# Dropping known unreliable data
lcudata <- subset(lcudata, country != "DEU" | time > 1983)
lcudata <- subset(lcudata, country != "ESP" | time > 1986)
lcudata <- subset(lcudata, country != "BEL")

# Dropping incomplete observations before first complete observation
lcudata$complete <- !is.na(lcudata$income * lcudata$price_ng * lcudata$demand * lcudata$hdd)

firstYears <- c()

for (state in unique(lcudata$country)) {
  statedf <- subset(lcudata, country == state)
  firstYear <- statedf[which(statedf$complete)[1],]$time
  lcudata <- subset(lcudata, country != state | time >= firstYear)
  firstYears[state] <- firstYear
}

# Counting number of complete observations
counts <- aggregate(complete ~ country, data = lcudata, sum)
counts_suff <- subset(counts, complete > 25)

# List of countries analyzed
suff <- counts_suff$country
suff <- sort(suff)

# Dropping countries out of scope
lcudata <- subset(lcudata, country %in% suff)

# Flagging successive incomplete observations
lcudata$two_incomplete <- !lcudata$complete & !(dplyr::lag(lcudata$complete) & dplyr::lead(lcudata$complete))

# Imputing missing price observations, linear interpolation
lcudata <- lcudata %>%
  mutate(price_ng_imp = na.approx(price_ng))

# PPP data for later comparisons
ppp <- read_csv("./data/SNA_TABLE4_24102022144336354.csv")
ppp <- dplyr::rename(ppp, "ppp_rate" = "Value")
ppp$id <- paste(ppp$LOCATION, ppp$TIME)
pppadj <- merge(lcudata, ppp[c("ppp_rate", "id")], by = "id")
pppadj[c("income", "price_ng", "price_lfo", "price_el", "price_ng_imp")] <- pppadj[c("income", "price_ng", "price_lfo", "price_el", "price_ng_imp")] / pppadj$ppp_rate


# Exporting output files
write.csv(pppadj, "./output/pppadj.csv")
write.csv(lcudata, "./output/lcudata.csv") # Local currencies used in modelling
