# Loading packages
library(tidyverse)
library(HMDHFDplus)



# Check countries
getHFDcountries()


# Downloading data for 3 countries: Spain, Bulgaria, Korea


# Spain
spain_birth <- readHFDweb(CNTRY = "ESP",
                    item = "birthsTR",
                    username = "gonzalo.fce@gmail.com",
                    password = "fermat31416")


spain_exposure <- readHFDweb(CNTRY = "ESP",
                          item = "exposTR",
                          username = "gonzalo.fce@gmail.com",
                          password = "fermat31416")

# Bulgaria
bul_birth <- readHFDweb(CNTRY = "BGR",
                          item = "birthsTR",
                          username = "gonzalo.fce@gmail.com",
                          password = "fermat31416")


bul_exposure <- readHFDweb(CNTRY = "BGR",
                             item = "exposTR",
                             username = "gonzalo.fce@gmail.com",
                             password = "fermat31416")


# Korea
bul_birth <- readHFDweb(CNTRY = "KOR",
                        item = "birthsTR",
                        username = "gonzalo.fce@gmail.com",
                        password = "fermat31416")


bul_exposure <- readHFDweb(CNTRY = "KOR",
                           item = "exposTR",
                           username = "gonzalo.fce@gmail.com",
                           password = "fermat31416")


# Wrangling time
spain_birth2 <- spain_birth %>% 
                  filter(OpenInterval == FALSE) %>% 
                  group_by(Year) %>% 
                  summarise(Births = sum(Total, na.rm = TRUE))

spain_exposure2 <- spain_exposure %>% 
                  filter(OpenInterval == FALSE) %>% 
                  group_by(Year) %>% 
                  summarise(Exposure = sum(Exposure, na.rm = TRUE))



