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

# Czechia
cze_birth <- readHFDweb(CNTRY = "CZE",
                          item = "birthsTR",
                          username = "gonzalo.fce@gmail.com",
                          password = "fermat31416")


cze_exposure <- readHFDweb(CNTRY = "CZE",
                             item = "exposTR",
                             username = "gonzalo.fce@gmail.com",
                             password = "fermat31416")


# Korea
kor_birth <- readHFDweb(CNTRY = "KOR",
                        item = "birthsTR",
                        username = "gonzalo.fce@gmail.com",
                        password = "fermat31416")


kor_exposure <- readHFDweb(CNTRY = "KOR",
                           item = "exposTR",
                           username = "gonzalo.fce@gmail.com",
                           password = "fermat31416")


# Wrangling time

# Spain
spain_birth2 <- spain_birth %>% 
                  filter(OpenInterval == FALSE) %>% 
                  group_by(Year, Age) %>% 
                  summarise(Births = sum(Total, na.rm = TRUE))

spain_exposure2 <- spain_exposure %>% 
                  filter(OpenInterval == FALSE) %>% 
                  group_by(Year, Age) %>% 
                  summarise(Exposure = sum(Exposure, na.rm = TRUE))


# Czechia
cze_birth2 <- cze_birth %>% 
  filter(OpenInterval == FALSE) %>% 
  group_by(Year, Age) %>% 
  summarise(Births = sum(Total, na.rm = TRUE))

cze_exposure2 <- cze_exposure %>% 
  filter(OpenInterval == FALSE) %>% 
  group_by(Year, Age) %>% 
  summarise(Exposure = sum(Exposure, na.rm = TRUE))

# Korea
kor_birth2 <- kor_birth %>% 
  filter(OpenInterval == FALSE) %>% 
  group_by(Year, Age) %>% 
  summarise(Births = sum(Total, na.rm = TRUE))

kor_exposure2 <- kor_exposure %>% 
  filter(OpenInterval == FALSE) %>% 
  group_by(Year, Age) %>% 
  summarise(Exposure = sum(Exposure, na.rm = TRUE))




# All together now
spain <- spain_exposure2 %>% left_join(spain_birth2, by = c("Year", "Age"))

czechia <- cze_exposure2 %>% left_join(cze_birth2, by = c("Year", "Age"))

korea <- kor_exposure2 %>% left_join(kor_birth2, by = c("Year", "Age"))

# Deleting what we don't use anymore
rm(spain_birth, spain_birth2,
   spain_exposure, spain_exposure2,
   cze_birth, cze_birth2,
   cze_exposure, cze_exposure2,
   kor_birth, kor_birth2,
   kor_exposure, kor_exposure2)

# All together in 1 data.frame
spain$cty <- "Spain"
czechia$cty <- "Czechia"
korea$cty <- "Korea"

challenge2 <- rbind(spain, czechia, korea)



# Some plotting
challenge2 %>% 
  group_by(cty, Year) %>% 
  summarise(Total_Birth = sum(Births, na.rm = T),
            Total_Exposure = sum(Exposure, na.mr = T)) %>% 
  mutate(TFR = Total_Birth/Total_Exposure) %>% 
  filter(Year >= 2000) %>% 
  ggplot(aes(x=Year, y = TFR, color = cty)) + geom_line()









