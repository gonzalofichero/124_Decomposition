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


# Bulgaria
bul_birth2 <- bul_birth %>% 
  filter(OpenInterval == FALSE) %>% 
  group_by(Year, Age) %>% 
  summarise(Births = sum(Total, na.rm = TRUE))

bul_exposure2 <- bul_exposure %>% 
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

bulgaria <- bul_exposure2 %>% left_join(bul_birth2, by = c("Year", "Age"))

korea <- kor_exposure2 %>% left_join(kor_birth2, by = c("Year", "Age"))

# Deleting what we don't use anymore
rm(spain_birth, spain_birth2,
   spain_exposure, spain_exposure2,
   bul_birth, bul_birth2,
   bul_exposure, bul_exposure2,
   kor_birth, kor_birth2,
   kor_exposure, kor_exposure2)

# All together in 1 data.frame
spain$cty <- "Spain"
bulgaria$cty <- "Bulgaria"
korea$cty <- "Korea"

challenge2 <- rbind(spain, bulgaria, korea)



# Some plotting
challenge2 %>% 
  group_by(cty, Year) %>% 
  summarise(Total_Birth = sum(Births, na.rm = T),
            Total_Exposure = sum(Exposure, na.mr = T)) %>% 
  mutate(TFR = Total_Birth/Total_Exposure) %>% 
  filter(Year >= 2000) %>% 
  ggplot(aes(x=Year, y = TFR, color = cty)) + geom_line()









