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
  filter(Age >= 15, Age <= 49) %>% 
  mutate(ASFR = Births/Exposure) %>%
  group_by(cty, Year) %>%
  summarise(GFR = sum(ASFR, na.rm = T)*(Exposure/sum(Exposure, na.rm=T))) %>% 
  filter(Year >= 2000) %>% 
  ggplot(aes(x=Year, y = TFR, color = cty)) + geom_line() +
  theme_bw()



# Create Kitagawa function
# Instead of having Mx, now I have ASFR for each age group. t1 and t2 are subsequent years for all ages
# Using:
    # Tønnessen, M. (2019). Declined total fertility rate among immigrants and the role of newly arrived women in Norway. European Journal of Population, 1-27.
    # Canudas-Romo, V. (2003). Decomposition methods in demography. Amsterdam: Rozenberg Publishers.

challenge2 <- challenge2 %>% 
                mutate(Births = replace_na(Births, 0),
                        ASFR = Births/Exposure)


# Loop Kitagawa on full dataset

# From-to (generalize):
init_y <- 2000
end_y <- 2018


# Spain
spain <- challenge2 %>% 
          filter(cty == "Spain", Year >= init_y)

spain_decom <- data.frame(matrix(NA, nrow = end_y - init_y, ncol = 4))
names(spain_decom) <- c("Year", "CC", "RC", "true_delta")

spain_decom$Year <- seq(init_y + 1, end_y, by=1)
spain_decom$cty <- "Spain"

for (i in init_y:(end_y-1)){
  # Select ASFR for first period
  ASFR1 <- spain[spain$Year == i,]$ASFR
  # Select population for first period
  Nx1 <- spain[spain$Year == i,]$Exposure
  # Do the same for period 2
  ASFR2 <- spain[spain$Year == i+1,]$ASFR
  Nx2 <- spain[spain$Year == i+1,]$Exposure
  
  spain_decom$CC[i - init_y + 1] <- sum(0.5 * (ASFR2+ASFR1) * ( (Nx2/sum(Nx2)) - (Nx1/sum(Nx1)) ) )
  spain_decom$RC[i - init_y + 1] <- sum(0.5 * ( (Nx2/sum(Nx2)) + (Nx1/sum(Nx1)) ) * (ASFR2-ASFR1) )
  spain_decom$true_delta[i - init_y + 1] <- sum(ASFR2*Nx2/sum(Nx2)) - sum(ASFR1*Nx1/sum(Nx1))
}


# CHECK!!!
spain_decom$kit_delta <- spain_decom$CC + spain_decom$RC


