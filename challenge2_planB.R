# Plan B: just compare 2 periods (10 years apart) for the 3 countries
# then for 2018, take Korea vs Czechia and decompose the difference between them


# Spain
  ASFR1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Spain") %>% select(ASFR)
  # Select population for first period
  Nx1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Spain") %>% select(Exposure)
  # Do the same for period 2
  ASFR2 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Spain") %>% select(ASFR)
  Nx2 <- challenge2 %>% ungroup() %>%filter(Year == 2018, cty == "Spain") %>% select(Exposure)
  
  spain_CC <- sum(0.5 * (ASFR2+ASFR1) * ( (Nx2/sum(Nx2)) - (Nx1/sum(Nx1)) ) )
  spain_RC <- sum(0.5 * ( (Nx2/sum(Nx2)) + (Nx1/sum(Nx1)) ) * (ASFR2-ASFR1) )
  spain_delta_TFR <- sum(ASFR2) - sum(ASFR1)
  spain_kitagawa <- spain_CC + spain_RC

  
# Korea
  ASFR1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Korea") %>% select(ASFR)
  # Select population for first period
  Nx1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Korea") %>% select(Exposure)
  # Do the same for period 2
  ASFR2 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Korea") %>% select(ASFR)
  Nx2 <- challenge2 %>% ungroup() %>%filter(Year == 2018, cty == "Korea") %>% select(Exposure)
  
  korea_CC <- sum(0.5 * (ASFR2+ASFR1) * ( (Nx2/sum(Nx2)) - (Nx1/sum(Nx1)) ) )
  korea_RC <- sum(0.5 * ( (Nx2/sum(Nx2)) + (Nx1/sum(Nx1)) ) * (ASFR2-ASFR1) )
  korea_delta_TFR <- sum(ASFR2) - sum(ASFR1)
  korea_kitagawa <- korea_CC + korea_RC
  

# Czechia
  ASFR1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Czechia") %>% select(ASFR)
  # Select population for first period
  Nx1 <- challenge2 %>% ungroup() %>% filter(Year == 2008, cty == "Czechia") %>% select(Exposure)
  # Do the same for period 2
  ASFR2 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Czechia") %>% select(ASFR)
  Nx2 <- challenge2 %>% ungroup() %>%filter(Year == 2018, cty == "Czechia") %>% select(Exposure)
  
  cze_CC <- sum(0.5 * (ASFR2+ASFR1) * ( (Nx2/sum(Nx2)) - (Nx1/sum(Nx1)) ) )
  cze_RC <- sum(0.5 * ( (Nx2/sum(Nx2)) + (Nx1/sum(Nx1)) ) * (ASFR2-ASFR1) )
  cze_delta_TFR <- sum(ASFR2) - sum(ASFR1)
  cze_kitagawa <- cze_CC + cze_RC
  

# Second part of challenge: 2 countries, 1 difference

  ASFR1 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Korea") %>% select(ASFR)
  # Select population for first period
  Nx1 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Korea") %>% select(Exposure)
  # Do the same for period 2
  ASFR2 <- challenge2 %>% ungroup() %>% filter(Year == 2018, cty == "Czechia") %>% select(ASFR)
  Nx2 <- challenge2 %>% ungroup() %>%filter(Year == 2018, cty == "Czechia") %>% select(Exposure)
  
  cze_vs_kor_CC <- sum(0.5 * (ASFR2+ASFR1) * ( (Nx2/sum(Nx2)) - (Nx1/sum(Nx1)) ) )
  cze_vs_kor_RC <- sum(0.5 * ( (Nx2/sum(Nx2)) + (Nx1/sum(Nx1)) ) * (ASFR2-ASFR1) )
  cze_vs_kor_delta_TFR <- sum(ASFR2) - sum(ASFR1)
  cze_vs_kor_kitagawa <- cze_vs_kor_CC + cze_vs_kor_RC 
  
  
  
  