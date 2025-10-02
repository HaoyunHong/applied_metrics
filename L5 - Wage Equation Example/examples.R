suppressMessages(library(tidyverse))
suppressMessages(library(fixest))

# first, the Cornwell and Rupert regression
data <- read.csv('./cornwell-rupert.csv') %>% mutate(EXP2 = EXP^2)

# see counts of each education level

data2<-data %>% mutate(ED_LEVEL=cut(ED,c(0,8,11,12,15,16,17),
                      labels = c("NOHS", "SOMEHS", "HS", "SOMECOL","COL","POST"),
                      right=TRUE))

# check that we did it correctly
table(data$ED,data2$ED_LEVEL)

reg_1 <- feols(LWAGE ~ ED + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
  + MS + UNION + FEM, data = data)

# dropping the constant
reg_2 <- feols(LWAGE ~ -1 + i(ED_LEVEL) + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
          + MS + UNION + FEM, data = data2)

# not dropping the constant -- which category is omitted?
reg_3 <- feols(LWAGE ~ 1+ i(ED_LEVEL) + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
        + MS + UNION + FEM, data = data2)

# change the omitted category -- how do coefficients change?
reg_4 <- feols(LWAGE ~ 1+ i(ED_LEVEL,ref="COL") + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
          + MS + UNION + FEM, data = data2)
etable(list(reg_1,reg_2,reg_3,reg_4), export='./table_1.png')

suppressMessages(library(car))
suppressMessages(library(sandwich))

# separate male and female categories
data3 <- data2 %>% mutate(MALE = ifelse(FEM == 1, 0, 1))
reg_5 <- feols(LWAGE ~ -1 + ED + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + UNION + FEM + MALE, data = data3)
f5<-linearHypothesis(reg_5, c("FEM = MALE"), vcoev = vcovHC(reg_5, type = "HC1"))$Chisq

# now with intercept and different (but equivalent) hypothesis test
reg_6 <- feols(LWAGE ~ 1 + ED + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS  + UNION +FEM, data = data3)
f6<-linearHypothesis(reg_6, c("FEM = 0"), vcov = vcovHC(reg_6, type = "HC1"))$Chisq

esttable(reg_5,hypothesis = "FEM-MALE= 0"))


reg_7 <- lm(LWAGE ~ ED + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + UNION + FEM, data %>% filter(LWAGE>=6))
summary(reg_7)

table(data$ED)
low_wage_data <- subset(data, LWAGE<6)
table(low_wage_data$ED)


noise <- sample(-1:1,dim(data)[1],replace=T)
data <- cbind(data, ED_NOISY=data$ED + noise)

reg_8 <- lm(LWAGE ~ ED_NOISY + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_8)


library(alr3)
deltaMethod(reg_1, "EXP + 20*EXP2")



data <- cbind(data, EDFEM=data$ED*data$FEM)
reg_9 <- lm(LWAGE ~ ED + EDFEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_9)


reg_10 <- lm(LWAGE ~ ED + ED:FEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS + FEM + UNION, data = data)
summary(reg_10)


reg_11 <- lm(LWAGE ~ ED*FEM + EXP + EXP2 + WKS + OCC + SOUTH + SMSA
+ MS  + UNION, data = data)
summary(reg_11)
