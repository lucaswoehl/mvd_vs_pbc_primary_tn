### ROBINS-I TN MVD vs PBC

## ROBINS-I TN MVD vs PBC

install.packages("robvis")

library(robvis)

robMVD <- read.csv("C:/Users/lucas/Desktop/ROBINS-I MVD.csv", header = TRUE)

rob_summary(data = robMVD,
            tool = "ROBINS-I",
            overall = T,
            weighted = T)

rob_summary

rob_traffic_light(data = robMVD,
                  tool = "ROBINS-I",
                  psize = 18)
