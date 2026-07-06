#  install.packages("redcapAPI")

library(redcapAPI)

unlockREDCap(c(rcon= 'rage-redcap'),  
             keyring= "rage-redcap",
             envir= globalenv(),
             url= 'https://cvr-redcap.mvls.gla.ac.uk/redcap/redcap_v15.5.18/API/')
