library(EMC2)
load("WDDM.RData")
hWDDM=fit(samplers,verbose=TRUE, iter = 2000,
             cores_per_chain = 3,fileName = "tmpWDDM.RData")
save(hWDDM,file="WDDM.RData")
