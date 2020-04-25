glimpse(telecom)


length(telecom$Partner)
length(telecom$Dependents)



temp_tbl <- table(telecom$Partner, telecom$Dependents, dnn = c("Partner", "Dependents"))

temp_chi = chisq.test(temp_tbl, correct = F)

temp_chi$p.value


telecom$InternetService
telecom$OnlineSecurity
telecom$OnlineBackup
telecom$DeviceProtection
telecom$TechSupport
telecom$StreamingTV
telecom$StreamingMovies

temp_tbl1 <- table(telecom$InternetService, telecom$OnlineSecurity, dnn = c("InternetService", "OnlineSecurity"))
temp_chi1 <- chisq.test(temp_tbl1, correct = F)
temp_chi1

#a Chi-Squred test. Null hypothesis: they are independent, 
#Alternative hypothesis is that they are correlated in some way.


temp_tbl2 <- table(telecom$InternetService, telecom$OnlineSecurity, dnn = c("InternetService", "OnlineSecurity"))
temp_chi2 <- chisq.test(temp_tbl2, correct = F)
temp_chi2

c(temp_chi2$statistic, temp_chi2$p.value)

sqrt(temp_chi2$statistic / sum(temp_tbl2))
