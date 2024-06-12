i = 6
j = 2
k = 7
gegevens=read.csv(file=file.choose(),header=TRUE,dec=",",sep=";")
gegevens = gegevens[-c(k + 1, j + 1, i + 1, j*k+1, i*j+1, i * k +1, i*j*k+1, i+j+k+1), ]

variableAnalyse <- function (variable, title, nameParam){
  cat("-- ", title," --", "\n")
  cat("range: ", range(variable), "\n")
  cat("mode: ", names(sort(-table(variable)))[1], "\n")
  cat("gem: ", mean(variable), "\n")
  cat("med: ", median(variable), "\n")
  cat("IRQ: ", IQR(variable), "\n")
  cat("sd: ", sd(variable), "\n")
  
  png(paste('output/', title, '.png', sep=""), width = 1000, height = 400)
  par(mfcol=c(1,3))
  hist(variable, main=paste("Histogram van", nameParam), breaks=max(variable), xlab=paste(nameParam,"(dagen)"))
  qqnorm(variable, main=paste("Q-Q PLOT van", nameParam))
  qqline(variable)
  boxplot(variable, main=paste("Boxplot van", nameParam), ylab=paste(nameParam, "(dagen)"))
  dev.off()
  shapiro_test_result <- shapiro.test(variable)
  shapiro_test_result
}


#-----------
#LOS-varible analyse
#-----------
cat("LOS-varible analyse\n")
variableAnalyse(gegevens$los, "default_LOS_analyse", "LOS")

#Anaylse with log function
logLOS <- log(gegevens$los + 1)
variableAnalyse(logLOS, "log_LOS_analyse", "LOS")

#Anaylse with sqrt function
sqrtLOS <- sqrt(gegevens$los)
variableAnalyse(sqrtLOS, "sqrt_LOS_analyse", "LOS")

#Anaylse with sqrt function
inverseLOS <- 1 / (gegevens$los + 1)
variableAnalyse(inverseLOS, "inverse_LOS_analyse", "LOS")

#-----------
#verband is tussen het type hartinfarct en de ontslagstatus uit het ziekenhuis na opname
#-----------
cat("verband is tussen het type hartinfarct en de ontslagstatus uit het ziekenhuis na opname\n")
miTypeData <- gegevens$mitype
dstatData <- gegevens$dstat

miTypeData = sapply(miTypeData, function(x){
  if(x == 0){
    return ("geen Q-golflengtes")
  }
  else{
    return ("Q-golflengtes")
  }
})
dstatData = sapply(dstatData, function(x){
  if(x == 0){
    return ("levend")
  }
  else{
    return ("dood")
  }
})

Ctable = ftable(miTypeData,dstatData)
Ctable
ChiSq = chisq.test(Ctable)
ChiSq
ChiSq$observed
ChiSq$expected


#-----------
#Leeftijd voorspellen met BMI
#-----------
png(paste('output/', "bmi_age_pred", '.png', sep=""), width = 1000, height = 400)
par(mfcol=c(1,3))
plot(gegevens$age, gegevens$bmi, main="Leeftijd vs BMI", xlab="Leeftijd", ylab="BMI")
correlation_result <- cor(gegevens$age, gegevens$bmi)
correlation_result

# Fit lineair regressiemodel
model <- lm(gegevens$bmi ~ gegevens$age)
model_summary <- summary(model)
model_summary

# Plot residuals
plot(model$residuals, main="Residuals", ylab="Residuals")

# Q-Q plot van residuals
qqnorm(model$residuals)
qqline(model$residuals)
dev.off()

