# Universidade Federal de Ouro Preto
# Federal University of Ouro Preto

# Instituto de Ciências Sociais Aplicadas
# Institute for Applied Social Sciences






# Econometria Cross-Section: Uma análise Cross-Country dos indicadores
# de investimento do WDI.



# Researcher: Bruno Rodrigues Candea




# OBS.: This code is for educational purposes only. Errors and mistakes
# of theory and/or code may occurs. I'm a student yet :D 
# Thanks.




# OBS.: All lines of codes used for tests, plots ans summaries, except the
# most crucial, are deactivated with # before its functions. To run any of
# this plots and summaries, please just delete the # and run the line.





# INTRODUCTION TO CODE





	# Installing the packages for analysis



		# OBS.: Please remove the # before the commands if you don't 
		# have one, some or any of the packages installed yet.


		#install.packages("car")
		#install.packages("AER")
		#install.packages("RDCOMClient")
		#install.packages("tseries")
		#install.packages("corrgram")



	# Requiring (if installed) the packages for analysis



		# OBS.: Please remove the # before the commands if you don't
		# have required one, some or any of the packages yet.


		require(car)
		require(AER)
		require(tseries)
		require(corrgram)



	# Auxiliar function to clear the console and organize our view:



		cls <- function() { 
		require(RDCOMClient) 
		wsh <- COMCreate("Wscript.Shell") 
		wsh$SendKeys("\014") 
		invisible(wsh) 
		} 
		
		# OBS.: Everytime you want to clear the console, please 
		# tap the function cls() here or in console.





# SECTION 1 - Individual linear models for innitial tests





	# Loading the main database



		data2 <- read.csv("data2.csv", header = T, sep = ";")
		#fix(data2)



	# Creating the regressand Y and regressors Xi



		# Gross Domestic Product (Regressand)


		Y <- data2[3]
		gdp <- data.matrix(Y)


		# Education Expenditures


		X1 <- data2[4]
		edu <- data.matrix(X1)


		# Government Expenditures (all areas, except Education)


		X2 <- data2[6]
		gov <- data.matrix(X2)


		# Credit to Private Sector


		X3 <- data2[7]
		cdt <- data.matrix(X3)


		# Investments in Telecom with Participation of Private Sector


		X4 <- data2[8] 
		tel <- data.matrix(X4)


      	# Investments in Energy with Participation of Private Sector


		X5 <- data2[9] 
		eng <- data.matrix(X5)


      	# Investments in Transport with Participation of Private Sector


		X6 <- data2[10]
		tra <- data.matrix(X6)


      	# Investments in Water and Sanitation with Participation of Private Sector


		X7 <- data2[11]
		was <- data.matrix(X7)


		# Number of Internet Users per 100 people


		X8 <- data2[12]
		int <- data.matrix(X8)


		# OECD Member Dummy
	

		X9 <- data2[13]
		omd <- data.matrix(X9)



	# First linear regressions models for test and innitial knowledge



		# Role of Government Expenditures in Education on GDP


		mod1 <- lm(gdp ~ edu, data=data2)
		#summary(mod1)
		#plot(gdp ~ edu,data=data2)


		# Role of Government Expenditures (all areas) on GDP


		mod2 <- lm(gdp ~ gov, data=data2)
		#summary(mod2)
		#plot(gdp ~ gov,data=data2)


      # Relevance of Credit provided to Private Sector on GDP


		mod3 <- lm(gdp ~ cdt, data=data2)
		#summary(mod3)
		#plot(gdp ~ cdt,data=data2)


      # Relevance of Telecom Investments with participation of Private Sector on GDP


		mod4 <- lm(gdp ~ tel, data=data2)
		#summary(mod4)
		#plot(gdp ~ tel,data=data2)


      # Relevance of Energy Investments with participation of Private Sector on GDP


		mod5 <- lm(gdp ~ eng, data=data2)
		#summary(mod5)
		#plot(gdp ~ eng,data=data2)


      # Relevance of Transport Investments with participation of Private Sector on GDP


		mod6 <- lm(gdp ~ tra, data=data2)
		#summary(mod6)
		#plot(gdp ~ tra,data=data2)


      # Relevance of Water and Sanitation Investments with participation of Private Sector on GDP


		mod7 <- lm(gdp ~ was, data=data2)
		#summary(mod7)
		#plot(gdp ~ was,data=data2)


      # Relevance of Internet Users per 100 people on GDP

		mod8 <- lm(gdp ~ int, data=data2)
		#summary(mod8)
		#plot(gdp ~ int,data=data2)


      # Relevance of being an OECD Member


		mod9 <- lm(gdp ~ omd, data=data2)
		#summary(mod9)
		#plot(gdp ~ omd,data=data2)





# SECTION 2 - Discovering the best fitted model





      # Linear Model for testing what are the best parameters



		test1 <- lm(gdp ~ edu + gov + cdt + tel + eng + tra + was + int + omd, data=data2)
		#summary(test1)
	   

		test2 <- lm(gdp ~ edu + gov + cdt + tel + eng, data=data2)
		#summary(test2)


		test3 <- lm(gdp ~ edu + tel + eng, data=data2)
		#summary(test3)


		test4 <- lm(gdp ~ edu + tel + tra, data=data2)
		#summary(test4)


		reg <- lm(gdp ~ edu + tel + tra, data=data2)
		summary(reg)



	# Tests with a model that uses per capita data:



		data4 <- read.csv("data4.csv", header=T, sep=";")



		gdp4 <- data4[4]
		gdp4 <- data.matrix(gdp4)

		edu4 <- data4[5]
		edu4 <- data.matrix(edu4)

		cdt4 <- data4[6]
		cdt4 <- data.matrix(edu4)

		tel4 <- data4[7]
		tel4 <- data.matrix(tel4)

		eng4 <- data4[8]
		eng4 <- data.matrix(eng4)


		#fix(data4)


		reg1 <- lm(gdp4 ~ edu4 + cdt4 + tel4 + eng4, data=data4)
		#summary(reg1)


		reg2 <- lm(gdp4 ~ edu4 + cdt4 + tel4, data=data4)
		#summary(reg2) 


		reg3 <- lm(gdp4 ~ edu4 + tel4 + eng4, data=data4)
		#summary(reg3)  


		reg4 <- lm(gdp4 ~ edu4 + tel4, data=data4)	   
		#summary(reg4)



	# The best model to use is:



		model <- lm(gdp4 ~ edu4 + tel4 + eng4, data=data4)
		#summary(model)



	# Until now, we performed many tests. Let's clear the console to use what we need only:
		


		cls()



	# Comparing the model with current values with per capita model:



		#summary(reg);summary(model)

		# Notice that the second model (per capita) is the best fitted.



	# Getting the table with coefficients to show in paper:



		#summary(model)$coefficients
 


	# Showing the exact value of the coefficients of this regression:
		


		#summary(model)$coefficients[1]
		#summary(model)$coefficients[2]
		#summary(model)$coefficients[3]
		#summary(model)$coefficients[4]



	# Variance-Covariance Matrices



		#vcov(model)





# SECTION 3 - Tests





	# 3.1 - NORMALITY OF DATA



		# 3.1.1 - Shapiro-Wilk


			shapiro.test(gdp4)
			shapiro.test(edu4)
			shapiro.test(tel4)
			shapiro.test(eng4)


		# 3.1.2 - Jarque-Bera 


			jarque.bera.test(gdp4)
			jarque.bera.test(edu4)
			jarque.bera.test(tel4)
			jarque.bera.test(eng4)



	# 3.2 - OUTLIERS



		# STEP 1 - Testing the presence of outliers



			# 3.2.1 - Bonferonni p Test


				outmodel <- outlierTest(model)
				#outmodel


			# 3.2.2 - Hat Values Test


				hatmodel <- hatvalues(model)
				hatmodel <- data.matrix(hatmodel)		
				#fix(hatmodel)



		# STEP 2 - Plotting studentized residuals



				#qqPlot(model, main="QQ Plot")


				#plot(hatmodel)
				#abline(h=c(1,3)*mean(hatmodel),col=2) # linhas iguais à media e três vezes a média
				#id <- which(hatmodel>3*mean(hatmodel))    # Pontos que excedem três vezes a média da variável predita
				#text(id,hatmodel[id],rownames(data4)[id],pos=1,xpd=TRUE)



		# STEP 3 - Leverage Plots



				#leveragePlots(model)



		# STEP 4 - Influential Observations



			# Cook's D plot (identify D values > 4/(n-k-1))


				cutoff <- 4/((nrow(model)-length(model$coefficients)-1)) 
				#plot(model, which=4, cook.levels=cutoff)


			# Measuring the Influence:


				influence.mod <- influence.measures(model)
				#influence.mod


				influence.hat <- which(hatmodel>3*mean(hatmodel))
				#influence.hat



		# STEP 5 - Comparing a model with and without the influential info:

	   

				#influencePlot(model,id.method="identify", main="Influence Plot", sub="Área do círculo proporcional à Distância de Cook" )


		
			# Identifier of the observations that overpass aceptable results:


				inf <- which(apply(influence.measures(model)$is.inf,1,any)) 
			

			# Creating a new data without influencial observations:


				data5 <- data4[-inf,]
				#fix(data5)


				gdp5 <- data5[4]
				gdp5 <- data.matrix(gdp5)


				edu5 <- data5[5]
				edu5 <- data.matrix(edu5)


				cdt5 <- data5[6]
				cdt5 <- data.matrix(edu5)


				tel5 <- data5[7]
				tel5 <- data.matrix(tel5)


				eng5 <- data5[8]
				eng5 <- data.matrix(eng5)

			
			# Comparing the size of data with and without influential obs:


				#length(gdp4)
				#length(gdp5)

			
			# Creating a model without influential obs:


				model_noinf <- lm(gdp5 ~ edu5 + tel5 + eng5, data=data5)


			# One variable is no more significant. Let's remove it and
			# create another model without it:


				model2 <- lm(gdp5 ~ edu5 + tel5, data=data5)


			#Summaries of old and new models:

				#summary(model); summary(model_noinf)
				#summary(model); summary(model2)
				#summary(model); summary(model_noinf); summary(model2)



	# 3.3 - HETEROSKEDASTICITY


			
		# INTRO - Creating a log model:


				logmodel <- lm(log(gdp5) ~ log(edu5) + log(tel5), data=data5)
				#summary(logmodel)



		# STEP 1 - Inspecting the residuals and comparing models (Lin x Log):



			# Linear model:


				error    <- model2$residuals
				error_sq <- (model2$residuals)^2
				y_hat    <- model2$fitted.values


			# Log model:


				logerror    <- logmodel$residuals
				logerror_sq <- (logmodel$residuals)^2
				logy_hat    <- logmodel$fitted.values



		# STEP 2 - Graphically inspecting the residuals:



			# Linear model:


				#plot(y_hat,error)
				#plot(y_hat,error_sq)


			# Log model:

			
				#plot(logy_hat,logerror)
				#plot(logy_hat,logerror_sq)



		# STEP 3 - Formally testing the presence of heteroskedasticity:



			# Breusch-Pagan test



				# Linear model:


					#bptest(model2)


				# Log model:


					#bptest(logmodel)


			# Goldfeld-Quandt test


				# Linear model:


					#gqtest(model2,order.by=~model2$fitted.values,data=data5, alternative=c("less"))
			

				# Log model:


					#gqtest(logmodel,order.by=~logmodel$fitted.values,data=data5, alternative=c("less"))


			# Testing if the fitted model has removed some observation


				# Linear model:


					#length(model2$fitted.values)


				# Log model:


					#length(logmodel$fitted.values)


			# Fractionized Goldfeld-Quandt:


				# Linear model:


					#gqtest(model2,order.by=~model2$fitted.values,data=data5, fraction=(1/3), alternative=c("less"))
			

				# Log model:


					#gqtest(logmodel,order.by=~logmodel$fitted.values,data=data5, fraction=(1/3), alternative=c("less"))



		# STEP 4 - White's matrices



				# Linear model:


					white <- coeftest(model2, vcov = vcovHC(model2 , type = "HC0"))
				

				# Log model:


					logwhite <- coeftest(logmodel, vcov = vcovHC(logmodel , type = "HC0"))


				# White's matrice for linear model:


					#white


				# White's matrice for log model:


					#logwhite



		# STEP 5 - Estimating MQG



			# 5.1 - Creating the weights:


				# Linear model weights:


					wi <- 1/(residuals(model2)^2)


				# Log model weights:


					logwi <- 1/(residuals(logmodel)^2) 


			# 5.2 - Re-estimating the models:


				# Linear model:


					model.lin <- lm(gdp5 ~ edu5 + tel5, data=data5, weights=wi)
					#summary(model.lin)

					error2    <- model.lin$residuals
					error2_sq <- (model.lin$residuals)^2
					y_hat2    <- model.lin$fitted.values

					#plot(y_hat2,error2)
					#plot(y_hat2,error2_sq)


				# Log model:


					model.log <- lm(log(gdp5) ~ log(edu5) + log(tel5), data=data5, weights=logwi)
					#summary(model.log)

					logerror2    <- model.log$residuals
					logerror2_sq <- (model.log$residuals)^2
					logy_hat2    <- model.log$fitted.values

					#plot(logy_hat2,logerror2)
					#plot(logy_hat2,logerror2_sq)



		# STEP 6 - Creating dummies:



					tab.lin <- tabulate(data5$GRO)
					summary(tab.lin)
					length(tab.lin)


					data5$GRO <- data5$GRO
					group <- data5$GRO


				# Linear model:


					lin.model <- lm(gdp5 ~ edu5 + tel5 + group, data = data5)
					#summary(lin.model)

					error3    <- lin.model$residuals
					error3_sq <- (lin.model$residuals)^2
					y_hat3    <- lin.model$fitted.values

					#plot(y_hat3,error3)
					#plot(y_hat3,error3_sq)


				# Log model:


					log.model <- lm(log(gdp5) ~ log(edu5) + log(tel5) + group, data=data5)
					summary(log.model)

					logerror3    <- log.model$residuals
					logerror3_sq <- (log.model$residuals)^2
					logy_hat3    <- log.model$fitted.values

					#plot(logy_hat3,logerror3)
					#plot(logy_hat3,logerror3_sq)



	# 3.4 - MULTICOLINEARITY


		# Correlation between variables:


				#corrgram(data5, order=NULL, lower.panel=panel.shade, upper.panel=NULL)

				varcov <- vcov(lin.model)
				#fix(varcov)



		# VIF (Variance Inflator Factor)


			# Linear model:


				#vif(lin.model)


			# Log model:


				#vif(log.model)



	# 3.5 - CORRELATION (NOT SERIAL)
	


		# Correlation between the error term and variables


			# Linear model:


				#cor(lin.model$residuals, gdp5)
				#cor(lin.model$residuals, edu5)
				#cor(lin.model$residuals, tel5)


			# Log model:


				#cor(log.model$residuals, gdp5)
				#cor(log.model$residuals, edu5)
				#cor(log.model$residuals, tel5)





# SECTION 4 - GENERALIZED LEAST SQUARES MODEL





	# Linear model:



		# Weight: pondered by the inverse of the telecom investments squared


			lin.model.w1 <- lm(gdp5 ~ edu5 + tel5 + group, data = data5, weights=1/tel5^2)
			#summary(lin.model.w1)

			error.lin1    <- lin.model.w1$residuals
			error.lin1.sq <- lin.model.w1$residuals^2
			y_hat.lin1    <- lin.model.w1$fitted.values
			#plot(y_hat.lin1,error.lin1)
			#plot(y_hat.lin1,error.lin1.sq)


		# Weight: pondered by the inverse of the telecom investments only


			lin.model.w2 <-lm(gdp5 ~ edu5 + tel5 + group, data = data5, weights=1/tel5)
			#summary(lin.model.w2)

			error.lin2    <- lin.model.w2$residuals
			error.lin2.sq <- lin.model.w2$residuals^2
			y_hat.lin2    <- lin.model.w2$fitted.values
			#plot(y_hat.lin2,error.lin2)
			#plot(y_hat.lin2,error.lin2.sq)


		# Comparing the models residuals


			#par(mfrow=c(1,3))
			#plot(y_hat.lin1,error.lin1, main="PLS 1: Squared Weight")
			#plot(y_hat.lin2,error.lin2,col=2, main="PLS 2: Non-Squared Weight")
			#plot(y_hat,error, col=3, main="OLS")


		# Comparing the models squared residuals


			#par(mfrow=c(1,3))
			#plot(y_hat.lin1,error.lin1.sq, main="PLS 1: Squared Weight")
			#plot(y_hat.lin2,error.lin2.sq,col=3, main="PLS 2: Non-Squared Weight")
			#plot(y_hat,error_sq, col=3, main="OLS")



	# Log model:



		# Weight: pondered by the inverse of the telecom investments squared


			log.model.w1 <- lm(log(gdp5) ~ log(edu5) + log(tel5) + group, data = data5, weights=1/tel5^2)
			#summary(log.model.w1)

			error.log1    <- log.model.w1$residuals
			error.log1.sq <- log.model.w1$residuals^2
			y_hat.log1    <- log.model.w1$fitted.values
			#plot(y_hat.log1,error.log1)
			#plot(y_hat.log1,error.log1.sq)


		# Weight: pondered by the inverse of the telecom investments only


			log.model.w2 <-lm(log(gdp5) ~ log(edu5) + log(tel5) + group, data = data5, weights=1/tel5)
			#summary(log.model.w2)

			error.log2    <- log.model.w2$residuals
			error.log2.sq <- log.model.w2$residuals^2
			y_hat.log2    <- log.model.w2$fitted.values
			#plot(y_hat.log2,error.log2)
			#plot(y_hat.log2,error.log2.sq)


		# Comparing the models residuals


			#par(mfrow=c(1,3))
			#plot(y_hat.log1,error.log1, main="PLS 1: Squared Weight")
			#plot(y_hat.log2,error.lin2,col=2, main="PLS 2: Non-Squared Weight")
			#plot(logy_hat,logerror, col=3, main="OLS")


		# Comparing the models squared residuals


			#par(mfrow=c(1,3))
			#plot(y_hat.log1,error.log1.sq, main="PLS 1: Squared Weight")
			#plot(y_hat.log2,error.lin2.sq,col=2, main="PLS 2: Non-Squared Weight")
			#plot(logy_hat,logerror_sq, col=3, main="OLS")


		# Comparing the models


			# Linear model:


				#summary(model2)
				model2$coefficient

				#summary(lin.model)
				lin.model$coefficient

				#summary(lin.model.w1)
				lin.model.w1$coefficient
 
				#summary(lin.model.w2)
				lin.model.w2$coefficient


			# Log model:


				#summary(log.model)
				#log.model$coefficient

				#summary(log.model.w1)
				#log.model.w1$coefficient

				#summary(log.model.w2)
				#log.model.w2$coefficient




# SECTION 5 - FEASIBLE GENERALIZED LEAST SQUARES





		# Linear model



			# Auxiliar regression of the residuals ~ model

				lin.auxiliar <- lm(residuals(lin.model)^2 ~ edu5 + tel5 + group, data=data5) 
				#summary(lin.auxiliar)


			# FGLS Models weighted by the inverse of exponential of the fitted model


				lin.model.fgls <-lm(gdp5 ~ edu5 + tel5 + group, weights=1/exp(fitted(lin.auxiliar)),data=data5)
				#summary(lin.model.fgls)


				error_fgls1  <- lin.model.fgls$residuals
				y_hat_fgls1 <- lin.model.fgls$fitted.values
				#plot(y_hat_fgls1,error_fgls1)



		# Log model



			# Auxiliar regression of the residuals ~ model

				log.auxiliar <- lm(log(residuals(log.model)^2) ~ log(edu5) + log(tel5) + group, data=data5) 
				#summary(log.auxiliar)


			# FGLS Models weighted by the inverse of exponential of the fitted model


				log.model.fgls <-lm(log(gdp5)~ log(edu5) + log(tel5) + group, weights=1/exp(fitted(log.auxiliar)),data=data5)
				#summary(log.model.fgls)


				error_fgls2  <- log.model.fgls$residuals
				y_hat_fgls2 <- log.model.fgls$fitted.values
				#plot(y_hat_fgls2,error_fgls2)





# SECTION 6 - FINAL MODEL





	# Linear model



		# Residuals


			#par(mfrow=c(1,4))
			#plot(y_hat,error, main="Model2 (OLS)")
			#plot(y_hat3,error3, col=2, main="Linear Model")
			#plot(y_hat.lin1,error.lin1,col=3, main="Squared Weight")
			#plot(y_hat.lin2,error.lin2,col=4, main="Non-Squared Weight")


		# Squared Residuals


			#par(mfrow=c(1,4))
			#plot(y_hat,error_sq, main="Model2 (OLS)")
			#plot(y_hat3,error3_sq, col=2, main="Linear Model")
			#plot(y_hat.lin1,error.lin1.sq,col=3, main="Squared Weight")
			#plot(y_hat.lin2,error.lin2.sq,col=4, main="Non-Squared Weight")


		# Models

		
			#summary(model2);summary(lin.model);summary(lin.model.w1);summary(lin.model.w2)



	# Log model



		# Residuals


			#par(mfrow=c(1,4))
			#plot(logy_hat,logerror, main="Model2 (Log OLS)")
			#plot(logy_hat3,logerror3, col=2, main="Log Model")
			#plot(y_hat.log1,error.lin1,col=3, main="Log with Squared Weight")
			#plot(y_hat.log2,error.lin2,col=4, main="Log with Non-Squared Weight")


		# Squared Residuals


			#par(mfrow=c(1,5))
			#plot(logy_hat,logerror_sq, main="Model2 (Log OLS)")
			#plot(logy_hat3,logerror3_sq, col=2, main="Log Model")
			#plot(y_hat.log1,error.lin1.sq,col=3, main="Log with Squared Weight")
			#plot(y_hat.log2,error.lin2.sq,col=4, main="Log with Non-Squared Weight")
			#plot(y_hat_fgls1,error_fgls1.sq,col=5, main="Feasible GLS Log")


		# Models


			#summary(logmodel);summary(log.model);summary(log.model.w1);summary(log.model.w2);summary(log.model.fgls)

 



		# Clear all the console to display only the decision:
		cls()





# SECTION 7 - DECISIONS





		# DECISIONS: Use the linear model estimed by GLS with Squared Weights
			

			summary(lin.model.w1)
			lin.model.w1$coefficient


		# DECISIONS: Use the log model estimed by GLS with Squared Weights


			summary(log.model.w1)
			log.model.w1$coefficient


