# server.R

library("xgboost")
library("caret")
library("ROCR")
library("ggplot2")
library("corrplot")
library("e1071")
library("MASS")
library("plotly")


source("helpers.R")


# Importing data
d<-read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Data cleaning
# change categorical to numerical
colnames(d)[1] = "Age"
d$Attrition= as.integer(as.factor(d$Attrition))-1
d$BusinessTravel= as.integer(as.factor(d$BusinessTravel))
d$Department= as.integer(as.factor(d$Department))
d$Gender= as.integer(as.factor(d$Gender))
d$JobRole= as.integer(as.factor(d$JobRole))
d$MaritalStatus= as.integer(as.factor(d$MaritalStatus))
d$OverTime= as.integer(as.factor(d$OverTime))
d$EducationField= as.integer(as.factor(d$EducationField))
d$StandardHours<-NULL
d$PerformanceRating<-NULL
d$Over18<-NULL
d$EmployeeCount<-NULL
d$JobLevel<-NULL
d$DailyRate<-NULL
d$HourlyRate<-NULL
d$DailyRate<-NULL
d$MonthlyRate<-NULL
d$PercentSalaryHike<-NULL

# Classification
# Creat data for training and test
set.seed(0)
tr.number<-sample(nrow(d),nrow(d)*2/3) 
train<-d[tr.number,]
test<-d[-tr.number,]

column_names = names(test)

# split dataset
train_Y = train$Attrition
train$Attrition<-NULL
test_Y = test$Attrition
test$Attrition<-NULL


param <- list(max_depth=3, 
              silent=1, 
              eta = 0.3,
              objective='binary:logistic',
              eval_metric = 'auc')
train[] <- lapply(train, as.numeric)
test[] <- lapply(test, as.numeric)

# XGBoost
dtrain <- xgb.DMatrix(as.matrix(train), label = train_Y)
bst <- xgb.train(param, nrounds = 82, dtrain)
pred.xgb <- predict(bst, as.matrix(test))

importance <- xgb.importance(feature_names = column_names, model = bst)

# SVM
train$Attrition<-train_Y
svm_model<-svm(Attrition~.,
  				type="C-classification",
  				gamma=0.001668101,
  				cost=35.93814,
  				data=train,
  				cross=3,
  				probability = TRUE
)
svm_model.predict<-predict(svm_model, test, probability=TRUE)
svm_model.prob <-attr(svm_model.predict,"probabilities")

#Logistic Regression
LR_model <- glm(Attrition ~.,family=binomial(link='logit'),data=train)
LR_model.predict <- predict(LR_model, test, type = "response")



test$Attrition <- test_Y
test$prediction_xgb <- pred.xgb
test$prediction_svm <- svm_model.prob[,2]
test$prediction_lr <- LR_model.predict

train$Attrition <- train_Y

zoom_out = 1.4
camera = list(eye=list(x=zoom_out, y=zoom_out, z=zoom_out))

nbins = list(Age = 20,
             BusinessTravel = 3,
             Department = 3, 
             DistanceFromHome = 10, 
             Education = 5, 
             EducationField = 6, 
             EnvironmentSatisfaction = 4, 
             Gender = 2, 
             JobInvolvement = 4, 
             JobRole = 9, 
             JobSatisfaction = 4, 
             MaritalStatus = 3, 
             MonthlyIncome = 30, 
             NumCompaniesWorked = 10, 
             OverTime = 2, 
             RelationshipSatisfaction = 4, 
             StockOptionLevel = 4, 
             TotalWorkingYears = 30, 
             TrainingTimesLastYear = 7, 
             WorkLifeBalance = 4, 
             YearsAtCompany = 20, 
             YearsInCurrentRole = 15,
             YearsSinceLastPromotion = 15,
             YearsWithCurrManager = 10)

shinyServer(function(input, output) {
	
	thresh <- reactive({input$thresh})
	thresh_precision <- reactive({input$thresh_precision})
	thresh_recall <- reactive({input$thresh_recall})
	

	ageRange <- reactive({switch(input$Age, 
               		"< 35" = c(0, 35),
               		"35-45" = c(35, 46),
               		"> 45" = c(46, 1000),
               		"all" = c(0, 1000)           		
               		) })
 	gender <- reactive({switch(input$Gender, 
               		"Male" = 2,
               		"Female" = 1,
               		"all" = 0             		
               		) })
  	MonthlyIncome <- reactive({switch(input$MonthlyIncome, 
               		"< 2500" = c(0, 2500),
               		"2500-5000" = c(2500, 5001),
               		"5001-7500" = c(5001, 7501),
               		"7501-10000" = c(7501, 10001),
               		"> 10001" = c(10001, 1000000),
               		"all" = c(0, 1000000)                		
               		) })
     MaritalStatus <- reactive({switch(input$MaritalStatus, 
               		"Divorced" = 1,
               		"Married" = 2,
               		"Single" = 3,
               		"all" = 0             		
               		) })
    
    
               		
    good_columns <- reactive({get_fixed_columns_names(column_names, c(input$Age, input$Gender, input$Education, input$MonthlyIncome, input$MaritalStatus), 
    																c("Age", "Gender", "Education", "MonthlyIncome", "MaritalStatus"))})
    																  
	test_sub <- reactive({subset(test, (Age >= ageRange()[1] & Age < ageRange()[2]) &
									   (Gender == gender() | gender() == 0) & 
									   (Education == input$Education | input$Education == "all") &
									   (MonthlyIncome >= MonthlyIncome()[1] & MonthlyIncome < MonthlyIncome()[2]) &
									   (MaritalStatus == MaritalStatus() | MaritalStatus() == 0)
									   )
									   })
	train_sub <- reactive({subset(train, (Age >= ageRange()[1] & Age < ageRange()[2]) &
									   (Gender == gender() | gender() == 0) & 
									   (Education == input$Education | input$Education == "all") &
									   (MonthlyIncome >= MonthlyIncome()[1] & MonthlyIncome < MonthlyIncome()[2]) &
									   (MaritalStatus == MaritalStatus() | MaritalStatus() == 0)
									   )})
    ROCRpred_xgb <- reactive({ prediction(test_sub()$prediction_xgb, test_sub()$Attrition) })
    ROCRpred_svm <- reactive({ prediction(test_sub()$prediction_svm, test_sub()$Attrition) })
    ROCRpred_lr <- reactive({ prediction(test_sub()$prediction_lr, test_sub()$Attrition) })

    m <- reactive({
    	train_sub <- train_sub()[good_columns()]
    	cor(train_sub) 
    })
	output$corr_plot <- renderPlot({
		par(mar=c(0., 0, 0, 0))
		corrplot(m(), method="circle", tl.col="#3982B7")		
	}, width =510, height = 510)

###### ROC plots ############
	output$plot <- renderPlot({par(mar=c(10., 2, .5, 1))
					#XGBoost
					perf_xgb <- performance(ROCRpred_xgb(), 'tpr','fpr')					
					roc_xgb.data <- data.frame(fpr=unlist(perf_xgb@x.values), tpr=unlist(perf_xgb@y.values), model="XGBoost")
					
					#SVM
					perf_svm <- performance(ROCRpred_svm(), 'tpr','fpr')					
					roc_svm.data <- data.frame(fpr=unlist(perf_svm@x.values), tpr=unlist(perf_svm@y.values), model="SVM")
					
					#Logistic Regression
					perf_lr <- performance(ROCRpred_lr(), 'tpr','fpr')					
					roc_lr.data <- data.frame(fpr=unlist(perf_lr@x.values), tpr=unlist(perf_lr@y.values), model="LR")
					cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
					ggplot() +
					geom_line(data = roc_xgb.data, aes(x=fpr, y=tpr, colour = "XGBoost")) + 
					geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "SVM")) + 
					geom_line(data = roc_lr.data, aes(x = fpr, y=tpr, colour = "Logistic Regression")) + 
					scale_colour_manual(name = "Models", values = cols) + 
					xlab("False Positive Rate") +
					ylab("True Positive Rate") +
					geom_vline(xintercept = thresh(), color = "red", linetype=2) + theme_bw() +
					theme(legend.position = c(0.8, 0.2), 
							legend.text = element_text(size = 15), 
							legend.title = element_text(size = 15))

	 	})#, width = 650, height = 400)

	output$confusionMatrix <- renderPlot({
		auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")
		perf_xgb <- performance(ROCRpred_xgb(), 'tpr','fpr')					
		cut <- get_cutoff_point(perf_xgb, thresh())
		pred_values_xgb <- ifelse(test_sub()$prediction_xgb > cut,1,0)
		cm_xgb <- confusionMatrix(data = pred_values_xgb, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_xgb, auc_xgb@y.values, "#3DB7E4")
		
		})

	output$confusionMatrix_svm <- renderPlot({
		auc_svm <- performance(ROCRpred_svm(), measure = "auc")
		perf_svm <- performance(ROCRpred_svm(), 'tpr','fpr')					
		cut <- get_cutoff_point(perf_svm, thresh())
		pred_values_svm <- ifelse(test_sub()$prediction_svm > cut,1,0)
		cm_svm <- confusionMatrix(data = pred_values_svm, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_svm, auc_svm@y.values, "#FF8849")				
		})
	output$confusionMatrix_lr <- renderPlot({
		auc_lr <- performance(ROCRpred_lr(), measure = "auc")
		perf_lr <- performance(ROCRpred_lr(), 'tpr','fpr')					
		cut <- get_cutoff_point(perf_lr, thresh())
		pred_values_lr <- ifelse(test_sub()$prediction_lr > cut,1,0)
		cm_lr <- confusionMatrix(data = pred_values_lr, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_lr, auc_lr@y.values, "#69BE28")		
		
		})

###### precision plots ############
	output$plot_precision <- renderPlot({par(mar=c(10., 2, .5, 1))
					#XGBoost
					perf_xgb <- performance(ROCRpred_xgb(),'prec', 'cutoff')					
					xgb.data <- data.frame(x=unlist(perf_xgb@x.values), y=unlist(perf_xgb@y.values), model="XGBoost")
					
					#SVM
					perf_svm <- performance(ROCRpred_svm(),'prec', 'cutoff')					
					svm.data <- data.frame(x=unlist(perf_svm@x.values), y=unlist(perf_svm@y.values), model="SVM")
					
					#Logistic Regression
					perf_lr <- performance(ROCRpred_lr(),'prec', 'cutoff')					
					lr.data <- data.frame(x=unlist(perf_lr@x.values), y=unlist(perf_lr@y.values), model="LR")
					cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
					ggplot() +
					geom_line(data = xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
					geom_line(data = svm.data, aes(x =x, y=y, colour = "SVM")) + 
					geom_line(data = lr.data, aes(x =x, y=y, colour = "Logistic Regression")) + 
					scale_colour_manual(name = "Models", values = cols) + 
					xlab("Cutoff") +
					ylab("Precision") +
					geom_vline(xintercept = thresh_precision(), color = "red", linetype=2) + theme_bw() +
					theme(legend.position = c(0.8, 0.2), 
							legend.text = element_text(size = 15), 
							legend.title = element_text(size = 15))

	 	})#, width = 650, height = 400)

	output$confusionMatrix_precision <- renderPlot({
		auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")
		pred_values_xgb <- ifelse(test_sub()$prediction_xgb > thresh_precision(),1,0)
		cm_xgb <- confusionMatrix(data = pred_values_xgb, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_xgb, auc_xgb@y.values, "#3DB7E4")		
		})

	output$confusionMatrix_svm_precision <- renderPlot({
		auc_svm <- performance(ROCRpred_svm(), measure = "auc")
		pred_values_svm <- ifelse(test_sub()$prediction_svm > thresh_precision(),1,0)
		cm_svm <- confusionMatrix(data = pred_values_svm, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_svm, auc_svm@y.values, "#FF8849")				
		})
	output$confusionMatrix_lr_precision <- renderPlot({
		auc_lr <- performance(ROCRpred_lr(), measure = "auc")
		pred_values_lr <- ifelse(test_sub()$prediction_lr > thresh_precision(),1,0)
		cm_lr <- confusionMatrix(data = pred_values_lr, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_lr, auc_lr@y.values, "#69BE28")		
		})

###### recall plots ############
	output$plot_recall <- renderPlot({par(mar=c(10., 2, .5, 1))
					#XGBoost
					perf_xgb <- performance(ROCRpred_xgb(),'rec', 'cutoff')					
					xgb.data <- data.frame(x=unlist(perf_xgb@x.values), y=unlist(perf_xgb@y.values), model="XGBoost")
					
					#SVM
					perf_svm <- performance(ROCRpred_svm(),'rec', 'cutoff')					
					svm.data <- data.frame(x=unlist(perf_svm@x.values), y=unlist(perf_svm@y.values), model="SVM")
					
					#Logistic Regression
					perf_lr <- performance(ROCRpred_lr(),'rec', 'cutoff')					
					lr.data <- data.frame(x=unlist(perf_lr@x.values), y=unlist(perf_lr@y.values), model="LR")
					cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
					ggplot() +
					geom_line(data = xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
					geom_line(data = svm.data, aes(x =x, y=y, colour = "SVM")) + 
					geom_line(data = lr.data, aes(x =x, y=y, colour = "Logistic Regression")) + 
					scale_colour_manual(name = "Models", values = cols) + 
					xlab("Cutoff") +
					ylab("Recall") +
					geom_vline(xintercept = thresh_recall(), color = "red", linetype=2) + theme_bw() +
					theme(legend.position = c(0.8, 0.8), 
							legend.text = element_text(size = 15), 
							legend.title = element_text(size = 15))

	 	})#, width = 650, height = 400)

	output$confusionMatrix_recall <- renderPlot({
		auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")
		pred_values_xgb <- ifelse(test_sub()$prediction_xgb > thresh_recall(),1,0)
		cm_xgb <- confusionMatrix(data = pred_values_xgb, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_xgb, auc_xgb@y.values, "#3DB7E4")		
		})

	output$confusionMatrix_svm_recall <- renderPlot({
		auc_svm <- performance(ROCRpred_svm(), measure = "auc")
		pred_values_svm <- ifelse(test_sub()$prediction_svm > thresh_recall(),1,0)
		cm_svm <- confusionMatrix(data = pred_values_svm, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_svm, auc_svm@y.values, "#FF8849")				
		})
	output$confusionMatrix_lr_recall <- renderPlot({
		auc_lr <- performance(ROCRpred_lr(), measure = "auc")
		pred_values_lr <- ifelse(test_sub()$prediction_lr > thresh_recall(),1,0)
		cm_lr <- confusionMatrix(data = pred_values_lr, reference = test_sub()$Attrition)
		draw_confusion_matrix(cm_lr, auc_lr@y.values, "#69BE28")		
		})


	output$pred_xgb <- renderPlot({ 
		ggplot(test_sub(), aes(prediction_xgb, fill = factor(Attrition))) + geom_density(alpha = 0.2) + theme_bw() + labs(x = "Prediction Probabilities") +
		theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
		guides(fill=guide_legend(title="Attrition:"))
	})

	output$pred_svm <- renderPlot({ 
		ggplot(test_sub(), aes(prediction_svm, fill = factor(Attrition))) + geom_density(alpha = 0.2) + theme_bw() + labs(x = "Prediction Probabilities") +
		theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
		guides(fill=guide_legend(title="Attrition:"))
	})

	output$pred_lr <- renderPlot({ 
		ggplot(test_sub(), aes(prediction_lr, fill = factor(Attrition))) + geom_density(alpha = 0.2) + theme_bw() + labs(x = "Prediction Probabilities") + 
		theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
		guides(fill=guide_legend(title="Attrition:"))
	})

	output$info_plot <- renderPlot({ ggplot(test_sub(), aes(prediction, Attrition)) + geom_point(shape=1)})

    xgb_featureName <- reactive({
                            importance[ifelse(is.null(input$xgb_imp_click$y), 1, 11 - ceiling(input$xgb_imp_click$y*10))]$Feature
                        })


    xgb_0_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition < 0.5))$prediction_xgb, 
              y = subset(test_sub(), (Attrition < 0.5))[,xgb_featureName()], n = 20)
    })

    xgb_1_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition > 0.5))$prediction_xgb, 
              y = subset(test_sub(), (Attrition > 0.5))[,xgb_featureName()], n = 20)
    })

	output$xgb_importance <- renderPlot({ par(mar=c(6., 0, 0, 0))
		xgb.plot.importance(importance_matrix = importance, top_n = 10)
	})

    svm_0_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition < 0.5))$prediction_svm, 
              y = subset(test_sub(), (Attrition < 0.5))[,input$var_svm], n = 20)
    })

    svm_1_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition > 0.5))$prediction_svm, 
              y = subset(test_sub(), (Attrition > 0.5))[,input$var_svm], n = 20)
    })

    lr_0_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition < 0.5))$prediction_lr, 
              y = subset(test_sub(), (Attrition < 0.5))[,input$var_lr], n = 20)
    })

    lr_1_kde <- reactive({
        kde2d(x = subset(test_sub(), (Attrition > 0.5))$prediction_lr, 
              y = subset(test_sub(), (Attrition > 0.5))[,input$var_lr], n = 20)
    })

######  3D plot ###########
	output$prob_xgb_0 <- renderPlotly({ 
#                                    ggplot(test_sub(), aes_string(x = xgb_featureName(), "prediction_xgb")) + 
#									geom_point(aes(color = factor(Attrition)), , alpha =0.5) + theme_bw() + 
#									labs(y = "Prediction Probabilities") +
#									theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
#									guides(color=guide_legend(title="Attrition:"))
                                    plot_ly(x = xgb_0_kde()$y, 
                                            y = xgb_0_kde()$x,  
                                            z = xgb_0_kde()$z, 
                                            showscale=FALSE, size = 0.1) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 0",
                                        scene = list(
                                            xaxis = list(title = xgb_featureName()),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })

	output$prob_xgb_1 <- renderPlotly({ 
                                    plot_ly(x = xgb_1_kde()$y, y = xgb_1_kde()$x, z = xgb_1_kde()$z, 
                                            showscale=FALSE) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 1",
                                        scene = list(
                                            xaxis = list(title = xgb_featureName()),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })


	output$prob_svm_0 <- renderPlotly({ 
                                    plot_ly(x = svm_0_kde()$y, y = svm_0_kde()$x, z = svm_0_kde()$z, 
                                            showscale=FALSE) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 0",
                                        scene = list(
                                            xaxis = list(title = input$var_svm),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })

	output$prob_svm_1 <- renderPlotly({ 
                                    plot_ly(x = svm_1_kde()$y, y = svm_1_kde()$x, z = svm_1_kde()$z, 
                                            showscale=FALSE) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 1",
                                        scene = list(
                                            xaxis = list(title = input$var_svm),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })



	output$prob_lr_0 <- renderPlotly({ 
                                    plot_ly(x = lr_0_kde()$y, y = lr_0_kde()$x, z = lr_0_kde()$z, 
                                            showscale=FALSE) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 0",
                                        scene = list(
                                            xaxis = list(title = input$var_lr),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })

	output$prob_lr_1 <- renderPlotly({ 
                                    plot_ly(x = lr_1_kde()$y, y = lr_1_kde()$x, z = lr_1_kde()$z, 
                                            showscale=FALSE) %>% 
                                    add_surface() %>% 
                                    layout(
                                        title = "Employees with attrition = 1",
                                        scene = list(
                                            xaxis = list(title = input$var_lr),
                                            yaxis = list(title = "Attrition Probability"),
                                            zaxis = list(title = "Density"),
                                            camera = camera
                                        ),
                                        margin = list(l = 0, r = 0, b = 25, t = 50, pad = 4)
                                    )
								 })
	
###### Histogram plots #######	
	
	output$hist_plot_xgb <- renderPlot({
	  
	 dat1 = data.frame(x=subset(test_sub(), (Attrition <= 0.5))$prediction_xgb, group="Attrition=0")
	 dat2 = data.frame(x=subset(test_sub(), (Attrition > 0.5))$prediction_xgb, group="Attrition=1")
	 dat = rbind(dat1, dat2)
	 
	 # renders the histogram
	 ggplot(dat, aes(x, fill=group, colour=group)) +
	   geom_histogram(aes(y=..density..), breaks=seq(0,1,.04), alpha=0.6, 
	                  position="identity", lwd=0.2) +
	   xlab("XGBoost Confidence Score") + ylab("Normalized Counts") + theme_bw()
	})
	
	output$hist_plot_svm <- renderPlot({
	  
	  dat1 = data.frame(x=subset(test_sub(), (Attrition <= 0.5))$prediction_svm, group="Attrition=0")
	  dat2 = data.frame(x=subset(test_sub(), (Attrition > 0.5))$prediction_svm, group="Attrition=1")
	  dat = rbind(dat1, dat2)
	  
	  # renders the histogram
	  ggplot(dat, aes(x, fill=group, colour=group)) +
	    geom_histogram(aes(y=..density..), breaks=seq(0,1,.04), alpha=0.6, 
	                   position="identity", lwd=0.2) +
	    xlab("SVM Confidence Score") + ylab("Normalized Counts") + theme_bw()
	})
	
	output$hist_plot_lr <- renderPlot({
	  
	  dat1 = data.frame(x=subset(test_sub(), (Attrition <= 0.5))$prediction_lr, group="Attrition=0")
	  dat2 = data.frame(x=subset(test_sub(), (Attrition > 0.5))$prediction_lr, group="Attrition=1")
	  dat = rbind(dat1, dat2)
	  
	  # renders the histogram
	  ggplot(dat, aes(x, fill=group, colour=group)) +
	    geom_histogram(aes(y=..density..), breaks=seq(0,1,.04), alpha=0.6, 
	                   position="identity", lwd=0.2) +
	    xlab("Logistic Regression Probability") + ylab("Normalized Counts") + theme_bw()
	})
	


##############################	
	
	output$click_info <- renderPrint({
		test_sub <- test_sub()[c("MonthlyIncome", "OverTime", "TotalWorkingYears", "Age", "DailyRate", "EnvironmentSatisfaction", 	"NumCompaniesWorked", "Attrition", "prediction")]
		nearPoints(test_sub, input$plot_click, maxpoints = 1)
  	   }, width = 1000)
  	   
	output$corr_click_info <- renderPlot({
		x_column <- ifelse(is.null(input$corr_plot_click$x), 24, round(input$corr_plot_click$x))
		y_column <- ifelse(is.null(input$corr_plot_click$y), 1, round(input$corr_plot_click$y))
		n_columns <- length(good_columns())+1
		train_sub <- train_sub()[good_columns()]

		if(colnames(train_sub)[n_columns - y_column] == "Attrition"){
			ggplot(train_sub, aes_string(colnames(train_sub)[x_column])) + 
			geom_density(alpha = 0.2, aes(fill = factor(Attrition))) + 
			guides(fill=guide_legend(title="Attrition:")) + theme_bw() + 
			theme(legend.position = "bottom")
		} else if(colnames(train_sub)[x_column] == "Attrition"){
			ggplot(train_sub, aes_string(colnames(train_sub)[n_columns-y_column])) + 
			geom_density(alpha = 0.2, aes(fill = factor(Attrition))) + theme_bw() + 
			guides(fill=guide_legend(title="Attrition:")) + 
			theme(legend.position = "bottom")				
		} else if(colnames(train_sub)[x_column] != colnames(train_sub)[n_columns-y_column]) {
            x_name = colnames(train_sub)[x_column]
            y_name = colnames(train_sub)[n_columns-y_column]
			ggplot(train_sub, aes_string(x_name, y_name)) + 	
# 			geom_polygon(aes(fill = ..level.., group = ..piece..), stat = "density2d", alpha = .5) +
#   		 	scale_fill_continuous("Density Level", low = "#56B1F7", high = "#132B43") + 
  			stat_bin2d(bins = c(nbins[[x_name]], nbins[[y_name]])) + 
            guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         		fill = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + 
         	theme(legend.position = "bottom") 
  		} else {
            ggplot(train_sub, aes_string("factor(Attrition)", colnames(train_sub)[x_column])) + 
            geom_violin(alpha = 0.2, aes(fill = factor(Attrition))) + theme_bw() + 
            guides(fill=FALSE) + xlab("Attrition") +
            theme(legend.position = "bottom")
        }
		}, height = 280)
})
