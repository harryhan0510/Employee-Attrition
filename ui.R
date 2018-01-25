library("shiny")
library("shinythemes")
library("plotly")

varNames = c("Age", "BusinessTravel", "Department", 
             "DistanceFromHome", "Education", "EducationField", 
             "EnvironmentSatisfaction", "Gender", "JobInvolvement", "JobRole", "JobSatisfaction", 
             "MaritalStatus", "MonthlyIncome", "NumCompaniesWorked", 
             "OverTime", "RelationshipSatisfaction", 
             "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", 
             "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole", 
             "YearsSinceLastPromotion")

### Final Explanation Paragraphs ###

final_explanation_1 <- p("To actively address overall employee retention issues, we need to look at the most important",
                         "features that determine the attrition probability and see if we can improve company retention.",
                         "One of the benefits of using XGBoost is the in-built estimation of feature importance.",
                         "The importance metric provides a score indicating how valuable each factor was in the construction",
                         "of the boosted decision trees. Higher relative importance indicates a larger impact on the algorithm",
                         "and final prediction. Since the importance is calculated explicitly for each attribute, these",
                         "attributes can be ranked and compared to each other."
                          )

final_explanation_2 <- p("The algorithms themselves result in varying degrees of precision and recall; these can be",
                         "adjusted in real-time to optimize the model based on the needs of the business. In a true analysis, these",
                         "algorithms would be further tuned and potentially ensembled to provide the most accurate",
                         "prediction of employee attrition possible. For now, they can be compared using the graphical",
                         "plots in the app."
                          )

final_explanation_3 <- p("When the model is run on the entire dataset, the results show that Marital Status, Number of",
                         "Companies Worked For, and Age are the dominant drivers of employee attrition for this dataset.",
                         "In terms of HR adjustable parameters, we see that some Job Involvement, Stock Option and Monthly",
                         "Income might be used as incentive for high value employees. In addition to this, we can use similar",
                         "feature importance methods to create an employee specific ranking of controllable factors that can then",
                         "be used to target employees on an individual-by-individual basis."
                          )

final_explanation_4 <- p("The final output of the classifier can be both a ranked list of individuals most likely to leave and",
                          "also a ranked list of contributing factors that govern each employee’s attrition probability. Both are",
                          "highly useful for any HR department aiming to minimize talent loss and ultimately save company dollars."
                          )

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    column(width=12,
    a(img(src="SFL-LOGO.png",align="right",width="300px", padding="50px 50px 50px 50px"),href="https://www.sflscientific.com"),
    titlePanel("Employee Attrition")),
    sidebarLayout(
        sidebarPanel(
            helpText("We recommend 3 or fewer options at a time."),
            width = 2,
            selectInput("Age", label = "Select an age range", choices = c("< 35", "35-45", "> 45", "all"), selected = "all"),
            selectInput("Gender", label = "Select a gender", choices = c("Female", "Male", "all"),selected = "all"), 
            selectInput("Education", label = "Select an education level",choices = c("1", "2", "3", "4", "all"), selected = "all"),
            selectInput("MonthlyIncome", label = "Select a monthly income range", 
                        choices = c("< 2500", "2500-5000", "5001-7500", "7501-10000", "> 10001", "all"), selected = "all"),
            selectInput("MaritalStatus", label = "Select a marital status", choices = c("Single", "Married", "Divorced", "all"),
                        selected = "all")
    	),    
    	mainPanel(
    	    tabsetPanel(
                tabPanel("Data Problem", width = 12, class="well",
                    p("Employee attrition is the rate at which employees leave a company. The goal of this analysis is",
                    "to model employee attrition and determine the most dominant contributing factors that govern this turnover."
                    ),
                    p("Through this kind of analysis, we can understand not only how many employees are likely to leave,",
                    "but also which employees are at the highest risk of leaving and why.",
                    "Companies face significant costs for having to search for, interview and hire new employees. Although",
                    "this depends on the industry, in general, a company aims to retain their employees for a significant",
                    "period of time. This analysis is particularly useful if a company wants to lower attrition levels but",
                    "is unsure of the source of the problem. On the other hand, if the company needs to decrease their labor",
                    "costs, then employee attrition could be considered a positive thing. With this analysis, the company would",
                    "know which departments or groups of employees would be most likely to leave and prepare for those positions to be vacated."
                    ),
                    p("We first perform an exploratory analysis on the dataset using visual tools, which allows ",
                      "us to summarize the main characteristics of a dataset. From here, we perform modelling that will",
                      "determine the probability that each individual will attrite and uncover the most important",
                      "factors that lead to overall employee turnover. Based on the needs of the employer, this",
                      "analysis can also be narrowed down to determine key factors governing attrition for particular",
                      "demographics, job titles, working groups, and indeed specific individuals."
                    ),
                    p("In this study, we use several algorithms to model employee attrition: extreme gradient boosting",
                      "(XGBoost), support vector machines (SVM), and logistic regression."
                    ),
                    p("XGBoost uses boosted decision trees which classify the members of a family into different leaves,",
                      "then assign them a score on that leaf. Multiple trees are ensembled to improve the predictive power",
                      "of the model. SVM is a discriminative classifier that takes labeled training data and constructs a",
                      "hyperplane to categorize new examples. Finally, logistic regression is a simple classifier used to",
                      "estimate the probability of an binary outcome based on several predictors."
                    ),
                    p("We limited the subsetting options to five factors: age, gender, education level, monthly income",
                      "and marital status. The size of the dataset should be taken into account when deciding how many",
                      "options to provide the user. Certain algorithms need a certain amount of data in order to properly",
                      "run, and unless the user knows exactly how much data is in each subset, there are likely to be errors."
                      ),
                    style = "background-color:white;"
                ),

  				tabPanel("Data Exploration", width = 12,
				    column(width = 8, class = "well", h4("Correlation Matrix"),
      				    plotOutput("corr_plot", click = "corr_plot_click"),
                        div(style = "height:110px;background-color: white;"),
                        p("The above correlation matrix displays the linear correlation between every pair of",
                        "features in the form of dots of varying colors and sizes. A larger dot indicates that",
                        "the correlation between these selected features is stronger, whereas the color denotes",
                        "the strength of the positive (blue) or negative (red) correlation coefficient."
                        ),
      				          p("This application has an additional functionality: by clicking any element in this",
      				            "matrix, a 2D histogram is displayed in order to better observe the correlation between",
      				            "those features. Alternatively, clicking the elements along the leading diagonal will output",
                          "violin plots of the selected features, bucketed by the true underlying attrition value",
                          "(1 indicating employees that attrite, and 0 indicated those that remain)."
      				          ),
      					style = "background-color:white;"
					),
					
					column(width = 4, class = "well", h4("Correlation plot of chosen variables:"), 
						plotOutput("corr_click_info", height="280px"),
						    p("Correlation between variables allows us to determine how similar and",
						    "overlapping features are for the dataset. In general, the machine learning algorithm",
						    "should be given as much uncorrelated information as possible to maximise the predictive accuracy."
						    ),
						    p("Along the leading diagonal, violin plots are used to show the full distribution of the data.",
                "Violin plots were chosen over box plots because of their usefulness when the data is multimodal."
						    ),
                        style = "background-color:white;"
                    )
    	  		),
    	  		tabPanel("Training (ROC)", 
  					column(width = 8, class = "well", h4("ROC Curve"),
      				    plotOutput("plot"),
      					style = "background-color:white;",
	        			sliderInput("thresh", label = "", min = 0, max = 1, value = c(0.5)),
                        p("A receiver operating characteristic (ROC) curve is the result of plotting the true",
                        "positive rate against the false positive rate. The curve is shown for three machine",
                        "learning algorithms: XGBoosted decision trees, support",
                        "vector machines, and logistic regression. The closer the ROC curve is to the top",
                        "left corner, the greater the accuracy of the test."
                        ),
                        p("The slider allows the user to change the operation point of the algorithm by setting",
                          "the false positive rate. The changes made to this cut-off are then reflected in the",
                          "confusion matrices shown, where each confusion matrix shows the performance of the",
                          "predictions of the various algorithms with respect to the true label."
                        ),
      					        p("By controlling the tradeoff between False Positives and False Negatives, businesses",
                          "can determine where they would like to bias their analysis towards. For example, in",
                          "cases such as medicine where undiagnosed issues have much more importance, we can skew",
                          "the operation point to the left - where we find far fewer false negatives (but more",
                          "false positive) instances. Conversely for businesses with very limited HR resources, it may",
                          "be best to go for fewer false positives (to the right of the curve), rank the confidence",
                          "scores given by the raw algorithm and target only the highest risk employees with incentives."
      					        )
                    ),
					column(width = 4, class = "well", 
		       			tabsetPanel(
		       			    tabPanel("XGBoost", h4("Confusion Matrix (XGBoost)"), plotOutput("confusionMatrix"), style = "background-color:white;"),
		       				tabPanel("SVM", h4("Confusion Matrix (SVM)"), plotOutput("confusionMatrix_svm"), style = "background-color:white;"),
		       				tabPanel("Logistic Regression", h4("Confusion Matrix (Logistic Regression)"), 
		       				plotOutput("confusionMatrix_lr"), style = "background-color:white;")
                        ), style = "background-color:white;"
                    )
				),
    	  		tabPanel("Training (Precision)", 
  					column(width = 8, class = "well", h4("Precision vs Cutoff Curve"),
                        plotOutput("plot_precision"),
                        style = "background-color:white;",
                        sliderInput("thresh_precision", label = "", min = 0, max = 1, value = c(0.5)),
                        p("The precision vs cutoff curve is shown for three machine",
                          "learning algorithms: XGBoosted decision trees, support",
                          "vector machines, and logistic regression. The slider is present for",
                          "the user to change the cutoff point of the algorithm.",
                          "This is also reflected in the confusion matrix on the right,", 
                          "which shows the prediction of the different algorithms with", 
                          "respect to the true label."
                        ),
  					            p("By controlling the precision cutoff, businesses can determine where they would like to",
                          "bias their analysis towards depending on their goals and resources."
  					            )
  					       
                    ),
					column(width = 4, class = "well", 
		       			tabsetPanel(
		       				tabPanel("XGBoost", h4("Confusion Matrix (XGBoost)"), plotOutput("confusionMatrix_precision"), style = "background-color:white;"),
		       				tabPanel("SVM", h4("Confusion Matrix (SVM)"), plotOutput("confusionMatrix_svm_precision"), style = "background-color:white;"),
		       				tabPanel("Logistic Regression", h4("Confusion Matrix (Logistic Regression)"), 
		       				plotOutput("confusionMatrix_lr_precision"), style = "background-color:white;")
                        ), style = "background-color:white;"
                    )
				),
    	  		tabPanel("Training (Recall)", 
  					column(width = 8, class = "well", h4("Recall vs Cutoff Curve"),
      				    plotOutput("plot_recall"),
      					style = "background-color:white;",
	        			sliderInput("thresh_recall", label = "", min = 0, max = 1, value = c(0.5)),
                        p("The recall value  vs cutoff curve is shown for three machine learning", 
                          "algorithms: XGBoosted decision trees, support vector machines, and",
                          "logistic regression. The slider is present for the user to change the cutoff", 
                          "point of the algorithm.", 
                          "This is also reflected in the confusion matrix on the right, which shows", 
                          "the prediction of the different algorithms with respect to the true label."
                        ),
      					         p("By controlling the recall cutoff, businesses can determine where they would like to",
      					          "bias their analysis towards depending on their goals and resources."
      					        )
                    ),
					column(width = 4, class = "well", 
		       			tabsetPanel(
		       				tabPanel("XGBoost", h4("Confusion Matrix (XGBoost)"), plotOutput("confusionMatrix_recall"), style = "background-color:white;"),
		       				tabPanel("SVM", h4("Confusion Matrix (SVM)"), plotOutput("confusionMatrix_svm_recall"), style = "background-color:white;"),
		       				tabPanel("Logistic Regression", h4("Confusion Matrix (Logistic Regression)"), 
		       				plotOutput("confusionMatrix_lr_recall"), style = "background-color:white;")
                        ), style = "background-color:white;"
                    )
				
				),
				tabPanel("Predictions",
		       	    fluidRow(
		       		    tabsetPanel(
		       			    tabPanel("XGBoost", column(width = 6, plotOutput("pred_xgb")),
                                                column(width = 6, h4("Top 10 Features:"), plotOutput("xgb_importance", click = "xgb_imp_click")),
		       									column(width = 6, plotlyOutput("prob_xgb_0"), style = "background-color:white;"),
		       									column(width = 6, plotlyOutput("prob_xgb_1"), style = "background-color:white;")
		       				),
		       			    tabPanel("SVM", column(width = 6, plotOutput("pred_svm")),
		       								#column(width = 6, plotOutput("prob_svm"), style = "background-color:white;"),
							                column(width = 6,
							                    selectInput("var_svm", label = "Select feature to plot:", choices = varNames, selected = "Age")
		       				                ),
		       				                column(width = 7),
		       				                column(width = 6, plotlyOutput("prob_svm_0"), style = "background-color:white;"),
		       				                column(width = 6, plotlyOutput("prob_svm_1"), style = "background-color:white;")
		       				),		
		       			    tabPanel("Logistic Regression", column(width = 6, plotOutput("pred_lr")),
		       								#column(width = 6, plotOutput("prob_lr"), style = "background-color:white;"),
		       				                column(width = 6,
		       				                    selectInput("var_lr", label = "Select feature to plot:", choices = varNames, selected = "Age")
		       				                ),
		       				                column(width = 7),
		       				                column(width = 6, plotlyOutput("prob_lr_0"), style = "background-color:white;"),
		       				                column(width = 6, plotlyOutput("prob_lr_1"), style = "background-color:white;")
		       				)
                        ),
                        p("Top left: Predicted probability vs density of the two types of employees.", 
                          "The more separated they are, the better."),
                        p("Top right: Features one can select to be drawn in the plots below."), 
                        p("Bottom left: 3D density plot of the selected feature vs predicted probability",
                          "for employees with attrition = 0. The density corresponds to the normalised",
                          "number of people in that bin."),
                        p("Bottom right: 3D density plot of the selected feature vs predicted probability",
                          "for employees with attrition = 1. The density corresponds to the normalised", 
                          "number of people in that bin.")
                    ), 
		       		style = "background-color:white;"
                ),
                tabPanel("Explanation of Results",
                         fluidRow(column(width=12),
                                  tabsetPanel(
                                    tabPanel("XGBoost", column(width = 10, class="well", plotOutput("hist_plot_xgb"),
                                                          div(style = "height:110px;background-color: white;"),
                                                          final_explanation_1, final_explanation_2, final_explanation_3,
                                                          final_explanation_4,
                                                          style = "background-color:white;"
                                    )
                                    ),
                                    tabPanel("SVM", column(width = 10, class="well", plotOutput("hist_plot_svm"),
                                                           div(style = "height:110px;background-color: white;"),
                                                           final_explanation_1, final_explanation_2, final_explanation_3,
                                                           final_explanation_4,
                                                           style = "background-color:white;"
                                    )
                                    ),
                                    tabPanel("Logistic Regression", column(width = 10, class="well", plotOutput("hist_plot_lr"),
                                                            div(style = "height:110px;background-color: white;"),
                                                            final_explanation_1, final_explanation_2, final_explanation_3,
                                                            final_explanation_4,
                                                            style = "background-color:white;"
                                    )
                                    )
                                  )
                         )        
    	  	        )
    	        )
    	  	)
  		)
   )
)

