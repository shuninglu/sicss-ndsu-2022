# Supervised Machine Learning for Prediction
# Adapted from SICSS-Rutgers, 2021

## If do not have this package, use install.packages("")
## For multiple, can do install.packages(c("tidymodels", "sentimentr", "pROC", "themis"))
## Some of these packages may rely on dependencies. You can add install.packages("tidymodels", dependencies =T) to address this if you run into an issue.
library(tidyverse)
library(tidymodels)
library(readr) 
library(stringr)
library(tidyr)
library(yardstick)
library(pROC)
library(tidytext)
library(sentimentr)
library(themis)

# Load hate speech data
hate <- read_csv("https://github.com/t-davidson/hate-speech-and-offensive-language/blob/master/data/labeled_data.csv?raw=true")

#' The data include seven variables and 24,783 observations. 
#' 
#'   - Each observation is a tweet.
#'       + The text of the tweet is available in the variable `tweet`
#'       + Each tweet has been classified by three or more CrowdFlower users as hate speech, offensive language, or something else. These counts are available in the `count`, `hate_speech`, `offensive_language`, and `neither` variables.
#'   - The key outcome variable is `class`.
#'       + `class`: label given by majority of CrowdFlower users. 0 - hate speech 1 - offensive language 2 - neither
#'   
table(hate$class)

#' For the purpose of this example, we will recode this variable such that it is dichotomous 1= hate speech and 0= not hate speech. We will call the new variable `outcome`.
## Let's simplify the outcome. 1 if hate speech, 0 otherwise
hate <- hate %>%
  mutate(outcome = if_else(class == 0, 1, 0))

table(hate$outcome)

#' What proportion of tweets are hate speech in these data?
mean(hate$outcome)


#' ### Building features
#' We will construct a few variables for now. What might be defining features of tweets with hate speech? Examples:
#' 
#'   - Length of the tweet?
#'   - Capital letters?
#'   - Sentiment of the tweet?
#'   - Profanity?
#'   - Certain keywords?
#' 
## Length of tweet
hate$lengthoftweet <- nchar(hate$tweet)

## Number of capital letters
hate$capitals <- str_count(hate$tweet, "[A-Z]")

## Number of negative words
library(tidytext)
tidy_hate <- hate %>%
  unnest_tokens("word", tweet)

## Count number of negative, positive, NA words in tweet
hate_tweet_sentiment <- tidy_hate %>%
  left_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) 

## Add negative count to original dataframe
hate$nnegative <- hate_tweet_sentiment$negative


#' Number of profane words
#'   - Note: this function may generate a warning message, which you can ignore for this application.
library(sentimentr)
hate_profanity <- profanity_by(hate$tweet)

## Add profanity count to original dataframe
hate$nprofanity <- hate_profanity$profanity_count

 
#' ### Getting our data ready for training vs. testing
## How many rows are 80%
sample_size <- floor(0.8 * nrow(hate))
sample_size

## Randomly select row numbers to include in training data
set.seed(1234) # for reproducibility
train_ind <- sample(1:nrow(hate), size = sample_size)
training_hate <- hate[train_ind, ] 
holdout_hate <- hate[-train_ind,] # The - means "not"

#' ### Implementing our model

## Fitting logistic regression model in R
model1 <- glm(outcome ~ lengthoftweet + 
                nnegative + 
                nprofanity + 
                capitals,
              data = training_hate,
              family=binomial(link = "logit"))
model1

#' ### Testing performance of model
#' In-sample Prediction (not preferred): Predicting using the training data used in the model fitting process.
## Extracting probabilities outcome = 1 from model
training_hate$model1probs <- predict(model1, newdata = training_hate, type="response")

#' Out-of-sample Prediction (preferred): Predicting using data not used in the model.
## Extracting probabilities outcome = 1 from model
holdout_hate$model1probs <- predict(model1, newdata = holdout_hate, type="response")

#' Note: These are just probabilities at this point. There are some metrics for evaluating probabilities, but ultimately, we want **classifications** (hate speech or not hate speech). 
#' 
#' We need to set a threshold (a decision rule) above which a tweet will be classified as hate speech.
#'   
#'   - A default and intuitive threshold is .5.
#'   - This is where a data imbalance can be particularly problematic.
#'   - One initial approach is to use ROC curves to help select a threshold
#'       + For more on ROC curves and their usage, see this [post](https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5).
#'   - A different approach is to adjust the training sample itself using downsampling or upsampling so that the training data are more balanced by class. More on this later.
#' 
library(pROC)
rocCurve   <- roc(response = training_hate$outcome,
                  predictor = training_hate$model1probs)
plot(rocCurve, print.thres = "best")

#' Let's use this threshold for classification. We will focus on the out-of-sample test.
## classify based on probability
holdout_hate <- holdout_hate %>%
  mutate(predictedhate = if_else(model1probs > .059, 1, 0))

#' Did we get things right?
#' Let's start with accuracy.
#' 
#'   - This is a parsimonious measure but doesn't really tell us where the errors are.
#' 
## Proportion of correct matches between truth and prediction
mean(holdout_hate$outcome == holdout_hate$predictedhate)

## Confusion Matrix
table(truth = holdout_hate$outcome, predictions = holdout_hate$predictedhate)

#' Specificity: ' 
#'   - Of all the tweets that are not hate speech, what proportion did we correctly classify?
trueneg <- table(truth = holdout_hate$outcome, predictions = holdout_hate$predictedhate)[1,1]
falsepos <- table(truth = holdout_hate$outcome, predictions = holdout_hate$predictedhate)[1,2]

trueneg / (trueneg + falsepos)


#' Sensitivity (aka recall):  
#'   - Of all the tweets that are hate speech, what proportion did we correctly predict?
truepos <- table(truth = holdout_hate$outcome, predictions = holdout_hate$predictedhate)[2,2]
falseneg <- table(truth = holdout_hate$outcome, predictions = holdout_hate$predictedhate)[2,1]

truepos / (truepos + falseneg)
# We could sort of tell that TPR wouldn't be great after looking at the confusion matrix.



# =======================================================
#' # Using `tidymodels`
  
#' To get started, we will return to our original dataset. Let's subset the data to include only those columns we will use in analysis. This will make writing the data formula very easy.
#' 
hate <- hate %>% select(outcome, lengthoftweet, capitals, nnegative, nprofanity)

#' The functions will work better for classification exercises if we tell R that `outcome` should be a factor variable. There is a way to do this within the `tidymodels` process, but it's easier to do at the outset.
hate$outcome <- as.factor(hate$outcome)


#' ## Splitting data into train and test
 
library(tidymodels)

set.seed(5678)
hate_split <- hate %>%
    initial_split(prop = 0.8)

## Divide data based on split
hate_train <- training(hate_split)
hate_test <- testing(hate_split)


#' ## Establishing model type
#' 
#' In tidymodels, you specify models using three concepts.
#' 
#'   - `type` of models such as logistic regression, decision tree models,random forests, and so forth.
#'   - `mode` includes options regression and classification; some model types support either of these while some only have one mode. With a dichotomous outcome, we are in classification world.
#'   - `engine` is the computational tool to fit the model. You will notice our friend `glm` below.
#' 
## A logistic regression model specification
logit_mod <- logistic_reg() %>%
    set_mode("classification") %>%
    set_engine("glm")

## A decision tree model specification
tree_spec <- decision_tree() %>%         
    set_engine("rpart") %>%      
    set_mode("classification") 



#' ## Creating recipes 
library(themis)
hate_recipe <- recipe(outcome ~ ., data = hate_train) %>% 
    step_downsample(outcome)


#' To see how the downsampling works, we can apply it once to our data (not necessary to include this step).
hate_prep <- prep(hate_recipe)
hate_down <- bake(hate_prep, new_data = NULL)
hate_down %>%
    count(outcome)


#' ## Implementing workflows
## Start a workflow (recipe only)
hate_wf <- workflow() %>%
    add_recipe(hate_recipe)

## Add the model and fit the workflow
hate_log <- hate_wf %>%
    add_model(logit_mod) %>%
    fit(data = hate_train)

hate_tree <- hate_wf %>%
    add_model(tree_spec) %>%
    fit(data = hate_train)

#' ## Assessing model performance
# Add prediction columns to testing data
results <- hate_test %>%
    bind_cols(predict(hate_log, hate_test) %>%
                  rename(.pred_log = .pred_class)) %>%
    bind_cols(predict(hate_tree, hate_test) %>%
                  rename(.pred_tree = .pred_class))
# Note: when using regression it may be .pred instead of .pred_class

## Calculate accuracy
accuracy(results, truth = outcome, estimate = .pred_tree)
accuracy(results, truth = outcome, estimate = .pred_log)


#' Confusion Matrix
#'   - Let's focus on the decision tree specification
conf_tree <- results %>%
    conf_mat(truth = outcome, estimate = .pred_tree)

## Raw output
conf_tree


#' Additional metrics
#' 
#'   - See the `yardstick` documentation for definitions (e.g., [ppv](https://yardstick.tidymodels.org/reference/ppv.html))
## Provides a wide range of metrics
summary(conf_tree)

 
#' # Cross-Validation (using `tidymodels`)
## Break up training data into 5 folds. 
set.seed(5678)
hate_folds <- vfold_cv(hate_train, v = 5, strata=outcome)

## Create the recipe
hate_recipe <- recipe(outcome ~ ., data = hate_train) %>% 
    step_downsample(outcome)

## Start a workflow (recipe only)
hate_wf <- workflow() %>%
    add_recipe(hate_recipe)

## Add the model  but do not yet fit the workflow
hate_log_pre <- hate_wf %>%
    add_model(logit_mod)
hate_tree_pre <- hate_wf %>%
    add_model(tree_spec)

## Now fit with the resamples
set.seed(5678)
hate_log_res <- hate_log_pre %>%
    fit_resamples(
        hate_folds,
        metrics = yardstick::metric_set(sensitivity, accuracy, specificity),
        control = control_resamples(save_pred = TRUE)
    )

set.seed(5678)
hate_tree_res <- hate_tree_pre %>%
    fit_resamples(
        hate_folds,
        metrics = yardstick::metric_set(sensitivity, accuracy, specificity),
        control = control_resamples(save_pred = TRUE)
    )

#' ## Evaluate performance
#' 
#'   - With cross-validation, this will give us the average performance across tests
#'       + Instead of just one test, we now get as many tests as we had folds
#' 
## To see results for each fold add summarize = F to the function
collect_metrics(hate_tree_res)
collect_metrics(hate_log_res)


#' # Applying tidymodel to new data
#' ## Final test
#' Conveniently, the package contains a function `last_fit()` to do this for us.
## Recall the recipe
hate_recipe <- recipe(outcome ~ ., data = hate_train) %>% 
    step_downsample(outcome)

## We will choose the tree specification
hate_final_wf <- workflow() %>%
    add_recipe(hate_recipe) %>%
    add_model(tree_spec)

## Last fit-- supply the initial split data object
hate_final <- hate_final_wf %>%
    last_fit(hate_split)


#' Evaluate performance on `hate_test` data
## Confusion matrix
final_conf <- hate_final %>% 
    collect_predictions() %>% 
    conf_mat(outcome, .pred_class)
final_conf

## Other metrics
summary(final_conf)


#' ## Saving model for use with new data

# saved as workflow object
final_model <- fit(hate_final_wf, hate)
# if only want model
final_jmodel <- extract_model(fit(hate_final_wf, hate))

# can save fitted workflow object
saveRDS(final_model, file="hate_speech_classifier.RDS")

