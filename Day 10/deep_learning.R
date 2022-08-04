# This tutorial is taken from https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/
# The site has a lot of other useful resources for learning about DL in R

library(keras)
library(tidyverse)
install_keras()

fashion_mnist <- dataset_fashion_mnist()
View(fashion_mnist)


c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# =====================================================================
# The following code just shows you one image.
# You'll see that each image consists of 28x28 arrays with 255 pixel 'shades' in each slot
image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")
# =====================================================================


train_images <- train_images / 255 # Set pixel values to range between 0 and 1
test_images <- test_images / 255


# =====================================================================
# This is just a sanity check that our labels are aligned correctly:
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}
# =====================================================================


model <- keras_model_sequential() # Now we're cooking

model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax') # The last layer should reduce to # of classes
# Let's add some more layers.


# =====================================================================
# Or, try a more complex CNN (convolutional neural network) model:
# model <- keras_model_sequential() %>%
#   layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
#   layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
#   layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
#   layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
#   layer_dropout(rate = 0.25) %>% 
#   layer_flatten() %>% 
#   layer_dense(units = 128, activation = 'relu') %>% 
#   layer_dropout(rate = 0.5) %>% 
#   layer_dense(units = 10, activation = 'softmax')
# =====================================================================


model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',  # "Loss is the penalty for a bad prediction." https://developers.google.com/machine-learning/crash-course/descending-into-ml/training-and-loss
  metrics = c('accuracy')
)

model %>% fit(train_images, train_labels, epochs = 5, verbose = 2) # Run twice. Compare results.
# If you use a CNN, add more epochs.

score <- model %>% evaluate(test_images, test_labels, verbose = 0)
score


predictions <- model %>% predict(test_images)
View(predictions) # This mess of numbers is how confident the model is at mapping image to label.
which.max(predictions[1, ]) 
# You can run this programmatically across the whole table:
class_pred <- model %>% predict(test_images) %>% k_argmax()
class_pred[1:20]


# =====================================================================
# Let's take a peek again
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}


# =====================================================================
# And test on a single image:
img <- test_images[1, , , drop = FALSE]
prediction <- model %>% predict(img)
prediction
prediction <- prediction[1, ] - 1
which.max(prediction)


# What about text-based models? Embeddings!
# If you want to see how this works for text classification, see https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_text_classification/