wisc_bc_data <- read.csv("C:/TOWORK/R/git-project/KNN/breast-cancer-wisconsin-data/data.csv")
str(wisc_bc_data)
wbcd <- wisc_bc_data
wbcd <- wbcd[-1]

summary(wbcd)
wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c("B","M"), labels = c("Benign", "Malignant"))
head(wbcd$diagnosis)

# normalization
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}


wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_train <- wbcd_n[1:469,]
wbcd_test <-  wbcd_n[470:569,]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]


library("class")

# computing KNN
# k is equal to square root of the observation 469 ~ 21

wbcd_test_pred  <- knn(train = wbcd_train, test = wbcd_test, cl=wbcd_train_labels, k=21)


# evaluate the result
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)
