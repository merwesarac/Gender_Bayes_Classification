gc()
memory.size(max=TRUE)
require(bigmemory)
require(class)
require(RTextTools)
require(e1071)
require(tm)
tweet=read.csv("classificat/classification_gender_script/egitim.csv",
               sep=";",
               header=FALSE)
korpum<-Corpus(VectorSource(tweet[,3]))
tm <- DocumentTermMatrix(korpum,
                         control=list(wordLengths=c(5, Inf),
                                      bounds=list(global=c(floor(length(korpum)*0.03335),
                                                           Inf))))
mat<-as.matrix(inspect(tm))
matrix=as.big.matrix(mat)
classifier <- naiveBayes(matrix[1:5,],
                         as.factor(tweet[1:5,4]))
predicted = predict(classifier,
                    matrix[5:8,])
result<-data.frame(tweet[5:8,],
                   predicted)
result[1:5,]
recall_accuracy(tweet[5:8,4],
                predicted)
table(tweet[5:8,4], predicted)
new<-read.csv("classificat/classification_gender_script/test.csv",
              sep=";",
              header=FALSE)
korpumnewb<-Corpus(VectorSource(new[,3]))
tm_new<- DocumentTermMatrix(korpumnewb,
                            control=list(wordLengths=c(3, Inf),
                                         bounds=list(global=c(floor(length(korpumnewb)*0.03335),
                                                              Inf))))
mat_new<-as.matrix(inspect(tm_new))
matrix_new=as.big.matrix(mat_new)
e<-nrow(mat_new)
predicted = predict(classifier,
                    matrix_new[1:e,])
result<-data.frame(new[1:e,],
                   predicted)
