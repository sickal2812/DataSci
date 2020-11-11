library(arules)
library(arulesViz)
library(shinythemes)
library(pmml)
library(dplyr)



data = read.csv("야생동식물.csv")
head(data)



data = data %>% select(국명,서식지코드)
head(data)



code <- NULL
code <- data %>% select(서식지코드)
code <- unique(code)
head(code)



write.csv(data, "code.csv", row.names = FALSE)



## data = 국명,서식지코드 // code = 서식지코드
## code.scv에서 유일값을 지우고 싶은데 어떻게?
data1 = read.csv("code.csv")
summary(data1)



head(data1)
print(data1[[1]][1])



length(which(data1$국명=="붉은귀거북"))
length(which(data$국명==data[[1]][1]))



for(i in nrow(data):1) {
  if(length(which(data1$국명==data[[1]][i]))==1) {
    data1 <- data1[!(data1$국명==data[[1]][i]), ]
  }
}



data1 <- unique(data1)
write.csv(data1, "code2.csv", row.names = FALSE)



nrow(data1)
nrow(data)



## code2를 사용할것



data = read.csv("code2.csv")




code <- NULL
code <- data %>% select(서식지코드)
code <- unique(code)
head(code)



print("code2의 서식지 코드 갯수")
nrow(code)



code[[1]][1]
code[[1]][297]
nrow(code)



new_data <- NULL
new_data <- data %>% filter(서식지코드 == code[[1]][2])
new_data <- new_data %>% select(국명)
print(new_data)
new_data <- unlist(new_data, use.names = FALSE)
print(new_data)
new_data <- list(new_data)
print(new_data)



#new_data <- NULL
#new_data <- data %>% filter(서식지코드 == "p0005")
#new_data <- new_data %>% select(국명)
#print(new_data)



print(typeof(new_data))



df <- NULL
for(i in 1:nrow(data)) {
  new_data <- NULL
  new_data <- data %>% filter(서식지코드 == code[[1]][i])
  new_data <- new_data %>% select(국명)
  #print(new_data)
  new_data <- unlist(new_data, use.names = FALSE)
  #print(new_data)
  new_data <- list(new_data)
  df[i] <- c(new_data)
}



df.trans <- as(df, "transactions")
df.trans
summary(df.trans)



df.rules <- apriori(df.trans, parameter = list(support = 0.002, confidence = 0.01, maxlen=7, minlen=2))
summary(df.rules)
inspect(df.rules)



df.rules <- eclat(df.trans, parameter = list(support = 0.0025, maxlen=7, minlen=2))
inspect(df.rules)