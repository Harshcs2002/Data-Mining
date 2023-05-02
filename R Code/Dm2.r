dirty_iris <- read.csv('dirty_iris.csv')
View(dirty_iris)
# Q2 i
cat(
  'No. of Observations: ',
  sum(complete.cases(dirty_iris)), "\n"
)

c <- sum(complete.cases(dirty_iris))

cat(
  '% of Correct Observations: ', 
  c / (dim(dirty_iris)[1]) * 100, "\n\n"
)
# Q2 ii
dirty_iris[, -5] = lapply(
  dirty_iris[, -5], 
  function(y) as.numeric(as.character(y))
)

# Q2 iii
library(editrules)
E <- editfile('Q2rules.txt')
E
# Q2 iv
violations <- violatedEdits(E, dirty_iris)
violations
summary(violations)
plot(violations)

# Q2 v
boxplot(dirty_iris$Sepal.Length)
boxplot.stats(dirty_iris$Sepal.Length)$out