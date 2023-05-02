data <-  read.csv("Market_Basket_Optimisation.csv")
library(arules)

dataset <- read.transactions(file_path, sep = ",", rm.duplicates = TRUE )
summary(dataset)

# not necessary
library(RColorBrewer)
itemFrequencyPlot(dataset,
                  topN=20,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency (Relative)") 

# 4.1
# Set minimum support to 0.005 (50%) and confidence to 75%
rules.all <- apriori(dataset ,list(supp = 0.005, conf = 0.75)) 
summary(rules.all) 

# Sort the rules by confidence
library(arulesViz)
rules_sort<-sort(rules.all, by="confidence", decreasing=TRUE) 
inspect(rules_sort)
plot(rules.all)

# 4.2
# Set minimum support to 0.006 (60%) and confidence to 60%
rules.all <- apriori(dataset ,list(supp = 0.006, conf = 0.60)) 
summary(rules.all) 

# Sort the rules by confidence
rules_sort<-sort(rules.all, by="confidence", decreasing=TRUE) 
inspect(rules_sort)
plot(rules.all)

# error in dataset ,so no output is getting shown