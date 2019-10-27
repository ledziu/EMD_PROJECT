load_dataset <- function(){
  sledzie <- read.csv("sledzie.csv",header=TRUE)
  sledzie
}


dataset <- load_dataset()


head(dataset)
sumarry(dataset)