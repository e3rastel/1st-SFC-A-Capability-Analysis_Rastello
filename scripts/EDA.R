### Risk Visualization 

rm(list = ls())

# install.packages("reshape2")


library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library (plotly)

# Load data (update with actual file path)
data <- read.csv("CBA Quantitative Analysis_filled_CSV_test_total.csv", header = TRUE)
dt <- fread("CBA Quantitative Analysis_filled_CSV_test_total.csv")

# EDA
head(dt)

dim(dt) # 2899 rows and 6 columns

dt[,length(unique(Capability))] # 42 different capabilities

# Is the same volume of data for every capability?
dt[Capability=="MDO Mission Threads C-C5ISRT"] %>% dim()

for (i in dt[,unique(Capability)]) {
  print(sprintf("%s has %d samples", i, dim(dt[Capability==i])[1]))
}

# Nope some capabilities have 25, others have much more.

# Was every capability tested against all scenarios?
for (i in dt[,unique(Capability)]) {
  cat(sprintf("%s capability was tested against these scenarios:",i),
        dt[Capability==i,unique(Scenario)],"\n")
}

# Nope some were tested on all scenarios, some were only tested on one

# Precision Messaging has the most samples, with 98 observations. So lets start there.
precMessaging <- dt[Capability=="Precision Messaging"]

# Lets focus on Scenario X and see how close the distribution of scores is to a normal distribution

scores <- precMessaging[Scenario=="Scenario X",Score]
length(scores) #only 25 samples

# Create a histogram
plot_ly(
  x = ~scores,
  type = 'histogram',
  nbinsx = 30  # Number of bins
)

# Shapiro-wilk test (best for small sample size)
shapiro.test(scores) 

#p value is 0.04675, showing strong evidence that the data is not normally distributed

# Since we have a small sample size and a sample distribution that likely does not follow a parametric
# distribution, we can use the Wilcoxon signed rank test to assess whether the median of our sample distribution 
# is significantly different from a specified value.

# Wilcoxon signed rank test

sour_score <- 0
wilcox.test(scores, 
            mu = sour_score, 
            alternative = "less",
            exact=TRUE)

# p-value is 1 so we do not reject the null hypothesis.
# This shows evidence that the median of the sample does not lie below 0

high_risk_score <- 16
wilcox.test(scores, 
            mu = high_risk_score, 
            alternative = "greater",
            exact=FALSE)
# p-value is almost 1 so we do not reject the null hypothesis.
# This shows evidence that the median of the sample does not lie above 16

# Need some degree of measuring chances or risk of scoring negative or above 16
sum(scores<0) # 0 
sum(scores>16) # 4

# Binomial test to give confidence interval for probability of scoring above 16
btest <- binom.test(x=sum(scores>16),
                    n=length(scores),
                    p=sum(scores>16)/length(scores))

# Confidence interval
btest[4]$conf.int[1:2] 
# 95% probability that the risk of scoring above 16 lies between 0.04 and 0.36


# Binomial test to give confidence interval for probability of scoring below 0
btest <- binom.test(x=sum(scores<0),
                    n=length(scores),
                    p=sum(scores<0)/length(scores))

# Confidence interval
btest[4]$conf.int[1:2] 
# 95% probability that the risk of scoring below 0 lies between 0 and 0.14

btest[4]$conf.int[2] >= 0.5

# Run the same analysis for the other scenarios and store in a list,
# so you can present a 95% confidence interval for the RISK of falling above 16 or below 0.

# Define a list
precMessagingResults_missionCritical <- list()
  
# iterate against every scenario
for (scenario in precMessaging[,unique(Scenario)]) {
  
  # Define a vector of scores 
  scores <- precMessaging[Scenario==scenario,Score]
  
  # Run the binomial test
  btest <- binom.test(x=sum(scores > 16),
                      n=length(scores),
                      p=sum(scores > 16)/length(scores))
  
  # Store the conf interval
  precMessagingResults_missionCritical[[scenario]] <- btest[4]$conf.int[1:2]
}

# Define a function
getBinomTestConfIntervals <- function(observations,scenario,criticalValue,verbose=FALSE) {
  
  # Make sure the criticalValue is 16 or 0
  if (criticalValue != 16 & criticalValue != 0) {stop("criticalValue must be 0 or 16")}
  
  # Define subset of the data for the scenario of interest
  scenarioData <- observations[Scenario==scenario]
  
  # Define list for all capabilities to store sample probabilities
  sampleProbs <- vector("list",length(observations[,unique(Capability)]))
  names(sampleProbs) <- observations[,unique(Capability)]
  
  # Define list for all capabilities to store text
  scenarioText <- vector("list",length(observations[,unique(Capability)]))
  names(scenarioText) <- observations[,unique(Capability)]
  
  # Print the unique capabilities for the scenario just to check
  if (verbose==TRUE) {
    cat(sprintf("%s was tested against these capabilities: ",scenario),
        scenarioData[,unique(Capability)],
        "\n")
  }
  
  # iterate against every capability
  for (capability in scenarioData[,unique(Capability)]) {
    
    # Define a vector of scores 
    scores <- scenarioData[Capability==capability,Score]
    
    # Compute the sample statistics
    if (criticalValue==16) {
      sample_list <- list(
        sample_x=sum(scores > criticalValue),
        sample_n=length(scores),
        sample_p=sum(scores > criticalValue)/length(scores)
      )
    } else {
      sample_list <- list(
        sample_x=sum(scores < criticalValue),
        sample_n=length(scores),
        sample_p=sum(scores < criticalValue)/length(scores)
      )
    }
    
    # Run the binomial test
    btest <- binom.test(x=sample_list$sample_x,
                        n=sample_list$sample_n,
                        p=sample_list$sample_p)
    
    # Store the sample probability
    sampleProbs[[capability]] <- sample_list[["sample_p"]]
    
    # Store the text
    scenarioText[[capability]] <- paste0(
      "<b>Capability: </b>",capability,"\n",
      "<b>Scenario: </b>",scenario,"\n",
      "<b>Sample Risk: </b>",
       as.character(round(sample_list$sample_p,2)),
       "\n",
       "<b>95% Conf Interval: </b>[",
       as.character(round(btest[4]$conf.int[1],2)),
       " , ",
       as.character(round(btest[4]$conf.int[2],2)),
       "]")
  }
  
  # return the list
  return(list("sampleProbs"=sampleProbs,
              "scenarioText"=scenarioText))
}

# Test the function
test <- getBinomTestConfIntervals(observations=dt,
                          scenario="Scenario X",
                          criticalValue=16) 

# Run this function for every scenario and every capability and store the results in a mega list

# Define three lists where the names are the capabilities
sampleProbs_criticalCape <- vector("list",length(dt[,unique(Scenario)]))
names(sampleProbs_criticalCape) <- dt[,unique(Scenario)]

text_criticalCape <- vector("list",length(dt[,unique(Scenario)]))
names(text_criticalCape) <- dt[,unique(Scenario)]

# Run function and store intervals
for (scen in dt[,unique(Scenario)]) {
  results <- getBinomTestConfIntervals(observations=dt,
                                       scenario=scen,
                                       criticalValue=16)
  sampleProbs_criticalCape[[scen]] <- results[["sampleProbs"]]
  text_criticalCape[[scen]] <- results[["scenarioText"]]
}

# visualize in a baller heat map!

# organize in a data.table
sampleProbs_criticalCape_dt <- as.data.table(sampleProbs_criticalCape)
sampleProbs_criticalCape_dt[,"Capability" := dt[,unique(Capability)]]

# Organize data
sampleProbs_criticalCape_dt_long <- melt(sampleProbs_criticalCape_dt,
                                         id.vars = "Capability",
                                         variable.name = "Scenario",
                                         value.name = "Risk")

# Hover text
hover_text <- as.data.table(text_criticalCape) %>% as.matrix()

# Heat map
criticalCape_plot <- plot_ly(
  data=sampleProbs_criticalCape_dt_long,
  colorscale="Viridis",
  type="heatmap",
  y=~Capability,
  x=~Scenario,
  z=~Risk,
  text=hover_text,
  hoverinfo="text",
  colorbar=list(title="<b>Sample Risk</b>")
) %>% 
  layout(
    title=list(
      text="<b>High Risk Capabilities</b>\n",
      pad=list(b = 500)),
    xaxis = list(side="top",tickangle=0,title=""),
    yaxis = list(title="")) %>%
plotly_build()

save(criticalCape_plot,file="products/criticalCape_plot.RData")

# Delete later
# ```{r, fig.width=775}
# load("products/criticalCape_plot.RData")
# criticalCape_plot
# ```

