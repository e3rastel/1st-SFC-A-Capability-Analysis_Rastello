
rm(list = ls())



# install.packages("htmlwidgets")
# install.packages("plotly",type="binary")
library(plotly)
library(dplyr)
library(data.table)
library(parallel)
library(htmlwidgets)

# Set the seed for the random number generator 
set.seed(42)

##### Load data 
dt <- fread("data/CBA Quantitative Analysis_filled_CSV_test_total.csv")

highRisk <- dt[,.(Proportion = round(sum(Score >= 16)/.N,2),
                  sampleSize = .N),
               by = .(Capability, Scenario)]

#### Get the 95% confidence intervals
highRisk[,"Low Interval" := Proportion - 1.96*(sqrt((Proportion*(1-Proportion))/sampleSize))]
highRisk[highRisk < 0] <- 0
highRisk[,"High Interval" := Proportion + 1.96*(sqrt((Proportion*(1-Proportion))/sampleSize))]
highRisk[,"Error Interval" := paste0("[",round(`Low Interval`,2),
                                     " ,",
                                     round(`High Interval`,2),
                                     "]","\n",
                                     "<b># of Votes: </b>",sampleSize)]


#### Order the capabilities by highest risk
# flatten the data.table against the four different scenarios
flat <- dcast(highRisk, Capability ~ Scenario, value.var = "Proportion")
flat[,"Avg" := rowMeans(.SD, na.rm=TRUE), .SDcols=c("Scenario 1",
                                                    "Scenario 2",
                                                    "Scenario 3",
                                                    "Scenario X")]
setorder(flat,Avg)

highRisk[,"Capability" := factor(highRisk[,Capability], levels = flat[,Capability])]

#### Heat Map
highRiskPlot <- plot_ly(
  data=highRisk,
  colorscale="Viridis",
  type="heatmap",
  y=~Capability,
  x=~Scenario,
  z=~Proportion,
  text=~`Error Interval`,
  hovertemplate=paste0("<b>Capabililty: </b>%{y}<br>",
                       "<b>Scenario: </b>%{x}<br>",
                       "<b>Proportion: </b>%{z}<br>",
                       "<b>Error Interval: </b>%{text}"),
  colorbar=list(title="<b>Proportion of Scores<br>Voted >= 16</b>",
                tickvals=seq(0,1,0.2),ticks="",
                ticktext=c("0%","20%","40%","60%","80%","100%"))
) %>% layout(
  title=list(text=""),
  xaxis = list(side="top",tickangle=0,title="",gridcolor="#333333"),
  yaxis = list(title="",gridcolor="#333333"),
  plot_bgcolor  = "#444444",
  paper_bgcolor = "#444444",
  font = list(color = '#FFFFFF'))

#### Save
# Save as html and as RData
htmlwidgets::saveWidget(highRiskPlot,
                        file="products/highRiskPlot.html",
                        selfcontained=TRUE)
save(highRiskPlot, file = "products/highRiskPlot.RData")

#### Repeat above for scores <=0

nonEssential <- dt[,.(Proportion = round(sum(Score <= 0)/.N,2),
                      sampleSize = .N),
                   by = .(Capability, Scenario)]

#### Get the 95% confidence intervals
nonEssential[,"Low Interval" := Proportion - 1.96*(sqrt((Proportion*(1-Proportion))/sampleSize))]
nonEssential[nonEssential < 0] <- 0
nonEssential[,"High Interval" := Proportion + 1.96*(sqrt((Proportion*(1-Proportion))/sampleSize))]
nonEssential[,"Error Interval" := paste0("[",round(`Low Interval`,2),
                                         " ,",
                                         round(`High Interval`,2),
                                         "]","\n",
                                         "<b># of Votes: </b>",sampleSize)]


#### Order the capabilities by highest risk
# flatten the data.table against the four different scenarios
flat <- dcast(nonEssential, Capability ~ Scenario, value.var = "Proportion")
flat[,"Avg" := rowMeans(.SD, na.rm=TRUE), .SDcols=c("Scenario 1",
                                                    "Scenario 2",
                                                    "Scenario 3",
                                                    "Scenario X")]
setorder(flat,-Avg)

nonEssential[,"Capability" := factor(nonEssential[,Capability], levels = flat[,Capability])]

#### Heat Map
nonEssentialPlot <- plot_ly(
  data=nonEssential,
  colorscale="Viridis",
  type="heatmap",
  y=~Capability,
  x=~Scenario,
  z=~Proportion,
  text=~`Error Interval`,
  hovertemplate=paste0("<b>Capabililty: </b>%{y}<br>",
                       "<b>Scenario: </b>%{x}<br>",
                       "<b>Proportion: </b>%{z}<br>",
                       "<b>Error Interval: </b>%{text}"),
  colorbar=list(title="<b>Proportion of Scores<br>Voted <= 0</b>",
                tickvals=seq(0,1,0.2),ticks="",
                ticktext=c("0%","20%","40%","60%","80%","100%"))
) %>% layout(
  title=list(text=""),
  xaxis = list(side="top",tickangle=0,title="",gridcolor="#333333"),
  yaxis = list(title="",gridcolor="#333333"),
  plot_bgcolor  = "#444444",
  paper_bgcolor = "#444444",
  font = list(color = '#FFFFFF'))

#### Save
# Save as html and as RData
htmlwidgets::saveWidget(nonEssentialPlot,
                        file="products/nonEssentialPlot.html",
                        selfcontained=TRUE)
save(nonEssentialPlot, file = "products/nonEssentialPlot.RData")





#### Take the average sample mean!

# Try for Scenario 1 and Capability Precision Messaging

# Get the sample mean

sampleMean <- dt[Scenario=="Scenario 1" & Capability=="TRIAD (TTP/Means/Ways/Concepts)",Score] %>%
  
  # Take a bootstrap sample
  sample(replace=TRUE) %>% 
  
  # then take the average of the sample
  mean()


# Try to use data.table

# Define the function
getAverageSampleMean <- function(obs) {
  library(data.table)
  sampled_data <- obs[, .(Bootstrap = sample(Score, replace = TRUE)), by = .(Capability, Scenario)]
  result <- sampled_data[, .(sampleMean = mean(Bootstrap)), by = .(Capability, Scenario)]
  return(result)
}

set.seed(42)

# Set up a cluster using available cores
cl <- makeCluster(detectCores() - 1)  # Leaving one core free

# Export the data and function to the cluster
clusterExport(cl, varlist = c("dt", "getAverageSampleMean"))

# Use parLapply to run the function in parallel
sampleMeanList <- parLapply(cl, 1:1e6, function(i) getAverageSampleMean(obs=dt))

# Stop the cluster
stopCluster(cl)

# Organize results by combining into a data.table
sampleMeansDataTable <- rbindlist(sampleMeanList)

# write the data.table so you don't have to run the bootstrap again
fwrite(sampleMeansDataTable, "data/sampleMeansDataTable.csv")

sampleMeansDataTable <- fread("data/sampleMeansDataTable.csv")


# Check for normality
sampleMeanVector <- sampleMeansDataTable[Capability=="Precision Messaging" & 
                                           Scenario=="Scenario 1",
                                         sampleMean]
plot_ly(x = sampleMeanVector,
        type = "histogram")
qqnorm(sampleMeanVector)
qqline(sampleMeanVector, col = "red")

# Take the average of the 1M sample means by scenario and capability and record the 95% CI
getConfInterval <- function(vector) {
  intervalText <- quantile(vector,c(.025,.975)) %>% round(2) %>% as.character() 
  return(paste0("[",intervalText[1],", ",intervalText[2],"]"))
}

# Test the function
getConfInterval(sampleMeanVector)

# Average the bootstrap sample means, and take the 95% confidence interval
averageDataTable <- sampleMeansDataTable[,.(averageSampleMean = round(mean(sampleMean),2),
                                            `Error Interval` = getConfInterval(sampleMean)),
                                         by = .(Capability, Scenario)] %>% 
  merge.data.table(dt[,.(sampleSize=.N), by = .(Capability, Scenario)])

averageDataTable[,"hoverText" := paste0(`Error Interval`,"\n",
                                        "<b># of Votes: </b>",sampleSize)]

# Order the Capabilities by highest average sample mean
# flatten the data.table against the four different scenarios
flat <- dcast(averageDataTable, Capability ~ Scenario, value.var = "averageSampleMean")
flat[,"Avg" := rowMeans(.SD, na.rm=TRUE), .SDcols=c("Scenario 1",
                                                    "Scenario 2",
                                                    "Scenario 3",
                                                    "Scenario X")]
setorder(flat,Avg)

averageDataTable[,"Capability" := factor(averageDataTable$Capability, 
                                         levels = flat$Capability)]

# Heatmap
averageSampleMeanPlot <- plot_ly(
  data=averageDataTable,
  colorscale="Viridis",
  type="heatmap",
  y=~Capability,
  x=~Scenario,
  z=~averageSampleMean,
  text=~hoverText,
  hovertemplate=paste0("<b>Capabililty: </b>%{y}<br>",
                       "<b>Scenario: </b>%{x}<br>",
                       "<b>Average Score: </b>%{z}<br>",
                       "<b>Error Interval: </b>%{text}"),
  colorbar=list(title="<b>Average Score</b>")
) %>% 
  layout(
    title=list(text=""),
    xaxis = list(side="top",tickangle=0,title="",gridcolor="#333333"),
    yaxis = list(title="",gridcolor="#333333"),
    plot_bgcolor  = "#444444",
    paper_bgcolor = "#444444",
    font = list(color = '#FFFFFF')
  )

# Save as html and as RData
htmlwidgets::saveWidget(averageSampleMeanPlot,
                        file="products/averageSampleMeanPlot.html",
                        selfcontained=TRUE)
save(averageSampleMeanPlot, file = "products/averageSampleMeanPlot.RData")










####### EDA Stuff 

# Test
testScores <- dt[Capability=="Mission Command Culture" & Scenario=="Scenario 3",Score]
sum(testScores >= 16)/length(testScores)

binom.test(x=sum(testScores >= 16),
           n=length(testScores),
           p=sum(testScores >= 16)/length(testScores))

(testScores >= 16)/length(testScores) + sqrt(sum(testScores >= 16)/length(testScores)(testScores))

# Define function
getBinomTestConfIntervals <- function(observations,scenario,criticalValue,verbose=FALSE) {
  
  # Make sure the critcalValue is 16 or 0
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
        sample_x=sum(scores >= criticalValue),
        sample_n=length(scores),
        sample_p=sum(scores >= criticalValue)/length(scores)
      )
    } else {
      sample_list <- list(
        sample_x=sum(scores <= criticalValue),
        sample_n=length(scores),
        sample_p=sum(scores <= criticalValue)/length(scores)
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
      "]","\n",
      "<b>Sample Size: </b>",sample_list$sample_n)
  }
  
  # return the list
  return(list("sampleProbs"=sampleProbs,
              "scenarioText"=scenarioText))
}

# Test the function
test <- getBinomTestConfIntervals(observations=dt,
                                  scenario="Scenario X",
                                  criticalValue=16) 

test2 <- getBinomTestConfIntervals(observations=dt,
                                   scenario="Scenario 1",
                                   criticalValue=0) 

# Run this function for every scenario and every capability and store the results in a mega list

# Define two lists where the names are the capabilities
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
    yaxis = list(title="")) 

criticalCape_plot 

# Save as html and as RData
htmlwidgets::saveWidget(criticalCape_plot,
                        file="products/criticalCape_plot.html",
                        selfcontained=TRUE)
save(criticalCape_plot, file = "products/criticalCape_plot.RData")


### Build the same plot for Nonessential Capabilities

# Define two lists where the names are the capabilities
sampleProbs_nonEssentialCape <- vector("list",length(dt[,unique(Scenario)]))
names(sampleProbs_nonEssentialCape) <- dt[,unique(Scenario)]

text_nonEssentialCape <- vector("list",length(dt[,unique(Scenario)]))
names(text_nonEssentialCape) <- dt[,unique(Scenario)]

# Run function and store intervals
for (scen in dt[,unique(Scenario)]) {
  results <- getBinomTestConfIntervals(observations=dt,
                                       scenario=scen,
                                       criticalValue=0)
  sampleProbs_nonEssentialCape[[scen]] <- results[["sampleProbs"]]
  text_nonEssentialCape[[scen]] <- results[["scenarioText"]]
}

# visualize in a baller heat map!

# organize in a data.table
sampleProbs_nonEssentialCape_dt <- as.data.table(sampleProbs_nonEssentialCape)
sampleProbs_nonEssentialCape_dt[,"Capability" := dt[,unique(Capability)]]

# Organize data
sampleProbs_nonEssentialCape_dt_long <- melt(sampleProbs_nonEssentialCape_dt,
                                             id.vars = "Capability",
                                             variable.name = "Scenario",
                                             value.name = "Risk")

# Hover text
hover_text <- as.data.table(text_nonEssentialCape) %>% as.matrix()

# Heat map
nonEssentialCape_plot <- plot_ly(
  data=sampleProbs_nonEssentialCape_dt_long,
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
      text="<b>Non-Essential Capabilities</b>\n",
      pad=list(b = 500)),
    xaxis = list(side="top",tickangle=0,title=""),
    yaxis = list(title="")) 

nonEssentialCape_plot

# Save as html and as RData
htmlwidgets::saveWidget(nonEssentialCape_plot,
                        file="products/nonEssentialCape_plot.html",
                        selfcontained=TRUE)
save(nonEssentialCape_plot, file = "products/nonEssentialCape_plot.RData")


# Test results to make sure they are correct
dt[Capability=="UAS (TTP/Means/Ways/Concepts)"]
