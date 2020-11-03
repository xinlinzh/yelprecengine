#########################################
# Install Packages
#########################################
rm(list=ls()) 
list.of.packages <- c("rjson", 
                      "jsonlite", 
                      "dplyr", 
                      "recommenderlab", 
                      "arules", 
                      "tidyverse", 
                      "sqldf",
                      "RSQLite",
                      "Matrix",
                      "ggplot2",
                      "ggthemes",
                      "scales",
                      "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#########################################
# Load Libraries + Set Working Directory 
#########################################

library(rjson)
library(jsonlite)
library(dplyr)
library(recommenderlab)
library(arules)
library(tidyverse)
library(sqldf)
library(RSQLite)
library(Matrix)
library(ggplot2)
library(ggthemes)
library(scales)
library(geosphere)

# set working dir
setwd(getwd())

#########################################
# Reviews Table
#########################################

# read in reviews csv
reviews = read.csv("yelp_reviews_Phoenix.csv")

#extract only relevant columns and turn business/user to numerical ids
review.short = as.data.frame(data.matrix(reviews[,1:3]))
review.short$business_id = as.numeric(review.short$business_id)


#########################################
# Business Table
#########################################

business = read.csv("yelp_business_Phoenix.csv")

x = as.data.frame(t(sapply(strsplit(as.character(business$categories), ", "), "[", 1:10)))

business = cbind(business[,1:10],x)

business$id =data.matrix(business)[,1]

#########################################
# Recommender Model Function
#########################################
model_evaluation <- function(df){
  # set seed
  set.seed(12345)
  
  # disable scientific notation
  options(scipen=999)
  
  # convert to realRatingMatrix
  reviews_rrm <- as(df, "realRatingMatrix")
  
  # Create a list of models
  models = c(
    # no parameters
    "RANDOM",
    "ALS",
    "SVD",
    "UBCF",
    "IBCF",
    # SVD, UBCF, IBCF normalizing
    "SVD",
    "UBCF",
    "IBCF",
    # UBCF, IBCF Euclidean
    "UBCF",
    "IBCF",
    # UBCF, IBCF pearson
    "UBCF",
    "IBCF",
    # UBCF, IBCF cosine
    "UBCF",
    "IBCF",
    # UBCF, IBCF jaccard
    "UBCF",
    "IBCF",
    # IBCF Euclidean / k
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    # IBCF pearson / k
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    # IBCF  cosine/ k
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    # IBCF jaccard / k
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    "IBCF",
    # UBCF Euclidean / nn
    "UBCF",
    "UBCF",
    "UBCF",
    "UBCF",
    # UBCF pearson / nn
    "UBCF",
    "UBCF",
    "UBCF",
    "UBCF",
    # UBCF cosine/ nn
    "UBCF",
    "UBCF",
    "UBCF",
    "UBCF",
    # UBCF jaccard / nn
    "UBCF",
    "UBCF",
    "UBCF",
    "UBCF",
    # SVD / normalize / k
    "SVD",
    "SVD",
    "SVD",
    # SVDF / normalize / k 
    "SVDF",
    "SVDF",
    "SVDF",
    # ALS / normalize / lambda
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    # ALS / n_factors
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    "ALS",
    # LIBMF
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF",
    "LIBMF"
  )

  # Create a list of parameters 
  params = list(
    # no parameters
    "NA", 
    "NA", 
    "NA", 
    "NA",
    "NA",
    # SVD, UBCF, IBCF normalizing
    list(normalize= "Z-score"),
    list(normalize= "Z-score"),
    list(normalize= "Z-score"),
    # UBCF, IBCF Euclidean
    list(normalize= "Z-score", method = "Euclidean"),
    list(normalize= "Z-score", method = "Euclidean"),
    # UBCF, IBCF pearson
    list(normalize= "Z-score", method = "pearson"),
    list(normalize= "Z-score", method = "pearson"),
    # UBCF, IBCF cosine
    list(normalize= "Z-score", method = "cosine"),
    list(normalize= "Z-score", method = "cosine"),
    # UBCF, IBCF jaccard
    list(normalize= "Z-score", method = "jaccard"),
    list(normalize= "Z-score", method = "jaccard"),
    # IBCF Euclidean / k
    list(normalize= "Z-score", method = "Euclidean", k = 10),
    list(normalize= "Z-score", method = "Euclidean", k = 20),
    list(normalize= "Z-score", method = "Euclidean", k = 30),
    list(normalize= "Z-score", method = "Euclidean", k = 40),
    list(normalize= "Z-score", method = "Euclidean", k = 60),
    list(normalize= "Z-score", method = "Euclidean", k = 80),
    # IBCF pearson / k
    list(normalize= "Z-score", method = "pearson", k = 10),
    list(normalize= "Z-score", method = "pearson", k = 20),
    list(normalize= "Z-score", method = "pearson", k = 30),
    list(normalize= "Z-score", method = "pearson", k = 40),
    list(normalize= "Z-score", method = "pearson", k = 60),
    list(normalize= "Z-score", method = "pearson", k = 80),
    # IBCF cosine / k
    list(normalize= "Z-score", method = "cosine", k = 10),
    list(normalize= "Z-score", method = "cosine", k = 20),
    list(normalize= "Z-score", method = "cosine", k = 30),
    list(normalize= "Z-score", method = "cosine", k = 40),
    list(normalize= "Z-score", method = "cosine", k = 60),
    list(normalize= "Z-score", method = "cosine", k = 80),
    # IBCF jaccard / k
    list(normalize= "Z-score", method = "jaccard", k = 10),
    list(normalize= "Z-score", method = "jaccard", k = 20),
    list(normalize= "Z-score", method = "jaccard", k = 30),
    list(normalize= "Z-score", method = "jaccard", k = 40),
    list(normalize= "Z-score", method = "jaccard", k = 60),
    list(normalize= "Z-score", method = "jaccard", k = 80),
    # UBCF Euclidean / nn
    list(normalize= "Z-score", method = "Euclidean", nn = 10),
    list(normalize= "Z-score", method = "Euclidean", nn = 20),
    list(normalize= "Z-score", method = "Euclidean", nn = 30),
    list(normalize= "Z-score", method = "Euclidean", nn = 40),
    # UBCF pearson / nn
    list(normalize= "Z-score", method = "pearson", nn = 10),
    list(normalize= "Z-score", method = "pearson", nn = 20),
    list(normalize= "Z-score", method = "pearson", nn = 30),
    list(normalize= "Z-score", method = "pearson", nn = 40),
    # UBCF cosine / nn
    list(normalize= "Z-score", method = "cosine", nn = 10),
    list(normalize= "Z-score", method = "cosine", nn = 20),
    list(normalize= "Z-score", method = "cosine", nn = 30),
    list(normalize= "Z-score", method = "cosine", nn = 40),
    # UBCF jaccard / nn
    list(normalize= "Z-score", method = "jaccard", nn = 10),
    list(normalize= "Z-score", method = "jaccard", nn = 20),
    list(normalize= "Z-score", method = "jaccard", nn = 30),
    list(normalize= "Z-score", method = "jaccard", nn = 40),
    # SVD  normalize / k
    list(normalize= "Z-score", k = 10),
    list(normalize= "Z-score", k = 15),
    list(normalize= "Z-score", k = 20),
    # SVDF / normalize / k 
    list(normalize= "Z-score", k = 10),
    list(normalize= "Z-score", k = 15),
    list(normalize= "Z-score", k = 20),
    # ALS / lambda
    list(lambda = 0.03),
    list(lambda = 0.04),
    list(lambda = 0.05),
    list(lambda = 0.07),
    list(lambda = 0.08),
    list(lambda = 0.01),
    # ALS / n_factors
    list(n_factors = 6),
    list(n_factors = 8),
    list(n_factors = 10),
    list(n_factors = 12),
    list(n_factors = 14),
    list(n_factors = 16),
    # LIBMF
    list(dim = 10, costp_l2 = 0.01, costq_l2 = 0.01),
    list(dim = 10, costp_l2 = 0.05, costq_l2 = 0.01),
    list(dim = 10, costp_l2 = 0.01, costq_l2 = 0.05),
    list(dim = 20, costp_l2 = 0.01, costq_l2 = 0.01),
    list(dim = 20, costp_l2 = 0.05, costq_l2 = 0.01),
    list(dim = 20, costp_l2 = 0.01, costq_l2 = 0.05),
    list(dim = 30, costp_l2 = 0.01, costq_l2 = 0.01),
    list(dim = 30, costp_l2 = 0.05, costq_l2 = 0.01),
    list(dim = 30, costp_l2 = 0.01, costq_l2 = 0.05)
  )
  
  # Vector to iterate over 
  model.len = 1:length(models)
  
  # create empty dataframe to store results
  results = data.frame("Model" = as.vector(models), 
                       "Parameters" = character(length(model.len)), 
                       "RMSE"= numeric(length(model.len)),
                       "MSE"= numeric(length(model.len))
  )
  results$Parameters = as.character(results$Parameters)
  
  #add progress bar
  pb <- txtProgressBar(min = 0, max = max(model.len), style = 3)
  
  # start for loop
  for (j in model.len){
    
    # define model and params
    model_algo = models[j]
    #print(models[j])
    parameter = params[[j]]
    #print(params[[j]])
    param.paste = paste(parameter, collapse=' ')
    
    # split data train & test
    data.split <- evaluationScheme(reviews_rrm, 
                                   method = "cross-validation",      
                                   k = 10,  
                                   given = 5,
                                   goodRating = 4
    )
    # Build Model
    if (parameter == "NA") {
      model = Recommender(recommenderlab::getData(data.split, "train"), 
                          model_algo) 
    }
    
    if (parameter != "NA") {
      model = Recommender(recommenderlab::getData(data.split, "train"), 
                          model_algo,
                          param = parameter)
    }
    
    
    # Make predictions using model 
    pred = recommenderlab::predict(model, 
                                   recommenderlab::getData(data.split, "known"), 
                                   type = "ratings") 
    
    # Set all predictions that fall outside the valid range to the boundary values
    pred@data@x[pred@data@x[] < 0] = 0
    pred@data@x[pred@data@x[] > 5] = 5
    
    # Round into integers (not necessary)
    #pred@data@x = round(pred@data@x, 0)
    
    # RMSE of The Model 
    pred.error = recommenderlab::calcPredictionAccuracy(pred, 
                                                        recommenderlab::getData(data.split, "unknown"))
    
    
    # Add error into results dataframe
    results[j,2] <- param.paste
    results[j,3] <- pred.error[1]
    results[j,4] <- pred.error[2]
    
    # Adjust progress bar
    setTxtProgressBar(pb, j)
    
    # End loop
  }
  
  # Close progress bar 
  close(pb)
  
  # Order dataframe by error
  results = results[order(results$RMSE),]
  
  # Only select models which outperform "random"
  model.analysis = results
  random.rmse = model.analysis[model.analysis$Model == 'RANDOM',][[3]]
  random.row = model.analysis[model.analysis$Model == 'RANDOM',]
  model.analysis = subset(model.analysis, RMSE <= random.rmse)
  model.analysis = head(model.analysis, 15)
  model.analysis = rbind(model.analysis, random.row)
  
  # Re-organize columns
  model.analysis$model = paste(model.analysis$Model, "(", model.analysis$Parameters, ")")
  model.analysis = model.analysis[,c("model", "RMSE")] 
  colnames(model.analysis) = c("Model","RMSE")
  
  # min/max RMSE
  min.rmse = min(model.analysis$RMSE, na.rm = TRUE)
  min.rmse = min.rmse - 0.1
  
  max.rmse = max(model.analysis$RMSE, na.rm = TRUE)
  max.rmse = max.rmse + 0.2
  
  # Plot
  print(
    ggplot(data=model.analysis, aes(x = reorder(Model, -RMSE, sum), y = RMSE)) +
      theme_gdocs() +
      geom_bar(position="dodge", stat="identity", fill = "#D22323") + 
      coord_flip() +
      ggtitle("Model RMSE") +
      ylab("RMSE") +
      xlab("Model + Parameters") +
      scale_y_continuous(limits = c(min.rmse, max.rmse) , oob = rescale_none) +
      geom_text(aes(label= round(RMSE, 3))) 
    
  )
  
  # Return
  print(results)
  return(results)
  
}

#########################################
# Main Product Function
#########################################
YELP_ENGINE = function(){
  cat("This engine generates a pool of target customers for your marketing materials 
      by extracting information from businesses similar to yours.\n\n")
  
  # extract client business info
  cat("*** Business ID range: 1~15473. \n 
*** To test for our demo businesses, please enter 1000 or 14223.\n\n")
  bus_id = as.numeric(readline(prompt="Enter Your Business ID: "))
  entry = business[business$id == bus_id,]
  cat("Your business is", as.character(entry$name),"\n")
  bus_lat = entry$latitude
  bus_lon = entry$longitude
  char =as.vector(business[business$id == bus_id,11:20])
  char = char[!is.na(char)] 
  # print business categories
  cat("Your business has the following categories:\n", char, "\n\n")
  
  # see if the client has a limit on # of customers being targeted
  targ.limit = as.numeric(readline(prompt="What's maximum number of customers you want to reach:"))
  
  # looking for similar businesses
  cat("First, we find similar businesses.\n\n")
  
  # asking if there's geographical limit
  if (readline(prompt = "Do you prefer to locate similar businesses within a certain radious? (Y/N)") == "Y"){
    dist = as.numeric(readline(prompt = "Enter your preferred distance in miles:"))
    sim_busi_dist = data.frame()
    print("Calculating distances...")
    # add progress bar
    pb <- txtProgressBar(min = 0, max = nrow(business), style = 3)
    for (i in 1:nrow(business)){
      lon2 = business$longitude[i]
      lat2 = business$latitude[i]
      # calculate the distance between businesses and convert meter to miles
      dis = (distm (c(bus_lon, bus_lat), c(lon2, lat2), fun = distHaversine))/1609  
      # only keep the ones that are within the input radius
      if (dis < dist) {
        sim_busi_dist = rbind(sim_busi_dist,business[i,])
      }
      setTxtProgressBar(pb, i)}
    close(pb)
    cat("There are", nrow(sim_busi_dist), "businesses left after filtering for distances.\n")
  } else{sim_busi_dist = business} # if not limiting distance, take the original business df
  
  cat("Filtering businesses on categories...\n\n")
  # select similar businesses across all categories of target business, 
  # while checking if the number of customer has exceed client's desired limit, 
  # or if the result dataset is too big for a predicting model
  sim_busi_cate = data.frame()
  for (i in 1:length(char)){
      sim_busi_cate = rbind(sim_busi_cate,
                            rbind(sim_busi_dist[grep(char[i], sim_busi_dist$V1), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V2), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V3), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V4), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V5), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V6), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V7), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V8), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V9), ],
                                  sim_busi_dist[grep(char[i], sim_busi_dist$V10), ]))
      n = nrow(sim_busi_cate)
      cat("Filter on:", char[i],"\n # of Businesses:", n, "\n")
      # eliminate duplicates
      sim_busi_cate = unique(sim_busi_cate)
      cat("Extracting relevant reviews...\n")
      
      # extract relevant reviews of these businesses
      targ.customer = data.frame()
      pb <- txtProgressBar(min = 0, max = nrow(sim_busi_cate), style = 3)
      for (row in 1:nrow(sim_busi_cate)){
        targ.customer = rbind(targ.customer, review.short[review.short$business_id == sim_busi_cate$id[row],])
        setTxtProgressBar(pb, row)
      }
      close(pb)
      # eliminate duplicate reviews
      targ.customer = unique(targ.customer)
      # this pool of customers contains all that falls into client's initial segment (location, number)
      first.level.targ.customer = unique(targ.customer$user_id)
      
      # Check if the number of target customer exceed limit OR if the dataset is too large
      # If so, filter for users and business who only reviews more
      decide = (length(first.level.targ.customer ) > targ.limit || nrow(targ.customer) > 40000)
      if (decide){
        cat("\n")
        cat("After filtering for", i, "category, you have", length(first.level.targ.customer), "customers to target.\n", length(unique(targ.customer$business_id)), "similar businesses are found.\n\n")
        cat("Optimize the current review dataset for prediction...\n\n")
        for (review.limit in c(10,15,20)){
          # make sure client business is not filtered out
          if (bus_id %in% targ.customer$business_id){
            if (nrow(targ.customer) > 40000){
              targ.customer <- targ.customer %>%
                group_by(user_id) %>%
                filter(n() > review.limit) %>%
                ungroup() 
              targ.customer <- targ.customer %>%
                group_by(business_id) %>%
                filter(n() > review.limit) %>%
                ungroup()
              print(paste("Reviews after taking only more than", review.limit, "ratings:", nrow(targ.customer)))
              }
          }}
        # this customer pool will contain only customers who rate a lot
        second.level.targ.customer = unique(targ.customer$user_id)
        break
      }
  }
  targ.customer <- as.data.frame(targ.customer)
  targ.customer$review_stars = as.numeric(targ.customer$review_stars)
  # rearrange column order so that when transform to realRatingMatrix users are on the row
  targ.customer = targ.customer[,c(2,1,3)]
  
  # print out summary 
  cat("There are", length(unique(targ.customer$business_id)), "similar businesses left after all.\n")
  cat("There are", length(second.level.targ.customer), "targeting audience for your marketing campaign.\n")
  cat("They have in total", nrow(targ.customer), "reviews written for your business and similar businesses.\n\n")
  
  #prompt for recommender system
  cat("This might take a long time -> ")
  recommender.decision = readline("Do you want to see what are the predicted ratings? (Y/N)")
  if (recommender.decision == "Y"){
    # run recommender evaluation function to find the optimal model
    model.errors = try(model_evaluation(targ.customer))
    # the most common error is when size < given, which means the optimized dataset still doesn't have good enough data
    # therefore, simply printing out there is not enough data to make predictions
    if("try-error" %in% class(model.errors)) {
      predict.rating = NULL
      cat("\n")
      cat("There is not enough data to make rating predictions.\n\n")
      } else{
        # take the optimal model from the above evaluation
        model_algo = as.character(model.errors[which.min(model.errors$RMSE),1])
        parameter = as.character(model.errors[which.min(model.errors$RMSE),2])
        
        # convert to realRatingMatrix
        reviews_rrm <- as(targ.customer, "realRatingMatrix")
        set.seed(12345)
        data.split <- evaluationScheme(reviews_rrm, 
                                       method = "cross-validation",      
                                       k = 10,  
                                       given = 5,
                                       goodRating = 4)
        if (parameter == "NA") {
          model = Recommender(recommenderlab::getData(data.split, "train"), 
                              model_algo)}
        if (parameter != "NA") {
          model = Recommender(recommenderlab::getData(data.split, "train"), 
                              model_algo,
                              param = parameter)}
        pred = recommenderlab::predict(model, 
                                       reviews_rrm, 
                                       type = "ratings") 
        # Set all predictions that fall outside the valid range to the boundary values
        pred@data@x[pred@data@x[] < 0] = 0
        pred@data@x[pred@data@x[] > 5] = 5
        
        predict.rating = as.data.frame(as(pred, "matrix")[,as.character(bus_id)])
        cat("There are",
            sum(predict.rating$`as(pred, "matrix")[, as.character(bus_id)]`>4, na.rm = TRUE),
            "customers who will rate your business more than 4 stars.")
      }
  }
  # return client business id, similar businesses found, 
  # first pool of target customer (before selecting high reviews customers) and second pool of target customer,
  # the whole review dataset for recommender model and the predicted ratings
  cat("Finished exporting recommended customers and other relevant information.")
  return(list(bus_id, sim_busi_cate, 
              first.level.targ.customer, second.level.targ.customer, 
              targ.customer, predict.rating))
}

result = YELP_ENGINE()

client.id = result[[1]]
sim.business = result[[2]]
first.targ.customer = result[[3]]
second.targ.customer = result[[4]]
targ.reviews = result[[5]] 
predict.rating = result[[6]]

###########################################################################################################
################################## this is th end of marketing engine #####################################
###########################################################################################################



