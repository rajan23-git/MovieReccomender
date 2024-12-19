library("arrow")
library("dplyr")
#Main Purpose
#has the user input parameters such as startYear, runtime, rating, and numVotes
#the user also input a selected list of genres to filter by
#the script filters the movies by the genres, and builds a hierarchical model on this filtered data
#this hierarchical model is used to reccomend a movie, that closely resembles the input paramters for startYear,runtime,rating, and numVotes


#this function reccomends a movie, given the user input parameters for startYear,runtime,rating,numVotes
reccomendMovie <- function(genre_group,input_movie,searched_genres){
  
  Data = genre_group[,c(6,8,10,11)]
  sample_size = 1000
  #performs clusering on sample of 1000, but if sample size exceeds num of data points
  #sample size set to num of data points
  if(sample_size > nrow(genre_group)){
    sample_size = nrow(genre_group)
  }
  sample_index = sample(nrow(genre_group),sample_size)
  Data = Data[sample_index,]
  
  Labeled_Data = genre_group[sample_index,]
  
  Data = sapply(Data, as.numeric)
  
  Data = scale(Data)
  movie_point = scale(input_movie)
  #hierarchial clustering performed with distance matrix
  #and complete linkage
  heir_clust.model = hclust(dist(Data),method = "complete")
  title = paste("Clusters for Genre = ",toString(searched_genres))
  plot(heir_clust.model,  main=title)
  rect.hclust(heir_clust.model, k = 7, border = 2:6)
  #clustering tree cut at 7 clusters
  heir_clust.clusters = cutree(heir_clust.model,7)
  #this for loop calculates the distance from the given movie parameters
  #to the centers of each cluster
  #creates an array of distances out of these, to the cluster centers
  distance_array = c()
  for(i in 1:7){
    mean_x = mean(Data[(heir_clust.clusters == i),1])
    mean_y = mean(Data[(heir_clust.clusters == i),2])
    mean_z = mean(Data[(heir_clust.clusters == i),3])
    mean_a = mean(Data[(heir_clust.clusters == i),4])
    cluster_center = list(mean_x,mean_y,mean_z,mean_a)
    cluster_center = sapply(cluster_center,as.numeric)
    
    distance = sqrt(sum((movie_point - cluster_center)^2))
    distance_array = append(distance_array,distance)
    
  }
  #finds the cluster whose center is closest to the given parameters
  assigned_cluster = which(distance_array == min(distance_array))[1]
  matching_cluster = Labeled_Data[heir_clust.clusters == assigned_cluster,]
  matching_cluster_data = Data[heir_clust.clusters == assigned_cluster,]
  dist_pointsin_cluster = c()
  #iterates through all the points in the closest cluster
  #finds the distance from the given parameters to each point within the closest cluster
  for(i in nrow(matching_cluster_data)){
    dist = sqrt(sum((movie_point - matching_cluster_data[i,])^2))
    dist_pointsin_cluster = append(dist_pointsin_cluster,dist)
  }
  #finds point within the closest cluster, that is closest to the input parameters
  #this closest point becomes the reccomended movie
  index_closest_point = which(dist_pointsin_cluster == min(dist_pointsin_cluster))[1]
  reccomended_movie = matching_cluster[index_closest_point,]
  return (reccomended_movie)
}

#filters all the movies by a given set of genres
filterByGenre <- function(searched_genres){
  filtered = rep(TRUE,nrow(movie_dataset))
  for(i in 1:length(searched_genres)){
    filtered = filtered & grepl(searched_genres[i],movie_dataset$genres)
  }
  filtered_genre = movie_dataset[filtered,]
  return (filtered_genre)
}




unique(basics_ratings$titleType)
#gets only the movies from the dataframe that contains movies, tv shows, short movies 
only_movies = basics_ratings$titleType == "movie" 
movie_dataset = basics_ratings[only_movies,]

#getting movie data without n/a values
omitted_values = !is.na(movie_dataset$runtimeMinutes)
omitted_values = omitted_values & (!is.na(movie_dataset$startYear))
omitted_values = omitted_values & (!is.na(movie_dataset$averageRating))
omitted_values = omitted_values & (!is.na(movie_dataset$numVotes))
omitted_values = omitted_values & (!is.na(movie_dataset$genres))

movie_dataset = movie_dataset[omitted_values,]

#converting the columns of the movie dataset to numeric data, in preparation
#for heirarchical clustering
movie_dataset$startYear<- as.numeric(movie_dataset$startYear)
movie_dataset$runtimeMinutes<- as.numeric(movie_dataset$runtimeMinutes)
movie_dataset$averageRating<- as.numeric(movie_dataset$averageRating)
movie_dataset$numVotes<- as.numeric(movie_dataset$numVotes)

#list of all genres that occur in the movie dataset
all_genres = c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama","Family","Fantasy","Film-Noir","History","Horror","Musical","Mystery","Romance","Sci-Fi","Sport","Thriller","War","Western")


library(shiny)

# Define UI for application that plots hierchical clustering dendogram
#and shows table for reccomended movie
ui <- fluidPage(
  
  # Application title
  titlePanel("Hierarchical Clustering Within Genre"),
  
  sidebarLayout(
    sidebarPanel(
      #slider input for start year
      sliderInput("start_year",
                 "Start Year:",
                 min = min(movie_dataset$startYear),
              max = max(movie_dataset$startYear),
                 value = mean(movie_dataset$startYear))
    ,
    #slider input for runtime
    sliderInput("runtime",
                "Runtime in Minutes",
                min = min(movie_dataset$runtimeMinutes),
                max = 300,
                value =150)
  ,
  #slider input for average Movie Rating
  sliderInput("average_rating",
              "Average rating:",
              min = min(movie_dataset$averageRating),
              max = max(movie_dataset$averageRating),
              value =mean(movie_dataset$averageRating))
,
#slider input for number of votes
sliderInput("num_votes",
            "Number of Votes:",
            min = min(movie_dataset$numVotes),
            max = max(movie_dataset$numVotes),
            value =mean(movie_dataset$numVotes) ),
#selector for which genres to filter by
selectInput(
  "genre_select",
  "Select The Genre To Cluster",
  all_genres,
  selected = c("Action"),
  multiple = TRUE,
  selectize = TRUE,
  width = NULL,
  size = NULL
)


),

mainPanel(
  plotOutput("Hclust"),titlePanel("The Reccomended Movie"),tableOutput("Table")
)

)

)


# Define server logic to create Dendogram for heirarchical clustering
server <- function(input, output) {

  output$Hclust <- renderPlot({
    #given input parameters for startYear,runtime,rating,and numVotes
    #reccomends movie that is close to these parameters
    given_movie = c(input$start_year,input$runtime,input$average_rating,input$num_votes)
    print(input$genre_select)
    #filters movie dataset with selected genres
    movies_filtered = filterByGenre(input$genre_select)
    # reccomends movie that is closest to the given movie parameters
    # reccomendation is perfomed by creating heirarchical model on genre filtered data
    #this model that was built from genre filtered data, is then used to reccomend movies
    rec_movie = reccomendMovie(movies_filtered,given_movie,input$genre_select)
  })

  #repeats the movie reccomendation from before with exact same parameters 
  #and exact same heirarchical model, and renders reccomended movie in table format
  output$Table<- renderTable({
    given_movie = c(input$start_year,input$runtime,input$average_rating,input$num_votes)
    print(input$genre_select)
    movies_filtered = filterByGenre(input$genre_select)
    rec_movie = reccomendMovie(movies_filtered,given_movie,input$genre_select)
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


