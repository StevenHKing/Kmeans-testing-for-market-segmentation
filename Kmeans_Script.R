
#### Steven King


# THE BASICS -----

setwd("/Users/StevenKing/Desktop/MGT_100/week_2_data)

getwd()

# Today we look at segmentation via the kmeans algorithm.

    library(tidyverse)
    
    cust_dat <- read_csv("smartphone_customer_data.csv")

    
## We're going to focus on customers' time spent gaming and handsize
    
    # Let's subset the data to these two dimensions
    
    sub <- cust_dat |> select(gaming, handsize)
    sub
    
    # and plot them 
    
    ggplot(sub) +
        geom_point(aes(gaming, handsize)) + 
        theme_minimal()

 #### Sure, we got a basic scatterplot. But we should be able to group and segment further.
    
# Let's run a clustering algorithm (kmeans) on these data
    
    # kmeans performs clustering based on distances.  We can get different distance
    # results if we, e.g., measure handsize in inches or millimeters. So first 
    # we standardize the data -- that is, we scale each variable to have a mean
    # of zero and a standard deviation of one
    
    # note that scale() returns a matrix, so we use as_tibble() to convert the 
    # output of scale() into a data.frame/tibble
    
    scl <- sub |> scale() |> as_tibble()
    scl
    
    # let's check that the scaling worked
    #### It worked. 
    
    scl |> summarize_all(mean) |> round(3) # check means
    scl |> summarize_all(sd)                # check std devs
    
    # Now when we run the kmeans clusering algorithm, R handles all of
    # hard stuff for us!  All we need to do is call the kmeans() function
    # and specify the "k" (ie, the number of clusters).  We do that with the "centers" 
    # argument. Suppose we believe there are 4 clusters in these data.
    
    # We can also optionally do more than 1 start by specifying the 'nstart' argument.
    # This is a good idea, since it helps to ensure that the kmeans algorithm finds
    # the global optimum, and not a local optimum.  
    
    # Let's save the output from the kmeans() function into an object named "out"
    
    out <- kmeans(scl, centers=4, nstart=25)
    
    # congratulations! You just ran a fancy algorithm. You're pretty much a data scientist now :)
    
    # to tie this code to the slides, We're going to use K=4 and D=25 instead of hard-
    # coding the numbers into the kmeans() function directly.  We'll also set the starting
    # point for the random number generator using the set.seed() function.
    
    K <- 4
    D <- 25
    
    set.seed(1234)
    out <- kmeans(scl, centers=K, nstart=D)
    
    # "out" is a list.  This is common with model-fitting functions in R.  To get
    # a better sense for what's included in "out", let's run the structure function:
    
    str(out)
    
    # We see that out$cluster is a vector of cluster-membership, i.e., a vector of 
    # length n that tells us which of the k clusters each of our n data points belongs to.
    
    # I want to take a moment to talk about how to "access" or "extract" these list elements:
    
        # 3 ways to extract a list element -- $ and [[]] return the element
        str(out$cluster)
        str(out[["cluster"]])
        str(out[[1]])
        
        # 2 ways with single brackets [] to subset a list into a one-element list (usually not what you want)
        str(out["cluster"])
        str(out[1])
    
    # We can also see that out$centers is a k-by-J matrix with the coordinates of the
    # clusters' centers
        
        str(out$centers)
        out$centers
    
    # We can use these elements of "out" to enhance our plot:
    
    # First, let's grab the cluster membership as a variable and add it to our 
    # dataset as a factor/categorical variable
    sub <- sub |> mutate(cluster = factor(out$cluster))
    
    # Let's check our work by comparing a count from our data to the kmeans() 'size' output
    sub |> count(cluster)
    out$size
    
    # Second, these clusters are in units of scaled data. In order to add them to our  
    # plot, we'll need to "unscale" them. To do that, we multiply by the variables'  
    # standard deviations and then add their means. I made a function that will do this.  
    # Just supply the centers as "c", the data as "d", and the number of clusters as "K".
    
    unscale <- function(c, d, K=nrow(c)) {
        # get varnames
        vars <- colnames(c)
    
        # calculate mean and sd
        SD   <- d |> select(all_of(vars)) |> summarize_all(sd)
        MEAN <- d |> select(all_of(vars)) |> summarize_all(mean)
        
        # repeat/format the values so we can do matrix math
        SD   <- SD   |> unlist() |> rep(K) |> matrix(nrow=K, ncol=2, byrow=T)
        MEAN <- MEAN |> unlist() |> rep(K) |> matrix(nrow=K, ncol=2, byrow=T)
        
        # unscale the centers (convert back into original units)
        return(as_tibble(c*SD + MEAN))
    }
    
    centers <- unscale(c=out$centers, d=sub, K)
    round(centers, 1)
    
    # Then we plot the points (colored by cluster membership) and the cluster centers
    ggplot() + 
        geom_point(data=sub,     aes(x=gaming, y=handsize, color=cluster)) + 
        geom_point(data=centers, aes(x=gaming, y=handsize), size=4) + 
        ggtitle("Kmeans cluster membership and centroids") + 
        theme_minimal()
    
    # What do we think of this result?
    
    #### This result shows that customers can be grouped into four clear clusters 
    #### based on their gaming and hand size, and each color is a different group with similar traits.
    
    # How useful is this segmentation?
    
    #### The segmentation is fairly useful, because it helps us identify different customer types,
#### however it’s somewhat limited given we’re only using the two variables gaming and hand size,
#### and that might not fully describe the real differences between customers.

# NICE EXAMPLE -----
    
# Example with "iris" data
    
    # load data
    data(iris)
    str(iris)
    
    # plot petal length vs petal width by species
    ggplot(iris) + 
        geom_point(aes(x=Petal.Length, y=Petal.Width, col=Species))
    
    # run kmeans
    out_iris <- iris |> 
        select(Petal.Length, Petal.Width) |> 
        kmeans(centers = 3, nstart = 10)
    
    # add segment membership
    iris <- iris |> mutate(cluster = factor(out_iris$cluster))
    
    # plot segmented data
    ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
        geom_point(aes(col=cluster)) + 
        geom_point(data=as_tibble(out_iris$centers), size=4) + 
        theme_minimal()
    
    # "confusion matrix" -- ie, a table of actual vs predicted
    iris |> select(Species, cluster) |> table()
    
    ####Neat!
    

# HOW THE ALGO WORKS -----
        
# Let's understand the kmeans algorithm (using phone data). You do NOT need to know
# what all of the code does in these functions.  Just run them and look at the plots.
    
    # Run this function to show initial cluster points, in scaled space
    
        fun1 <- function() { 
            # specify a starting point for the cluster centroids
            c1 <<- c(gaming=-1, handsize= 2)
            c2 <<- c(gaming= 1, handsize= 1)
            c3 <<- c(gaming=-1, handsize=-1)
            c4 <<- c(gaming= 2, handsize=-1)
            
            # convert to a data.frame
            cent_dat <<- data.frame(rbind(c1, c2, c3, c4))
            
            # pick colors
            col4 <- c("magenta", "green", "cyan", "purple")
            
            # plot
            p <- ggplot() +
                geom_point(data=scl, aes(gaming, handsize)) +
                geom_point(data=cent_dat, aes(gaming, handsize), 
                           shape=21, fill=col4, color="black", size=5) + 
                ggtitle("Kmeans centroids") + 
                theme_minimal()
            
            print(p)
            return(invisible())
        }
        
        fun1()
    
    #### Cool! This function is showing where the starting positions of the 4 cluster centers are.
#### The colored dots are the initial guesses before the algo starts grouping them.
    
    # Run this function to show assignment of points
    
        fun2 <- function() {
            # get assignment criteria (euclidean distance to centroids)
            c1ssq <- apply(scl, 1, function(x) sqrt(sum((x-c1)^2)))
            c2ssq <- apply(scl, 1, function(x) sqrt(sum((x-c2)^2)))
            c3ssq <- apply(scl, 1, function(x) sqrt(sum((x-c3)^2)))
            c4ssq <- apply(scl, 1, function(x) sqrt(sum((x-c4)^2)))
            
            # pick closest centroid as cluster to which each point is assigned
            clust <<- factor(apply(cbind(c1ssq, c2ssq, c3ssq, c4ssq), 1, which.min))
            
            # plot
            p <- ggplot() +
                geom_point(data=scl, aes(gaming, handsize, color=clust)) +
                geom_point(data=cent_dat, aes(gaming, handsize), size=4) + 
                ggtitle("Kmeans cluster membership and centroids") + 
                theme_minimal() + 
                theme(legend.position = "none")
                
            
            print(p)
            return(invisible())
        }
    
        fun2()
        
        #### Ah, I see. They are starting to converge to their closest points.
        #### This is like a step by step look 'behind the scenes' of how the algo works.
        
    # run these functions a few times to show convergence
    
        fun3 <- function() {
            # Update cluster centers
            c1 <<- apply(scl[clust==1, ], 2, mean)
            c2 <<- apply(scl[clust==2, ], 2, mean)
            c3 <<- apply(scl[clust==3, ], 2, mean)
            c4 <<- apply(scl[clust==4, ], 2, mean)
            
            cent_dat <<- data.frame(rbind(c1, c2, c3, c4))
            
            # plot
            p <- ggplot() +
                geom_point(data=scl, aes(gaming, handsize, color=clust)) +
                geom_point(data=cent_dat, aes(gaming, handsize), size=4) + 
                ggtitle("Kmeans cluster membership and centroids") + 
                theme_minimal() + 
                theme(legend.position = "none")
            
            print(p)
            return(invisible())
        }
        
        fun4 <- function() {
            # get assignment criteria (euclidean distance to centroids)
            c1ssq <- apply(scl, 1, function(x) sqrt(sum((x-c1)^2)))
            c2ssq <- apply(scl, 1, function(x) sqrt(sum((x-c2)^2)))
            c3ssq <- apply(scl, 1, function(x) sqrt(sum((x-c3)^2)))
            c4ssq <- apply(scl, 1, function(x) sqrt(sum((x-c4)^2)))
            
            clust <<- factor(apply(cbind(c1ssq, c2ssq, c3ssq, c4ssq), 1, which.min))
        
            # plot
            p <- ggplot() +
                geom_point(data=scl, aes(gaming, handsize, color=clust)) +
                geom_point(data=cent_dat, aes(gaming, handsize), size=4) + 
                ggtitle("Kmeans cluster membership and centroids") + 
                theme_minimal() + 
                theme(legend.position = "none")
            
            print(p)
            return(invisible())
        }
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()

        stop("You need more iterations!")
        
        #####As in you want us to keep running these functions until the clusters coverge more?
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
        fun3()
        fun4()
        
    #### I see the clusters are moving less and less with each iteration.
        
        
        
    # clean up
    rm(cent_dat, centers, c1, c2, c3, c4, clust)
    
    
# PROFILE -----

# Profile the segments by demographics.  Specifically:
# summarize the segments by age, gender, height, and time spent chatting

    # start by adding cluster labels back to data
    
    cust_dat <- cust_dat |> mutate(cluster = factor(out$cluster))
    head(cust_dat)
        
    # For numeric variables, we can simply take means. 
    # For categorical variables, we calculate a proportion by taking the mean over the number of 
    # times something is "true"
    
    cdat <- cust_dat |> 
                group_by(cluster) |> 
                summarize(mean_age    = mean(age), 
                          prop_female = mean(gender=="female"), 
                          mean_height = mean(height),
                          mean_chat   = mean(chat))
    
    # view results
    cdat
    
    # We see that cluster two of the clusters chat substantially more than the other two clusters
    # And we see that two of the clusters have a lower percentage of females than the other two clusters
    
    # We can plot some of these relationships
    
    ggplot(cdat) + 
        geom_col(aes(y=mean_chat, x=cluster, fill=cluster)) + 
        ggtitle("Time spent in chat apps by segment") + 
        theme_minimal()
    
    
# SKREE PLOT -----
    
# is k=4 the right number for k?  #### I guess we'll find out.
    
    # we might want more information on which to base our choice of k
    # One thing we might do is try many different values of k, and evaluate
    # the performance of the algorithm for each k.  Here, our performance
    # criteria will be the within-group sum of squares (WSS) from the model.
    # As k increases, the WSS will decrease. The question is:
    # how fast does it decrease?
    
    # let's try k=1, k=2, ..., k=10
    
    # we'll create a vector named 'res' to store our results
    res <- vector(length=10)
    
    # we loop over k=1 through k=10
    for(i in 1:10) {
        # run k means
        out <- kmeans(scl, centers=i, nstart=25)
        
        # grab the WSS value, store it in the i'th position of res
        res[i] <- out$tot.withinss
    }
    
    # let's plot the WSS for each value of k
    ggplot(data.frame(x=1:10, y=res), aes(x,y)) + 
        geom_line(color="grey") + 
        geom_point(size=3) + 
        xlab("Number of Clusters (K)") + 
        ylab("Within-Group Sum of Squares (WSS)") + 
        theme_minimal()
    
    #### This is the answer to Quiz Question 4. We can see the 'elbow' around 3-4. 
    
    # We see a lot of benefit (ie, a substantial reduction in WSS) as
    # we move from 1-2 and 2-3 clusters. But the rate of benefit drops off
    # quickly after 3 clusters. 
    # Perhaps 3 would have been a better choice than 4 for these data
        
    
    # How does this look for the Iris data?
    
    res2 <- vector(length=10)
    
    for(i in 1:10) {
        # run k means
        out_iris <- iris |> 
                        select(Petal.Length, Petal.Width) |> 
                        kmeans(centers = i, nstart = 10)
        
        # grab the WSS value, store it in the i'th position of res
        res2[i] <- out_iris$tot.withinss
    }
    
    ggplot(data.frame(x=1:10, y=res2), aes(x,y)) + 
        geom_line(color="grey") + 
        geom_point(size=3) + 
        xlab("Number of Clusters (K)") + 
        ylab("Within-Group Sum of Squares (WSS)") + 
        theme_minimal()
    
    #### Ok, so maybe from the data it would have been better to use k=3 instead of k=4
    #### for the first dataset with customers. 
    #### Clearly, we can see k=3 is the best choice for the iris data since the WSS drops significantly.
    
# Summary of R commands introduced
    
    # kmeans algorithm
        # scale()             scales data so that mean=0 and sd=1
        # out <- kmeans()     runs the algorithm, stores results in 'out'
        # out$cluster         grabs each row's cluster membership
        # out$centers         grabs each centroid's location
        # out$tot.withinss    grabs the sum of squares
    
    # for loop syntax
        # for(var in vector) { do things in terms of var }
    
    # pre-allocate a vector and fill it in
        # res <- vector(length=10)    to pre-allocate
        # res[i] <- ...               to fill it in
    
    #### Quiz question 1

#### Scale the two variables we care about: social and gaming
sub2 <- cust_dat |> select(social, gaming) |> scale() |> as_tibble()

#### This answers Quiz Question 7-- That should standardized both variables (mean = 0, sd = 1) 

set.seed(3355)
out2 <- kmeans(sub2, centers = 5, nstart = 10)
out2$size
sort(out2$size, decreasing = TRUE)

#### We get 714, 622, 613, 547, and 504 

#### Quiz question 2

# Convert the clusters and their means back into the orignal units
centers_unscaled <- as_tibble(out2$centers * sapply(cust_dat |> select(social, gaming), sd) +
                              sapply(cust_dat |> select(social, gaming), mean))

# Round the results
round(centers_unscaled, 0)

#### Quiz Question 3 
#### Total Within-Group Sum of Squares (WSS)
out2$tot.withinss
#### measure the value how compact the clusters are. Answer 1083.355


    #### Quiz Question 5
    
    set.seed(3355)
out3 <- kmeans(sub2, centers = 3, nstart = 10)
out3$centers

sub3 <- sub2 |> mutate(cluster = factor(out3$cluster))

ggplot(sub3, aes(x = gaming, y = social, color = cluster)) +
  geom_point() +
  ggtitle("K-means Clustering (K = 3)") +
  theme_minimal()
  
  sub3_orig <- cust_dat |> transmute(social, gaming, cluster = factor(out3$cluster))
sub3_orig |> dplyr::group_by(cluster) |> dplyr::summarise(avg_social = mean(social), avg_gaming = mean(gaming))



    
    
