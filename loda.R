## LODA: Lightweight on-line detector of anomalies
# Compiled by: Ivy Sandberg (June, 2020)
# algorithm derived from Thomas Pevny (published: 2015)
# code pieces pulled from github: liusi2019 (Tadesse Zemicheal <zemichet@oregonstate.edu>, Si Liu <lius2@oregonstate.edu>) (license: NA)

# Steps: 1. build projections, 2. build histograms, 3. generate anomaly scores

# Step 1: generate projections used to fit model dependent on # features & sparsity (% outliers in the data)
get_random_proj <-
  function(nproj, d, sp) {
    nzeros <- (d*sp) %/% 1 # number of features (d) selected to build projections depends on sparsity
    idxs <- 1:d # set of dims that will be sampled to be set to zero
    w <- matrix(0, nrow=d, ncol=nproj)
    for (i in 1:nproj) {
      w[,i] <- rnorm(d, mean=0, sd=1) 
      if (nzeros > 0) {
        z <- sample(idxs, nzeros, replace=F) # randomly select some of the features to be set to 0
        w[z,i] <- 0
      }
      w[, i] <- w[, i] / sqrt(sum(w[, i] * w[, i])) # essentially dividing each row by 1
    }
    return (w)
  }


## Step 2: build historgram of each projections, output = list of histograms
build_proj_hist <-
  function(a, w) {
    d <- ncol(w) # ncol(w) = # projections (k)
    x <- a %*% w
    hists <- list()
    for (j in 1:d) {
      hists[[j]] <- hist(x[,j], plot=F) 
      # model significantly slower without plot=F
    }
    return (hists)
  }

## Step 3: Generate anomaly score = avg of the log of probabilites estimated on individual projection vectors
# determined by the probability density functions of histograms
get_neg_ll_all_hist <-
  function(a, w, hists, inf_replace=log(1e-09)) {
    a = as.matrix(a)
    x <- a %*% w
    k <- length(hists)
    pds <- matrix(0, nrow=nrow(x), ncol=k)
    for (j in 1:k) {
      pds[,j] <- pdf_hist(x[,j], hists[[j]])
    }
    pds <- log(pds)
    if (!is.na(inf_replace)) pds[is.infinite(pds)] = inf_replace
    ll <- -apply(pds, 1, mean) # neg. log-lik
    return (ll)
  }

# probability density function of a single histogram (h), x = data * projections
# returns the density of each point => pds matrix of densities
pdf_hist <-
  function(x, h) {
    n <- length(x)
    pd <- rep(0, n)
    for (j in 1:n) {
      i <- binsearch(h$breaks, x[j])
      # -1 adjustment since the upper index into the array is returned
      if (i > 1) i <- i - 1
      # More accurately, we should also multiply by diff(h$breaks)[i]; 
      # however, all breaks are equal in length in this algorithm,
      # hence, ignoring that for now.
      pd[j] <- h$density[i]
    }
    return (pd)
  }

# used to determine the bin location of xi and corresponding density is determined in pdf_hist
binsearch <-
  function(arr, x) {
    # Assumes arr contains the histogram bin breaks.
    # Note: Returns the index of the upper boundary of interval
    # since we assume the intervals are right closed i.e. (l,r]
    l <- 1
    r <- length(arr)
    if (x <= arr[l]) return (l)
    if (x >= arr[r]) return (r)
    while (l < r) {
      p <- (l+r) %/% 2
      if (x > arr[p]) {
        l <- p
      } else if (x <= arr[p]) {
        r <- p
      }
      if (l == r-1 && x <= arr[r]) l <- r
    }
    return (r)
  }

# Loda model parameters:
# a = data array of shape (n_samples, n_features), must be a non-empty 2D array containing only finite values
# sparsity = the proportion of outliers in the data
# maxk = number of histograms used to fit model
loda <-
  function(a,sparsity=NA, maxk=1000, inf_replace = log(1e-09)) {
    a <- as.matrix(a)
    l <- nrow(a) 
    d <- ncol(a)
    
    if (is.na(sparsity)) {
      sp <- ifelse(ncol(a) == 1, 0, 1-1/sqrt(ncol(a)))
    } else {
      sp <- sparsity
    }
    pvh <- loda_proj(a, maxk=maxk, sp=sp) # outputs a list of the projections & histograms
    # generate anomaly scores = negative log likelihood
    nll <- get_neg_ll_all_hist(a, pvh$pvh$w, pvh$pvh$hists, inf_replace = inf_replace)
    # returns a list of the anomaly scores, the projection vectors, and summaries of the histograms
    return(list(nll = nll, pvh = pvh$pvh)) 
  }

## build projections and histograms
loda_proj <-
  function(a, maxk=1000, sp=1-1/sqrt(ncol(a))) {
    n <- nrow(a)
    d <- ncol(a)
    
    w <- matrix(0, nrow=d, ncol=maxk) # initalize projection matrix (rows=datafeatures x cols=#histograms)
    hists <- list() # initalize list of output histograms
    
    w <- get_random_proj(nproj=maxk, d=d, sp=sp) # proj matrix, # projections = # histograms
    # initialize matrix of total observations x histograms 
    record_mat <- matrix(nrow = n, ncol = maxk)
    # for each of the projections record if an observation is present
    for (i in 1:maxk){
      # sample n number of observations with replacement
      loda_index = sample(1:n, n, replace = TRUE, prob = NULL)
      loda_data = a[loda_index, ] 
      ## keep record of if one point appeared in the bootstrap sample or not
      record_mat[,i] = as.integer(c(1:n) %in% loda_index) # read as: "is the sample value present in the loda_index"
      # build out histograms of each of the projections
      hists[i] <- build_proj_hist(loda_data, matrix(w[,i], ncol = 1))
    }
    # ouput = list of projections & histograms, and the matrix recording obersvation appearances
    return (list(pvh=list(w=w,hists=hists), record_mat = record_mat))
  }

full_loda <-
  function(a, n_hists, num_outliers) {
    
    # remove non-numeric columns
    num_cols=unlist(lapply(a, is.numeric)) 
    a=a[ ,num_cols]
    # drop rows with missing values
    # a=na.omit(a)
    
    loda_output=loda(a, sparsity = NA, n_hists)
    w <- loda_output$pvh$w
    
    # run a one-sample t-test to determine confidence internval for anomaly scores
    scores <- loda_output$nll
    
    # pull out top anomalies
    sort_scores <- sort(loda_output$nll, index.return=TRUE, decreasing=TRUE)
    outliers <- lapply(sort_scores, `[`, sort_scores$x %in% head(unique(sort_scores$x), num_outliers)) # output: x = scores, ix= index
    
    return(list(loda_output=list(top_anomaly_scores=outliers$x, anomaly_indexes=outliers$ix, 
                anomaly_score_range=range(loda_output$nll)), w=w, scores=scores)) 
    # Return top anomalies and the full loda output
    # return("Full LODA Output: ", loda_output)
  }

# return row of each observed anomaly 
print_anomaly_rows <-
  function(a, loda_output){
    ix <- loda_output$anomaly_indexes
    rows <- a[ix,]
  
    return(list(anomaly_rows=rows)) 
  }


# function to determine which random projections include feature j
# construct histograms including feature j separately
build_impfeat_hists <- function(a, w, j){
  # a = data in matrix form
  # w = projection matrix (use w <- get_random_proj(nproj=maxk, d=d, sp=sp))
  # j = index of feature of interest
  n <- nrow(a)
  projs_containing_feature <- w[,(w[j,]) != 0]
  projs_NOT_containing_feature <- w[,(w[j,]) == 0]
  
  # sample n number of observations with replacement to use to populate the histograms
  loda_index = sample(1:n, n, replace = TRUE, prob = NULL)
  loda_data = a[loda_index, ] 
    
  hists_wj <- list() # initalize list of output histograms
  hists_woutj <- list ()

  for (i in 1:ncol(projs_containing_feature)){
    # build out histograms of each of the projections
    hists_wj[i] <- build_proj_hist(loda_data, matrix(projs_containing_feature[,i], ncol = 1))
  }
  for (i in 1:ncol(projs_NOT_containing_feature)) {
    hists_woutj[i] <- build_proj_hist(loda_data, matrix(projs_NOT_containing_feature[,i], ncol=1))
  }
  return (projshists=list(wj=projs_containing_feature, hists_wj=hists_wj, 
                   wnoj=projs_NOT_containing_feature, histswoutj=hists_woutj))
}

test_feature <- function(a, w, j){
  loda_featimp <- build_impfeat_hists(a, w, j)
  scores_w_j <- get_neg_ll_all_hist(a, loda_featimp$wj, loda_featimp$hists_wj)
  scores_wout_j <- get_neg_ll_all_hist(a, loda_featimp$wnoj, loda_featimp$histswoutj)
  
  # test significance - measures the difference between the average anomaly score including j and not including j
  n <- nrow(a)
  t <- (mean(scores_w_j) - mean(scores_wout_j)) / sqrt((var(scores_w_j)/n + var(scores_wout_j)/n))
  
  return(list(avg_w_j=mean(scores_w_j), avg_wout_j=mean(scores_wout_j), feature_tstat=t))
}

# determine the most important features give data (a) and set of random projections (w)
feature_importances <- function(a, w) {
  # remove non-numeric columns
  num_cols=unlist(lapply(a, is.numeric)) 
  a=a[ ,num_cols]
  # drop rows with missing values
  # a=na.omit(a)
  a <- as.matrix(a)
  d <- ncol(a)
  features <- vector()
  tstats <- vector()
  for (j in 1:d){
    tstats[[j]] <- test_feature(a, w, j)$feature_tstat
    features[[j]] <- colnames(a)[j]
  }
  result <- cbind(features, tstats)
  rank <- data.frame(result[order(as.numeric(result[,2]), decreasing = TRUE),])
  return(list(feature_ranking=rank))
}


