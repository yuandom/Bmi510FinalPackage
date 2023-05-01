# bmi510.R

#' Sampling from dataset x
#'
#' @param x A vector or dataframe-like object.
#' @param n A number
#' @param replace A boolean value
#' @return Result with the same type of input
#' @examples
#' rando(c(1,2,3), 2)
rando = function(x,n=1,replace=T){
  if (is.atomic(x) && is.vector(x)){
    return (sample(x, n,replace = replace))
  } else if (is.data.frame(x)) {
    sampleidx = sample(nrow(x), n,replace = replace)
    return (x[sampleidx,])
  } else {
    stop('Invalid input type of x')
  }
}

#' Verify the minimum
#'
#' @param x An atomic vector.
#' @param na.rm A boolean value that records if na is allowed
#' @return A logical vector
#' @examples
#' is_min(c(0,1,1,0,3))
is_min = function(x,na.rm=T){
  if (is.atomic(x) && is.vector(x)){
    minx = min(x, na.rm = na.rm)
    return (ifelse(x == minx, T, F))
  } else {
    stop('Invalid input type of x')
  }
}

#' Verify the maximum
#'
#' @param x An atomic vector.
#' @param na.rm A boolean value that records if na is allowed
#' @return A logical vector
#' @examples
#' is_max(c(0,1,1,0,3))
is_max = function(x,na.rm=T){
  if (is.atomic(x) && is.vector(x)){
    maxx = max(x, na.rm = na.rm)
    return (ifelse(x == maxx, T, F))
  } else {
    stop('Invalid input type of x')
  }
}

#' Repeating the input data based on the number of rows and columns
#'
#' @param x A matrix or a dataframe.
#' @param M A integer that records the number of repeating rows
#' @param N A integer that records the number of repeating columns
#' @return Result that with the same type of input
#'
rep_mat = function(x, M=1, N=1){
  if(is.matrix(x) || is.data.frame(x)){
    return (x[rep(c(1:nrow(x)), M), rep(c(1:ncol(x)), N)])
  } else {
    stop('Invalid input type of x')
  }
}

#' Finding the classes of each variable
#'
#' @param x Tibble type
#' @return Result that with the same type of input
#'
classes = function(x){
  if (is_tibble(x)){
    return (sapply(x, class))
  } else {
    stop('Invalid input type of x')
  }

}

#' Scaling the numeric columns of the input x
#'
#' @param x A tibble.
#' @param center A boolean value that records centering
#' @param scale A boolean value that records scaling
#' @return Result with the same type of input
#'
df_scale = function(x, center = T, scale = T){
  numeric_cols = which(classes(x) == 'numeric')
  for (col in numeric_cols){
      x[col] = scale(x[col],center = center, scale = scale)
  }
  return (x)
}

#' Calculating the log-likelihood of x under the normal distribution
#'
#' @param x A numeric vector or matrix.
#' @param mean A number that records mean of x.
#' @param sd A number that records standard deviation of x.
#' @return A number type result
#'
log_likelihood_norm = function(x, mean, sd) sum(dnorm(x, mean = mean, sd = sd, log = TRUE))

#' Calculating the log-likelihood of x under the uniform distribution
#'
#' @param x A numeric vector or matrix.
#' @param min A number that records min of x.
#' @param max A number that records max of x.
#' @return A number type result
#'
log_likelihood_unif = function(x, min, max) sum(dunif(x, min = min, max = max, log = TRUE))

#' Calculate the log-likelihood of x under the chi-square distribution
#'
#' @param x A numeric vector or matrix.
#' @param df A number that records degrees of freedom.
#' @return A number type result
#'
log_likelihood_chisq = function(x, df) sum(dchisq(x, df, log = TRUE))

#' Calculating the log-likelihood of x under the f distribution
#'
#' @param x A numeric vector or matrix.
#' @param df1 A number that records degrees of freedom.
#' @param df2 A number that records degrees of freedom.
#' @return A number type result
#'
log_likelihood_f = function(x, df1, df2) sum(df(x, df1 = df1, df2 = df2, log = TRUE))

#' Calculating the log-likelihood of x under the t distribution
#'
#' @param x A numeric vector or matrix.
#' @param df A number that records degrees of freedom.
#' @return A number type result
#'
log_likelihood_t = function(x, df) sum(dt(x, df, log = TRUE))

#' Calculating the sensitivity value
#'
#' @param pred A vector that records prediction results
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
sensitivity = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxP = which(truth == 1)
  idxTP = intersect(idxP, which(truth == pred))
  return (length(idxTP)/length(idxP))
}

#' Calculating the specificity value
#'
#' @param pred A vector that records prediction results
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
specificity = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxN = which(truth == 0)
  idxTN = intersect(idxN, which(truth == pred))
  return (length(idxTN)/length(idxN))
}

#' Calculating the precision value
#'
#' @param pred A vector that records prediction result
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
precision = function(pred,truth){
  pred = as.integer(pred)
  truth = as.integer(truth)
  idxPredP = which(pred == 1)
  idxTP = intersect(idxPredP, which(truth == pred))
  return (length(idxTP)/length(idxPredP))
}

#' Calculating the recall value
#'
#' @param pred A vector that records prediction results
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
recall = function(pred,truth) sensitivity(pred,truth)

#' Calculating accuracy
#'
#' @param pred A vector that records prediction result
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
accuracy = function(pred,truth) {
  pred = as.integer(pred)
  truth = as.integer(truth)
  correct = which(truth == pred)
  return(length(correct)/length(pred))
}

#' Calculating f1 score
#'
#' @param pred A vector that records prediction results
#' @param truth A vector that records ground truth label
#' @return A number type result
#'
f1 = function(pred,truth){
  prec = precision(pred,truth)
  rec = recall(pred,truth)
  return (2 * prec * rec / (prec + rec))
}

#' Calculating the minimum n for a t-test with two sample
#'
#' @param d A number type result, Cohen’s d.
#' @param power A number type result, whose defaults is 0.8
#' @return A number type result
#'
#'
minimum_n_per_group = function(d,power = 0.8) ceiling(power.t.test(power = power, delta = d)$n)

#' Calculating R-squared statistics
#'
#' @param pred A vector that records prediction results
#' @param truth A vector that records ground truth label
#' @return Result of the calculation
#'
r2 = function(pred,truth) {
  ss_res = sum((pred-truth)^2)
  ss_tot = sum((pred - mean(pred))^2)
  return (1 - ss_res/ss_tot)
}

#' Calculating adjusted R-squared statistics
#'
#' @param pred A vector that records the prediction results
#' @param truth A vector that records the ground truth label
#' @return A number type result
#'
adj_R2 = function(pred,truth,n_p) {
  r2_val = r2(pred,truth)
  n = length(pred)
  return (1-(1-r2_val)*(n-1)/(n-n_p-1))
}
