
# private auxiliary function to check if trials is valid
check_trials <- function(trials) {
  if(trials >= 0 && (trials %% 1 == 0) && (length(trials) == 1)) {
    return(TRUE)
  }
  stop("invalid trials value")
}

# private auxiliary function to check if prob is valid
check_prob <- function(prob) {
  if (prob <= 1 && prob >= 0 && (length(prob) == 1)) {
    return(TRUE)
  }
  stop("invalid prob value")
}

# private auxiliary function to check if success is valid
check_success <- function(success, trials) {
  for (i in length(success)) {
    if (success[i] > trials || success[i] < 0 || (success[i] %% 1 != 0)) {
      stop("invalid success value")
    }
    return(TRUE)
  }
}

#' @title bin_choose
#' @description calculate the number of possibilities to choose k from n
#' @param n number indicating the number of objects to be chosed from k
#' @param k number indicating the total number of objects
#' @return the number of ways to choose k from n
#' @export
#' @examples bin_choose(n = 5, k = 2)
#' @examples bin_choose(5, 0)
#' @examples bin_choose(5, 1:3)
bin_choose <- function(n, k) {
  if (k > n) {
    stop("k cannot be greater than n")
  }
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

#' @title bin_variable
#' @description get a binvar object with the provided trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return a binvar object
#' @export
#' @examples bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    result = list(trials, prob)
    names(result) = c("trials", "prob")
    class(result) = "binvar"
    return(result)
  }
}

#' @title bin_probability
#' @description calculate the probability of success with a number of trials with prob of success in each trial
#' @param success number indicating the needed number of successes
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the probability
#' @export
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5)
#' @examples bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' @examples bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob) {
  if(!check_trials(trials)) {
    stop("invalid trials value")
  } else if (!check_prob(prob)) {
    stop("invalid prob value")
  } else if (!check_success(success, trials)) {
    stop("invalid success value")
  } else {
    return(bin_choose(trials, success) * (prob ** success) * ((1 - prob) ** (trials - success)))
  }
}

#' @export
print.binvar <- function(bin) {
  cat("\"Binomial variable\" \n\nParamaters\n- number of trials: ")
  cat(bin$trials)
  cat("\n- prob of success : ")
  cat(bin$prob)
  cat("\n")
}

#' @export
summary.binvar <- function(bin) {
  result = list(trials = bin$trials, prob = bin$prob,
                mean = aux_mean(bin$trials, bin$prob),
                variance = aux_variance(bin$trials, bin$prob),
                mode = aux_mode(bin$trials, bin$prob),
                skewness = aux_skewness(bin$trials, bin$prob),
                kurtosis = aux_kurtosis(bin$trials, bin$prob))
  class(result) = "summary.binvar"
  return(result)
}

#' @export
print.summary.binvar <- function(bin) {
  cat("\"Binomial variable\" \n\nParamaters\n- number of trials: ")
  cat(bin$trials)
  cat("\n- prob of success : ")
  cat(bin$prob)
  cat("\n\nMeasures\n- mean    : ")
  cat(bin$mean)
  cat("\n- variance: ")
  cat(bin$variance)
  cat("\n- mode    : ")
  cat(bin$mode)
  cat("\n- skewness: ")
  cat(bin$skewness)
  cat("\n- kurtosis: ")
  cat(bin$kurtosis)
  cat("\n")
}

#' @title bin_distribution
#' @description get a data frame that the first column is the number of success, and the second column is the probability
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the data frame with the probability distribution
#' @export
#' @examples bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  result = data.frame(0:trials, bin_probability(0:trials, trials, prob))
  names(result) = c("success", "probability")
  class(result) = c("bindis", "data.frame")
  return(result)
}

#' @title bin_cumulative
#' @description get a data frame that the first column is the number of success, the second column is the probability, and the third column is the cumulative probability
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the data frame with the probability distribution and the cumulative of probability
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  added = c()
  add = 0
  for (i in 0:trials) {
    add = add + bin_probability(i, trials, prob)
    added = append(added, add)
  }
  result = data.frame(0:trials, bin_probability(0:trials, trials, prob), added)
  names(result) = c("success", "probability", "cumulative")
  class(result) = c("bincum", "data.frame")
  return(result)
}

# private auxiliary function to calculate the mean
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# private auxiliary function to calculate the variance
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# private auxiliary function to calculate the mode
aux_mode <- function(trials, prob) {
  result = trials * prob + prob
  if ((result %% 1 == 0) && (result %% 2 = 1)) {
    return(c(result, result - 1))
  }
  return(as.integer(result))
}

# private auxiliary function to calculate the skewness
aux_skewness <- function(trials, prob) {
  return((1 - 2 * prob) / ((trials * prob * (1 - prob)) ** 0.5))
}

# private auxiliary function to calculate the kurtosis
aux_kurtosis <- function(trials, prob) {
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}

#' @export
plot.bindis <- function(df) {
  barplot(df$probability, xlab = "successes", ylab = "probability", names.arg = df$success)
}

#' @export
plot.bincum <- function(df) {
  plot(df$success, df$cumulative, xlab = "successes", ylab = "probability", type = "o")
}

# main function
#' @title bin_mean
#' @description calculate the mean with certain trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the mean
#' @export
#' @examples bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    return(aux_mean(trials, prob))
  }
}

# main function
#' @title bin_variance
#' @description calculate the variance with certain trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the variance
#' @export
#' @examples bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    return(aux_variance(trials, prob))
  }
}

# main function
#' @title bin_mode
#' @description calculate the mode with certain trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the mode
#' @export
#' @examples bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    return(aux_mode(trials, prob))
  }
}

# main function
#' @title bin_skewness
#' @description calculate the skewness with certain trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the skewness
#' @export
#' @examples bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    return(aux_skewness(trials, prob))
  }
}

# main function
#' @title bin_kurtosis
#' @description calculate the kurtosis with certain trials and prob
#' @param trials number indicating the number of trials
#' @param prob number indicating teh probability of getting success
#' @return the kurtosis
#' @export
#' @examples bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  if(check_trials(trials) && check_prob(prob)) {
    return(aux_kurtosis(trials, prob))
  }
}

