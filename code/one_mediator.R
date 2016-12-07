#-----------------------------------------------------------------------------# 
# The code below runs a Monte Carlo Power Analysis Simulation for a
# simple mediation model fit via 2 regression equations. Input for this
# approach includes R^2 values of the consituent effects (a, b, c') and
# desired variances for the X, M, and Y variables. See Thoemmes,
# MacKinnon, and Reiser (2010), Appendix A for details.

# The endpoint for each iteration is whether the XX% Monte Carlo-based
# confidence interval contains 0. The results of each iteration are
# stored in a logical vector "pow", and estimated power is calculated as
# the number of replications in which the confidence interval does NOT
# include zero (i.e., significant) divided by the 

# Note that because this approach is regression, standard errors are used
# instead of the full asymptotic covariance matrix, which could be used
# via path analysis/SEM.
#-----------------------------------------------------------------------------#

require(MASS)

#--- IMPORT USER-SPECIFIED VALUES --------------------------------------------#

obj <- input$obj
powReps <- input$powReps
mcmcReps <- input$mcmcReps
seed <- input$seed
conf <- input$conf
input_method <- input$input_method

if (obj == "choose_power") {
  TarPow <- input$TarPow
  Nlow <- input$Nlow
  Nhigh <- input$Nhigh
  Nsteps <- input$Nsteps
} else {
  N <- input$N
}

# Import model input values
cor21 <- as.numeric(input$cor21)
cor31 <- as.numeric(input$cor31)
cor32 <- as.numeric(input$cor32)
SDX <- as.numeric(input$SDX)
SDM <- as.numeric(input$SDM)
SDY <- as.numeric(input$SDY)

# Create correlation matrix
corMat <- diag(3)
corMat[2,1] <- cor21
corMat[1,2] <- cor21
corMat[3,1] <- cor31
corMat[1,3] <- cor31
corMat[2,3] <- cor32
corMat[3,2] <- cor32
#corMat <- diag(3)
#corMat[2,1] <- .35
#corMat[1,2] <- .35
#corMat[3,1] <- .10
#corMat[1,3] <- .10
#corMat[2,3] <- .25
#corMat[3,2] <- .25
#SDX <- 1
#SDM <- 1.5
#SDY <- 2

# Get diagonal matrix of SDs
SDs <- diag(c(SDX, SDM, SDY))

# Convert to covariance matrix
covMat <- SDs %*% corMat %*% SDs

# CHECK: Is the input covariance matrix positive definite
if (all(eigen(covMat)$values > 0) == F) {
  stop("The input correlation matrix is not positive definite")
}

#--- OBJECTIVE == CHOOSE N, CALCULATE POWER --------------------------------#

if (input$obj == "choose_n") {

  withProgress(message = 'Running Replications', value = 0, {

    # Create function for 1 rep
    powRep <- function(seed = 1234, Ns = N, covMatp = covMat){
      #set.seed(seed)

      incProgress(1 / powReps)

      dat <- mvrnorm(Ns, mu = c(0,0,0), Sigma = covMatp)

      # Run regressions
      m1 <- lm(dat[,2] ~ dat[,1])
      m2 <- lm(dat[,3] ~ dat[,2] + dat[,1])

      # Output parameter estimates and standard errors
      pest <- c(coef(m1)[2], coef(m2)[2])
      covmat <- diag(c((diag(vcov(m1)))[2],
                       (diag(vcov(m2)))[2]))

      # Simulate draws of a, b from multivariate normal distribution
      mcmc <- mvrnorm(mcmcReps, pest, covmat, empirical = FALSE)
      ab <- mcmc[, 1] * mcmc[, 2]

      # Calculate confidence intervals
      low <- (1 - (conf / 100)) / 2
      upp <- ((1 - conf / 100) / 2) + (conf / 100)
      LL <- quantile(ab, low)
      UL <- quantile(ab, upp)

      # Is rep significant?
      LL*UL > 0
    }

    set.seed(seed)
    # Calculate Power
    pow <- lapply(sample(1:50000, powReps), powRep)

    # Output results data frame
    #df <- "YOU'RE GONNA NEED A BIGGER BOAT"
    df <- data.frame("Parameter" = "ab",
                     "N" = N,
                     "Power" = sum(unlist(pow)) / powReps)
  })
} else {

#--- OBJECTIVE == CHOOSE POWER, CALCULATE N --------------------------------#

  withProgress(message = 'Running Replications', value = 0, {

    # Create function for 1 rep
    powRep <- function(Ns = N, covMatp = covMat){
      #set.seed(seed)

      incProgress(1 / powReps)

      dat <- mvrnorm(Ns, mu = c(0, 0, 0), Sigma = covMatp)
      # Run regressions
      m1 <- lm(dat[,2] ~ dat[,1])
      m2 <- lm(dat[,3] ~ dat[,2] + dat[,1])

      # Output parameter estimates and standard errors
      pest <- c(coef(m1)[2], coef(m2)[2])
      covmat <- diag(c((diag(vcov(m1)))[2],
                       (diag(vcov(m2)))[2]))

      # Simulate draws of a, b from multivariate normal distribution
      mcmc <- mvrnorm(mcmcReps, pest, covmat, empirical = FALSE)
      ab <- mcmc[, 1] * mcmc[, 2]

      # Calculate confidence intervals
      low <- (1 - (conf / 100)) / 2
      upp <- ((1 - conf / 100) / 2) + (conf / 100)
      LL <- quantile(ab, low)
      UL <- quantile(ab, upp)

      # Is rep significant?
      LL*UL > 0

    }

    # Create vector of sample sizes
    Nused <- seq(Nlow, Nhigh, Nsteps)

    # Divide powReps among sample sizes; Create input vector for simulation
    Nvec <- rep(Nused, round(powReps/length(Nused)))
    
    set.seed(seed)
    # Run power analysis and logistic regression
    pow <- lapply(Nvec, powRep)

    # Checks:
    if (sum(unlist(pow)) == length(Nvec)) {
      stop("Power for all sample sizes is 1, please choose a smaller lower sample size")
    }
    if (sum(unlist(pow)) == 0) {
      stop("Power for all sample sizes is 0, please choose a larger upper sample size")
    }
    else {
      try(mod <- glm(unlist(pow) ~ Nvec, family = binomial(link = "logit")), silent = TRUE)

      #Funtion for predicted probability from simsem
      ## predProb: Function to get predicted probabilities from logistic regression

      # \title{
      # Function to get predicted probabilities from logistic regression
      # }
      # \description{
      # Function to get predicted probabilities from logistic regression
      # }
      # \usage{
      # predProb(newdat, glmObj)
      # }
      # \arguments{
      # \item{newdat}{
      # A vector of values for all predictors, including the intercept
      # }
      # \item{glmObj}{
      # An object from a fitted glm run with a logit link
      # }
      # }
      # \value{
      # Predictive probability of success given the values in the \code{newdat} argument.
      # }

      predProb <- function(newdat, glmObj, alpha = 0.05) {
        slps <- as.numeric(coef(glmObj))
        logi <- sum(newdat * slps)
        predVal <- as.matrix(newdat)
        se <- sqrt(t(predVal) %*% vcov(glmObj) %*% predVal)
        critVal <- qnorm(1 - alpha/2)
        logi <- c(logi - critVal * se, logi, logi + critVal * se)
        logi[logi > 500] <- 500
        logi[logi < -500] <- -500
        pp <- exp(logi)/(1 + exp(logi))
        if(round(pp[2], 6) == 1) pp[3] <- 1
        if(round(pp[2], 6) == 0) pp[1] <- 0
        return(pp)
      }

      powVal <- cbind(1, Nused)

      # List of power estimates with prediction intervals
      res <- apply(powVal, 1, predProb, mod)
      res <- cbind(powVal[, 2], t(as.matrix(res)))
      colnames(res) <- c("N", "LL", "Power", "UL")

      ## Taken from simsem (thanks Sunthud!)
      # findTargetPower: Find a value of a given independent variable that provides a
      # given value of power. This function can handle only one independent variable.

      # \title{
      # Find a value of varying parameters that provides a given value of power.
      # }
      # \description{
      # Find a value of varying parameters that provides a given value of power. This function can deal with only one varying parameter only (\code{\link{findPower}} can deal with more than one varying parameter).
      # }
      # \usage{
      # findTargetPower(iv, dv, power)
      # }
      # \arguments{
      # \item{iv}{
      # A vector of the target varying parameter
      # }
      # \item{dv}{
      # A \code{data.frame} of the power table of target parameters
      # }
      # \item{power}{
      # A desired power.
      # }
      # }
      # \value{
      # The value of the target varying parameter providing the desired power. If the value is \code{NA}, there is no value in the domain of varying parameters that provide the target power. If the value is the minimum value of the varying parameters, it means that the minimum value has already provided enough power. The value of varying parameters that provides exact desired power may be lower than the minimum value.
      # }

      findTargetPower <- function(iv, dv, power) {
        FUN <- function(dv, iv, power) {
          x <- dv > power
          target <- which(x)
          if (length(target) > 0) {
            minIndex <- min(target)
            maxIndex <- max(target)
            if (dv[minIndex] > dv[maxIndex]) {
              return(iv[maxIndex, ])
            } else if (dv[minIndex] < dv[maxIndex]) {
              return(iv[minIndex, ])
            } else {
              return(Inf)
            }
          } else {
            return(NA)
          }
        }
        apply(dv, 2, FUN, iv = iv, power = power)
      }

      # Check:
      if (all(res[, 3] < TarPow)) {
        stop("Power for all sample sizes is less than the target power value.
             Please choose a larger upper sample size")
      }

      # TO DO: OUTPUT TARGET N + DATA FRAME
      # Calculate Target N
      #Ntarget <- unlist(findTargetPower(as.matrix(res[,1]),
      #                                  data.frame(res[,3]), .80))
      #paste("The target sample size is", Ntarget, "which results in power of",
      #      round(res[res[,1]==Ntarget, 3], 3), sep = " ")
      df <- data.frame("Parameter" = "ab", res)
    }
  })
}
