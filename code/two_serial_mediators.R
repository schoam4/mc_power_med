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

#--- INPUT VALUE CHECKS -------------------------------------------------------#

if (obj == "choose_n") {
  
  # CHECK: Is N greater than 5 and an integer?
  if (N < 5 | !abs(N - round(N)) < .Machine$double.eps ^ 0.5) {
    stop("\"Sample Size (N)\" must be an integer greater than 5. Please change this value.")
  }
  
} else {
  
  # CHECK: Is Target Power between 0 and 1?
  if (TarPow < 0 | TarPow > 1) {
    stop("\"Target Power\" must be a number between 0 and 1. Please change this value.")
  }
  
  # CHECK: Is Nlow greater than 5 and an integer?
  if (Nlow < 5 | !abs(Nlow - round(Nlow)) < .Machine$double.eps ^ 0.5) {
    stop("\"Minimum N\" must be an integer greater than 5. Please change this value.")
  }
  
  # CHECK: Is Nhigh greater than 5 and an integer?
  if (Nhigh < 5 | !abs(Nhigh - round(Nhigh)) < .Machine$double.eps ^ 0.5) {
    stop("\"Maximum N\" must be an integer greater than 5. Please change this value.")
  }
  
  # CHECK: Is Nsteps greater than 1 and an integer?
  if (Nsteps < 1 | !abs(Nsteps - round(Nsteps)) < .Machine$double.eps ^ 0.5) {
    stop("\"Sample Size Steps\" must be an integer greater than 1. Please change this value.")
  }
  
  # CHECK: Is Nhigh greater than nlow?
  if (Nlow >= Nhigh) {
    stop("\"Maxmimum N\" must be larger than \"Minimum N\". Please change these values.")
  }
  
  # CHECK: Is Nsteps smaller than N range?
  if (abs(Nhigh - Nlow) < Nsteps) {
    stop("\"Sample Size Steps\" must be smaller than the sample size range. Please change this value.")
  }
  
}

# CHECK: Is the number of replications > 5 and an integer?
if (powReps < 5 | !abs(powReps - round(powReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"# of Replications\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the number of MC replications > 5 and an integer?
if (mcmcReps < 5 | !abs(mcmcReps - round(mcmcReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"Monte Carlo Draws per Rep\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the seed > 5 and an integer?
#if (mcmcReps < 5 | !abs(mcmcReps - round(mcmcReps)) < .Machine$double.eps ^ 0.5) {
#  stop("\"Monte Carlo Draws per Rep\" must be an integer greater than 5")
#}

# CHECK: Is the confidence level (%) between 0 and 100?
if (conf < 0 | conf > 100) {
  stop("\"Confidence Level (%)\" must be a number between 0 and 100. Please change this value.")
}

#--- CONVERT / CHECK COVARIANCE MATRIX ----------------------------------------#

if (input_method == "correlations") {
# Import model input values
cor21 <- as.numeric(input$cor21)
cor31 <- as.numeric(input$cor31)
cor32 <- as.numeric(input$cor32)
cor41 <- as.numeric(input$cor41)
cor42 <- as.numeric(input$cor42)
cor43 <- as.numeric(input$cor43)


if(abs(cor21)> .999 | abs(cor31)> .999 | abs(cor32)> .999 |
   abs(cor41)> .999 | abs(cor42)> .999 | abs(cor43)> .999 ) {
   stop("One or more correlations are out of range (greater than 1 or less than -1)
         check your inputs and try again")
}

# Create correlation / covariance matrix
corMat <- diag(4)
corMat[2,1] <- cor21
corMat[1,2] <- cor21
corMat[3,1] <- cor31
corMat[1,3] <- cor31
corMat[2,3] <- cor32
corMat[3,2] <- cor32
corMat[4,1] <- cor41
corMat[1,4] <- cor41
corMat[2,4] <- cor42
corMat[4,2] <- cor42
corMat[3,4] <- cor43
corMat[4,3] <- cor43
} else {
  a1 <- as.numeric(input$STa1)
  a2 <- as.numeric(input$STa2)
  b1 <- as.numeric(input$STb1)
  b2 <- as.numeric(input$STb2)
  d <- as.numeric(input$STd)
  cprime <- as.numeric(input$STcprime)
  
  if(abs(a1)> .999 | abs(a2)> .999 | abs(b1)> .999 | 
     abs(b2)> .999 | abs(cprime)> .999 | abs(d) > .999 ) {
    stop("One or more standardized coefficients are out of range (greater than 1 or less than -1)
         check your inputs and try again")
  }
  
  corMat <- diag(4)
  corMat[2,1] <- a1
  corMat[1,2] <- a1
  corMat[3,1] <- a2 + d*a1
  corMat[1,3] <- a2 + d*a1
  corMat[2,3] <- d + a1*a2
  corMat[3,2] <- d + a1*a2 
  corMat[4,1] <- cprime + a1*b1 + corMat[3,1]
  corMat[1,4] <- cprime + a1*b1 + corMat[3,1]
  corMat[2,4] <- a1*cprime + b1 + b2*corMat[3,1]
  corMat[4,2] <- a1*cprime + b1 + b2*corMat[3,1]
  corMat[3,4] <- a2*corMat[3,1] + b2 + b1*corMat[3,2]
  corMat[4,3] <- a2*corMat[3,1] + b2 + b1*corMat[3,2]
  
}

SDX <- as.numeric(input$SDX)
SDM1 <- as.numeric(input$SDM1)
SDM2 <- as.numeric(input$SDM1)
SDY <- as.numeric(input$SDY)
# Get diagonal matrix of SDs
SDs <- diag(c(SDX, SDM1, SDM2, SDY))

# Convert to covariance matrix
covMat <- SDs%*%corMat%*%SDs

if (input$obj == "choose_n") {
  withProgress(message = 'Running Replications', value = 0, {
    
    # Start power simulation
    
    # Create function for 1 rep
    powRep <- function(seed = 1234, Ns = N, covMatp = corMat){
      set.seed(seed)
      require(MASS)
      
      incProgress(1/powReps)
      
      dat <- mvrnorm(Ns, mu = c(0,0,0,0), Sigma = covMatp)
      # Run regressions
      m1 <- lm(dat[,2] ~ dat[,1])
      m2 <- lm(dat[,3] ~ dat[,1] + dat[,2])
      m3 <- lm(dat[,4] ~ dat[,2] + dat[,3] + dat[,1])
      
      # Output parameter estimates and standard errors
      a1 <- rnorm(mcmcReps, coef(m1)[2], sqrt(vcov(m1)[2,2]))
      a2 <- rnorm(mcmcReps, coef(m2)[2], sqrt(vcov(m2)[2,2]))
      b1 <- rnorm(mcmcReps, coef(m3)[2], sqrt(vcov(m3)[2,2]))
      b2 <- rnorm(mcmcReps, coef(m3)[3], sqrt(vcov(m3)[3,3]))
      d  <- rnorm(mcmcReps, coef(m2)[3], sqrt(vcov(m2)[3,3]))
      
      a1b1 <- a1*b1
      a2b2 <- a2*b2
      a1db2 <- a1*d*b2
      
      # Calculate confidence intervals
      low <- (1 - (conf / 100)) / 2
      upp <- ((1 - conf / 100) / 2) + (conf / 100)
      LL1 <- quantile(a1b1, low)
      UL1 <- quantile(a1b1, upp)
      LL2 <- quantile(a2b2, low)
      UL2 <- quantile(a2b2, upp)        
      LLd <- quantile(a1db2, low)
      ULd <- quantile(a1db2, upp)  
      
      # Is rep significant?
      c(LL1*UL1 > 0, LL2*UL2 > 0, LLd*ULd > 0)
      
    }
    set.seed(seed)
    pow <- lapply(sample(1:50000, powReps), powRep)
    
    #Turn results into a matrix and then compute power for each effect
    df <- data.frame("Parameter" = c("a1b1", "a2b2", "a1db2"),
                     "N" = rep(N, 3),
                     "Power" = colSums(matrix(unlist(pow), nrow = powReps, byrow = TRUE)) / powReps)
  })
} else {
  #--- OBJECTIVE == CHOOSE POWER, CALCULATE N --------------------------------#
  
  withProgress(message = 'Running Replications', value = 0, {
    
    # Create function for 1 rep
    powRep <- function(seed = 1234, Ns = N, covMatp = corMat){
      set.seed(seed)
      require(MASS)
      
      incProgress(1/powReps)
      
      dat <- mvrnorm(Ns, mu = c(0,0,0,0), Sigma = covMatp)
      #Run regressions
      m1 <- lm(dat[,2] ~ dat[,1])
      m2 <- lm(dat[,3] ~ dat[,1] + dat[,2])
      m3 <- lm(dat[,4] ~ dat[,2] + dat[,3] + dat[,1])
      
      # Output parameter estimates and standard errors
      a1 <- rnorm(mcmcReps, coef(m1)[2], sqrt(vcov(m1)[2,2]))
      a2 <- rnorm(mcmcReps, coef(m2)[2], sqrt(vcov(m2)[2,2]))
      b1 <- rnorm(mcmcReps, coef(m3)[2], sqrt(vcov(m3)[2,2]))
      b2 <- rnorm(mcmcReps, coef(m3)[3], sqrt(vcov(m3)[3,3]))
      d  <- rnorm(mcmcReps, coef(m2)[3], sqrt(vcov(m2)[3,3]))
      
      a1b1 <- a1*b1
      a2b2 <- a2*b2
      a1db2 <- a1*d*b2
      
      # Calculate confidence intervals
      low <- (1 - (conf / 100)) / 2
      upp <- ((1 - conf / 100) / 2) + (conf / 100)
      LL1 <- quantile(a1b1, low)
      UL1 <- quantile(a1b1, upp)
      LL2 <- quantile(a2b2, low)
      UL2 <- quantile(a2b2, upp)        
      LLd <- quantile(a1db2, low)
      ULd <- quantile(a1db2, upp)  
      
      # Is rep significant?
      c(LL1*UL1 > 0, LL2*UL2 > 0, LLd*ULd > 0)
    }
    
    # Create vector of sample sizes
    Nused <- seq(Nlow, Nhigh, Nsteps)
    
    # Divide powReps among sample sizes; Create input vector for simulation
    Nvec <- rep(Nused, round(powReps/length(Nused)))
    
    set.seed(seed)
    # Run power analysis and logistic regression
    pow <- mapply(FUN = powRep, Ns = Nvec, seed = sample(1:50000, length(Nvec)),
                  SIMPLIFY = FALSE)
    pow <- data.frame(Nvec, do.call("rbind", pow))
    names(pow) <- c("N", "a1b1", "a2b2", "a1db2")
    #pow <- data.frame("N" = Nused,
    
    
    #as.numeric(by(pow$a1b1, INDICES = pow$N, FUN = mean))
    #as.numeric(by(pow$a2b2, INDICES = pow$N, FUN = mean))
    #as.numeric(by(pow$difference, INDICES = pow$N, FUN = mean))
    
    #   ?by
    #colMeans(pow[,2:4])
    
    # Checks:
    # if (sum(unlist(pow)) == length(Nvec)) {
    #   stop("Power for all sample sizes is 1, please choose a smaller lower sample size")
    # }
    # if (sum(unlist(pow)) == 0) {
    #   stop("Power for all sample sizes is 0, please choose a larger upper sample size")
    # }
    # else {
    out <- list()
    for(i in 2:4) {
      #i <- 2
      try(mod <- glm(pow[, i] ~ pow[, "N"], family = binomial(link = "logit")), silent = TRUE)
    
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
      
      # findTargetPower <- function(iv, dv, power) {
      #   FUN <- function(dv, iv, power) {
      #     x <- dv > power
      #     target <- which(x)
      #     if (length(target) > 0) {
      #       minIndex <- min(target)
      #       maxIndex <- max(target)
      #       if (dv[minIndex] > dv[maxIndex]) {
      #         return(iv[maxIndex, ])
      #       } else if (dv[minIndex] < dv[maxIndex]) {
      #         return(iv[minIndex, ])
      #       } else {
      #         return(Inf)
      #       }
      #     } else {
      #       return(NA)
      #     }
      #   }
      #   apply(dv, 2, FUN, iv = iv, power = power)
      # }
      out[[i-1]] <- res
    }
    df <- data.frame("Parameter" = rep(c("a1b1", "a2b2", "a1db2"),
                                       each = length(Nused)),
                     do.call("rbind", out))
    # Check:
    #if (all(res[, 3] < TarPow)) {
    #  stop("Power for all sample sizes is less than the target power value.
    #       Please choose a larger upper sample size")
    #}
    
    # TO DO: OUTPUT TARGET N + DATA FRAME
    # Calculate Target N
    #Ntarget <- unlist(findTargetPower(as.matrix(res[,1]),
    #                                  data.frame(res[,3]), .80))
    #paste("The target sample size is", Ntarget, "which results in power of",
    #      round(res[res[,1]==Ntarget, 3], 3), sep = " ")
    #df <- data.frame("Parameter" = "ab", res)
  })  
}