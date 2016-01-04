## multiple plots of anything
multiplot <- function(..., plotlist=NULL, cols) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}

## apply multiple functions
multi_sapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}

## multiple ggplots
makeplot <- function(x=NULL, y=NULL, data, density=FALSE, bar=FALSE, ...){
  require(ggplot2)
  require(ggthemes)
  if (bar) {
    if (is.null(y)) {
      
      data$x <- as.factor(data[ ,x])
      
      base <-
        ggplot(data) +
        theme_few(base_size = 10, base_family = "Helvetica")
      
      plot <-  base + geom_bar(aes(x = x, ...),
                               ...) + xlab(x)
    }
    if (!is.null(y)) {
      
      data$treatment <- as.factor(data[ ,x])
      data$y <- as.factor(data[ ,y])
      base <-
        ggplot(data) +
        theme_few(base_size = 10, base_family = "Helvetica")
      plot <- base + geom_bar(aes(x = y, fill = treatment, ...),
                              position = "dodge",
                              ...) + xlab(y)
    }
  }
  
  if (density) {
    if (is.null(y)) {
      data$x <- data[ ,x]
      
      base <-
        ggplot(data) +
        theme_few(base_size = 10, base_family = "Helvetica")
      plot <-
        base +
        geom_density(aes(x = x)) + xlab(x)
    }
    
    if (!is.null(y)) {
      data$treatment <- as.factor(data[ ,x])
      data$y <- data[ ,y]
      base <-
        ggplot(data) +
        theme_few(base_size = 10, base_family = "Helvetica")
      plot <-
        base +
        geom_density(aes(x = y, fill= treatment), alpha=0.5) +
        xlab(y)
    }
  }
  return(plot)
}

## user-written function to build discrete surface plot
gg_surface_discrete <-
  function(
    x, # sequence for the 1st dimension variable
    y, # sequence for the 2nd dimension variable
    xyz_func, # function to produce 3rd dimension variable
    quant = FALSE, # use quantiles to produce discrete breaks?
    quant_probs = seq(from = 0, to = 1, length.out = 10), # quantiles to produce discrete breaks
    brks_seq = seq(from = 0, to = 100, length.out = 10), # direct breaks on the 3rd dimension scale
    brew_palette = "PuBuGn", # color palette to use
    xyz_names = c("x","y","z"), # variable names to use
    title = NULL # title to use
  ) {
    # require packages
    require(ggplot2)        # plotting package
    require(RColorBrewer)   # for beautiful color palettes
    require(magrittr)       # for pipeline operator
    require(ggthemes)       # for plot themes
    
    # expand grid over 1st and 2nd dimensions
    gg <- expand.grid(x = x, y = y)
    # calculate 3rd dimension using grid
    gg$z <- gg %$% xyz_func(x,y)
    
    
    if (quant) { # if quant = TRUE, use quant_probs to produce breaks on the 3rd dimension
      brks <-
        cut(x = gg$z,
            breaks = quantile(gg$z,
                              probs = quant_probs,
                              na.rm = T),
            include.lowest = T)
    } else { # if quant = FALSE, use brks_seq to produce breaks on the 3rd dimension
      brks <-
        cut(x = gg$z,
            breaks = brks_seq,
            include.lowest = T)
    }
    
    brks <- gsub(pattern = ",",
                 replacement = " - ",
                 x = brks,
                 fixed = TRUE)  # replace "," with " - "
    
    gg$brks <- gsub(pattern = "\\(|\\[|\\]",
                    replacement = "",
                    x = brks)  # get rid of brackets in the labels
    
    # calculate number of breaks
    brks_len <- length(unique(brks))
    # expand palette to have required number of colors
    get_palette <- colorRampPalette(brewer.pal(8, brew_palette))
    # make the plot
    plot <- ggplot(gg, aes(x, y)) +
      geom_tile(aes(fill = brks)) +
      scale_fill_manual(xyz_names[3],
                        values = get_palette(brks_len)) +
      theme_few(base_size = 14, base_family = "Helvetica") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      coord_fixed() +
      labs(title = title, x = xyz_names[1], y = xyz_names[2])
    # return plot
    return(plot)
  }

## plot missing data
ggplot_missing <- function(x){
  
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

## weighted variance function
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  return(sum(w * (x - mean.w)^2, na.rm = na.rm))
  #     (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm)
}

## clustered standard errors (Stata style)
cl <- function(dat,fm, cluster) {
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  uj  <- apply(X = estfun(fm),
               MARGIN = 2,
               FUN = function(x) tapply(x, cluster, sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(coeftest(fm, vcovCL))
}

## bayes integration from alex
bayes_updater <- function(est_1, se_1, est_2, se_2){
  est_pooled <- weighted.mean(c(est_1, est_2), w = 1/(c(se_1, se_2)^2))
  se_pooled <- sqrt(1/((1/se_1^2) + (1/se_2^2)))
  return(c(est_pooled, se_pooled))
}

## randomization inference and confidence set
rand_inference <-
  function(
    # Outcome variable, must be vector of numerics
    Y,
    # Treatment indicator, muct be vector of numerics
    Z,
    # If defined, block or pair indicator, must be vector
    block = NULL,
    # Bounds of possible treatment effects to be tested
    # Must be vector of two numerics
    tau_range = c(-10,10),
    # Number of treatment effects within the bounds to be tested
    # Must be single integer
    tau_length = 100,
    # Number of simulations to generate randomization distribution
    sims = 100,
    # Significance level for two-sided test
    # must be numeric in (0,1)
    alpha = .05,
    # Exact mean of randomization distribution if known
    # If NULL, function takes the mean of randomization
    # distribution generated per each treatment effect tested
    # Must be single numeric
    exp_rand = NULL) {
    
    # Preliminaries
    require(dplyr)
    require(magrittr)
    taus <- taus_sup <- taus_inf <- c(NULL)
    
    # Start loop over the range of possible taus
    for (tau in seq(from = tau_range[1],
                    to = tau_range[2],
                    length.out = tau_length)) {
      
      # Adjusted outcome
      Y0 <- Y - Z*tau
      
      if ( is.null(block) ) {
        # Test statistic
        t_stat_obs <- coef(lm(Y0 ~ Z))[2]
        # Permute treatment assignment
        t_stats <- replicate(sims,
                             coef(lm(Y0 ~ sample(Z)))[2])
      } else {
        # Within block test statistic
        t_stat_obs <- coef(lm(Y0 ~ Z + block))[2]
        # Permute treatment assignment within blocks
        t_stats <-
          replicate(sims,
                    { Z_sim <- Z %>%
                      split(., block) %>%
                      lapply(., sample) %>%
                      unsplit(., block)
                    return(coef(lm(Y0 ~ Z_sim + block))[2])
                    }
          )
      }
      
      # Get the p-value
      pval <-
        ( sum( t_stats >= t_stat_obs ) / sims ) %>%
        min(., (1 - .))
      # Append current tau to vector of taus
      # based on two-sided alpha level
      if ( pval >= alpha/2 ) taus <- c(taus, tau)
      
      # Append tau to taus_sup or taus_inf
      # based on comparison to expectation of
      # randomization distribution
      exp_rand <- ifelse(test = is.null(exp_rand),
                         yes = mean(t_stats),
                         no = exp_rand)
      if ( t_stat_obs < exp_rand )
        taus_inf <- c(taus_inf, tau)
      if ( t_stat_obs > exp_rand )
        taus_sup <- c(taus_sup, tau)
    }
    
    # Compute Hodges-Lehmann estimate
    HL_est <- .5 * ( min(taus_inf) + max(taus_sup) )
    
    # Check for irregularities
    if ( any( abs( c(min(taus),
                     max(taus),
                     HL_est) ) == Inf ) )
      cat("Warning: Tau range could be wrong\n")
    
    output <- list("lower_cs" = min(taus),
                   "upper_cs" = max(taus),
                   "HL_est" = HL_est)
    return(invisible(output))
    cat("Confidence set = [",
        output[[1]],
        ", ",
        output[[2]],
        "]; Hodges-Lehmann estimate = ",
        output[[3]], sep = "")
  }

## trimmed residualized outcomes from rlm() object
trimmed_resid <- function(arlm) {
  stopifnot(inherits(arlm, "rlm")) # only works for rlms
  with(arlm, residuals * psi(residuals/s)) }

## function for HC2 Confidence Intervals
confint_HC <- function(object, parm, level = 0.95, thevcov, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) parm <- pnames
  else if (is.numeric(parm)) parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, object$df.residual)
  pct <- stats:::format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(thevcov))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

# ## Check coverage rate; hold Y fixed and permute Z which invents
# ## unit level truth of 0, then check proportion of times 0 is
# ## enveloped by the 10^5 confidence intervals
new_CI <- function(.df) {
  Y <- .df$outcome
  origZ <- .df$Z
  .cohort <- .df$cohort
  Znew <- sample(origZ)
  if (length(unique(.df["cohort"])) == 1) lm_new <- lm(Y ~ Znew)
  else lm_new <- lm(Y ~ Znew + cohort)
  hc2_ci <- confint_HC(lm_new, level = 0.95, parm = "Znew",
                       thevcov = vcovHC(lm_new, type = "HC2"))[1, ]
  iid_ci <- confint(lm_new, level = 0.95, parm = "Znew")[1, ]
  neyman_se_est <- sqrt( ( var(Y[Znew==1])/length(Y[Znew==1]) ) +
                           ( var(Y[Znew==0])/length(Y[Znew==0]) ) )
  neyman_ci <- c(coef(lm_new)[2] - 1.96 * neyman_se_est,
                 coef(lm_new)[2] + 1.96 * neyman_se_est)
  # se_est_fpc <- ( nrow(data)/(nrow(data) - 1) ) *
  #   sqrt( ( var(Y[Znew==1])/length(Y[Znew==1]) ) +
  #           ( var(Y[Znew==0])/length(Y[Znew==1]) ) )
  # fpc_ci <- c(coef((lm_new)[2])+1.96*se_est_fpc, coef((lm_new)[2])-1.96*se_est_fpc)
  zero_in_hc2 <- 0 >= hc2_ci[1] & 0 <= hc2_ci[2]
  zero_in_iid <- 0 >= iid_ci[1] & 0 <= iid_ci[2]
  zero_in_neyman <- 0 >= neyman_ci[1] & 0 <= neyman_ci[2]
  # zero_in_fpc <- 0 >= fpc_ci[1] & 0 <= fpc_ci[2]
  return(c(hc2 = zero_in_hc2[[1]],
           iid = zero_in_iid[[1]],
           neyman = zero_in_neyman[[1]]))
}
