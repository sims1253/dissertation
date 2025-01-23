library(ggplot2)
library(ggthemes)
library(patchwork)
library(latex2exp)
library(purrr)
library(bayesplot)
library(brms)
library(posterior)
library(dplyr)

setwd("~/Documents/dr/dissertation/")

PALETTE = palette.colors(palette = "Okabe-Ito")
color_scheme_set(PALETTE[2:7])


shared_colors <- scale_colour_manual(
  name = "",
  values = c(
    "Prior" = PALETTE[[3]],
    "Observations" = PALETTE[[1]],
    "Posterior" = PALETTE[[2]],
    "Difference" = PALETTE[[4]],
    "CI" = PALETTE[[5]]
  ))

shared_fills <- scale_fill_manual(
  name = "",
  values = c(
    "Prior" = PALETTE[[3]],
    "Observations" = PALETTE[[1]],
    "Posterior" = PALETTE[[2]],
    "Difference" = PALETTE[[4]],
    "CI" = PALETTE[[5]]
  ))

shared_linetypes <- scale_linetype_manual(
  name = "",
  values = c(
    "Prior" = 2,
    "Observations" = 5,
    "Posterior" = 1,
    "CI" = 3
  ))


# 1.1 beta-binmial case
beta_binomial_update <- function()
{
  y = 3
  N = 10
  alpha = 10
  beta = 10
  linewidth = 0.6
  
  shared_theme <- theme_bw(base_size = 12) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.text = ggplot2::element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.width = unit(2, "cm"),
          plot.subtitle = element_text(hjust = 0.5))
  
  shared_x_axis <- scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
  shared_y_axis <- scale_y_continuous(limits = c(0, 4.5))
  
  p1 = ggplot() +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta,
                  args = list(shape1 = alpha, shape2 = beta),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(shape1=y + alpha, shape2 = N - y + beta),
                  linewidth = linewidth) +
    geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 3, 
                     color = "Observations", linetype = "Observations"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis + 
    labs(subtitle = TeX("$\\alpha$ = 10, $\\beta$ = 10"))
  
  p2 = ggplot() +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta, args = list(shape1 = 1, shape2 = 1),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(shape1=y + 1, shape2 = N - y + 1),
                  linewidth = linewidth) +
    geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 3, 
                     color = "Observations", linetype = "Observations"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis + 
    labs(subtitle = TeX("$\\alpha$ = 1, $\\beta$ = 1"))
  
  p3 = ggplot() +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta, args = list(shape1 = 3.5, shape2 = 5),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(shape1=y + 3.5, shape2 = N - y + 5),
                  linewidth = linewidth) +
    geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 3, 
                     color = "Observations", linetype = "Observations"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis + 
    labs(subtitle = TeX("$\\alpha$ = 3.5, $\\beta$ = 5"))
  
  p1 + p3 + p2 + plot_layout(guides = 'collect') &
    theme(legend.position = "bottom",
          legend.margin = margin(t = -10, b = -10),
          legend.spacing.y = unit(0.1, "cm"))
}
beta_binomial_update()
ggsave(filename = "figures/beta_binomial_update.pdf",
       width = 210-50, height = ((297 - 70)/4) * 0.8, units = "mm", useDingbats = TRUE)

# 1.2 sequential updating
sequential_updating <- function()
{
  linewidth = 0.6
  
  shared_theme <- theme_bw(base_size = 10) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.text = ggplot2::element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.width = unit(2, "cm"),
          plot.subtitle = element_text(hjust = 0.5))
  
  shared_x_axis <- scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
  shared_y_axis <- scale_y_continuous(limits = c(0, 4.2))
  color_alpha = 0.125
  
  alpha = 1
  beta = 1
  
  # Create sequence of x values
  x_vals <- seq(0, 1, length.out = 1000)
  
  # Calculate y values for both distributions
  prior_y <- dbeta(x_vals,
                   shape1 = 0 + alpha,
                   shape2 = 0 + beta)
  posterior_y <- dbeta(x_vals,
                       shape1 = 1 + alpha,
                       shape2 = 0 + beta)
  
  # Create the plot
  p1 = ggplot() +
    geom_ribbon(data = data.frame(
      x = x_vals,
      prior = prior_y,
      posterior = posterior_y
    ),
    aes(x = x,
        ymin = pmin(prior, posterior),
        ymax = pmax(prior, posterior),
        fill = "Difference"),
    alpha = color_alpha) +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta, 
                  args = list(shape1 = 0 + alpha,
                              shape2 = 0 - 0 + beta),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(
                    shape1 = 1 + alpha,
                    shape2 = 1 - 1 + beta),
                  linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               0 + alpha,
                               0 - 0 + beta),
                     y = 0, xend = qbeta(0.975,
                                         0 + alpha,
                                         0 - 0 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               0 + alpha,
                               0 - 0 + beta),
                     y = 0, xend = qbeta(0.025,
                                         0 + alpha,
                                         0 - 0 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               1 + alpha,
                               1 - 1 + beta),
                     y = 0, xend = qbeta(0.975,
                                         1 + alpha,
                                         1 - 1 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               1 + alpha,
                               1 - 1 + beta),
                     y = 0, xend = qbeta(0.025,
                                         1 + alpha,
                                         1 - 1 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_fills +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis
  
  # Calculate y values for both distributions
  prior_y <- dbeta(x_vals,
                   shape1 = 2 + alpha,
                   shape2 = 9 - 2 + beta)
  posterior_y <- dbeta(x_vals,
                       shape1 = 3 + alpha,
                       shape2 = 10 - 3 + beta)
  
  # Create the plot
  p2 = ggplot() +
    geom_ribbon(data = data.frame(
      x = x_vals,
      prior = prior_y,
      posterior = posterior_y
    ),
    aes(x = x,
        ymin = pmin(prior, posterior),
        ymax = pmax(prior, posterior),
        fill = "Difference"),
    alpha = color_alpha) +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta, 
                  args = list(shape1 = 2 + alpha,
                              shape2 = 9 - 2 + beta),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(
                    shape1 = 3 + alpha,
                    shape2 = 10 - 3 + beta),
                  linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               2 + alpha,
                               9 - 2 + beta),
                     y = 0, xend = qbeta(0.975,
                                         2 + alpha,
                                         9 - 2 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               2 + alpha,
                               9 - 2 + beta),
                     y = 0, xend = qbeta(0.025,
                                         2 + alpha,
                                         9 - 2 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               3 + alpha,
                               10 - 3 + beta),
                     y = 0, xend = qbeta(0.975,
                                         3 + alpha,
                                         10 - 3 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               3 + alpha,
                               10 - 3 + beta),
                     y = 0, xend = qbeta(0.025,
                                         3 + alpha,
                                         10 - 3 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_fills +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis
  
  # Calculate y values for both distributions
  prior_y <- dbeta(x_vals,
                   shape1 = 5 + alpha,
                   shape2 = 19 - 5 + beta)
  posterior_y <- dbeta(x_vals,
                       shape1 = 6 + alpha,
                       shape2 = 20 - 6 + beta)
  
  # Create the plot
  p3 = ggplot() +
    geom_ribbon(data = data.frame(
      x = x_vals,
      prior = prior_y,
      posterior = posterior_y
    ),
    aes(x = x,
        ymin = pmin(prior, posterior),
        ymax = pmax(prior, posterior),
        fill = "Difference"),
    alpha = color_alpha) +
    geom_function(aes(colour = "Prior", linetype = "Prior"),
                  fun = dbeta,
                  args = list(shape1 = 5 + alpha,
                              shape2 = 19 - 5 + beta),
                  linewidth = linewidth) +
    geom_function(aes(colour = "Posterior", linetype = "Posterior"),
                  fun = dbeta,
                  args = list(
                    shape1 = 6 + alpha,
                    shape2 = 20 - 6 + beta),
                  linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               5 + alpha,
                               19 - 5 + beta),
                     y = 0, xend = qbeta(0.975,
                                         5 + alpha,
                                         19 - 5 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               5 + alpha,
                               19 - 5 + beta),
                     y = 0, xend = qbeta(0.025,
                                         5 + alpha,
                                         19 - 5 + beta),
                     yend = 3, 
                     color = "Prior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.975,
                               6 + alpha,
                               20 - 6 + beta),
                     y = 0, xend = qbeta(0.975,
                                         6 + alpha,
                                         20 - 6 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    geom_segment(aes(x = qbeta(0.025,
                               6 + alpha,
                               20 - 6 + beta),
                     y = 0, xend = qbeta(0.025,
                                         6 + alpha,
                                         20 - 6 + beta),
                     yend = 3, 
                     color = "Posterior", linetype = "CI"),
                 linewidth = linewidth) +
    shared_theme +
    shared_colors +
    shared_fills +
    shared_linetypes +
    shared_x_axis +
    shared_y_axis

  
  p1 + p2 +p3 + plot_layout(guides = 'collect', nrow = 1) &
    theme(legend.position = "bottom",
          legend.margin = margin(t = -10, b = -10),
          legend.spacing.y = unit(0.1, "cm"),
          legend.key.width = unit(1, "cm"))
}
sequential_updating()
ggsave(filename = "figures/sequential_updating.pdf",
       width = 210-50, height = ((297 - 70)/4) * 0.8, units = "mm", useDingbats = TRUE)


# 1.3 Sampling
set.seed(2112)
steps = 8

# First create the chain with proper MH behavior
taken <- data.frame(iter = 1:steps,
                    theta = NA,
                    proposal = "accepted")

# Initialize first value
taken$theta[1] <- rnorm(1, mean = 42, sd = 0.25)

props_list <- list()
prop_counter <- 1

for(current_iter in 2:steps) {
  # Generate a single proposal
  proposal <- rnorm(1, mean = taken$theta[current_iter-1], sd = 0.5)
  
  # Accept with 70% probability
  if(runif(1) < 0.4) {
    taken$theta[current_iter] <- proposal
  } else {
    # If rejected, store the proposal and keep previous value
    taken$theta[current_iter] <- taken$theta[current_iter-1]
    
    # Store single rejected proposal
    props_list[[prop_counter]] <- data.frame(
      iter = c(current_iter-1, current_iter),
      theta = c(taken$theta[current_iter-1], proposal),
      line = 1,
      proposal = "rejected"
    )
    prop_counter <- prop_counter + 1
  }
}

# Plot
p1 = ggplot() +
  # Add each set of rejected proposals separately
  {if(length(props_list) > 0) {
    lapply(props_list, function(prop_df) {
      list(
        geom_line(aes(x = iter, y = theta, group = line, color = proposal), 
                  data = prop_df, alpha = 0.5),
        geom_point(aes(x = iter, y = theta, color = proposal), 
                   data = prop_df, alpha = 0.5)
      )
    })
  }} +
  # Add accepted chain
  geom_line(aes(x = iter, y = theta, color = proposal), 
            data = taken, linewidth = 0.7) +
  geom_point(aes(x = iter, y = theta, color = proposal), 
             data = taken, size = 2) +
  scale_colour_manual(
    name = "Proposal",
    values = c(
      "accepted" = PALETTE[[6]],
      "rejected" = "grey"
    )) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(41, 43), breaks = c(41, 42, 43)) +
  labs(y = TeX("$\\theta$")) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  theme(legend.position = "none")
p1



mh_norm_par <- function(y, init = 0, iter = 10000) {
  mu_0 <- init # initial point
  mu_t <- c(mu_0, rep(NA, iter - 1)) # pre-allocate the chain-vector
  
  for (i in 2:iter) {
    p_mu_t <- rnorm(1, mean = mu_t[i - 1], 0.5) # proposal at time t
    # we calculate the likelihood of the current proposal, conditional
    # on the data y and the assumed sd of 3 versus the old proposal.
    # mind the sum
    alpha <- min(
      1,
      exp(sum(dnorm(y, mean = p_mu_t, sd = 3, log = TRUE)) -
            sum(dnorm(y, mean = mu_t[i - 1], sd = 3, log = TRUE)))
    )
    if (alpha > runif(1)) {
      mu_t[i] <- p_mu_t # update
    } else {
      mu_t[i] <- mu_t[i - 1] # stay in same place
    }
  }
  return(mu_t)
}

data = list(y = rnorm(200, 42, 3))
mu_samples_1 <- mh_norm_par(data$y, init = 41, iter = 50)
mu_samples_2 <- mh_norm_par(data$y, init = 43, iter = 50)

mu_draws_array <- array(data = c(mu_samples_1, mu_samples_2), 
                        dim = c(length(mu_samples_1), 2, 1),  # dimensions for iterations, chains, variables
                        dimnames = list(iterations = 1:length(mu_samples_1),
                                        chains = c("1", "2"),
                                        variables = "theta"))


p2 = mcmc_trace(mu_draws_array,size = 2)
p2 = p2 + theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text()) +
  labs(x = "iter") +
  scale_y_continuous(limits = c(41, 43), breaks = c(41, 42, 43))
p2

m1 = brm(y ~ 1, data = data, chains = 2, file = "models/m1")


p3 = mcmc_areas(m1, pars = "b_Intercept") +
  coord_flip() +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

(p1 + guides(color = "none") + p2 + p3) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "none",
        legend.margin = margin(t = -10, b = -10),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"))

ggsave(filename = "figures/mcmc.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 1, units = "mm", useDingbats = TRUE)


# 1.4 Diagnostics

## Trace Plots
set.seed(123)
dat <- data.frame(
  Iteration = rep(1:1000, 8),
  Chain = factor(rep(c(1:2, 1:2, 1:2, 1:2), each = 1000)),
  Cond = factor(rep(1:4, each = 2000))
) %>%
  mutate(
    Simulation = c(
      arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      -2 + arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      1 + arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      - + 0.003 * 1:1000 + arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      1 + -0.001 * 1:1000 + arima.sim(list(ar = 0.7), n = 1000, sd = 0.5),
      arima.sim(list(ar = 0.7), n = 1000, sd = 0.8),
      arima.sim(list(ar = 0.7), n = 1000, sd = 0.3)
    )
  )

build_draw_array <- function(sim_data, cond, split = FALSE) {
  cond_frame = filter(sim_data, Cond == cond)
  draws = as_draws(array(
    data = c(
      filter(cond_frame, Chain == 1)$Simulation,
      filter(cond_frame, Chain == 2)$Simulation
    ),
    dim = c(nrow(cond_frame)/2, 2, 1),
    dimnames = list(iterations = 1:length(filter(cond_frame, Chain == 1)$Simulation),
                    chains = c("1", "2"),
                    variables = "theta")
  ))
  if(split){
   return(split_chains(draws))
  } else {
    draws
  }
}

line_size = 0.4
p1 = mcmc_trace(m1, size = line_size, pars = "b_Intercept") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p1

p2 = mcmc_trace(build_draw_array(dat, 2), size = line_size) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p2

p3 = mcmc_trace(build_draw_array(dat, 3), size = line_size) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p3

p4 = mcmc_trace(build_draw_array(dat, 4), size = line_size) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p4

BP1 = (p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 1)
BP1

ggsave(filename = "figures/chains.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 0.65, units = "mm", useDingbats = TRUE)

## Rank and ecdf plots

#p1 = mcmc_rank_overlay(build_draw_array(dat, 1)) +
p1 = mcmc_rank_overlay(m1, pars = c("Intercept")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(30, 70)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
p1

p2 = mcmc_rank_overlay(build_draw_array(dat, 2)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p2

p3 = mcmc_rank_overlay(build_draw_array(dat, 3)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p3

p4 = mcmc_rank_overlay(build_draw_array(dat, 4)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p4

BP2 = (p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 1)
BP2

ggsave(filename = "figures/trace_ranks.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 0.65, units = "mm", useDingbats = TRUE)

#p1 = mcmc_rank_ecdf(build_draw_array(dat, 1), plot_diff = TRUE, interpolate_adj = FALSE) +
p1 = mcmc_rank_ecdf(m1, pars = c("Intercept"), plot_diff = TRUE, interpolate_adj = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
p1

p2 = mcmc_rank_ecdf(build_draw_array(dat, 2), plot_diff = TRUE, interpolate_adj = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p2

p3 = mcmc_rank_ecdf(build_draw_array(dat, 3), plot_diff = TRUE, interpolate_adj = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p3

p4 = mcmc_rank_ecdf(build_draw_array(dat, 4), plot_diff = TRUE, interpolate_adj = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p4

BP3 = (p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 1)
BP3

ggsave(filename = "figures/ecdf_diff.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 0.85, units = "mm", useDingbats = TRUE)


(BP1 / BP2 / BP3) + plot_layout(heights = c(1, 1, 1.3))
ggsave(filename = "figures/vis_diag.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 1.75, units = "mm", useDingbats = TRUE)



rhat(as_draws_array(m1, variable = "b_Intercept"))
ess_bulk(as_draws_array(m1, variable = "b_Intercept"))
ess_tail(as_draws_array(m1, variable = "b_Intercept"))

rhat(build_draw_array(dat, 2))
ess_bulk(build_draw_array(dat, 2))
ess_tail(build_draw_array(dat, 2))

rhat(build_draw_array(dat, 3))
ess_bulk(build_draw_array(dat, 3))
ess_tail(build_draw_array(dat, 3))

rhat(build_draw_array(dat, 4))
ess_bulk(build_draw_array(dat, 4))
ess_tail(build_draw_array(dat, 4))
