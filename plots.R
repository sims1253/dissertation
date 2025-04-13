library(ggplot2)
library(ggthemes)
library(patchwork)
library(latex2exp)
library(purrr)
library(bayesplot)
library(brms)
library(posterior)
library(dplyr)
library(tidyr)

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
    # geom_segment(aes(x = qbeta(0.975,
    #                            0 + alpha,
    #                            0 - 0 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      0 + alpha,
    #                                      0 - 0 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            0 + alpha,
    #                            0 - 0 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      0 + alpha,
    #                                      0 - 0 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.975,
    #                            1 + alpha,
    #                            1 - 1 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      1 + alpha,
    #                                      1 - 1 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            1 + alpha,
    #                            1 - 1 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      1 + alpha,
    #                                      1 - 1 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
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
    # geom_segment(aes(x = qbeta(0.975,
    #                            2 + alpha,
    #                            9 - 2 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      2 + alpha,
    #                                      9 - 2 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            2 + alpha,
    #                            9 - 2 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      2 + alpha,
    #                                      9 - 2 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.975,
    #                            3 + alpha,
    #                            10 - 3 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      3 + alpha,
    #                                      10 - 3 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            3 + alpha,
    #                            10 - 3 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      3 + alpha,
    #                                      10 - 3 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
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
    # geom_segment(aes(x = qbeta(0.975,
    #                            5 + alpha,
    #                            19 - 5 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      5 + alpha,
    #                                      19 - 5 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            5 + alpha,
    #                            19 - 5 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      5 + alpha,
    #                                      19 - 5 + beta),
    #                  yend = 3, 
    #                  color = "Prior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.975,
    #                            6 + alpha,
    #                            20 - 6 + beta),
    #                  y = 0, xend = qbeta(0.975,
    #                                      6 + alpha,
    #                                      20 - 6 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
    # geom_segment(aes(x = qbeta(0.025,
    #                            6 + alpha,
    #                            20 - 6 + beta),
    #                  y = 0, xend = qbeta(0.025,
    #                                      6 + alpha,
    #                                      20 - 6 + beta),
    #                  yend = 3, 
    #                  color = "Posterior", linetype = "CI"),
    #              linewidth = linewidth) +
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


# 2.1 Likelihood Examples
likelihood_examples_plot <- function() {
  
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10)) # Center title and adjust size
  
  # Normal Distribution
  p_norm <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = PALETTE[[2]], linewidth = 0.6) +
    base_theme +
    labs(title = "Normal", x = "Value")
  
  # Poisson Distribution
  poisson_lambda <- 4
  poisson_data <- data.frame(x = 0:10, y = dpois(0:10, lambda = poisson_lambda))
  p_pois <- ggplot(poisson_data, aes(x = x, y = y)) +
    geom_bar(stat = "identity", fill = PALETTE[[2]], width = 0.5) +
    base_theme +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) + 
    labs(title = "Poisson", x = "Count")
    
  # Exponential Distribution
  p_exp <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
    stat_function(fun = dexp, args = list(rate = 1), color = PALETTE[[2]],
    linewidth = 0.6) +
    base_theme +
    labs(title = "Exponential", x = "Value")
    
  # Beta Distribution
  p_beta <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
    stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 5), color = PALETTE[[2]], linewidth = 0.6) +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    base_theme +
    labs(title = "Beta", x = "Proportion")

  # Combine plots
  combined_plot <- p_norm + p_pois + p_exp + p_beta + plot_layout(nrow = 1)
  
  return(combined_plot)
}

likelihood_plot <- likelihood_examples_plot()
likelihood_plot

ggsave(filename = "figures/likelihood_examples.pdf",
       plot = likelihood_plot,
       width = 210-50, height = ((297 - 70)/5), units = "mm", useDingbats = TRUE)


# 2.2 Link Function Examples
link_examples_plot <- function() {
  
  # Base theme similar to likelihood_examples_plot
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10))
  
  eta_vals <- c(-5, 5)
  line_color = PALETTE[[2]]
  line_width = 0.6
  
  # Identity Link (Inverse)
  p_identity <- ggplot(data.frame(eta = eta_vals), aes(x = eta)) +
    stat_function(fun = function(eta) eta, color = line_color, linewidth = line_width) +
    base_theme +
    labs()+
    labs(title = "Identity", x = TeX("$\\eta$"), y = TeX("Mean Parameter ($\\mu$)")) +
    coord_cartesian(ylim = eta_vals)

  # Log Link (Inverse: Exponential)
  p_log <- ggplot(data.frame(eta = eta_vals), aes(x = eta)) +
    stat_function(fun = exp, color = line_color, linewidth = line_width) +
    base_theme +
    labs(title = "Log", x = TeX("$\\eta$")) +
    theme(axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, exp(eta_vals[2]))) # Y starts at 0

  # Logit Link (Inverse: Logistic)
  p_logit <- ggplot(data.frame(eta = eta_vals), aes(x = eta)) +
    stat_function(fun = function(eta) 1 / (1 + exp(-eta)), color = line_color, linewidth = line_width) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) + # Specific breaks for [0,1]
    base_theme +
    theme(axis.title.y = element_blank()) + # Keep text for logit
    labs(title = "Logit", x = TeX("$\\eta$")) +
    coord_cartesian(ylim = c(0, 1)) # Y is bounded [0,1]

  # Combine plots
  # Add common y-axis label manually via patchwork annotation if desired, 
  # but individual plots follow the likelihood_examples style for now.
  combined_plot <- p_identity + p_log + p_logit + plot_layout(nrow = 1)
  
  return(combined_plot)
}

link_plot <- link_examples_plot()
link_plot

# Use similar height as likelihood_examples
ggsave(filename = "figures/link_examples.pdf",
       plot = link_plot,
       width = 210-50, height = ((297 - 70)/5), units = "mm", useDingbats = TRUE)


# 2.3 Link Function Step Illustration
link_steps_plot <- function() {

  logistic <- function(eta) 1 / (1 + exp(-eta))
  eta_range <- seq(-6, 6, length.out = 300)
  # Define starting points on the eta scale and the constant step size
  start_etas <- c(-0.75, 2.5)
  delta_eta <- 1.5

  # Data for the main logistic curve
  curve_data <- data.frame(eta = eta_range, mu = logistic(eta_range))

  # Calculate start/end points for eta and mu for each step
  steps_data <- map_df(start_etas, function(eta1) {
    eta2 <- eta1 + delta_eta
    mu1 <- logistic(eta1)
    mu2 <- logistic(eta2)
    data.frame(
      eta1 = eta1, eta2 = eta2, mu1 = mu1, mu2 = mu2,
      delta_mu = mu2 - mu1,
      start_eta_group = factor(eta1) # Group steps by starting eta
    )
  })

  # Pre-generate the TeX labels as a separate vector
  delta_mu_labels <- TeX(sprintf("$\\Delta\\mu \\approx %.2f$", steps_data$delta_mu))

  # Base theme consistent with other plots
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          # Explicitly set axis text and title to black if theme_bw default isn't sufficient
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black")
          )

  p <- ggplot() +
    # Plot the logistic curve - Changed color to "black"
    geom_line(data = curve_data, aes(x = eta, y = mu),
     color = PALETTE[2], linewidth = 0.6) +

    # Horizontal segments representing delta_eta
    geom_segment(data = steps_data,
                 aes(x = eta1, y = mu1, xend = eta2, yend = mu1),
                 arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed"),
                 linewidth = 0.4,
                 color = "black") + # Set fixed color OUTSIDE aes()

    # Vertical segments representing delta_mu
    geom_segment(data = steps_data,
                 aes(x = eta2, y = mu1, xend = eta2, yend = mu2), # Removed color from aes()
                 arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed"),
                 linewidth = 0.4,
                 color = "black") + # Set fixed color OUTSIDE aes()

    # Add text labels for delta_eta
    geom_text(data = steps_data,
      aes(x = eta1 + delta_eta / 2, y = mu1),
      label = TeX("$\\Delta\\eta$"),
      vjust = 1.5, size = 3.5, show.legend = FALSE,
      color = "black") + # Set fixed color OUTSIDE aes()

    # Add text labels for delta_mu, showing the calculated difference
    geom_text(data = steps_data,
      aes(x = eta2, y = mu1 + delta_mu / 4), # Removed color from aes()
      label = delta_mu_labels, # Use pre-generated labels
      hjust = -0.1, size = 3.5, show.legend = FALSE,
      color = "black") + # Set fixed color OUTSIDE aes()

    labs(x = TeX("Linear Predictor ($\\eta$)"),
         y = TeX("Mean Parameter ($\\mu$)")) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), limits = c(0, 1)) +
    base_theme # Theme already sets axis text/titles to black by default or explicitly

  return(p)
}

# Generate the plot object
link_steps_plot_obj <- link_steps_plot()
link_steps_plot_obj

# Save the plot with the specified filename and adjusted dimensions
ggsave(filename = "figures/logit_steps.pdf", 
       plot = link_steps_plot_obj,
       width = 210-80, height = ((297 - 70)/4), units = "mm", useDingbats = TRUE)


# 2.4 Conditional Effects Illustration
conditional_effect_plot <- function(n_draws = 1000, n_points = 100) {
  set.seed(1234) # for reproducibility
  
  # Simulate posterior draws for coefficients (logit scale)
  # Intercept, effect of x1, effect of x2, interaction x1:x2
  beta_draws <- data.frame(
    beta0 = rnorm(n_draws, -0.5, 0.5),
    beta1 = rnorm(n_draws, 0.8, 0.4),
    beta2 = rnorm(n_draws, -0.6, 0.3),
    beta3 = rnorm(n_draws, 0.5, 0.2) 
  ) %>% mutate(draw = 1:n()) # Add draw ID
  
  # Define range for predictor x1 and specific values for x2
  x1_range <- seq(-2, 2, length.out = n_points)
  x2_values <- c(-1, 1) # e.g., low and high values or two groups
  
  # Inverse logit function
  inv_logit <- function(x) 1 / (1 + exp(-x))
  
  # Calculate predictions for each draw, x1 value, and x2 value
  pred_data <- expand.grid(draw = 1:n_draws, x1 = x1_range, x2 = x2_values) %>%
    left_join(beta_draws, by = "draw") %>%
    mutate(
      eta = beta0 + beta1 * x1 + beta2 * x2 + beta3 * x1 * x2,
      mu = inv_logit(eta)
    )
    
  # Summarize predictions (mean and 95% CI)
  summary_data <- pred_data %>%
    group_by(x1, x2) %>%
    summarise(
      mu_mean = mean(mu),
      mu_lower = quantile(mu, 0.025),
      mu_upper = quantile(mu, 0.975),
      .groups = 'drop'
    ) %>%
    mutate(x2_label = factor(x2, levels = x2_values, labels = paste("x2 =", x2_values))) # Create factor for plotting

  # Base theme
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.8, 0.2), # Position legend inside plot
          legend.background = element_rect(fill = alpha("white", 0.5))) # Semi-transparent background

  # Create the plot
  p <- ggplot(summary_data, aes(x = x1, y = mu_mean, color = x2_label, fill = x2_label)) +
    geom_line(linewidth = 0.6) +
    geom_ribbon(aes(ymin = mu_lower, ymax = mu_upper), alpha = 0.2, linetype = 0) + # Ribbons for CI
    scale_color_manual(name = "Condition", values = PALETTE[c(3, 5)]) +
    scale_fill_manual(name = "Condition", values = PALETTE[c(3, 5)]) +
    labs(
      x = TeX("Predictor $x_1$"),
      y = TeX("Predicted Outcome ($\\mu$)")
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    base_theme +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) # Center title

  return(p)
}

# Generate the plot object
conditional_effect_plot_obj <- conditional_effect_plot()
conditional_effect_plot_obj

# Save the plot
ggsave(filename = "figures/conditional_effect_example.pdf", 
       plot = conditional_effect_plot_obj,
       width = 210-80, height = ((297 - 70)/4) * 0.8, units = "mm", useDingbats = TRUE)


# 3.1 Prior Sensitivity Analysis using manual density plots

# Define the function for prior sensitivity plot
prior_sensitivity_plot_manual <- function(n_sims = 15, n_draws_per_sim = 1000) { # Reduced n_sims further for pp_check style
  set.seed(2024) # for reproducibility

  # Define prior settings
  priors <- list(
    Narrow = list(intercept_mean = 0, intercept_sd = 0.1, sigma_mean = 0, sigma_sd = 0.2),
    Reasonable = list(intercept_mean = 0, intercept_sd = 5, sigma_mean = 3, sigma_sd = 1),
    Wide = list(intercept_mean = 0, intercept_sd = 50, sigma_mean = 0, sigma_sd = 50)
  )

  # Simulate prior predictive data
  sim_data_list <- list()
  for (prior_name in names(priors)) {
    prior_params <- priors[[prior_name]]
    
    # Draw parameters from priors
    intercept_draws <- rnorm(n_sims, prior_params$intercept_mean, prior_params$intercept_sd)
    # Ensure sigma is positive and non-zero, using max(0.01, ...) avoids issues with sd=0
    sigma_draws <- pmax(0.01, abs(rnorm(n_sims, prior_params$sigma_mean, prior_params$sigma_sd))) 

    # Simulate outcome data for each parameter draw, adding simulation ID
    y_rep_list <- list()
    for (i in 1:n_sims) {
      y_rep_list[[i]] <- data.frame(
        y_rep = rnorm(n_draws_per_sim, mean = intercept_draws[i], sd = sigma_draws[i]),
        sim_id = i # Add simulation ID
      )
    }
    
    # Combine simulations for this prior
    sim_data_list[[prior_name]] <- bind_rows(y_rep_list) %>%
                                   mutate(prior_type = prior_name)
  }
  
  # Combine all simulation data
  all_sim_data <- bind_rows(sim_data_list) %>%
                  mutate(prior_type = factor(prior_type, levels = names(priors))) # Ensure order

  # Base theme
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(), # Cleaner facet labels
          plot.title = element_text(hjust = 0.5, size = 10))
          
  # Data for the reference N(0, 3) density curve (hypothetical 'observed' data y)
  # Determine a reasonable range based potentially on the wide prior's output
  plot_xlim <- c(-40, 40) 
  ref_data <- data.frame(x = seq(plot_xlim[1], plot_xlim[2], length.out = 400)) %>%
              mutate(y = dnorm(x, mean = 0, sd = 3)) # Hypothetical y ~ N(0, 3)
              
  # Calculate max density for reference curve to set y-limit + buffer
  max_ref_density <- max(ref_data$y)
  plot_ylim <- c(0, max_ref_density * 1.1) # Set y limit based on ref peak + 10%

  # Create the density plot faceted by prior type
  p <- ggplot(all_sim_data, aes(x = y_rep)) +
    # Add reference N(0, 3) density (Observations color, thicker line) - Represents hypothetical y
    geom_line(data = ref_data, aes(x = x, y = y, color = "Observations"), linewidth = 0.6, inherit.aes = FALSE) + # Added aes(color = ...)
    # Add individual prior predictive density lines (y_rep) using geom_line(stat="density") (Prior color)
    geom_line(stat = "density", aes(group = sim_id, color = "Prior Predictive Draws"), alpha = 0.4, linewidth = 0.3) + # Added aes(color = ...)
    facet_wrap(~ prior_type) +
    scale_color_manual(
      name = "", # Legend title (empty)
      values = c(
        "Observations" = PALETTE[[1]], # Keep simple names for matching aes
        "Prior Predictive Draws" = PALETTE[[3]]
      ),
      labels = c( # Use labels argument for TeX formatting
        "Observations" = TeX("Observed data ($y$)"),
        "Prior Predictive Draws" = TeX("Prior predictive draws ($y_{rep}$)")
      )
    ) +
    coord_cartesian(xlim = plot_xlim, ylim = plot_ylim) + # Apply consistent x and y axis limits
    base_theme +
    theme(legend.position = "bottom",
          axis.title.x = element_blank(), # Keep this theme setting to ensure x-axis title is blank
          legend.margin = margin(t = -10)) # Add negative top margin to reduce space

  return(p)
}


# Generate the plot object
prior_sensitivity_plot_manual_obj <- prior_sensitivity_plot_manual()
prior_sensitivity_plot_manual_obj

# Save the plot
ggsave(filename = "figures/prior_sensitivity.pdf", 
       plot = prior_sensitivity_plot_manual_obj,
       # Adjust dimensions for a 3-panel plot (wider)
       width = 210-50, height = ((297 - 70)/4)* 0.8, units = "mm", useDingbats = TRUE)


# 3.2 Posterior Predictive Check Illustration
posterior_predictive_check_plot <- function(n_obs = 100, n_sims = 15) {
  set.seed(100) # For reproducibility

  # 1. Simulate observed data 'y' with outliers (from a t-distribution)
  true_df <- 3
  true_loc <- 0
  true_scale <- 2
  y_obs <- rt(n_obs, df = true_df) * true_scale + true_loc # Scale and shift standard t

  # Convert y_obs to a data frame for ggplot
  y_obs_df <- data.frame(value = y_obs, type = "Observed")

  # 2. Simulate y_rep from a Normal model approximation
  # Use true location but a smaller, fixed SD to represent a model that misses outliers
  narrow_sd <- sd(y_obs) # Fixed narrow standard deviation
  y_rep_norm_list <- replicate(n_sims, rnorm(n_obs, mean = true_loc, sd = narrow_sd), simplify = FALSE)
  y_rep_norm_df <- bind_rows(lapply(1:n_sims, function(i) data.frame(value = y_rep_norm_list[[i]], sim_id = i)), .id = "sim_id_unused") %>%
                   mutate(model = "Normal")

  # 3. Simulate y_rep from a Student-t model (using true parameters)
  y_rep_t_list <- replicate(n_sims, rt(n_obs, df = true_df) * true_scale + true_loc, simplify = FALSE)
  y_rep_t_df <- bind_rows(lapply(1:n_sims, function(i) data.frame(value = y_rep_t_list[[i]], sim_id = i)), .id = "sim_id_unused") %>%
                mutate(model = "Student-t")

  # Combine y_rep data
  all_y_rep_df <- bind_rows(y_rep_norm_df, y_rep_t_df)

  # 4. Plotting
  base_theme <- theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom",
          legend.margin = margin(t = -10))

  # Determine plot limits based on observed data range + buffer
  plot_xlim <- range(y_obs) + c(-1, 1) * sd(y_obs) * 0.5

  # Calculate max density for observed data to set y-limit
  density_y_obs <- density(y_obs)
  plot_ylim <- c(0, max(density_y_obs$y) * 1.1)

  p <- ggplot() +
    # Add individual y_rep density lines (lighter color)
    geom_line(data = all_y_rep_df, aes(x = value, group = sim_id, color = "Simulated"), stat = "density", alpha = 0.3, linewidth = 0.3) +
    # Add observed data density line (darker color)
    geom_line(data = y_obs_df, aes(x = value, color = "Observed"), stat = "density", linewidth = 0.7) +
    facet_wrap(~ model) + # Facet by Normal vs Student-t
    scale_color_manual(
      name = "",
      values = c(
        "Observed" = PALETTE[[1]],
        "Simulated" = PALETTE[[3]] # Using Prior color for simulated draws, consistent with prior sensitivity plot
      ),
      labels = c(
        "Observed" = TeX("Observed data ($y$)"),
        "Simulated" = TeX("Simulated data ($y_{rep}$)")
      )
    ) +
    coord_cartesian(xlim = plot_xlim, ylim = plot_ylim) +
    base_theme

  return(p)
}

# Generate and save the plot
posterior_predictive_check_plot_obj <- posterior_predictive_check_plot()
posterior_predictive_check_plot_obj

ggsave(filename = "figures/posterior_predictive_check.pdf",
       plot = posterior_predictive_check_plot_obj,
       width = 210-50, height = ((297 - 70)/4), units = "mm", useDingbats = TRUE)
