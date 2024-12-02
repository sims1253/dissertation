library(ggplot2)
library(ggthemes)
library(patchwork)
library(latex2exp)
library(purrr)
library(bayesplot)
library(brms)
library(posterior)

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


## 1.1.4 MCMC
set.seed(2025)
steps = 5

# First create the taken data
taken <- data.frame(iter = 1:steps,
                    theta = rnorm(steps, mean = 42, sd = 0.25),
                    proposal = rep("accepted", steps))

# Create multiple proposals (let's say 10 different lines)
n_proposals <- 2
prop_1 <- data.frame(
  iter = rep(c(1, 2), each = n_proposals),
  theta = c(rep(taken$theta[[1]], n_proposals), rnorm(n_proposals, mean = taken$theta[[1]], sd = 0.25)),
  line = rep(1:n_proposals, 2),  # Add line identifier for grouping
  proposal = rep("rejected", 2*n_proposals)
)

n_proposals <- 4
prop_2 <- data.frame(
  iter = rep(c(2, 3), each = n_proposals),
  theta = c(rep(taken$theta[[2]], n_proposals), rnorm(n_proposals, mean = taken$theta[[2]], sd = 0.25)),
  line = rep(1:n_proposals, 2),
  proposal = rep("rejected", 2*n_proposals)# Add line identifier for grouping
)

n_proposals <- 1
prop_3 <- data.frame(
  iter = rep(c(3, 4), each = n_proposals),
  theta = c(rep(taken$theta[[3]], n_proposals), rnorm(n_proposals, mean = taken$theta[[3]], sd = 0.25)),
  line = rep(1:n_proposals, 2),
  proposal = rep("rejected", 2*n_proposals)# Add line identifier for grouping
)

n_proposals <- 3
prop_4 <- data.frame(
  iter = rep(c(4, 5), each = n_proposals),
  theta = c(rep(taken$theta[[4]], n_proposals), rnorm(n_proposals, mean = taken$theta[[4]], sd = 0.25)),
  line = rep(1:n_proposals, 2),
  proposal = rep("rejected", 2*n_proposals)# Add line identifier for grouping
)

# Plot with the proposals grouped by line
p1 = ggplot() +
  geom_line(aes(x = iter, y = theta, group = line, color = proposal), data = prop_1, alpha = 0.5) +
  geom_point(aes(x = iter, y = theta, color = proposal), data = prop_1, alpha = 0.5) +
geom_line(aes(x = iter, y = theta, group = line, color = proposal), data = prop_2, alpha = 0.5) +
  geom_point(aes(x = iter, y = theta, color = proposal), data = prop_2, alpha = 0.5) +
geom_line(aes(x = iter, y = theta, group = line, color = proposal), data = prop_3, alpha = 0.5) +
  geom_point(aes(x = iter, y = theta, color = proposal), data = prop_3, alpha = 0.5) +
  geom_line(aes(x = iter, y = theta, group = line, color = proposal), data = prop_4, alpha = 0.5) +
  geom_point(aes(x = iter, y = theta, color = proposal), data = prop_4, alpha = 0.5) +
  geom_line(aes(x = iter, y = theta, color = proposal), data = taken,  linewidth = 0.7) +
  geom_point(aes(x = iter, y = theta, color = proposal), data = taken, size = 2) +
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

data = list(y = rnorm(1000, 42, 3))
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


m1 = brm(y ~ 1, data = data, chains = 2)

p3 = mcmc_areas(m1, pars = "b_Intercept", size = 2) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text()) +
  labs(x = TeX("$\\theta$"))

p3 = mcmc_trace(m1, pars = "b_Intercept", size = 2)
p3 = p3 + theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text()) +
  labs(x = "iter")

(p1 + guides(color = "none") + p2 + p3) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom",
        legend.margin = margin(t = -10, b = -10),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"))

ggsave(filename = "figures/mcmc.pdf",
       width = (210-50)*1.2, height = ((297 - 70)/4) * 1, units = "mm", useDingbats = TRUE)
