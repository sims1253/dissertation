library(ggplot2)
library(ggthemes)
library(patchwork)
library(latex2exp)
library(purrr)

PALETTE = palette.colors(palette = "Okabe-Ito")

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

