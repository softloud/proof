# density proof for r figure

library(extrafont)
library(ggplot2)
library(tidyverse)
library(xkcd)
conflicted::conflict_prefer("filter", "dplyr")


# bc noise
set.seed(38)

# we'll assume, without loss of generality, |a| < |b|

# this script file is a mess; an unholy mess

# both negative -----------------------------------------------------------



# set up x and y args
a = -2
b = -5

# set up arrow head
arrow_specs <- arrow(
  angle = 30,
  length = unit(0.5, "inches"),
  ends = "first",
  type = "open")

# x and y axes on plot
xrange <- c((a + b - 0.5), 0)
yrange <- c(0, 4)

# preplot calculations

ratioxy <- diff(xrange)/diff(yrange)

steps <- c(0, a, (a+b) / 2, b, a + b)
steps_labels <- c("0", "x", "(x+y) / 2", "y", "x + y") %>%
  as_factor() %>%
  fct_relevel("0", "x", "(x+y) / 2", "y", "x + y")

# plot
plot_dat <- tribble(
  ~y, ~start, ~end, ~line,
  3, 0, a, "a",
  2, 0, b, "b",
  1, b + 0.2, (a + b), "ba"
) %>%
  as_tibble() %>%
  gather(
    key = "terminal",
    value = "x",
    start, end
  )


# data man
mapping <- aes(x=x,
               y=y,
               scale=scale,
               ratioxy=ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg =  anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck)

dataman <- data.frame(x = 1, y = 4,
                      scale = 100,
                      ratioxy = ratioxy,
                      angleofspine =  - pi / 2,
                      anglerighthumerus = 4*pi/6,
                      anglelefthumerus = pi/6,
                      anglerightradius = pi/2,
                      angleleftradius = pi/2,
                      angleleftleg = 3*pi/2  + pi / 12 ,
                      anglerightleg = 3*pi/2  - pi / 12,
                      angleofneck =  3 * pi / 2 - pi/10)

# plot
plot_dat %>%
  ggplot(aes(x = x, y = y, group = line)) +
   geom_line(arrow = arrow_specs) +
  geom_line(
    arrow = arrow_specs,
  data = tibble(
      line = "ba",
      y = 1,
      terminal = c("start", "end"),
      x = c(0, b))) +
  geom_vline(
    data = tibble(
      x = steps,
      steps = steps_labels
    ),
    alpha = 0.25,
    aes(xintercept = x, linetype = steps)
) +
  # geom_text(
  #   data = tibble(
  #
  #   ))
theme(axis.text.y = element_blank(),
      axis.ticks.y = element_blank()) +
  theme_xkcd() +
  scale_x_continuous(breaks = steps,
                     label = steps_labels) +
  # xkcdman(mapping, dataman) +
  # xkcdaxis(xrange = xrange,
  #         yrange = yrange) +
  labs(y = NULL, x = "Steps",
       title = "What if both numbers are negative?")  +
   ylim(0.5, 3.5)

ggsave("negative-steps.png")


# one postive, one negative -----------------------------------------------

a <- -2
b <- 5

arrow_specs <- arrow(
  angle = 30,
  length = unit(0.5, "inches"),
  ends = "last",
  type = "open")


steps <- c(0, a, (a+b) / 2, b, a + b)
steps_labels <- c("0", "x", "(x+y) / 2", "y", "x + y") %>%
  as_factor() %>%
  fct_relevel("0", "x", "(x+y) / 2", "y", "x + y")

# plot
plot_dat <- tribble(
  ~y, ~start, ~end, ~line, ~arrow,
  3, 0, a, "a", "first",
  2, 0, b, "b", "last",
  1.35, b, (a + b), "ba", "first",
  1, 0, b, "ab", "last"
) %>%
  as_tibble() %>%
  gather(
    key = "terminal",
    value = "x",
    start, end
  )


# plot
plot_dat %>%
  ggplot(aes(x = x, y = y, group = line)) +
  geom_line(
    arrow = arrow(
      angle = 30,
      length= unit(0.5, "inches"),
      ends = "first",
      type = "open"
    ),
    data = plot_dat %>% filter(arrow == "first")) +
  geom_line(
    arrow = arrow(
      angle = 30,
      length= unit(0.5, "inches"),
      ends = "last",
      type = "open"
    ),
    data = plot_dat %>% filter(arrow == "last")) +
  geom_vline(
    data = tibble(
      x = steps,
      steps = steps_labels
    ),
    alpha = 0.25,
    aes(xintercept = x, linetype = steps)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_xkcd() +
  scale_x_continuous(breaks = steps,
                     label = steps_labels) +
  # xkcdman(mapping, dataman) +
  # xkcdaxis(xrange = xrange,
  #         yrange = yrange) +
  labs(y = NULL, x = "Steps to the left and right",
       title = "What if one number is negative and one number is positive?")  +
  ylim(0.5, 3.5)

ggsave("analysis/figures/neg-pos-steps.png")
