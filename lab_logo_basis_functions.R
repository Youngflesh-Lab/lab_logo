####################
# Youngflesh Lab logo creation using basis functions
#
# Code by Casey Youngflesh, with assistance from ChatGPT
# Design inspired from the Colburn School (Los Angeles) logo
####################


# load packages -----------------------------------------------------------

library(tidyverse)


# Colors ------------------------------------------------------------------

# palettes: https://coolors.co/palettes/popular/5%20colors

#Sunny Beach Day
# colors <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51")
# #Sunrise Glow
# colors <- c("#233d4d","#fe7f2d","#fcca46","#a1c181","#619b8a")
# Ocean Breeze
# colors <- c("#031d44","#04395e","#70a288","#dab785","#d5896f")
# modified Ocean Breeze
colors <- c("#010b17","#155e75","#70a288","#dab785","#d5896f")

# Coastal Vibes
# colors <- c("#160f29","#246a73","#368f8b","#f3dfc1","#ddbea8")
# #Ocean Serenity
# colors <- c("#01161e","#124559","#598392","#aec3b0","#eff6e0")
# #Refreshing Summer Fun
# colors <- c("#8ecae6","#219ebc","#023047","#ffb703","#fb8500")
# Earthy Forest Hues
# colors <- c("#dad7cd","#a3b18a","#588157","#3a5a40","#344e41")
# modified Earthy Forest Hues
# colors <- c("#dad7cd","#a3b18a","#588157","#2f5d47","#132c24")
# Silent Waters
# colors <- c("#e0fbfc","#c2dfe3","#9db4c0","#5c6b73","#253237")
# Forest Green Tones
# colors <- c("#c9e4ca","#87bba2","#55828b","#3b6064","#364958")


# set order -----------------------------------------------------

x <- seq(0, 1, length.out = 1400)

# Evenly spaced endpoints
ymin <- 1
ymax <- 5
left_offsets  <- seq(ymin, ymax, length.out = 5)   # bottom -> top
right_positions <- seq(ymin, ymax, length.out = 5)

# Mapping: (1 -> 1, 2 -> 5, 3 -> 2, 4 -> 3, 5 -> 4)
mapping <- c(1, 5, 2, 3, 4)
right_offsets <- right_positions[mapping]

# to make ordering of colors based on right side positions rather than left
# colors2 <- colors[mapping]
colors2 <- colors

# Smooth interpolation for endpoints
smoothstep <- function(t) t^2 * (3 - 2 * t)
sx <- smoothstep(x)

# to get the lines from starting position to the ending position
base_curves <- lapply(1:5, function(i) {
  #i <- 1
  (1 - sx) * left_offsets[i] + sx * right_offsets[i]
})

#visualize
# plot(x, base_curves[[1]], type = 'l', col = colors[1], lwd = 2,
#      ylim = c(0, 6), ylab = '')
# lines(x, base_curves[[2]], col = colors[2], lwd = 2)
# lines(x, base_curves[[3]], col = colors[3], lwd = 2)
# lines(x, base_curves[[4]], col = colors[4], lwd = 2)
# lines(x, base_curves[[5]], col = colors[5], lwd = 2)


# create wiggles to add to base curves ------------------------------

# Gaussian wiggles (basis functions) - using 6
mus <- c(0.1, 0.3, 0.4, 0.5, 0.6, 0.75)
sigmas <- c(0.05, 0.05, 0.1, 0.1, 0.1, 0.05)
gauss_basis <- sapply(seq_along(mus),
                      function(k) dnorm(x, mean = mus[k], sd = sigmas[k]))

# #visualize basis functions
# plot(gauss_basis[,1], type = 'l', xlim = c(0, NROW(gauss_basis)))
# for (i in 2:NCOL(gauss_basis))
# {
#   lines(gauss_basis[,i])
# }


# coefs to apply to basis funcs --------------------------------------------

coefs <- matrix(c(
  0.0,   0.0,  0.3,  1.6,  0.0, 0.0, # bottom - dark blue
  0.0,   0.4, -0.5, -2.8,  0.0, 0.0, # 2nd up - light blue
  0.0,   0.0,  3.0, -0.1,  0.0, 0.0, # 3rd up - yellow
  0.0,   0.0,  0.0,  0.3,  2.0, 0.0, # 4th up - orange
  0.0,   0.0, -0.3, -1.7,  0.6, 0.0), # top - red
  nrow = 5, byrow = TRUE)

# Wiggle amplitude
amp <- 0.2


# create curves -----------------------------------------------------------

Y <- lapply(1:5, function(i) {
  #i <- 1
  #base curve
  base <- base_curves[[i]]
  #wiggles to add on top
  wig  <- amp * as.numeric(gauss_basis %*% coefs[i, ])
  #add them
  out <- base + wig
  
  # plot(base, type = 'l', lwd = 2, ylim = range(c(base, out)))
  # lines(out, type = 'l', col = 'red', lwd = 2)
  
  return(out)
})

# Data
dat <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(x = x, y = Y[[i]], id = paste0("L", i))
}))


# add leading/trailing ----------------------------------------------------

# #add leading values that do not vary
# lnum <- seq(-0.4, 0, length.out = 10)
# dat2 <- rbind(dat,
#                   data.frame(x = rep(lnum, 5),
#                              y = c(rep(1, 10),
#                                    rep(2, 10),
#                                    rep(3, 10),
#                                    rep(4, 10),
#                                    rep(5, 10)),
#                              id = c(rep('L1', 10),
#                                     rep('L2', 10),
#                                     rep('L3', 10),
#                                     rep('L4', 10),
#                                     rep('L5', 10)))) %>%
#   dplyr::group_by(id) %>%
#   dplyr::arrange(x) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(id) %>% 
#   dplyr::filter(x <= 0.9)


#add trailing values
tnum <- seq(1, 1.4, length.out = 10)
dat2 <- rbind(dat,
              data.frame(x = rep(tnum, 5),
                         y = c(rep(1, 10),
                               rep(5, 10),
                               rep(2, 10),
                               rep(3, 10),
                               rep(4, 10)),
                         id = c(rep('L1', 10),
                                rep('L2', 10),
                                rep('L3', 10),
                                rep('L4', 10),
                                rep('L5', 10)))) %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(x) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(id) #%>%
# dplyr::filter(x > 0.03)


# plot --------------------------------------------------------------------

p <- ggplot(dat2, aes(x = x, y = y, color = id)) +
  geom_path(size = 3,
            lineend = "round") +
  scale_color_manual(values = colors2) +
  theme_void() +
  theme(legend.position = 'none')

print(p)

ggsave(p, filename = '~/Desktop/logo_r.pdf', 
       width = 6,
       height = 1.5)
