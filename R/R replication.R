# Replicate https://medium.com/@andrew.schleiss/pythons-top-visualization-library-seaborn-80758d336c04
rm(list = ls())

# Load our libraries

library(tidyverse)
library(regclass)
library(GGally)
library(kableExtra)

# Load our data

titanic <- as.data.frame(datasets::Titanic)
data("TIPS")

# Look at the first five rows of each

head(titanic)

head(TIPS)

# Note the equivalent piped notation as python's "." is "%>%". So, for consistency, we could instead say:

titanic %>% head()

TIPS %>% head()

# Note, our Titanic data is different. It is organised according to frequency of each observation combination instead of each row being a single observation, each row is some combination of the four variables and Freq shows how often that occurs.

# We could have tidied it up but we would still only have two categories for age being "Child" and "Adult" instead of the years of age. Instead we will just read the python data in. We exclude the first column because it is row numbers.

titanic = read.csv("Python/titanic.csv", stringsAsFactors = T) %>% 
  select(-1)

# Let's recreate the pairs plot witha  bit of customisation

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.5) 
}

ggpairs(titanic, columns = c("pclass", "age", "survived"), mapping = aes(fill = sex, colour = sex), diag = list(continuous = my_dens))

# Problem with this graph is that it treats categorical variables as numbers. Only age should have a density function.

# Regression and line plots (tried to find a similar colour)

ggplot(TIPS, aes(x = Bill, y = Tip))+
  geom_point(colour = "#3182bd", size = 3)+
  geom_smooth(method = "lm")

# Add a second variable by colour

ggplot(TIPS, aes(x = Bill, y = Tip, colour = Smoker))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")

# Distribution

ggplot(titanic, aes(x = age, fill = sex))+
  geom_histogram(binwidth = 4, colour = "black")

# Can't get the binwidth exactly right to replicate the python one. bins of 4 was closest

# Boxplot

ggplot(titanic, aes(x = sex, y = age, fill = sex))+
  geom_boxplot(outlier.size = 2, varwidth = T)+
  guides(fill = F)

# Not sure why Python changes the order of "sex" from female-male (previous graphs) to male-female here?

# Violin plot split through the middle

# This one was genuinely tough because ggplot2 doesn't have this as an option. I found a greta solution here: https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

ggplot(titanic, aes(x = sex, y = age, fill = factor(survived)))+
  geom_split_violin()+
  scale_fill_discrete(name = NULL, labels = c("1" = "Survived", "0" = "Died"))

# Correlation matrix

corr_df = titanic[c("survived", "pclass", "age", "sibsp", "parch", "fare", "adult_male", "alone")] %>% 
  map_df(., as.numeric)

correlations = cor(corr_df, use = "pairwise.complete.obs")
library(reshape)
test = melt(correlations) %>% 
  mutate(X1 = factor(X1, levels = unique(X1)),
         X2 = factor(X2, levels = rev(unique(X2))))

ggplot(test, aes(x = X1, y = X2, fill = value))+
  geom_tile()+
  scale_fill_gradient2(low = "black", mid = "#c51b8a", high = "white")+
  geom_text()

options(digits = 2)

ggplot(test, aes(x = X1, y = X2, fill = value))+
  geom_tile()+
  scale_fill_distiller(type = "div", palette = "Spectral")+
  # geom_text(aes(X2, X1, label = paste0(round(value*100, 0), "%")), size = 4)+
  labs(x = NULL,
       y = NULL)



