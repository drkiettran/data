# Chapter 2 - Data
# Using iris data back in 1936, `https://archive.ics.uci.edu/ml/datasets/Iris`
# From chapter 2 on Data: 
#   `https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/data.html`

install.packages("GGally")
install.packages("sampling")
install.packages("plotly")
install.packages("factoextra")
install.packages("arulesViz")
install.packages("proxy")
install.packages("ggplot2")
install.packages("lattice")
install.packages("ggcorrplot")

# 2.1 The Iris Dataset

library(tidyverse)
data(iris) # This loads the dataset automatically.
iris <- as_tibble(iris) 
iris
str(iris)

# four (4) features(Sepal.Length, Sepal.Width, Pedal.Length, Pedal.Width), and 
#  one (1) label (`Species`)

print(iris, n = 3, width = Inf) # `Inf` -> infinity.

# 2.2 Data Quality
## Assessing data quality via `summary` function
summary(iris)

## Transpose first column into a matrix
str(t(iris[,1]))
quantile(t(iris[,1]))

## first quantile [4.3, 5.1]
## second quantile [5.1, 5.8]
## third quantile [5.8, 6.4]
## fourth quantile [6.4, 7.9]

## 0%  25%  50%  75% 100% 
## 4.3  5.1  5.8  6.4  7.9 

t(iris[,5])

# Summary `specific` columns: In this case excluding non-numeric.
iris %>% summarize_if(is.numeric, mean)

# Drawing
library(GGally)
ggpairs(iris, aes(color = Species))

clean.data <- iris %>% drop_na() %>% unique()
summary(clean.data)
str(clean.data)

# 2.3 Data Aggregation
iris %>% group_by(Species) %>% summarize_all(mean)
iris %>% group_by(Species) %>% summarize_all(median)

# 2.4. Sampling
# 2.4.1 Random Sampling
sample(c("A", "B", "C"), size = 10, replace = TRUE)

# Sampling from existing dataset `without replacement`
take <- sample(seq(nrow(iris)), size = 15)
take

iris[take,]

# slice_sample ... repeatable ...
set.seed(1000)

s <- iris %>% slice_sample(n = 15)
ggpairs(s, aes(color = Species))

## 2.4.2 Stratified Sampling
### Stratified Sampling ... subpopulation group = `Species`
### selecting 5 from each subpopluation
### simple random sampling without replacement (srswor), 
### simple random sampling with replacement (srswr), 
### Poisson sampling (poisson), systematic sampling (systematic); 
### if "method" is missing, the default method is "srswor".
library(sampling)
id2 <- strata(iris, stratanames = "Species", size = c(5,5,5), method = "srswor")
id2
str(id2)
# Now do another ggpairs: 
s2 <- iris %>% slice(id2$ID_unit)
ggpairs(s2, aes(color = Species))


# 2.5 Features:
## 2.5.1 Dimensionality reduction
### 2.5.1.1. PCA
# library(plotly) # don't load the package because it's namespace clashes with select in dplyr.
plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Sepal.Width,
                size = ~Petal.Width, color = ~Species, type="scatter3d")

pc <- iris %>% select(-Species) %>% as.matrix() %>% prcomp()
summary(pc)

plot(pc, type = "line")

str(pc)

iris_projected <- as_tibble(pc$x) %>% add_column(Species = iris$Species)
ggplot(iris_projected, aes(x = PC1, y = PC2, color = Species)) + 
  geom_point()


ggplot(iris_projected, 
  aes(x = PC1, y = 0, color = Species)) + 
  geom_point() +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()
  )

library(factoextra)
fviz_pca(pc)
fviz_pca_var(pc)

### 2.5.1.2 Multi-Dimensional Scaling (MDS)
d <- iris %>% select(-Species) %>% dist()
fit <- cmdscale(d, k = 2)
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column(Species = iris$Species)

ggplot(fit, aes(x = comp1, y = comp2, color = Species)) + geom_point()

## 2.5.2 Feature Selection.
## Non-Parametric Multidimensional Scaling
### Discretize Features
ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = .2)

iris %>% pull(Sepal.Width) %>% cut(breaks = 3)

library(arules)
iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3)

iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3)

iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3)

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept = iris %>% pull(Petal.Width) 
                               %>% discretize(method = "interval", breaks = 3, 
                                              onlycuts = TRUE),
             color = "blue") + labs(title = "Discretization: interval", 
                                    subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept = iris %>% pull(Petal.Width) 
                               %>% discretize(method = "frequency", breaks = 3, 
                                              onlycuts = TRUE),
             color = "blue") + labs(title = "Discretization: frequency", 
                                    subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept = iris %>% pull(Petal.Width) 
                               %>% discretize(method = "cluster", breaks = 3, 
                                              onlycuts = TRUE),
             color = "blue") + labs(title = "Discretization: cluster", 
                                    subtitle = "Blue lines are boundaries")

## 2.5.4 Standardize data

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

iris.scaled <- iris %>% scale_numeric()
iris.scaled

summary(iris.scaled)

# 2.6 Proximities: Dissimilarities & Distances
## 2.6.1. Minkowsky Distances
iris_sample <- iris.scaled %>% select(-Species) %>% slice(1:5)
iris_sample

dist(iris_sample, method = "euclidean")
dist(iris_sample, method = "manhattan")
dist(iris_sample, method = "maximum")

## 2.6.2 Distance for binary data
b <- rbind(
  c(0,0,0,1,1,1,1,0,0,1),
  c(0,0,1,1,1,0,0,1,0,0)
)
b

b_logical <- apply(b, MARGIN = 2, as.logical)
b_logical

### 2.6.2.1 Hamming distance
dist(b, method = "manhattan")
dist(b, method = "euclidean")^2

### 2.6.2.2 Jaccard Index
dist(b, method = "binary") # Jaccard Index

## 2.6.3 Distances for Mixed Data
people <- tibble(
  height = c(      160,    185,    170),
  weight = c(       52,     90,     75),
  sex    = c( "female", "male", "male")
)
people

people <- people %>% mutate_if(is.character, factor)
people

### 2.6.3.1 Gower's Coefficient
library(proxy)
d_Gower <- dist(people, method = "Gower") 
d_Gower

### 2.6.3.2 Eclidean distance with Mixed Data.
library(ggplot2)
# library(lattice)
library(caret)

str(people)
data_dummy <- dummyVars(~., people) %>% predict(people)
data_dummy

weight_matrix <- matrix(c(1, 1, 1/2, 1/2), ncol = 4, nrow = nrow(data_dummy), byrow = TRUE)
data_dummy_scaled <- scale(data_dummy) * weight_matrix

d_dummy <- dist(data_dummy_scaled)
d_dummy

ggplot(tibble(d_dummy, d_Gower), aes(x = d_dummy, y = d_Gower)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## 2.6.4 Additional proximity Measures Avaiable in Package `proxy`
### Package `proxy` implements these distances ...
library(proxy)
pr_DB$get_entry_names()

# 2.7 Relationship between Features.
## 2.7.1 Correlation
cc <- iris %>% select(-Species) %>% cor()
cc

ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  geom_point() +
  geom_smooth(method = "lm")

with(iris, cor(Petal.Length, Petal.Width))
with(iris, cor.test(Petal.Length, Petal.Width)) # Significant tests

## 2.7.2 Rank Correlation
iris_ord <- iris %>% mutate_if(is.numeric, 
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), 
                                               ordered = TRUE))

iris_ord
summary(iris_ord)
iris_ord %>% pull(Sepal.Length)

## Kendall's Tau Rank Correlation Coefficient.
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "kendall")

## Spearman's Rho
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "spearman")

iris %>% select(-Species) %>% cor()

# 2.8 Density Estimation
ggplot(iris, aes(x = Petal.Length, y = 0)) + geom_point()

## 2.8.1 Histograms
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram() +
  geom_rug(alpha = 1/2)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_bin2d(bins = 10) +
  geom_jitter(color = "red")

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_hex(bins = 10) +
  geom_jitter(color = "red")

## 2.8.2 Kernel Density Estimate (KDE)
ggplot(iris, aes(Petal.Length)) +
  geom_density(bw = .2) +
  geom_rug(alpha = 1/2)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_density_2d_filled() +
  geom_jitter()

# 2.9 Exploring data
## 2.9.1 Basic statistics
summary(iris)

## Mean & stddev sepal length
iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% sd()

mean(c(1, 2, NA, 3, 4, 5))
mean(c(1, 2, NA, 3, 4, 5),  na.rm = TRUE)

iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% mean(trim = .1) # Trim 10% from each end of distribution

iris %>% summarize_if(is.numeric, mean)
iris %>% summarize_if(is.numeric, sd)
iris %>% summarize_if(is.numeric, list(min = min, median = median, max = max))

iris %>% summarize_if(is.numeric, mad) # mad=Median Absolute Deviation

# 2.9.2 Grouping
iris %>% group_by(Species) %>% summarize(across(Sepal.Length, mean))
iris %>% group_by(Species) %>% summarize_all(mean)

## ANOVA (Analysis of Variance)
res.aov <- aov(Sepal.Length ~ Species, data = iris)
summary(res.aov)

TukeyHSD(res.aov)

## 2.9.3 Tabulate data
iris %>% group_by(Species) %>% summarize(n())

## Descretize the data using `cut`
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), 
                                               ordered = TRUE))

iris_ord
summary(iris_ord)

tbl <- iris_ord %>% select(Sepal.Length, Species) %>% table()
tbl

iris_ord %>%
  select(Species, Sepal.Length) %>%
  ### Relationship Between Nominal and Ordinal Features
  pivot_longer(cols = Sepal.Length) %>%
  group_by(Species, value) %>% count() %>% ungroup() %>%
  pivot_wider(names_from = Species, values_from = n)

## Pearson's chi-squared test
tbl %>% chisq.test()

## Fisher's exact test
fisher.test(tbl)

## 2.9.4 Percentiles (Quantiles)
iris %>% pull(Petal.Length) %>% quantile()
iris %>% summarize(IQR = quantile(Petal.Length, probs = 0.75) - 
                         quantile(Petal.Length, probs = 0.25))

# 2.10 Visualization
## 2.10.1 Historgram
ggplot(iris, aes(Petal.Width)) + geom_histogram(bins = 20)

## 2.10.2 Boxplot
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()

## Group-wise medians
iris %>% group_by(Species) %>% summarize_if(is.numeric, median)

library(tidyr)
iris_long <- iris %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Original value")


#library(tidyr)
iris_long_scaled <- iris %>% scale_numeric() %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long_scaled, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Scaled value")


## 2.10.3 Scatter plot
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point()

## 2.10.4 Scatter plot Matrix
library("GGally")
ggpairs(iris,  aes(color = Species))


## 2.10.5 Data Matrix Visualation
iris_matrix <- iris %>% select(-Species) %>% as.matrix()

iris_long <- as_tibble(iris_matrix) %>% mutate(id = row_number()) %>% pivot_longer(1:4)
head(iris_long)

ggplot(iris_long,
       aes(x = name, y = id, fill = value)) + geom_tile()

library(seriation)

ggpimage(iris_matrix, prop = FALSE)

iris_scaled <- scale(iris_matrix)
ggpimage(iris_scaled, prop = FALSE)

ggpimage(iris_scaled, order = seriate(iris_scaled), prop = FALSE)

## 2.10.6 Correlation Matrix
cm1 <- iris %>% select(-Species) %>% as.matrix %>% cor()
cm1

library(ggcorrplot)
ggcorrplot(cm1)

gghmap(cm1, prop = TRUE)

cm2 <- iris %>% select(-Species) %>% as.matrix() %>% t() %>% cor()
ggcorrplot(cm2)

## 2.10.7 Parallel Coordinates Plot
library(GGally)
ggparcoord(iris, columns = 1:4, groupColumn = 5)

o <- seriate(as.dist(1-cor(iris[,1:4])), method = "BBURCG")
get_order(o)

ggparcoord(iris, columns = get_order(o), groupColumn = 5)
