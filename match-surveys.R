library(tidyverse)
library(readxl)

d1 = read_csv("data/2015.csv")

d2 = read_excel("data/2024.xlsx")


# Filter to focal rows and cols
d1 = d1 |>
  filter(experiment == "BDF") |>
  select(id, block, plot = seedlot, ht_15 = ht.15) |>
  mutate(id = as.numeric(id)) |>
  mutate(ht_15 = as.numeric(ht_15))

d2 = d2 |>
  select(id = Tree,
         block = Block,
         plot = Plot,
         ht_24 = Height,
         conf = Confidence,
         notes = Notes) |>
  mutate(id = as.character(id)) |>
  mutate(id = as.numeric(id)) |>
  mutate(block_plot = paste0(block, "_", plot)) |>
  mutate(conf_cat = case_when(conf == "5" ~ "high",
                              conf %in% c("1", "2", "3") ~ "low",
                              .default = "unk")) |>
  mutate(ht_24 = as.numeric(ht_24))

d = left_join(d1, d2) |>
  mutate(across(c(ht_15, ht_24), as.numeric))


ggplot(d, aes(x = ht_15, y = ht_24, color = block_plot, pch = conf_cat)) +
  geom_point() +
  scale_shape_manual(values=c(16, 1, 4)) +
  geom_abline(slope = 1, intercept = 0)

sum(is.na(d$ht_15) | is.na(d$ht_24))


## When was it missing in 15 but not 24?
d = d |>
  mutate(appeared = is.na(ht_15) & !is.na(ht_24)) |>
  mutate(missing_15 = ifelse(is.na(ht_15), "missing", "")) |>
  mutate(missing_24 = ifelse(is.na(ht_24), "missing", "")) |>
  mutate(problem = ifelse(appeared, "appeared", ""))


## Test cor in AN if d2 ID is shifted

# Initial cor
d_unshift = d |>
  filter(block_plot == "A_N")
cor(d_unshift$ht_15, d_unshift$ht_24, use = "complete.obs")
ggplot(d_unshift, aes(x = ht_15, y = ht_24)) +
  geom_point()

# Shifted cor
d1_shift = d1 |>
  mutate(id = id + 1)
d_shift = left_join(d1_shift, d2) |>
  mutate(across(c(ht_15, ht_24), as.numeric)) |>
  filter(block_plot == "A_N")
cor(d_shift$ht_15, d_shift$ht_24, use = "complete.obs")
ggplot(d_shift, aes(x = ht_15, y = ht_24)) +
  geom_point()
# Doesn't help


## Tree ring type figure

# NA (missing) heights to 0
d2 = d |>
  mutate(across(c(ht_15, ht_24), ~ifelse(is.na(.x), 0, .x)))

# Long form
d2 = d2 |>
  pivot_longer(cols = c(ht_15, ht_24), values_to = "height",
               names_to = "year") |>
  mutate(year = str_sub(year, 4, 5))

# Double the yr_15 ht so it sivually aligns
d2[d2$year == "15", "height"] = 4 * d2[d2$year == "15", "height"] 



p = ggplot(d2, aes(y = height, x = id, color = year)) +
  geom_point(aes(shape = conf_cat), size = 3) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~block_plot, ncol = 1) +
  geom_vline(xintercept = c(7.5, 14.5, 21.5, 28.5, 35.5, 42.5)) +
  scale_shape_manual(values=c(16, 1, 4)) +
  theme_bw(12)

png("figures/series-comp.png", width = 1600, height = 1200, res = 150)
p
dev.off()

