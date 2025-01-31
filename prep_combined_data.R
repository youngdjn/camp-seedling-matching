library(tidyverse)
library(readxl)

d1 = read_csv("data/2015.csv")

d2 = read_excel("data/2024.xlsx")


# Filter to focal rows and cols
d1 = d1 |>
  filter(experiment == "BDF") |>
  filter(is.na(dead.prob.curr)) |> # make sure it's not dead
  mutate(browsed_forked_15 = any(!is.na(browsed.prob.curr), !is.na(forked.prob.curr))) |>
  select(id, block, plot = seedlot, ht = ht.15, diam = d.15,
         shrub = sc, browsed_forked_15) |>
  mutate(id = as.numeric(id)) |>
  mutate(ht = as.numeric(ht),
        diam = as.numeric(diam),
         year = 2015)
  
# Convert shrub cover classes to percentages
# convert shrub cover classes to percentages

d1 = d1 |>
  mutate(shrub = recode(shrub,
                     "A" = 0.5,
                        "B" = 3,
                        "C" = 7.5,
                        "D" = 17.5,
                        "E" = 37.5,
                        "F" = 62.5,
                        "G" = 82.5,
                        "H" = 92.5,
                        "I" = 97.5))


d2 = d2 |>
  select(id = Tree,
         block = Block,
         plot = Plot,
         ht = Height,
         diam = Diameter,
         shrub = `Shrub Cover`) |>
  mutate(id = as.character(id)) |>
  mutate(id = as.numeric(id)) |>
  mutate(ht = as.numeric(ht),
         shrub = as.numeric(shrub),
         diam = as.numeric(diam),
         year = 2024)

d = bind_rows(d1, d2)


## Remove trees with no data (remmebering it might have been dead, skipped, inaccessible, unknown)
# Then add elevation and seedlot
d = d |>
  filter(!is.na(ht)) |>
  filter(!is.na(diam)) |>
  filter(!is.na(shrub)) |>
  mutate(block_plot = paste0(block, "_", plot)) |>
  mutate(species = "JP") |>
  mutate(source_elev = recode(block_plot,
                              "A_N" = 7500,
                              "A_S" = 5500,
                              "B_N" = 7500,
                              "B_S" = 5500,
                              "C_E" = 5500,
                              "C_W" = 7500)) |>
  mutate(seedlot_id = recode(source_elev, 
                             "7500" = "6625",
                             "5500" = "7130"))
  
  
# Seedlot 6625 is from San Bernardino Meridian T1N R2E S23 
# Seedlot 7130 is from San Bernardino Meridian T1N R1E S7 



## Remove year 2024 trees from the edges of each plot from the 2024 data
border_tree_ids = c(1:7, 43:49, 8, 15, 22, 29, 36, 14, 21, 28, 35, 42)

d = d |>
  filter(!(year == 2024 & (id %in% border_tree_ids)))

## Summarize and visualize
summ = d |>
  group_by(year, source_elev, block) |>
  summarise(n = n(),
            mean_ht = mean(ht),
            mean_diam = mean(diam)) |>
  ungroup()


ggplot(summ, aes(x = source_elev, y = mean_ht, color = block)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)










