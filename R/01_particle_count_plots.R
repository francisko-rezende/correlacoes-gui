list_of_packages <- c("tidyverse", "here", "readxl", "ggbeeswarm")
new_packages <-
  list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

library(tidyverse)
library(here)
library(readxl)
library(ggbeeswarm)

vars_count_categories <- c("`<2.5`",
                           "`2.5-10`",
                           "`10-100`")

particle_count_levels <- c("<2.5",
                           "2.5-10",
                           "10-100",
                           "total")



readxl::read_xlsx(here::here("data", "Contagem final.xlsx")) %>%
  dplyr::rename(
    sp = Sp,
    face = Face,
    frag = Frag,
    site = Site
  ) %>%
  dplyr::mutate(
    site = dplyr::case_when(
      site == "BR" ~ 1,
      site == "CA" ~ 2,
      site == "VZ" ~ 3,
      site == "CN" ~ 4,
      site == "REF" ~ 5
    ),
    `<2.5` = as.numeric(`<2.5`),
    `2.5-10` = as.numeric(`2.5-10`),
    `10-100` = as.numeric(`10-100`),
    total = `<2.5` + `2.5-10` + `10-100`
  ) %>%
  tidyr::pivot_longer(cols = `<2.5`:total,
                      names_to = "particle_category",
                      values_to = "count") %>%
  dplyr::mutate(
    across(where(is.character), as.factor),
    particle_category = factor(particle_category, levels = particle_count_levels),
    face = factor(face, levels = c("ABA", "ADA")),
    site = as.factor(site)
  ) -> particle_count



# particle_count %>%
#   filter(particle_category == particle_count_levels[1]) %>%
#   ggplot(aes(x = site, y = count, color = face)) +
#   labs(title = paste0(particle_count_levels[1])) +
#   ggbeeswarm::geom_quasirandom(dodge.width = .8,
#                                groupOnX = T,
#                                size = 2) +
#   facet_wrap(~sp)
# 
# ggsave(paste0(here::here("plots/"), particle_count_levels[1], ".png"), dpi = 300, width = 13.1, height =  8, units = "in")
# 

for (i in 1:4) {
  particle_count %>%
    filter(particle_category == particle_count_levels[{i}]) %>%
    ggplot(aes(x = site, y = count, color = face)) +
    labs(title = paste0(particle_count_levels[{i}])) +
    ggbeeswarm::geom_quasirandom(dodge.width = .8,
                                 groupOnX = T,
                                 size = 2) +
    facet_wrap(~sp)
  
  ggsave(paste0(here::here("plots/"), particle_count_levels[{i}], ".png"), dpi = 300, width = 13.1, height =  8, units = "in")
  
}
