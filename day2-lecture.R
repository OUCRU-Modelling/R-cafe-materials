library(tidyverse)

ggplot(
  data = ungroup(summarise(group_by(mtcars, gear), mean_mpg = mean(mpg))),
  aes(x = gear, y = mean_mpg)
) +
  geom_col()

# read in data
covid_cases <- read_rds("data/covid_cases.rds")
covid_cases

View(covid_cases)


pivot_longer(
  covid_cases, 
  cols = starts_with("cases"), 
  names_to = "country", 
  values_to = "cases"
) %>% mutate(
  country = str_remove(country, "cases_"),
  year = year(date),
  month = month(date, label = TRUE)
) %>% group_by(
  country
) %>% summarise(
  total_cases = sum(cases)
) %>% mutate(
  pct_cases = round(total_cases / sum(total_cases) * 100, 3),
  pct_text = paste0(pct_cases, "%")
)





covid_cases %>% 
select(date, cases_chn, cases_vnm, cases_kor) %>% 
pivot_longer( 
  cols = starts_with("cases"), 
  names_to = "country", 
  values_to = "cases"
) %>% mutate(
  country = str_remove(country, "cases_"),
  year = year(date),
  month = month(date, label = TRUE)
) %>% ggplot(aes(x = date, y = cases, group = country, color = country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 20000, 1000), minor_breaks = NULL) +
  theme_bw() +
  ggtitle("Covid cases in 2020")



