# REPOSITORIO GITHUB

library(tidyverse)
library(survival)
library(coxme)

rm(list = ls())

# Load necessary data
# fec_alta: day nursing home admission
# fec_baja: day nursing home exit (death or other reason)
# futime: follow up time
# exitus: death from all causes
# exitus_otro: death from all causes except COVID-19
# edad: age 
# sexo: biological sex
# id_residencia: nursing home id

load('data/personas.RData')

personas2 <- personas |> 
  mutate(tperiodo1 = case_when(fec_alta <= '2020-03-07' & fec_baja <= '2020-03-07' ~ as.integer(fec_baja - fec_alta), 
                               fec_alta <= '2020-03-07' & fec_baja > '2020-03-07' ~ as.integer(as.Date('2020-03-07') - fec_alta),
                               TRUE ~ NA_integer_), 
         tperiodo2 = case_when(fec_alta <= '2021-02-22' & fec_baja > '2020-03-07' & fec_baja <= '2021-02-22' ~ as.integer(fec_baja - fec_alta), 
                               fec_alta <= '2021-02-22' & fec_baja > '2020-03-07' & fec_baja > '2021-02-22' ~ as.integer(as.Date('2021-02-22') - fec_alta),
                               TRUE ~ NA_integer_), 
         tperiodo3 = case_when(fec_alta <= '2021-12-26' & fec_baja > '2021-02-22' & fec_baja <= as.Date('2021-12-26') ~ as.integer(fec_baja - fec_alta), 
                               fec_alta <= '2021-12-26' & fec_baja > '2021-02-22' & fec_baja > as.Date('2021-12-26') ~ as.integer(as.Date('2021-12-26') - fec_alta), 
                               TRUE ~ NA_integer_), 
         tperiodo4 = if_else(fec_baja > '2021-12-26', as.integer(fec_baja - fec_alta), NA_integer_)) |> 
  mutate(tperiodo1 = if_else(tperiodo1 == 0, tperiodo1 + 0.5, tperiodo1), # If death occurs on the same day as admission, add 0.5 days to the length of stay
         tperiodo2 = if_else(tperiodo2 == 0, tperiodo2 + 0.5, tperiodo2), 
         tperiodo3 = if_else(tperiodo3 == 0, tperiodo3 + 0.5, tperiodo3), 
         tperiodo4 = if_else(tperiodo4 == 0, tperiodo4 + 0.5, tperiodo4))

# Create the dataset for the analysis

tmer_personas <- tmerge(personas2, 
                        personas2, 
                        id = id,
                        exitus = event(futime, exitus), 
                        exitus_otro = event(futime, exitus_otro))

tmer_personas <- tmerge(tmer_personas, 
                        personas2, 
                        id = id,
                        tperiodo = event(tperiodo1), 
                        tperiodo = event(tperiodo2), 
                        tperiodo = event(tperiodo3), 
                        tperiodo = event(tperiodo4))

periodo <- personas2 |> 
  select(id, tperiodo1, tperiodo2, tperiodo3, tperiodo4) |> 
  pivot_longer(cols = starts_with('tperiodo'), 
               values_to = 'tperiodo', 
               names_to = 'periodo_org') |> 
  filter(!is.na(tperiodo)) |> 
  mutate(periodo = as.integer(substr(periodo_org, nchar(periodo_org), nchar(periodo_org))))

surv_residencia <- tmer_personas |> 
  left_join(periodo |> 
              select(id, tperiodo, periodo), 
            by = join_by(id, tstop == tperiodo)) |> 
    mutate(periodo = factor(periodo,
                          levels = 1L:4L,
                          labels = c('Pre COVID-19', 
                                     'COVID-19 Prevacunal', 
                                     'COVID-19 Postvacunal', 
                                     'COVID-19 Postvacunal-Ómicron')), 
         edadt = edad + tstart/365.25) |> # Create age as dependent time covariate
  select(id, fec_alta, fec_baja, 
         edad, edadt, sexo, 
         futime, exitus, exitus_otro, 
         starts_with('tperiodo'),
         tstart, tstop, periodo, id_residencia)

m1 <- coxme(Surv(time = tstart, 
           time2 = tstop, 
           event = exitus) ~ periodo  + edad + sexo + (1|id_residencia), data = surv_residencia)

summary(m1)

confint(m1) |> 
  exp()
