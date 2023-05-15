
library(tidyverse)
library(readxl)
library(broom)
library(minpack.lm)

d <- read_excel("./data/Task 2. Release kinetics active paper_CARVACROL_050922.xlsx",
                sheet = "abierto_forR") 

d %>%
    group_by(day, temperature, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc)) %>%
    ggplot(aes(x = day, y = m_conc, colour = factor(dose))) +
    geom_point(aes()) +
    geom_errorbar(aes(
                      ymin = m_conc - sd_conc,
                      ymax = m_conc + sd_conc)
    ) +
    geom_line(aes()) +
    facet_wrap("temperature") +
    scale_y_log10()

summ_data <- d %>%
    filter(day > 0) %>%
    group_by(day, temperature, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc))

aa <- d %>%
    filter(day == 0) %>%
    select(-temperature) %>%
    group_by(day, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc))

d %>% 
    group_by(temperature) %>% 
    summarize() %>%
    ungroup() %>%
    na.omit() %>%
    mutate(day = 0) %>%
    full_join(aa, .) %>%
    bind_rows(., summ_data) %>% 
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("Initial dose:", dose)) %>%
    ggplot(aes(x = day, y = m_conc)) +
    geom_point() +
    geom_errorbar(aes(
        ymin = m_conc - sd_conc,
        ymax = m_conc + sd_conc)
    ) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(dose~temperature, scales = "free_x") +
    scale_y_log10()

d %>% 
    group_by(temperature) %>% 
    summarize() %>%
    ungroup() %>%
    na.omit() %>%
    mutate(day = 0) %>%
    full_join(aa, .) %>%
    bind_rows(., summ_data) %>% 
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("Initial dose:", dose)) %>%
    ggplot(aes(x = day, y = m_conc, colour = dose)) +
    geom_point() +
    geom_errorbar(aes(
        ymin = m_conc - sd_conc,
        ymax = m_conc + sd_conc)
    ) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~temperature, scales = "free_x") +
    scale_y_log10()

## Put the t=0

aa <- d %>%
    filter(day == 0) %>%
    select(-temperature)

d_model <- d %>% 
    group_by(temperature) %>% 
    summarize() %>%
    ungroup() %>%
    na.omit() %>%
    mutate(day = 0) %>%
    full_join(aa, .) %>%
    bind_rows(.,
              filter(d, day > 0)
    )  %>%
    filter(dose > 100) %>%  # horizontal line
    # filter(day < 140)
    # filter(day < 115)
    filter(conc >= 10)  # LOC
 
d_model %>% 
    filter(day > 0) %>%
    ggplot(aes(x = day, y = conc, colour = factor(dose))) +
    geom_point() +
    # geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~temperature, scales = "free_x", nrow = 1) +
    scale_y_log10() +
    geom_line()

## 

d_model %>% 
    # filter(day > 0) %>%
    # filter(temperature != 15) %>%
    mutate(logconc = log(conc)) %>%
    group_by(day, dose, temperature) %>%
    summarize(m_conc = mean(logconc, na.rm = TRUE), sd_conc = sd(logconc, na.rm = TRUE)) %>%
    ggplot(aes(x = day, y = m_conc, colour = factor(dose))) +
    geom_point() +
    # geom_smooth(method = "lm", se = FALSE) +
    geom_line() +
    geom_errorbar(aes(ymin = m_conc - sd_conc, ymax = m_conc + sd_conc)) +
    facet_wrap(~temperature, scales = "free_x", nrow = 1)

## Excel observations

d_model %>% 
    # filter(day > 0) %>%
    # filter(temperature != 15) %>%
    mutate(logconc = log(conc)) %>%
    group_by(day, dose, temperature) %>%
    summarize(mean.logconc = mean(logconc, na.rm = TRUE), std.error = sd(logconc, na.rm = TRUE)) %>%
    write_excel_csv("concentration_abierto.csv")

## Models

get_residuals <- function(par, temp, d) {
    
    d_model %>%
        # filter(day > 0) %>%
        mutate(logconc = log(conc)) %>%
        filter(temperature == temp, dose == d) %>%
        # group_by(day) %>%
        # summarise(logconc = mean(logconc, na.rm = TRUE)) %>%
        mutate(pred = par$logC0 - (par$k*day)^par$n) %>%
        mutate(res = pred - logconc) %>%
        pull(res)
}


my_models <- expand.grid(temp = c(2, 8, 15, 22), dose = c(500, 1000)) %>%
    mutate(i = row_number()) %>%
    split(.$i) %>%
    map(.,
        ~ nls.lm(par = list(n = 1, k = 1, logC0 = 4),
                 fn = get_residuals,
                 temp = .$temp, d = .$dose,
                 control = nls.lm.control(maxiter = 500))
    )

## Figure 3

my_preds <- my_models %>%
    map(., ~.$par) %>%
    map(., 
        ~ tibble(day = seq(0, 150, length = 100),
                 logC = .$logC0 - (.$k*day)^.$n)
        ) %>%
    imap_dfr(., ~ mutate(.x, cond = .y)) 

p <- expand.grid(temp = c(2, 8, 15, 22), dose = c(500, 1000)) %>% 
    mutate(cond = as.character(row_number())) %>%
    full_join(., my_preds) %>%
    rename(temperature = temp) %>% 
    mutate(temperature = paste0(temperature, "ºC")) %>%
    mutate(temperature = factor(temperature, 
                                levels = c("2ºC", "8ºC", "15ºC", "22ºC")
                                )) %>%
    ggplot() +
    geom_line(aes(x = day, y = logC, colour = factor(dose))) +
    facet_wrap("temperature") +
    theme_bw(base_size = 14) +
    # ggthemes::theme_clean(base_size = 16) +
    theme(legend.position = "none") +
    ylab(bquote(ln~of~carvacrol~concentration~(mg~m^-2))) +
    xlab("Storage time (days)")

my_points <- d_model %>%
    mutate(temperature = paste0(temperature, "ºC")) %>%
    mutate(temperature = factor(temperature, 
                                levels = c("2ºC", "8ºC", "15ºC", "22ºC")
    )) %>%
    mutate(logc = log(conc)) %>%
    group_by(temperature, day, dose) %>%
    summarize(m_conc = mean(logc, na.rm = TRUE),
              sd_conc = sd(logc, na.rm = TRUE)) %>%
    geom_point(aes(x = day, y = m_conc, colour = factor(dose)),
               data = .)

my_bars <- d_model %>%
    mutate(temperature = paste0(temperature, "ºC")) %>%
    mutate(temperature = factor(temperature, 
                                levels = c("2ºC", "8ºC", "15ºC", "22ºC")
    )) %>%
    mutate(logc = log(conc)) %>%
    group_by(temperature, day, dose) %>%
    summarize(m_conc = mean(logc, na.rm = TRUE),
              sd_conc = sd(logc, na.rm = TRUE)) %>%
    geom_errorbar(aes(x = day, ymin = m_conc - sd_conc, 
                      ymax = m_conc + sd_conc, colour = factor(dose)),
               data = .)

p + my_points + my_bars

## Data for plot

expand.grid(temp = c(2, 8, 15, 22), dose = c(500, 1000)) %>% 
    mutate(cond = as.character(row_number())) %>%
    full_join(., my_preds)  %>%
    split(.$cond) %>%
    imap(., ~ write_excel_csv(.x, file = paste0("cond_", .y, ".csv")))

##

my_pars <- my_models %>%
    map(., ~ summary(.)$coef) %>%
    map(., ~ as_tibble(., rownames = "par")) %>%
    imap_dfr(., ~ mutate(.x, cond = .y))

expand.grid(temp = c(2, 8, 15, 22), dose = c(500, 1000)) %>% 
    mutate(cond = as.character(row_number())) %>%
    full_join(., my_pars) %>%
    ggplot() +
    geom_col(aes(x = factor(temp), y = Estimate, fill = factor(dose)),
             position = "dodge") +
    facet_wrap("par", scales = "free")

## Table parameters

expand.grid(temp = c(2, 8, 15, 22), dose = c(500, 1000)) %>% 
    mutate(cond = as.character(row_number())) %>%
    full_join(., my_pars) %>%
    select(temp, dose, par, Estimate, `Std. Error`) %>%
    write_excel_csv(., file = "pars_abierto.csv")


