
library(tidyverse)
library(readxl)
library(broom)

d <- read_excel("./data/Task 1.1. Release kinetics active paper_CARVACROL_050922.xlsx",
                sheet = "forR_cerrado") 

d %>%
    group_by(day, temperature, humidity, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc)) %>%
    ggplot(aes(x = day, y = m_conc, colour = factor(dose))) +
    geom_point(aes(shape = factor(humidity))) +
    geom_errorbar(aes(linetype = factor(humidity),
                      ymin = m_conc - sd_conc,
                      ymax = m_conc + sd_conc)
    ) +
    geom_line(aes(linetype = factor(humidity))) +
    facet_wrap("temperature")

summ_data <- d %>%
    filter(day > 0) %>%
    group_by(day, temperature, humidity, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc))

aa <- d %>%
    filter(day == 0) %>%
    select(-temperature, -humidity) %>%
    group_by(day, dose) %>%
    summarize(m_conc = mean(conc), sd_conc = sd(conc))

d %>% 
    group_by(temperature, humidity) %>% 
    summarize() %>%
    ungroup() %>%
    na.omit() %>%
    mutate(day = 0) %>%
    full_join(aa, .) %>%
    bind_rows(., summ_data) %>% 
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("Initial dose:", dose)) %>%
    ggplot(aes(x = day, y = m_conc, colour = factor(humidity))) +
    geom_point() +
    geom_errorbar(aes(
                      ymin = m_conc - sd_conc,
                      ymax = m_conc + sd_conc)
    ) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(dose~temperature, scales = "free_x") +
    scale_y_log10()

## Add the d=0

aa <- d %>%
    filter(day == 0) %>%
    select(-temperature, -humidity)

d_model <- d %>% 
    group_by(temperature, humidity, dose) %>% 
    summarize() %>%
    ungroup() %>%
    na.omit() %>%
    mutate(day = 0) %>%
    full_join(aa, .) %>%
    bind_rows(.,
              filter(d, day > 0)
              )

## General model

lm(log(conc) ~ day + dose + day:temperature + day:humidity + day:dose, data = d_model) %>% summary()

## Model for each condition

d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
    ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, condition = .y))

## Excel fits


d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
    ) %>%
    map(coef) %>%
    map(.,
        ~ tibble(t = seq(0, 150, length = 100),
                 logC = .["(Intercept)"] + .["day"]*t
                 )
        ) %>%
    imap(.,
         ~ mutate(.x, condition = .y)
         ) %>%
    map(., ~ separate(., condition, into = c("temp", "hr", "dose"))) %>%
    map2(., 1:length(.),
         ~ write_excel_csv(.x, file = paste0("cond", .y, ".csv"))
         )
    

## Figure 2

d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
        ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    filter(term == "day") %>%
    # separate(condition, into = c("temperature (ºC)", "relative humidity", "initial dose"), sep = "\\|") %>%
    separate(condition, into = c("temp", "hr", "dose"), sep = "\\|") %>%
    select(-statistic, -p.value)  %>%
    mutate(temp = as.numeric(temp)) %>%
    mutate(dose = paste0("Initial dose: ", dose)) %>%
    ggplot(aes(x = temp, y = -estimate, colour = hr)) +
    geom_point() +
    geom_errorbar(aes(ymin = -estimate - std.error, ymax = -estimate + std.error,
                      colour = hr),
                  width = 1) +
    # geom_line() +
    geom_smooth(method = "lm", se = FALSE, linetype = 2) +
    facet_wrap("dose") +
    ggthemes::theme_clean(base_size = 14) +
    theme(legend.position = "none") +
    xlab("Storage temperature (ºC)") +
    ylab("Estimate of k (1/day)") 
    # map(.,
    #     ~ try(nls(logconc ~ a - (day/b)^c,
    #           data = .,
    #           start = list(a = 1, b = .1, c = 1)))
    #     ) %>%
    # map(summary)


##

xref <- 7

d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
    ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    filter(term == "day") %>%
    # separate(condition, into = c("temperature (ºC)", "relative humidity", "initial dose"), sep = "\\|") %>%
    separate(condition, into = c("temp", "hr", "dose"), sep = "\\|") %>%
    select(-statistic, -p.value)  %>%
    mutate(temp = as.numeric(temp)) %>%
    mutate(cond = paste(hr, dose, sep = "|")) %>%
    mutate(estimate = -estimate,
           x = temp - xref) %>%
    split(.$cond) %>%
    map(.,
        ~ lm(estimate ~ x, data = .)
        ) %>%
    map(summary)
    # map(tidy)

## Table 1

d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
    ) %>%
    map2(., ., 
         ~ tidy(.x) %>%
            mutate(R2 = summary(.y)$r.squared)) %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    filter(term == "day") %>%
    separate(condition, into = c("temp", "hr", "dose"), sep = "\\|") %>%
    select(temp, hr, dose, estimate, std.error, R2) %>%
    mutate(temp = as.numeric(temp)) %>%
    arrange(desc(temp), hr, dose) %>%
    mutate(dose = paste0("C", dose)) %>%
    write_excel_csv(., file = "pars_cerrado.csv")

## Table 2

d_model %>%
    filter(dose > 100) %>%
    filter(conc > 10) %>%
    mutate(g = paste(temperature, humidity, dose, sep = "|"),
           logconc = log(conc)) %>%
    split(.$g) %>%
    map(.,
        ~ lm(logconc ~ day, data = .)
    ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    filter(term == "day") %>%
    # separate(condition, into = c("temperature (ºC)", "relative humidity", "initial dose"), sep = "\\|") %>%
    separate(condition, into = c("temp", "hr", "dose"), sep = "\\|") %>%
    select(-statistic, -p.value)  %>%
    mutate(temp = as.numeric(temp)) %>%
    mutate(cond = paste(hr, dose, sep = "|")) %>%
    mutate(estimate = -estimate,
           x = temp - xref) %>%
    split(.$cond) %>%
    map(.,
        ~ lm(estimate ~ x, data = .)
    ) %>%
    map2(., ., 
         ~ tidy(.x) %>%
             mutate(R2 = summary(.y)$r.squared))  %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    separate(condition, into = c("hr", "dose"), sep = "\\|") %>%
    mutate(term = ifelse(term == "x", "a", "Kref")) %>%
    rename(parameter = term) %>%
    select(hr, dose, parameter, estimate, std.error, R2) %>%
    write_excel_csv(., file = "pars_secundario.csv")

## Figure 1

d_model %>% 
    filter(conc > 10) %>%
    mutate(logconc = log(conc)) %>%
    group_by(dose, temperature, humidity, day) %>%
    summarize(m_conc = mean(logconc, na.rm = TRUE), 
              sd_conc = sd(logconc, na.rm = TRUE))  %>%
    ungroup() %>%
    # mutate(col = ifelse(dose == 100, "aa", as.character(humidity))) %>%
    mutate(x = ifelse(dose == 100, NA, day)) %>%
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("C", dose)) %>%
    mutate(temp_dose = paste(temperature, dose, sep = " - ")) %>%
    mutate(temp_dose = factor(temp_dose,
                              levels = c("2ºC - C100", "8ºC - C100", "15ºC - C100", "22ºC - C100",
                                         "2ºC - C500", "8ºC - C500", "15ºC - C500", "22ºC - C500",
                                         "2ºC - C1000", "8ºC - C1000", "15ºC - C1000", "22ºC - C1000")
                              )
           ) %>%
    # mutate(temperature = factor(temperature, levels = c("2ºC", "8ºC", "15ºC", "22ºC")),
    #        dose = factor(dose, levels = c("C100", "C500", "C1000"))) %>%
    ggplot(aes(x = day, y = m_conc, colour = factor(humidity))) +
    geom_point(size = 2) +
    # geom_line() +
    geom_smooth(method = "lm", se = FALSE, aes(x = x), alpha = 0) +
    geom_errorbar(aes(ymin = m_conc - sd_conc, ymax = m_conc + sd_conc)) +
    # geom_smooth(method = "lm", se = FALSE) +
    # facet_wrap(dose~temperature) + #, scales = "free_x") +
    facet_wrap("temp_dose") +
    # scale_y_log10() +
    theme_bw(base_size = 14) +
    # ggthemes::theme_clean(base_size = 16) +
    theme(legend.position = "none") +
    ylab(bquote(ln~of~carvacrol~concentration~(mg~m^-2))) +
    # ylab("Log concentration of carvacrol (mg/m3)") +
    xlab("Storage time (days)")

## Table out

d_model %>% 
    filter(conc > 10) %>%
    mutate(logconc = log(conc)) %>%
    group_by(dose, temperature, humidity, day) %>%
    summarize(m_conc = mean(logconc, na.rm = TRUE), 
              sd_conc = sd(logconc, na.rm = TRUE))  %>%
    ungroup() %>%
    # mutate(col = ifelse(dose == 100, "aa", as.character(humidity))) %>%
    mutate(x = ifelse(dose == 100, NA, day)) %>%
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("C", dose)) %>%
    mutate(temp_dose = paste(temperature, dose, sep = " - ")) %>%
    mutate(temp_dose = factor(temp_dose,
                              levels = c("2ºC - C100", "8ºC - C100", "15ºC - C100", "22ºC - C100",
                                         "2ºC - C500", "8ºC - C500", "15ºC - C500", "22ºC - C500",
                                         "2ºC - C1000", "8ºC - C1000", "15ºC - C1000", "22ºC - C1000")
    )
    ) %>%
    select(-x, -temp_dose) %>%
    write_excel_csv("out_cerrado.csv")

## Table observations

d_model %>% 
    filter(conc > 10) %>%
    group_by(dose, temperature, humidity, day) %>%
    summarize(mean.conc = mean(conc, na.rm = TRUE), std.error = sd(conc, na.rm = TRUE))  %>%
    mutate(temperature = paste0(temperature, "ºC"),
           dose = paste0("Initial dose: ", dose))  %>%
    write_excel_csv(file = "observations_cerrado.csv")

##

# d_model %>%
#     filter(dose > 100) %>%
#     filter(conc > 10) %>%
#     mutate(g = paste(temperature, humidity, sep = "|"),
#            logconc = log(conc)) %>%
#     split(.$g) %>%
#     map(.,
#         ~ lm(logconc ~ day, data = .)
#     ) %>%
#     map(tidy) %>%
#     imap_dfr(., ~ mutate(.x, condition = .y)) %>%
#     filter(term == "day") %>%
#     separate(condition, into = c("temp", "humidity"), sep = "\\|") %>%
#     select(-statistic, -p.value) %>%
#     ggplot() + 
#     geom_col(aes(x =  temp, y = abs(estimate), fill = humidity), position = "dodge")

    