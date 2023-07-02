library(tidyverse)
# Web scraping
library(rvest)
# Graphics that can deal with Hebrew text
library(ragg)
locale("he")

IDI_byyear <- IDI %>% 
  group_by(year, arab_jew) %>% 
  filter(arab_jew != 'אחר') %>% 
  summarise(trust_police = mean(trust_police, na.rm = T),
            trust_gov = mean(trust_gov, na.rm = T))

moked <- tibble(
  year = 2003:2021,
  calls = c(NA, NA, NA,
            NA, NA, NA, 10087191, mean(8411990,8463645),
            8081250, mean(8462931, 8455278, 8455278), mean(9208890, 9218107, 9215914), mean(8230516, 8228476), 7452941,
            6907081, 7062772, 8287527, 8815017, 9283051,
            9265746),
  calls_110 = c(NA, NA, NA,
                NA, NA, NA, NA, NA,
                NA, NA, NA, NA, 375498,
                857826, 1600345, 1825573, 1952490, 2459646,
                2726681),
  answered = c(NA, NA, NA,
               NA, NA, NA, mean(940892, 1470314), mean(1029933, 1625261),
               1781898, 1849291, mean(2111214, 1978657), mean(2286854, 2072764), mean(2153435, 2165956),
               2298233, 2340883, 2563693, 2606551, 2820671,
               2848259)
)

demographics <- tibble(
  year = 2003:2021,
  population = c(6689700, 6809000, 6930100,
                 7053700, 7180100, 7308800, 7485600, 7623600, 
                 7765800, 7910500, 8059500, 8215700, 8380100, 
                 8546000, 8713300, 8882800, 9054000, 9215100, 
                 9364000)
)

IDI %>% 
  left_join(demographics) %>% 
  left_join(moked) %>% 
  mutate(calls_percap = calls/population,
         answered_percap = answered/population) %>% 
  ggplot() +
    geom_area(aes(year, calls_percap, fill = "קריאות למוקד"), color = "grey", alpha = .8) +
    # geom_area(aes(year, answered_percap), fill = "grey2", alpha = .8) +
    geom_smooth(aes(year_precise, trust_police/3, color = "אמון במשטרה")) +
    scale_color_manual(name = "", values = c("blue")) +
    scale_fill_manual(name = "", values = c("grey")) +
    scale_y_continuous("ממוצע קריאות למוקד לאזרח לשנה", 
                       sec.axis = sec_axis(~ .*3, name = "אחוז המדווחים על אמון במשטרה",
                                           breaks = c(1, 2, 3, 4),
                                           labels = c("אין אמון כלל",
                                                      "מעט אמון",
                                                      "אמון מסויים",
                                                      "הרבה אמון"))) +
    coord_cartesian(xlim = c(2010, 2021),
                    ylim = c(.6, 1.4)) +
    labs(x = "שנה", title = "המוקד והציבור: יחסי גומלין",
         caption = "הנתונים באדיבות מרכז ויטרבי לחקר דעת קהל ומדיניות שבמכון הישראלי לדמוקרטיה") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .7),
          plot.caption = element_text(hjust = 3),
          axis.text.y.right = element_text(angle = -90, margin = margin(r = 10)))


