# OPTION 1: the og

ggplot(Baynes05) +
  geom_line(aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(aes(x = Date, y = Roll_Temp),
            size = 0.5) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_hline(yintercept = 17, color = "black", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 12, color = "dodgerblue1", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 22, color = "orangered1", size = 1, alpha = 0.8) +
  #ylim(-5,35) +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")

# OPTION 2: removed lines, adding peak temp to 2022

options(ggrepel.max.overlaps = Inf)

ggplot(Baynes05) +
  geom_line(aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(aes(x = Date, y = Roll_Temp),
            size = 0.5) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_label_repel(x = as.Date('2021-08-04'), y = 21.6,
                   data = data.frame(x = 1, y = 1),
                   label = "Max Temp = 21.6ºC",
                   min.segment.length = unit(0, 'lines'),
                   nudge_x = 10, nudge_y = -2,
                   label.r = 0.1,
                   size = 3) + 
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  ylim(4,24) +
  theme_classic() +
  theme(legend.position = "none")

# OPTION 3: multipanel with 20m below

# OPTION 4: both on same graph
ggplot(data = NULL) +
  geom_line(data = Baynes05, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(data = Baynes20, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_line(data = Baynes05, 
            aes(x = Date, y = Roll_Temp),
            col = "black", alpha = 0.8,
            size = 0.5) +
  geom_line(data = Baynes20, 
            aes(x = Date, y = Roll_Temp),
            col = "black", alpha = 1.1,
            size = 0.5, linetype = "dashed") +
  geom_label_repel(x = as.Date('2021-08-04'), y = 21.6,
                   data = data.frame(x = 1, y = 1),
                   label = "Max Temp = 21.6ºC",
                   min.segment.length = unit(0, 'lines'),
                   nudge_x = 10, nudge_y = -2,
                   label.r = 0.1,
                   size = 3) + 
  ylim(5,24) +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")

# OPTION 5: without label
ggplot(data = NULL) +
  geom_line(data = Baynes05, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(data = Baynes20, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_line(data = Baynes05, 
            aes(x = Date, y = Roll_Temp),
            col = "black", alpha = 0.8,
            size = 0.5) +
  geom_line(data = Baynes20, 
            aes(x = Date, y = Roll_Temp),
            col = "black", alpha = 1.1,
            size = 0.5, linetype = "dashed") +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")
