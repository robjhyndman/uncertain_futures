library(fpp3)
set.seed(1)
pdf(file = "figure/cover.pdf", width=5, height=2.5)
austa <- readr::read_csv("http://OTexts.com/fpp3/extrafiles/austa.csv") %>%
  as_tsibble(index = Year)
fit <- austa %>% model(ETS(Visitors))
sim <- fit %>% generate(h = 10, times = 20) %>%
  mutate(
    replicate = factor(.rep, levels = 1:20, labels = paste("Future", 1:20))
  )
ggplot(austa, aes(x = Year)) +
  geom_line(aes(y = Visitors, colour = "Data")) +
  geom_line(aes(y = .sim, colour = replicate), data = sim) +
  scale_colour_manual(values = c("#000000", rainbow(20)),
                      breaks = c("Data", paste("Future", 1:20)),
                      name = " ") +
  guides(color="none") +
  theme_minimal() +
  scale_y_discrete(breaks = "none") +
  scale_x_discrete(breaks = "none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
crop::dev.off.crop()
