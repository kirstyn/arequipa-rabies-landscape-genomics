

res_exchange <- merged_summary %>%
  group_by(from) %>%
  summarise(
    total_outgoing = sum(avg_jumps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(name = from) %>%
  left_join(
    rewards %>% select(name, percent_time),
    by = "name"
  )

incoming <- merged_summary %>%
  group_by(to) %>%
  summarise(total_incoming = sum(avg_jumps, na.rm = TRUE), .groups = "drop")

res_exchange <- res_exchange %>%
  left_join(incoming, by = c("name" = "to"))


ggplot(res_exchange, aes(x = percent_time, y = total_outgoing)) +
  
  geom_point(aes(color = name), size = 5) +
  
  geom_text(
    aes(label = name),
    vjust = -1,
    fontface = "bold"
  ) +
  
  scale_color_manual(values = state_cols, name = "State") +
  
  labs(
    x = "Persistence (% time in state)",
    y = "Total outgoing Markov jumps",
    title = "Residence vs Exchange dynamics"
  ) +
  
  theme_minimal(base_size = 12)+
geom_vline(xintercept = mean(res_exchange$percent_time), linetype = "dashed") +
  geom_hline(yintercept = mean(res_exchange$total_outgoing), linetype = "dashed")

