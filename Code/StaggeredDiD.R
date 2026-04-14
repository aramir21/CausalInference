library(did)
library(ggplot2)

data(mpdta)
set.seed(9152024)

out1 <- att_gt(
  yname   = "lemp",
  tname   = "year",
  idname  = "countyreal",
  gname   = "first.treat",
  xformla = NULL,
  data    = mpdta
)

summary(out1)

agg_simple <- aggte(out1, type = "simple")
summary(agg_simple)

agg_group <- aggte(out1, type = "group")
summary(agg_group)

agg_event <- aggte(out1, type = "dynamic")
summary(agg_event)

ggdid(agg_event) +
  labs(
    title = "Event Study: Staggered DiD",
    x = "Periods relative to treatment",
    y = "ATT(g,t)"
  ) +
  theme_minimal()

event_df <- data.frame(
  time = agg_event$egt,
  att  = agg_event$att.egt,
  se   = agg_event$se.egt
)

event_df$upper <- event_df$att + 1.96 * event_df$se
event_df$lower <- event_df$att - 1.96 * event_df$se

ggplot(event_df, aes(x = time, y = att)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Event-Study (Staggered DiD)",
    x = "Time relative to treatment",
    y = "ATT(g,t)"
  ) +
  theme_minimal()


# ================== Nice plot ===================== #
agg_event <- aggte(out1, type = "dynamic")

event_df <- data.frame(
  time = agg_event$egt,
  att  = agg_event$att.egt,
  se   = agg_event$se.egt
)

# z-values
z90 <- 1.645
z95 <- 1.96
z99 <- 2.576

event_df$low90  <- event_df$att - z90 * event_df$se
event_df$high90 <- event_df$att + z90 * event_df$se

event_df$low95  <- event_df$att - z95 * event_df$se
event_df$high95 <- event_df$att + z95 * event_df$se

event_df$low99  <- event_df$att - z99 * event_df$se
event_df$high99 <- event_df$att + z99 * event_df$se

library(ggplot2)

ggplot(event_df, aes(x = time, y = att)) +
  geom_ribbon(aes(ymin = low99, ymax = high99, fill = "99%"), alpha = 0.20) +
  geom_ribbon(aes(ymin = low95, ymax = high95, fill = "95%"), alpha = 0.30) +
  geom_ribbon(aes(ymin = low90, ymax = high90, fill = "90%"), alpha = 0.40) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(
    title = "Event Study: Staggered DiD",
    x = "Time relative to treatment",
    y = "ATT(g,t)",
    fill = "Confidence level"
  ) +
  scale_fill_manual(
    values = c("90%" = "blue", "95%" = "royalblue3", "99%" = "lightblue")
  ) +
  theme_minimal()
