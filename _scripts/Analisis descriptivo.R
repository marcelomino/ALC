
names(liensalc)

d <- liensalc %>% select(-starts_with("sl"))
d_ind <- liensalc %>% 
  select(ind_lf, ind_lpe, ind_lpe_alt, ind_lpo, formal, ind_lc)
d_ind

### Estadísticas Descriptivas
# LF
summary(d[, c("lf1",
              "lf2",
              "lf3",
              "lf4",
              "ind_lf")]
        )
# LPE
summary(d[, c("lpe1",
              "lpe2",
              "lpe1_alt",
              "ind_lpe",
              "ind_lpe_alt")]
)
# LPO
summary(d[, c("eccv_oit",
              "formal",
              "ind_lpo")]
)
# LC
summary(d[, c("lc1",
              "lc2",
              "ind_lc")]
)

### Correlaciones indicadores
library("Hmisc")
rcorr(as.matrix(d_ind))

GGally::ggpairs(d_ind, title="Correlogram") 


### Gráficos (scatter) correlaciones significativas ----
library(ggrepel)

# LF vs LPO 
corrgraf_lf_lpo <- d %>% 
  ggplot(aes(x = ind_lpo, 
             y = ind_lf)) +
  geom_point(size = 3L) +
  geom_text_repel(aes(label = code3), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "LF vs LPO") +
  theme_light()
corrgraf_lf_lpo
ggsave(
  filename = "_resultados/corrgraf_lf_lpo_v1.png",
  plot = corrgraf_lf_lpo,
  device = "png",
  scale = 1,
  width = 12,
  height = 10,
  units = "cm",
  dpi = 300
)

# LF vs LC 
corrgraf_lf_lc <- d %>% 
  ggplot(aes(x = ind_lf, 
             y = ind_lc)) +
  geom_point(size = 3L) +
  geom_text_repel(aes(label = code3), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "LF vs LC") +
  theme_light()
corrgraf_lf_lc
ggsave(
  filename = "_resultados/corrgraf_lf_lc_v1.png",
  plot = corrgraf_lf_lc,
  device = "png",
  scale = 1,
  width = 12,
  height = 10,
  units = "cm",
  dpi = 300
)

# LPO vs LC 
corrgraf_lpo_lc <- d %>% 
  ggplot(aes(x = ind_lpo, 
             y = ind_lc)) +
  geom_point(size = 3L) +
  geom_text_repel(aes(label = code3), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "LPO vs LC") +
  theme_light()
corrgraf_lpo_lc
ggsave(
  filename = "_resultados/corrgraf_lpo_lc_v1.png",
  plot = corrgraf_lpo_lc,
  device = "png",
  scale = 1,
  width = 12,
  height = 10,
  units = "cm",
  dpi = 300
)
