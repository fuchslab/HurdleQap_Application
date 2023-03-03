# Gütefunktion

library(RColorBrewer)
brewer.pal(9, "YlGn")
brewer.pal(9, "PuBu")
display.brewer.pal(9, "YlGn")
display.brewer.pal(9, "PuBu")

group.colors <- c("hurdle-QAP" = "#006837", hurdle = "#41AB5D", "poisson-QAP" = "#045A8D", poisson = "#3690C0")

#------ binDist ---------------------------------------------------------

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pleeq))
p_bin_dist <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binDist)), p_bin_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_bin_dist <= 0.05, 1, 0)
dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

# hurdle
p_bin_dist <- unlist(list.select(allpvalues_hurdle, bin_dist))

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binDist)), p_bin_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_bin_dist <= 0.05, 1, 0)

dat3 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat3$model <- "hurdle"

catData <- full_join(dat2, dat3)


guete_binDist <- ggplot(catData, aes(x = coef_cat, y = prop, col = model, group = model)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = bquote(beta[binDist]), x = "", y = "Fraction of significant p-values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) +
  scale_color_manual(values = group.colors)



#------ binPub ---------------------------------------------------------

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pleeq))
p_bin_pub <- ifelse(v1 < v2, v1, v2)*2


dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binPub)), p_bin_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_bin_pub <= 0.05, 1, 0)
dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

# hurdle
p_bin_pub <- unlist(list.select(allpvalues_hurdle, bin_pub))

# xbin_dist
dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binPub)), p_bin_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_bin_pub <= 0.05, 1, 0)

dat3 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat3$model <- "hurdle"

catData <- full_join(dat2, dat3)


guete_binPub <- ggplot(catData, aes(x = coef_cat, y = prop, col = model, group = model)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = bquote(beta[binPub]), x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) +
  scale_color_manual(values = group.colors)

#-------  poisDist   ------------------------------------

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pleeq))
p_pois_dist <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisDist)), p_pois_dist)) 
dat$coef_cat <- cut(dat$coef, breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                          0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_pois_dist <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

# hurdle
p_pois_dist <- unlist(list.select(allpvalues_hurdle, pois_dist))

# xbin_dist
dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisDist)), p_pois_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_pois_dist <= 0.05, 1, 0)

dat3 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat3$model <- "hurdle"

catData <- full_join(dat2, dat3)

# poission Qap

v1 <- unlist(list.select(allpvalues_poissonQap, x_dist$pgreq))
v2 <- unlist(list.select(allpvalues_poissonQap, x_dist$pleeq))
p_dist <- ifelse(v1 < v2, v1, v2)*2
sum(p_dist <= 0.05)/length(p_dist)

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisDist)), p_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_dist <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "poisson-QAP"

catData <- full_join(catData, dat2)

# poisson
p_dist <- unlist(list.select(allpvalues_poisson, pois_dist))

# xpois_dist
dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisDist)), p_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_dist <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "poisson"

catData <- full_join(catData, dat2)

guete_poisDist <- ggplot(catData, aes(x = coef_cat, y = prop, col = model, group = model)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = bquote(beta[poisDist]), x = "True coefficients", y = "Fraction of significant p-values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "none",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) +
  scale_color_manual(values = group.colors)

#-------  poisPub   ------------------------------------

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pleeq))
p_pois_pub <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisPub)), p_pois_pub)) 
dat$coef_cat <- cut(dat$coef, breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                          0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_pois_pub <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

# hurdle
p_pois_pub <- unlist(list.select(allpvalues_hurdle, pois_pub))

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisPub)), p_pois_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_pois_pub <= 0.05, 1, 0)

dat3 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat3$model <- "hurdle"

catData <- full_join(dat2, dat3)

# poission Qap
v1 <- unlist(list.select(allpvalues_poissonQap, x_pub$pgreq))
v2 <- unlist(list.select(allpvalues_poissonQap, x_pub$pleeq))
p_pub <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisPub)), p_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_pub <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "poisson-QAP"

catData <- full_join(catData, dat2)

# poisson
p_pub <- unlist(list.select(allpvalues_poisson, pois_pub))

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisPub)), p_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_pub <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "poisson"

catData <- full_join(catData, dat2)

guete_poisPub <- ggplot(catData, aes(x = coef_cat, y = prop, col = model, group = model)) +
  geom_point() +
  geom_line() +
  labs(title = bquote(beta[poisPub]), x = "True coefficients", y = "") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "None",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) +
  scale_color_manual(values = group.colors)



#---------------- 
#Plot
p_legend <- ggplot(catData, aes(x = coef_cat, y = prop, col = model, group = model)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = bquote(beta[binDist]), x = "", y = "Fraction of significant p-values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) +
  scale_color_manual(values = group.colors)


legend_b <- get_legend(p_legend)
plot_guete <- grid.arrange(guete_binDist, guete_binPub, guete_poisDist, guete_poisPub, ncol = 2,
                           top = textGrob("stimated power function for model estimates, n = 50", gp = gpar(fontsize = 20)))

p_combined <- plot_grid(plot_guete, legend_b, ncol = 2, rel_widths = c(1, .2))

pdf("Plots/plot_guete_combined_n50.pdf", width = 15, height = 13)
grid.draw(p_combined) 
dev.off()

#____________________________________________________________
#____________________________________________________________
# Hurdle-Qap -----------
# binDist

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_dist$pleeq))
p_bin_dist <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binDist)), p_bin_dist)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_bin_dist <= 0.05, 1, 0)
dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

guete_binDist <- ggplot(dat2, aes(x = coef_cat, y = prop, group = model)) +
  geom_point(col = "#006837") +
  geom_line(col = "#006837") +
  labs(title = bquote(beta[binDist]), x = "", y = "Fraction of significant p-values") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
         legend.position = "None",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) 


#------ binPub 

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xbin_pub$pleeq))
p_bin_pub <- ifelse(v1 < v2, v1, v2)*2


dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_binPub)), p_bin_pub)) 
dat$coef_cat <- cut(dat$coef, 
                    breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_bin_pub <= 0.05, 1, 0)
dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

guete_binPub <- ggplot(dat2, aes(x = coef_cat, y = prop, group = model)) +
  geom_point(col = "#006837") +
  geom_line(col = "#006837") +
  labs(title = bquote(beta[binPub]), x = "", y = "") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "None",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) 

#-------  poisDist

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_dist$pleeq))
p_pois_dist <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisDist)), p_pois_dist)) 
dat$coef_cat <- cut(dat$coef, breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                          0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))
dat$sig <- ifelse(dat$p_pois_dist <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

guete_poisDist <- ggplot(dat2, aes(x = coef_cat, y = prop, group = model)) +
  geom_point(col = "#006837") +
  geom_line(col = "#006837") +
  labs(title = bquote(beta[poisDist]), x = "True coefficients", y = "Fraction of significant p-values") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "None",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1) 


#-------  poisPub

# hurdleQap
v1 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pgreq))
v2 <- unlist(list.select(allpvalues_hurdleQap, xpois_pub$pleeq))
p_pois_pub <- ifelse(v1 < v2, v1, v2)*2

dat <- as.data.frame(cbind("coef" = unlist(list.select(allcoefs, beta_poisPub)), p_pois_pub)) 
dat$coef_cat <- cut(dat$coef, breaks =  c(-0.0032, -0.0028, -0.0024, -0.0021, -0.0019, -0.0016, -0.0013, -0.0010, -0.0007, -0.0004, -0.0002, -0.00001, 
                                          0.00001, 0.0002, 0.0004, 0.0007, 0.0010, 0.0013, 0.0016, 0.0019, 0.0021, 0.0024, 0.0028, 0.0032))

dat$sig <- ifelse(dat$p_pois_pub <= 0.05, 1, 0)

dat2 <- dat %>% arrange(coef_cat) %>% group_by(coef_cat) %>%
  summarize(count_all = n(), count_sig = sum(sig == 1)) %>%
  mutate(prop = count_sig/count_all)
dat2$model <- "hurdle-QAP"

guete_poisPub <- ggplot(dat2, aes(x = coef_cat, y = prop, group = model)) +
  geom_point(col = "#006837") +
  geom_line(col = "#006837") +
  labs(title = bquote(beta[poisPub]), x = "True coefficients", y = "") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "None",
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8))) +
  geom_hline(yintercept = 0.05, lty = "dashed", col = "red", size = 1.1)


pdf("Plots/Guetefkt_hurdleQap_n50.pdf", width = 15, height = 13)
plot_guete <- grid.arrange(guete_binDist, guete_binPub, guete_poisDist, guete_poisPub, ncol = 2,
                          top = textGrob("Estimated power function for hurdle-QAP estimates, n = 50", gp = gpar(fontsize = 20)))
grid.draw(plot_guete) 
dev.off()




