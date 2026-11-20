library(tidyverse)

file_paths <- list.files(path = ".", pattern = "\\.tsv$", full.names = TRUE)

import_corpus_file <- function(file_path) {
  df <- read_tsv(file_path, col_names = FALSE)
  colnames(df) <- c("agreeing", "gen")
  df <- df %>% mutate(position = c("DAT", "LOC", "INS"))
  
  file_name <- basename(file_path)
  table_id <- str_extract(file_name, "\\d+") 
  
  df <- df %>%
    mutate(
      table_id = paste0("table_", table_id),
    )
  
  return(df)
}

corpus_data <- map_dfr(file_paths, import_corpus_file)

period_mapping <- tibble(
  table_id = c("table_1", "table_3", "table_5", "table_6", "table_7",
               "table_8", "table_9", "table_10", "table_11", "table_12",
               "table_13", "table_14"),
  period = c(
    rep("15th-18th", 5), 
    rep("19th-early 20th", 2),
    rep("20th-21st", 3),
    rep("21st", 2)
  )
)

corpus_data <- corpus_data %>%
  left_join(period_mapping, by = "table_id") %>%
  mutate(period = factor(period, levels = c("15th-18th", "19th-early 20th", "20th-21st", "21st")))

overall_stats <- corpus_data %>%
  group_by(position) %>%
  summarise(
    total_agreeing = sum(agreeing),
    total_gen = sum(gen),
    agreeing_percentage = (total_agreeing / (total_agreeing + total_gen)) * 100,
    gen_percentage = (total_gen / (total_agreeing + total_gen)) * 100
  )

diachronic_stats <- corpus_data %>%
  group_by(period, position) %>%
  summarise(
    total_agreeing = sum(agreeing),
    total_gen = sum(gen),
    agreeing_percentage = round((total_agreeing / (total_agreeing + total_gen)) * 100, 1),
    gen_percentage = round((total_gen / (total_agreeing + total_gen)) * 100, 1),
    .groups = "drop"
  )


for (pos in c("DAT", "LOC", "INS")) {
  prefix <- tolower(pos)
  
  pos_df <- corpus_data %>% 
    filter(position == pos) %>%
    group_by(period) %>%
    summarise(agreeing = sum(agreeing), gen = sum(gen)) %>%
    column_to_rownames("period")
  
  pos_chisq <- chisq.test(pos_df)
  round(pos_chisq$residuals, 2)
  pos_fisher <- fisher.test(pos_df, simulate.p.value = TRUE)
  
  assign(paste0(prefix, "_chisq"), pos_chisq)
  assign(paste0(prefix, "_fisher"), pos_fisher)
}


library(dplyr)
library(ggplot2)

diachronic_stats <- diachronic_stats %>%
  rowwise() %>%
  mutate(
    ci_low_gen = prop.test(total_gen, (total_agreeing + total_gen), conf.level = 0.95)$conf.int[1],
    ci_high_gen = prop.test(total_gen, (total_agreeing + total_gen), conf.level = 0.95)$conf.int[2],
    ci_low_agreeing = prop.test(total_agreeing, (total_agreeing + total_gen), conf.level = 0.95)$conf.int[1],
    ci_high_agreeing = prop.test(total_agreeing, (total_agreeing + total_gen), conf.level = 0.95)$conf.int[2]
  ) %>%
  ungroup()

diachronic_stats <- diachronic_stats %>%
  mutate(
    position = factor(position, levels = c("DAT", "LOC", "INS"))
  )

p <- ggplot(diachronic_stats,
            aes(x = period, y = gen_percentage / 100,
                color = position, shape = position,
                linetype = position, group = position)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = ci_low_gen, ymax = ci_high_gen),
                width = 0.12, linewidth = 0.5) +
  scale_x_discrete(labels = c("15th to 18th century", "19th to early 20th century",
                              "20th to 21st century", "21st century")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0, 0)) +
  scale_color_manual(values = c("DAT" = "black",
                                "LOC" = "grey40",
                                "INS" = "grey65")) +
  scale_shape_manual(values = c("DAT" = 16, "LOC" = 17, "INS" = 15)) +
  scale_linetype_manual(values = c("DAT" = "solid", "LOC" = "dashed", "INS" = "dotted")) +
  labs(
    x = "Historical period",
    y = "Proportion of nouns in the genitive case",
    color = "Syntactic position",
    shape = "Syntactic position",
    linetype = "Syntactic position"
  ) +
  theme_classic(base_size = 11, base_family = "serif") +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.key.width = unit(1.5, "cm")
  )

ggsave("fig_diachronic.pdf", p, width = 6, height = 4, device = cairo_pdf)


library(broom.mixed)

for (pos in c("DAT", "LOC", "INS")) {
  prefix <- tolower(pos)
  
  df <- diachronic_stats %>%
    filter(position == pos, period %in% c("15th-18th", "21st")) %>%
    select(period, position, total_agreeing, total_gen)
  
  pos_df <- glm(
    cbind(total_gen, total_agreeing) ~ period,
    data = df,
    family = binomial(link = "logit")
  )
  
  assign(paste0(prefix, "_model"), pos_df)
}

get_glm_results <- function(model, pos) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(position = pos) %>%
    filter(term == "period21st") %>%
    select(position, estimate, conf.low, conf.high, p.value)
}

all_glm_results <- bind_rows(
  get_glm_results(dat_model, "DAT"),
  get_glm_results(loc_model, "LOC"),
  get_glm_results(ins_model, "INS")
)
