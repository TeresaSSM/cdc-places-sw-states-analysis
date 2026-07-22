library(pacman)
p_load(tidyverse, janitor, skimr, ggthemes, ggpubr, rstatix, purrr, scales)

theme_set(theme_classic())

cdc_full<-read_csv("PLACES_Data.csv")

skim(cdc_full)

cdc<-cdc_full%>%
 filter(Data_Value_Type == "Age-adjusted prevalence")%>%
 select(StateAbbr, LocationName, Category, Data_Value, MeasureId, Short_Question_Text) #the Data_Value_Unit for all these measures is percentage

skim(cdc)

#defining state color palette
state_colors <- c("CA" = "#f8dfa5",  "AZ" = "#deac23", "NM" = "#ec7c2b",   "TX" = "#c23022", "OK" = "#9b2c0a")
#defining measure labels
measure_labels <- c(DIABETES = "Diabetes", OBESITY = "Obesity", BPHIGH = "High Blood Pressure",
  ACCESS2 = "Health Insurance", CHECKUP = "Annual Checkup")

## 1. Distribution of chronic disease burden look like across counties in SW states: 
# What's typical, and how much variation is there?

# Summary table of mean/median/sd/range 
cdc_summary<- cdc%>% 
    group_by(StateAbbr, MeasureId)%>%
    summarize(mean = mean(Data_Value, na.rm = TRUE),
    median = median(Data_Value, na.rm = TRUE),
    sd = sd(Data_Value, na.rm = TRUE),
    max = max(Data_Value, na.rm = TRUE),
    min = min(Data_Value, na.rm = TRUE),
    range = max-min)

view(cdc_summary)

# Fig 1a (overall histogram)
Fig1<-cdc%>%
mutate( StateAbbr = factor(StateAbbr,
      levels = c("TX", "OK", "NM", "AZ", "CA")))%>%
ggplot(aes(x=Data_Value, fill=StateAbbr, color= StateAbbr))+
    geom_histogram(bins = 30, position = "identity", alpha =0.75)+
    scale_fill_manual(values = state_colors)+
    scale_color_manual(values = state_colors)+
    labs(x="Age-adjusted prevalence (%)", y="Count of counties")+
    facet_wrap(Category~MeasureId, scales = "free")

# Fig 1b (boxplots for each metric; facet by state and measure)
Fig2<-cdc %>%
    ggplot(aes(x=StateAbbr, y=Data_Value, fill=StateAbbr, color=StateAbbr))+
    scale_fill_manual(values = state_colors)+
    scale_color_manual(values = state_colors)+
    geom_boxplot(alpha = 0.75)+
    geom_jitter(width=0.2)+
    facet_wrap(Category~Short_Question_Text)+ 
    stat_compare_means(method = "kruskal.test", label = "p.signif", 
    label.y=70,label.x = 2.9)

# TABLE Do the states differ from the overall mean for each measure? (Kruskal-Wallis test, non-parametric)
Table1<-cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(Short_Question_Text) %>%
  summarise(p_value = kruskal.test(Data_Value ~ StateAbbr)$p.value, .groups = "drop")
#they (above) are all significant

#kruskal effect size
Table2<-cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(Short_Question_Text) %>%
  kruskal_effsize(Data_Value ~ StateAbbr)
#the effect size for Checkup  (ε² = 0.73) is the highest but 4/5 are large magnitude!
#diabetes is the lowest, so avg age, pop genetics, etc, may explain diabetes variation more than state.

#Dunn test: pairwise comparisions
#large z-scores in conjunction with pval for significance
Table3<-cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(Short_Question_Text) %>%
  dunn_test(Data_Value ~ StateAbbr) %>%
  filter(p.adj < 0.05, abs(statistic)>10)%>%
  select(!c(.y.,p, p.adj.signif))%>%
  print(n=Inf)

# TABLE of medians of the states - so we can see visually which ones are diff from avg
Table4<-cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(Short_Question_Text, StateAbbr) %>%
  summarise(median_value = median(Data_Value), .groups = "drop") %>%
  pivot_wider(names_from = StateAbbr, values_from = median_value)

## 2. Is there a relationship b/t preventive care access and chronic disease burden, overall?

wide_cdc<-cdc%>%
    filter(!is.na(Data_Value)) %>%
    mutate(LocationId = paste(StateAbbr, LocationName, sep = "_")) %>%
    select(LocationId, StateAbbr,MeasureId, Data_Value) %>%
    pivot_wider(names_from = MeasureId, values_from = Data_Value)
#spearman's doesn't assume the relationship is perfectly linear, and it's more robust to outlier counties 
burden_measures <- c("DIABETES", "OBESITY", "BPHIGH")
access_measures <- c("ACCESS2", "CHECKUP")

#TABLE of rho (spearman's corr (>|0.75| is strong corr)) and p-vals
correlation_summary<- expand.grid(burden = burden_measures, access = access_measures,
                            stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate( test = list(cor.test(wide_cdc[[burden]], wide_cdc[[access]],
                          method = "spearman")),
    rho = test$estimate,
    p_value = test$p.value) %>%
  select(-test) %>%
  ungroup()%>%
  arrange(desc(abs(rho)))%>%
  mutate(pair_name = paste(burden, "vs", access))
#only diabetes~checkup was not statistically significant

#scatterplots and add either regression line or correlation
pair_data <- tribble(
  ~pair_name, ~x_var, ~y_var,
  "DIABETES vs ACCESS2", "DIABETES", "ACCESS2",
  "OBESITY vs ACCESS2", "OBESITY", "ACCESS2",
  "BPHIGH vs ACCESS2", "BPHIGH","ACCESS2",
  "DIABETES vs CHECKUP", "DIABETES", "CHECKUP",
  "OBESITY vs CHECKUP", "OBESITY", "CHECKUP",
  "BPHIGH vs CHECKUP", "BPHIGH", "CHECKUP")
# Make one long data frame for plotting
plot_data <- purrr::map2_dfr(
  pair_data$x_var,
  pair_data$y_var,
  \(xvar, yvar) {
    tibble(
      pair_name = paste(xvar, "vs", yvar),
      x = wide_cdc[[xvar]],
      y = wide_cdc[[yvar]],
      StateAbbr = wide_cdc$StateAbbr
    )
  }
)

# Plot
 # Labels
label_data <- correlation_summary %>%
  mutate(stars = case_when(p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ "ns"),
    label = paste0("rho = ", round(rho, 2), ", ", stars))

setdiff(unique(plot_data$pair_name), unique(label_data$pair_name))

Fig3<-plot_data%>%
mutate( StateAbbr = factor(StateAbbr,
    levels = c( "CA",  "AZ", "NM", "TX", "OK")))%>%
ggplot(aes(x = x, y = y, color= StateAbbr)) +
  geom_point(alpha=0.7, size= 4) +
  geom_smooth(data = plot_data, aes(x = x, y = y), method = "lm", se = FALSE,
    color = "black", inherit.aes = FALSE) +
  scale_color_manual(values = state_colors, name = "State")+
  facet_wrap(~ pair_name, scales = "free") +
  labs(x = "Burden (%)", y = "Access (%)")+
  geom_text(data = label_data, aes(x = -Inf, y = Inf, label = label),
  hjust = -0.1, vjust = 1.5, inherit.aes = FALSE, size = 3.5, fontface = "bold")

## 3. Are some states showing a stronger or weaker access/disease-burden pattern than others?

## Build state-by-state correlation results
state_pairs <- expand.grid(
  StateAbbr = unique(wide_cdc$StateAbbr),
  burden = burden_measures,
  access = access_measures,
  stringsAsFactors = FALSE
)

state_correlation_summary <- state_pairs %>%
  mutate(
    result = pmap(list(StateAbbr, burden, access), function(st, b, a) {
      sub <- wide_cdc %>% filter(StateAbbr == st)
      n_obs <- sum(!is.na(sub[[b]]) & !is.na(sub[[a]]))
      if (n_obs < 4) {
        return(tibble(rho = NA_real_, p_value = NA_real_, n = n_obs))
      }
      test <- cor.test(sub[[b]], sub[[a]], method = "spearman")
      tibble(rho = test$estimate, p_value = test$p.value, n = n_obs)
    })
  ) %>%
  unnest(result) %>%
  mutate(pair_name = paste(burden, "vs", access))

## Build matching labels
state_label_data <- state_correlation_summary %>%
  mutate(
    stars = case_when(
      is.na(p_value)  ~ "",
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    label = paste0("rho = ", round(rho, 2), ", ", stars)
  )

## Plot with labels added
Fig4<-plot_data %>%
  mutate(StateAbbr = factor(StateAbbr, levels = c("CA", "AZ", "NM", "TX", "OK"))) %>%
  ggplot(aes(x = x, y = y, color = StateAbbr)) +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = state_colors, name = "State") +
  facet_grid(StateAbbr ~ pair_name, scales = "free") +
  labs(x = "Burden (%)", y = "Access (%)") +
  geom_text(
    data = state_label_data %>%
      mutate(StateAbbr = factor(StateAbbr, levels = c("CA", "AZ", "NM", "TX", "OK"))),
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.5, color = "black",
    size = 3, fontface = "bold")+ theme(
    panel.border = element_rect(color = "#403838", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.6, "lines"),
    strip.background = element_rect(fill = "#fffcfc", color = "black"),
    strip.text = element_text(face = "bold", size = 8))

## 4. How does the chronic disease burden and prev care access differ b/t major metro counties and their state avgs?

# create lookup object to divide counties into urban and rural
metro_lookup <- tribble( ~LocationName,       ~metro,

  # California
  "Los Angeles",       "Greater Los Angeles Metro",
  "Orange",            "Greater Los Angeles Metro",
  "Riverside",         "Greater Los Angeles Metro",
  "San Bernardino",    "Greater Los Angeles Metro",

  "San Diego",         "San Diego",

  "San Francisco",     "Bay Area",
  "Alameda",           "Bay Area",
  "Santa Clara",       "Bay Area",
  "Contra Costa",      "Bay Area",
  "San Mateo",         "Bay Area",
  "Marin",             "Bay Area",
  "Napa",              "Bay Area",
  "Solano",            "Bay Area",
  "Sonoma",            "Bay Area",

  "Sacramento",        "Sacramento",
  "Fresno",            "Fresno",

  # Arizona
  "Maricopa",          "Phoenix",
  "Pima",              "Tucson",

  # New Mexico
  "Bernalillo",        "Albuquerque",
  "Santa Fe",          "Santa Fe",

  # Oklahoma
  "Oklahoma",          "Oklahoma City",
  "Tulsa",             "Tulsa",

  # Texas
  "Dallas",            "Dallas-Fort Worth Metro",
  "Tarrant",           "Dallas-Fort Worth Metro",
  "Collin",            "Dallas-Fort Worth Metro",
  "Denton",            "Dallas-Fort Worth Metro",

  "Harris",            "Houston",
  "Fort Bend",         "Houston",
  "Montgomery",        "Houston",

  "Travis",            "Austin",
  "Williamson",        "Austin",

  "Bexar",             "San Antonio",

  "El Paso",           "El Paso")

cdc <- cdc %>%
  left_join(metro_lookup, by = "LocationName") %>%
  mutate(
    UrbanStatus = !is.na(metro),
    metro = replace_na(metro, "Rural"))

# TABLE
urban_status_summary<- cdc%>%
 group_by(StateAbbr,UrbanStatus, Short_Question_Text)%>%
 summarise(median = median(Data_Value, na.rm= TRUE))%>%
   mutate(UrbanStatus = ifelse(UrbanStatus, "Urban", "Rural")) %>%
  pivot_wider(names_from = c(UrbanStatus), values_from = median)%>%
  mutate(diff = Urban - Rural)

Table5<-urban_status_summary

#FIGURE
Fig5<-urban_status_summary %>%
  mutate(diff = Urban - Rural,
  Short_Question_Text = factor( Short_Question_Text, 
  levels = c("Diabetes","High Blood Pressure","Obesity","Annual Checkup","Health Insurance" ))) %>%
  ggplot(aes(x = StateAbbr)) +
  geom_segment(aes(xend = StateAbbr, y = Rural, yend = Urban), color = "grey60") +
  geom_point(aes(y = Urban, color = StateAbbr, shape = "Urban"), size = 3) +
  geom_point(aes(y = Rural, color = StateAbbr, shape = "Rural"), size = 3) +
  geom_text(
    aes(y = (Urban + Rural) / 2, label = paste0(round(abs(diff), 1), "%")),
    size = 3, vjust = -0.6, fontface = "bold"
  ) +
  scale_color_manual(values = state_colors, guide = "none") +
  scale_shape_manual(
    name = "",
    values = c("Urban" = 16, "Rural" = 1),
    guide = guide_legend(override.aes = list(color = "black"))
  )+
  scale_y_continuous(labels = label_number(accuracy = 1))  +
  facet_wrap(~ Short_Question_Text, scales = "free") +
  coord_flip() +
  labs(x = "", y = "Median prevalence (%)")


### Save objects

saveRDS(list(Fig1 = Fig1, Fig2= Fig2, Fig3 = Fig3, Fig4 = Fig4, Fig5 = Fig5,
    Table1=Table1, Table2 = Table2, Table3 = Table3, Table4 = Table4, Table5 = Table5),
  "cdc_analysis_objects.rds")
