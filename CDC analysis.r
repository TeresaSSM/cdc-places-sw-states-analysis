library(pacman)
p_load(tidyverse, janitor, skimr, ggthemes, ggpubr, rstatix)

theme_set(theme_classic())

cdc_full<-read_csv("PLACES_Data.csv")

skim(cdc_full)

cdc<-cdc_full%>%
 filter(Data_Value_Type == "Age-adjusted prevalence")%>%
 select(StateAbbr, LocationName, Category, Data_Value, MeasureId, Short_Question_Text) #the Data_Value_Unit for all these measures is percentage

skim(cdc)

#defining state color palette
state_colors <- c("CA" = "#f8dfa5",  "AZ" = "#deac23", "NM" = "#ec7c2b",   "TX" = "#c23022", "OK" = "#9b2c0a")

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
cdc%>%
mutate( StateAbbr = factor(StateAbbr,
      levels = c("TX", "OK", "NM", "AZ", "CA")))%>%
ggplot(aes(x=Data_Value, fill=StateAbbr, color= StateAbbr))+
    geom_histogram(bins = 30, position = "identity", alpha =0.75)+
    scale_fill_manual(values = state_colors)+
    scale_color_manual(values = state_colors)+
    labs(x="Age-adjusted prevalence (%)", y="Count of counties")+
    facet_wrap(Category~MeasureId, scales = "free")

# Fig 1b (comparing means)
cdc_summary %>%
    filter(MeasureId %in% c("DIABETES", "OBESITY", "BPHIGH"))%>%
    ggplot(aes(x=StateAbbr, y=mean, fill=MeasureId))+
    geom_col(position = "dodge")

# Fig 1c (violin plots for each metric; facet by state and measure)
cdc %>%
    ggplot(aes(x=StateAbbr, y=Data_Value, fill=StateAbbr, color=StateAbbr))+
    scale_fill_manual(values = state_colors)+
    scale_color_manual(values = state_colors)+
    geom_boxplot(alpha = 0.75)+
    geom_jitter(width=0.2)+
    facet_wrap(Category~MeasureId)+ 
    stat_compare_means(method = "kruskal.test", label = "p.signif")

# Do the states differ from the overall mean for each measure? (Kruskal-Wallis test, non-parametric)
cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(MeasureId) %>%
  summarise(p_value = kruskal.test(Data_Value ~ StateAbbr)$p.value, .groups = "drop")
#they (above) are all significant

#kruskal effect size
cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(MeasureId) %>%
  kruskal_effsize(Data_Value ~ StateAbbr)
#the effect size for Checkup  (ε² = 0.73) is the highest but 4/5 are large magnitude!
#diabetes is the lowest, so avg age, pop genetics, etc, may explain diabetes variation more than state.

#Dunn test: pairwise comparisions
#large z-scores in conjunction with pval for significance
cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(MeasureId) %>%
  dunn_test(Data_Value ~ StateAbbr) %>%
  filter(p.adj < 0.05, abs(statistic)>10)%>%
  print(n=Inf)

# TABLE of medians of the states - so we can see visually which ones are diff from avg
cdc %>%
  filter(!is.na(Data_Value)) %>%
  group_by(MeasureId, StateAbbr) %>%
  summarise(median_value = median(Data_Value), .groups = "drop") %>%
  pivot_wider(names_from = StateAbbr, values_from = median_value)

## 2. Is there a relationship b/t preventive care access and chronic disease burden, overall?

#Correlation bt combined burdon score (avg of 3 burdons) and combined access score (avg of 2 access measures)

#scatterplots and add either regression line or correlation

## 3. Are some states showing a stronger or weaker access/disease-burden pattern than others?

#same as above, but break up by state

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

#this is a table of city name/city value/state/state avg/difference; benchmarking

#a dumbbell plot showing the diff could be a cool addition

## 5. [Discussion section] Which counties shows the strongest case for targeted intervention, based on the combined disease-burden and access picture? 
#(Address whether primary differences are state-based or urban-v-rural based)