rm(list=ls())

library(gt)
library(gtsummary)
library(tidyverse)
library(scales)
library(kableExtra)

# Import studies from folder
studies <- read.csv("data/studies.csv", row.names=NULL, stringsAsFactors=TRUE)


#studies <- readxl::read_excel("data/studies.xlsx", sheet = "studies")

Include = c("comparison", "doseNum", "perspective", "income", "herdEffects", 
            "seroReplace", "efficacyPneum", "efficacyIPD", "efficacyOtitis", 
            "budgetImpact", "studyDecide")
Label   = list(comparison     = "Type of comparison",
               doseNum        = "Number of doses",
               perspective    = "Study perspective", 
               income         = "Income group",
               herdEffects    = "Included herd effects",
               seroReplace    = "Included serotype replacement",
               efficacyPneum  = "Pneumonia efficacy endpoint",
               efficacyIPD    = "IPD efficacy endpoint",
               efficacyOtitis = "Otitis media efficacy endpoint",
               budgetImpact   = "Included budget impact analysis",
               studyDecide    = "Study decision")

comparisonLevels <- c("PCV7 vs NoVax", "PCV10 vs NoVax", "PCV13 vs NoVax", 
                      "PCV13 vs PCV10", "Others")

# Function to get type of comparison by unique studyid
unique <- function(var) {
  (studies %>% group_by(across(var)) %>% 
     summarise(n = n_distinct(across(studyID))))$n
}

studies <- studies %>% 
  mutate(comparison = factor(comparison, levels=comparisonLevels),
         doseNum = factor(doseNum, levels=c("Four", "Three", "One/two", "Unknown")),
         studyDecide = factor(studyDecide, levels=c("Cost-saving", "Cost-effective",
                                                    "Not cost-effective")))

# Table of study characteristics 
df1 <- studies %>%
  tbl_summary(missing="no",  include=all_of(Include), label=Label,
              digits    = everything() ~ c(0,0))  %>% 
  modify_header(stat_0 ~ "Number (%) of evaluations, N={n}")

df2 <- studies %>%
  tbl_summary(missing="no",  include=all_of(Include), label=Label,
              statistic = everything() ~ "")  %>%
  modify_header(stat_0 ~ "Count of unique studies, N={n_distinct(studies$studyID)}")

df2$table_body$stat_0[2:6] <- unique("comparison")
df2$table_body$stat_0[8:11] <- unique("doseNum")
df2$table_body$stat_0[13:14] <- unique("perspective")
df2$table_body$stat_0[16:18] <- unique("income")
df2$table_body$stat_0[19] <- unique("herdEffects")[2]
df2$table_body$stat_0[20] <- unique("seroReplace")[2]
df2$table_body$stat_0[21] <- unique("efficacyPneum")[1]
df2$table_body$stat_0[22] <- unique("efficacyIPD")[2]
df2$table_body$stat_0[23] <- unique("efficacyOtitis")[2]
df2$table_body$stat_0[24] <- unique("budgetImpact")[2]
df2$table_body$stat_0[26:28] <- unique("studyDecide")

df <- tbl_merge(list(df1, df2), tab_spanner=FALSE) %>%
  modify_header(label ~ "Characteristic") %>% 
  modify_footnote(update = list(stat_0_1 ~ NA, stat_0_2 ~ NA)) %>% 
  as_kable_extra(, format = "latex", escape = FALSE) %>%
  gsub(".(begin|end){tabular.*}", "", ., perl = TRUE) %>% # remove tabular
  sub("\\\\hline", "", .)  # remove the first hline
writeLines(df, "output/tables/tab_evals_studies.tex")
rm(df1, df2, df)


# Table of study decisions by comparison
df <- bind_rows(studies, studies %>% mutate(comparison = "All comparisons")) %>%
  group_by(across(comparison)) %>%
  summarise(number = length(studyID),
            studyDecide = scales::percent(sum(studyDecide != "Not cost-effective") / 
                                            length(studyDecide)),
            OHdecide = scales::percent(sum(OHdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(OHdecide))),
            OLdecide = scales::percent(sum(OLdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(OLdecide))),
            WHdecide = scales::percent(sum(WHdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(WHdecide))),
            WLdecide = scales::percent(sum(WLdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(WLdecide))),
            RHdecide = scales::percent(sum(RHdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(RHdecide))),
            RLdecide = scales::percent(sum(RLdecide != "Not cost-effective", na.rm = T) /
                                         length(na.omit(RLdecide)))) %>% 
  arrange(factor(comparison, levels=comparisonLevels))

gt(df) # View table

df <- kable(as.data.frame(df), format = "latex", 
            col.names = c("Comparison", "Number of evaluations", "Study decision", 
                          "High", "Low", "High", "Low", "High", "Low"))
df <- add_header_above(df, c(" ", " ", " ", "Ochalek (2018)" = 2, 
                             "Woods (2016)" = 2, "P-Rivière (2023)" = 2),
                       border_left = T, border_right = T) %>% 
  gsub(".(begin|end){tabular.*}", "", ., perl = TRUE) %>% # remove tabular
  sub("\\\\hline", "", .)  # remove the first hline
writeLines(df, "output/tables/tab_evals_thresholds.tex")




# Make "new_switches" variable that is only relevant to evaluations that switch from being cost-effective (not "cost-saving" or "not cost-effective")
studies <- studies %>%
  mutate(new_switches = case_when(switches==0 & studyDecide=="Cost-saving" ~ NA,
                                  switches==0 & studyDecide=="Not cost-effective" ~ NA,
                                  TRUE ~ switches),
         anySwitch = case_when(is.na(new_switches) ~ NA, 
                               new_switches!=0 ~ 1,
                               TRUE ~ 0),
         # Natalie analysis
         anySwitch2 = case_when(is.na(new_switches) ~ 1, 
                               new_switches!=0 ~ 1,
                               TRUE ~ 0))


# Create contingency table with frequencies
f.Switch <- function(typeVar, switchVar){
  data = studies[studies$comparison!="Others", ]
  data = droplevels(data)
  x = table(data[[typeVar]], data[[switchVar]])
  switchProp = prop.table(x, margin = 1) * 100
  return(list(typeVar = typeVar,
              switchVar = switchVar,
              level = rownames(switchProp)[which.max(switchProp[, "1"])], 
              max   = round(max(switchProp[, "1"]), 1), 
              `p-value` = round(fisher.test(x)$p, 3)))
}

df <- as.data.frame(rbind(f.Switch("comparison", "switchOH"),
                           f.Switch("comparison", "switchOL"),
                           f.Switch("comparison", "switchWH"),
                           f.Switch("comparison", "switchWL"),
                           f.Switch("comparison", "switchRH"),
                           f.Switch("comparison", "switchRL"),
                           f.Switch("income", "switchOH"),
                           f.Switch("income", "switchOL"),
                           f.Switch("income", "switchWH"),
                           f.Switch("income", "switchWL"),
                           f.Switch("income", "switchRH"),
                           f.Switch("income", "switchRL"),
                           f.Switch("herdEffects", "switchOH"),
                           f.Switch("herdEffects", "switchOL"),
                           f.Switch("herdEffects", "switchWH"),
                           f.Switch("herdEffects", "switchWL"),
                           f.Switch("herdEffects", "switchRH"),
                           f.Switch("herdEffects", "switchRL"),
                           f.Switch("seroReplace", "switchOH"),
                           f.Switch("seroReplace", "switchOL"),
                           f.Switch("seroReplace", "switchWH"),
                           f.Switch("seroReplace", "switchWL"),
                           f.Switch("seroReplace", "switchRH"),
                           f.Switch("seroReplace", "switchRL"),
                           f.Switch("perspective", "switchOH"),
                           f.Switch("perspective", "switchOL"),
                           f.Switch("perspective", "switchWH"),
                           f.Switch("perspective", "switchWL"),
                           f.Switch("perspective", "switchRH"),
                           f.Switch("perspective", "switchRL")))

df$level[df$level=="0"] <- "Excluded"
df$level[df$level=="1"] <- "Included"

df$typeVar[df$typeVar=="comparison"]  <- "Type of comparison"
df$typeVar[df$typeVar=="income"]      <- "Income group"
df$typeVar[df$typeVar=="herdEffects"] <- "Herd effects"
df$typeVar[df$typeVar=="seroReplace"] <- "Serotype replacement"
df$typeVar[df$typeVar=="perspective"] <- "Study perspective"

df$switchVar[df$switchVar=="switchOH"] <- "Ochalek (2018) high"
df$switchVar[df$switchVar=="switchOL"] <- "Ochalek (2018) low"
df$switchVar[df$switchVar=="switchWH"] <- "Woods (2016) high"
df$switchVar[df$switchVar=="switchWL"] <- "Woods (2016) low"
df$switchVar[df$switchVar=="switchRH"] <- "P-Rivière (2023) high"
df$switchVar[df$switchVar=="switchRL"] <- "P-Rivière (2023) low"


# Clean up table for Kable
df$`p-value` <- format(df$`p-value`, nsmall = 3)
df$max       <- format(df$max, nsmall = 1)
df$lSwitch   <- paste(df$max, "% (p=", df$`p-value`, ")", sep="")
df           <- df[, c("typeVar", "switchVar", "level", "lSwitch")]
names(df)    <- c("Characteristic", "Threshold", 
                  "Most likely switch", "Switch proportion")

# Convert to Kable and remove latex preambles
tab_likely_switch <- kable(df, format = "latex")
tab_likely_switch <- tab_likely_switch %>% 
  collapse_rows(columns = 1, valign = "top") %>%
  gsub(".(begin|end){tabular.*}", "", ., perl = TRUE) %>%
  sub("\\\\hline", "", .)  
writeLines(tab_likely_switch, "output/tables/tab_likely_switch.tex")




# Create contingency table with frequencies
f.Switch2 <- function(typeVar){
  data = studies[studies$comparison!="Others", ]
  data$herdEffects = factor(data$herdEffects)
  data$seroReplace = factor(data$seroReplace)
  data = droplevels(data)
  x = table(data[[typeVar]], data[["anySwitch"]])
  switchProp = prop.table(x, margin = 1) * 100
  return(list(typeVar = rep(typeVar, length(levels(data[[typeVar]]))),
              level = rownames(switchProp),
              switchProp = round(switchProp[, "1"], 1),
              `p-value` = rep(round(fisher.test(x)$p, 3), 
                              length(levels(data[[typeVar]])))))
}
df <- rbind(data.frame(f.Switch2("comparison")),
            data.frame(f.Switch2("income")),
            data.frame(f.Switch2("herdEffects")),
            data.frame(f.Switch2("seroReplace")),
            data.frame(f.Switch2("perspective")))
rownames(df) <- NULL

df$level[df$level=="0"] <- "Excluded"
df$level[df$level=="1"] <- "Included"

df$typeVar[df$typeVar=="comparison"]  <- "Type of comparison"
df$typeVar[df$typeVar=="income"]      <- "Income group"
df$typeVar[df$typeVar=="herdEffects"] <- "Herd effects"
df$typeVar[df$typeVar=="seroReplace"] <- "Serotype replacement"
df$typeVar[df$typeVar=="perspective"] <- "Study perspective"


# Clean up table for Kable
df$switchProp <- format(df$switchProp, nsmall = 1)
df$switchProp <- paste(df$switchProp, "%", sep="")
names(df)     <- c("Characteristic", "Category levels", "Switch proportion", "P-value")

# Convert to Kable and remove latex preambles
tab_likely_anyswitch <- kable(df, format = "latex")
tab_likely_anyswitch <- tab_likely_anyswitch %>% 
  collapse_rows(columns = c(1,4), valign = "top") %>%
  gsub(".(begin|end){tabular.*}", "", ., perl = TRUE) %>%
  sub("\\\\hline", "", .)  
writeLines(tab_likely_anyswitch, "output/tables/tab_likely_anyswitch.tex")




# Table of price intervals
studies %>%
  group_by(anySwitch) %>%
  summarize(priceIntvUSD = mean(priceIntvUSD, na.rm = T))


#
aggregate(priceIntvUSD ~ switchOH, studies, mean, na.rm = T)

t.test(priceIntvUSD ~ switchOH, studies)
t.test(priceIntvUSD ~ switchOL, studies)
t.test(priceIntvUSD ~ switchWH, studies)
t.test(priceIntvUSD ~ switchWL, studies)
t.test(priceIntvUSD ~ switchRH, studies)
t.test(priceIntvUSD ~ switchRL, studies)


ttest <- t.test(priceIntvUSD ~ anySwitch, studies)

ttest$estimate
ttest$conf.int
ttest$p.value

# Exclude rows with missing values in 'anySwitch' before calculating weights
df_filtered <- studies %>% 
  filter(!is.na(anySwitch)) %>% 
  group_by(studyID) %>%
  mutate(weights = 1/n()) %>% 
  select(studyID, uniqueID, weights)

# Merge the weights back to the original data frame, setting weight to 0 for excluded rows
studies <- studies %>%
  left_join(df_filtered, by = "uniqueID") %>%
  mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
  select(-studyID.y)

# Create new data frame, no rows with missing price and other comparisons
df_model <- studies %>%
  filter(!is.na(priceIntvUSD), comparison!="Others")


# Replicating original (NC) analysis
print(summary(glm(anySwitch2 ~ comparison + perspective + income + 
                herdEffects + seroReplace + priceIntvUSD,
              family = binomial(link = "probit"), data = df_model)),
        digits = 1)

# Fit a probit model on anySwitch with weights
model <- studies %>%
  filter(!is.na(anySwitch), comparison != "Others")

glm(anySwitch2 ~ comparison + perspective + income + 
      herdEffects + seroReplace + priceIntvUSD,
    family = binomial(link = "probit"), data = model2)


probit <- glm(anySwitch ~ comparison + perspective + income + 
                herdEffects + seroReplace + priceIntvUSD,
              family = binomial(link = "probit"), data = df_model)
print(summary(probit), digits = 1)
tbl_regression(probit, exponentiate = F)

probitW <- glm(anySwitch ~ comparison + perspective + income + 
                 herdEffects + seroReplace + priceIntvUSD,
               family = binomial(link = "probit"), data = df_model, 
               weights = weights)
print(summary(probitW), digits = 2)
tbl_regression(probitW, exponentiate = F)

logitW <- glm(anySwitch ~ comparison + perspective + income + 
                 herdEffects + seroReplace + priceIntvUSD,
               family = binomial(link = "logit"), data = df_model, 
               weights = weights)
print(summary(logitW), digits = 2)
tbl_regression(logitW, exponentiate = F)

