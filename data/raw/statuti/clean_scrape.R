library(dplyr)
library(stringr)
library(jsonlite)
library(readr)

setwd("C:/Users/dealb/Documents/Fascism/Statuti")

s1 <- read.csv("medieval_statutes_data_20250626_064139.csv")
s1 <- s1[,-1]
s2 <- read.csv("medieval_statutes_data_20250626_102403.csv")
s2 <- s2[,-1]

names <- fromJSON("ISTAT_names_json.json")
names <- names$resultset
names <- names[,c(1,2,5)]
names$PRO_COM_T <- as.numeric(names$PRO_COM_T)

manual_match <- read.csv("Fill_names.csv")
manual_match$stat <- 1
manual_match <- manual_match[, c(4,6)] %>%
  filter(PRO_COM!= "")

s <- rbind(s1,s2)
s <- unique(s)
s <- s %>%
  filter(document_link!="")
s <- s[-1,]

exact_match <- s %>%
  filter(dropdown_text %in% names$COMUNE_DT_FI | dropdown_text %in% names$COMUNE_DT)
colnames(exact_match)[1] <- "correct_name"

s <- left_join(s,exact_match, by="document_link")

s <- s %>%
  group_by(document_link) %>%
  mutate(com_name = first(correct_name))

matched <- s %>%
  filter(!is.na(com_name))

matched <- matched[, -c(1,3)] %>%
  distinct()

matched <- left_join(matched, names, by=c("com_name"="COMUNE_DT_FI")) %>%
  filter(PRO_COM_T!=41041) #Manually drop the wrong Peglio

matched$stat <- 1

# Extract earliest year from the matched data
matched <- matched %>%
  group_by(document_link) %>%
  mutate(year_earliest_statuto = first(earliest_year)) %>%
  ungroup()

matched <- matched[,c(3,5,6)]
colnames(matched)[1] <- "PRO_COM"

# Add year column to manual_match (assuming it doesn't have years)
manual_match$year_earliest_statuto <- NA

statuti_clean <- rbind(matched, manual_match)

write.csv(statuti_clean, "statuti_clean.csv", row.names = F)

# Print summary statistics
cat("\n=== YEAR STATISTICS ===\n")
cat("Total records:", nrow(statuti_clean), "\n")
cat("Records with years:", sum(!is.na(statuti_clean$year_earliest_statuto)), "\n")
cat("Records without years:", sum(is.na(statuti_clean$year_earliest_statuto)), "\n")

if(sum(!is.na(statuti_clean$year_earliest_statuto)) > 0) {
  cat("\nEarliest year:", min(statuti_clean$year_earliest_statuto, na.rm = TRUE), "\n")
  cat("Latest year:", max(statuti_clean$year_earliest_statuto, na.rm = TRUE), "\n")
  cat("Average year:", round(mean(statuti_clean$year_earliest_statuto, na.rm = TRUE), 0), "\n")
}