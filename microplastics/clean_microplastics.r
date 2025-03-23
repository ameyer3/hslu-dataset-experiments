# Load necessary libraries
library(dplyr)

# Read both datasets
df_left <- read.csv("microplastics/Marine Microplastic Concentrations-left.csv", stringsAsFactors = FALSE)
df_right <- read.csv("microplastics/Marine Microplastic Concentrations-right.csv", stringsAsFactors = FALSE)

# Concatenate the datasets
df_combined <- rbind(df_left, df_right)

# Remove duplicate rows
df_combined <- distinct(df_combined)

# Save the cleaned dataset (without duplicates)
write.csv(df_combined, "Microplastics_Combined_NoDuplicates.csv", row.names = FALSE)
df_combined %>% arrange(Longitude) %>%  slice(1:10)  

colnames(df_combined)

df_combined <- subset(df_combined, select = -c(Short.Reference, Long.Reference, DOI, Keywords, Organization, NCEI.Accession.Number, NCEI.Accession.Link) )
names(df_combined)[names(df_combined) == "Microplastics.Measurement..density."] <- "Microplastics.Measurement.Density"

colnames(df_combined)

# Save the final cleaned dataset
write.csv(df_combined, "Microplastics_Cleaned.csv", row.names = FALSE)
