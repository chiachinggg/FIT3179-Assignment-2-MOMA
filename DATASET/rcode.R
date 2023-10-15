setwd("D:\\2023\\SEM 2\\FIT3179\\\\A2\\FIT3179-Assignment-2-MOMA\\DATASET")
library(tidyr)
library(dplyr)
artwork = read.csv("artworks.csv", header = TRUE)
View(artwork)
artist = read.csv("artists.csv", header = TRUE)
#View(artist)

cleaned = read.csv("cleaned.csv", header = T)
df = read.csv("sampled_data.csv", header = T)
View(df)



#cleaning 
cleaned <- cleaned %>%
  filter(!(Gender == ""))
genders = artist[, c('ConstituentID', 'Gender')]
genders = genders[!is.na(genders$Gender)& genders$Gender != "",]
cleaned$Gender[cleaned$Gender == 'male'] <- 'Male'
cleaned$Gender[cleaned$Gender == 'female'] <- 'Female'
print(genders)

gendersplit <- table(cleaned$Gender)
View(gendersplit)
genders$Gender[genders$Gender == 'Non-binary'] <- 'Non-Binary'

artworkclean = artwork[!is.na(artwork$ConstituentID) & 
                         artwork$ConstituentID!= "" & 
                         artwork$Title!="" &
                         artwork$Nationality != "" &
                         artwork$Nationality != "()"&
                         artwork$Nationality != "() ()"&
                         artwork$Nationality != "() () ()"&
                         artwork$Nationality != "() () () ()" &
                         artwork$Nationality != "() () () () ()"&
                         artwork$Nationality != "() () () () () ()" &
                         artwork$Nationality != "() () () () () () () () () () () () () ()",]
View(artworkclean)

nationalitysplit <- table(artworkclean[, c('Nationality')])
View(nationalitysplit)

dirty = artwork[artwork$ConstituentID== "" | artwork$Title=="",]

artworkclean$Gender <- gsub("\\(|\\)", "", artworkclean$Gender)
artworkclean$Nationality <- gsub("\\(|\\)", "", artworkclean$Nationality)
artworkclean <- subset(artworkclean, select = -ArtistBio)

df <- artworkclean %>% separate_rows(Nationality, sep = " ")
df <- subset(df, Nationality != "Yugoslav")
df <- subset(df, Nationality != "Czechoslovakian")
df <- subset(df, Nationality != "Nationality unknown")


df <- artworkclean %>%
  filter(lengths(strsplit(ConstituentID, ", ")) <= 1)
nationalitysplit2 <- table(df[, c('Nationality')])
View(nationalitysplit2)

country_names <- c(
  "Afghan" = "Afghanistan",
  "American" = "United States of America",
  "Albanian" = "Albania",
  "Algerian" = "Algeria",
  "Argentine" = "Argentina",
  "Australian" = "Australia",
  "Austrian" = "Austria",
  "Azerbaijani" = "Azerbaijan",
  "Bahamian" = "Bahamas",
  "Bangladeshi" = "Bangladesh",
  "Belgian" = "Belgium",
  "Beninese" = "Benin",
  "Bolivian" = "Bolivia",
  "Bosnian" = "Bosnia and Herzegovina",
  "Brazilian" = "Brazil",
  "British" = "United Kingdom",
  "Bulgarian" = "Bulgaria",
  "Burkinabé" = "Burkina Faso",
  "Cameroonian" = "Cameroon",
  "Canadian" = "Canada",
  "Chilean" = "Chile",
  "Chinese" = "China",
  "Colombian" = "Colombia",
  "Congolese" = "Congo",
  "Costa Rican" = "Costa Rica",
  "Croatian" = "Croatia",
  "Cuban" = "Cuba",
  "Czech" = "Czechia",
  "Danish" = "Denmark",
  "Dutch" = "Netherlands",
  "Ecuadorian" = "Ecuador",
  "Egyptian" = "Egypt",
  "Emirati" = "United Arab Emirates",
  "Estonian" = "Estonia",
  "Ethiopian" = "Ethiopia",
  "Filipino" = "Philippines",
  "Finnish" = "Finland",
  "French" = "France",
  "Georgian" = "Georgia",
  "German" = "Germany",
  "Ghanaian" = "Ghana",
  "Greek" = "Greece",
  "Guatemalan" = "Guatemala",
  "Haitian" = "Haiti",
  "Hungarian" = "Hungary",
  "Icelandic" = "Iceland",
  "Indian" = "India",
  "Iranian" = "Iran",
  "Iraqi" = "Iraq",
  "Irish" = "Ireland",
  "Israeli" = "Israel",
  "Italian" = "Italy",
  "Ivorian" = "Ivory Coast",
  "Japanese" = "Japan",
  "Kenyan" = "Kenya",
  "Korean" = "South Korea",
  "Kuwaiti" = "Kuwait",
  "Latvian" = "Latvia",
  "Lebanese" = "Lebanon",
  "Lithuanian" = "Lithuania",
  "Luxembourger" = "Luxembourg",
  "Macedonian" = "North Macedonia",
  "Malaysian" = "Malaysia",
  "Malian" = "Mali",
  "Mexican" = "Mexico",
  "Moroccan" = "Morocco",
  "Mozambican" = "Mozambique",
  "Namibian" = "Namibia",
  "New Zealander" = "New Zealand",
  "Nicaraguan" = "Nicaragua",
  "Nigerian" = "Nigeria",
  "Norwegian" = "Norway",
  "Pakistani" = "Pakistan",
  "Palestinian" = "Palestinian territories",
  "Panamanian" = "Panama",
  "Paraguayan" = "Paraguay",
  "Peruvian" = "Peru",
  "Polish" = "Poland",
  "Portuguese" = "Portugal",
  "Puerto Rican" = "Puerto Rico",
  "Romanian" = "Romania",
  "Russian" = "Russia",
  "Salvadoran" = "El Salvador",
  "Scottish" = "Scotland",
  "Senegalese" = "Senegal",
  "Serbian" = "Serbia",
  "Sierra Leonean" = "Sierra Leone",
  "Singaporean" = "Singapore",
  "Slovak" = "Slovakia",
  "Slovenian" = "Slovenia",
  "South African" = "South Africa",
  "Spanish" = "Spain",
  "Sudanese" = "Sudan",
  "Swedish" = "Sweden",
  "Swiss" = "Switzerland",
  "Taiwanese" = "Taiwan",
  "Tanzanian" = "Tanzania",
  "Thai" = "Thailand",
  "Tunisian" = "Tunisia",
  "Turkish" = "Turkey",
  "Ugandan" = "Uganda",
  "Ukrainian" = "Ukraine",
  "Uruguayan" = "Uruguay",
  "Venezuelan" = "Venezuela",
  "Vietnamese" = "Vietnam",
  "Welsh" = "Wales",
  "Zimbabwean" = "Zimbabwe",
  "Canadian Inuit" ="Canada" ,
  "Coptic" = "Egypt",
  "Native American" = "United States of America",	
  "Persian" = "Iran"
)


df$Country <- country_names[df$Nationality]
df <- subset(df, select = -Nationality)

na_rows <- df[is.na(df$Country), ]
df <- subset(df, Title != "Yugoslavian travel poster")
View(na_rows)
haha <- table(df[, c('Country')])
View(haha)
write.csv(df, file = 'cleaned.csv', row.names = FALSE)
write.csv(haha, file = 'ugh.csv', row.names = FALSE)


install.packages("ggmap")
library(ggmap)
View(sampled_data)
# List of countries
countries <- c(
  "Guatemala", "Haiti", "Hungary", "Iceland", "India", "Iran", "Iraq", "Ireland", "Israel", "Italy", 
  "Ivory Coast", "Japan", "Kenya", "Kuwait", "Latvia", "Lebanon", "Lithuania", "Luxembourg", "Malaysia", 
  "Mali", "Mexico", "Morocco", "Mozambique", "Namibia", "Netherlands", "New Zealand", "Nicaragua", 
  "Nigeria", "North Macedonia", "Norway", "Pakistan", "Palestinian territories", "Panama", "Paraguay", 
  "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Romania", "Russia", "Scotland", 
  "Senegal", "Serbia", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "South Africa", 
  "South Korea", "Spain", "Sudan", "Sweden", "Switzerland", "Taiwan", "Tanzania", "Thailand", 
  "Tunisia", "Turkey", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
  "United States of America", "Uruguay", "Venezuela", "Vietnam", "Wales", "Zimbabwe"
)

# Initialize empty data frame
country_coordinates <- data.frame(Country = character(0), Latitude = numeric(0), Longitude = numeric(0))

# Get coordinates for each country
for (country in countries) {
  geo <- geocode(country)
  country_coordinates <- rbind(country_coordinates, data.frame(Country = country, Latitude = geo$lat, Longitude = geo$lon))
}

# Print the resulting data frame
print(country_coordinates)


# data exploration
aa = cleaned$Department
distinct_departments <- unique(aa)
print(distinct_departments)

column_names <- names(cleaned)
# Print the column names
print(column_names)


columns_to_drop <- c("Circumference..cm.", "Depth..cm.", "Diameter..cm.",
                     "Height..cm.", "Length..cm.", "Weight..kg.",
                     "Width..cm.", "Seat.Height..cm.", "Duration..sec..")
cleaned <- cleaned[, !(colnames(cleaned) %in% columns_to_drop)]
cleaned <- cleaned[complete.cases(cleaned$URL, cleaned$ThumbnailURL), ]
library(dplyr)
american_artworks <- cleaned %>%
  filter(Country == "United States of America")
View(american_artworks)

# Identify rows where Country is "United States of America"
rows_to_drop <- cleaned %>%
  filter(Country == "United States of America") %>%
  sample_n(50000)

# Drop the selected rows
cleaned <- anti_join(cleaned, rows_to_drop)
cleaned <- cleaned %>%
  filter(!(Title == "Untitled"))
cleaned <- cleaned %>%
  filter(!grepl("Untitled", Title, ignore.case = TRUE))
cleaned <- cleaned %>%
  filter(!is.na(Medium) & Medium != "")
cleaned <- cleaned %>%
  filter(!is.na(Title) & Title != "")
cleaned <- cleaned %>%
  filter(!is.na(Dimensions) & Dimensions != "")
cleaned <- cleaned %>%
  filter(!is.na(URL) & URL != "" & !is.na(ThumbnailURL) & ThumbnailURL != "")
cleaned <- cleaned %>%
  filter(Cataloged != "N")
cleaned <- subset(cleaned, BeginDate != "(0)")
cleaned <- subset(cleaned, EndDate != "(0)")
cleaned$BeginDate <- as.numeric(gsub("[()]", "", cleaned$BeginDate))
cleaned$EndDate <- as.numeric(gsub("[()]", "", cleaned$EndDate))
# Set the desired number of rows to keep (e.g., 10,000)
desired_rows <- 10000

# Randomly sample the specified number of rows
df <- cleaned %>%
  sample_n(desired_rows)

aggregated_data <- sampled_data %>%
  group_by(Country, Classification, Department) %>%
  summarize(
    Total_Count = n()
    # Add more summary statistics as needed
  )

write.csv(sampled_data, file = 'sampled_data.csv', row.names = FALSE)
artists_per_country <- aggregate(Artist ~ Country, data = df, FUN = length)
# Rename the count column to "ArtistCount"
artists_per_country <- rename(artists_per_country, ArtistCount = Artist)
write.csv(artists_per_country, file = 'artistspercountry.csv', row.names = FALSE)
artists_per_country$Country <- gsub("\"", "", artists_per_country$Country)


#population pyramid
# Install and load necessary packages
install.packages("dplyr")
install.packages("jsonlite")

library(dplyr)
library(jsonlite)

# Assuming your data frame is named df
# Calculate counts per year, department, and gender
result <- df %>%
  group_by(Year = as.integer(EndDate), Department, Gender) %>%
  summarise(Count = n())

# Print the result
print(result)

# Export the result to JSON
population_pyramid <- toJSON(result, pretty = TRUE)
writeLines(population_pyramid, "population_pyramid.json")

cat("Distinct Departments:\n")
cat(unique(df$Department), sep = "\n")
filtered_data <- subset(df, Department == "Architecture & Design - Image Archive")

print(filtered_data)
df$Department <- gsub(" - Image Archive", "", df$Department)
write.csv(df, file = 'sampled_data.csv', row.names = FALSE)

#isotype
gender_counts <- table(df$Gender)
representation <- prop.table(gender_counts)
print(gender_counts)
# Print the results
cat("Representation of Genders:\n")
cat("Male: ", round(representation["Male"] * 100, 3), "%\n")
cat("Female: ", round(representation["Female"] * 100, 3), "%\n")


#artist count
artist_counts <- table(df$Artist)

# Create a data frame with artist names and their corresponding counts
artist_counts_df <- data.frame(Artist = names(artist_counts), Count = as.numeric(artist_counts))

# Rank the artists in descending order based on count
ranked_artists <- artist_counts_df[order(-artist_counts_df$Count), ]

result <- df %>%
  group_by(EndDate, Artist) %>%
  summarize(count = n()) %>%
  arrange(EndDate, desc(count)) %>%
  mutate(rank = row_number())

#ok this is per year, not consecutive years
# Export the result to JSON
artistcount <- toJSON(result, pretty = TRUE)
writeLines(artistcount, "artistworkcount.json")


#every 50 years
all_years <- data.frame(Artist = df$Artist, EndDate = df$EndDate)

# Left join with the actual data
result <- all_years %>%
  mutate(Period = case_when(
    EndDate >= 1800 & EndDate < 1850 ~ "Romanticism",
    EndDate >= 1850 & EndDate < 1900 ~ "Realism/Impressionism",
    EndDate >= 1900 & EndDate < 1920 ~ "Cubism/Futurism",
    EndDate >= 1920 & EndDate < 1940 ~ "Dada/Surrealism",
    EndDate >= 1940 & EndDate < 1960 ~ "Abstract Expressionism",
    EndDate >= 1960 & EndDate < 1980 ~ "Pop Art/Minimalism",
    EndDate >= 1980 & EndDate < 2000 ~ "Postmodernism/Y2K Resurgence",
    EndDate >= 2000 ~ "Contemporary Art",
    TRUE ~ "Other"  # Optional: for any years not covered by the conditions
  ))

top_artists <- result %>%
  group_by(Artist, Period) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(Period) %>%
  slice_head(n = 1)

result <- all_years %>%
  mutate(Period = case_when(
    EndDate >= 1800 & EndDate < 1850 ~ "Romanticism",
    EndDate >= 1850 & EndDate < 1900 ~ "Realism/Impressionism",
    EndDate >= 1900 & EndDate < 1920 ~ "Cubism/Futurism",
    EndDate >= 1920 & EndDate < 1940 ~ "Dada/Surrealism",
    EndDate >= 1940 & EndDate < 1960 ~ "Abstract Expressionism",
    EndDate >= 1960 & EndDate < 1980 ~ "Pop Art/Minimalism",
    EndDate >= 1980 & EndDate < 2000 ~ "Postmodernism/Y2K Resurgence",
    EndDate >= 2000 ~ "Contemporary Art",
    TRUE ~ "Other"  # Optional: for any years not covered by the conditions
  )) %>%
  group_by(Period, Artist) %>%
  summarize(count = n()) %>%
  arrange(Period, desc(count)) %>%
  group_by(Period) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10)
df2 <- read.csv("artist_period.csv", header = TRUE)
write.csv(result, file = 'period_artist_ranked.csv', row.names = FALSE)
df22 <- toJSON(df2, pretty = TRUE)
writeLines(df22, "artist_period.json")
