# Load data 
seer <- data.table::fread(file.path("~", "Downloads", "Young_NSCLC_California_Firstprimary_2011-2021_FINAL.csv"))

# Rename variables
seer <- seer |>
  rename(
    race = `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`,
    year_dx = `Year of diagnosis`,
    survival = `Survival months`,
    age = `Age recode with single ages and 90+`,
    sex = `Sex`,
    ICD = `Histologic Type ICD-O-3`,
    stage = `Combined Summary Stage (2004+)`
  )

# Convert variables to integers or factors
seer$race <- as.factor(seer$race)
seer$year_dx <- as.integer(seer$year_dx)
seer$survival <- as.integer(seer$survival)
seer$age <- gsub(" years", "", seer$age) 
seer$age <- as.integer(seer$age)
seer$sex <- as.factor(seer$sex)
seer$ICD <- as.integer(seer$ICD)
seer$stage <- as.factor(seer$stage)

# Create a new categorical variable named "Histology" based on ICD-0-3 codes with 4 categories (Squamous cell carcinoma, Adenocarcinoma, Large cell carcinoma, and Not otherwise specified)
seer <- seer |>
  mutate(
    histology = case_when(
      ICD %in% c(8051, 8052, 8070:8076, 8078, 8083, 8084, 8090, 8094, 8123) ~ "Squamous Cell Carcinoma",
      ICD %in% c(8015, 8050, 8140, 8141, 8143:8145, 8147, 8190, 8201, 8211, 8250:
                   8255, 8260, 8290, 8310, 8320, 8323, 8333, 8401, 8440, 8470, 8471,
                 8480, 8481, 8490, 8503, 8507, 8550, 8570:8572, 8574, 8576) ~ "Adenocarcinoma",
      ICD %in% c(8012:8014, 8021, 8034, 8082) ~ "Large Cell Carcinoma",
      TRUE ~ "Not Otherwise Specified"
    ))
seer$histology <- as.factor(seer$histology)

# Remove missing survival data
seer <- seer[!is.na(seer$survival), ]

#Filter out "Non-Hispanic Unknown Race" 
seer <- seer |>
  filter(race != "Non-Hispanic Unknown Race")

# Arrange levels for the categorical variables
seer <- seer |>
  mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic (All Races)", "Non-Hispanic Asian or Pacific Islander", "Non-Hispanic American Indian/Alaska Native"))
  )
seer$stage <- factor(seer$stage, levels = c("Localized", "Regional", "Distant", "Unknown/unstaged"))
seer$histology <- factor(seer$histology, levels = c("Adenocarcinoma", "Squamous Cell Carcinoma", "Large Cell Carcinoma", "Not Otherwise Specified"))

