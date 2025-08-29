library(tidycensus)
library(dplyr)
library(tidyverse)
library(tigris)
library(janitor)
library(mapview)
library(sf)
library(openxlsx)
options(scipen = 999)
setwd("O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/")

#read all nesscery files------------------------------------
pgc_cd <- st_read("GIS/PGC_maps/data/Cnclmnc_Dstrct_2014_Py/Cnclmnc_Dstrct_2014_Py.shp") %>% 
  st_as_sf()%>% 
  filter(DISTRICT_N!=0)
st_crs(pgc_cd)

#2014
demo_2014_text <- read_csv("Census_R/Outputs/PGC_data_2014_selected.csv") %>% 
  mutate(GEOID = as.character(GEOID))
shp_2014 <- st_read("Census_R/Outputs/Census_shp/PGC_shp_2014.shp") %>% st_as_sf()
shp_2014 <- st_transform(shp_2014, st_crs(pgc_cd)) %>% 
  mutate(area_tract = st_area(.) %>% as.numeric()) %>% 
  select(-NAME)


demo_2014 <- shp_2014 %>% 
  left_join(demo_2014_text,by = "GEOID")
n_distinct(demo_2014$GEOID)

mapview(demo_2014)
mapview(pgc_cd,col.regions = NA,lwd=2, color="grey") +mapview(demo_2014, alpha = 0.5,col.regions = "blue")

#2022
demo_2022_text <- read_csv("Census_R/Outputs/PGC_data_2022_selected.csv") %>% 
  mutate(GEOID = as.character(GEOID))
shp_2022 <- st_read("Census_R/Outputs/Census_shp/PGC_shp_2022.shp") %>% st_as_sf()
shp_2022 <- st_transform(shp_2022, st_crs(pgc_cd)) %>% 
  mutate(area_tract = st_area(.) %>% as.numeric()) %>% 
  select(-NAME)

demo_2022 <- shp_2022 %>%
  left_join(demo_2022_text, by = "GEOID") 

#Join demo with council district function-------------------------------------------------------
process_intersection_data <- function(demo_data, demo_data_text, district_data) {
  
  # Step 1: Perform spatial intersection
  intersected <- st_intersection(demo_data, district_data)
  unique_geoid_count <- n_distinct(intersected$GEOID)
  
  # Step 2: Calculate area overlap and proportion
  overlap <- intersected %>%
    mutate(
      area_overlap = st_area(geometry) %>% as.numeric(),       # Area of each intersected piece
      proportion = area_overlap /area_tract              # Proportion of each tract within the district
    )
  
  # Step 3: Identify duplicates and non-duplicates
  overlap_with_duplicates <- overlap %>%
    add_count(GEOID, name = "duplicate_count")
  
  duplicates_only <- overlap_with_duplicates %>%
    filter(duplicate_count > 1)
  
  non_duplicates <- overlap_with_duplicates %>%
    filter(duplicate_count == 1)
  
  # Step 4: Filter duplicates by proportion and separate groups
  overlap_filtered <- duplicates_only %>% 
    filter(proportion > 0.02)
  
  overlap_filtered2 <- overlap_filtered %>%
    add_count(GEOID) %>%
    filter(n > 1) %>%
    select(-n)
  
  overlap_filtered3 <- overlap_filtered %>%
    add_count(GEOID) %>%
    filter(n == 1) %>%
    select(-n)
  
  # Step 5: Multiply selected columns by proportion and adjust area_tract
  overlap_filtered2_multiplied <- overlap_filtered2 %>%
    mutate(across(3:81, ~ . * proportion)) %>% 
    mutate(area_tract = area_tract * proportion)
  
  # Step 6: Combine cleaned data
  clean_demo14 <- bind_rows(non_duplicates, overlap_filtered2_multiplied, overlap_filtered3)
  
  # Step 7: Update column names
  colnames(clean_demo14)[3:81] <- colnames(demo_data_text)[2:80]  # Adjusting indices to match
  
  # # Step 8: Save data to CSV and Shapefile
  # write.csv(st_drop_geometry(clean_demo14), output_csv_path)
  # st_write(clean_demo14, output_shp_path, delete_layer = TRUE)
  
  # Step 9: Prepare for grouping by dropping selected columns
  clean_demo14_2 <- clean_demo14 %>%
    select(-GEOID, -DAMS_LINK, -COUNCILMEM, -POLITICAL_, 
           -TELEPHONE, -EMAIL_ADDR, -ACREAGE, -SHAPE_AREA, 
           -SHAPE_LEN, -area_overlap, -proportion, 
           -duplicate_count, -geometry) %>%
    st_drop_geometry()
  
  # Step 10: Group by DISTRICT_N and summarize
  clean_demo14_summarized <- clean_demo14_2 %>%
    group_by(DISTRICT_N) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
  
  # Return the summarized data
  return(list(clean_data = clean_demo14, summarized_data = clean_demo14_summarized))
}

##run the year results-------------------------------------------------------
# access demo 2014
results14<- process_intersection_data(
  demo_data = demo_2014, 
  district_data = pgc_cd,
  demo_data_text = demo_2014_text
)


clean_demo14 <- results14$clean_data
clean_demo14_summarized <- results14$summarized_data %>% select(-area_tract)

write.csv(st_drop_geometry(clean_demo14), "Census_R/Outputs/Data_joined_councilD/clean_demo14.csv")
st_write(clean_demo14, "Census_R/Outputs/Data_joined_councilD/SHP/clean_demo14.shp", delete_layer = TRUE)
write.csv(clean_demo14_summarized, "Census_R/Outputs/Data_joined_councilD/clean_demo14_by_cd.csv",row.names = FALSE)


# access demo 2022
results22<- process_intersection_data(
  demo_data = demo_2022, 
  district_data = pgc_cd,
  demo_data_text = demo_2022_text
)

clean_demo22 <- results22$clean_data
clean_demo22_summarized <- results22$summarized_data %>% select(-area_tract)

write.csv(st_drop_geometry(clean_demo22), "Census_R/Outputs/Data_joined_councilD/clean_demo22.csv")
st_write(clean_demo22, "Census_R/Outputs/Data_joined_councilD/SHP/clean_demo22.shp", delete_layer = TRUE)
write.csv(clean_demo22_summarized, "Census_R/Outputs/Data_joined_councilD/clean_demo22_by_cd.csv", row.names = FALSE)

colnames(clean_demo22_summarized)

numeric_columns <- setdiff(names(clean_demo22_summarized), c("DISTRICT_N"))
CD_trend <- clean_demo22_summarized  %>% 
  select(DISTRICT_N, all_of(numeric_columns)) %>%
  mutate(across(all_of(numeric_columns), ~ (. - clean_demo14_summarized[[cur_column()]]) / clean_demo14_summarized[[cur_column()]], .names = "{.col}_trend")) %>%
  select(DISTRICT_N, ends_with("_trend"))  # Keep only trend columns along with GEOID and geometry


wb <- createWorkbook()

addWorksheet(wb, "CD_trend")
writeData(wb, "CD_trend", CD_trend) 

addWorksheet(wb, "CD_2022")
writeData(wb, "CD_2022", clean_demo22_summarized) 

addWorksheet(wb, "CD_2014")
writeData(wb, "CD_2014", clean_demo14_summarized) 

# Save the workbook
saveWorkbook(wb, "Census_R/CD_trend_summary.xlsx", overwrite = TRUE)

write.csv(st_drop_geometry(CD_trend), "Census_R/Outputs/Data_joined_councilD/clean_demo_trend.csv")




#Join demo with council district 2014 dont use-------------------------------------------------------
intersected <- st_intersection(demo_2014, pgc_cd)
n_distinct(intersected$GEOID)

overlap <- intersected %>%
  mutate(
    area_overlap = st_area(geometry) %>% as.numeric(),      # Area of each intersected piece
    proportion = area_overlap / area_tract                  # Proportion of each tract within the district
  )

overlap_with_duplicates <- overlap %>%
  add_count(GEOID, name = "duplicate_count")

duplicates_only <- overlap_with_duplicates %>%
  filter(duplicate_count > 1)
n_distinct(duplicates_only$GEOID) #104

non_duplicates <- overlap_with_duplicates %>%
  filter(duplicate_count == 1)
n_distinct(non_duplicates$GEOID)

# clean duplicates
overlap_filtered <- duplicates_only %>% 
  filter (proportion > 0.02)
n_distinct(overlap_filtered$GEOID)

overlap_filtered2 <- overlap_filtered %>%
  add_count(GEOID) %>%          # Count occurrences of each GEOID
  filter(n > 1) %>%            # Keep only rows where GEOID appears once
  select(-n) 
n_distinct(overlap_filtered2$GEOID)

overlap_filtered3 <- overlap_filtered %>%
  add_count(GEOID) %>%          # Count occurrences of each GEOID
  filter(n == 1) %>%            # Keep only rows where GEOID appears once
  select(-n) 
n_distinct(overlap_filtered3$GEOID)

overlap_filtered2_multiplied <- overlap_filtered2 %>%
  mutate(across(3:61, ~ . * proportion)) %>% 
  mutate(area_tract = area_tract * proportion)
n_distinct(overlap_filtered2_multiplied$GEOID)

clean_demo14 <- rbind(non_duplicates,
                      overlap_filtered2_multiplied,
                      overlap_filtered3)
n_distinct(clean_demo14$GEOID)

colnames(clean_demo14)[0:60] <- colnames(demo_2014_text)[0:60]

colnames(clean_demo14)

write.csv(st_drop_geometry(clean_demo14), "Census_R/Outputs/Data_joined_councilD/clean_demo14.csv")
st_write(clean_demo14, "Census_R/Outputs/Data_joined_councilD/clean_demo14.shp")

colnames(clean_demo14)

clean_demo14_2 <- clean_demo14 %>%
  select(-GEOID, -NAME, -DAMS_LINK, -COUNCILMEM, -POLITICAL_, 
         -TELEPHONE, -EMAIL_ADDR, -ACREAGE, -SHAPE_AREA, 
         -SHAPE_LEN, -area_overlap, -proportion, 
         -duplicate_count, -geometry) %>% 
  st_drop_geometry()
colnames(clean_demo14_2)

clean_demo14_summarized <- clean_demo14_2 %>%
  group_by(DISTRICT_N) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


#Join demo with council district 2022 dont use-------------------------------------------------------
intersected <- st_intersection(demo_2022, pgc_cd)
n_distinct(intersected$GEOID)

overlap <- intersected %>%
  mutate(
    area_overlap = st_area(geometry) %>% as.numeric(),      # Area of each intersected piece
    proportion = area_overlap / area_tract                  # Proportion of each tract within the district
  )

overlap_with_duplicates <- overlap %>%
  add_count(GEOID, name = "duplicate_count")

duplicates_only <- overlap_with_duplicates %>%
  filter(duplicate_count > 1)
n_distinct(duplicates_only$GEOID) #104

non_duplicates <- overlap_with_duplicates %>%
  filter(duplicate_count == 1)
n_distinct(non_duplicates$GEOID)

# clean duplicates
overlap_filtered <- duplicates_only %>% 
  filter (proportion > 0.02)
n_distinct(overlap_filtered$GEOID)

overlap_filtered2 <- overlap_filtered %>%
  add_count(GEOID) %>%          # Count occurrences of each GEOID
  filter(n > 1) %>%            # Keep only rows where GEOID appears once
  select(-n) 
n_distinct(overlap_filtered2$GEOID)

overlap_filtered3 <- overlap_filtered %>%
  add_count(GEOID) %>%          # Count occurrences of each GEOID
  filter(n == 1) %>%            # Keep only rows where GEOID appears once
  select(-n) 
n_distinct(overlap_filtered3$GEOID)

overlap_filtered2_multiplied <- overlap_filtered2 %>%
  mutate(across(3:61, ~ . * proportion)) %>% 
  mutate(area_tract = area_tract * proportion)
n_distinct(overlap_filtered2_multiplied$GEOID)

clean_demo14 <- rbind(non_duplicates,
                      overlap_filtered2_multiplied,
                      overlap_filtered3)
n_distinct(clean_demo14$GEOID)

colnames(clean_demo14)[0:60] <- colnames(demo_2014_text)[0:60]

colnames(clean_demo14)

write.csv(st_drop_geometry(clean_demo14), "Census_R/Outputs/Data_joined_councilD/clean_demo14.csv")
st_write(clean_demo14, "Census_R/Outputs/Data_joined_councilD/clean_demo14.shp")

colnames(clean_demo14)

clean_demo14_2 <- clean_demo14 %>%
  select(-GEOID, -NAME, -DAMS_LINK, -COUNCILMEM, -POLITICAL_, 
         -TELEPHONE, -EMAIL_ADDR, -ACREAGE, -SHAPE_AREA, 
         -SHAPE_LEN, -area_overlap, -proportion, 
         -duplicate_count, -geometry) %>% 
  st_drop_geometry()
colnames(clean_demo14_2)

clean_demo14_summarized <- clean_demo14_2 %>%
  group_by(DISTRICT_N) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

overlap_test3 <- demo_2014 %>% 
  filter (GEOID == "24033801302")

mapview(pgc_cd) +mapview(overlap_test3)

overlap_test4 <- duplicates_only %>% 
  filter (GEOID == "24033801302")




