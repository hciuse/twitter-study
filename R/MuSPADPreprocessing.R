here()

MuSPAD_subsetUZL <- read_delim("./data/MuSPAD_data_subset.csv") 
MuSPAD_s22 <- readRDS("./data/muspad_22-Nov-2022.rds")

MuSPAD_df <- left_join(MuSPAD_s22 %>% mutate(user_id = gsub("_", "-", user_id)) %>% select(user_id), MuSPAD_subsetUZL, by = join_by(user_id == merge_id))
