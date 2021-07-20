library(tidyverse)
library(stringdist)
library(stringr)
library(ggpol)

files <- list.files("data",full.names = T)

data_raw <- map(files, read_csv)

names(data_raw) <- tools::file_path_sans_ext(files) %>% str_remove("data/")

n <- names(data_raw)

data_raw <- map2(data_raw,n, function(x,y){
  mutate(x, state = y)
  }
)


seats_state <- n %>% 
  as_tibble_col(column_name = "state") %>% 
  mutate(state_seats = c(8,6,10,9,7,7,8,9,9,5,6,8,7,9,9,7,6,7,9,8,6,10,4,4,9,8,5))

data_unite <- data_raw %>%
  map(~ pivot_longer(., cols = !c(list,state), names_to = "deleg", values_to = "votes")) %>%
  map(~ rename(., party = "list")) %>%
  map(~ group_by(., party, state) %>% summarise(votes = sum(votes))) %>%
  map(~ ungroup(.)) %>%
  #map(~ nest_by(., state)) %>% 
  bind_rows() %>%
  left_join(seats_state, by = "state")  %>% 
  #mutate(data = list(data)) %>%
  group_by(state) %>%
  mutate(total_votes = sum(votes),
         hare_quota = total_votes/state_seats) %>% 
  ungroup() %>%
  mutate(percent = votes/total_votes) %>% 
  # adding percent step (3ataba)
  filter(percent >= 0.05) %>% 
  mutate(party_quota = votes/hare_quota,
         quota_seats = as.integer(party_quota),
         remains = party_quota - quota_seats, 
         remains_seats = 0) %>%
  group_by(state) %>%
  mutate(remaining_seat = state_seats - sum(quota_seats)) %>%
  #adding a 3ataba 5%
  #filter(percent >= 0.05)
  arrange(desc(party_quota), .by_group = T) %>%
  ungroup()

# create a vector with each state and its respective remainng seats

slice_factor <- data_unite %>% 
  group_by(state, state_seats, remaining_seat) %>% 
  select(state, remaining_seat) %>% 
  slice_head(., n = 1) %>%
  ungroup() %>% 
  select(remaining_seat) %>% 
  as.matrix()

# create a table where the remaining sequence is ready and all is needed i just to add +1

remaining_list <- data_unite %>% 
  select(state, party, remaining_seat) %>% 
  nest_by(state, remaining_seat) %>% 
  mutate(data2 = map2(data, remaining_seat, function(x,y) rep(x,3)[1:y])) %>% 
  unnest(data2) %>% 
  add_column(remains_seats = 0) %>% 
  select(-data) %>% 
  nest() 


# a function that takes the number of seats and slice the table by it 

slicer <- function(x){
  x %>% 
    slice_head(n = x$remaining_seat[1])
}

good_data_remains <- data_unite %>% 
  group_by(state, state_seats) %>%
  nest() %>% 
  # slice when the percent requirement is not 0%
  #mutate(data = map(data, slicer)) %>% 
  left_join(remaining_list, by = 'state') %>% 
  unnest(data.y) %>% 
  mutate(remains_seats = remains_seats+1) %>% 
  group_by(data2) %>% 
  summarise(r_seats = sum(remains_seats)) %>% 
  mutate(data2 = str_replace_all(data2, pattern = '\r', replacement = ' '))


good_data_quota <- data_unite %>% 
  group_by(party) %>%
  summarise(q_seats = sum(quota_seats)) %>% 
  mutate(party = str_replace_all(party, pattern = '\r', replacement = ' '))


#
# Clean each good data on it own 
#


# To visualize this data properly, we will need to match the names of each party with a list of coherent names
# I've done this before I just nned to rememeber how I ddid it? I think in the education project
# found this one in the map making code, I need to find a way to make this one  work properly with my data : 

# cleaning the \r characters that durties our data and prohibit matching, we will still have to match the output though with clean names

good_data_1 <- inner_join(good_data_quota, 
                          good_data_remains, 
                          by = c('party' = 'data2')) 
  # group_by(party) %>% 
  # summarise(q_seats = sum(q_seats),
  #           r_seats = sum(r_seats),
  #           total_seats = q_seats+ r_seats)
  
# vector of clean party names to match with dirty ones in the comigng sections

my_parties_names <- c('افاق تونس','القائمة المستقلة الإقلاع','حركة النهضة','حركة نداء تونس','الجبهة الشعبية',
                      'تيار المحبة','الجبهة الوطنية للإنقاذ','الحزب الجمهوري','حركة الشعب',
                      'حزب الاتحاد الوطني الحر', 'حزب التحالف' , 'حزب التيار الديمقراطي',
                      'المؤتمر من أجل الجمهورية', 'حزب المبادرة', 'حزب صوت الفلاحين',  'لمجد الجريد' )


#matchng the names of the parties properly

good_data_2 <- good_data_1 %>% 
  group_by(party) %>% 
  nest() %>% 
  mutate(matchy = map(party, function(.) rep(., 16) %>% as_tibble_col('old_names'))) %>% 
  mutate(matchy = map(matchy, function(.) mutate(., new_names = my_parties_names))) %>% 
  mutate(matchy = map(matchy, function(.) mutate(., dist = stringdist(a = old_names,
                                                                      b = new_names,
                                                                      method = 'jaccard')))) %>% 
  mutate(matchy = map(matchy, function(.) filter(., dist == min(.$dist)))) %>%
  unnest(matchy) %>% 
  ungroup() %>% 
  unnest(data) %>% 
  ungroup()  

#summarize the seats totals for each list
clean_data <- good_data_2 %>%  
  select(new_names, q_seats, r_seats) %>% 
  group_by(new_names) %>%
  summarise(q_seats = sum(q_seats),
            r_seats = sum(r_seats),
            total_seats = q_seats+ r_seats)

  
#Viz

clean_data %>% 
  group_by(new_names) %>% 
  mutate(new_names = fct_reorder(new_names, total_seats)) %>% 
  ggplot() +
  geom_col(aes(x = total_seats,
              y = new_names))
  geom_parliament(aes(seats = total_seats, fill = new_names)) +
  scale_fill_discrete(labels = clean_data$new_names)
  
  

  
  
                       
