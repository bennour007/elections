# FUNCTIONS USED IN THE SCRIPT 

# a function that takes the number of seats and slice the table by it 

slicer <- function(x){
  x %>% 
    slice_head(n = x$remaining_seat[1])
}



#matchng the names of the parties properly

# turning this into ta function 
# x is the data we wiull work on, y is the column we will modify the party, c is the real names with which we wanna compare
matchy <- function(x,c){
  x %>% 
    group_by(party) %>% 
    nest() %>% 
    mutate(matchy = map(party, function(.) rep(., 16) %>% as_tibble_col('old_names'))) %>% 
    mutate(matchy = map(matchy, function(.) mutate(., new_names = c))) %>% 
    mutate(matchy = map(matchy, function(.) mutate(., dist = stringdist(a = old_names,
                                                                        b = new_names,
                                                                        method = 'cosine')))) %>% 
    mutate(matchy = map(matchy, function(.) filter(., dist == min(.$dist)))) %>%
    unnest(matchy) %>% 
    ungroup() %>% 
    unnest(data) %>% 
    ungroup() 
}
