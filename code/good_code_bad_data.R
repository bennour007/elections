###############################################################################
# loading libraries

library(tidyverse)
library(janitor)
library(stringdist)
library(tidytext)

################################################################################
# load data 
# the folks in data4tunisia.org got us some interesting data, basically a clean 
# version of my previous work which I have neglegted
# https://www.data4tunisia.org/en/datasets/resultats-des-elections-legislatives-2014-par-centre-de-vote/

data_raw <- read_csv("data/data_2014.csv")

################################################################################
# exploring, taking note of issues

data_raw %>%
  glimpse()

# the data contains so much unnecessary information, what I need simply 
# the vote counts, and the the number of votes in each center divided by regions
# I appreciate only leaving the parliamentary parties and lumping the rest 
# to others. It will help get our data cleaner.

# cleaning row names
data_raw <- data_raw %>%
  clean_names() 

# discarding all percentages and computations already done
# starting from a clean page
data_processed_0 <- data_raw %>%
  select(center_name, deleg_name, circ_name, a_signing_voters_sum,
         registered_voters_sum, number_of_records_sum, l_blank_votes_sum, 
         k_cancelled_votes_sum, j_list_votes_sum, afek_tounes_sum, upl_sum,
         nida_tounes_sum, jabha_sum, ennahdha_sum, autres_sum)


# for now I will not focus on fixing the names, I will do it when need.

################################################################################
# I need to tidy the data up.
# parties shoud be in one column, and their sum in another.
# also the voters and the votes per deleg and circ

data_processed_1 <- data_processed_0 %>%
  pivot_longer(cols = afek_tounes_sum:autres_sum,
               names_to = "party_list",
               values_to = "center_party_votes") %>%
  group_by(deleg_name, party_list) %>%
  mutate(deleg_party_votes = sum(center_party_votes)) %>%
  ungroup() %>%
  group_by(circ_name, party_list) %>%
  mutate(circ_party_votes = sum(center_party_votes)) %>%
  ungroup()

data_processed_2 <- data_processed_1 %>%
  group_by(deleg_name) %>%
  mutate(deleg_voters = sum(a_signing_voters_sum)) %>% 
  ungroup() %>% 
  group_by(circ_name) %>%
  mutate(circ_voters = sum(a_signing_voters_sum)) %>% 
  ungroup() 

data_ready <- data_processed_2 %>%
  select(-a_signing_voters_sum, -registered_voters_sum, -number_of_records_sum, 
         -l_blank_votes_sum, -k_cancelled_votes_sum, -j_list_votes_sum)

################################################################################
# making the election counting function.
# this function should provide us with 2 columns, the number of seats affected 
# each state, and the number of seats from that state taken with the rest
# this functon takes the number of votes for each state, the quota and the seats

data_ready %>%
  group_by(circ_name, circ_voters) %>%
  filter(circ_name == "Sousse") %>%
  mutate(hare_quota = circ_voters/10,
         quota_party = circ_party_votes/hare_quota,
         quota_seats = as.integer(quota_party),
         remains = quota_party - quota_seats,
         percent = circ_party_votes/circ_voters)
  }))

function(){
  # hq = circ_voters/number of seats
  
  # q = circ_party_votes / hq for each list(group_by)
  
  # qs = int(qs) 
  
  # r = dec(qs)
  
  # p = circ_party_votes/circ_voters  
  
  
}