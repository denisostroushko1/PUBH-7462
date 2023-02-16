
# clear R working environment. Will run the file top tp bottom so to make sure there are no bugs in the script 

rm(list = ls())

# read in packages
source('/Users/denisostroushko/Desktop/UofM MS/Package master list .R')

# make sure CASEID is read in as a character string 
atus_data <- read_csv('/Users/denisostroushko/Desktop/UofM MS/MS 2023 - 1 Spring/PUBH 7462/PUBH-7462/W2/atus_data.csv', 
                      guess_max = Inf, 
                      col_types = list(CASEID = col_character()))

# note: CASEIDs are all the same 

### 
# all data wrangling for the questions below

# descriptions of the health status, taken from the data dictionary 
  atus_data$GENHEALTH_DESCR <- 
    
    factor(
      with(atus_data, 
           case_when(
             GENHEALTH == 1 ~ "Excellent",
             GENHEALTH == 2 ~ "Very good",
             GENHEALTH == 3 ~ "Good",
             GENHEALTH == 4 ~ "Fair",
             GENHEALTH == 5 ~ "Poor",
             GENHEALTH == 96 ~ "Refured",
             GENHEALTH == 97 ~ "Don't know",
             GENHEALTH == 99 ~ "NIU (Not in universe)",
             TRUE ~ "NA"
           )),
      levels = c(
        "Excellent", "Very good", "Good", "Fair", 
            "Poor", "Refused", "Don't know", "NIU (Not in universe)")
    )

# descriptions of sex 
  atus_data$SEX_DESCR <- 
    factor(
      with(atus_data, 
           case_when(
             SEX == 1 ~ "Male", 
             SEX == 2 ~ "Female"
           )), 
      levels = c("Male", "Female")
    )
  
# make sure all NAs are replaced with 0 
atus_data <- atus_data %>% 
    mutate_at(c("ACT_CAREHH", "ACT_CARENHH", "ACT_EDUC",   
                "ACT_FOOD", "ACT_HHACT", "ACT_PURCH", "ACT_RELIG",
                "ACT_SOCIAL", "ACT_SPORTS", "ACT_VOL", "ACT_WORK", "ACT_SLEEPING"), ~replace_na(.,0))
  

# Q1

  q1_table <- 
    atus_data %>% 
    
    group_by(
      GENHEALTH_DESCR
    ) %>% 
    
    summarize(
      N = n(), 
      age = round(mean(AGE), 1)
    )
  
  q1_table %>% 
    kable(booktabs = T, full_wifth = T, col.names = c("GENHEALTH", "N", "AVG_AGE")) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))


# Q2

# create initial table in the tidy format 
  
  q2_initial_table <- 
    atus_data %>% 
    
    group_by(
      SEX_DESCR, GENHEALTH_DESCR
    ) %>% 
    
    summarize(N = n())
  
  q2_initial_table %>% 
    kable(booktabs = T, full_wifth = T) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))
  
# create a wide version of the table 
  
  q2_initial_wide_table <- 
    q2_initial_table %>% 
    pivot_wider(
      names_from = "SEX_DESCR", 
      values_from = "N"
    )

# rename columns here instead of using 'prefix' since we also need to rename genhealth column name 
# also can change column in just the kable output, as I show in the example below 
  
  colnames(q2_initial_wide_table) <- c("GENHEALTH", "N_MALE", "N_FEMALE")
  
  q2_initial_wide_table %>% 
    kable(booktabs = T, full_wifth = T) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))

# final table 
  
  q2_final_table <- 
    bind_cols(q1_table %>% select(GENHEALTH_DESCR, N), 
              q2_initial_wide_table %>% select(N_MALE, N_FEMALE)
              ) %>% 
    rename(GENHEALTH = GENHEALTH_DESCR)
  
  q2_final_table %>% 
    kable(booktabs = T, full_wifth = T) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))
  
# optional table 
  q2_final_table$P_MALE <- with(q2_final_table, N_MALE/N)
  q2_final_table$P_FEMALE <- with(q2_final_table, N_FEMALE/N)
  
  # nicely formatted summary of the data for males 
  q2_final_table$M_COL_PRESENT <- 
    with(q2_final_table, 
         paste0(
           prettyNum(N_MALE, big.mark = ","), 
           " (", 
           round(P_MALE, 2) * 100, 
           " %)"
         )
    )
  
  # same for females 
  q2_final_table$F_COL_PRESENT <- 
    with(q2_final_table, 
         paste0(
           prettyNum(N_FEMALE, big.mark = ","), 
           " (", 
           round(P_FEMALE, 2) * 100, 
           " %)"
         )
    )
  
  # probably would be better to write a function that creates a nice formatting and then apply it to both M and F columns 
  
  q2_final_table %>% 
    select(GENHEALTH, N, M_COL_PRESENT, F_COL_PRESENT) %>% 
    
    kable(booktabs = T, full_width = T, 
          col.names = c(
            "GENERAL HEALTH", "TOTAL, N (%)", "MALE, N (%)", 	"FEMALE, N(%)"
          ),
          format.args = list(big.mark = ",") # add formatting of total column here instead of modifying the data itself
          ) %>% 
    
    kable_styling(bootstrap_options = c("striped", "condensed"))
  

# Q3 

# okay so it seems that the activities are all in their own column. So, we summarize the data and then pivot it 
# not a reproducible solution since we hard coded the column names. 
  
  # atus_data %>% 
  #   summarize(
  #     "SLEEPING" = mean(ACT_SLEEPING), 
  #     "SOCIAL"	= mean(ACT_SOCIAL),
  #     "WORK" = mean(ACT_WORK),
  #     "HHACT" = mean(ACT_HHACT),
  #     "FOOD" = mean(ACT_FOOD),
  #     "CAREHH" = mean(ACT_CAREHH),
  #     "PURCH"	= mean(ACT_PURCH),
  #     "SPORTS"	= mean(ACT_SPORTS),
  #     "EDUC"	= mean(ACT_EDUC),
  #     "RELIG"	= mean(ACT_RELIG),
  #     "CARENHH"	= mean(ACT_CARENHH),
  #     "VOL"	= mean(ACT_VOL)
  #   ) %>% 
  #   
  #   pivot_longer(
  #     cols = c('SLEEPING', 'SOCIAL', 'WORK', 'HHACT', 'FOOD', 'CAREHH', 'PURCH', 'SPORTS', 'EDUC', 'RELIG', 'CARENHH', 'VOL' ), 
  #     names_to = 'ACTIVITY', 
  #     values_to = 'MINS_PER_DAY'
  #   ) %>% 
  #   
  #   mutate(MINS_PER_DAY = round(MINS_PER_DAY)) %>% 
  # 
  #   arrange(-MINS_PER_DAY) ->  sorted_data_store # sort the table according to the requirement and store the table for future use 
  #   
  #   sorted_data_store %>% 
  #     kable(booktabs = T, full_width = T) %>%
  #     kable_styling(bootstrap_options = c("striped", "condensed"))
  
# A better practice and a better solution by the TA. 
atus_long <- atus %>% pivot_longer(starts_with("ACT_"), # pivot all columns that contain a string ACT_ in it
                    names_to = "activity",
                    values_to = "minutes", 
                    names_prefix = "ACT_") 

activity_means <- atus_long %>% # now we have a much more simple chunk of code that works with a restricted class or 
                                # number of columns 
  group_by(activity) %>%
  summarize(mins_per_day = round(mean(minutes, na.rm = TRUE), 0)) %>%
  arrange(desc(mins_per_day))
  
  
  # create this id here so that we can use it in problem 4 for the calclation of average time use. 
  # apparently CASEID does not uniquely identify rows, hence, we need to create some sort of ID so that we can calcuate the number 
  # of unique individuals from the long format table 
  atus_data$fake_id = rownames(atus_data)
  
  atus_data_long <- 
    atus_data %>% 
    
    select(
      fake_id, 
      ACT_SLEEPING, ACT_SOCIAL, ACT_WORK, ACT_HHACT, ACT_FOOD, 
      ACT_CAREHH, ACT_SPORTS, ACT_EDUC, ACT_RELIG, ACT_PURCH, ACT_CARENHH, ACT_VOL,
      GENHEALTH_DESCR # bring this varaible here now so that we can use it later in Q4
    ) %>% 
    
    pivot_longer(
      cols = c('ACT_SLEEPING', 'ACT_SOCIAL', 'ACT_WORK', 'ACT_HHACT', 'ACT_FOOD', 
      'ACT_CAREHH', 'ACT_SPORTS', 'ACT_EDUC', 'ACT_RELIG', 'ACT_PURCH', 'ACT_CARENHH', 'ACT_VOL'), 
      
    names_to = "ACT_TYPE", 
    
    values_to = "ACT_MINUTES"
    ) %>% 
    
    arrange(ACT_TYPE) %>% 
    
    mutate(ACT_TYPE_DESCR = substr(ACT_TYPE, 5,100))
  
  ggplot(data = atus_data_long, 
         aes(x = ACT_MINUTES, 
             y =ACT_TYPE_DESCR, 
             color = ACT_TYPE_DESCR, 
             fill = ACT_TYPE_DESCR)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) + 
    ylab("") + 
    xlab("Minutes Per Day") 
    
# extra question: need to display categories according to sorted list. So, set levels according to sorted list 
# did not see the hint right away, hopefully this solution is good practice as well 
  sorted_data_store <- sorted_data_store %>% arrange(MINS_PER_DAY)
  atus_data_long$ACT_TYPE_DESCR <- factor(atus_data_long$ACT_TYPE_DESCR, levels = sorted_data_store$ACTIVITY)

  ggplot(data = atus_data_long, 
         aes(x = ACT_MINUTES, 
             y =ACT_TYPE_DESCR, 
             color = ACT_TYPE_DESCR, 
             fill = ACT_TYPE_DESCR)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) + 
    
    scale_x_continuous(breaks = c(0, 500, 1000, 1500), 
                       trans = 'sqrt') + 
    
    ylab("") + 
    xlab("Minutes Per Day") 

# Q4 

# keep working with the long data we already have in hand
# we already applied pivot_longer when creating a long version of the data 

  atus_data_long$ACT_GROUP <- 
    with(atus_data_long, 
         case_when(
           ACT_TYPE %in% c('ACT_CAREHH', 'ACT_CARENHH', 'ACT_HHACT', 'ACT_SLEEPING') ~ "HOUSEHOLD", 
           TRUE ~ "NON-HOUSEHOLD"
         ))


atus_data_long %>% 
  
  group_by(
    GENHEALTH_DESCR, ACT_GROUP
  ) %>% 
  
  summarise(
    avg = round(sum(ACT_MINUTES)/length(unique(fake_id))) # this is where we need our id, itherwse we get numbers that are in the 100's, 
                                                          #  due to averaging over a much larger number of rows 
  ) %>%  

  pivot_wider( # we can use pivot wider here, finishing the requirement 
    names_from = ACT_GROUP, 
    values_from = c(avg)
  ) %>% 
  
  kable(booktabs = T, full_width = T, 
        col.names = c("GENHEALTH_DESCR", "MINUTES_NON-HOUSEHOLD",	"MINUTES_HOUSEHOLD")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"))


# optional: 
# list of household activities: 
h_list <- c('ACT_CAREHH', 'ACT_CARENHH', 'ACT_HHACT', 'ACT_SLEEPING') # store a list of all houseghold activities 

non_h_list <- colnames(atus_data)[substr(colnames(atus_data), 1, 4) == "ACT_" &  # select only those columns that start with ACT
                                    !(colnames(atus_data) %in%  h_list) ]          # and remove those that are on the household list --> produce a list of non-household 

atus_data$HOUSEHOLD_M <- 
  with(atus_data, 
       # have to wrap a list of columns to select with an 'all_of' command. 
       # it ensures a strict selection, so if the data set does not have a column, an error with occur
       # this will ensure that if an error occurs, we know where it is 
       rowSums( atus_data %>% select(all_of(h_list)) # now we can neatly sum columns without reying on A + B + .... equation that is tediuos to manage 
  ))

atus_data$NON_HOUSEHOLD_M <- 
  with(atus_data, 
       rowSums( atus_data %>% select(all_of(non_h_list)) 
  ))


atus_data %>% 
  
  group_by(
    GENHEALTH_DESCR
  ) %>% 
  
  summarize(
    `MINUTES_NON-HOUSEHOLD`	= round(mean(NON_HOUSEHOLD_M)), # now to simply go to the wide table because we can take a summary of two distinct columns 
    MINUTES_HOUSEHOLD = round(mean(HOUSEHOLD_M)) , 
  ) %>% 
  
  kable(booktabs = T, full_width = T, 
        col.names = c("GENHEALTH_DESCR", "MINUTES_NON-HOUSEHOLD",	"MINUTES_HOUSEHOLD")) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"))




