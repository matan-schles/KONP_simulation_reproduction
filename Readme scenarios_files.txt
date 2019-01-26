There are two files that contain functions that creates all the simulation scenarios that appear in the article

--- configurations_2_sample
contains all the scenarios for the 2_sample case, each name of a function of a scenario is in the format "scenario_letter_censor_type".
Where the letter stands for the scenario letter from article, and there are four censoring options for each scenario:
light - equal censoring of approximately 25% in both groups
medium - equal censoring of approximately 50% in both groups
diff - unequal censoring distributions with censoring rates of approximately 40% and 55% in both groups 
diff_much - unequal censoring distributions with censoring rates of approximately 27%and 55% in both groups 


--- configurations_K_sample
contains all the scenarios for the K_sample case (K>2), each name of a function of a scenario is in the format "scenario_letter_K_censor_type".
Where the letter stands for the scenario letter from article, K stands for K, and there are four censoring options for each scenario:
light - equal censoring of approximately 25% in all groups
medium - equal censoring of approximately 50% in all groups
part_diff - unequal censoring distributions in part of the groups
all_diff - unequal censoring distributions in all groups