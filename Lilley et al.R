library(lmerTest)
library(MuMIn)

#### ----- ANALYSES ----- #####

#### Body mass throughout reproductive period ####

mother_BM_df # dataset used (see Excel-file)

# Testing if the model is better with our without a quadratic time effect
BM_model <- lmer(MotherWeight ~ rep_period*DaysOld + total_rain + (1 | Mother) + (1 | Year), data = mother_BM_df, REML = F)
BM_model_quadratic <- lmer(MotherWeight ~ rep_period*(DaysOld + I(DaysOld^2)) + total_rain + (1 | Mother) + (1 | Year), data = mother_BM_df, REML = F)

AICc(BM_model, BM_model_quadratic) # model with quadratic time-effect is better

# model results reported in article
summary(BM_model_quadratic)


#### Number of foraging trips ####

foraging_trips_df # dataset used (see Excel-file)

# Testing if the model is better with our without a quadratic time effect
trip_number_model <- lm(trips_per_bat ~ rep_period*(time_to_birth + night_Ta) + night_length + night_rain + night_wind, data = foraging_trips_df)
trip_number_model_quadratic <- lm(trips_per_bat ~ rep_period*(time_to_birth + I(time_to_birth^2) + night_Ta) + night_length + night_rain + night_wind, data = foraging_trips_df)

AICc(trip_number_model, trip_number_model_quadratic) # model with quadratic time-effect is better

# model results reported in article
summary(trip_number_model_quadratic)



#### Duration of foraging trips ####

foraging_durations_df # dataset used (see Excel-file)


# Testing if the model is better with our without a quadratic time effect
trip_duration_model <- lmer(duration_min ~ rep_period*(time_to_birth + night_Ta) + night_length + night_wind + night_rain + (1 | StartDate), data= foraging_durations_df, REML = F)
trip_duration_model_quadratic <- lmer(duration_min ~ rep_period*(time_to_birth + I(time_to_birth^2) + night_Ta) + night_length + night_wind + night_rain + (1 | StartDate), data= foraging_durations_df, REML = F)

AICc(trip_duration_model, trip_duration_model_quadratic) # model with quadratic time-effect is better

# model results reported in article
summary(trip_duration_model_quadratic)



#### Timing of first emergences ####

activity_out_df # dataset used (see Excel-file)

# Testing if the model is better with our without a quadratic time effect
emergence_model <- lmer(time_since_sunset ~ rep_period*(sunset_Ta + time_to_birth) + night_length + sunset_wind + sunset_rain + (1 | StartDate), data=activity_out_df, REML=F)
emergence_model_quadratic <- lmer(time_since_sunset ~ rep_period*(sunset_Ta + time_to_birth + I(time_to_birth^2)) + night_length + sunset_wind + sunset_rain + (1 | StartDate), data=activity_out_df, REML=F)

AICc(emergence_model, emergence_model_quadratic) # model with quadratic time-effect is better

# model results reported in article
summary(emergence_model_quadratic)




#### Timing of last return ####

activity_in_df # dataset used (see Excel-file)

# Testing if the model is better with our without a quadratic time effect
return_model <- lmer(time_until_sunrise ~ rep_period*(night_Ta + time_to_birth) + night_length + night_wind + night_rain + (1 | StartDate), data = activity_in_df, REML = F)
return_model_quadratic <- lmer(time_until_sunrise ~ rep_period*(night_Ta + time_to_birth + I(time_to_birth^2)) + night_length + night_wind + night_rain + (1 | StartDate), data = activity_in_df, REML = F)

AICc(return_model, return_model_quadratic) # model with quadratic time-effect is better

summary(return_model_quadratic)

