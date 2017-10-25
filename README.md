# timers_f

This module abstracts common use cases for system time.
(c) Petar Mimica 2014

Data type: timer_t
- contains full date and time (to millisecond precision), as well as the time zone

1. Constructors:
  * timer_time: returns a timer_t type variable with the current system time (converted to CET if necessary)
  * timer_zero: returns a timer_t type variable with all zeros
  * timer_init: initializes a timer_t variable with the arguments passed by the user
2. Printing
  * timer_t2s_dmy_hms: returns a string with the current date and time
  * timer_t2s_hms: returns a string with only the current hour, minute and second
  * timer_t2s_dmy: returns a string with only the current day, month and year
  * timer_t2s_hms_long: returns a string with only the hour (4 digits), minute and second (used for time intervals)
  * timer_t2s_dhms: returns a string with only the day, hour, minute and second (used for time intervals)
3. Operations
  * timer_sum: returns the sum of a date (dmy and hms) and an interval (only hms)
  * timer_diff: returns the difference of a date (dmy and hms( and an interval (only hms)
  * timer_sum_hms: returns sum of two time intervals
  * timer_diff_hms: retuns a difference of two time intervals        
  * timer_inc_hour: increments hours of a timer_t variable      
  * timer_inc_minute: increments minutes of a timer_t variable
  * timer_inc_second: increments seconds of a timer_t variable      
  * timer_inc_ms: increments milliseconds of a timer_t variable
  * timer_dist: returns the "distance" (in milliseconds) between two dates      
4. Conversion
  * timer_t2ms: converts timer_t to number of milliseconds since January 1st, 2014 00:00:00.000 CET
  * timer_ms2t: converts the number of milliseconds since January 1st, 2014 00:00:00.000 CET to timer_t
  * timer_t_hms2ms: converts only hours, minutes and seconds into milliseconds (used for time intervals)
  * timer_ms2t_hms: converts the number of milliseconds to hours, minutes and seconds (used for time intervals)
