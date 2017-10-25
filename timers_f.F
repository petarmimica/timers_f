!     This module abstracts common use cases for system time.
!     (c) Petar Mimica 2014
!
!     Data type: timer_t
!     - contains full date and time (to millisecond precision), as well as the time zone
!
!     1. Constructors:
!
!         timer_time: returns a timer_t type variable with the current system time (converted to CET if necessary)
!         timer_zero: returns a timer_t type variable with all zeros
!         timer_init: initializes a timer_t variable with the arguments passed by the user
      
!
!     2. Printing
!
!         timer_t2s_dmy_hms: returns a string with the current date and time
!         timer_t2s_hms: returns a string with only the current hour, minute and second
!         timer_t2s_dmy: returns a string with only the current day, month and year
!         timer_t2s_hms_long: returns a string with only the hour (4 digits), minute and second (used for time intervals)
!         timer_t2s_dhms: returns a string with only the day, hour, minute and second (used for time intervals)
!
!     3. Operations
!
!         timer_sum: returns the sum of a date (dmy and hms) and an interval (only hms)
!         timer_diff: returns the difference of a date (dmy and hms( and an interval (only hms)
!         timer_sum_hms: returns sum of two time intervals
!         timer_diff_hms: retuns a difference of two time intervals        
!         timer_inc_hour: increments hours of a timer_t variable      
!         timer_inc_minute: increments minutes of a timer_t variable
!         timer_inc_second: increments seconds of a timer_t variable      
!         timer_inc_ms: increments milliseconds of a timer_t variable
!         timer_dist: returns the "distance" (in milliseconds) between two dates
      
!
!     4. Conversion
!         timer_t2ms: converts timer_t to number of milliseconds since January 1st, 2014 00:00:00.000 CET
!         timer_ms2t: converts the number of milliseconds since January 1st, 2014 00:00:00.000 CET to timer_t
!         timer_t_hms2ms: converts only hours, minutes and seconds into milliseconds (used for time intervals)
!         timer_ms2t_hms: converts the number of milliseconds to hours, minutes and seconds (used for time intervals)
      


      MODULE timers_f
      IMPLICIT NONE

!     time data structure
      TYPE :: timer_t
      INTEGER :: year, month, day, hour, minute, second, ms
      END TYPE timer_t
      
      CONTAINS


!     1. Constructors
      
!     get current date and time and return a data structure
      FUNCTION timer_time() RESULT(res)
      IMPLICIT NONE

!     return value
      TYPE(timer_t) :: res

!     temporary variables
      INTEGER, DIMENSION(8) :: values

!     call the system function
      CALL date_and_time(VALUES=values)

!     store into timer_t variables
      res%year = values(1)
      res%month = values(2)
      res%day = values(3)
      res%hour = values(5)
      res%minute = values(6)
      res%second = values(7)
      res%ms = values(8)

      
      RETURN
      END FUNCTION timer_time

!     return a zero time variabe
      FUNCTION timer_zero() RESULT(res)
      IMPLICIT NONE

      TYPE(timer_t) :: res

      res%ms = 0
      res%second = 0
      res%minute = 0
      res%hour = 0
      res%day = 0
      res%month = 0
      res%year = 0

      RETURN
      END FUNCTION timer_zero

!     initialize from user supplied values
      FUNCTION timer_init(year, month, day, hour, minute, second) RESULT(res)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: year, month, day, hour, minute, second
      type(timer_t) :: res

      res%year = year
      res%month = month
      res%day = day
      res%hour = hour
      res%minute = minute
      res%second = second
      res%ms = 0
      
      RETURN
      END FUNCTION timer_init

!     2. Printing

!     Print time in the format dd.mm.yyy hh:mm:ss
!     Exmple: 14.11.2014 09:55:15
      FUNCTION timer_t2s_dmy_hms(t) RESULT(str)
      IMPLICIT NONE

!     input variable
      type(timer_t), intent(in) :: t

!     return value
      CHARACTER(19) :: str

!     print into string
      write(str, "(i2.2, '.', i2.2, '.', i4.4, ' ',i2.2, ':', i2.2, ':', i2.2)") t%day, t%month, t%year, t%hour, t%minute, t%second
      
      RETURN
      END FUNCTION timer_t2s_dmy_hms

!     Print time in the format dd.mm.yyy
!     Exmple: 14.11.2014
      FUNCTION timer_t2s_dmy(t) RESULT(str)
      IMPLICIT NONE

!     input variable
      type(timer_t), intent(in) :: t

!     return value
      CHARACTER(10) :: str

!     print into string
      write(str, "(i2.2, '.', i2.2, '.', i4.4)") t%day, t%month, t%year
      
      RETURN
      END FUNCTION timer_t2s_dmy

!     Print time in the format hh:mm:ss
!     Exmple: 09:57:02
      FUNCTION timer_t2s_hms(t) RESULT(str)
      IMPLICIT NONE

!     input variable
      type(timer_t), intent(in) :: t

!     return value
      CHARACTER(8) :: str

!     print into string
      write(str, "(i2.2, ':', i2.2, ':', i2.2)") t%hour, t%minute, t%second
      
      RETURN
      END FUNCTION timer_t2s_hms

!     Print time in the format hhhh:mm:ss
!     Exmple: 1009:57:02
      FUNCTION timer_t2s_hms_long(t) RESULT(str)
      IMPLICIT NONE

!     input variable
      type(timer_t), intent(in) :: t

!     return value
      CHARACTER(10) :: str

!     print into string
      write(str, "(i4.4, ':', i2.2, ':', i2.2)") t%hour, t%minute, t%second
      
      RETURN
      END FUNCTION timer_t2s_hms_long

!     Print time in the format dddD hh:mm:ss
!     Exmple: 112D 09:57:02
      FUNCTION timer_t2s_dhms(t) RESULT(str)
      IMPLICIT NONE

!     input variable
      type(timer_t), intent(in) :: t

!     return value
      CHARACTER(13) :: str

!     print into string
      
      write(str, "(i3.3,'D ', i2.2, ':', i2.2, ':', i2.2)") (t%hour - mod(t%hour, 24)) / 24, mod(t%hour, 24), t%minute, t%second
      
      RETURN
      END FUNCTION timer_t2s_dhms
      



!     3. Operations

!     Sum an interval to a date
      FUNCTION timer_sum(dat, int) result(res)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) :: dat, int
      type(timer_t) :: res

      DOUBLEPRECISION :: date_ms, interval_ms

      date_ms = timer_t2ms(dat)
      interval_ms = timer_t_hms2ms(int)

      res = timer_ms2t(date_ms + interval_ms)
      
      RETURN
      END FUNCTION timer_sum

!     Subtract an interval from a date
      FUNCTION timer_diff(dat, int) result(res)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) :: dat, int
      type(timer_t) :: res

      DOUBLEPRECISION :: date_ms, interval_ms

      date_ms = timer_t2ms(dat)
      interval_ms = timer_t_hms2ms(int)

      res = timer_ms2t(date_ms - interval_ms)
      
      RETURN
      END FUNCTION timer_diff


!     Sum two time intervals
      FUNCTION timer_sum_hms(int1, int2) RESULT(res)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) :: int1, int2
      type(timer_t) :: res

      DOUBLEPRECISION :: int1_ms, int2_ms

      int1_ms = timer_t_hms2ms(int1)
      int2_ms = timer_t_hms2ms(int2)

      res = timer_ms2t_hms(int1_ms + int2_ms)
      
      RETURN
      END FUNCTION timer_sum_hms

!     Subtract two time intervals
      FUNCTION timer_diff_hms(int1, int2) RESULT(res)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) :: int1, int2
      type(timer_t) :: res

      DOUBLEPRECISION :: int1_ms, int2_ms

      int1_ms = timer_t_hms2ms(int1)
      int2_ms = timer_t_hms2ms(int2)

      res = timer_ms2t_hms(int1_ms - int2_ms)
      
      RETURN
      END FUNCTION timer_diff_hms
      
      
!     Increment/decrement milliseconds
      SUBROUTINE timer_inc_ms(t, millisecs)
      INTEGER NONE

!     input/output variables
      type(timer_t), intent(inout) :: t
      DOUBLEPRECISION, INTENT(IN) :: millisecs

!     temporary variables
      DOUBLEPRECISION :: cur_ms
      
!     convert to milliseconds
      cur_ms = timer_t2ms(t)

!     increment
      cur_ms = cur_ms + millisecs

!     convert back to timer_t
      t = timer_ms2t(cur_ms)

      RETURN
      END SUBROUTINE timer_inc_ms

!     Increment/decrement hours
      SUBROUTINE timer_inc_hour(t, hours)
      INTEGER NONE

!     input/output variables
      type(timer_t), intent(inout) :: t
      INTEGER, INTENT(IN) :: hours

!     use millisecond incrementer
      call timer_inc_ms(t, dfloat(hours) * 60d0 * 60d0 * 1000d0)
      
      RETURN
      END SUBROUTINE timer_inc_hour
 
      
!     Increment/decrement minutes
      SUBROUTINE timer_inc_minute(t, mins)
      INTEGER NONE

!     input/output variables
      type(timer_t), intent(inout) :: t
      INTEGER, INTENT(IN) :: mins

!     use millisecond incrementer
      call timer_inc_ms(t, dfloat(mins) * 60d0 * 1000d0)
      
      RETURN
      END SUBROUTINE timer_inc_minute

      
!     Increment/decrement seconds
      SUBROUTINE timer_inc_second(t, seconds)
      INTEGER NONE

!     input/output variables
      type(timer_t), intent(inout) :: t
      INTEGER, INTENT(IN) :: seconds

!     use millisecond incrementer
      call timer_inc_ms(t, dfloat(seconds) * 1000d0)
      
      RETURN
      END SUBROUTINE timer_inc_second


!     Distance
      FUNCTION timer_dist(t1, t2) RESULT(res)
      IMPLICIT NONE
      type(timer_t), intent(inout) :: t1, t2
      DOUBLEPRECISION :: res

      res = timer_t2ms(t2) - timer_t2ms(t1)
      
      RETURN
      END FUNCTION timer_dist


!     4. Conversions

!     Converts timer_t variable to number of milliseconds since January 1st, 2014 at 00:00:00.000
      FUNCTION timer_t2ms(t) RESULT(ms)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) :: t

      DOUBLEPRECISION :: ms
      
      INTEGER :: cur_year, cur_month, cur_day, cur_hour, cur_minute, cur_second

!     if the date is earlier than January 1st 2014, abort
      IF (t%year.lt.2014) STOP 'timer_t2ms does not handle years earlier than 2014'
      
!     start counting from January 1st, 2014
      ms = 0d0

!     match year
      cur_year = 2014
      DO WHILE(cur_year.lt.t%year)
         ms = ms + 86400d0 * 1d3 * 365
         IF (timer_is_leap(cur_year)) ms = ms + 86400d0 * 1d3
         cur_year = cur_year + 1
      ENDDO

!     match month
      cur_month = 1
      DO WHILE(cur_month.lt.t%month)
         ms = ms + 86400d0 * 1d3 * timer_days(t%year, cur_month)
         cur_month = cur_month + 1
      ENDDO

!     match day
      ms = ms + 86400d0 * 1d3 * (t%day - 1)

!     match hour
      ms = ms + 3600d0 * 1d3 * t%hour

!     match minute
      ms = ms + 60d0 * 1d3 * t%minute

!     match seconds
      ms = ms + 1d3 * t%second

!     match milliseconds
      ms = ms + t%ms
      
      RETURN
      END FUNCTION timer_t2ms

      
!     Converts a number of milliseconds since January 1st, 2014 at 00:00:00.000 to a timer_t variable
      FUNCTION timer_ms2t(ms) RESULT(t)
      IMPLICIT NONE

      DOUBLEPRECISION, INTENT(IN) :: ms
      type(timer_t) :: t

      DOUBLEPRECISION :: cur_ms, diff_ms
      type(timer_t) :: cur_t, diff_t

!     set t to zero
      t = timer_zero()

!     start filling t
      cur_ms = ms

!     milliseconds
      t%ms = modulo(cur_ms, 1000d0)
      cur_ms = cur_ms - modulo(cur_ms, 1000d0)

!     match year
      cur_t = timer_init(2014, 1, 1, 0, 0, 0)
      DO WHILE(timer_t2ms(cur_t).lt.cur_ms)
         cur_t%year = cur_t%year + 1
      ENDDO
      t%year = cur_t%year - 1

!     match month
      cur_t = timer_init(t%year, 1, 1, 0, 0, 0)
      DO WHILE(timer_t2ms(cur_t).lt.cur_ms)
         IF (cur_t%month.lt.12) THEN
            cur_t%month = cur_t%month + 1
         ELSE
            cur_t%month = 1
            cur_t%year = cur_t%year + 1
         ENDIF
      ENDDO
      
      IF (cur_t%year.eq.t%year) THEN
         t%month = cur_t%month - 1
      ELSE
         t%month = 12
      ENDIF

!     match day
      cur_t = timer_init(t%year, t%month, 1, 0, 0, 0)
      DO WHILE(timer_t2ms(cur_t).lt.cur_ms)
         IF (cur_t%day.lt.timer_days(cur_t%year, cur_t%month)) THEN
            cur_t%day = cur_t%day +1
         ELSE
            cur_t%day = 1
            cur_t%month = cur_t%month + 1
         ENDIF
      ENDDO

      IF (cur_t%month.eq.t%month) THEN
         t%day = cur_t%day - 1
      ELSE
         t%day = timer_days(t%year, t%month)
      ENDIF

      cur_t = timer_init(t%year, t%month, t%day, 0, 0, 0)
      diff_ms = cur_ms - timer_t2ms(cur_t)
      
!     match hour, minute and second
      diff_t = timer_ms2t_hms(diff_ms)

      t%hour = diff_t%hour
      t%minute = diff_t%minute
      t%second = diff_t%second

      
      RETURN
      END FUNCTION timer_ms2t

!     convert the number of milliseconds to hours, minutes and seconds (used for time differences)
      FUNCTION timer_ms2t_hms(ms) RESULT(t)
      IMPLICIT NONE
      
      DOUBLEPRECISION, INTENT(IN) :: ms
      type(timer_t) :: t
      DOUBLEPRECISION :: cur_ms

      cur_ms = ms
      
      t%ms = int(modulo(cur_ms, 1d3))
      cur_ms = (cur_ms - t%ms) / 1d3

      t%second = int(modulo(cur_ms, 60d0))
      cur_ms = (cur_ms - t%second) / 60d0

      t%minute = int(modulo(cur_ms, 60d0))
      cur_ms = (cur_ms - t%minute) / 60d0

      t%hour = int(cur_ms)      
      
      RETURN
      END FUNCTION timer_ms2t_hms

!     convert the number of milliseconds to hours, minutes and seconds (used for time intervals)      
      FUNCTION timer_t_hms2ms(t) RESULT(ms)
      IMPLICIT NONE

      type(timer_t), INTENT(IN) ::t
      DOUBLEPRECISION :: ms

      ms = t%second * 1d3 + t%minute * 60d0 * 1d3 + t%hour * 3600d0 * 1d3

      RETURN
      END FUNCTION timer_t_hms2ms
      
      
!     Returns the number of days in a given month of a given year
      FUNCTION timer_days(year, month) RESULT(res)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: year, month
      INTEGER :: res

!     Handle February separately
      IF (month.eq.2) THEN
         IF (timer_is_leap(year)) THEN
            res = 29
         ELSE
            res = 28
         ENDIF
      ELSE
         IF (ANY((/1, 3, 5, 7, 8, 10, 12/).eq.month)) THEN
            res = 31
         ELSE
            res = 30
         ENDIF
      ENDIF
      
      RETURN
      END FUNCTION timer_days
      
!     Determines if the year is a leap year
      FUNCTION timer_is_leap(year) RESULT(ans)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: year
      LOGICAL :: ans

      IF (mod(year, 4).ne.0) THEN
         ans = .false.
      ELSE
         IF (mod(year, 100).ne.0) THEN
            ans = .true.
         ELSE
            IF (mod(year, 400).ne.0) THEN
               ans = .false.
            ELSE
               ans = .true.
            ENDIF
         ENDIF
      ENDIF
      
      RETURN
      END FUNCTION timer_is_leap

      
      END MODULE timers
