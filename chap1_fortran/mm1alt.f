*     Main program for single-server queueing system, fixed run length.
 
*     Bring in declarations file.
 
      INCLUDE 'mm1alt.dcl'
 
*     Open input and output files.
 
      OPEN (5, FILE = 'mm1alt.in')
      OPEN (6, FILE = 'mm1alt.out')
 
*     Specify the number of event types for the timing routine.
 
      NEVNTS = 3
 
*     Set mnemonics for server's being busy and idle.

      BUSY = 1
      IDLE = 0

*     Read input parameters.
 
      READ (5,*) MARRVT, MSERVT, TEND
 
*     Write report heading and input parameters.
 
      WRITE (6,2010) MARRVT, MSERVT, TEND
 2010 FORMAT (' Single-server queueing system with fixed run length'//
     &        ' Mean interarrival time',F11.3,' minutes'//
     &        ' Mean service time',F16.3,' minutes'//
     &        ' Length of the simulation',F9.3,' minutes'//)
 
*     Initialize the simulation.
 
      CALL INIT
 
*     Determine the next event.
 
   10 CALL TIMING

*     Update time-average statistical accumulators.
 
      CALL UPTAVG
 
*     Call the appropriate event routine.
 
      GO TO (20, 30, 40), NEXT
   20    CALL ARRIVE
         GO TO 10
   30    CALL DEPART
         GO TO 10
 
*        Simulation is over; call report generator and end
*        simulation.
 
   40    CALL REPORT
 
      CLOSE (5)
      CLOSE (6)

      STOP
      END


      SUBROUTINE INIT
      INCLUDE 'mm1alt.dcl'
 
*     Initialize the simulation clock.
 
      TIME   = 0.0
 
*     Initialize the state variables.
 
      SERVER = IDLE
      NIQ    = 0
      TLEVNT = 0.0
 
*     Initialize the statistical counters.
 
      NUMCUS = 0
      TOTDEL = 0.0
      ANIQ   = 0.0
      AUTIL  = 0.0
 
*     Initialize event list. Since no customers are present, the
*     departure (service completion) event is eliminated from
*     consideration.  The end-simulation event (type 3) is scheduled for
*     time TEND.
 
      TNE(1) = TIME + EXPON(MARRVT)
      TNE(2) = 1.0E+30
      TNE(3) = TEND

      RETURN
      END


      SUBROUTINE TIMING
      INCLUDE 'mm1alt.dcl'
      INTEGER I
      REAL MINTNE
 
      MINTNE = 1.0E+29
      NEXT   = 0
 
*     Determine the event type of the next event to occur.
 
      DO 10 I = 1, NEVNTS
         IF (TNE(I) .LT. MINTNE) THEN
            MINTNE = TNE(I)
            NEXT   = I
         END IF
   10 CONTINUE
 
*     Check to see whether the event list is empty.
 
      IF (NEXT .EQ. 0) THEN

*        The event list is empty, so stop the simulation.

         WRITE (6,2010) TIME
 2010    FORMAT (' Event list empty at time',F10.3)
         STOP

      END IF

*     The event list is not empty, so advance the simulation clock.

      TIME = MINTNE

      RETURN
      END


      SUBROUTINE ARRIVE
      INCLUDE 'mm1alt.dcl'
      REAL DELAY
 
*     Schedule next arrival.
 
      TNE(1) = TIME + EXPON(MARRVT)
 
*     Check to see whether server is busy.
 
      IF (SERVER .EQ. BUSY) THEN

*        Server is busy, so increment number of customers in queue.

         NIQ = NIQ + 1

*        Check to see whether an overflow condition exists.

         IF (NIQ .GT. QLIMIT) THEN

*           The queue has overflowed, so stop the simulation.

            WRITE (6,2010) TIME
 2010       FORMAT (' Overflow of the array TARRVL at time',F10.3)
            STOP

         END IF

*        There is still room in the queue, so store the time of arrival
*        of the arriving customer at the (new) end of TARRVL.

         TARRVL(NIQ) = TIME

      ELSE

*        Server is idle, so arriving customer has a delay of zero.  (The
*        following two statments are for program clarity and do not
*        affect the results of the simulation.)

         DELAY  = 0.0
         TOTDEL = TOTDEL + DELAY
 
*        Increment the number of customers delayed, and make server
*        busy.
 
         NUMCUS = NUMCUS + 1
         SERVER = BUSY
 
*        Schedule a departure (service completion).
 
         TNE(2) = TIME + EXPON(MSERVT)

      END IF

      RETURN
      END


      SUBROUTINE DEPART
      INCLUDE 'mm1alt.dcl'
      INTEGER I
      REAL DELAY
 
*     Check to see whether the queue is empty.
 
      IF (NIQ .EQ. 0) THEN
 
*        The queue is empty so make the server idle and eliminate the
*        departure (service completion) event from consideration.

         SERVER = IDLE
         TNE(2) = 1.0E+30
 
      ELSE

*        The queue is nonempty, so decrement the number of customers in
*        queue.

         NIQ = NIQ - 1
 
*        Compute the delay of the customer who is beginning service and
*        update the total delay accumulator.
 
         DELAY  = TIME - TARRVL(1)
         TOTDEL = TOTDEL + DELAY
 
*        Increment the number of customers delayed, and schedule
*        departure.
 
         NUMCUS = NUMCUS + 1
         TNE(2) = TIME + EXPON(MSERVT)
 
*        Move each customer in queue (if any) up one place.
 
         DO 10 I = 1, NIQ
   10    TARRVL(I) = TARRVL(I + 1)

      END IF

      RETURN
      END


      SUBROUTINE REPORT
      INCLUDE 'mm1alt.dcl'
      REAL AVGDEL,AVGNIQ,UTIL
 
*     Compute and write estimates of desired measures of performance.
 
      AVGDEL = TOTDEL / NUMCUS
      AVGNIQ = ANIQ / TIME
      UTIL   = AUTIL / TIME
      WRITE (6,2010) AVGDEL, AVGNIQ, UTIL, NUMCUS
 2010 FORMAT (/' Average delay in queue',F11.3,' minutes'//
     &         ' Average number in queue',F10.3//
     &         ' Server utilization',F15.3//
     &         ' Number of delays completed',I7)

      RETURN
      END


      SUBROUTINE UPTAVG
      INCLUDE 'mm1alt.dcl'
      REAL TSLE
 
*     Compute time since last event, and update last-event-time marker.
 
      TSLE   = TIME - TLEVNT
      TLEVNT = TIME

*     Update area under number-in-queue function.
 
      ANIQ   = ANIQ + NIQ * TSLE
 
*     Update area under server-busy indicator function.
 
      AUTIL  = AUTIL + SERVER * TSLE

      RETURN
      END


      REAL FUNCTION EXPON(RMEAN)
      REAL RMEAN
      REAL RAND

*     Return an exponential random variate with mean RMEAN.
 
      EXPON = -RMEAN * LOG(RAND(1))

      RETURN
      END
