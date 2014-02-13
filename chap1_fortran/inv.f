*     Main program for inventory system.
 
*     Bring in declarations file and declare local variables.
 
      INCLUDE 'inv.dcl'
      INTEGER I,NPOLCY
 
*     Open input and output files.
 
      OPEN (5, FILE = 'inv.in')
      OPEN (6, FILE = 'inv.out')
 
*     Specify the number of event types for the timing routine.
 
      NEVNTS = 4
 
*     Read input parameters.
 
      READ (5,*) INITIL, NMNTHS, NPOLCY, NVALUE, MDEMDT, SETUPC, INCRMC,
     &           H, PI, MINLAG, MAXLAG
      READ (5,*) (PROBD(I), I = 1, NVALUE)
 
*     Write report heading and input parameters.
 
      WRITE (6,2010) INITIL, NVALUE, (PROBD(I), I = 1, NVALUE)
 2010 FORMAT (' Single-product inventory system'//
     &        ' Initial inventory level',I24,' items'//
     &        ' Number of demand sizes',I25//
     &        ' Distribution function of demand sizes',2X,8F8.3)
      WRITE (6,2020) MDEMDT, MINLAG, MAXLAG, NMNTHS, SETUPC, INCRMC, H,
     &               PI, NPOLCY
 2020 FORMAT (/' Mean interdemand time',F26.2,' months'//
     &         ' Delivery lag range',F29.2,' to',F10.2,' months'//
     &         ' Length of the simulation',I23,' months'//
     &         ' K =',F6.1,'   i =',F6.1,'   h =',F6.1,'   pi =',F6.1//
     &         ' Number of policies',I29//
     &         10X,4(8X,'Average')/
     &         '   Policy       total cost    ordering cost',
     &         '  holding cost   shortage cost')
 
*     Run the simulation varying the inventory policy.
 
      DO 60 I = 1, NPOLCY
 
*        Read the inventory policy, and initialize the simulation.
 
         READ (5,*) SMALLS, BIGS
         CALL INIT
 
*        Determine the next event.
 
   10    CALL TIMING
 
*        Update time-average statistical accumulators.
 
         CALL UPTAVG
 
*        Call the appropriate event routine.
 
         GO TO (20, 30, 50, 40), NEXT
   20       CALL ORDARV
            GO TO 10
   30       CALL DEMAND
            GO TO 10
   40       CALL EVALU8
            GO TO 10
   50       CALL REPORT

   60 CONTINUE
 
      CLOSE (5)
      CLOSE (6)

      STOP
      END


      SUBROUTINE INIT
      INCLUDE 'inv.dcl'
 
*     Initialize the simulation clock.
 
      TIME   = 0.0
 
*     Initialize the state variables.
 
      INVLEV = INITIL
      TLEVNT = 0.0
 
*     Initialize the statistical counters.
 
      TORDC  = 0.0
      APLUS  = 0.0
      AMINUS = 0.0
 
*     Initialize the event list. Since no order is outstanding, the
*     order-arrival event is eliminated from consideration.
 
      TNE(1) = 1.0E+30
      TNE(2) = TIME + EXPON(MDEMDT)
      TNE(3) = NMNTHS
      TNE(4) = 0.0

      RETURN
      END


      SUBROUTINE TIMING
      INCLUDE 'inv.dcl'
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


      SUBROUTINE ORDARV
      INCLUDE 'inv.dcl'
 
*     Increment the inventory level by the amount ordered.
 
      INVLEV = INVLEV + AMOUNT
 
*     Since no order is now outstanding, eliminate the order-arrival
*     event from consideration.
 
      TNE(1) = 1.0E+30

      RETURN
      END


      SUBROUTINE DEMAND
      INCLUDE 'inv.dcl'

*     Decrement the inventory level by a generated demand size.
 
      INVLEV = INVLEV - IRANDI(NVALUE,PROBD)
 
*     Schedule the time of the next demand.
 
      TNE(2) = TIME + EXPON(MDEMDT)

      RETURN
      END


      SUBROUTINE EVALU8
      INCLUDE 'inv.dcl'
 
*     Check whether the inventory level is less than SMALLS.
 
      IF (INVLEV .LT. SMALLS) THEN

*        The inventory level is less than SMALLS, so place an order for
*        the appropriate amount.

         AMOUNT = BIGS - INVLEV
         TORDC  = TORDC + SETUPC + INCRMC * AMOUNT
 
*        Schedule the arrival of the order.
 
         TNE(1) = TIME + UNIFRM(MINLAG,MAXLAG)

      END IF
 
*     Regardless of the place-order decision, schedule the next
*     inventory evaluation.
 
      TNE(4) = TIME + 1.0

      RETURN
      END


      SUBROUTINE REPORT
      INCLUDE 'inv.dcl'
      REAL ACOST,AHLDC,AORDC,ASHRC
 
*     Compute and write estimates of desired measures of performance.
 
      AORDC = TORDC / NMNTHS
      AHLDC = H  * APLUS / NMNTHS
      ASHRC = PI * AMINUS / NMNTHS
      ACOST = AORDC + AHLDC + ASHRC
      WRITE (6,2010) SMALLS, BIGS, ACOST, AORDC, AHLDC, ASHRC
 2010 FORMAT (/' (',I3,',',I3,')',4F15.2)

      RETURN
      END


      SUBROUTINE UPTAVG
      INCLUDE 'inv.dcl'
      REAL TSLE
 
*     Compute time since last event, and update last-event-time marker.
 
      TSLE   = TIME - TLEVNT
      TLEVNT = TIME
 

*     Determine the status of the inventory level during the previous
*     interval.  If the inventory level during the previous interval was
*     negative, update AMINUS.  If it was zero, no update is needed.  If
*     it was positive, update APLUS.
 
      IF (INVLEV) 10, 20, 30
   10    AMINUS = AMINUS - INVLEV * TSLE
   20    RETURN
   30    APLUS  = APLUS + INVLEV * TSLE

      RETURN
      END


      REAL FUNCTION EXPON(RMEAN)
      REAL RMEAN
      REAL RAND

*     Return an exponential random variate with mean RMEAN.
 
      EXPON = -RMEAN * LOG(RAND(1))

      RETURN
      END


      INTEGER FUNCTION IRANDI(NVALUE,PROBD)
      INTEGER I,NVALUE
      REAL PROBD(1),U
      REAL RAND
 
*     Generate a U(0,1) random variate.
 
      U = RAND(1)
 
*     Return a random integer between 1 and NVALUE in accordance with
*     the (cumulative) distribution function PROBD.
 
      DO 10 I = 1, NVALUE - 1
         IF (U .LT. PROBD(I)) THEN
            IRANDI = I
            RETURN
         END IF
   10 CONTINUE
      IRANDI = NVALUE

      RETURN
      END


      REAL FUNCTION UNIFRM(A,B)
      REAL A,B
      REAL RAND

*     Return a U(A,B) random variate.
 
      UNIFRM = A + RAND(1) * (B - A)

      RETURN
      END
