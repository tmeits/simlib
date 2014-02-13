      REAL FUNCTION RAND(ISTRM)

*     Prime modulus multiplicative linear congruential generator
*     Z(I) = (630360016 * Z(I - 1)) (MOD(2**31 - 1)), based on Marse and
*     Roberts' portable random-number generator UNIRAN.  Multiple (100) streams
*     are supported, with seeds spaced 100,000 apart.  Throughout, input
*     argument ISTRM must be an INTEGER giving the desired stream number.

*     Usage: (Three options)
*
*     1. To obtain the next U(0,1) random number from stream ISTRM, execute
*           U = RAND(ISTRM)
*        The REAL variable U will contain the next random number.
*
*     2. To set the seed for stream ISTRM to a desired value IZSET, execute
*           CALL RANDST(IZSET,ISTRM)
*        where IZSET must be an INTEGER constant or variable set to the desired
*        seed, a number between 1 and 2147483646 (inclusive).  Default seeds for
*        all 100 streams are given in the code.
*
*     3. To get the current (most recently used) integer in the sequence being
*        generated for stream ISTRM into the INTEGER variable IZGET, execute
*           IZGET = IRANDG(ISTRM)

      INTEGER B2E15,B2E16,HI15,HI31,ISTRM,IZSET,LOW15,LOWPRD,MODLUS,
     &        MULT1,MULT2,OVFLOW,ZI,ZRNG(100)
      INTEGER IRANDG,RANDST

*     Force saving of ZRNG between calls.

      SAVE ZRNG

*     Define the constants.

      DATA MULT1,MULT2/24112,26143/
      DATA B2E15,B2E16,MODLUS/32768,65536,2147483647/

*     Set the default seeds for all 100 streams.

      DATA ZRNG/1973272912, 281629770,  20006270,1280689831,2096730329,
     &          1933576050, 913566091, 246780520,1363774876, 604901985,
     &          1511192140,1259851944, 824064364, 150493284, 242708531,
     &            75253171,1964472944,1202299975, 233217322,1911216000,
     &           726370533, 403498145, 993232223,1103205531, 762430696,
     &          1922803170,1385516923,  76271663, 413682397, 726466604,
     &           336157058,1432650381,1120463904, 595778810, 877722890,
     &          1046574445,  68911991,2088367019, 748545416, 622401386,
     &          2122378830, 640690903,1774806513,2132545692,2079249579,
     &            78130110, 852776735,1187867272,1351423507,1645973084,
     &          1997049139, 922510944,2045512870, 898585771, 243649545,
     &          1004818771, 773686062, 403188473, 372279877,1901633463,
     &           498067494,2087759558, 493157915, 597104727,1530940798,
     &          1814496276, 536444882,1663153658, 855503735,  67784357,
     &          1432404475, 619691088, 119025595, 880802310, 176192644,
     &          1116780070, 277854671,1366580350,1142483975,2026948561,
     &          1053920743, 786262391,1792203830,1494667770,1923011392,
     &          1433700034,1244184613,1147297105, 539712780,1545929719,
     &           190641742,1645390429, 264907697, 620389253,1502074852,
     &           927711160, 364849192,2049576050, 638580085, 547070247/

*     Generate the next random number.

      ZI     = ZRNG(ISTRM)
      HI15   = ZI / B2E16
      LOWPRD = (ZI - HI15 * B2E16) * MULT1
      LOW15  = LOWPRD / B2E16
      HI31   = HI15 * MULT1 + LOW15
      OVFLOW = HI31 / B2E15
      ZI     = (((LOWPRD - LOW15 * B2E16) - MODLUS) +
     &          (HI31 - OVFLOW * B2E15) * B2E16) + OVFLOW
      IF (ZI .LT. 0) ZI = ZI + MODLUS
      HI15   = ZI / B2E16
      LOWPRD = (ZI - HI15 * B2E16) * MULT2
      LOW15  = LOWPRD / B2E16
      HI31   = HI15 * MULT2 + LOW15
      OVFLOW = HI31 / B2E15
      ZI     = (((LOWPRD - LOW15 * B2E16) - MODLUS) +
     &          (HI31 - OVFLOW * B2E15) * B2E16) + OVFLOW
      IF (ZI .LT. 0) ZI = ZI + MODLUS
      ZRNG(ISTRM) = ZI
      RAND   = (2 * (ZI / 256) + 1) / 16777216.0
      RETURN


*     Set the current ZRNG for stream ISTRM to IZSET.

      ENTRY RANDST(IZSET,ISTRM)
      ZRNG(ISTRM) = IZSET
      RETURN


*     Return the current ZRNG for stream ISTRM.

      ENTRY IRANDG(ISTRM)
      IRANDG = ZRNG(ISTRM)
      RETURN

      END

