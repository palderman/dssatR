C=======================================================================
C  The following code was adapted from:
C
C  LMATCH, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/1991 JWJ Written 
C  05/28/1993 PWW Header revision and minor changes  
C  08/27/2004 CHP A value of -99 for missing data is preserved in all 
C                 layers containing any portion of missing layer data.
C
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,VI,DLAYR
C
C  LOCAL  : J,K,L,VS,ZIL,ZOL,SUMZ,SUMV,ZT,ZB
C
C  OUTPUT : DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL IPSLIN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE LMATCH (NLAYRI,DSI,VI,NLAYRO,DSO,VS)

!     USE ModuleDefs
      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

      double precision  VI(NLAYRI),DSO(NLAYRO),VS(NLAYRO),DSI(NLAYRI)
      double precision  ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      LOGICAL MISSING

!      NLAYRO = 0
      IF (NLAYRI .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------
C     This subroutine assumes that DSI(L) values are depths to the
C     bottom of layer L
      VS = -99.
      K   = 1
      ZIL = 0.0
      ZOL = 0.0

      LOOP1: DO L = 1, NLAYRO
        SUMZ = 0.0
        SUMV = 0.0

        MISSING = .FALSE.

        LOOP2: DO WHILE (.TRUE.)
          ZT   = MAX (ZOL,ZIL)
          ZB   = MIN (DSO(L),DSI(K))
          SUMZ = SUMZ + (ZB - ZT)
          SUMV = SUMV + VI(K)*(ZB - ZT)
          IF (ABS(VI(K) + 99.) < 0.001 .AND. ZB > ZT) THEN
            !missing data if VI=-99
            MISSING = .TRUE.
          ENDIF

          IF (DSO(L) .LT. DSI(K)) EXIT LOOP2
C
C         Either establish last layer or go to next input layer
C
          IF (K .EQ. NLAYRI) GOTO 20
C
C         Go to next input layer to add characteristics
C
          ZIL = DSI(K)
          K   = K + 1
        END DO LOOP2

        VS(L) = VI(K)
        IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
        ENDIF
        IF (MISSING) THEN
          VS(L) = -99.0
        ENDIF
        ZOL = DSO(L)
      END DO LOOP1

C
C     Set last layer characteristics, and depth of profile
C
   20 VS(L) = VI(K)
      IF (ABS(SUMV + 99.0) > 0.001) THEN
        IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
        ENDIF
      ELSE
        VS(L) = -99.0
      ENDIF

      RETURN
      END SUBROUTINE LMATCH
