C read_inp_spe is a combination of code from IPPLNT,IPPHENOL

      subroutine for_inp_spe(FILECC,filecc_len,FNPGL,FNPGN,PGREF,LNREF,
     &                       QEREF,SLWREF,TYPPGL,TYPPGN,XLMAXT,YLMAXT,
     &                       CCNEFF,CICAD,CMXSF,CQESF,PGPATH)

      implicit none
      
      integer filecc_len
      character(len=filecc_len) :: FILECC
      CHARACTER*3   CTMP(20), DLTYP(20)
      INTEGER NPRIOR(20), TSELC(20)
      DOUBLE PRECISION WSENP(20), NSENP(20), PSENP(20)

      CHARACTER*6 SECTION
      CHARACTER*6  ERRKEY
      parameter(ERRKEY='rdipsp')
      CHARACTER*80 PATHCR, CHAR, PATHEC, C80

      DOUBLE PRECISION EVMODC
      DOUBLE PRECISION TB(5), TO1(5), TO2(5), TM(5)

      
      integer II,I,J,ERR,NPHS,LNUM,ISECT,FOUND,LINC,ERRNUM
      integer :: LUNCRP=100
      
      DOUBLE PRECISION
     &  CADPR1, CMOBMX, FRCNOD, FREEZ1, FREEZ2,
     &  PCARSH, PCH2O, PLIPSH, PLIGSD,
     &  PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &  PROLFI, PRORTI, PROSHI, PROSTI, R30C2,
     &  RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,
     &  RNH4C, RNO3C, ROA, RPRO, TTFIX
     
      CHARACTER*1 MRSWITCH, TRSWITCH
      CHARACTER*3 TRSTYP
      DOUBLE PRECISION LFMRC, mft, RTMRC, SDMRC, SHELMRC, STMMRC,
     &   STRMRC, TRST(4)
      DOUBLE PRECISION CADPV

      CHARACTER*1  BLANK, UPCASE, DETACH

!     Species-dependant variables exported to SPAM or WATBAL:
      DOUBLE PRECISION EORATIO, KCAN, KEP, PORMIN, RWUMX, RWUEP1

      DOUBLE PRECISION PROSRI, STRSRFL, STRLYR1
     
      DOUBLE PRECISION CCEFF, CCMAX, CCMP, LMXSTD, LNREF, 
     &  PARMAX, PGREF, PHTMAX, XPGSLW(15)

      DOUBLE PRECISION YPGSLW(15)

      CHARACTER*3  TYPPGN, TYPPGL
      double precision 
     &  AZIR,BETN,FNPGL(4),LFANGB,LFANGD(3),LMXREF,NSLOPE,
     &  PALBW,PLTPOP,QEREF,ROWSPC,SALBW,SCVP,SLWREF,
     &  SLWSLO,FNPGN(4),XLMAXT(6),YLMAXT(6),PHTHRS10

      character(len=2) pgpath
      character(len=8) model
      double precision ccneff, cicad, cmxsf, cqesf

        OPEN (LUNCRP,FILE=FILECC(1:filecc_len),STATUS='OLD',IOSTAT=ERR)
        SECTION = '!*LEAF'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(24X,F6.1)') EVMODC
        SECTION = '!*PHEN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(1), TO1(1), TO2(1), TM(1)
250   FORMAT(13F6.1)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(2), TO1(2), TO2(2), TM(2)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(3), TO1(3), TO2(3), TM(3)
        DO I = 1,NPHS
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,270,IOSTAT=ERR) J, NPRIOR(J), DLTYP(J), CTMP(J),
     &        TSELC(J), WSENP(J), NSENP(J), PSENP(J)  !PSENP not used
270     FORMAT(I3,I3,2(2X,A3),1X,I2,3(1X,F5.2))
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO

C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
        rewind(luncrp)
        LINC = 1
        SECTION = '!*PHOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(12X,F6.2)',IOSTAT=ERR) KCAN

C-----------------------------------------------------------------------
C READ RESPIRATION PARAMETERS **********************
C-----------------------------------------------------------------------
        SECTION = '!*RESP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(G12.0,F6.1)',IOSTAT=ERR) RES30C, R30C2

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0)',IOSTAT=ERR) RNO3C, RNH4C, RPRO, RFIXN

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(6F6.0)',IOSTAT=ERR) 
     &    RCH2O, RLIP, RLIG, ROA, RMIN, PCH2O

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2(5X,A1))',IOSTAT=ERR) MRSWITCH, TRSWITCH

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(3G12.0)',IOSTAT=ERR)
     &    LFMRC, STMMRC, RTMRC 

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(3G12.0)',IOSTAT=ERR)
     &    STRMRC, SHELMRC, SDMRC

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0,3X,A3,F6.0)',IOSTAT=ERR)
     &    TRST(1), TRST(2), TRST(3), TRST(4), TRSTYP, mft

C-----------------------------------------------------------------------
C READ PLANT COMPOSITION PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PROLFI, PROSTI

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PROSHI

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PCARSH

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PLIPSH

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PLIGSH, PLIGSD

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) POASH, POASD

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PMINSH, PMINSD

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0)',IOSTAT=ERR) PROSRI

C-----------------------------------------------------------------------
C READ CARBON AND NITROGEN MINING PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,6X,F6.0)',IOSTAT=ERR) CMOBMX, CADPR1

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)


        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(30X,F6.0)',IOSTAT=ERR) CADPV



C-----------------------------------------------------------------------
C READ NITROGEN FIXATION PARAMATERS
C-----------------------------------------------------------------------
        SECTION = '!*NITR'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) TTFIX

C-----------------------------------------------------------------------
C  
C     ***** READ PARTITIONING PARAMETERS *****************
C
C-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(30X,F6.0)',IOSTAT=ERR) FRCNOD

C-----------------------------------------------------------------------
C
C     ***** READ SENESCENCE PARAMETERS ******************
C       This is found in the second heading that begins with '!*LEAF'
C-----------------------------------------------------------------------
        SECTION = '!*LEAF'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)

        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) FREEZ1, FREEZ2

!-----------------------------------------------------------------------
C         Read ROOT parameters
!-----------------------------------------------------------------------
        SECTION = '!*ROOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(30X,2F6.0)',IOSTAT=ERR) RWUEP1, RWUMX

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) PORMIN

C-----------------------------------------------------------------------
C
C     ***** READ POD DETACHMENT PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*POD '
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(5X,A1)',IOSTAT=ERR) DETACH
        DETACH = UPCASE(DETACH)

C-----------------------------------------------------------------------
C
C     ***** READ EVAPOTRANSPIRATION PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*EVAP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) KEP, EORATIO

C-----------------------------------------------------------------------
C
C     ***** READ STORAGE ORGAN PARTITIONING PARAMETERS *****
C
C-----------------------------------------------------------------------


        SECTION = '!*STOR'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) STRSRFL, STRLYR1

C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
      rewind(luncrp)
5     CONTINUE
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      IF (ISECT .EQ. 2) GO TO 5
      READ(CHAR,'(5F6.2)',IOSTAT=ERR) PARMAX, PHTMAX, KCAN

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(3F6.1)',IOSTAT=ERR) CCMP, CCMAX, CCEFF

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPGN(II),II=1,4), TYPPGN

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPGL(II),II=1,4), TYPPGL

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) LNREF, PGREF

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(10F6.0)',IOSTAT=ERR) (XPGSLW(II),II = 1,10)

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
      READ(CHAR,'(10F6.0)',IOSTAT=ERR) (YPGSLW(II),II = 1,10)

      rewind(LUNCRP)
      SECTION = '!*PHOT'
      CALL FIND(LUNCRP,SECTION,LNUM,FOUND)

!     Read 3rd line of photosynthesis section of species file
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      READ(C80,'(4F6.0,3X,A)',IOSTAT=ERRNUM) (FNPGN(I),I=1,4), TYPPGN

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !5th line
      READ(C80,'(6F6.0)',IOSTAT=ERRNUM) (XLMAXT(I),I=1,6)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !6th line
      READ(C80,'(6F6.0)',IOSTAT=ERRNUM) (YLMAXT(I),I=1,6)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !7th line
      READ(C80,'(4F6.0,3X,A)',IOSTAT=ERRNUM) (FNPGL(I),I=1,4),TYPPGL

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !8th line
      READ(C80,'(2F6.0,6X,F6.0)',IOSTAT=ERRNUM) QEREF,SCVP,LFANGB

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !9th line
      READ(C80,'(4F6.0)',IOSTAT=ERRNUM) SLWREF,SLWSLO,NSLOPE,LNREF

         CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
         CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
         CALL IGNORE(LUNCRP,LNUM,ISECT,C80) !12th line
         READ(C80,'(4F6.0,2X,A)',IOSTAT=ERRNUM) CICAD,CCNEFF,
     &        CMXSF,CQESF,PGPATH 

      CLOSE (LUNCRP)
      
      end subroutine
      
C=======================================================================
C  FIND, Subroutine, J.W.Jones, 01/03/91
C  Finds appropriate SECTION in a file of logical unit number LUNUM by
C  searching for a 6-character NAME at beginning of each line.
C-----------------------------------------------------------------------
C  INPUT : LUNUM  - logical unit number of the file to read
C          NAME  - 6-character variable name of section to find
C  OUTPUT: LNUM  - Line number of the file currently being read
C          FOUND - Indicator of completion of find routine
C                    0 - End-of-file encountered i.e. name not found
C                    1 - NAME was found
C  LOCAL :
C  IFILE : LUNUM
C  NOTES : Modified N.B. Pickering, 08/27/91
C=======================================================================

      SUBROUTINE FIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      INTEGER FOUND,I,LNUM,LUNUM
      CHARACTER SECTION*6,NAME*6,UPCASE*1
C
C     Initialization.
C
      FOUND = 0
      LNUM  = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO
C
C     Loop to read through data file.
C
   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = UPCASE(SECTION(I:I))
         END DO
C
C        String found, set FOUND to 1, and exit loop.
C
         IF (NAME .EQ. SECTION) then
            FOUND = 1
            GOTO 20
C
C           String not found, set FOUND to 0.
C
          ELSE
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END

C=======================================================================
C  IGNORE, Subroutine, J.W.Jones, 01/03/91
C----------------------------------------------------------------------------
C  PURPOSE: To read lines as an n-character variable and check it
C           for a blank line or for a comment line denoted by ! in col 1.
C  INPUTS:  LUN - Logical unit number of the file to be read
C           LINEXP - Starting line number at which this routine begins to
C                    read the file
C  OUTPUTS: LINEXP - Line number last read by the routine
C           ISECT - Indicator of completion of IGNORE routine
C                   0 - End of file encountered
C                   1 - Found a good line to read
C                   2 - End of Section in file encountered, denoted by *
C                       in column 1
C           CHARTEST - n-character variable containing the contents of
C                      the last line read by the IGNORE routine
C----------------------------------------------------------------------------
C
      SUBROUTINE IGNORE(LUN,LINEXP,ISECT,CHARTEST)

      CHARACTER BLANK*(80),CHARTEST*(*)
      INTEGER   LENGTH, LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/

      LENGTH = LEN(CHARTEST)

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70, END=70)CHARTEST
      LINEXP = LINEXP + 1

!     CHP 5/1/08
      IF (CHARTEST(1:1) == CHAR(26)) THEN
        GO TO 70
      ENDIF

C     Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*'  .OR. CHARTEST(1:1) .EQ. '$') THEN
C        End of section encountered
         ISECT = 2
         RETURN
      ENDIF
C
C     Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
!         IF(CHARTEST(1:80).NE.BLANK)THEN
         IF(CHARTEST(1:LENGTH).NE.BLANK)THEN
C           FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF

      GO TO 30
C     To read the next line

 70   ISECT = 0
      RETURN
      END SUBROUTINE IGNORE

C=======================================================================
C  UPCASE, Function
C
C  Function to return the upper case of a lower case letter.  Otherwise
C  returns the same character
C-----------------------------------------------------------------------
C  Revision history
C
C  05/15/1992 BDB Written
C  05/28/1993 PWW Header revision and minor changes   
C-----------------------------------------------------------------------
C  INPUT  : INCHAR
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called : INPUT READS IPEXP JULIAN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  INCHAR :
C  CHAVAL :
C=======================================================================

      CHARACTER*1 FUNCTION UPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER INCHAR*1
      INTEGER   CHAVAL

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL .LE. 122) .AND. (CHAVAL .GE. 97)) THEN
         UPCASE = CHAR(CHAVAL-32)
       ELSE
         UPCASE = INCHAR
      ENDIF

      END FUNCTION UPCASE
