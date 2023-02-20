************************************************************************
**                                                                    **
**               SUBROUTINE MSSEAL(ID,E,STE,NAME,RC)                  **
**                                                                    **
************************************************************************
**                                                                    **
**  Returns the electronic stopping power 'STE' of alpha projectiles  **
**  (energy 'E') in target 'ID', using the M. J. Berger data.         **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  ID     ( INTEGER )    Target ID number.        **
**  -----         (I)  E      (DBLE PREC)    Energy in MeV.           **
**                (O)  STE    (DBLE PREC)    Electronic stopping      **
**                                           power in MeV cm2 /mg.    **
**                (O)  NAME   ( CHAR*80 )    Target tag (ASTAR).      **
**                (O)  RC     ( INTEGER )    Returncode.              **
**                                                                    **
**                                                                    **
**  PARAMETER:         MXDAT  ( INTEGER )    Max. no. of data.        **
**  ----------                                                        **
**                                                                    **
**  COMMONS:           NONE                                           **
**  --------                                                          **
**                                                                    **
**  CALLS:             MSRDB1     Read database.                      **
**  ------             MSSPL1     Spline interpolation part 1.        **
**                     MSSPL2     Spline interpolation part 2.        **
**                                                                    **
************************************************************************

      SUBROUTINE MSSEAL(ID,E,STE,NAME,RC)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER ID,RC
        CHARACTER*80 NAME

        INTEGER MXDAT,NACT,LASTID
        LOGICAL INIT
        CHARACTER*80 LASTNM


        PARAMETER    (   MXDAT   =   500   )


        DIMENSION ENI(MXDAT),SEI(MXDAT),YPP(MXDAT),WS(MXDAT)
        SAVE ENI,SEI,YPP,LASTID,LASTNM,INIT,NACT

        DATA INIT /.TRUE./ , LASTID / -1 /


*...... Is it initial call or new ID ?

        IF(INIT.OR.ID.NE.LASTID) THEN

*........ Access database to get NAME, ENI and SEI

          CALL MSRDB1(ID,MXDAT,LASTNM,ENI,SEI,NACT,RC)

*........ If an error occurred --> push it through & set init flag

          IF(RC.NE.0) THEN
            INIT=.TRUE.
            RETURN
          ENDIF

*........ Call interpolator part 1

          CALL MSSPL1(NACT,ENI,SEI,YPP,WS)

*........ Save ID and reset INIT flag

          LASTID=ID
          INIT  =.FALSE.

        ENDIF

*...... ---------------------------------------------------------
*       Here is the entry for a call with already preprocessed ID
*       ---------------------------------------------------------

*...... Check if E inside interpolation range

        IF(E.LT.ENI(1).OR.E.GT.ENI(NACT)) GOTO 9030

*...... Call spline interpolator

        CALL MSSPL2(NACT,ENI,SEI,YPP,E,STE)

*...... Set NAME

        NAME=LASTNM

*...... Exits

        RC=0
        RETURN

*-----> E outside energy data
9030    RC=30
        RETURN

*...... End of subroutine MSSEAL

      END

