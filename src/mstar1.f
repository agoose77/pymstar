************************************************************************
**                                                                    **
**             SUBROUTINE MSTAR1(ID,CG,Z1,E,NAME,STE,RC)              **
**                                                                    **
************************************************************************
**                                                                    **
**  This is the top level routine of the MSTAR subprogram package.    **
**                                                                    **
**  REVISION DATE: 17 Nov 2000 : Root version                         **
**                 10 Oct 2001 : Mode b added                         **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:    (I)  ID    ( INTEGER )       Target material ID.         **
**  -----  (I/O)  CG    (CHARACTER)       Input: 'a'|'A' -> automatic **
**                                                state selection +   **
**                                                force c|g usage     **
**                                                                    **
**                                               'b'|'B' -> automatic **
**                                                state selection +   **
**                                                d|h preferred       **
**                                                                    **
**                                               'c'|'C' -> condensed **
**                                               'g'|'G' -> gaseous   **
**                                               'd'|'D' -> special c **
**                                               'h'|'H' -> special g **
**                                                                    **
**                                        Output: Actual mode used.   **
**                                        (For Z1==2 it is set but    **
**                                        otherwise ignored).         **
**           (I)  Z1    ( INTEGER )       Projectile atomic number.   **
**           (I)  E     (DBLE PREC)       Projectile spec. energy     **
**                                          in  MeV/nucleon.          **
**           (O)  NAME  ( CHAR*80 )       Target material name.       **
**           (O)  STE   (DBLE PREC)       Electronic stopping power   **
**                                          in MeV cm2 /mg.           **
**           (O)  RC    ( INTEGER )       Returncode (0==no error).   **
**                                                                    **
**                                                                    **
**                                                                    **
**  PARAMETER:    NGAS  ( INTEGER )      Number of gaseous targets    **
**  ----------                           in 'GASIDS' - table.         **
**                                                                    **
**  COMMONS:      /MSDEBG/ A,B,C                                      **
**  --------                                                          **
**                                                                    **
**  CALLS:        MSPAUL      Transformation Alpha -> Z1.             **
**  ------        MSRDB1      Database access routine.                **
**                MSSEAL      Alpha stopping power (Berger data).     **
**                MSSPL1      Cubic spline interpolator part1.        **
**                MSSPL2      Cubic spline interpolator part2.        **
**                                                                    **
************************************************************************

      SUBROUTINE MSTAR1(ID,CG,Z1,E,NAME,STE,RC)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER ID,Z1,RC
        CHARACTER CG
        CHARACTER*80 NAME

        INTEGER I
        LOGICAL PREFDH

        COMMON /MSDEBG/ A,B,C

*...... Table of target IDS with gaseous state

        INTEGER    GASIDS,NGAS
        PARAMETER      (    NGAS   =   17   )
        DIMENSION  GASIDS(NGAS)
        SAVE       GASIDS
        DATA       GASIDS /   1 ,   2 ,   7 ,   8 ,  10 ,  18 ,  36 ,
     *                       54 , 101 , 104 , 134 , 155 , 197 , 238 ,
     *                      263 , 264 , 277                         /


*...... Initialize /MSDEBG/ A,B,C

        A=0.0D0
        B=0.0D0
        C=0.0D0

*...... Handle CG mode & check parameters

        RC    =0
        PREFDH=.FALSE.

        IF(CG.EQ.'A') CG='a'
        IF(CG.EQ.'B') CG='b'
        IF(CG.EQ.'C') CG='c'
        IF(CG.EQ.'D') CG='d'
        IF(CG.EQ.'G') CG='g'
        IF(CG.EQ.'H') CG='h'

        IF(CG.EQ.'a'.OR.CG.EQ.'b') THEN

*........ Is special mode preferred ?

          PREFDH=(CG.EQ.'b')

*........ Select correct state

          DO 10 I=1,NGAS

            IF(ID.EQ.GASIDS(I)) THEN
              CG='g'
              IF(PREFDH) CG='h'
              GOTO 20
            ENDIF

10        CONTINUE
          CG='c'
          IF(PREFDH) CG='d'
20        CONTINUE

        ENDIF

        IF(CG.NE.'c'.AND.CG.NE.'g'.AND.
     *     CG.NE.'d'.AND.CG.NE.'h'.AND.Z1.NE.2) RC=1
        IF(Z1.LT.2.OR.Z1.GT.18                ) RC=2

        IF(RC.NE.0) RETURN

*...... Get alpha result

        CALL MSSEAL(ID,4.0D0*E,STE,NAME,RC)

        IF(RC.NE.0) RETURN
        IF(Z1.EQ.2) RETURN

*...... H and He targets are forbidden. Removed 4 Jul 01 (HP)

C       IF(ID.EQ.1.OR.ID.EQ.2) THEN
C         RC=3
C         RETURN
C       ENDIF

*...... Evaluate Paul's formula

        CALL MSPAUL(CG,Z1,ID,E,FOUT,RC)

*...... Handle illegal CG mode

        IF(RC.NE.0) THEN

*........ Forced d/h on unsupported Z1/Z2 ?

          IF(RC.NE.4.OR..NOT.PREFDH) RETURN

*........ Retry with c/g mode

          IF(CG.EQ.'d') CG='c'
          IF(CG.EQ.'h') CG='g'

          CALL MSPAUL(CG,Z1,ID,E,FOUT,RC)

          IF(RC.NE.0) THEN
*.......... Severe internal error of program logic
            RC=5
            RETURN
          ENDIF

        ENDIF

*...... Return final result

        STE=STE*FOUT*DBLE(Z1)**2/4.0D0
        RETURN

*...... End of subroutine MSTAR1

      END

