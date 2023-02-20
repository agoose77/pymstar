************************************************************************
**                                                                    **
**              SUBROUTINE MSRDB1(ID,N,NAME,E,STE,NACT,RC)            **
**                                                                    **
************************************************************************
**                                                                    **
**  Reads the database 'msdbs1.d' for target 'ID' and returns         **
**  the energies (E) and the He electronic stopping powers (STE),     **
**  as well as the target tag.                                        **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  ID     ( INTEGER )    Target ID (ASTAR).       **
**  -----         (O)  N      ( INTEGER )    Max. reserved array size **
**                (O)  NAME   ( CHAR*80 )    Target tag (ASTAR).      **
**                (O)  E      (DBLE PREC)    Energies in MeV.         **
**                                           -> Array(1..NACT)        **
**                (O)  STE    (DBLE PREC)    Electronic He stopping   **
**                                           powers in MeV cm2 /mg.   **
**                                           -> Array(1..NACT)        **
**                (O)  NACT   ( INTEGER )    Actual array lengths.    **
**                (O)  RC     ( INTEGER )    Returncode.              **
**                                                                    **
**                                                                    **
**  PARAMETER:         MXTAR  ( INTEGER )    Max. no. of targets.     **
**  ----------         MXDAT  ( INTEGER )    Max. no. of data.        **
**                                                                    **
**                                                                    **
**  COMMONS:      NONE                                                **
**  --------                                                          **
**                                                                    **
**  CALLS:        NONE                                                **
**  ------                                                            **
**                                                                    **
************************************************************************

      SUBROUTINE MSRDB1(ID,N,NAME,E,STE,NACT,RC)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER ID,N,NACT,RC
        CHARACTER*80 NAME
        DOUBLE PRECISION E(N),STE(N)


        INTEGER MXTAR,MXDAT,IDS,UN,TAR,IPT,I,K,DT,QT
        LOGICAL INIT,OP,BL
        CHARACTER HT,CHAR
        CHARACTER*80 NAMES
        CHARACTER*120 LINE
        CHARACTER*255 DBPATH


        PARAMETER     (    MXTAR    =   100    ,
     *                     MXDAT    =   500    )


        DIMENSION STEDAT(MXDAT,0:MXTAR),IDS(MXTAR),NAMES(MXTAR)

        SAVE STEDAT,IDS,NAMES,INIT,TAR,IPT

        DATA INIT /.TRUE./


*...... Initialization (first call) ?

        IF(INIT) THEN

          CALL get_environment_variable("__MSTAR_DATABASE_PATH", DBPATH)

*........ Search for unused unit number

          DO 10 UN=10,99
            INQUIRE(UNIT=UN,OPENED=OP,ERR=10)
            IF(.NOT.OP) GOTO 20
10        CONTINUE
          GOTO 9010
20        CONTINUE

*........ Open database file

          OPEN(UN,FILE=DBPATH,STATUS='OLD',ERR=9011)

*........ Main input loop

          HT =CHAR(9)
          TAR=0
          IPT=0

30        CONTINUE

*.......... Read line

            READ(UN,1000,ERR=9012,END=50) LINE
1000        FORMAT(A120)

*.......... Replace white space , handle comments & count . and '

            BL=.TRUE.
            DT= 0
            QT= 0

            DO 40 I=1,120

              IF(LINE(I:I).EQ.HT        ) LINE(I:I)=' '
              IF(LINE(I:I).EQ.'#'.AND.BL) GOTO 30
              IF(LINE(I:I).NE.' '       ) BL=.FALSE.
              IF(LINE(I:I).EQ.'.'       ) DT=DT+1
              IF(LINE(I:I).EQ.''''      ) QT=QT+1

40          CONTINUE

            IF(BL) GOTO 30

*.......... Do actual reading

            BACKSPACE(UN)

            IF(QT.EQ.2) THEN

*............ New target, read ID & tag

              IF(TAR.EQ.0) NACT=IPT
              IF(IPT.NE.NACT.OR.IPT.LE.0) GOTO 9015

              IPT=0
              TAR=TAR+1
              IF(TAR.GT.MXTAR) GOTO 9013

              READ(UN,*,ERR=9012,END=9012) IDS(TAR),NAMES(TAR)

              IF(IDS(TAR).LE.0) GOTO 9015

            ELSEIF(QT.EQ.0.AND.DT.EQ.1) THEN

*............ Data entry

              IPT=IPT+1
              IF(IPT.GT.MXDAT) GOTO 9014

              READ(UN,*,ERR=9012,END=9012) STEDAT(IPT,TAR)

              IF(STEDAT(IPT,TAR).LE.0.0D0) GOTO 9015

            ELSE

*............ Error

              GOTO 9015

            ENDIF

          GOTO 30

50        IF(IPT.NE.NACT.OR.TAR.LE.0.OR.IPT.LT.4) GOTO 9015

*........ Close database file

          CLOSE(UN,ERR=9016)

*........ Check E(1) < E(2) < ... < E(n)

          DO 60 I=2,IPT
            IF(STEDAT(I,0).LE.STEDAT(I-1,0)) GOTO 9015
60        CONTINUE

*........ Reset INIT flag

          INIT=.FALSE.

        ENDIF

*...... ------------------------------------------
*       For non-init calls this is the entry point
*       ------------------------------------------

        IF(IPT.LE.0.OR.IPT.GT.MXDAT.OR.TAR.LE.0.OR.TAR.GT.MXTAR)
     *    GOTO 9017

*...... Search ID

        DO 70 K=1,TAR
          IF(IDS(K).EQ.ID) GOTO 80
70      CONTINUE
        GOTO 9018
80      CONTINUE

*...... Check output array size

        IF(N.LT.IPT) GOTO 9019

*...... Set output parameters

        DO 90 I=1,IPT
          E(I)  =STEDAT(I,0)
          STE(I)=STEDAT(I,K)
90      CONTINUE

        NAME=NAMES(K)
        NACT=IPT

*...... Exits

        RC=0
        RETURN

*-----> Unit assignment error
9010    RC=10
        RETURN
*-----> Open error
9011    RC=11
        RETURN
*-----> Read error
9012    RC=12
        RETURN
*-----> Too many targets
9013    RC=13
        RETURN
*-----> Too many datapoints
9014    RC=14
        RETURN
*-----> File has been corrupted
9015    RC=15
        RETURN
*-----> Close error
9016    RC=16
        RETURN
*-----> Internal error
9017    RC=17
        RETURN
*-----> ID not found
9018    RC=18
        RETURN
*-----> N too small
9019    RC=19
        RETURN

*...... End of subroutine MSRDB1

      END

