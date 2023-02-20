************************************************************************
**                                                                    **
**                      SUBROUTINE MSEMSG(RC,TEXT)                    **
**                                                                    **
************************************************************************
**                                                                    **
**  Returns in 'TEXT' the error message, corresponding to returncode  **
**  'RC'.                                                             **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  RC     ( INTEGER )    Returncode.              **
**  -----         (O)  TEXT   ( CHAR*80 )    Error message text.      **
**                                                                    **
**                                                                    **
**  PARAMETER:         NMSG   ( INTEGER )    Number of messages.      **
**  ----------                                                        **
**                                                                    **
**  COMMONS:      NONE                                                **
**  --------                                                          **
**                                                                    **
**  CALLS:        NONE                                                **
**  ------                                                            **
**                                                                    **
************************************************************************

      SUBROUTINE MSEMSG(RC,TEXT)

        INTEGER RC
        CHARACTER*80 TEXT

        INTEGER NMSG,R,I
        CHARACTER*45 T


        PARAMETER     (    NMSG    =    18  )


        DIMENSION R(NMSG),T(NMSG)
        SAVE R,T

*...... Returncodes and message texts ----------------------------------

        DATA R(  1) /     0 /
     *       T(  1) /'NO ERROR                                     '/

        DATA R(  2) /    10 /
     *       T(  2) /'MSRDB1 : NO FREE UNIT FOR DATABASE           '/

        DATA R(  3) /    11 /
     *       T(  3) /'MSRDB1 : DATABASE FILE OPEN ERROR            '/

        DATA R(  4) /    12 /
     *       T(  4) /'MSRDB1 : DATABASE FILE READ ERROR            '/

        DATA R(  5) /    13 /
     *       T(  5) /'MSRDB1 : TOO MANY TARGETS IN DATABASE        '/

        DATA R(  6) /    14 /
     *       T(  6) /'MSRDB1 : TOO MANY DATA POINTS IN DATABASE    '/

        DATA R(  7) /    15 /
     *       T(  7) /'MSRDB1 : DATABASE FILE HAS BEEN CORRUPTED    '/

        DATA R(  8) /    16 /
     *       T(  8) /'MSRDB1 : DATABASE FILE CLOSE ERROR           '/

        DATA R(  9) /    17 /
     *       T(  9) /'MSRDB1 : INTERNAL ERROR                      '/

        DATA R( 10) /    18 /
     *       T( 10) /'MSRDB1 : TARGET ID NOT IN DATABASE           '/

        DATA R( 11) /    19 /
     *       T( 11) /'MSRDB1 : N TOO SMALL                         '/

        DATA R( 12) /    30 /
     *       T( 12) /'MSSEAL : ENERGY OUTSIDE SUPPORTED RANGE      '/

        DATA R( 13) /     1 /
     *       T( 13) /'MSTAR1 : ILLEGAL CG PARAMETER                '/

        DATA R( 14) /     2 /
     *       T( 14) /'MSTAR1 : UNSUPPORTED PROJECTILE Z1 VALUE     '/

        DATA R( 15) /     3 /
     *       T( 15) /'MSTAR1 : H AND He TARGETS ARE NOT SUPPORTED  '/

        DATA R( 16) /     4 /
     *       T( 16) /'MSTAR1 : SPECIAL MODE D OR H NOT SUPPORTED   '/

        DATA R( 17) /     5 /
     *       T( 17) /'MSTAR1 : INTERNAL ERROR                      '/

        DATA R( 18) /    40 /
     *       T( 18) /'MSID2Z : TARGET ID NOT IN ID TABLE           '/

*...... ----------------------------------------------------------------


*...... Search for message ID

        DO 10 I=1,NMSG

          IF(RC.EQ.R(I)) THEN
            TEXT=T(I) // '                                   '
            RETURN
          ENDIF

10      CONTINUE

*...... Return default message

        TEXT='UNKNOWN ERROR                                ' //
     *       '                                   '

        RETURN

*...... End of subroutine MSEMSG

      END

