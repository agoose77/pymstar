************************************************************************
**                                                                    **
**                    SUBROUTINE MSID2Z(ID,DBLEZ,RC)                  **
**                                                                    **
************************************************************************
**                                                                    **
**  Converts the target ID to an effective Z (DBLE PREC).             **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  ID     ( INTEGER )    Target ID number.        **
**  -----         (O)  DBLEZ  (DBLE PREC)    Effective atomic number. **
**                (O)  RC     ( INTEGER )    Returncode.              **
**                                           40 : Illegal ID          **
**                                                                    **
**  PARAMETER:         NIDS   ( INTEGER )    Table length.            **
**  ----------                                                        **
**                                                                    **
**  COMMONS:           NONE                                           **
**  --------                                                          **
**                                                                    **
**  CALLS:             NONE                                           **
**  ------                                                            **
**                                                                    **
************************************************************************

      SUBROUTINE MSID2Z(ID,DBLEZ,RC)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER ID,RC

        INTEGER NIDS,IDTAB,LASTID,I


        PARAMETER   (   NIDS   =   49  )


        DIMENSION IDTAB(NIDS),DZTAB(NIDS)

        SAVE IDTAB,DZTAB
        SAVE LASTID,LASTDZ

        DATA LASTID / -100000 /


*...... IDs of compounds and mixtures

        DATA IDTAB /       99 ,       101 ,       103 ,       104 ,
     *                    106 ,       111 ,       119 ,       120 ,
     *                    126 ,       130 ,       134 ,       138 ,
     *                    139 ,       141 ,       155 ,       160 ,
     *                    169 ,       179 ,       185 ,       189 ,
     *                    191 ,       197 ,       200 ,       201 ,
     *                    202 ,       203 ,       204 ,       209 ,
     *                    213 ,       215 ,       216 ,       219 ,
     *                    221 ,       222 ,       223 ,       225 ,
     *                    226 ,       227 ,       232 ,       238 ,
     *                    245 ,       252 ,       255 ,       263 ,
     *                    264 ,       266 ,       276 ,       277 ,
     *                    906                                     /

*...... Effective Z values of compounds and mixtures calculated as
*...... weighted averages from Table 1.2 of ICRU Report 49

        DATA DZTAB / 5.942618 ,  5.612910 ,  5.905021 ,  7.372747 ,
     *              10.646255 ,  8.732800 ,  9.247112 , 10.628862 ,
     *               7.313059 , 14.646751 ,  7.454168 ,  7.131620 ,
     *               7.463094 , 54.023098 ,  5.281445 ,  7.347248 ,
     *              10.322468 ,  6.359930 ,  7.394490 ,  6.822535 ,
     *               6.461884 ,  4.743470 ,  6.776222 ,  7.129601 ,
     *               7.095221 ,  6.964475 ,  7.010650 ,  5.916677 ,
     *               5.256975 , 35.808854 ,  5.575000 ,  6.100061 ,
     *               5.281445 ,  6.456261 ,  6.236538 ,  5.281445 ,
     *               5.612910 ,  8.279451 , 11.997960 ,  5.085725 ,
     *              10.804610 , 46.558334 ,  5.664495 ,  6.339387 ,
     *               6.108394 ,  5.562450 ,  7.216742 ,  7.216742 ,
     *               6.000000                                     /


*...... Set default return code

        RC=0

*...... Is it elemental ID ?

        IF(ID.LT.99) THEN
          DBLEZ=DBLE(ID)
          RETURN
        ENDIF

*...... Z(ID) already in cache ?

        IF(ID.EQ.LASTID) THEN
          DBLEZ=LASTDZ
          RETURN
        ENDIF

*...... Look up ID in table

        DO 10 I=1,NIDS

          IF(ID.EQ.IDTAB(I)) THEN
            DBLEZ =DZTAB(I)
            LASTID=ID
            LASTDZ=DBLEZ
            RETURN
          ENDIF

10      CONTINUE

*...... Bad news, ID not found

        DBLEZ=0.0D0
        RC   =40

        RETURN

*...... End of subroutine MSID2Z

      END

