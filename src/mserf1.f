************************************************************************
**                                                                    **
**               DOUBLE PRECISION FUNCTION MSERF1(X)                  **
**                                                                    **
************************************************************************
**                                                                    **
**  Approximation to the error function using rational approximation  **
**  Eq.(7.1.26) from Abramowitz/Stegun.                               **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:    (I)  X     (DBLE PREC)       Argument.                   **
**  -----                                                             **
**                                                                    **
**  PARAMETER:    BIGX  (DBLE PREC)       Large x cutoff.             **
**  ----------                                                        **
**                                                                    **
**  COMMONS:      NONE                                                **
**  --------                                                          **
**                                                                    **
**  CALLS:        NONE                                                **
**  ------                                                            **
**                                                                    **
************************************************************************

      DOUBLE PRECISION FUNCTION MSERF1(X)

        IMPLICIT DOUBLE PRECISION (A-Z)

        INTEGER I


        PARAMETER (  BIGX  =   5.0D0  )


        DIMENSION A(5)

        DATA A /  0.254829592  ,  -0.284496736  ,
     *            1.421413741  ,  -1.453152027  ,
     *            1.061405429                   /
        DATA P /  0.327591100                   /


*...... Handle sign of X

        IF(X.LT.0.0D0) THEN
          XA=-X
          SI=-1.0D0
        ELSE
          XA= X
          SI= 1.0D0
        ENDIF

*...... Handle large x

        IF(XA.LE.BIGX) THEN

*........ Evaluate rational approximation

          T  =1.0D0/(1.0D0+P*XA)
          TI =1.0D0
          SUM=0.0D0

          DO 10 I=1,5
            TI =TI*T
            SUM=SUM+A(I)*TI
10        CONTINUE

          MSERF1=SI*(1.0D0-SUM*DEXP(-X*X))

        ELSE

*........ Large X

          MSERF1=SI

        ENDIF

        RETURN

*...... End of function MSERF1

      END

