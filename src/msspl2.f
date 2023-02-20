************************************************************************
**                                                                    **
**                  SUBROUTINE MSSPL2(N,X,Y,YPP,XS,YS)                **
**                                                                    **
************************************************************************
**                                                                    **
**  Cubic spline interpolation of tabulated function X/Y :            **
**  Part 2 ---> Calculation interpolant YS at point XS.               **
**                                                                    **
**  WARNING: The routine requires X(1) < X(2) < ... < X(N) ,  N>2 ,   **
**  -------- X(1) <= XS <= X(N) and YPP set by a call to MSSPL1 !     **
**           NO CHECKS ARE DONE WITHIN THIS ROUTINE !                 **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  N      ( INTEGER )    Array size.              **
**  -----         (I)  X      (DBLE PREC)    x_i  --> Array(1..N).    **
**                (I)  Y      (DBLE PREC)    y_i  --> Array(1..N).    **
**                (I)  YPP    (DBLE PREC)    y_i" --> Array(1..N).    **
**                (I)  XS     (DBLE PREC)    Evaluation point.        **
**                (O)  YS     (DBLE PREC)    Result YS=y(XS).         **
**                                                                    **
**                                                                    **
**  PARAMETER:    NONE                                                **
**  ----------                                                        **
**                                                                    **
**  COMMONS:      NONE                                                **
**  --------                                                          **
**                                                                    **
**  CALLS:        NONE                                                **
**  ------                                                            **
**                                                                    **
************************************************************************

      SUBROUTINE MSSPL2(N,X,Y,YPP,XS,YS)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER N
        DOUBLE PRECISION X(N),Y(N),YPP(N)

        INTEGER IL,IR,I


*...... Search interval [ IL , IR ] in X/Y table

        IL =1
        IR =N

10      CONTINUE

          IF(IR-IL.LE.1) GOTO 20

          I=(IL+IR)/2

          IF(X(I).LE.XS) THEN
            IL=I
          ELSE
            IR=I
          ENDIF

        GOTO 10
20      CONTINUE

*...... Calculate spline

        H  = X(IR)-X(IL)
        A  =(X(IR)-XS)/H
        B  =(XS-X(IL))/H
        C  =A*(A*A-1.0D0)*H*H/6.0D0
        D  =B*(B*B-1.0D0)*H*H/6.0D0

        YS =A*Y(IL)+B*Y(IR)+C*YPP(IL)+D*YPP(IR)

        RETURN

*...... End of subroutine MSSPL2

      END

