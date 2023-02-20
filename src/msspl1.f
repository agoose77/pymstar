************************************************************************
**                                                                    **
**                  SUBROUTINE MSSPL1(N,X,Y,YPP,WS)                   **
**                                                                    **
************************************************************************
**                                                                    **
**  Cubic spline interpolation of tabulated function X/Y :            **
**  Part 1 ---> Calculation of second derivatives YPP.                **
**                                                                    **
**  WARNING: The routine requires X(1) < X(2) < ... < X(N) and N>2 !  **
**  -------- NO CHECKS ARE DONE WITHIN THIS ROUTINE !                 **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:         (I)  N      ( INTEGER )    Array size.              **
**  -----         (I)  X      (DBLE PREC)    x_i  --> Array(1..N).    **
**                (I)  Y      (DBLE PREC)    y_i  --> Array(1..N).    **
**                (O)  YPP    (DBLE PREC)    y_i" --> Array(1..N).    **
**                (O)  WS     (DBLE PREC)    Workspace                **
**                                                --> Array(1..N).    **
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

      SUBROUTINE MSSPL1(N,X,Y,YPP,WS)

        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER N
        DOUBLE PRECISION X(N),Y(N),YPP(N),WS(N)

        INTEGER I


*...... Set lower boundary to natural spline

        YPP(1)=0.0D0
        WS(1) =0.0D0

*...... Tridiagonal matrix decomposition loop

        DO 10 I=2,N-1

          HI    =X(I)  -X(I-1)
          HI1   =X(I+1)-X(I)
          SI    =HI/(HI1+HI)
          AI    =2.0D0/SI
          BI    =1.0D0/SI-1.0D0
          CI    =6.0D0*( (Y(I+1)-Y(I))/HI1-(Y(I)-Y(I-1))/HI )/HI

          YPP(I)=-BI/(YPP(I-1)+AI)
          WS(I) =(CI-WS(I-1))/(YPP(I-1)+AI)

10      CONTINUE

*...... Set upper boundary to natural spline

        YPP(N)=0.0D0

*...... Backsubstitution loop of tridiagonal algorithm

        DO 20 I=N-1,1,-1
20        YPP(I)=YPP(I)*YPP(I+1)+WS(I)

        RETURN

*...... End of subroutine MSSPL1

      END

