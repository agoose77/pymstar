
************************************************************************
**                                                                    **
**              SUBROUTINE MSPAUL(MODE,IZ1,ID2,E,FOUT,RC)             **
**                                                                    **
************************************************************************
**                                                                    **
**  Returns the stopping power correction fit functions FOUT          **
**  based on the "Cumulative Weibull Function"                        **
**                                                                    **
**    for solids:         d1=17.18567-0.65734*Z1                      **
**    and for gases:      d1=9.1963 - 0.29643*Z1                      **
**        FOUT=a+(1.01-a)*(1-exp(-((x+c*(log(2))^(1/d1)-b)/c)^d1))    **
**             *(1+a'+b'exp(-x))                                      **
**                                                                    **
**                                                                    **
**  developed by H. Paul for ions from Li to Ar.                      **
**                                                                    **
**  REVISIONS:                                                        **
**  24 Jan 2001: Z1-oscillations included for solids                  **
**               Data for Li ions in gases by An78 included           **
**  13 Feb 2001: Fermi3 replaced by WeibullC2 ('c' only)              **
**  16 Feb 2001: Weibull multiplied by 1+a'+b'exp(-x): WeibullC3      **
**   9 Mar 2001: new Z2-dependent coefficients (mode 'd')             **
**               for C, N, O, Ar and (1st time!) Li ions in solids    **
**  14 Mar 2001: Z2-dependent coefficients for B, F, Si, and Cl ions  **
**  16 Mar 2001: Z2-dependent coefficients for B removed again        **
**  22 Mar 2001: WeibullC2 for gases (mode 'g')                       **
**  23 Mar 2001: WeibullC2 for gases (mode 'h', Z2-independent,       **
**                        for all ions except Mg, Al, Si, P)          **
**  16 May 2001: WeibullC3 renewed after removing Bi90, adding LuX00  **
**  13 Jun 2001: WeibullC3 renewed after adding ions up to Ti, mylar  **
**                        targets                                     **
**  25 Jun 2001: logical error removed ('d' was wrong until now!?)    **
**  25 Jun 2001: Special case Mylar treated                           **
**  26 Jun 2001: Z2-dependent coefficients (mode 'd') with new data   **
**                for  Li, B, C, N, O, F, Si, S, Cl, and Ar ions      **
**  28 Jun 2001: New WeibullC2g for gases (mode 'g')                  **
**                after removing Bi90 (O ions) and Pr93 (N ions)      **
**  02 Jul 2001: Mode 'h' also changed for O and N ions               **
**  10 Jul 2001: WeibullC2g for H, He targets                         **
**  06 Aug 2001: Block 'h' mode for H and He targets [A.S.]           **
**               Z2 change for Mylar corrected [A.S.]                 **
**  21 Aug 2001  Added ID->Z conversion for compounds/mixtures [A.S.] **
**  13 May 2002  Mode d for N in targets Ti - Cu improved [H.P.]      **
**  23 Oct 2002  New Weibull Function fit (mode 'c') with new data,   **
**                 and with data for nickel targets                   **
**  11 Nov 2002  Mode d calculated as a correction to mode c,         **
**                 now independent of Z1                              ** 
**  21 Nov 2002  Mode 'h' improved for S,Cl,Ar ions                   **
**  29 Nov 2002  Mode 'd' set equal mode 'c' for Z2 around 26         **
**  24 May 2004  Replaced NAGLIB S15AEF by local routine MSERF1. A.S. **
**                                                                    **
************************************************************************
**                                                                    **
**                                                                    **
**  VARS:   (I)  MODE  (CHARACTER)   'c' --> condensed                **
**  -----                                   (for all solid targets)   **
**                                   'd' --> (particular              **
**                                            solid targets)          **
**                                   'g' --> gaseous                  **
**                                         (all gases including H,He; **
**                                        special treatment for H,He) **
**                                   'h' --> (particular ions on      **
**                                         gaseous targets, not H,He) **
**          (I)  IZ1   ( INTEGER )   Projectile atomic number.        **
**          (I)  ID2   ( INTEGER )   Target ID.                       **
**          (I)  E     (DBLE PREC)   Projectile spec. energy          **
**                                      in  MeV/nucleon.              **
**          (O)  FOUT  (DBLE PREC)   Result.                          **
**          (O)  RC    ( INTEGER )   Return code:                     **
**                                        0 : no error                **
**                                        4 : illegal MODE            **
**                                        5 : internal error          **
**                                       40 : MSID2Z unknown ID       **
**                                                                    **
**  PARAMETER:    NONE                                                **
**  ----------                                                        **
**                                                                    **
**  COMMONS:      /MSDEBG/ A,B,C                                      **
**  --------                                                          **
**                                                                    **
**  CALLS:        MSID2Z        Converts ID to effective Z            **
**  ------                                                            **
**                                                                    **
************************************************************************

      SUBROUTINE MSPAUL(MODE,IZ1,ID2,E,FOUT,RC)

        IMPLICIT DOUBLE PRECISION (A-Z)
        CHARACTER MODE
        INTEGER IZ1,ID2,RC

        INTEGER Z1,Z2,INT

        COMMON /MSDEBG/ A,B,C


*...... Useful abbreviations & initialization

        Z1   = IZ1
        DZ1  = DBLE(Z1)
        X    = DZ1
        LX   = DLOG(X)

        A    = 0.0D0
        B    = 0.0D0
        C    = 0.0D0
        FOUT =0.0D0
        RC   = 0

        CALL MSID2Z(ID2,DZ2,RC)
        IF(RC.NE.0) RETURN

        Z2 =INT(DZ2+0.5D0)

*...... Block MODE 'h' for Z2<3

        IF(MODE.EQ.'h'.AND.Z2.LT.3) THEN

          RC=4
          RETURN

        ENDIF

*...... Set parameters a,b,c

        IF(MODE.EQ.'c'.OR.MODE.EQ.'d') THEN

*........ Here the parameters a,b,c are defined for condensed matter
*........ (All Solid Targets, Coef(a,b,c)A2s.prn, 22 Oct 02)

          A =  152.3281067D 0 -509.175005D 0 * X**0.5D 0
     *         + 764.0597161D 0 * X -666.456225D 0 * X**1.5D 0
     *         + 369.6135044D 0 * X**2.D 0 -134.506826D 0 * X**2.5D 0
     *         + 32.02328376D 0 * X**3.D 0 -4.80247840D 0 * X**3.5D 0
     *         + 0.411498619D 0 * X**4.D 0 -0.015353889D 0 * X**4.5D 0
   
          B =  (-1.60299361D 0 +2.594689067D 0 * X**0.5D 0
     *         - 1.73972608D 0 * X +0.601973528D 0 * X**1.5D 0
     *         - 0.10683182D 0 * X**2.0D 0 +0.0077231543D 0*X**2.5D 0)   
     *         / (1.0D 0 -1.44605189D 0 * X**0.5D 0 +0.871321669D 0*X
     *         - 0.24591206D 0 * X**1.5D 0 +0.027726119D 0 * X**2.0D 0
     *         - 0.00042121782D 0 * X**2.5D 0)
 
          C =  1.0 D 0 / (0.15607719D 0 -0.0023161329 * X**2.D 0 * LX
     *         + 0.0016853943 * X**2.5D 0)
        

        ELSEIF(MODE.EQ.'g'.AND.Z2.GT.2) THEN

*........ Here the parameters a,b,c for gases are defined (X = Z1)
*........ (All gaseous Targets except H, He; Bi90 and Pr93 removed
*........  FitCoef2g(a,b,c)R.prn, 28 Jun 01)

          A =  38.150347 D 0 - 5.0998443D 0 * X - 151.25405D 0 / X
     *         + 0.3682134 D 0 * X**2 + 301.77474 D 0 / X**2
     *         - 1.3542411 D-2 * X**3 - 225.08009 D 0 / X**3
     *         + 1.9888596 D-4 * X**4

          B =  -  0.1983923 D 0
     *         +  3.2653672 D-2 * X
     *         -  12.281832 D 0 * DEXP(-X)

          C =  -  1.3058678 D 0
     *         -  0.5460869 D 0 * LX**2
     *         +  2.3894315 D 0 * LX
          C =  DEXP(C)

        ELSEIF(MODE.EQ.'g'.AND.Z2.LE.2) THEN

*......  Here a,b,c are defined for H and He targets (X = Z1):
*......  (CoefHHe.opj, 2 Jul, Coefb_HHeS.prn, 9 Jul 01,
*......   Coefa_HHeS1.prn, 10 Jul)

          A = 82.333704 D 0 - 1.7353839D 2 * X**0.5
     *        + 1.4994474 D 2 * X    - 67.608335 D 0 * X**1.5
     *        + 1.6771605 D 1 * X**2 - 2.1732999 D 0 * X**2.5
     *        + 1.1514042 D-1 * X**3

          B = 0.74789173 D 0 - 1.6701292 D 0 / LX

          C = 3.2370042  D 0 - 0.049755185 D 0 * X

*...... Here a,b,c for gases are defined for particular ions:
*...... (Z2-independent)

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.3) THEN

*........ Parameters for Li ions in N, Air, O, Ne, Ar, Kr, Xe
*         (PrLiN_Xe2.opj, 20 Mar 01)

          A = 0.59251
          B =-0.71211
          C = 1.91956

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.4) THEN

*........ Parameters for Be ions in Air, Ne, Ar
*         (PrBeAi_Ar2.opj, 21 Mar 01)

          A = 0.35030
          B =-0.28291
          C = 2.92675

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.5) THEN

*........ Parameters for B ions in N, Air, O, Ne, Ar
*         (PrBNtoAr2.opj, 21 Mar 01)

          A = 0.32192
          B =-0.13151
          C = 2.97854

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.6) THEN

*........ Parameters for C ions in N, Air, O, Ne, Ar
*         (PrCN_Ar2.opj, 20 Mar 01)

          A = 0.27061
          B =-0.01952
          C = 2.80761

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.7) THEN

*........ Parameters for N ions in N, Air, O, Ne, Ar (Pr93 removed)
*         (PrNN_Xe2r.opj, 26 Mar 01)

          A = 0.21054
          B = 0.01528
          C = 3.99705


        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.8) THEN

*........ Parameters for O ions in N, air, O, Ne, Ar, Kr, Xe
*         (PrON_Xe2b.opj, 29 Jun 01, Bi90 removed)

          A = 0.16969
          B = 0.1075
          C = 3.75755

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.9) THEN

*........ Parameters for F ions in N, Air, O, Ne, Ar
*         (PrFNtoAr2.opj, 21 Mar 01)

          A = 0.12192
          B = 0.08955
          C = 3.59459

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.10) THEN

*........ Parameters for Ne ions in N, Air, Ne, Ar, Kr, Xe
*         (PrNeN_Xe2.opj, 21 Mar 01)

          A = 0.09358
          B = 0.08692
          C = 3.39734

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.11) THEN

*........ Parameters for Na ions in Air, Ne, Ar
*         (PrNaAirtoAr2.opj, 21 Mar 01)

          A = 0.06353
          B = 0.21303
          C = 4.29082

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.16) THEN

*........ Parameters for S ions in N, Air, Ar, Kr
*         (PrSN_Kr2ba.opj, 21 Nov 02)

          A = 0.05698
          B = 0.34491
          C = 3.01333

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.17) THEN

*........ Parameters for Cl ions in N, Air, Ar
*         (PrClN_Ar2ba.opj, 21 Nov 02)

          A = 0.05163
          B = 0.37948
          C = 2.81286

        ELSEIF(MODE.EQ.'h'.AND.Z1.EQ.18) THEN

*........ Parameters for Ar ions in N, Air, Ne, Ar, Kr, Xe
*         (PrArN_Xe2ba.opj, 21 Nov 02)

          A = 0.048
          B = 0.38887
          C = 2.84076

        ELSE

*........ Illegal MODE (e.g., mode 'h' and Z1 = 12)

          RC=4
          RETURN

        ENDIF

*...... Evaluation of proper function

        IF(MODE.EQ.'c'.OR.MODE.EQ.'d') THEN

*........ Calculate output function for solid targets & return
*........ a', b' from AllAsrC2.prn, 23 Oct 02

          X   =DLOG10(E)
          D1  =17.18567D0-.65734D0*DZ1
          FOUT=1.01D0-A
          TP  =(X+C*(DLOG(2.0D0))**(1/D1)-B)/C
          IF(TP.LT.0) TP=0
          FOUT=FOUT*(1.0D0-DEXP(-TP**D1))
          FOUT=A+FOUT
          FOUT=(1.0D0-0.0041466419D0-0.0010098198D0*DEXP(-X))*FOUT

          FACTOR = 1.0D0

*........ Convert FOUT value from mode c to mode d 
*........          (for particular values Z2):

*........ for C and Mylar Targets (CMylrC3.prn, 11 Nov 02)

          IF(MODE.EQ.'d'.AND.Z2.GT.4.AND.Z2.LT.10) THEN  
          
          FACTOR = 1.0D0 + 0.0012171149D0 - 0.04021975D0/(1.0D0 +
     *    ((X + 2.0388341D0)/0.48193661D0)**2.0D0)

*........ for Al and Si targets (AlSirC3.prn, 11 Nov 02)

          ELSEIF(MODE.EQ.'d'.AND.Z2.GT.10.AND.Z2.LT.19) THEN

          FACTOR = 1.0D0 + 0.003491232D0 - 0.11423452D0/(1.0D0 +
     *    ((X + 1.5906109D0)/0.41741941D0)**2.0D0)

*........ for Ti,Fe,Ni,Cu targets (TiFeNiCurC3.prn, 11 Nov 02)

          ELSEIF(MODE.EQ.'d'.AND.Z2.GT.19.AND.Z2.LT.36) THEN

          FACTOR = 1.0D0 

*........ for Mo, Ag, and Sn targets (MoAgSnrC3.prn, 11 Nov 02)

          ELSEIF(MODE.EQ.'d'.AND.Z2.GT.36.AND.Z2.LT.62) THEN

          FACTOR = 1.0D0 - 0.0030928609D0 + 0.11488954D0/(1.0D0 +
     *    ((X + 1.4359953D0)/0.37177939D0)**2.0D0)

*........ for W, Pt, Au, Pb, and U targets (WPtAuPbUrC3.prn, 11 Nov 02)
*........     (MSERF1 is error function)

          ELSEIF(MODE.EQ.'d'.AND.Z2.GT.62.AND.Z2.LT.93) THEN

          a0 = -0.069155044 D0
          b0 = -0.65644678 D0
          c0 =  0.18379076 D0
          d0 = -0.0010104478 D0
          n0 =  d0**2.0D0 + c0**2.0D0

          FACTOR =1.0D0 + a0 * c0 * exp(-0.5D0*(X - b0)**2.0D0/n0)*
     *    (1.0D0 + MSERF1 (d0*(x - b0)/((2.0D0*n0)**0.5D0*c0)))/
     *    n0**0.5D0


          ELSEIF(MODE.EQ.'d') THEN

*........ Illegal MODE (i.e., mode 'd' with unsupported Z2)

           RC=4
           RETURN

          ENDIF

          FOUT=FOUT * FACTOR
          RETURN


        ELSEIF(MODE.EQ.'g'.OR.MODE.EQ.'h') THEN

*........ Calculate output function for gas targets & return

          X   =DLOG10(E)
          D1  =9.1963D0-0.29643D0*DZ1
          FOUT=1.01D0-A
          TP  =(X+C*(DLOG(2.0D0))**(1/D1)-B)/C
          IF(TP.LT.0) TP=0
          FOUT=FOUT*(1.0D0-DEXP(-TP**D1))
          FOUT=A+FOUT

          RETURN

        ELSE

*........ Illegal MODE (you should never end up here)

          RC=5
         RETURN

        ENDIF

*...... End of subroutine MSPAUL

      END

