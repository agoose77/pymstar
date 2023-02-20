************************************************************************
**                                                                    **
**  PROGRAMSYSTEM : MORESTAR                                          **
**                                                                    **
************************************************************************
**                                              **                    **
**  MODULE : MSTAR                              **       V 3.12       **
**                                              **                    **
************************************************************************
**                                                                    **
**  WRITTEN BY   :  ANDREAS SCHINNER AND HELMUT PAUL                  **
**  DATE         :  14-09-2002                                        **
**  WRITTEN ON   :  ATHLON 1GHz                                       **
**  UNDER SYSTEM :  LINUX 2.2.16                                      **
**  LANGUAGE     :  ANSI FORTRAN 77                                   **
**                                                                    **
************************************************************************
**                                                                    **
**  This is a sample main program for the MSTAR subroutine package.   **
**                                                                    **
************************************************************************

      PROGRAM MSTAR

        IMPLICIT DOUBLE PRECISION (A-Z)
        CHARACTER    CG,CG0
        CHARACTER*2  PTABLE
        CHARACTER*4  VERSN
        CHARACTER*13 FNAME,BK /'             '/
        CHARACTER*80 TAG,EMSG,TAG1
        INTEGER Z1,ID,I,J,K,L,MODE,RC,NEP


        PARAMETER    (    VERSN   =   '3.12'   ,
     *                    NEP     =    132     )


        COMMON /MSDEBG/ APL,BPL,CPL


        DIMENSION EP(NEP),EI(NEP),SEI(NEP)


        DIMENSION PTABLE(99)
        DATA PTABLE / 'H ','He','Li','Be','B ','C ','N ','O ','F ' ,
     *                'Ne','Na','Mg','Al','Si','P ','S ','Cl','Ar' ,
     *                'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co' ,
     *                'Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr' ,
     *                'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh' ,
     *                'Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe' ,
     *                'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu' ,
     *                'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf' ,
     *                'Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl' ,
     *                'Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th' ,
     *                'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es' /


*...... Preset NEP (alpha) energies

        DATA EP /
     1 0.0010,0.0015,0.0020,0.0025,0.0030,0.0040,0.0050,0.0060,0.0070,
     2 0.0080,0.0090,0.0100,0.0125,0.0150,0.0175,0.0200,0.0225,0.0250,
     3 0.0275,0.0300,0.0350,0.0400,0.0450,0.0500,0.0550,0.0600,0.0650,
     4 0.0700,0.0750,0.0800,0.0850,0.0900,0.0950,0.1000,0.1250,0.1500,
     5 0.1750,0.2000,0.2250,0.2500,0.2750,0.3000,0.3500,0.4000,0.4500,
     6 0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,0.8000,0.8500,0.9000,
     7 0.9500,1.0000,1.2500,1.5000,1.7500,2.0000,2.2500,2.5000,2.7500,
     8 3.0000,3.5000,4.0000,4.5000,5.0000,5.5000,6.0000,6.5000,7.0000,
     9 7.5000,8.0000,8.5000,9.0000,9.5000,10.000,12.500,15.000,
     1 17.500,20.000,22.500,25.000,27.500,30.000,35.000,40.000,
     2 45.000,50.000,55.000,60.000,65.000,70.000,75.000,80.000,
     2 85.000,90.000,95.000,100.00,125.00,150.00,175.00,200.00,225.00,
     3 250.00,275.00,300.00,350.00,400.00,450.00,500.00,550.00,600.00,
     4 650.00,700.00,750.00,800.00,850.00,900.00,950.00,1000.0,
     * 1250.0,1500.0,1750.0,2000.0,2250.0,2500.0,2750.0,3000.0,
     * 3500.0,4000.0                                                 /

*...... Header & dialog

        WRITE(6,1000) VERSN
1000    FORMAT(/1X,'PROGRAM MSTAR - VERSION ',A                /
     *          1X,'============================'              /
     *          1X,'[Written by A. Schinner and H. Paul]'      /)

2000    FORMAT(1X,A)

10      WRITE(6,2000) 'Enter projectile atomic number             : '
        READ(5,*,ERR=10) Z1
        IF(Z1.LT.1.OR.Z1.GT.99) GOTO 10

20      WRITE(6,2000) 'Enter target ID number                     : '
        READ(5,*,ERR=20) ID
        IF(ID.LE.0.OR.ID.GT.99999) GOTO 20

        IF(Z1.NE.2) THEN
30        WRITE(6,2000) 'Enter a or b                                 '
          WRITE(6,2000) 'a use standard mode c or g                   '
          WRITE(6,2000) ' (correct state is selected automatically)   '
          WRITE(6,*)
          WRITE(6,2000) 'b use preferred mode d or h if available,    '
          WRITE(6,2000) ' (d means: Z2-dependent effective charge!)   '
          WRITE(6,2000) ' (correct state is selected automatically) ? '
          READ(5,*,ERR=30) CG
          IF(CG.EQ.'A') CG='a'
          IF(CG.EQ.'B') CG='b'
          IF(CG.NE.'a'.AND.CG.NE.'b') GOTO 30
        ELSE
          CG=' '
        ENDIF

        CG0=CG

40      WRITE(6,2000) 'Single energy (1) or table of energies (2) ? '
        READ(5,*,ERR=40) MODE
        IF(MODE.NE.1.AND.MODE.NE.2) GOTO 40

2500    FORMAT(' ===================================================='/
     *         '  WARNING : MODE d/h NOT AVAILABLE ---> USING MODE ',A/
     *         ' ===================================================='/
     *        )

3000    FORMAT(1X,'PROJECTILE         :  ',A                 /
     *         1X,'TARGET             :  ',A                 /
     *         1X,'TARGET STATE MODE  :  ',A                 /
     *         1X,'SPECIFIC ENERGY    : ',1P,E13.6,
     *                                    '  MeV/nucleon'    /
     *         1X,'EL. STOPPING POWER : ',1P,E13.6,
     *                                    '  MeV cm2 /mg'    /
     *         1X,'PARAMETER A        : ',1P,E13.6           /
     *         1X,'PARAMETER B        : ',1P,E13.6           /
     *         1X,'PARAMETER C        : ',1P,E13.6           /
     *         1X,'ERROR STATUS       :  ',A                 /)

3500    FORMAT(1X,'PROJECTILE         :  ',A                 /
     *         1X,'TARGET             :  ',A                 /
     *         1X,'TARGET STATE MODE  :  ',A                 /
     *         1X,'OUTPUT FILE        :  ',A                 /
     *         1X,'ERROR STATUS       :  ',A                 /)

*...... Energy range warning

        PRINT *
        PRINT *, '-----------------------------------------------',
     *           '-----------------'
        PRINT *, ' Note that the experimental data on which these',
     *           ' calculations '
        PRINT *, ' are based, go up to several 100 MeV/nucleon'   ,
     *           ' for O and Ar ions,'
        PRINT *, ' but only up to about 10 MeV/nucleon for many'  ,
     *           ' other ions.'
        PRINT *, '-----------------------------------------------',
     *           '-----------------'
        PRINT *

*...... Handle modes

        IF(MODE.EQ.1) THEN

*........ Single energy

50        WRITE(6,2000) 'Enter spec. energy in MeV/nucl (-1 to exit): '
          READ(5,*,ERR=50) E1
          IF(E1.LE.0.0D0) GOTO 9500

          CALL MSTAR1(ID,CG,Z1,E1,TAG,STE,RC)
          CALL MSEMSG(RC,EMSG)

          IF(CG0.EQ.'b'.AND.CG.NE.'d'.AND.CG.NE.'h') WRITE(6,2500) CG

          IF(RC.EQ.0) THEN
            WRITE(6,3000) PTABLE(Z1),TAG(1:55),CG,E1,STE,
     *                    APL,BPL,CPL,EMSG(1:55)
          ELSE
            WRITE(6,3000) PTABLE(Z1),' ',CG,E1,0.0D0,
     *                    0.0D0,0.0D0,0.0D0,EMSG(1:55)
          ENDIF

          GOTO 50

        ELSE

*........ Preset energies -> main loop

          J  =0
          TAG='                                                       '

          DO 90 I=1,NEP

*.......... E1 = MeV/nucleon

            E1 = EP(I)/4.0D0

            CALL MSTAR1(ID,CG,Z1,E1,TAG1,STE,RC)

            IF(RC.EQ.0) THEN

*............ Result is OK

              J     =J+1
              TAG   =TAG1
              EI(J) =E1
              SEI(J)=STE

            ELSEIF(RC.EQ.30) THEN

*............ Energy value outside allowed range --> continue

              GOTO 90

            ELSE

*............ Severe error --> break

              GOTO 9100

            ENDIF

90        CONTINUE

*........ If not a single valid data point ---> severe error

          IF(J.LT.1) GOTO 9100

*........ Create output file name

          WRITE(FNAME,4000) PTABLE(Z1),ID,CG
4000      FORMAT(A2,I5,A1)

*........ Compress filename --> FNAME(1:K-1)

          DO 60 K=1,13
            IF(FNAME(K:K).NE.' ') GOTO 60
            DO 70 L=K+1,13
              IF(FNAME(L:L).NE.' ') THEN
                FNAME(K:K)=FNAME(L:L)
                FNAME(L:L)=' '
                GOTO 60
              ENDIF
70          CONTINUE
            GOTO 80
60        CONTINUE
80        CONTINUE

*........ Print status

          IF(CG0.EQ.'b'.AND.CG.NE.'d'.AND.CG.NE.'h') WRITE(6,2500) CG

          WRITE(6,3500) PTABLE(Z1),TAG(1:55),CG,
     *                  FNAME(1:K-1)//'.ms','NO ERROR'

*........ Open output file

          OPEN(8,FILE=FNAME(1:K-1)//'.ms',ERR=9200)

*........ Write output

          WRITE(8,5000,ERR=9300) VERSN,PTABLE(Z1),TAG(1:55),CG,
     *                           APL,BPL,CPL,
     *                           FNAME(1:K-1),BK(1:(15-K)/2),
     *                           FNAME(1:K-1)

5000      FORMAT('FILE CREATED BY     : PROGRAM MSTAR - VERSION ',A /
     *           'PROJECTILE          : ',A                         /
     *           'TARGET              : ',A                         /
     *           'TARGET STATE MODE   : ',A                         /
     *           'PARAMETER A         : ',1P,E13.6                  /
     *           'PARAMETER B         : ',1P,E13.6                  /
     *           'PARAMETER C         : ',1P,E13.6                  /
     *           'SPECIFIC ENERGIES   : column E (in MeV/nucleon)'  /
     *           'EL. STOPPING POWERS : column ',A,
     *                                  ' (in MeV cm2 /mg)'        //
     *            9X,'E',11X,A,A                                    )


          DO 100 I=1,J

            WRITE(8,5500,ERR=9300) EI(I),SEI(I)
5500        FORMAT(3X,1P,E13.6,5X,E13.6)

100       CONTINUE

*........ Close output file

          CLOSE(8,ERR=9400)

          GOTO 9500

        ENDIF

*...... Exits

9000    STOP 'PROGRAM MSTAR TERMINATED NORMALLY'

9100    IF(RC.NE.30) THEN
            CALL MSEMSG(RC,EMSG)
            WRITE(6,3000) PTABLE(Z1),TAG(1:55),CG,E1,0.0D0,
     *                    0.0D0,0.0D0,0.0D0,EMSG(1:55)
        ELSE
            WRITE(6,3000) PTABLE(Z1),' ',CG,E1,0.0D0,0.0D0,0.0D0,0.0D0,
     *                    'ALL ENERGIES OUTSIDE ALLOWED RANGE'
        ENDIF

        GOTO 9500

9200    STOP '*** MSTAR : Output file open error ***'
9300    STOP '*** MSTAR : Output file write error ***'
9400    STOP '*** MSTAR : Output file close error ***'

9500    WRITE(6,2000) 'Restart (1) or exit (2) program            : '
        READ(5,*,ERR=9500) MODE
        IF(MODE.EQ.1) GOTO 10
        IF(MODE.EQ.2) GOTO 9000
        GOTO 9500

*...... End of program MSTAR

      END


*.... Including MSTAR subroutine package

      INCLUDE 'mssubs.f'

