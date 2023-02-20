************************************************************************
**                                                                    **
**  PROGRAMSYSTEM : MORESTAR                                          **
**                                                                    **
************************************************************************
**                                              **                    **
**  MODULE : MSSUBS                             **       V 3.12       **
**                                              **                    **
************************************************************************
**                                                                    **
**  WRITTEN BY   :  ANDREAS SCHINNER AND HELMUT PAUL                  **
**  DATE         :  14-10-2001                                        **
**  WRITTEN ON   :  ATHLON 1GHz                                       **
**  UNDER SYSTEM :  LINUX 2.2.16                                      **
**  LANGUAGE     :  ANSI FORTRAN 77                                   **
**                                                                    **
************************************************************************
**                                                                    **
**  This is the 'MSTAR' subroutine package. It calculates the         **
**  electronic stopping power for a great variety of projectiles and  **
**  targets, using the Berger data [1] for alpha projectiles,         **
**  together with Paul's fitting formulae [2,3] that transform the    **
**  alpha results into results for 2 < Z1 < 19.                       **
**                                                                    **
**  [1] M. J. Berger, NISTIR 4999 (1992);                             **
**                    Program ASTAR, Version 2 of 12 August 1993      **
**  [2] H. Paul and A. Schinner,                                      **
**                Nucl. Instrum. Methods Phys. Res. B179 (2001) 299   **
**  [3] H. Paul and A. Schinner,                                      **
**                Nucl. Instrum. Methods Phys. Res. B195 (2002) 166   **  
**                                                                    **
**--------------------------------------------------------------------**
**                                                                    **
**  Only two of the routines are intended for direct usage:           **
**                                                                    **
**  (1) SUBROUTINE MSTAR1(ID,CG,Z1,E,NAME,STE,RC)                     **
**                                                                    **
**      INTEGER ID           : (input)                                **
**                             ID-value of the target, as described   **
**                             in the work by Berger for his ASTAR    **
**                             program [1].                           **
**                                                                    **
**      CHARACTER CG         : (input/output)                         **
**                             Must be set to                         **
**                             'C' or 'c' for condensed targets.      **
**                             'D' or 'd' for special cond. targets.  **
**                             'G' or 'g' for gaseous targets.        **
**                             'H' or 'h' for special gas. targets.   **
**                             'A' or 'a' for using c or g            **
**                                        (automatic state selection) **
**                             'B' or 'b' for using d or h if avail.  **
**                                        (automatic state selection) **
**                             On exit it is set to the actually      **
**                             used mode 'C' or 'G'.                  **
**                                                                    **
**      INTEGER Z1           : (input)                                **
**                             Atomic number of the projectile.       **
**                                                                    **
**      DOUBLE PRECISION E   : (input)                                **
**                             Projectile energy in MeV / nucleon.    **
**                                                                    **
**      CHARACTER*80 NAME    : (output)                               **
**                             Returns the name tag of the target.    **
**                                                                    **
**      DOUBLE PRECISION STE : (output)                               **
**                             Returns the electronic stopping power  **
**                             in MeV cm2 /mg.                        **
**                                                                    **
**      INTEGER RC           : (output)                               **
**                             Returncode. A nonzero value indicates  **
**                             an error. The corresponding error      **
**                             message can be obtained by routine     **
**                             'MSEMSG'.                              **
**                                                                    **
**                                                                    **
**  (2) SUBROUTINE MSEMSG(RC,TEXT)                                    **
**                                                                    **
**      INTEGER RC           : (input)                                **
**                             The returncode set by routine 'MSTAR1' **
**                                                                    **
**      CHARACTER*80 TEXT    : (output)                               **
**                             The error message text corresponding   **
**                             to 'RC'.                               **
**                                                                    **
**                                                                    **
**  The other routines should not be called directly by the user of   **
**  this package. The  following routine names are reserved:          **
**                                                                    **
**  MSID2Z , MSPAUL , MSRDB1 , MSSEAL , MSSPL1 , MSSPL2 , MSERF1      **
**                                                                    **
**  as well as the named COMMON block /MSDEBG/.                       **
**                                                                    **
************************************************************************
**                                                                    **
**                        H I S T O R Y                               **
**                                                                    **
**--------------------------------------------------------------------**
**                                                                    **
**  VERSION     DATE                    MODIFICATIONS                 **
**  -------     ----                    -------------                 **
**                                                                    **
**   1.00    19-09-2000                *** ROOT VERSION ***           **
**   1.10    28-09-2000                Minor changes.                 **
**   1.11    05-10-2000                Minor changes.                 **
**   1.20    03-11-2000                Automatic CG selection added.  **
**   1.30    10-11-2000                /MSDEBG/ block in 'MSPAUL'.    **
**   1.40    17-11-2000                MODES 'd'+'h' added.           **
**   1.43    14-08-2001                'MSPAUL' modified.             **
**   1.50    21-08-2001                'MSID2Z' added + minor modif.  **
**   1.60    25-09-2001                High energy data added.        **
**   2.00    14-10-2001                Mode b added, exch. a<->b.     **
**   2.10    22-03-2002                Nickel target added.           **
**   3.00    13-11-2002                'MSPAUL' modified.             **
**   3.10    18-03-2003                Tantalum target added.         **
**   3.11    31-03-2003                Boron, Zirconium targets added **
**   3.12    24-05-2004                Routine 'MSERF1' introduced.   **
**                                                                    **
************************************************************************

        INCLUDE 'msemsg.f'
        INCLUDE 'mstar1.f'

        INCLUDE 'msid2z.f'
        INCLUDE 'mspaul.f'
        INCLUDE 'msrdb1.f'
        INCLUDE 'msseal.f'
        INCLUDE 'msspl1.f'
        INCLUDE 'msspl2.f'
        INCLUDE 'mserf1.f'

