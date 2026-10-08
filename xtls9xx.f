C#RUN: L=LIB/ASL7 F=X60656/nimr2p(01)  F=XOUT2(02)
      PROGRAM XTLS
      IMPLICIT NONE 
C
C  XTLS (CRYSTALS) FOR HI-ENERGY SPECTROSCOPY  
C 
C           Coded by Arata Tanaka
C
C     INPUT FILE (PARAMETER DESCRIPTION FILE CALLED "X-card")
C           FILE NAME: xcard01 
C        UNIT NUNMBER: 01
C
C     OUTPUT FILE (INITIAL STATE INFORMATION, SPECTRAL DATA AND 
C             STATISTICS ON THE CALCULATIONS ARE WRITTEN IN THIS FILE)
C           FILE NAME: xobj02
C         UNIT NUMBER: 02
C
C  VERSION  5.00,  November 1992 (at Osaka Univ.) 
C       FOR FULL MULTIPLET IMPURITY ANDERSON MODEL
C       WITH SPIN-OBIT INTERRACTION AND CRYSTALFIELD
C       INTEGRATED PROGRAM FOR THREE PHOTO PROCESS: XAS, XPS & RXPS 
C     
C  VERSION  6.00,  August 1993 (at Hiroshima Univ.)
C       EXTENSION FOR A MULTI-SITE MODEL 
C       PHOTO PROCESS: BIS (IPES) & RBIS (RIPES) ARE ALSO AVAILABLE
C     
C  VERSION  7.00,  November 1994 (at Hiroshima Uinv.)
C       IMPROVEMENT OF SUBROUTINE "EIGN"
C       MULTI-CONFIGURATION EXPRESSIONS ARE AVAILABLE WITH
C       USE OF "pre-X" PREPROCESSOR. 
C       PHOTO PROCESS: XES IS ALSO AVAILABLE 
C       
C  VERSION  8.00,  August 1996 (at Hiroshima Uinv.)
C       EXTENSION FOR A DENSITY MATRIX RENORMALIZATION GROUP METHOD 
C       (DMRG MODE)
C  VERSION  8.10,  February 2002 (at Hiroshima Uinv.)
C       MODIFICAITION FOR VECTOR-PARALLEL MACHINE (SUBROUTINE OPRT)
C  VERSION  8.11,  January 2003 
C       NEW MODES FOR OPTICAL CONDUCTIVITY (PHCND) AND ZL WAS ADDED.
C  VERSION  8.20,  February 2003 
C       OPTIMAIZATION FOR VECTOR-PARALLEL MACHINE 
C       EXTENSIVE RECONSTRUCTION OF THE PROGRAM 
C  VERSION  8.30,  September 2004 
C       NEW MODE FOR X-RAY RESONANT SCATTERING (XRS) WAS ADDED.
C  VERSION  8.50,  September 2004 
C       THE LAST VERSION OF 8.xx BRANCH.
C
C  VERSION  9.00,  October 2009 
C       Renewed to its core, this program now support dynamical memory 
C       allocation with Fortran90 compilation and the degeneracy of 
C       orbitals can be even number. This code is chimeric and can be 
C       still compiled as FORTRAN77 program.
C          
C     (last modification 5th Oct. 2009) 
C
      CHARACTER*(60) VERSION
      PARAMETER(VERSION=
     &'Xtls ver. 9.0 coded by Arata Tanaka on 5th Oct. 2009')
C=======================================================
C                    PARAMETERS
C=======================================================
C max. numbers of configurations and states
C max. numbers of orbital and state for static arrays
      INTEGER   NCIM,   NSTM
C>>>START F77
      PARAMETER(NCIM=13000,NSTM=3)
C>>>END F77
      INTEGER   NOBTM 
      PARAMETER(NOBTM=120)
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)
C max. size of arrays (H, IHP1,IHP2) for matrices elements
C max. number of diagonal elements 
      INTEGER   NHM,       NHDM 
C>>>START F77
      PARAMETER(NHM=5000001, NHDM=500001)
C>>>END F77

C max. number of the initial state and their basis functions
      INTEGER   NGM,     NBGM 
C>>>START F77
      PARAMETER(NGM=20,NBGM=100001)
C>>>END F77

C max. sizes of the vector registers 
      INTEGER   NBMM,      NBFM,  NBSM
C>>>START F77
      PARAMETER(NBMM=1,NBFM=1,NBSM=500001)
C>>>END F77

C max. number of the oprators
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C max. number of the delta commands
      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

C size of configuration labels
      INTEGER   NLBLM
      PARAMETER(NLBLM=10)
C size of configuration labels
      INTEGER   IUOUT 
      PARAMETER(IUOUT=2)

C max. number of incident/emitted electron angles 
C max. number of polarizations of light
      INTEGER   NEPHM,     NEANGM,   NDCRM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200)

C max. number of sampling points for a spectrum
C max. number of iteration for the recursion method
      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

C max. orbital momentum for coefficients c^k(lm,l'm') 
C max. orbital momentum for atomic orbitals (not for continuum states) 
C max. degeneracy of atomic orbitals 
      INTEGER   LMAX,  LMAX2,  KMAX,       NEMAX
      PARAMETER(LMAX=8,LMAX2=3,KMAX=2*LMAX,NEMAX=4*LMAX2+2)

C=======================================================
C                   VARIABLES
C=======================================================
C Execution modes
      CHARACTER*(8) MODE
      INTEGER NMODE
      INTEGER NSTATE 

C The number of the electronic configurations 
C The sizes of matrices
C The number of orbital in solid
C The number of orbital including the continuum states
C The degeneracy of each orbital
      INTEGER NCI(0:NSTM2)
      INTEGER NOBT, NOBTX
C>>>START F77
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NELC(NOBTM,NCIM,NSTM)
C>>>END F77
C!>>>START F90
C      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NSFT
C      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: NELC
C!>>>END F90
      INTEGER NDEG(NOBTM)

C State, configuration and orbital labels
      CHARACTER*(NSTM2)    ASTS
C>>>START F77
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
C>>>END F77
C!>>>START F90
C      CHARACTER(NLBLM), DIMENSION(:,:), ALLOCATABLE :: ACI
C!>>>END F90
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Pointers to a matrices elements for each operators
C The diagonal part of a Hamiltonian
C Matrix elements
      INTEGER NHOP(0:NHOPM)
C>>>START F77
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      REAL*8  HD(NHDM)
C>>>END F77
C!>>>START F90
C      REAL*8,  DIMENSION(:), ALLOCATABLE :: H
C      INTEGER, DIMENSION(:), ALLOCATABLE :: IHP1, IHP2
C      REAL*8,  DIMENSION(:), ALLOCATABLE :: HD
C!>>>END F90


C U (Coulomb repulsion energy)
C Delta (settings for relative energies among configurations)
C Absolute energy of a configuration 
C The imaginary and real parts of self energy of the intermediate state
C The real part of the self-energy for the final state(s)
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM2*3+3)
C>>>START F77
      REAL*8  ERED(NCIM)
C>>>END F77
C!>>>START F90
C      REAL*8, DIMENSION(:), ALLOCATABLE :: ERED 
C!>>>END F90
      REAL*8  GAM, GALF
      REAL*8  GALF2(0:NSTM2)

C The ground state eigenvalues and eigenvectors
      INTEGER NG
C>>>START F77
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)
C>>>END F77
C!>>>START F90
C      REAL*8, DIMENSION(:), ALLOCATABLE :: EG 
C      REAL*8, DIMENSION(:,:), ALLOCATABLE :: UG 
C!>>>END F90

      REAL*8  ORBM(0:NOBTM*7)

C Incident photon/electron energies for RPES/RIPES/XES modes
      REAL*8  EPH(0:NEPHM)

C The angles of the incident electrons
      REAL*8  EANG(0:NEANGM)

C The polarizations of the incident/emitted light
C The polarizations of the incident/emitted spin 
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
      INTEGER NSPN(2)

C Number of discrete points, energy range and life-time broadening
C width of the final state(s) for the spectral data
C Spectral intensities
      REAL*8  RANGE(4,NSTM2)
      REAL*8  SPU(0:NPMAX),SPD(0:NPMAX),SPR(0:NPMAX),SPI(0:NPMAX)

C max. number of iteration for the lanczos method 
C and tri-diagonal matrix elements 
      INTEGER NREC
      REAL*8  AR(0:NRECM),AI(0:NRECM),BR(0:NRECM),BI(0:NRECM)

C Parameters for density matrix renormalization group method
      INTEGER NDMO
      INTEGER NDMG(0:NSTM2),NDMALF(0:NSTM2)
      CHARACTER*(32) MATSYB

C Vector registers for the calculations
C>>>START F77
      REAL*8  T1(NBSM), T2(NBSM)
      REAL*8  T3(NBFM), T4(NBFM), T5(NBFM)
      REAL*8  TMR(NBMM), TMI(NBMM)
      REAL*8  TMW1(NBMM),TMW2(NBMM),TMW3(NBMM),TMW4(NBMM),TMW5(NBMM)
      REAL*8  TMW6(NBMM),TMW7(NBMM),TMW8(NBMM),TMW9(NBMM),TMW10(NBMM)
      REAL*8  TMW11(NBMM),TMW12(NBMM)
C>>>END F77
C!>>>START F90
C      REAL*8, DIMENSION(:), ALLOCATABLE :: T1, T2
C      REAL*8, DIMENSION(:), ALLOCATABLE :: T3, T4, T5
C      REAL*8, DIMENSION(:), ALLOCATABLE :: TMR, TMI, TMW1,TMW2
C      REAL*8, DIMENSION(:), ALLOCATABLE :: TMW3,TMW4,TMW5,TMW6,TMW7
C      REAL*8, DIMENSION(:), ALLOCATABLE :: TMW8,TMW9,TMW10,TMW11,TMW12
C!>>>END F90

C 
C coefficient c^k(lm,l'm')
      REAL*8  RC(0:KMAX,0:LMAX,0:KMAX,0:LMAX,0:KMAX)
      COMMON /ANGL/RC 
C
C Bit functions
      INTEGER IBRN(0:2**NEMAX-1),ICNT(0:2**NEMAX-1)
      COMMON /BIT/IBRN,ICNT

      INTEGER I 
      INTEGER IUNIT
      INTEGER IERR
      COMMON /SYS/IERR
C

      OPEN(UNIT=IUOUT,FILE='xobj02')

      WRITE(IUOUT,'(A)')VERSION
      WRITE(6,'(A)')VERSION
C!>>>START F90
C      WRITE(IUOUT,'(A)')'  ("Progressive" edition with dynamical memory allocation)'
C      WRITE(6,'(A)')'  ("Progressive" edition with dynamical memory allocation)'
C!>>>END F90
C>>>START F77
      WRITE(IUOUT,'(A)')
     &  '  ("Legacy" edition with static memory allocation)'
      WRITE(6,'(A)')'  ("Legacy" edition with static memory allocation)'
C!>>>END F77


C>>>START SCALAR 
      WRITE(IUOUT,'(A)')'compiled for a scalar machine'
      WRITE(6,'(A)')'compiled for a scalar machine'
C>>>END SCALAR 
C      DO !Main Loop
      CALL WATCH('TOTAL   ')
      CALL WATCH('INIT    ')
      CALL CCOF(RC)
      CALL BITFN(IBRN,ICNT)
      CALL WATCH('END     ')
      IERR=0

C!>>>START F90
C      NHM=5000001
C      CALL GETSZ(NSTM,NOBT,NCIM,NHM)
C
C      ALLOCATE(NSFT(NCIM+1,NSTM),NELC(NOBT,NCIM,NSTM),ACI(NSTM,NCIM)&
C      ,ERED(NCIM),H(NHM),IHP1(NHM),IHP2(NHM))
C      WRITE(6,*)'Size of H=',SIZE(H)
C      WRITE(6,*)'Size of IHP1=',SIZE(IHP1)
C      WRITE(6,*)'Size of IHP2=',SIZE(IHP2)
C!>>>END F90

C-------------------------------
C  read the parameter file X-card
C-------------------------------
      CALL WATCH('RDCARD  ')
      CALL RDCD(MODE,NMODE,NSTATE
     &          ,NSTM,NCIM,NCI,NOBT,NOBTX,NELC,NDEG
     &          ,ASTS,ACI,AOBT
     &          ,NHOP,NHM,H,IHP1,IHP2,U,NDLT,DLT,EADJ
     &          ,GAM,GALF,NG,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &          ,RANGE,GALF2
     &          ,NREC
     &          ,NDMO,NDMG,NDMALF,MATSYB)
      CALL WATCH('END     ')
      CALL SFTS(NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      IF (IERR.NE.0) STOP
 
C      WRITE(2,'(5I10/)')((NSFT(IC,IS),IC=1,NCI(IS)+1),IS=1,NCI(0))
      WRITE(6,*)'NOBT&NOBTX= ',NOBT,' & ',NOBTX
        
      WRITE(6,*)'size of H'
      write(6,*)(nsft(nci(I)+1,i),i=1,nci(0))
      WRITE(6,*)'NHOP='
      WRITE(6,*)(NHOP(I),I=0,NHOP(0)+1)
C-------------------------------
C  Check the consistency in the X-card
C-------------------------------
      CALL CHKXCD(MODE,NMODE,NSTATE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM
     &           ,NDLT,DLT
     &           ,NGM,NG,NBGM,NBMM,NBFM,NBSM,IERR
     &           ,ASTS,ACI,AOBT)

C!>>>START F90
C      ALLOCATE(EG(NGM),UG(NGM,NBGM))
C      ALLOCATE(HD(NHDM),T1(NBSM),T2(NBSM),TMR(NBMM),TMI(NBMM))
C      ALLOCATE(T3(NBFM),T4(NBFM),T5(NBFM))
C      ALLOCATE(TMW1(NBMM),TMW2(NBMM),TMW3(NBMM),TMW4(NBMM),TMW5(NBMM))
C      ALLOCATE(TMW6(NBMM),TMW7(NBMM),TMW8(NBMM),TMW9(NBMM),TMW10(NBMM))
C      ALLOCATE(TMW11(NBMM),TMW12(NBMM))
C!>>>END F90

      IF (IERR.NE.0) THEN
        CALL SSTAT(NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NHOP
     &            ,NGM,NG,NHM,NHDM,NBGM,NBMM,NBFM,NBSM,NMODE,MODE
     &            ,ASTS,ACI,AOBT)
        STOP
      END IF
C-------------------------------
C Calculations for the initial state
C-------------------------------
      IF (MODE.NE.'dos') THEN
        CALL GRND(NSTATE,MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG,ORBM
     &           ,NBSM,T1,T2
     &           ,ASTS,ACI,AOBT )
      END IF

      IF (IERR.NE.0) THEN
        CALL SSTAT(NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NHOP
     &            ,NGM,NG,NHM,NHDM,NBGM,NBMM,NBFM,NBSM,NMODE,MODE
     &            ,ASTS,ACI,AOBT)
        STOP
      END IF

C  jump to required photoprocess
C-------------------------------
C Ground State Properties
C ------------------------------
      IF (MODE.EQ.'nop'.OR.MODE.EQ.'grnd') THEN

        CALL GOPRT(NSTATE,NSTATE+1,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &            ,NGM,NBGM,NG,EG,UG,NDCR,CDCR
     &            ,NBSM,T1,T2, IUNIT)

      ELSE IF  (MODE.EQ.'zl') THEN

        CALL ZL   (MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,U,NDLT,DLT,EADJ,ERED
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR
     &            ,NREC
     &            ,NBSM,T1,T2 )

C-------------------------------
C First Order Optical Process
C-------------------------------

      ELSE IF (MODE.EQ.'pes') THEN 

        CALL PES (MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &           ,RANGE,SPR,SPI,SPD
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'ipes') THEN

        CALL IPES(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &           ,RANGE,SPR,SPI,SPD
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'xas') THEN

        CALL XAS (MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'xasr') THEN

        CALL XASR(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF  (MODE.EQ.'opcnd') THEN

        CALL OPCND(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'xrs') THEN

        CALL XRS (MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'xrsq') THEN

        CALL XRSQ(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,BR, NBSM,T1,T2 )

      ELSE IF (MODE.EQ.'dos') THEN

        CALL DOS (NSTATE,MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI,SPU,SPD
     &           ,NREC,AR,BR, NBSM,T1,T2 )

C>>>START RESONANCE
C-------------------------------
C Second Order Optical Process
C-------------------------------

      ELSE IF (MODE.EQ.'rxas' .OR. MODE.EQ.'rxasf'
     &    .OR. MODE.EQ.'rxasg'.OR. MODE.EQ.'rxasfg') THEN

        CALL RXAS(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,AI,BR,BI
     &           ,NBSM, T1,T2, NBMM,TMR,TMI,TMW1,TMW2,TMW3)

      ELSE IF (MODE.EQ.'rpes' .OR. MODE.EQ.'rpesf'
     &    .OR. MODE.EQ.'rpesg'.OR. MODE.EQ.'rpesfg') THEN

        CALL RPES(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &           ,RANGE,GALF2,SPR,SPI,SPD
     &           ,NREC,AR,AI,BR,BI
     &           ,NBSM,T1,T2, NBFM,T3,T4,T5, NBMM,TMR,TMI,TMW1,TMW2
     &           ,TMW3,TMW4,TMW5,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &           ,ASTS,ACI,AOBT)

      ELSE IF (MODE.EQ.'tipes') THEN

        CALL TIPES(MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,U,NDLT,DLT,EADJ,ERED
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR
     &            ,NBSM,T1,T2, NBMM,TMR,TMI )

      ELSE IF (MODE.EQ.'ripes'.OR. MODE.EQ.'ripesf'
     &    .OR. MODE.EQ.'aps'  .OR. MODE.EQ.'apsf'  ) THEN

        CALL RIPES(MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR
     &            ,NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2,TMW3,TMW4,TMW5
     &            ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &            ,ASTS,ACI,AOBT)
  
      ELSE IF (MODE.EQ.'xes'.OR.MODE.EQ.'xesf') THEN

        CALL  XES (MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR
     &            ,NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2,TMW3,TMW4,TMW5
     &            ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &            ,ASTS,ACI,AOBT)
 
C      ELSE IF (MODE.EQ.'modxes'.OR.MODE.EQ.'modxesf') THEN
C
C        CALL  MODXES(MODE,NMODE
C     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
C     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &            ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
C     &            ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
C     &            ,RANGE,SPR,SPI,SPD
C     &            ,NREC,AR,BR
C     &            ,NBSM,T1,T2, NBMM,TMR,TMI )
C
      ELSE IF (MODE.EQ.'mltp') THEN

        CALL EXMLTP(NSTATE,MODE,NMODE
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,U,NDLT,DLT,EADJ,ERED
     &             ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
     &             ,NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2
     &             ,ASTS,ACI,AOBT)
 
C>>>END RESONANCE
      ELSE

        WRITE(6,*)'unknown mode ==>',MODE
        STOP

      END IF
C
C get statistical information of the system
C
      CALL WATCH('END     ')
      CALL SSTAT(NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NHOP
     &          ,NGM,NG,NHM,NHDM,NBGM,NBMM,NBFM,NBSM,NMODE,MODE
     &          ,ASTS,ACI,AOBT)

C      END DO !Main Loop

      END PROGRAM XTLS

C=================================================
C This routine evaluates values of the parameters
C NSTM, NOBT, NCIM and NHM from X-card for 
C fortran 90 edition 
C=================================================
      SUBROUTINE GETSZ(NSTM,NOBT,NCIM,NHM)
      IMPLICIT NONE

C Parameters
C --Unit number for Input
      INTEGER   IUNIT 
      PARAMETER(IUNIT=1)

      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM 
      PARAMETER(NOBTM=120)

      INTEGER   ITXMAX,    LBLM
      PARAMETER(ITXMAX=128,LBLM=32)

      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER LIN, IERR, ITYP

C Dummy arguments
      INTEGER NSTM, NOBT, NCIM
      INTEGER NHM
 
C Local variables 
      CHARACTER*(NSTM2) ASTS
      INTEGER I, ICI, IT
      REAL*8  DNUM2
C -------------------------------------------

      LIN=0
      OPEN(UNIT=IUNIT,FILE='xcard01',STATUS='OLD')
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      IF ( AWRD == 'xcrd' .AND. DLM == ':' ) THEN
      
        WRITE(6,*)'enter xcrd: line=',LIN
       
        DO WHILE( AWRD /= 'cnfg' .OR. DLM /= ':')

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          WRITE(6,*)AWRD
          IF ( DLM == '(' ) THEN

            I=0
            DO !until I=0
              IF ( DLM == '(' ) THEN
                I=I+1
              ELSE IF ( DLM == ')' ) THEN
                I=I-1
              END IF
              IF (I.EQ.0) EXIT
              IF (IERR.EQ.3) THEN
                WRITE(6,*)'end of file!'
                RETURN
              END IF
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            END DO !until I=0

          ELSE IF( AWRD == 'nhm' .AND. DLM == '=' ) THEN

            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            NHM = NINT( DNUM2(NWRD, AWRD, IT ) )

          ELSE IF( AWRD == 'line' .AND. DLM == '=' ) THEN

            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            LIN = NINT( DNUM2(NWRD, AWRD, IT ) )

          ELSE IF( AWRD /= 'cnfg' .OR. DLM /= ':' ) THEN

            WRITE(6,*)'Error! Unknown command in the item XCRD:'
            STOP

          END IF 
        END DO

      ELSE

        WRITE(6,*)'Error! item "XCRD:" expected !! at line=',LIN
        STOP

      END IF

      IF ( AWRD == 'cnfg' .AND. DLM == ':' ) THEN
      
        WRITE(6,*)'enter cnfg: line=',LIN
         
        ICI =0
        NSTM=3
        NOBT=0
        NCIM=13000
        ASTS=' '

! count number of orbital labels
        DO WHILE( DLM /= '#' .AND. IERR /= 3 )
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          IF ( DLM == '=' ) THEN
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          END IF
          IF ( DLM == '/ ' ) THEN
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          END IF
          IF( DLM == ' ' .OR. DLM == '#' )THEN
            NOBT=NOBT+1
          ELSE 
            WRITE(6,*)'Error! Wrong format in orbital lebel' 
     &      // ' in the item "CNFG:"'
          END IF
        END DO

! count numbers of configuration labels and states
        DO WHILE( DLM == '# ' .AND. IERR /= 3 )
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          IF ( INDEX( ASTS, AWRD(1:1) ) /= 0 ) THEN
            ICI=ICI+1 
            NCIM=MAX(NCIM,ICI)
          ELSE
            NSTM=NSTM+1
            ASTS(NSTM:NSTM)=AWRD(1:1)
            NCIM=MAX(NCIM,ICI)
            ICI=1
          END IF
          DO I=1, NOBT
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          END DO
        END DO
      ELSE IF( AWRD /= 'para' .OR. DLM /= ':' ) THEN

        WRITE(6,*)'Error! item "PARA:" expected !! at line=',LIN
        STOP

      END IF

      NCIM=NCIM+2  
      WRITE(6,*)'NSTM=',NSTM
      WRITE(6,*)'NOBT=',NOBT
      WRITE(6,*)'NCIM=',NCIM

C!>>>START F90
C      IF (NSTM.GT.NSTM2) THEN
C        WRITE(6,*)'Error! parameter "NSTM2" must be enhanced: NSTM2 >=',NSTM
C        STOP
C      END IF
C      IF (NOBT.GT.NOBTM) THEN
C        WRITE(6,*)'Error! parameter "NOBTM" must be enhanced: NOBTM >=',NOBT
C        STOP
C      END IF
C!>>>END F90
 
      END SUBROUTINE GETSZ 


C==============================================================
C This subroutine examines the consistency in the X-card.
C For fortran 90 edtion, it also evaluates values of the parameters 
C NGM, NBGM, NBMM, NBFM and NBSM.
C==============================================================
      SUBROUTINE CHKXCD(MODE,NMODE,NSTATE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM
     &           ,NDLT,DLT
     &           ,NGM,NG,NBGM,NBMM,NBFM,NBSM,IERR
     &           ,ASTS,ACI,AOBT)
      IMPLICIT NONE

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE, NSTATE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
     
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
    
      INTEGER NGM, NG, NBGM, NBMM, NBFM, NBSM, IERR

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER I,ISHL,NSHL,NE1,NE2,N,NEXL
      INTEGER H_i, H_m, H_f, H_a, T_A, T1_A
      REAL*8  T
      LOGICAL AUGER

C Function
      INTEGER NTOLEL
C ----------------------------------
C!>>>START F90
C      NHDM=500001
C      NBGM=100001
C      NBMM=1
C      NBFM=1
C      NBSM=500001
C!>>>END F90
       
      NEXL=MOD(NMODE/10,10)
      IERR=0
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  NOP, GRND, ZL or OPCND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      IF (    MODE.EQ.'nop' .OR. MODE.EQ.'grnd'
     &   .OR. MODE.EQ.'zl'  .OR. MODE.EQ.'opcnd'  ) THEN
        H_i =1
        H_f =1  ! Op.# for the final state Hamiltoniam
        NSHL=1
        IF (NCI(0).LT.1) THEN
          WRITE(6,*)'Error! The number of Hamiltonian must be one'
          IERR=1
        ELSE IF( ( MODE.EQ.'opcnd' .AND. NHOP(0).NE.3 )
     &      .OR. ( MODE.EQ.'zl'    .AND. NHOP(0).NE.3 ) ) THEN
          WRITE(6,*)'Error! The number of operator is incorrect.'
          IERR=1
        END IF
        CALL ERR_BSZ( H_i, NBGM,'NBGM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C PES, IPES, XAS, XASR, XASQ or XRS 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ELSE IF ( MODE.EQ.'pes' .OR. MODE.EQ.'ipes' .OR. MODE.EQ.'xas'
     &     .OR. MODE.EQ.'xasr'.OR. MODE.EQ.'xasq' .OR. MODE.EQ.'xrs'
     &  ) THEN

        H_i =1  ! Op.# for the initial state Hamiltoniam
        H_f =2  ! Op.# for the final state Hamiltoniam
        NSHL=1

        IF ( NCI(0).NE.H_f 
     &  .AND. .NOT.( NCI(0).EQ.1 .AND. MODE.EQ.'opcnd' ) ) THEN
          WRITE(6,*)'Error! The number of Hamiltonian is incorrect.'
          IERR=1
        ELSE IF( NHOP(0).NE.3 ) THEN
          WRITE(6,*)'Error! The number of operator is incorrect.'
          IERR=1
        END IF

        NE1=NTOLEL( H_i ,NSTM,NCIM,NCI,NOBT,NELC)
        NE2=NTOLEL( H_f ,NSTM,NCIM,NCI,NOBT,NELC)
        N=0
        IF (MODE.EQ.'pes')  N=-1 
        IF (MODE.EQ.'ipes') N= 1
        IF (NE1+N.NE.NE2) THEN
          WRITE(6,*)'Wrong number of electrons for the mode: '//MODE
          WRITE(6,*)'The number in the initial state is ',NE1
          WRITE(6,*)'whereas that in the final state is ',NE2
          IERR=1
        END IF
        CALL ERR_BSZ( H_i, NBGM,'NBGM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C RPES, RPESF, RPESG, RPESFG 
C RXAS, RXASF, RXASG, RXASFG modes
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ELSE IF (MODE.EQ.'rpes' .OR. MODE.EQ.'rpesf'
     &    .OR. MODE.EQ.'rpesg'.OR. MODE.EQ.'rpesfg'
     &    .OR. MODE.EQ.'rxas' .OR. MODE.EQ.'rxasf'
     &    .OR. MODE.EQ.'rxasg'.OR. MODE.EQ.'rxasfg') THEN

        IF (INDEX(MODE,'g').NE.0) THEN
          NSHL=(NCI(0)-2)/2
        ELSE 
          NSHL=NCI(0)-2
        END IF

        H_i =1  ! Op.# for the initial state Hamiltoniam
        H_m =2  ! Op.# for the intermediate state Hamiltoniam
        H_f =3  ! Op.# for the final state Hamiltoniam
        H_a =2+NSHL+1  ! Op.# for the Auger state
        WRITE(6,*)'NSHL=',NSHL

C --number of the operators
        IF ( NCI(0) .LT. H_f ) THEN
          WRITE(6,*)'Error! too few states for RXPS.'
          IERR=1
        ELSE IF((3*NSHL+H_f.NE.NHOP(0).AND.INDEX(MODE,'g').EQ.0)
     &      .OR.(4*NSHL+H_f.NE.NHOP(0).AND.INDEX(MODE,'g').NE.0))THEN
          WRITE(6,*)'Error! The number of operators is incorrect.'
          IERR=1
        END IF

C --number of the electrons
        NE1=NTOLEL( H_i ,NSTM,NCIM,NCI,NOBT,NELC)
        NE2=NTOLEL( H_m ,NSTM,NCIM,NCI,NOBT,NELC)
        IF ( NE2 .NE. NE1 ) THEN
          WRITE(6,*)
     & 'Error! Wrong electron number as for the intermediate state #'
     &    //ASTS(2:2)//' Ne=',NE2
          IERR=1
        END IF
        DO ISHL=1, NSHL
          I=H_f + ISHL-1 
          NE2=NTOLEL( I,NSTM,NCIM,NCI,NOBT,NELC)
          IF ( NE2 .NE. NE1-1 ) THEN
            WRITE(6,*)
     &      'Error! Wrong electron number as for the final state #'
     &      //ASTS(I:I)//' Ne=',NE2
            IERR=1
          END IF
        END DO

        IF ( INDEX(MODE,'g') .NE.0 ) THEN
          DO ISHL=1,NSHL
            I=H_a + ISHL-1 
            NE2=NTOLEL( I ,NSTM,NCIM,NCI,NOBT,NELC)
            IF ( NE2 .NE. NE1-2 ) THEN
              WRITE(6,*)
     &        'Error! Wrong electron number as for the Auger state #'
     &        //ASTS(I:I)//' Ne=',NE2
              IERR=1
            END IF
            CALL ERR_BSZ( H_a + ISHL-1, NBFM,'NBFM'
     &                  ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
          END DO
        END IF 

C -- number of basis functions
        CALL ERR_BSZ( H_i, NBGM,'NBGM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
        CALL ERR_BSZ( H_m, NBMM,'NBMM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  RIPES, RIPESF, AP, APSF or TIPES modes 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ELSE IF (MODE.EQ.'ripes'.OR. MODE.EQ.'ripesf'
     &    .OR. MODE.EQ.'aps'  .OR. MODE.EQ.'apsf'  
     &    .OR. MODE.EQ.'tipes') THEN

        H_i =1
        H_m =2 
        H_f =3

        IF ( NCI(0) .LT. H_f ) THEN
          WRITE(6,*)'Error! Too few states for RIPES/RIPESf mode' 
          IERR=1
        END IF

C --number of the electrons
        NE1=NTOLEL( H_i ,NSTM,NCIM,NCI,NOBT,NELC)
        NE2=NTOLEL( H_m ,NSTM,NCIM,NCI,NOBT,NELC)
        IF ( NE2 .NE. NE1+1 ) THEN
          WRITE(6,*)
     & 'Error! Wrong electron number as for the intermediate state #'
     &   //ASTS(2:2)//' Ne=',NE2
          IERR=1
        END IF

        NSHL=NCI(0)-3 

        AUGER=.TRUE.
        NE2=NTOLEL( NCI(0) ,NSTM,NCIM,NCI,NOBT,NELC)
        IF ( NE2 .EQ. NE1+1 ) THEN 
          WRITE(6,*)
     & 'The initial state is assumed to be identical to the Auger state'
          NSHL=NSHL+1
          AUGER=.FALSE.
        END IF

        WRITE(6,*)'NSHL=',NSHL

        DO ISHL=1, NSHL
          I=H_f + ISHL-1
          NE2=NTOLEL( I ,NSTM,NCIM,NCI,NOBT,NELC)
          IF ( NE2 .NE. NE1+1 ) THEN
            WRITE(6,*)
     &      'Error! Wrong electron number as for the final state #'
     &      //ASTS(I:I)//' Ne=',NE2
            IERR=1
          END IF
        END DO

C -- number of the operators
        T_A =2+NSHL+NSHL+NSHL+1         ! the Auger state A
        T1_A=2+NSHL+NSHL+NSHL+1+NSHL+1  ! the Auger state A'
        IF (.NOT.AUGER) T1_A=T_A 

        IF (.NOT.( AUGER .AND. NHOP(0).EQ.T1_A 
     &        .OR. AUGER .AND. NHOP(0).EQ.T_A )  ) THEN
          WRITE(6,*)
     &    'Error! Wrong number of operators for RIPES or RIPESf mode.'
          IERR=1
        END IF

C -- number of the basis functions
        CALL ERR_BSZ( H_i, NBGM,'NBGM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
        CALL ERR_BSZ( H_m, NBMM,'NBMM'
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  XES, XESF mode
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ELSE IF (MODE.EQ.'xes'.OR.MODE.EQ.'xesf') THEN

        H_i=1
        H_m=2
        H_f=3

C --number of the states
        IF ( NCI(0) .LT. 2 ) THEN
          WRITE(6,*)'Error! Too few states for the xes or xesf mode.'
          IERR=1
        ELSE
          NSHL=NCI(0)-2
        END IF
        IF( MODE.EQ.'xesf' ) NSHL=NCI(0)-3
        IF (NSHL.EQ.0) H_f=H_i

C --number of the operator
        IF( MODE.EQ.'xesf' ) THEN
          IF ( NHOP(0) .NE. 2+NSHL+1+NSHL+2+NSHL ) THEN
            WRITE(6,*)
     &      'Error! Wrong number of the operators for XESf mode'
            IERR=1
          END IF
        ELSE IF( NHOP(0).NE.2+NSHL+1+NSHL ) THEN
          WRITE(6,*)
     &   'Error! The number of the operators is incorrect for XES mode'
          IERR=1
        END IF

C --number of the electrons
        NE1=NTOLEL( H_i ,NSTM,NCIM,NCI,NOBT,NELC)
        NE2=NTOLEL( H_m ,NSTM,NCIM,NCI,NOBT,NELC)
        IF ( NE2 .NE. NE1 ) THEN
          WRITE(6,*)
     &'Error! Wrong electron number as for the intermediate state #'
     &    //ASTS(2:2)//' Ne=',NE2
          IERR=1
        END IF
        DO ISHL=1, NSHL
          I=H_f + ISHL-1
          NE2=NTOLEL( I ,NSTM,NCIM,NCI,NOBT,NELC)
          IF ( NE2 .NE. NE1 ) THEN
            WRITE(6,*)
     &      'Error! Wrong electron number as for the final state #'
     &      //ASTS(I:I)//' Ne=',NE2
            IERR=1
          END IF
        END DO
        IF (MODE.EQ.'xesf') THEN
          I=NCI(0) 
          NE2=NTOLEL( I ,NSTM,NCIM,NCI,NOBT,NELC)
          IF ( NE2 .NE. NE1-1 ) THEN
            WRITE(6,*)
     &      'Error! Wrong electron number as for the Auger state #'
     &      //ASTS(I:I)//' Ne=',NE2
            IERR=1
          END IF
        END IF
C --number of the basis function
        CALL ERR_BSZ( H_i, NBGM, 'NBGM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
        CALL ERR_BSZ( H_m, NBMM, 'NBMM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  MLTP or DOS mode 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ELSE IF ( MODE.EQ.'mltp' .OR. MODE.EQ.'dos' ) THEN
        IF (NSTATE.LE.0) THEN
          WRITE(6,*)'The state should be defined by "State" command.'
          IERR=1
        END IF
        IF (MODE.EQ.'mltp') THEN
          CALL ERR_BSZ(NSTATE, NBGM, 'NBGM '
     &                ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
          CALL ERR_BSZ(NSTATE, NBMM, 'NBMM '
     &                ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
        END IF
      ELSE

        WRITE(6,*)'unknown mode ==>',MODE
        STOP

      END IF

C -- number of basis function 
      DO I=1,NCI(0)
        CALL ERR_BSZ( I, NBSM,'NBSM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
      END DO
      IF (NEXL.EQ.0) THEN
        DO I=1,NCI(0)
          CALL ERR_BSZ( I, NHDM,'NHDM '
     &                ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
        END DO
      ELSE
C>>>START F77
        IF (NHDM.LT.NCIM) THEN
          WRITE(6,*)
     &    'The parameter NHDM must equal or larger than NCIM'
          IERR=1
        END IF
C>>>END F77
C!>>>START F90
C        NHDM=NCIM
C!>>>END F90
      END IF

      IF (MODE.EQ.'mltp') THEN
        N=NSFT( NCI(NSTATE)+1, NSTATE )
        NG=1
      ELSE
        N=NSFT( NCI(H_i)+1, H_i )
      END IF

C!>>>START F90
C      NGM=MIN(NG,N)
C!>>>END F90

      N=NOBT
C>>>START F77
      N=NOBTM 
C>>>END F77
C -- calcuate required memory
      T=DBLE(NHM)*2d0+DBLE(NHDM)+DBLE(NBSM)*2d0+DBLE(NGM)*DBLE(NBGM)
     & +DBLE(NBMM)*14d0+DBLE(NBFM)*3d0
     & +0.5d0*(DBLE(N)+1d0+DBLE(NLBLM)/8d0)*DBLE(NCIM)*DBLE(NSTM)
     & +DBLE(NPMAX)*4d0+DBLE(NRECM)*4d0
      T=T*8d0/1024d0**2 + 7d0
      IF ( T .GT. 1024D0 ) THEN
        T=T/1020D0
        WRITE(6,'(A,F10.3,A)')
     &   'Memory required for calculations: ',T,' gigabyte'
      ELSE
        WRITE(6,'(A,F7.1,A)')
     &   'Memory required for calculations: ',T,' megabyte'
      END IF

      IF (IERR.NE.0) RETURN

C -- check for interactions 
      CALL CHKBLK(NSTM,NCIM,NCI,NOBT,NOBTX,NELC,NDEG 
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM
     &           ,NDLT,DLT, H_f+NSHL-1, NMODE,IERR
     &           ,ASTS,ACI,AOBT)

      IF (IERR.NE.0) RETURN

      END SUBROUTINE CHKXCD

C=============================================================
C Ground State Properties
C=============================================================
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Solve low-energy eigenvalues and eigenvectors
C for a state NST
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GRND(NST,MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG,ORBM
     &           ,NBSM, T1,T2
     &           ,ASTS,ACI,AOBT)
      IMPLICIT NONE

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      INTEGER NST

      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      INTEGER NBSM 
      REAL*8  T1(NBSM), T2(NBSM)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER I,IG,NHTOP 
C
      INTEGER IERR,IO,N1
      COMMON /SYS/IERR
C
      INTEGER IOU(2)
      DATA IOU/2,6/
C -----------------------
C
      IERR=0
C
      IF (NST.EQ.0) THEN
        WRITE(6,'(A)')'The state #'//ASTS(1:1)//
     &         ' is assumed to be the initial state'
        NST=1
      END IF
C
      IF (NCI(0).GE.1) THEN   
        N1=NSFT(NCI(NST)+1,NST)
        CALL ERR_BSZ(NST,NBGM,'NBSM '
     &              ,NSTM,NCIM,NCI,NSFT,IERR,ASTS,ACI,AOBT)
      ELSE
        WRITE(6,*)'Error! No operator found for the initial state.'
        IERR=1
      END IF
      IF (NBGM.GT.NBSM) THEN
        WRITE(6,*)'Error! Too large value for NBGM  (NBGM <= NBSM) '
        IERR=1
      END IF
      IF (NG.GT.NGM) THEN
        WRITE(6,'(A,I9)') 
     &    'Error! The number of the eigen states Ninit=',NG
        WRITE(6,'(A,I9)')' exceed the maximum value NGM=',NGM
        IERR=1
      ELSE 
        WRITE(6,'(A,I9)')'The number of the eigen states: Ninit=',NG
        WRITE(6,'(A,I9)')'         The maximum value: NGM=',NGM
      END IF
C
      IF (IERR.NE.0) RETURN 
C
      NHTOP=IHP1(1)
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(NST,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
      IF (MODE.NE.'dos') THEN
        CALL WATCH('EIGN    ')
        CALL EIGN(NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,NSTM,NCIM,NCI,NSFT
     &           ,NGM,NBGM,NG,UG,EG,NBSM,T1,T2,NMODE)
        IF (IERR.NE.0) STOP
        CALL WATCH('END     ')
      END IF
      IHP1(1)=NHTOP
      IHP2(1)=NHTOP
C
C get information from initial state
      IF (NG.NE.0) THEN
        DO IG=1,NG
          DO I=1,2
            IO=IOU(I)
            WRITE(IO,*)'====== state #'//ASTS(NST:NST)//' ========='
            WRITE(IO,*)'  #',IG,'  E=',EG(IG)
          END DO
          CALL MAGT(NST,ORBM
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &             ,NBGM,UG(1,IG),NMODE, NBSM,T1,T2
     &             ,ASTS,ACI,AOBT)
        END DO
C
        WRITE(6,*)'---------- configuration mixing ---------'
        WRITE(2,*)'---------- configuration mixing ---------'
        DO IG=1,NG
          WRITE(6,*)'  #',IG,'  E=',EG(IG)
          WRITE(2,*)'  #',IG,'  E=',EG(IG)
          CALL CNFMIX(NST,NSTM,NCIM,NCI,NSFT,UG(1,IG),ASTS,ACI,AOBT)

        END DO 
        IHP1(1)=NHTOP
        IHP2(1)=NHTOP
        CALL CHRD(NST,ORBM,NGM,NBGM,NG,EG,UG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,NMODE,NBSM,T1,T2,0
     &           ,ASTS,ACI,AOBT)
        IHP1(1)=NHTOP
        IHP2(1)=NHTOP
      END IF
      IHP1(1)=NHTOP
      IHP2(1)=NHTOP
C
      END SUBROUTINE GRND

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   The expectation values of operators NOP specified by 
C   the "Goprt" command in the X-card for a state NST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GOPRT(NST,NOP,NMODE
     &                ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &                ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &                ,NGM,NBGM,NG,EG,UG,NDCR,CDCR
     &                ,NBSM,T1,T2,IUNIT)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NDCRM
      PARAMETER(NDCRM=1200)

C Dummy arguments
      INTEGER NST, NOP

      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
  
      INTEGER NBSM 
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER K,IG,IT,IFAC,ITP,ISB,N,IUNIT
      INTEGER NSIZE,NTYPE,LIN,IERR,NOPRT,NCPX,NEXL 
      REAL*8 SR,SI

      INTEGER NARGM
      PARAMETER(NARGM=2000)
      REAL*8  ARG(NARGM)
      CHARACTER*(32) ASYMB
      CHARACTER*(26) ALPHABET
C
      CHARACTER*(8) COPRT
      CHARACTER*(1) C
      LOGICAL HIT
C -------------------------------------
C
      ALPHABET='abcdefghijklmnopqrstuvwxyz' 
C
      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C
      ASYMB='__goprt'
      CALL X_CODE(3,IUNIT,ASYMB,NSIZE,NTYPE,ARG,NARGM,LIN,IERR)
      IF (IERR.NE.0) RETURN
C
      CALL HGEN(NBSM,T1,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,10+NCPX)
C
      N=NSFT(NCI(NST)+1,NST)
      WRITE(2,*)
     &  '     ------ Expectation values of the operators ------     '
      DO IG=1,NG
        IFAC=1
        ITP=0
        ISB=0
        NOPRT=0
        WRITE(2,*)'  #',IG,'  E=',EG(IG)
        
        DO IT=1,NSIZE
          C=CHAR( NINT(ARG(IT)) )
          K=INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',C)
          IF (K.GT.0) C=ALPHABET(K:K) 
          IF (INDEX('*; ',C).LE.0) THEN
            NOPRT=NOPRT+1
            COPRT(NOPRT:NOPRT)=C
            ITP=ITP+1
            ASYMB(ITP:ITP)=C
          END IF
          IF (C.EQ.'*'.OR.C.EQ.';'.OR.IT.EQ.NSIZE) THEN
            HIT=.FALSE.
            DO ISB=1,NDCR(1)
              HIT= COPRT(1:NOPRT) .EQ. CDCR(ISB)
              IF (HIT) EXIT        
            END DO
            IF ( .NOT. HIT ) THEN
              WRITE(6,*)'A wrong operator is specified in Goprt: '
     &                //COPRT(1:NOPRT)
              STOP
            END IF
            NOPRT=0
            IF (IFAC.EQ.1) THEN
              CALL CLRV(N,T1)
              CALL OPRT(N,T1,N,UG(1,IG),NOP,0,0,ISB-1
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              WRITE(6,*)'IFAC=1,N,ISB,NOP,IG=',N,ISB,NOP,IG
            ELSE
              CALL MOVV(N,T2,T1)
              CALL CLRV(N,T1)
              CALL OPRT(N,T1,N,T2,NOP,0,0,ISB-1,NHOP,NHM,H,IHP1,IHP2)
            END IF
            IF (C.EQ.';'.OR.IT.EQ.NSIZE) THEN
              CALL MULCV (N,T1,UG(1,IG),SR,SI,NCPX)
              WRITE(6,*)ASYMB(1:ITP),'=',SR,SI
              WRITE(2,*)ASYMB(1:ITP),'=',SR,SI
              IFAC=1
              ITP=0
            ELSE
              IFAC=IFAC+1
              ITP=ITP+1
              ASYMB(ITP:ITP)=C
            END IF
          END IF
        END DO !IT
        WRITE(2,*)
     &  '----------------------------------------------------------'
      END DO !IG

      END SUBROUTINE GOPRT 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   ZL 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ZL(MODE,NMODE
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,U,NDLT,DLT,EADJ,ERED
     &             ,NGM,NBGM,NG,EG,UG
     &             ,ORBM,EPH,EANG,NDCR,CDCR
     &             ,NREC
     &             ,NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000)
      REAL*8 PI
      PARAMETER(PI=3.141592653897932D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      INTEGER NREC

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IOP,IG,IREC,N1,NCPX,NEXL
      REAL*8  SR, SI
C -----------------------

      N1=NSFT(NCI(1)+1,1)

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C
      CALL FLHEAD(MODE,NMODE, 1, NDCR, CDCR, 0 ,-1 ,1
     &           ,NG,EG,EPH,EANG)
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=NCI(0)+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
C main
C
      IF (NCPX.EQ.0) THEN
        WRITE(6,*)'Error!! Must be complex mode'
        STOP
      END IF
      DO IG=1,NG
        CALL MOVV(N1,T1,UG(1,IG))
        DO IREC=1,NREC
          CALL CLRV(N1,T2)
          CALL WATCH('OPRT2   ')
          CALL OPRT(N1,T2,N1,T1,2,0,0,IREC,NHOP,NHM,H,IHP1,IHP2)
          CALL OPRT(N1,T2,N1,T1,2,0,0,IREC,NHOP,NHM,H,IHP2,IHP1)
          CALL WATCH('END     ')
          CALL MADCS(N1,T2,T1
     &              ,COS( 2D0*PI* DBLE(IREC)/DBLE(NREC) )-1D0
     &              ,SIN( 2D0*PI* DBLE(IREC)/DBLE(NREC) )  
     &              ,NCPX)
        END DO
        CALL MULCV(N1,T1,UG(1,IG),SR,SI,NCPX)
        WRITE(2,*)'ZJ='
        WRITE(2,*)SR,SI
      END DO !IG

      END SUBROUTINE ZL

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   PES (PHOTOEMISSION SPECTRA)
C  The operators in your X-card should be appeared in an order:
C  H_i, H_f, T_D.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE PES (MODE,NMODE
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,U,NDLT,DLT,EADJ,ERED
     &               ,NGM,NBGM,NG,EG,UG
     &               ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &               ,RANGE,SPR,SPI,SPD
     &               ,NREC,AR,BR, NBSM, T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)
      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NSPN(2)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IEANG,NEANG,IG,IDCR,NDCR1,IS
      INTEGER IO,IO1,IEX,IEX1,NPX
      INTEGER LEX,NSDEG
      INTEGER NE,N1,N2,NR
      INTEGER H_i, H_f, T_D 
      REAL*8 E, YRE, YIM

C Function
      INTEGER NTOLEL
C -----------------------
      H_i=1
      H_f=2
      T_D=3

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_f)+1, H_f )

      CALL RMBLK( H_i,NHOP,NHM,H,IHP1,IHP2)

      CALL NSUBS(NDCR1, T_D, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,NSPN(1),NSPN(2),1
     &           ,NG,EG,EPH,EANG)
C
C creation of the final state Hamiltonian
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV( H_f, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C creation of transition matrices  
C
      CALL WATCH('TRNSMAT ')
      CALL HGEN(NBSM,T1, T_D, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL WATCH('END     ')
C
C main loop
C
      NEANG=NINT( EANG(0) )
      NPX  =NINT( RANGE(1,1) )
      DO IEANG=1,MAX(ABS(NEANG),1)
      DO IDCR=0,NDCR1-1
      DO IG=1,NG
C------------------------------------------
C With "LMlim" command in the item "EXEC:"
C------------------------------------------
        IF (NEANG.NE.0) THEN
          DO IS=NSPN(1),NSPN(2)
            CALL CLRV(N2,T1)
            IF (NEANG.LT.0) THEN

              IO=NINT(EANG(IEANG))/100
              IEX=MOD(NINT(EANG(IEANG)),100)
              IF (MOD(NDEG(IO),2).EQ.1) THEN
                LEX=NDEG(IO)/2
                NSDEG=1
              ELSE IF (MOD(NDEG(IO),4).EQ.2) THEN
                LEX=NDEG(IO)/4
                NSDEG=2
              ELSE
                WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                STOP
              END IF
              IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                CALL OPRT(N2,T1,N1,UG(1,IG),T_D,IO,IEX,  IDCR
     &                   ,NHOP,NHM,H,IHP1,IHP2)
              END IF
              IF ( IS/NSDEG .EQ. 1 ) THEN
                CALL OPRT(N2,T1,N1,UG(1,IG),T_D,IO,IEX+1,IDCR
     &                   ,NHOP,NHM,H,IHP1,IHP2)
              END IF
              WRITE(6,*)
     &          'L=',LEX,' M=',IEX/NSDEG-LEX,' UDbit=',IS
              WRITE(2,*)
     &          'L=',LEX,' M=',IEX/NSDEG-LEX,' UDbit=',IS
C------------------------------------------
C With "Eang" command in the item "EXEC:"
C------------------------------------------
            ELSE
              DO IO=NOBT+1,NOBTX

                IF (MOD(NDEG(IO),2).EQ.1) THEN
                  LEX=NDEG(IO)/2
                  NSDEG=1
                ELSE IF ( MOD(NDEG(IO),4).EQ.2) THEN
                  LEX=NDEG(IO)/4
                  NSDEG=2
                ELSE
                  WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                  STOP
                END IF

                DO IEX=0,NDEG(IO)-1,NSDEG
                  CALL Y( LEX, IEX/NSDEG - LEX
     &                  , EANG(2*IEANG-1), EANG(2*IEANG), YRE, YIM )
                  WRITE(6,*)'L=',LEX,' M=', IEX/NSDEG - LEX
                  WRITE(6,*)'Yreal=',YRE
                  WRITE(6,*)'Yimag=',YIM
                  WRITE(6,*)' TH=',EANG(2*IEANG-1),' PY=',EANG(2*IEANG)
                  IF ( MOD( LEX, 2 ) .EQ.1) THEN
                    YRE=-YRE
                    YIM=-YIM
                  END IF
                  IO1=IO
                  IEX1=IEX
                  CALL CLRV(N2,T2)
                  IF ( MOD(IS,NSDEG ).EQ.1 ) THEN
                    CALL OPRT(N2,T2,N1,UG(1,IG),T_D,IO1,IEX1,  IDCR
     &                       ,NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF (  IS/NSDEG   .EQ. 1 ) THEN
                    CALL OPRT(N2,T2,N1,UG(1,IG),T_D,IO1,IEX1+1,IDCR
     &                       ,NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF (MOD(NMODE,10).NE.0) THEN
                    CALL EXCV(N2,T1,T2,-YIM)
                  END IF
                  CALL MULS(N2,T2,YRE)
                  CALL ADDV(N2,T1,T2)
                END DO !IEX
              END DO !IO
            END IF

            CALL WATCH('LANCZ   ')
            CALL LANCZ(N2,T1,T2, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            E=EG(IG)
            CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
            CALL SPOUT(1,SPI,NSTM,RANGE)
          END DO !IS
C------------------------------------------
C No "Eang" command in the item "EXEC:"
C------------------------------------------
        ELSE
          DO IS=NSPN(1),NSPN(2)
            CALL CLRV(NPX+1,SPD)
            NSDEG=MOD( NDEG(NOBT+1)+1 ,2 )+1 
            DO IO=NOBT+1,NOBTX
              DO IEX=0, NDEG(IO)-1, NSDEG
                IO1=IO
                IEX1=IEX
                CALL CLRV(N2,T1)
                IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                  CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO1,IEX1,  IDCR
     &                     ,NHOP,NHM,H,IHP1,IHP2)
                END IF
                IF ( IS/NSDEG      .EQ. 1 ) THEN
                  CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO1,IEX1+1,IDCR
     &                     ,NHOP,NHM,H,IHP1,IHP2)
                END IF
                CALL WATCH('LANCZ   ')
                CALL LANCZ(N2,T1,T2, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                    ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                CALL WATCH('END     ')
                E=EG(IG)
                CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                CALL ADDV(NPX+1,SPD,SPI)
              END DO !IEX
            END DO !IO
            CALL SPOUT(1,SPD,NSTM,RANGE)
          END DO
        END IF
      END DO !IG
      END DO !IDCR
      END DO !IEANG

      END SUBROUTINE PES

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     IPES (INVERSE PHOTOEMISSION SPECTRA)
C  The operators in your X-card should be appeared in an order:
C  H_i, H_f,  T_D.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE IPES(MODE,NMODE
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,U,NDLT,DLT,EADJ,ERED
     &               ,NGM,NBGM,NG,EG,UG
     &               ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &               ,RANGE,SPR,SPI,SPD
     &               ,NREC,AR,BR, NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NSPN(2)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IEANG,NEANG,IG,IDCR,NDCR1,IS
      INTEGER IO,IO1,IEX,IEX1,NPX
      INTEGER LEX,NSDEG
      INTEGER NE,N1,N2,NR
      INTEGER H_i, H_f, T_D 
      REAL*8  E, YRE, YIM

C Function
      INTEGER NTOLEL
C -----------------------
C
      H_i=1
      H_f=2
      T_D=3

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_f)+1, H_f )

      CALL RMBLK( H_i,NHOP,NHM,H,IHP1,IHP2)

      CALL NSUBS(NDCR1, T_D, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,NSPN(1),NSPN(2),1
     &           ,NG,EG,EPH,EANG)
C
C creation of the final state Hamiltonian
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV( H_f, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C creation of transition matrices  
C
      CALL WATCH('TRNSMAT ')
      CALL HGEN(NBSM,T1, T_D, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL WATCH('END     ')
C
C main loop
C
      NEANG=NINT( EANG(0) )
      NPX  =NINT( RANGE(1,1) )
      DO IDCR=0,NDCR1-1 
      DO IG=1,NG
C------------------------------------------
C With "LMlim" command in the item "EXEC:"
C------------------------------------------
        IF (NEANG.NE.0) THEN
          IF (NEANG.LT.0) THEN
            DO IS=NSPN(1),NSPN(2)
              CALL CLRV(N2,T1)

              IO=NINT(EANG(IEANG))/100
              IEX=MOD(NINT(EANG(IEANG)),100)
              IF ( MOD( NDEG(IO), 2 ) .EQ. 1 ) THEN
                LEX=NDEG(IO)/2
                NSDEG=1
              ELSE IF ( MOD( NDEG(IO), 4 ) .EQ. 2 ) THEN
                LEX=NDEG(IO)/4
                NSDEG=2
              ELSE
                WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                STOP
              END IF
              IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO,IEX,  IDCR
     &                   ,NHOP,NHM,H,IHP2,IHP1)
              END IF
              IF ( IS/NSDEG      .EQ. 1 ) THEN
                CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO,IEX+1,IDCR
     &                   ,NHOP,NHM,H,IHP2,IHP1)
              END IF
              WRITE(6,*)
     &         'L=', LEX, ' M=', IEX/NSDEG - LEX, ' UDbit=',IS
              WRITE(2,*)
     &         'L=', LEX, ' M=', IEX/NSDEG - LEX, ' UDbit=',IS
            END DO !IS
          ELSE
C------------------------------------------
C With "Eang" command in the item "EXEC:"
C------------------------------------------
            DO IEANG=1,NEANG
            DO IS=NSPN(1),NSPN(2)
              CALL CLRV(N2,T1)
              DO IO=NOBT+1,NOBTX

                IF (MOD(NDEG(IO),2).EQ.1) THEN
                  LEX=NDEG(IO)/2
                  NSDEG=1
                ELSE IF (MOD(NDEG(IO),4).EQ.2) THEN
                  LEX=NDEG(IO)/4
                  NSDEG=2
                ELSE
                  WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                  STOP
                END IF

                DO IEX=0,NDEG(IO)-1,NSDEG
                  CALL Y( LEX, IEX/NSDEG - LEX
     &                  ,EANG(2*IEANG-1), EANG(2*IEANG), YRE ,YIM )
                  WRITE(6,*)'L=',LEX, ' M=', IEX/NSDEG - LEX
                  WRITE(6,*)'Yreal=',YRE
                  WRITE(6,*)'Yimag=',YIM
                  WRITE(6,*)' TH=',EANG(2*IEANG-1),' PY=',EANG(2*IEANG)
                  IF ( MOD( LEX, 2 ) .EQ. 1 ) THEN
                    YRE=-YRE
                    YIM=-YIM
                  END IF
                  CALL MOVV(N1,T2,UG(1,IG))
                  CALL MULS(N1,T2,YRE)
                  IF (MOD(NMODE,10).NE.0) THEN
                    CALL EXCV(N1,T2,UG(1,IG),-YIM)
                  END IF 
                  IO1=IO
                  IEX1=IEX
                  WRITE(6,*)'IO&IEX LOOP'
                  IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                    CALL OPRT(N2,T1,N1,T2, T_D, IO1,IEX1,  IDCR
     &                       ,NHOP,NHM,H,IHP2,IHP1)
                  END IF
                  IF ( IS/NSDEG      .EQ. 1 ) THEN
                    CALL OPRT(N2,T1,N1,T2, T_D, IO1,IEX1+1,IDCR
     &                       ,NHOP,NHM,H,IHP2,IHP1)
                  END IF
                END DO !IEX

              END DO !IO

              CALL WATCH('LANCZ   ')
              CALL LANCZ(N2,T1,T2, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                  ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
              CALL WATCH('END     ')
              E=EG(IG)
              CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
              CALL SPOUT(1,SPI,NSTM,RANGE)

            END DO !IS
            END DO !IEANG
          END IF
        ELSE 
C------------------------------------------
C No "Eang" command in the item "EXEC:"
C------------------------------------------
          DO IS=NSPN(1),NSPN(2)
            CALL CLRV(NPX+1,SPD)
            NSDEG=MOD( NDEG(NOBT+1)+1 ,2 )+1 
            DO IO=NOBT+1,NOBTX
              DO IEX=0,NDEG(IO)-1,NSDEG
                IO1=IO
                IEX1=IEX
                CALL CLRV(N2,T1)

                IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                  CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO1,IEX1,  IDCR
     &                     ,NHOP,NHM,H,IHP2,IHP1)
                END IF
                IF ( IS/NSDEG      .EQ. 1 ) THEN
                  CALL OPRT(N2,T1,N1,UG(1,IG), T_D, IO1,IEX1+1,IDCR
     &                     ,NHOP,NHM,H,IHP2,IHP1)
                END IF
                CALL WATCH('LANCZ   ')
                CALL LANCZ(N2,T1,T2, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                    ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                CALL WATCH('END     ')
                E=EG(IG)
                CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                CALL ADDV(NPX+1,SPD,SPI)
              END DO !IEX
            END DO !IO
            CALL SPOUT(1,SPD,NSTM,RANGE)
          END DO 
        END IF
      END DO !IG
      END DO !IDCR

      END SUBROUTINE IPES

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   XAS (X-ray ABSORPTION SPECTRA) WITHOUT FANO EFFECT
C  The operators in your X-card should be appeared in an order:
C  H_i, H_f and T_R.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XAS(MODE,NMODE
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,U,NDLT,DLT,EADJ,ERED
     &             ,NGM,NBGM,NG,EG,UG
     &             ,ORBM,EPH,EANG,NDCR,CDCR
     &             ,RANGE,SPR,SPI
     &             ,NREC,AR,BR, NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM, NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IG,IDCR,NDCR1,N1,N2,NR
      INTEGER H_i, H_f, T_R 
      REAL*8  E
C -----------------------
C
      H_i=1
      H_f=2
      T_R=3

      N1=NSFT( NCI(H_i)+1, H_i)
      N2=NSFT( NCI(H_f)+1, H_f)

      CALL RMBLK( H_i,NHOP,NHM,H,IHP1,IHP2)

      CALL NSUBS(NDCR1,T_R, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,0 ,-1 ,1
     &           ,NG,EG,EPH,EANG)
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      CALL HGEN(NBSM,T1, T_R, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL WATCH('END     ')
C
C main
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV( H_f, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')

      DO IDCR=0,NDCR1-1 
        DO IG=1,NG
          E=EG(IG)
          CALL CLRV(N2,T1)
          CALL WATCH('OPRT2   ')
          CALL OPRT(N2,T1,N1,UG(1,IG), T_R, 0,0,IDCR
     &             ,NHOP,NHM,H,IHP1,IHP2)
          CALL WATCH('END     ')
          CALL WATCH('LANCZ   ')
          CALL LANCZ(N2,T1,T2, H_f, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
          CALL WATCH('END     ')
          CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
          CALL WATCH('SPOUT   ')
          CALL SPOUT(1,SPI,NSTM,RANGE)
          CALL WATCH('END     ')
        END DO !IG
      END DO !IDCR

      END SUBROUTINE XAS 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   XAS (X-ray ABSORPTION SPECTRA) WITHOUT FANO EFFECT
C   including diffence of radial function )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XASR(MODE,NMODE
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,U,NDLT,DLT,EADJ,ERED
     &               ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
     &               ,RANGE,SPR,SPI
     &               ,NREC,AR,BR,NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IOP,NEANG,IG,IDCR,NDCR1,N1,N2,NR,NSHL
      INTEGER H_i, H_f, T_R 
      REAL*8 E
C --------------------------
      H_i=1
      H_f=2
      T_R=3

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_f)+1, H_f )
      NSHL=NCI(0)-1

      CALL RMBLK(H_i,NHOP,NHM,H,IHP1,IHP2)

      CALL NSUBS(NDCR1,5, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      NEANG=NINT( EANG(0) )
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,0 ,-1 ,NSHL
     &           ,NG,EG,EPH,EANG)
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=NCI(0)+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
C main
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(2,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
      DO IDCR=0,NDCR1-1 
        DO IG=1,NG
          E=EG(IG)
          CALL MOVV(N1,T1,UG(1,IG))
          CALL OPRT(N1,T1,N1,UG(1,IG),3,0,0,0   ,NHOP,NHM,H,IHP1,IHP2)
          CALL OPRT(N1,T1,N1,UG(1,IG),3,0,0,0   ,NHOP,NHM,H,IHP2,IHP1)
C
          CALL CLRV(N2,T2)
          CALL OPRT(N2,T2,N1,T1      ,4,0,0,IDCR,NHOP,NHM,H,IHP1,IHP2)
C
          CALL MOVV(N2,T1,T2)
          CALL OPRT(N2,T1,N2,T2      ,5,0,0,0   ,NHOP,NHM,H,IHP1,IHP2)
          CALL OPRT(N2,T1,N2,T2      ,5,0,0,0   ,NHOP,NHM,H,IHP2,IHP1)
C
          CALL WATCH('LANCZ   ')
          CALL LANCZ(N2,T1,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
          CALL WATCH('END     ')
C
          CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
          CALL WATCH('SPOUT   ')
          CALL SPOUT(1,SPI,NSTM,RANGE)
          CALL WATCH('END     ')
        END DO !IG
      END DO !IDCR
      END SUBROUTINE XASR

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   OPCND (Optical conductivety)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPCND(MODE,NMODE
     &                ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &                ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,U,NDLT,DLT,EADJ,ERED
     &                ,NGM,NBGM,NG,EG,UG
     &                ,ORBM,EPH,EANG,NDCR,CDCR
     &                ,RANGE,SPR,SPI
     &                ,NREC,AR,BR,NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   IUOUT
      PARAMETER(IUOUT=2)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER  IOP,IG,N1,NR,NCPX,NEXL
      INTEGER  H_i, H_J, H_K
      REAL*8   E, SR, SI
C --------------------------------------
      H_i=1
      H_J=2
      IF (NHOP(0).EQ.2) THEN
        H_K=2
      ELSE IF (NHOP(0).EQ.3) THEN
        H_K=3
      END IF

      N1=NSFT(NCI(H_i)+1,H_i)

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)

      CALL FLHEAD(MODE,NMODE, 0, NDCR, CDCR,0 ,-1 ,1
     &           ,NG,EG,EPH,EANG)
C
C create Hamiltonian
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,H_i,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(H_i,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=NCI(0)+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
C main
C
      WRITE(IUOUT,*)'Kinetic Energy'
      DO IG=1,NG
        CALL CLRV(N1,T1)
        CALL OPRT(N1,T1,N1,UG(1,IG),H_K,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N1,T1,N1,UG(1,IG),H_K,0,0,0,NHOP,NHM,H,IHP2,IHP1)
        CALL MULCV(N1,T1,UG(1,IG),SR,SI,NCPX)
        WRITE(IUOUT,*)SR,SI
      END DO

      CALL RMDIAG(H_J,NHOP,NHM,H,IHP1,IHP2)
      DO IG=1,NG
        E=EG(IG)
        CALL CLRV(N1,T1)
C
C operate current operator j(q=0) = sum_r j(r) 
C
        CALL WATCH('OPRT2   ')
        CALL OPRT(N1,T1,N1,UG(1,IG),H_J,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL MULS(N1,T1,-1D0)
        CALL OPRT(N1,T1,N1,UG(1,IG),H_J,0,0,0,NHOP,NHM,H,IHP2,IHP1)
        CALL MULS(N1,T1,-1D0)
        CALL WATCH('END     ')
        CALL WATCH('LANCZ   ')
C        DO IG2=1,NG
C          IF ( ABS(EG(IG2)-EG(1)).LT.1D-10 ) THEN
C            CALL MULCV(N1,T1,UG(1,IG2),SR,SI,NCPX)
C            CALL MADCS(N1,UG(1,IG2),T1,-SR,-SI,NCPX)
C          END IF 
C        END DO
        CALL LANCZ(N1,T1,T2,H_i,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
        CALL WATCH('END     ')
        CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
        CALL WATCH('SPOUT   ')
        CALL SPOUT(1,SPI,NSTM,RANGE)
        CALL WATCH('END     ')
      END DO !IG
C
      END SUBROUTINE OPCND

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     XRS (X-ray Resonant Scattering)
C  The operators in your X-card should be appeared in an order:
C  H_i, H_f and T_R.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XRS(MODE,NMODE
     &              ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &              ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,U,NDLT,DLT,EADJ,ERED
     &              ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
     &              ,RANGE,SPR,SPI
     &              ,NREC,AR,BR,NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   IUOUT
      PARAMETER(IUOUT=2)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IOP,IG,IDCR1,IDCR2,NDCR1,NE,N1,N2,NR
      INTEGER NEANG,NCPX,NEXL
      INTEGER H_i, H_f, T_R 
      REAL*8  E

C Function
      INTEGER NTOLEL
C -----------------------
C
      H_i=1
      H_f=2
      T_R=3

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_f)+1, H_f )

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)

      CALL RMBLK(H_i,NHOP,NHM,H,IHP1,IHP2)

      NDCR1=NDCR(1)
      NEANG=NINT( EANG(0) ) 
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,0 ,-1 ,1
     &           ,NG,EG,EPH,EANG)
C
C creation of the final state Hamiltonian
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,H_f,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(H_f,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C creation of transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=NCI(0)+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
C main loop
C
      DO IG=1,NG

        E=EG(IG)
        DO IDCR1=0,NDCR1-1

          CALL CLRV(N2,T1)
          CALL OPRT(N2,T1,N1,UG(1,IG),T_R ,0,0,IDCR1
     &             ,NHOP,NHM,H,IHP1,IHP2)

          CALL WATCH('LANCZ   ')
          CALL LANCZ(N2,T1,T2,H_f ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
          CALL WATCH('END     ')

          WRITE(IUOUT,'(''Diagonal Element'')')
          WRITE(IUOUT,'('' IG='',I4,'' IDCR='',I4)')IG,IDCR1
          CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
          CALL SPOUT(1,SPI,NSTM,RANGE)
        END DO

        E=EG(IG)
        DO IDCR1=0,NDCR1-1
          DO IDCR2=IDCR1+1,NDCR1-1

            CALL CLRV(N2,T1)
            CALL OPRT(N2,T1,N1,UG(1,IG),T_R,0,0,IDCR1
     &               ,NHOP,NHM,H,IHP1,IHP2)
            CALL OPRT(N2,T1,N1,UG(1,IG),T_R,0,0,IDCR2
     &               ,NHOP,NHM,H,IHP1,IHP2)

            CALL WATCH('LANCZ   ')
            CALL LANCZ(N2,T1,T2,H_f,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')

            WRITE(IUOUT,'(''Real Part (Off diagonal element)'')')
            WRITE(IUOUT,'(''IG='',I4,'' IDCR1='',I4,'' IDCR2='',I4)')
     &                 IG,IDCR1,IDCR2
            CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
            CALL SPOUT(1,SPI,NSTM,RANGE)
C             
            IF (NCPX.NE.0) THEN 
              CALL CLRV(N2,T1)
              CALL OPRT(N2,T1,N1,UG(1,IG),T_R,0,0,IDCR1
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL CLRV(N2,T2)
              CALL OPRT(N2,T2,N1,UG(1,IG),T_R,0,0,IDCR2
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL EXCV(N2,T1,T2,-1D0)

              CALL WATCH('LANCZ   ')
              CALL LANCZ(N2,T1,T2,H_f,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                  ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
              CALL WATCH('END     ')

             WRITE(IUOUT,'(''Imaginary Part (Off diagonal element)'')')
             WRITE(IUOUT,'(''IG='',I4,'' IDCR1='',I4,'' IDCR2='',I4)')
     &                   IG,IDCR1,IDCR2
              CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
              CALL SPOUT(1,SPI,NSTM,RANGE)
            END IF

          END DO !IDCR2
        END DO !IDCR1

      END DO !IG

      END SUBROUTINE XRS 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     XRSQ (X-ray Resonant Scattering)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XRSQ(MODE,NMODE
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,U,NDLT,DLT,EADJ,ERED
     &             ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
     &             ,RANGE,SPR,SPI
     &             ,NREC,AR,BR,NBSM,T1,T2 )

      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER IOP,IG,NE,N1,N2,NR,NSHL
      INTEGER IS,IO,IEX1,IEX2,NEANG,NCPX,NEXL
      REAL*8 E

C Function
      INTEGER NTOLEL
C-------------------------------
C
      N1=NSFT(NCI(1)+1,1)
      N2=NSFT(NCI(2)+1,2)
      NSHL=NCI(0)-1

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)

      CALL RMBLK(1,NHOP,NHM,H,IHP1,IHP2)

      NEANG=NINT( EANG(0) ) 
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,0 ,-1 ,NSHL
     &           ,NG,EG,EPH,EANG)
C
C creation of the final state Hamiltonian
C
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(2,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C creation of transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=NCI(0)+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
C main loop
C
      IO=NOBT+1
      DO IG=1,NG
        DO IEX1=0, NDEG(IO)-1, 2
          DO IS=0,1
            CALL CLRV(N2,T1)
            CALL OPRT(N2,T1,N1,UG(1,IG),3,IO,IEX1+IS,0
     &               ,NHOP,NHM,H,IHP2,IHP1)
            CALL WATCH('LANCZ   ')
            CALL LANCZ(N2,T1,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            E=EG(IG)
            WRITE(2,'(''Diagonal Element'')')
            WRITE(2,'('' IG='',I4,'' M='',I4,'' 2Sz='',I4)')
     &                 IG,IEX1/2-NDEG(IO)/4,2*IS-1
            CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
            CALL SPOUT(1,SPI,NSTM,RANGE)
          END DO !IS
        END DO !IEX1
C
        DO IEX1=0, NDEG(IO)-1, 2
          DO IEX2=IEX1+2, NDEG(IO)-1,2
            DO IS=0,1
              CALL CLRV(N2,T1)
              CALL OPRT(N2,T1,N1,UG(1,IG),3,IO,IEX1+IS,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
              CALL OPRT(N2,T1,N1,UG(1,IG),3,IO,IEX2+IS,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
              CALL WATCH('LANCZ   ')
              CALL LANCZ(N2,T1,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                  ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
              CALL WATCH('END     ')
              E=EG(IG)
              WRITE(2,'(''Real Part'')')
           WRITE(2,'(''IG='',I4,'' M1='',I4,'' M2='',I4,'' 2Sz='',I4)')
     &                 IG, IEX1/2-NDEG(IO)/4, IEX2/2-NDEG(IO)/4, 2*IS-1
              CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
              CALL SPOUT(1,SPI,NSTM,RANGE)
C             
              IF (NCPX.NE.0) THEN 
                CALL CLRV(N2,T1)
                CALL OPRT(N2,T1,N1,UG(1,IG),3,IO,IEX1+IS,0
     &                   ,NHOP,NHM,H,IHP2,IHP1)
                CALL CLRV(N2,T2)
                CALL OPRT(N2,T2,N1,UG(1,IG),3,IO,IEX2+IS,0
     &                   ,NHOP,NHM,H,IHP2,IHP1)
                CALL EXCV(N2,T1,T2,-1D0)
                CALL WATCH('LANCZ   ')
                CALL LANCZ(N2,T1,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                    ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                CALL WATCH('END     ')
                WRITE(2,'(''Imaginary Part'')')
           WRITE(2,'(''IG='',I4,'' M1='',I4,'' M2='',I4,'' 2Sz='',I4)')
     &                 IG,IEX1/2-NDEG(IO)/4, IEX2/2-NDEG(IO)/4,2*IS-1
                CALL RECUR(1,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                CALL SPOUT(1,SPI,NSTM,RANGE)
              END IF
C
            END DO !IS
          END DO !IEX2
        END DO !IEX1
C
      END DO !IG
C
      END SUBROUTINE XRSQ 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   DOS (DENSITY OF STATES)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DOS(NSTATE,MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI,SPU,SPD
     &           ,NREC,AR,BR,NBSM,T1,T2 )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

C Dummy arguments
      INTEGER NSTATE

      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPU(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

C Local variables
      INTEGER I,IC,IG,N,N2,NR,NHTOP,NPX
      REAL*8 T,ERR,RERR,R
C Function
      REAL*8 FRAND
C -----------------------

      CALL FLHEAD(MODE,NMODE, 0, NDCR,CDCR, 0, -1, NCI(NSTATE)
     &           ,NG,EG,EPH,EANG)

      CALL HGEN(NBSM,T1,NSTATE,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(NSTATE,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      N2 =NSFT(NCI(NSTATE)+1,NSTATE)
      NPX=NINT( RANGE(1,1) )
      DO IG=1,NG
        DO IC=1,NCI(NSTATE)
          N=0
          CALL CLRV(NPX+1,SPD)
          DO
            CALL CLRV(N2,T1)
            CALL WATCH('RNDVEC  ')
            DO I=NSFT(IC,NSTATE)+1,NSFT(IC+1,NSTATE)
              T1(I)=SIGN(1D0,FRAND()-0.5D0)
            END DO
            CALL WATCH('END     ')
            N=N+1
            CALL WATCH('LANCZ   ')
            CALL LANCZ(N2,T1,T2,NSTATE,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            CALL RECUR(1,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
            CALL MOVV(NPX+1,SPU,SPD)
            CALL ADDV(NPX+1,SPD,SPI)
            T=0D0
            IF (N.NE.1) T=DBLE(N)/DBLE(N-1)
            CALL MULS(NPX+1,SPU,T)
            CALL SUBV(NPX+1,SPU,SPD)
            CALL ABSV(NPX+1,SPU,RERR)
            CALL ABSV(NPX+1,SPD,R)
            ERR=RERR/R
            IF ( ERR.LT.0.01 .OR. N.GT.N2/2 ) EXIT 
          END DO 
          WRITE(6,*)'  NITA=',N,'  ERR=',ERR
          T=1D0/DBLE(N)
          CALL MULS(NPX+1,SPD,T)
          CALL SPOUT(1,SPD,NSTM,RANGE)
        END DO !IC
      END DO !IG
C
      END SUBROUTINE DOS
C
C>>>START RESONANCE
C=============================================================
C Second Order Optical Process
C=============================================================

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   XAS (X-ray ABSORPTION SPECTRA) WITH FANO EFFECT
C   and imaginary part of self-energy term
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RXAS(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,GAM,GALF
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,RANGE,SPR,SPI
     &           ,NREC,AR,AI,BR,BI
     &           ,NBSM,T1,T2, NBMM,TMR,TMI, TM1,TM2,TM3 )
     

      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8    PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
      REAL*8  GAM, GALF
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM),AI(0:NRECM),BI(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM)
      REAL*8  TM1(NBMM),TM2(NBMM),TM3(NBMM)

C Local variables
      INTEGER NF(NSTM)
      INTEGER I,IG,IDCR,IDCR1,NDCR1,ISTS
      INTEGER IO,IO1,IEX,IEX1,NPX
      INTEGER ISHL,IOP
      INTEGER N1,N2,N3,NR,NSHL,NHTOP
      INTEGER H_i, H_m, H_f, H_a, T_R, T_D, T_A
      REAL*8  S, T
C -----------------------

      IF (INDEX(MODE,'g').NE.0) THEN
        NSHL=(NCI(0)-2)/2
      ELSE 
        NSHL=NCI(0)-2
      END IF
      WRITE(6,*)'NSHL=',NSHL
     
      H_i =1
      H_m =2
      H_f =3 
      H_a =2+NSHL+1
      T_R =2+NSHL+NSHL+1
      T_D =2+NSHL+1
      T_A =2+NSHL+NSHL+2

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_m)+1, H_m )

      DO ISHL=1,NSHL
        NF(ISHL)=NSFT( NCI(H_f + ISHL-1)+1, H_f + ISHL-1)
      END DO

      ISTS=1
      IF (INDEX(MODE,'f').NE.0) ISTS=2
 
      RANGE(4,1)=SQRT(RANGE(4,1)**2+GAM**2) ! GAM2=SQRT(GAM2**2+GAM**2)

C
C   create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=T_D, NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')

      CALL NSUBS(NDCR1, T_R, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,0,-1, 1
     &           ,NG,EG,EPH,EANG )

      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,TMR, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV( H_m, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C main
C
      NPX=NINT( RANGE(1,1) )
      DO IDCR=0,NDCR1-1 
        DO IG=1,NG
          CALL CLRV(N2,TMI)
          DO ISHL=1,NSHL
            DO IO=NOBT+1,NOBTX
              DO IEX=0,NDEG(IO)-1
                IO1=IO
                IEX1=IEX
                IDCR1=IDCR
                N3=NF(ISHL)
                CALL CLRV(N3,T1)
                WRITE(6,*)'IO&IEX LOOP'
                CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                   ,IO1,IEX1,IDCR1,NHOP,NHM,H,IHP1,IHP2)
                CALL OPRT(N2,TMI,N3,T1     , T_A + ISHL-1
     &                   ,IO1,IEX1,0    ,NHOP,NHM,H,IHP2,IHP1)
              END DO !IEX
            END DO !IO
          END DO !ISHL

          CALL MULS(N2,TMI,-PI*RHO)
          CALL CLRV(N2,TMR)
          CALL OPRT(N2,TMR,N1,UG(1,IG), T_R, 0 ,0 ,IDCR1
     &             ,NHOP,NHM,H,IHP1,IHP2)
          CALL WATCH('LANCZ2  ')
          CALL LANCZ2(N2,TMR,TMI, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,GALF,NF, T_A, NSHL,NDEG,NOBT,NOBTX,ISTS
     &               ,NREC,NR,AR,AI,BR,BI
     &               ,TM1,TM2,TM3)
          CALL WATCH('END     ')
          CALL RECUR2(1,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,AI,BR,BI)

          S=0D0
          DO ISHL=1,NSHL
            DO IO=NOBT+1,NOBTX
              DO IEX=0,NDEG(IO)-1
                IO1=IO
                IEX1=IEX
                N3=NF(ISHL)
                CALL CLRV(N3,T1)
                WRITE(6,*)'IO&IEX LOOP'
                CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                   ,IO1,IEX1,IDCR  ,NHOP,NHM,H,IHP1,IHP2)
                CALL ABSV(N3,T1,T)
                S=S+T**2
              END DO
            END DO
          END DO

          S=S*PI*RHO
          DO I=0,NPX
            SPI(I)=SPI(I)+S
          END DO
          CALL SPOUT(1,SPI,NSTM,RANGE)
        END DO !IG
      END DO !IDCR

      END SUBROUTINE RXAS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    RPES (RESONANT PHOTOEMISSION SPECTRA)
C
C  The initial "i", intermedate "m", final "f_i" and Auger "a_i" 
C  states must be arranged in this order in the configuration 
C  table in the item "CNFG:". The Auger states "a_i", which 
C  is reached from the final state "f_i" by a Auger decay 
C  "T'_Ai" are only required for RPESg or RPESfg modes.
C
C  The operators should be appeared in an order: 
C  H_i, H_m, H_f1, H_f2,..., H_fN, T_D1, T_D2,..., T_DN, T_R,
C  T_A1, T_A2,..., T_AN, [T'_A1, T'_A2,...,T'_AN].
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RPES(MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED,GAM,GALF
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &           ,RANGE,GALF2,SPR,SPI,SPD
     &           ,NREC,AR,AI,BR,BI
     &           ,NBSM,T1,T2, NBFM,T3,T4,T5, NBMM,TMR,TMI,TMW1,TMW2
     &           ,TMW3,TMW4,TMW5,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &           ,ASTS,ACI,AOBT)
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
      REAL*8  GAM, GALF
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NSPN(2)

      REAL*8  RANGE(4,NSTM) 
      REAL*8  GALF2(0:NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM),AI(0:NRECM),BI(0:NRECM)

      INTEGER NBSM 
      REAL*8  T1(NBSM),T2(NBSM)

      INTEGER NBFM
      REAL*8  T3(NBFM),T4(NBFM),T5(NBFM)

      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM),TMW1(NBMM),TMW2(NBMM)
      REAL*8  TMW3(NBMM),TMW4(NBMM),TMW5(NBMM),TMW6(NBMM),TMW7(NBMM)
      REAL*8  TMW8(NBMM),TMW9(NBMM),TMW10(NBMM),TMW11(NBMM),TMW12(NBMM)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER NF(NSTM2)
      INTEGER NA(1)
      INTEGER IEPH,NEPH,IEANG,NEANG,IG,IDCR,IDCR1,NDCR1,IS
      INTEGER I,J,IO,IO1,IEX,IEX1,NPX
      INTEGER ISHL,IOP,LEX,NSDEG
      INTEGER N1,N2,N3,NR,NSHL,NHTOP,ISTS,ISTS2,ISTP
      INTEGER H_i, H_m, H_f, H_a, T_R, T_D, T_A, T1_A
      REAL*8 E, YRE, YIM, CU, CD
C -----------------------

      IF (INDEX(MODE,'g').NE.0) THEN
        NSHL=(NCI(0)-2)/2
      ELSE 
        NSHL=NCI(0)-2
      END IF
      WRITE(6,*)'NSHL=',NSHL

      H_i =1
      H_m =2
      H_f =3
      H_a =2+NSHL+1
      T_R =2+NSHL+NSHL+1
      T_D =2+NSHL+1
      T_A =2+NSHL+NSHL+2
      T1_A=2+NSHL+NSHL+1+NSHL+1

      N1=NSFT( NCI(H_i)+1, H_i )
      N2=NSFT( NCI(H_m)+1, H_m )

      ISTS=1
      IF (INDEX(MODE,'f').NE.0) ISTS=2
      ISTS2=1
      IF (INDEX(MODE,'g').NE.0) ISTS2=2

      DO ISHL=1,NSHL
        NF(ISHL)=NSFT( NCI(H_f + ISHL-1)+1, H_f + ISHL-1)
        IF ( NINT( RANGE(1,ISHL) ) .EQ. 0 ) THEN
          DO I=1,4
            RANGE(I,ISHL)=RANGE(I,1)
          END DO
        END IF 
      END DO
C
C create transition matrices 
      CALL WATCH('TRNSMAT ')
      DO IOP=T_D,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
      NHTOP=IHP1(1)

      CALL NSUBS(NDCR1,T_D, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      NEANG=NINT( EANG(0) )
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,NSPN(1),NSPN(2),NSHL
     &           ,NG,EG,EPH,EANG)
C
C main loop
C
      NEPH=NINT( EPH(0) )
      DO IEPH=1,NEPH
      DO IG=1,NG
      DO IDCR=0,NDCR1-1
C
C Create intermediate state Hamiltonian
        IF (IDCR.EQ.0) THEN
          CALL CLRV(N2,TMR)
          CALL CLRV(N2,TMI)
        END IF
        IHP1(1)=NHTOP
        IHP2(1)=NHTOP
        CALL WATCH('HGEN    ')
        CALL HGEN(NBSM,T1, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
        CALL HELV(H_m,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &           ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
        CALL WATCH('END     ')
C
C Interferece term
        CALL MOVV(N2,T2,TMI)
        CALL CLRV(N2,TMI)
        DO ISHL=1,NSHL
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              N3=NF(ISHL)
              CALL CLRV(N3,T1)
              IO1=IO
              IEX1=IEX
              CALL OPRT(N3,T1, N1,UG(1,IG), T_D + ISHL-1 
     &                 ,IO1,IEX1,IDCR, NHOP,NHM,H,IHP1,IHP2)
              CALL OPRT(N2,TMI,N3,T1     , T_A + ISHL-1
     &                 ,IO1,IEX1,0   , NHOP,NHM,H,IHP2,IHP1)
            END DO !IEX
          END DO !IO
        END DO !ISHL
        CALL MULS(N2,TMI,-PI*RHO)
C
C Resonance Term (Intermediate State Green's Function)
C
        CALL MOVV(N2,T1,TMR)
        CALL CLRV(N2,TMR)
        CALL OPRT(N2,TMR,N1,UG(1,IG), T_R ,0  ,0   ,IDCR
     &           ,NHOP,NHM,H,IHP1,IHP2)
        E=EPH(IEPH)+EG(IG)
        WRITE(6,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
        WRITE(2,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
        CALL WATCH('INVM    ')
        CALL INVM(N2,TMR,TMI, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,E,GAM,GALF
     &           ,NF, T_A, NSHL,NDEG,NOBT,NOBTX,ISTS,NMODE
     &           ,NBMM,TMW1,TMW2,TMW3,TMW4,TMW5
     &           ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12)
        CALL WATCH('END     ')
        CALL MAGT( H_m, ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,NBMM,TMR,NMODE, NBMM,TMW1,TMW2
     &           ,ASTS,ACI,AOBT)
        CALL CNFMIX( H_m, NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)

        CALL MOVV(N2,TMW1,T1)
        CALL MOVV(N2,TMW2,T2)
        CALL MULS(N2,TMR,-1D0)
        CALL MULS(N2,TMI,-1D0)

        DO ISHL=1,NSHL
C
C Create Final State Hamiltonian(s)
C
          N3 =NF(ISHL)
          NPX=NINT( RANGE(1,ISHL) )
          IHP1(1)=NHTOP
          IHP2(1)=NHTOP
          CALL WATCH('HGEN    ')
          CALL HGEN(NBSM,T1, H_f + ISHL-1
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
          CALL HELV( H_f + ISHL-1 ,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &             ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
          CALL WATCH('END     ')
          E=EG(IG)
          ISTP=1
C------------------------------------------
C With "Eang" command in the item "EXEC:"
C------------------------------------------
          IF (NEANG.NE.0) THEN
            DO IEANG=1,NEANG
            DO IS=NSPN(1),NSPN(2)
              CALL CLRV(N3,T1)
              DO IO=NOBT+1,NOBTX
                IF ( MOD( NDEG(IO) , 2 ) .EQ. 1 ) THEN
                  LEX=NDEG(IO)/2
                  NSDEG=1
                ELSE IF ( MOD( NDEG(IO), 4 ) .EQ. 2 ) THEN
                  LEX=NDEG(IO)/4
                  NSDEG=2
                ELSE
                  WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                  STOP
                END IF
                DO IEX=0, NDEG(IO)-1, NSDEG

                  CALL Y( LEX, IEX/NSDEG - LEX
     &                  , EANG(2*IEANG-1), EANG(2*IEANG) ,YRE, YIM )
                  IF (MOD(LEX,2).EQ.1) THEN
                    YRE=-YRE
                    YIM=-YIM
                  END IF
                  IO1=IO
                  IEX1=IEX
                  CALL CLRV(N3,T2)
                  IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                    CALL OPRT(N3,T2,N1,UG(1,IG), T_D + ISHL-1
     &                       ,IO1,IEX1,IDCR, NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,T2,N2,TMR,      T_A + ISHL-1
     &                       ,IO1,IEX1,0,    NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF ( IS/NSDEG .EQ. 1 ) THEN
                    CALL OPRT(N3,T2,N1,UG(1,IG),T_D + ISHL-1
     &                       ,IO1,IEX1+1,IDCR,NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,T2,N2,TMR,     T_A + ISHL-1
     &                       ,IO1,IEX1+1,0,   NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF (MOD(NMODE,10).NE.0) THEN
                    CALL EXCV(N3,T1,T2,-YIM)
                  END IF 
                  CALL MULS(N3,T2,YRE)
                  CALL ADDV(N3,T1,T2)

                END DO !IEX
              END DO !IO

              IF (ISTS2.EQ.1) THEN
                CALL WATCH('LANCZ   ')
                CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                    ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                    ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                CALL WATCH('END     ')
                CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
              ELSE 
                NA(1)=NSFT( NCI(H_a + ISHL-1)+1, H_a + ISHL-1 )
                CALL WATCH('LANCZ2  ')
                CALL CLRV(N3,T2)
                CALL LANCZ2(N3,T1,T2, H_f + ISHL-1
     &                     ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                     ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                     ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                CALL WATCH('END     ')
                CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,AI,BR,BI)
              END IF
              CALL MOVV (NPX+1,SPD,SPI)

              CALL CLRV(N3,T1)
              DO IO=NOBT+1,NOBTX
                IF (MOD(NDEG(IO),2).EQ.1) THEN
                  LEX=NDEG(IO)/2
                  NSDEG=1
                ELSE IF (MOD(NDEG(IO),4).EQ.2) THEN
                  LEX=NDEG(IO)/4
                  NSDEG=2
                ELSE
                  WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                  STOP
                END IF

                DO IEX=0, NDEG(IO)-1, NSDEG
                  CALL Y( LEX, IEX/NSDEG - LEX
     &                  , EANG(2*IEANG-1), EANG(2*IEANG), YRE, YIM )
                  IF ( MOD( LEX, 2 ) .EQ. 1 ) THEN
                    YRE=-YRE
                    YIM=-YIM
                  END IF
                  IO1=IO
                  CALL CLRV(N3,T2)
                  IF ( MOD(IS,NSDEG) .EQ. 1) THEN
                    CALL OPRT(N3,T2,N2,TMI, T_A + ISHL-1
     &                       , IO1,IEX1,  0 ,NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF ( IS/NSDEG .EQ. 1) THEN 
                    CALL OPRT(N3,T2,N2,TMI, T_A + ISHL-1
     &                       , IO1,IEX1+1,0 ,NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF (MOD(NMODE,10).NE.0) CALL EXCV(N3,T1,T2,-YIM)
                  CALL MULS(N3,T2,YRE)
                  CALL ADDV(N3,T1,T2)
                END DO !IEX
              END DO !IO

              IF (ISTS2.EQ.1) THEN
                CALL WATCH('LANCZ   ')
                CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                    ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                    ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                CALL WATCH('END     ')
                CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
              ELSE 
                NA(1)=NSFT( NCI(H_a + ISHL-1)+1, H_a + ISHL-1 )
                CALL WATCH('LANCZ2  ')
                CALL CLRV(N3,T2)
                CALL LANCZ2(N3,T1,T2, H_f + ISHL-1
     &                     ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                     ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                     ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                CALL WATCH('END     ')
                CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,AI,BR,BI)
              END IF
              CALL ADDV (NPX+1,SPD,SPI)
              CALL SPOUT(ISHL,SPD,NSTM,RANGE)
            END DO !IS
            END DO !IEANG

          ELSE IF (NSPN(1).GT.0) THEN
C------------------------------------------
C No "Eang" command in the item "EXEC:"
C------------------------------------------
C  Get Spectral Density Using Recursion Method
            DO IS=NSPN(1),NSPN(2)
              CALL CLRV(NPX+1,SPD)
              NSDEG=MOD( NDEG(NOBT+1)+1 ,2 )+1 
              DO IO=NOBT+1,NOBTX
                DO IEX=0,NDEG(IO)-1,NSDEG
                  IO1=IO
                  IEX1=IEX
C Real Part 
                  CALL CLRV(N3,T1)
                  IF ( MOD(IS,NSDEG) .EQ. 1 ) THEN
                    CALL OPRT(N3,T1,N1,UG(1,IG), T_D+ISHL-1
     &                       ,IO1, IEX1, IDCR, NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,T1,N2,TMR,      T_A+ISHL-1
     &                       ,IO1, IEX1, 0   , NHOP,NHM,H,IHP1,IHP2)
                  END IF
                  IF ( IS/NSDEG .EQ. 1 ) THEN
                    CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                       ,IO1,IEX1+1,IDCR ,NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,T1,N2,TMR,      T_A + ISHL-1
     &                       ,IO1, IEX1+1,0   ,NHOP,NHM,H,IHP1,IHP2)
                  END IF

                  IF (ISTS2.EQ.1) THEN
                    CALL WATCH('LANCZ   ')
                    CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                        ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                        ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                    CALL WATCH('END     ')
                    CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                  ELSE 
                    NA(1)=NSFT( NCI( H_a + ISHL-1)+1, H_a + ISHL-1 )
                    CALL WATCH('LANCZ2  ')
                    CALL CLRV(N3,T2)
                    CALL LANCZ2(N3,T1,T2, H_f +ISHL-1
     &                       ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                       ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                       ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                    CALL WATCH('END     ')
                    CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE
     &                         ,NR,AR,AI,BR,BI)
                  END IF
                  CALL ADDV (NPX+1,SPD,SPI)
C Imagimary Part
                  CALL CLRV(N3,T1)
                  IF ( MOD(IS,NSDEG) .EQ. 1 )
     &              CALL OPRT(N3,T1,N2,TMI, T_A + ISHL-1
     &                       ,IO1,IEX1,  0 ,NHOP,NHM,H,IHP1,IHP2)

                  IF (   IS/NSDEG    .EQ. 1 ) 
     &              CALL OPRT(N3,T1,N2,TMI, T_A + ISHL-1
     &                       ,IO1,IEX1+1,0 ,NHOP,NHM,H,IHP1,IHP2)

                  IF (ISTS2.EQ.1) THEN
                    CALL WATCH('LANCZ   ')
                    CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                        ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                        ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                    CALL WATCH('END     ')
                    CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                  ELSE 
                    NA(1)=NSFT( NCI(H_a + ISHL-1)+1, H_a + ISHL-1 )
                    CALL WATCH('LANCZ2  ')
                    CALL CLRV(N3,T2)
                    CALL LANCZ2(N3,T1,T2, H_f + ISHL-1
     &                       ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                       ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                       ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                    CALL WATCH('END     ')
                    CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE
     &                              ,NR,AR,AI,BR,BI)
                  END IF
                  CALL ADDV (NPX+1,SPD,SPI)
                END DO !IEX
              END DO !IO
              CALL SPOUT(ISHL,SPD,NSTM,RANGE)
            END DO !IS
          ELSE
C------------------------------------------
C "JJ" is specified in Spin command
C in the item "EXEC:" 
C------------------------------------------
C  Get Spectral Density Using Recursion Method
            DO J=1,IDCR+1
              IF (IDCR.EQ.0) THEN
                CD=1D0
              ELSE
                CD=SQRT(DBLE(3-J)/3D0)
              END IF
              IF (IDCR.EQ.1.AND.J.EQ.1.OR.IDCR.EQ.2.AND.J.EQ.2)CD=-CD
C
              IF (IDCR.EQ.0) THEN
                CU=0D0
              ELSE 
                CU=SQRT(DBLE(J)/3D0)
              END IF
              IDCR1=IDCR
              IF (IDCR.EQ.2.AND.J.EQ.3) THEN
                IDCR1=IDCR+1
                CALL MOVV(N2, T1,  TMR)
                CALL MOVV(N2, TMR, TMW1)
                CALL MOVV(N2, TMW1,T1)
                CALL MOVV(N2, T2,  TMI)
                CALL MOVV(N2, TMI, TMW2)
                CALL MOVV(N2, TMW2,T2)
              END IF
              CALL CLRV(NPX+1,SPD)
              DO IO=NOBT+1,NOBTX
              DO IEX=0,NDEG(IO)-1,2
                IO1=IO
C
C Real Part 
C
                CALL CLRV(N3,T1)
                CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                   ,IO1,IEX+0,IDCR1,NHOP,NHM,H,IHP1,IHP2)
                CALL OPRT(N3,T1,N2,TMR,      T_A + ISHL-1
     &                   ,IO1,IEX+0,0,    NHOP,NHM,H,IHP1,IHP2)
                CALL MULS(N3,T1,CD)

                CALL CLRV(N3,T2)
                CALL OPRT(N3,T2,N1,UG(1,IG), T_D + ISHL-1
     &                   ,IO1,IEX+1,IDCR1-1,NHOP,NHM,H,IHP1,IHP2)
                CALL OPRT(N3,T2,N2,TMW1,     T_A + ISHL-1
     &                   ,IO1,IEX+1,0,      NHOP,NHM,H,IHP1,IHP2)
                CALL MADS(N3,T1,T2,CU)

                IF (ISTS2.EQ.1) THEN
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                ELSE 
                  NA(1)=NSFT( NCI( H_a + ISHL-1 )+1, H_a + ISHL-1 )
                  CALL WATCH('LANCZ2  ')
                  CALL CLRV(N3,T2)
                  CALL LANCZ2(N3,T1,T2, H_f + ISHL-1
     &                       ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                       ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                       ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                  CALL WATCH('END     ')
                  CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,AI,BR,BI)
                END IF
                CALL ADDV (NPX+1,SPD,SPI)
C Imagimary Part
                CALL CLRV(N3,T1)
                CALL OPRT(N3,T1,N2,TMI,  T_A + ISHL-1
     &                   ,IO1,IEX,  0,  NHOP,NHM,H,IHP1,IHP2)
                CALL MULS(N3,T1,CD)
                CALL CLRV(N3,T2)
                CALL OPRT(N3,T2,N2,TMW2, T_A + ISHL-1
     &                   ,IO1,IEX+1,0,  NHOP,NHM,H,IHP1,IHP2)
                CALL MADS(N3,T1,T2,CU)

                IF (ISTS2.EQ.1) THEN
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
                ELSE 
                  NA(1)=NSFT( NCI(H_a + ISHL-1)+1, H_a + ISHL-1 )
                  CALL WATCH('LANCZ2  ')
                  CALL CLRV(N3,T2)
                  CALL LANCZ2(N3,T1,T2, H_f + ISHL-1
     &                       ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,GALF2(ISHL)
     &                       ,NA, T1_A + ISHL-1, 1,NDEG,NOBT,NOBTX
     &                       ,ISTS2,NREC,NR,AR,AI,BR,BI,T3,T4,T5)
                  CALL WATCH('END     ')
                  CALL RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,AI,BR,BI)
                END IF
                CALL ADDV (NPX+1,SPD,SPI)
              END DO !IEX
              END DO !IO
              CALL SPOUT(ISHL,SPD,NSTM,RANGE)
              IF (IDCR.EQ.2.AND.J.EQ.3) THEN
                CALL MOVV(N2,T1,TMR)
                CALL MOVV(N2,TMR,TMW1)
                CALL MOVV(N2,TMW1,T1)
                CALL MOVV(N2,T2,TMI)
                CALL MOVV(N2,TMI,TMW2)
                CALL MOVV(N2,TMW2,T2)
              END IF
            END DO !J
          END IF
        END DO !ISHL
      END DO !IDCR
      END DO !IG
      END DO !IEPH
C
      END SUBROUTINE RPES

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  INTERMEDIATE STATE OF INVERSE PHOTOEMISSION 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE TIPES(MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &            ,U,NDLT,DLT,EADJ,ERED
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR, NBSM,T1,T2, NBMM,TMR,TMI )
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      INTEGER NSPN(2)

      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM)

C Local variables
      INTEGER NF(NSTM)
      INTEGER IEANG,NEANG,IG,IDCR,NDCR1,IS
      INTEGER IO,IO1,IEX,IEX1,NPX
      INTEGER IOP
      INTEGER N,N1,N2,NR,NSHL,NHTOP,IST,NE1,NE2
      REAL*8 YRE, YIM
      LOGICAL AUGER

C Function
      INTEGER NTOLEL
C -----------------------
C
      AUGER=.TRUE.
      N1=NSFT(NCI(1)+1,1)
      N2=NSFT(NCI(2)+1,2)

      NSHL=NCI(0)-3 
      NE1=NTOLEL( 1      ,NSTM,NCIM,NCI,NOBT,NELC)
      NE2=NTOLEL( NCI(0) ,NSTM,NCIM,NCI,NOBT,NELC)
      IF ( NE2 .EQ. NE1+1 ) THEN 
        WRITE(6,*)
     &   'The initial state will be regarded as the Auger state'
        NSHL=NSHL+1
        AUGER=.FALSE.
      END IF

      DO IST=3,2+NSHL
        NF(IST-2)=NSFT(NCI(IST)+1,IST)
      END DO
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=2+NSHL+1,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')

      NDCR1=NDCR(1)
      NEANG=NINT( EANG(0) )
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,NSPN(1),NSPN(2),NSHL
     &           ,NG,EG,EPH,EANG)

C create the intermediate state Hamiltonian
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(2,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')
C
C main loop
C
      NPX=NINT ( RANGE(1,1) )
      DO IEANG=1,MAX(NEANG,1)
      DO IDCR=0,NDCR1-1
      DO IG=1,NG
      DO IS=NSPN(1),NSPN(2)
        IF (NEANG.NE.0) THEN
          CALL CLRV(N2,TMR)
          DO IO=NOBT+1,NOBTX
            DO IEX=0, NDEG(IO)-1, 2
              CALL Y( NDEG(IO)/4, IEX/2-NDEG(IO)/4
     &             , EANG(2*IEANG-1),EANG(2*IEANG), YRE, YIM )
              WRITE(6,*)'L=',NDEG(IO)/4, ' M=',IEX/2-NDEG(IO)/4
              WRITE(6,*)'Yreal=',YRE
              WRITE(6,*)'Yimag=',YIM
              IF ( MOD(NDEG(IO)/4,2) .EQ.1) THEN
                YRE=-YRE
                YIM=-YIM
              END IF
              CALL MOVV(N1,T2,UG(1,IG))
              CALL MULS(N1,T2,YRE)
              IF (MOD(NMODE,10).NE.0) THEN
                CALL EXCV(N1,T2,UG(1,IG),-YIM)
              END IF
C
C Inverse Auger decay
              IO1=IO
              IEX1=IEX
              IOP=2+NSHL+NSHL+NSHL+1

              IF ( MOD(IS,2) .EQ. 1 ) 
     &          CALL OPRT(N2,TMR,N1,T2,IOP,IO1,IEX1,  0
     &                   ,NHOP,NHM,H,IHP2,IHP1)
              IF ( IS/2 .EQ. 1 ) 
     &          CALL OPRT(N2,TMR,N1,T2,IOP,IO1,IEX1+1,0
     &                   ,NHOP,NHM,H,IHP2,IHP1)

            END DO
          END DO
C
C The intermediate state spectral density
C
          CALL WATCH('LANCZ   ')
          CALL LANCZ(N2,TMR,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
          CALL WATCH('END     ')
          CALL RECUR(1,SPR,SPD,EG(IG),NSTM,RANGE,NR,AR,BR)
        ELSE
          CALL CLRV(NPX+1,SPD)
          DO IO=NOBT+1,NOBTX
          DO IEX=0,NDEG(IO)-1,2
            CALL CLRV(N2,TMR)
C
C Inverse Auger decay
C
            IO1=IO
            IEX1=IEX
            IF ( MOD(IS,2) .EQ. 1 ) 
     &        CALL OPRT(N2,TMR,N1,UG(1,IG),IOP,IO1,IEX1,  0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
            IF ( IS/2 .EQ. 1 ) 
     &        CALL OPRT(N2,TMR,N1,UG(1,IG),IOP,IO1,IEX1+1,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
C
C The intermediate state spectral density
C
            CALL WATCH('LANCZ   ')
            CALL LANCZ(N2,TMR,T2,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            CALL RECUR(1,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
            CALL ADDV(NPX+1,SPD,SPI)
          END DO !IEX
          END DO !IO
        END IF
        CALL SPOUT(1,SPD,NSTM,RANGE)
      END DO !IS
      END DO !IG
      END DO !IDCR
      END DO !IEANG
C
      END SUBROUTINE TIPES

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   RIPES (RESONANT INVERSE PHOTOEMISSION SPECTRA)
C
C  The configuration table in a X-card must be arranged in an order:
C  the initial "i", intermedate "m", final "f_i" and Auger states "A".
C  The Auger state is only required for RIPESf mode.
C  If the Auger state is neglected, in the RIPESf mode, the state 
C  is assumed to be identical to the initial state "i".
C
C  The operators should be arranged in an order: 
C  H_i, H_m, H_f1, H_f2,..., H_fN, T_D1, T_D2,..., T_DN, T_R1, T_R2,..., T_RN,
C  T_A, [ T_D'1, T_D'2,..., T_D'N, T_A' ]
C  The operator "T_A'", which is an Auger decay matrix from the 
C  intermediate state to the Auger state "A", and "T_D'i", which are 
C  electric dipole transtion matrices from the final states "f_i"
C  to the Auger state "A" are only needed for the RIPESf mode.
C  If the Auger state is neglected, these operators are considered to
C  be identical to "T_A" and "T_Di"'s, in the RIPESf mode
C
C  Notice:  The operators should be defined reverse order for 
C           RIPES/RIPESf modes: final to initial state, final to 
C           intermediate and so on.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RIPES(MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,U,NDLT,DLT,EADJ,ERED
     &            ,GAM,GALF
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR, NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2
     &            ,TMW3,TMW4,TMW5,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &            ,ASTS,ACI,AOBT)
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
      REAL*8  RED, GAM, GALF
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
   
      INTEGER NSPN(2)

      REAL*8  RANGE(4,NSTM) 
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)
     
      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM),TMW1(NBMM),TMW2(NBMM)
      REAL*8  TMW3(NBMM),TMW4(NBMM),TMW5(NBMM),TMW6(NBMM),TMW7(NBMM)
      REAL*8  TMW8(NBMM),TMW9(NBMM),TMW10(NBMM),TMW11(NBMM),TMW12(NBMM)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER NF(NSTM)
      INTEGER NA(1)
      INTEGER IEPH,NEPH,IEANG,NEANG,IG,IDCR,NDCR1,IS,NE1,NE2
      INTEGER I,IO,IO1,IO2,IO3,IEX,IEX1,IEX2,IEX3,NPX
      INTEGER ISHL,IOP,LEX,NSDEG
      INTEGER NE,N1,N2,N3,NR,NSHL,NHTOP,ISTS,IST
      INTEGER H_i, H_m, H_f, T_D, T_R, T_A, T1_A, T1_D, H_A
      REAL*8  S, E, YRE, YIM, APS
      LOGICAL AUGER 

C Function
      INTEGER NTOLEL
C -----------------------

      AUGER=.TRUE.
      IF (NCI(0).LT.3) THEN
        WRITE(6,*)'Too few states for RIPES/RIPESf mode' 
      END IF
      N1=NSFT(NCI(1)+1,1)
      N2=NSFT(NCI(2)+1,2)
C
C Check for consistancy of the electron numbers among the states 
C
      NSHL=NCI(0)-3 
      NE1=NTOLEL( 1      ,NSTM,NCIM,NCI,NOBT,NELC)
      NE2=NTOLEL( NCI(0) ,NSTM,NCIM,NCI,NOBT,NELC)
      IF ( NE2 .EQ. NE1+1 ) THEN 
        WRITE(6,*)
     &   'The initial state will be regarded as the Auger state'
        NSHL=NSHL+1
        AUGER=.FALSE.
      END IF
C
      H_i =1
      H_m =2 
      H_f =3
      T_D =2+NSHL+1
      T_R =2+NSHL+NSHL+1
      T_A =2+NSHL+NSHL+NSHL+1

      T1_A=2+NSHL+NSHL+NSHL+1+NSHL+1
      IF (.NOT.AUGER) T1_A=T_A 

      T1_D=2+NSHL+NSHL+NSHL+1+1
      IF (.NOT.AUGER) T1_D=T_D 

      H_A =NCI(0)
      IF (.NOT.AUGER) H_A=1

      DO ISHL=1,NSHL
        NF(ISHL)=NSFT( NCI(H_f + ISHL-1)+1, H_f + ISHL-1 )
        IF ( NINT( RANGE(1,ISHL) ) .EQ. 0 ) THEN  ! When NPX=0
          DO I=1,4
            RANGE(I,ISHL)=RANGE(I,1)
          END DO
        END IF 
      END DO
      NA(1)=NSFT( NCI(H_A)+1, H_A)
C
C Create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=T_D, NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
      NHTOP=IHP1(1)

      CALL NSUBS(NDCR1,T_R, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      NEANG=NINT( EANG(0) )
      CALL FLHEAD(MODE,NMODE,1,NDCR,CDCR,NSPN(1),NSPN(2),NSHL
     &           ,NG,EG,EPH,EANG)
C
C Main Loop 
C
      IF (MODE(1:3).EQ.'aps') WRITE(2,*)'SPCD'
C
      NEPH=NINT( EPH(0) ) 
      DO IEPH=1,NEPH
      DO IG=1,NG
C------------------------------------------
C With "Eang" command in the item "EXEC:"
C------------------------------------------
        IF (NEANG.NE.0) THEN
          DO IEANG=1,NEANG
          DO IS=NSPN(1),NSPN(2)
            CALL CLRV(N2,TMR)
            DO IO=NOBT+1,NOBTX
              IF (MOD(NDEG(IO),2).EQ.1) THEN
                LEX=NDEG(IO)/2
                NSDEG=1
              ELSE IF (MOD(NDEG(IO),4).EQ.2) THEN
                LEX=NDEG(IO)/4
                NSDEG=2
              ELSE
                WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                STOP
              END IF
              DO IEX=0, NDEG(IO)-1, NSDEG
                CALL Y( LEX, IEX/NSDEG - LEX
     &                , EANG(2*IEANG-1), EANG(2*IEANG), YRE, YIM )
                IF ( MOD( LEX ,2) .EQ.1) THEN
                  YRE=-YRE
                  YIM=-YIM
                END IF
                CALL MOVV(N1,T1,UG(1,IG))
                CALL MULS(N1,T1,YRE)
                IF (MOD(NMODE,10).NE.0) THEN
                  CALL EXCV(N1,T1,UG(1,IG),-YIM)
                END IF
C Inverse Auger Decay
                IO1=IO
                IEX1=IEX
                IF ( MOD(IS,NSDEG) .EQ. 1 )
     &            CALL OPRT(N2,TMR,N1,T1,T_A
     &                     ,IO1,IEX1,  0, NHOP,NHM,H,IHP2,IHP1)
                IF ( IS/NSDEG .EQ. 1 )
     &            CALL OPRT(N2,TMR,N1,T1,T_A
     &                     ,IO1,IEX1+1,0, NHOP,NHM,H,IHP2,IHP1)
              END DO !IEX
            END DO !IO
            CALL CLRV(N2,TMI)
            CALL MULS(N2,TMR,-1D0)
C
C Create Intermediate State Hamiltonian
C
            IHP1(1)=NHTOP
            IHP2(1)=NHTOP
            CALL WATCH('HGEN    ')
            CALL HGEN(NBSM,T1, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
            CALL HELV( H_m, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &               ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
            CALL WATCH('END     ')
C
C Intermediate State Green's Function 
C
            E=EPH(IEPH)+EG(IG)
            ISTS=1
            IF (MODE.EQ.'ripesf'.OR.MODE.EQ.'apsf') ISTS=2
            WRITE(6,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
            WRITE(2,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
            CALL WATCH('INVM    ')
            CALL INVM(N2,TMR,TMI, H_m
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,E,GAM,GALF
     &               ,NA, T1_A,1,NDEG,NOBT,NOBTX,ISTS,NMODE
     &               ,NBMM,TMW1,TMW2,TMW3,TMW4,TMW5
     &               ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12)
            CALL WATCH('END     ')
            CALL MAGT( H_m, ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &               ,NBMM,TMR,NMODE, NBMM,TMW1,TMW2
     &               ,ASTS,ACI,AOBT)
            CALL CNFMIX( H_m, NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)
C
C Final state Hamiltonian
C
            DO ISHL=1,NSHL
              IHP1(1)=NHTOP
              IHP2(1)=NHTOP
              CALL WATCH('HGEN    ')
              CALL HGEN(NBSM,T1, H_f + ISHL-1
     &                 ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &                 ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
              CALL HELV( H_f + ISHL-1 
     &                 ,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &                 ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
              CALL WATCH('END     ')
              N3  =NF(ISHL)
              NPX =NINT( RANGE(1,ISHL) )
              DO IDCR=0,NDCR1-1
                CALL CLRV(N3,T1)
C
C Real part of interference term
C
                DO IO=NOBT+1,NOBTX
                  DO IEX=0, NDEG(IO)-1
                    CALL CLRV(NA(1),T2)
                    IO1=IO 
                    IEX1=IEX
                    CALL OPRT(NA(1),T2,N2,TMI, T1_A
     &                       ,IO1,IEX1,0,    NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,   T1,NA(1),T2,  T1_D + ISHL-1
     &                       ,IO1,IEX1,IDCR, NHOP,NHM,H,IHP2,IHP1)
                  END DO
                END DO
                CALL MULS(N3,T1,PI*RHO)
C
C Direct Process
C
                DO IO=NOBT+1,NOBTX

                  IF (MOD(NDEG(IO),2).EQ.1) THEN
                    LEX=NDEG(IO)/2
                    NSDEG=1
                  ELSE IF (MOD(NDEG(IO),4).EQ.2) THEN
                    LEX=NDEG(IO)/4
                    NSDEG=2
                  ELSE
                    WRITE(6,*)'Error! Wrong usage in "Eang" command!'
                    STOP
                  END IF

                  DO IEX=0, NDEG(IO)-1, NSDEG
                    CALL Y( LEX , IEX/NSDEG - LEX
     &                   ,EANG(2*IEANG-1),EANG(2*IEANG),YRE,YIM )
                    IF ( MOD( LEX, 2 ) .EQ.1 ) THEN
                      YRE=-YRE
                      YIM=-YIM
                    END IF
                    CALL MOVV(N1,T2,UG(1,IG))
                    CALL MULS(N1,T2,YRE)
                    IF (MOD(NMODE,10).NE.0) THEN 
                      CALL EXCV(N1,T2,UG(1,IG),-YIM)
                    END IF

                    IO1=IO
                    IEX1=IEX
                    IF ( MOD(IS,NSDEG) .EQ. 1) 
     &                CALL OPRT(N3,T1,N1,T2, T_D + ISHL-1
     &                         ,IO1, IEX1  ,IDCR,NHOP,NHM,H,IHP2,IHP1)
                    IF ( IS/NSDEG      .EQ. 1) 
     &                CALL OPRT(N3,T1,N1,T2, T_D + ISHL-1
     &                         ,IO1, IEX1+1,IDCR,NHOP,NHM,H,IHP2,IHP1)

                  END DO !IEX
                END DO !IO
C
C Real part of Resonance Process
C
                CALL OPRT(N3,T1,N2,TMR, T_R + ISHL-1
     &                   , 0  ,0   ,IDCR,NHOP,NHM,H,IHP2,IHP1)
C
C Spectral Density (real part)
C
                IF (MODE(1:3).EQ.'aps') THEN
C --- Real part of APS
                  CALL ABSV(N3,T1,S)
                  APS=S**2
                ELSE
C --- Real part of RIPES 
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
                  CALL MOVV(NPX+1,SPD,SPI)
                END IF
                CALL CLRV(N3,T1)
C
C Imaginary Part of Interference Term
C
                DO IO=NOBT+1,NOBTX
                  DO IEX=0,NDEG(IO)-1
                    CALL CLRV(NA(1),T2)
                    IO1=IO
                    IEX1=IEX
                    CALL OPRT(NA(1),T2,N2,TMR  , T1_A
     &                       ,IO1,IEX1,0   , NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,   T1,NA(1),T2,  T1_D + ISHL-1
     &                       ,IO1,IEX1,IDCR, NHOP,NHM,H,IHP2,IHP1)
                  END DO !IEX
                END DO !IO
                CALL MULS(N3,T1,-PI*RHO)
C
C Imaginary Part of Resonance Process
C
                CALL OPRT(N3, T1,N3,TMI, T_R + ISHL-1
     &                   , 0  ,0   ,IDCR, NHOP,NHM,H,IHP2,IHP1)
C
C Spectral Density (Imaginary Part)
C
                IF (MODE(1:3).EQ.'aps') THEN
C ---Imaginary Part of APS
                  CALL ABSV(N3,T1,S)
                  APS=APS+S**2
                ELSE
C ---Imaginary Part of RIPES 
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
                  CALL ADDV(NPX+1,SPD,SPI)
                  CALL SPOUT(ISHL,SPD,NSTM,RANGE)
                END IF
              END DO !IDCR
            END DO !ISHL 
          END DO !IS
          END DO !IEANG
        ELSE
C------------------------------------------
C No "Eang" command in the item "EXEC:"
C------------------------------------------
          DO ISHL=1,NSHL
          DO IS=NSPN(1),NSPN(2)
          DO IDCR=0,NDCR1-1
            N3 =NF(ISHL)
            NPX=NINT( RANGE(1,ISHL) )
            IF (MODE(1:3).EQ.'aps') THEN 
              APS=0D0
            ELSE
              CALL CLRV(NPX+1,SPD)
            END IF

            NSDEG=MOD( NDEG(NOBT+1)+1 ,2 )+1 
            DO IO=NOBT+1,NOBTX
              DO IEX=0,NDEG(IO)-1,NSDEG

                CALL CLRV(N2,TMR)
                CALL CLRV(N2,TMI)
                IO1=IO
                IEX1=IEX
                IF ( MOD(IS,NSDEG) .EQ. 1 ) 
     &            CALL OPRT(N2,TMR,N1,UG(1,IG), T_A, IO1,IEX1,  0
     &                     ,NHOP,NHM,H,IHP2,IHP1)
                IF ( IS/NSDEG .EQ. 1 ) 
     &            CALL OPRT(N2,TMR,N1,UG(1,IG), T_A, IO1,IEX1+1,0
     &                     ,NHOP,NHM,H,IHP2,IHP1)

                CALL MULS(N2,TMR,-1D0)
C
C Create Intermediate State Hamiltonian
C
                IHP1(1)=NHTOP
                IHP2(1)=NHTOP
                CALL WATCH('LANCZ   ')
                CALL HGEN(NBSM,T1, H_m
     &                   ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &                   ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
                CALL HELV( H_m, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &                   ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
                CALL WATCH('END     ')
C
C Intermediate State Green's Function 
C
                E=EPH(IEPH)+EG(IG)
                ISTS=1
                IF (MODE.EQ.'ripesf'.OR.MODE.EQ.'apsf') ISTS=2
                WRITE(6,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
                WRITE(2,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
                CALL WATCH('INVM    ')
                CALL INVM(N2,TMR,TMI,H_m
     &                ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,E,GAM,GALF
     &                ,NA, T1_A, 1,NDEG,NOBT,NOBTX,ISTS,NMODE
     &                ,NBMM,TMW1,TMW2,TMW3,TMW4,TMW5
     &                ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12)
                CALL WATCH('END     ')
                CALL MAGT( H_m, ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &                   ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &                   ,NBMM,TMR,NMODE, NBMM,TMW1,TMW2
     &                   ,ASTS,ACI,AOBT)
                CALL CNFMIX( H_m, NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)
C
C Final State Hamiltonian
C
                IHP1(1)=NHTOP
                IHP2(1)=NHTOP
                CALL WATCH('HGEN    ')
                CALL HGEN(NBSM,T1, H_f + ISHL-1
     &                 ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &                 ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
                CALL HELV( H_f + ISHL-1
     &                   ,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &                   ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
                CALL WATCH('END     ') 
C
C Real Part of Interference Term 
C
                CALL CLRV(N3,T1) 
                DO IO2=NOBT+1,NOBTX 
                  DO IEX2=0,NDEG(IO)-1
                    CALL CLRV(NA(1),T2)
                    IO3=IO2
                    IEX3=IEX2
                    CALL OPRT(NA(1),T2, N2,   TMI, T1_A
     &                       ,IO3,IEX3,0,    NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3   ,T1, NA(1),T2,  T1_D + ISHL-1
     &                       ,IO3,IEX3,IDCR, NHOP,NHM,H,IHP2,IHP1)
                  END DO !IEX2
                END DO !IO2
                CALL MULS(N3,T1,PI*RHO)
C
C Direct Process
C
                IO1=IO
                IEX1=IEX
                IF ( MOD(IS,NSDEG) .EQ. 1 ) 
     &            CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                     ,IO1,IEX1,  IDCR ,NHOP,NHM,H,IHP2,IHP1)
                IF ( IS/NSDEG .EQ. 1 ) 
     &            CALL OPRT(N3,T1,N1,UG(1,IG), T_D + ISHL-1
     &                     ,IO1,IEX1+1,IDCR ,NHOP,NHM,H,IHP2,IHP1)
C
C Real Part of Resonance Process
C
C T_R (inversed)
                CALL OPRT(N3,T1,N2,TMR, T_R + ISHL-1
     &                   , 0  ,0   ,IDCR ,NHOP,NHM,H,IHP2,IHP1)
C
C Spectral Density (real part)
C
                IF (MODE(1:3).EQ.'aps') THEN
C ---Real Part of RIPES 
                  CALL ABSV(N3,T1,S)
                  APS=APS+S**2
                ELSE
C ---Real Part of RIPES 
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
                  CALL ADDV(NPX+1,SPD,SPI)
                END IF
                CALL CLRV(N3,T1)
C
C Imaginary Part of Interference Term
C
                DO IO2=NOBT+1,NOBTX
                  DO IEX2=0,NDEG(IO)-1
                    CALL CLRV(NA(1),T2)
                    IO3=IO2
                    IEX3=IEX2
                    CALL OPRT(NA(1),T2,N2,TMR, T1_A
     &                       ,IO3,IEX3,0   , NHOP,NHM,H,IHP1,IHP2)
                    CALL OPRT(N3,T1,NA(1),T2,  T1_D + ISHL-1
     &                       ,IO3,IEX3,IDCR, NHOP,NHM,H,IHP2,IHP1)
                  END DO !IEX2
                END DO !IO2
                CALL MULS(N3,T1,-PI*RHO)
C
C Imaginary Part of Resonance Process
C
                CALL OPRT(N3,T1,N2,TMI, T_R + ISHL-1
     &                   , 0  ,0   ,IDCR,  NHOP,NHM,H,IHP2,IHP1)
C
C Spectral Density (Imaginary Part)
C
                IF (MODE(1:3).EQ.'aps') THEN
C ---Imaginary Part of APS
                  CALL ABSV(N3,T1,S)
                  APS=APS+S**2
                ELSE
C ---Imaginary Part of RIPES 
                  CALL WATCH('LANCZ   ')
                  CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                      ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                      ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
                  CALL WATCH('END     ')
                  CALL RECUR(ISHL,SPR,SPI,EG(IG),NSTM,RANGE,NR,AR,BR)
                  CALL ADDV(NPX+1,SPD,SPI)
                END IF
              END DO !IEX
            END DO !IO
            CALL SPOUT(ISHL,SPD,NSTM,RANGE)
          END DO !IDCR
          END DO !IS
          END DO !ISHL
        END IF
      END DO !IG
      END DO !IEPH
C
      END SUBROUTINE RIPES

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   XES   (X-RAY EMISSION SPECTRA)
C
C  The configuration table in a X-card must be arranged in an order:
C  the initial "i", intermedate "m", final "f_i" and Auger states "A".
C  The Auger state is only required for XESf mode.
C  If the initial state and the final state is identical,
C  you can ommit the final state.
C
C  In X-cards, operators should be arranged in an order:
C  H_i, H_m, H_f1, H_f2,..., H_fN, T_R, T_R1, T_R2,..., T_RN, 
C  [ T_A, T_D, T_D1, T_D2,..., T_DN ].
C
C  If you ommit the final state in the configuration table,
C  in this case, the operators must be appeared in an order:
C  H_i, H_m, T_R, T_A, T_D.
C
C  The operator "T_A", which is an Auger decay matrix from the 
C  intermediate state to the Auger state "A", and "T_Di", which are 
C  electric dipole transtion matrices from the final states "f_i"
C  to the Auger state "A" are only needed for the RIPESf mode.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XES(MODE,NMODE
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,U,NDLT,DLT,EADJ,ERED
     &            ,GAM,GALF
     &            ,NGM,NBGM,NG,EG,UG
     &            ,ORBM,EPH,EANG,NDCR,CDCR
     &            ,RANGE,SPR,SPI,SPD
     &            ,NREC,AR,BR, NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2
     &            ,TMW3,TMW4,TMW5,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12
     &            ,ASTS,ACI,AOBT)
      IMPLICIT NONE

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM, NPMAX, NRECM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

      REAL*8 PI,RHO
      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
      REAL*8  GAM, GALF
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      REAL*8  RANGE(4,NSTM)
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)

      INTEGER NREC
      REAL*8  AR(0:NRECM),BR(0:NRECM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM),TMW1(NBMM),TMW2(NBMM)
      REAL*8  TMW3(NBMM),TMW4(NBMM),TMW5(NBMM),TMW6(NBMM),TMW7(NBMM)
      REAL*8  TMW8(NBMM),TMW9(NBMM),TMW10(NBMM),TMW11(NBMM),TMW12(NBMM)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER NF(NSTM)
      INTEGER NA(1)
      INTEGER IEPH,NEPH,NEANG,IG,IDCR1,IDCR2,NDCR1,NDCR2
      INTEGER I,IO,IO1,IEX,IEX1,ISHL,IOP,NPX
      INTEGER N1,N2,N3,NR,NSHL,NHTOP,ISTS,NSCV
      INTEGER H_i, H_m, H_f, H_A, T_R, T_R1, T_A, T_D, T_D1
      REAL*8 E
C -----------------------
 
      N1=NSFT(NCI(1)+1,1)
      N2=NSFT(NCI(2)+1,2)
        
      NSHL=NCI(0)-2
      IF(MODE.EQ.'xesf') NSHL=NCI(0)-3

      H_i = 1
      H_m = 2
      H_f = 3
      H_A = 2+NSHL+1 
      T_R = 2+NSHL+1 
      T_R1= 2+NSHL+2
      T_A = 2+NSHL+1+NSHL+1
      T_D = 2+NSHL+1+NSHL+2
      T_D1= 2+NSHL+1+NSHL+3
  
      IF (NSHL.EQ.0) THEN
        H_f=H_i 
        T_D1=T_D
        T_R1=T_R
        NSHL=1
      END IF

      DO ISHL=1, NSHL
        NF(ISHL)=NSFT( NCI(H_f + ISHL-1)+1, H_f + ISHL-1 )
        IF ( NINT( RANGE(1,ISHL) ) .EQ. 0 ) THEN
          DO I=1,4
            RANGE(I,ISHL)=RANGE(I,1)
          END DO
        END IF 
      END DO

      NA(1)=0
      IF (MODE.EQ.'xesf') THEN
        NA(1)=NSFT( NCI(H_A)+1, H_A )
      END IF
C
C create transition matrices  
C
      CALL WATCH('TRNSMAT ')
      DO IOP=T_R,NHOP(0)
        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
      NHTOP=IHP1(1)

      CALL NSUBS(NDCR1,T_R,  1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      IF ( T_R.EQ.T_R1 ) THEN
        CALL NSUBS(NDCR2,T_R1, 2,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      ELSE
        NDCR2=NDCR1
      END IF

      NEANG=NINT( EANG(0) )
      CALL FLHEAD(MODE,NMODE,2,NDCR,CDCR,0,-1,NSHL
     &           ,NG,EG,EPH,EANG)
C
C main loop
C
      NEPH=NINT( EPH(0) ) 
      DO IEPH=1,NEPH
      DO IDCR1=0,NDCR1-1
      DO IG=1,NG
C create intermediate state Hamiltonian
        IHP1(1)=NHTOP
        IHP2(1)=NHTOP
        CALL WATCH('HGEN    ')
        CALL HGEN(NBSM,T1, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
        CALL HELV( H_m, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &           ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
        CALL WATCH('END     ')
C dipole transition (core to valence, real part)
        CALL CLRV(N2,TMR)
        CALL OPRT(N2,TMR,N1,UG(1,IG), T_R, 0,0,IDCR1
     &           ,NHOP,NHM,H,IHP1,IHP2)
C interference term (imaginaly part)
        CALL CLRV(N2,TMI)
        IF (MODE.EQ.'xesf') THEN
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              CALL CLRV(NA(1),T1)
              IO1=IO
              IEX1=IEX
              CALL OPRT(NA(1),T1,N1,UG(1,IG), T_D
     &                 ,IO1,IEX1,IDCR1, NHOP,NHM,H,IHP1,IHP2)
              CALL OPRT(N2,TMI,NA(1),T1,      T_A
     &                 ,IO1,IEX1,0,     NHOP,NHM,H,IHP2,IHP1)
            END DO !IEX
          END DO !IO
          CALL MULS(N2,TMI,-PI*RHO)
        END IF
C intermediate state Hamiltonian
        CALL WATCH('INVM    ')
        ISTS=1
        IF (MODE.EQ.'xesf') ISTS=2
        E=EPH(IEPH)+EG(IG)
        WRITE(6,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
        WRITE(2,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
        CALL INVM(N2,TMR,TMI, H_m, NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,E,GAM,GALF
     &           ,NA, T_A, 1,NDEG,NOBT,NOBTX,ISTS,NMODE
     &           ,NBMM,TMW1,TMW2,TMW3,TMW4,TMW5
     &           ,TMW6,TMW7,TMW8,TMW9,TMW10,TMW11,TMW12)
        CALL WATCH('END     ')
        CALL MAGT( H_m, ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,NBMM,TMR,NMODE, NBMM,TMW1,TMW2
     &           ,ASTS,ACI,AOBT)
        CALL CNFMIX( H_m, NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)
        DO ISHL=1,NSHL
          E=EG(IG)
          N3 =NF(ISHL)
          NPX=NINT( RANGE(1,ISHL) )
C create final state Hamiltonian
          IHP1(1)=NHTOP
          IHP2(1)=NHTOP
          CALL WATCH('HGEN    ')
          CALL HGEN(NBSM,T1, H_f + ISHL-1
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
          CALL HELV( H_f + ISHL-1, NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &             ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
          CALL WATCH('END     ')
          DO IDCR2=0,NDCR2-1
C interference term (real part)
            CALL CLRV(N3,T1)
            IF (MODE.EQ.'xesf') THEN
              DO IO=NOBT+1,NOBTX
                DO IEX=0,NDEG(IO)-1
                  IO1=IO
                  IEX1=IEX 
                  CALL CLRV(NA(1),T2)
                  CALL OPRT(NA(1),T2,N2,TMI,    T_A
     &                     ,IO1,IEX1,0,     NHOP,NHM,H,IHP1,IHP2)
                  CALL OPRT(N3,   T1,NA(1),T2,  T_D1 + ISHL-1
     &                     ,IO1,IEX1,IDCR2, NHOP,NHM,H,IHP2,IHP1)
                END DO !IEX
              END DO !IO
              CALL MULS(N3,T1,PI*RHO)
            END IF
C dipole transition (valence to core, real part)
            CALL OPRT(N3,T1,N2,TMR, T_R1 + ISHL-1
     &               ,0,0,IDCR2,NHOP,NHM,H,IHP2,IHP1)
            CALL WATCH('LANCZ   ')
            CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
            CALL MOVV(NPX+1,SPD,SPI)
C interference term (imaginaly part)
            CALL CLRV(N3,T1)
            IF (MODE.EQ.'xesf') THEN
              DO IO=NOBT+1,NOBTX
                DO IEX=0,NDEG(IO)-1
                  CALL CLRV(NA(1),T2)
                  IO1=IO
                  IEX1=IEX 
                  CALL OPRT(NA(1),T2,N2,TMR, T_A
     &                     ,IO1,IEX1,0,    NHOP,NHM,H,IHP1,IHP2)
                  CALL OPRT(N3,T1,NA(1),T2,  T_D1 + ISHL-1
     &                     ,IO1,IEX1,IDCR1,NHOP,NHM,H,IHP2,IHP1)
                END DO !IEX
              END DO !IO
            END IF
C non-resonant term  (go to continuum states and then return)
            IF (MODE.EQ.'xesf') THEN
              DO IO=NOBT+1,NOBTX
                DO IEX=0,NDEG(IO)-1
                  CALL CLRV(NA(1),T2)
                  IO1=IO
                  IEX1=IEX
                  CALL OPRT(NA(1),T2,N1,UG(1,IG), T_D
     &                     ,IO1,IEX1,IDCR1, NHOP,NHM,H,IHP1,IHP2)
                  CALL OPRT(N3,T1,NA(1),T2,       T_D1 + ISHL-1
     &                     ,IO1,IEX1,IDCR2, NHOP,NHM,H,IHP2,IHP1)
                END DO !IEX
              END DO !IO
            END IF
C multiply a factor -pi*rho to the imaginally parts  
            CALL MULS(N3,T1,-PI*RHO)
C dipole transition (valence to core, imaginaly part)
            CALL OPRT(N3,T1,N2,TMI, T_R1 + ISHL-1
     &               ,0,0,IDCR2,NHOP,NHM,H,IHP2,IHP1)
            CALL WATCH('LANCZ   ')
            CALL LANCZ(N3,T1,T2, H_f + ISHL-1
     &                ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
            CALL WATCH('END     ')
            CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
            CALL ADDV(NPX+1,SPD,SPI)
C
            CALL SPOUT(ISHL,SPD,NSTM,RANGE)
          END DO !IDCR2
        END DO !ISHL
      END DO !IG
      END DO !IDCR1
      END DO !IEPH
C
      END SUBROUTINE XES

C////////////////////////// Cation! //////////////////////////
C   MOD XES   (modified version of X-RAY EMISSION SPECTRA)
C  
C  In X-cards, the operators should be arranged in a order:
C  H_i, H_m, H_f1, H_f2,..., H_fN, T_R, T_R1, T_R2,..., T_RN
C  [ T_A, T_D, T_D1, T_D2,..., T_DN ]
C
C////////////////////////// Cation! //////////////////////////
C      SUBROUTINE MODXES(MODE,NMODE
C     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
C     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &            ,U,NDLT,DLT,EADJ,ERED
C     &            ,GAM,GALF
C     &            ,NGM,NBGM,NG,EG,UG
C     &            ,ORBM,EPH,EANG,NDCR,CDCR
C     &            ,RANGE,SPR,SPI,SPD
C     &            ,NREC,AR,BR, NBSM,T1,T2, NBMM,TMR,TMI )
C      IMPLICIT NONE
C
CC Parameters
C      INTEGER   NOBTM
C      PARAMETER(NOBTM=120)
C
C      INTEGER   NHOPM
C      PARAMETER(NHOPM=100)
C
C      INTEGER   NDLTM
C      PARAMETER(NDLTM=200)
C
C      INTEGER NEPHM, NEANGM, NDCRM, NPMAX, NRECM
C      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200,NPMAX=4000,NRECM=2000)
C
C      REAL*8    PI,RHO
C      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)
C
CC Dummy arguments
C      CHARACTER*(8) MODE
C      INTEGER NMODE
C
C      INTEGER NSTM,NCIM
C      INTEGER NCI(0:NSTM)
C      INTEGER NSFT(NCIM+1,NSTM)
C      INTEGER NOBT, NOBTX
C      INTEGER NELC(NOBT,NCIM,NSTM)
C      INTEGER NDEG(NOBTM)
C
C      INTEGER NHOP(0:NHOPM)
C      INTEGER NHM
C      REAL*8  H(NHM)
C      INTEGER IHP1(NHM),IHP2(NHM)
C      INTEGER NHDM
C      REAL*8  HD(NHDM)
C     
C      REAL*8  U(NOBTM,NOBTM)
C      INTEGER NDLT(0:NDLTM*10)
CC      REAL*8  DLT(NDLTM)
C      REAL*8  EADJ(NSTM*3+3)
C      REAL*8  ERED(NCIM)
C      REAL*8  GAM, GALF
C    
C      INTEGER NGM, NBGM, NG
C      REAL*8  EG(NGM)
C      REAL*8  UG(NBGM,NGM)
C
C      REAL*8  ORBM(0:NOBTM*7)
C
C      REAL*8  EPH(0:NEPHM)
C      REAL*8  EANG(0:NEANGM)
C      INTEGER NDCR(2)
C      CHARACTER*(8) CDCR(NDCRM)
C
C      REAL*8  RANGE(4,NSTM)
C      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX),SPD(0:NPMAX)
C
C      INTEGER NREC
C      REAL*8  AR(0:NRECM),BR(0:NRECM)
C
C      INTEGER NBSM
C      REAL*8  T1(NBSM),T2(NBSM)
C
C      INTEGER NBMM
C      REAL*8  TMR(NBMM),TMI(NBMM)
C
CC Global variables
C      REAL*8  TMW1(NBMM),TMW2(NBMM)
C      REAL*8  TM3(NBMM),TM4(NBMM),TM5(NBMM),TM6(NBMM)
C      REAL*8  TM7(NBMM),TM8(NBMM),TM9(NBMM),TM10(NBMM)
C      COMMON /IMWRK/TMW1,TMW2,TM3,TM4,TM5,TM6,TM7,TM8,TM9,TM10
C
CC Local variables
C      INTEGER NF(NSTM)
C      INTEGER NA(1)
C      INTEGER IEPH,NEPH,NEANG,IG,IDCR1,IDCR2,NDCR1,NDCR2
C      INTEGER IO,IO1,IEX,IEX1,ISHL,IOP,I,NTYP,NOP,NST,NPX
C      INTEGER N1,N2,N3,NR,NSHL,IERR,NHTOP,ISTS,NSCV,IST,NBLK
C      REAL*8 E
C      REAL*8 RR(NBLKM)
CC -----------------------
CC
C      N1=NSFT(NCI(1)+1,1)
C      N2=NSFT(NCI(2)+1,2)
CC      NSHL=NCI(0)-2
CC      IF(MODE.EQ.'modxesf') NSHL=NCI(0)-3
C      CALL ELCNT(NSCV,NOBT,NCI,NELC)
CC
C      IERR=0
C      IF (NSCV.NE.NSHL+2) THEN
C        WRITE(6,*)'Error! The electron number must be the same'
C        WRITE(6,*)' among the initial, intermediate and final states'
C        IERR=1
C      END IF
CC
C      IF (NCI(0).LT.3) THEN
C        WRITE(6,*)'Error! Too few states for XES.'
C        IERR=1
C      ELSE IF(MODE.EQ.'modxesf') THEN
C        IF (NHOP(0).NE.2+NSHL+1+NSHL+2+NSHL) THEN
C          WRITE(6,*)'Error! Wrong number of the operator for XESf mode'
CC          IERR=1
C        END IF
C      ELSE IF(NHOP(0).NE.2+NSHL+1+NSHL) THEN
C        WRITE(6,*)
C     &  'Error! The number of the operator is incorrect for XES mode'
C        IERR=1
C      END IF
CC
CC Check for number of basis functions
CC
C      CALL ERR_BSZ(2,NBMM,'NBMM',NSTM,NCIM,NCI,NSFT,IERR)
CC
C      DO ISHL=1,NSHL
C        CALL ERR_BSZ(2+ISHL,NBSM,'NBSM ',NSTM,NCIM,NCI,NSFT,IERR)
C        NF(ISHL)=NSFT(NCI(2+ISHL)+1,2+ISHL)
C        IF ( NINT( RANGE(1,ISHL) ) .EQ. 0 ) THEN
C          DO I=1,4
C            RANGE(I,ISHL)=RANGE(I,1)
C          END DO
C        END IF 
C      END DO
CC
C      NA(1)=0
C      IF (MODE.EQ.'modxesf') THEN
C        IST=2+NSHL+1
C        NA(1)=NSFT(NCI(IST)+1,IST)
C        CALL ERR_BSZ(IST,NBSM,'NBSM ',NSTM,NCIM,NCI,NSFT,IERR) 
C      END IF
CC
C      CALL CHKBLK(NSTM,NCIM,NCI,NOBT,NOBTX,NELC,NDEG
C     &                 ,NHOP,NHM,H,IHP1,IHP2
C     &                 ,NDLT,DLT,2+NSHL,NMODE,IERR)
C
C      IF (IERR.NE.0) RETURN 
CC!!!!!!!!!!!!!!!!!!!!!!!!!
C      NTYP=3
C      NBLK=1
C      RR(1)=4d0/DBLE(NDEG(NINT(ORBM(1))))
C      NOP=NHOP(0)+1
C      NST=2
C      CALL OP1GEN(NOP,NTYP,NST,ORBM,NBLK,RR,NHOP,NHM,H,IHP1,IHP2,NCI
C     &      ,NSTM,NCIM,NOBT,NELC,NDEG)
C      NHOP(0)=NOP
CC!!!!!!!!!!!!!!!!!!!!!!!!!!
CC
CC create transition matrices  
CC
C      CALL WATCH('TRNSMAT ')
C      DO IOP=2+NSHL+1,NHOP(0)
C        CALL HGEN(NBSM,T1,IOP+0,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
C     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
C      END DO
C      CALL WATCH('END     ')
C      NHTOP=IHP1(1)
CC
CC calculation for initial state
CC
C      CALL WATCH('GRND    ')
C      CALL GRND ( 1, MODE,NMODE
C     &          ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
C     &          ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &          ,U,NDLT,DLT,EADJ,ERED
C     &          ,NGM,NBGM,NG,EG,UG,ORBM
C     &          ,NBSM, T1,T2  )
C      CALL WATCH('END     ')
CC
C      CALL NSUBS(NDCR1,2+NSHL+1, 1,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
C      CALL NSUBS(NDCR2,2+NSHL+2, 2,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
C      NEANG=NINT( EANG(0) )
C      CALL FLHEAD(MODE,NMODE,2,NDCR,CDCR,0,-1,NSHL
C     &           ,NG,EG,EPH,EANG)
CC
C main loop
CCC
C      NEPH=NINT( EPH(0) ) 
C      DO IEPH=1,NEPH
CC      DO IDCR1=0,NDCR1-1
C      DO IG=1,NG
CC create intermediate state Hamiltonian
C        IHP1(1)=NHTOP
C        IHP2(1)=NHTOP
C        CALL WATCH('HGEN    ')
C        CALL HGEN(NBSM,T1,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
C     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
C        CALL HELV(2,NSTM,NCIM,NCI,NSFT,NOBT,NELC
C     &           ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
C        CALL WATCH('END     ')
CC dipole transition (core to valence, real part)
C        CALL CLRV(N2,TMR)
C        CALL OPRT(N2,TMR,N2,UG(1,IG),2+NSHL+1
C     &           ,0,0,IDCR1,NHOP,NHM,H,IHP1,IHP2)
CC interference term (imaginaly part)
C        CALL CLRV(N2,TMI)
CC        IF (MODE.EQ.'modxesf') THEN
CC          DO IO=NOBT+1,NOBTX
CC          DO IEX=0,NDEG(IO)-1
C            CALL CLRV(NA(1),T1)
C            IO1=IO
C            IEX1=IEX
C            IOP=2+NSHL+1+NSHL+2
C            CALL OPRT(NA(1),T1,NA(1),UG(1,IG),IOP,IO1,IEX1,IDCR1
C     &               ,NHOP,NHM,H,IHP1,IHP2)
C            IOP=2+NSHL+1+NSHL+1
C            CALL OPRT(N2,TMI,N2,T1,IOP,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
C          END DO !IEX
C          END DO !IO
C          CALL MULS(N2,TMI,-PI*RHO)
C        END IF
CC intermediate state Hamiltonian
C        CALL WATCH('INVM    ')
C        ISTS=1
C        IF (MODE.EQ.'modxesf') ISTS=2
C        IOP=2+NSHL+1+NSHL
C        E=EPH(IEPH)+EG(IG)
C        WRITE(6,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
C        WRITE(2,'(''Eph #='',I4,'' Eph='',G15.7)')IEPH,EPH(IEPH)
C        CALL MODINVM(N2,TMR,TMI,2,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &           ,E,GAM,GALF
CC     &           ,NA,IOP,1,NDEG,NOBT,NOBTX,ISTS,NMODE
C     &           ,NBMM,TR,TI,HPR,HPI,HDD,RR,RI,PR,PI,DG,TMW1,TMW2)
C        CALL WATCH('END     ')
C        CALL MAGT(2,ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
C     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
C     &           ,NBMM,TMR,NMODE, NBMM,TMW1,TMW2
C     &           ,ASTS,ACI,AOBT)
C        CALL CNFMIX(2,NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)
C        DO ISHL=1,NSHL
C          E=EG(IG)
C          N3=NF(ISHL)
C          NPX=NINT( RANGE(1,ISHL) )
CC create final state Hamiltonian
C          IHP1(1)=NHTOP
C          IHP2(1)=NHTOP
C          CALL WATCH('HGEN    ')
C          CALL HGEN(NBSM,T1,2+ISHL,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
C     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
C          CALL HELV(2+ISHL,NSTM,NCIM,NCI,NSFT,NOBT,NELC
C     &             ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
C          CALL WATCH('END     ')
C          DO IDCR2=0,NDCR2-1
CC interference term (real part)
C            CALL CLRV(N3,T1)
C            IF (MODE.EQ.'modxesf') THEN
C              DO IO=NOBT+1,NOBTX
C              DO IEX=0,NDEG(IO)-1
C                IO1=IO
C                IEX1=IEX 
C                CALL CLRV(NA(1),T2)
C                IOP=2+NSHL+1+NSHL+1
C                CALL OPRT(NA(1),T2,NA(1),TMI,IOP,IO1,IEX1,0
C     &                   ,NHOP,NHM,H,IHP1,IHP2)
C                IOP=2+NSHL+1+NSHL+2+ISHL
C                CALL OPRT(N3,T1,N3,T2,IOP,IO1,IEX1,IDCR2
C     &                   ,NHOP,NHM,H,IHP2,IHP1)
C              END DO !IEX
C              END DO !IO
C              CALL MULS(N3,T1,PI*RHO)
C            END IF
CC dipole transition (valence to core, real part)
C            CALL OPRT(N3,T1,N3,TMR,2+NSHL+1+ISHL,0,0,IDCR2
C     &               ,NHOP,NHM,H,IHP2,IHP1)
C            CALL WATCH('LANCZ   ')
C            CALL LANCZ(N3,T1,T2,2+ISHL,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
C            CALL WATCH('END     ')
C            CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
C            CALL MOVV(NPX+1,SPD,SPI)
CC interference term (imaginaly part)
C            CALL CLRV(N3,T1)
C            IF (MODE.EQ.'modxesf') THEN
C              DO IO=NOBT+1,NOBTX
C              DO IEX=0,NDEG(IO)-1
C                IO1=IO
C                IEX1=IEX 
C                CALL CLRV(NA(1),T2)
C                IOP=2+NSHL+1+NSHL+1
C                CALL OPRT(NA(1),T2,NA(1),TMR,IOP,IO1,IEX1,0
C     &                   ,NHOP,NHM,H,IHP1,IHP2)
C                IOP=2+NSHL+1+NSHL+2+ISHL
C                CALL OPRT(N3,T1,N3,T2,IOP,IO1,IEX1,IDCR1
C     &                   ,NHOP,NHM,H,IHP2,IHP1)
C              END DO !IEX
C              END DO !IO
C            END IF
CC non-resonant term  (go to continuum states and then return)
C            IF (MODE.EQ.'modxesf') THEN
C              DO IO=NOBT+1,NOBTX
C              DO IEX=0,NDEG(IO)-1
C                IO1=IO
C                IEX1=IEX
C                CALL CLRV(NA(1),T2)
C                IOP=2+NSHL+1+NSHL+2
C                CALL OPRT(NA(1),T2,NA(1),UG(1,IG),IOP,IO1,IEX1,IDCR1
C     &                   ,NHOP,NHM,H,IHP1,IHP2)
C                IOP=2+NSHL+1+NSHL+2+ISHL
C                CALL OPRT(N3,T1,N3,T2,IOP,IO1,IEX1,IDCR2
C     &                   ,NHOP,NHM,H,IHP2,IHP1)
C              END DO !IEX
C              END DO !IO
C            END IF
CC multiply a factor -pi*rho to the imaginally parts  
C            CALL MULS(N3,T1,-PI*RHO)
CC dipole transition (valence to core, imaginaly part)
C            CALL OPRT(N3,T1,N3,TMI,2+NSHL+1+ISHL,0,0,IDCR2
C     &               ,NHOP,NHM,H,IHP2,IHP1)
C            CALL WATCH('LANCZ   ')
C            CALL LANCZ(N3,T1,T2,2+ISHL,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &                ,NREC,NR,AR,BR,NSTM,NCIM,NCI,NSFT,NMODE)
C            CALL WATCH('END     ')
C            CALL RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NR,AR,BR)
C            CALL ADDV(NPX(ISHL)+1,SPD,SPI)
CC
CC            CALL SPOUT(ISHL,SPD,NSTM,RANGE)
C          END DO !IDCR2
C        END DO !ISHL
C      END DO !IG
C      END DO !IDCR
C      END DO !IEPH
CC
C      END SUBROUTINE MODXES
C
C
CC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    EXMLTP (get information about the final (intermediate) states 
C          MuLTiPlets)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EXMLTP(NSTATE,MODE,NMODE
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,U,NDLT,DLT,EADJ,ERED
     &           ,NGM,NBGM,NG,EG,UG
     &           ,ORBM,EPH,EANG,NDCR,CDCR
     &           ,NBSM,T1,T2, NBMM,TMR,TMI,TMW1,TMW2
     &           ,ASTS,ACI,AOBT)

      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM, NEANGM, NDCRM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      INTEGER NSTATE

      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM*3+3)
      REAL*8  ERED(NCIM)
    
      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM)

      INTEGER NBMM
      REAL*8  TMR(NBMM),TMI(NBMM)
      REAL*8  TMW1(NBMM),TMW2(NBMM)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      INTEGER I,N1,N2,IG,NGIN,IEPH,NEPH,IERR
      REAL*8  E,EGRND(1)
C -------------------------------

      IERR=0
      N1=NSFT(NCI(1)+1,1)
      N2=NSFT(NCI(NSTATE)+1,NSTATE)

      CALL FLHEAD(MODE,NMODE, 0, NDCR, CDCR,0,-1,0
     &           ,NGIN,EGRND,EPH,EANG)
C
C main loop
C
C create the final state Hamiltonian
      CALL WATCH('HGEN    ')
      CALL HGEN(NBSM,T1,NSTATE,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      CALL HELV(NSTATE,NSTM,NCIM,NCI,NSFT,NOBT,NELC
     &         ,U,NDLT,DLT,NHDM,HD,ERED,EADJ,NMODE)
      CALL WATCH('END     ')

      NEPH=NINT( EPH(0) )
      DO IEPH=1,NEPH,2

        E=EPH(IEPH)+EGRND(1)
        NGIN=NINT( EPH(IEPH+1) )

        WRITE(6,*)
     &    '==========================================================='
        WRITE(2,*)
     &    '==========================================================='
        WRITE(6,'(''>>>>>E_in #='',I4,'' E_in (Ef_in + Eg) ='',G15.7)')
     &    IEPH/2+1,EPH(IEPH)
        WRITE(2,'(''>>>>>E_in #='',I4,'' E_in (Ef_in + Eg) ='',G15.7)')
     &    IEPH/2+1,EPH(IEPH)

        CALL WATCH('EXEIGN  ')
        CALL EXEIGN(E,NSTATE,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,NSTM,NCIM,NCI,NSFT
     &             ,NGM,NBGM,NGIN,UG,EG
     &             ,NBSM,T1,T2,NMODE,NBMM,TMR,TMI,TMW1,TMW2)
        CALL WATCH('END     ')

        WRITE(6,'(''    Ef_in='',G15.7,'' Ef_ex='',G15.7)')
     &    EPH(IEPH)+EGRND(1),EG(1)
        WRITE(2,'(''    Ef_in='',G15.7,'' Ef_ex='',G15.7)')
     &    EPH(IEPH)+EGRND(1),EG(1)

        DO IG=1,NGIN
          DO I=1,N2
            TMR(I)=UG(I,IG)
          END DO
          WRITE(6,'('' Deg. #='',I5)')IG
          WRITE(2,'('' Deg. #='',I5)')IG
          CALL MAGT(NSTATE,ORBM,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &             ,NBMM,TMR,NMODE, NBSM,T1,T2
     &             ,ASTS,ACI,AOBT)
          CALL CNFMIX(NSTATE,NSTM,NCIM,NCI,NSFT,TMR,ASTS,ACI,AOBT)
        END DO !IG
      END DO !IEPH

      END SUBROUTINE EXMLTP

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C and add the result to a vector X.
C  X = X + 1/(H-E) B
C called by the subroutine EXEIGN.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADIM(N,X,B,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NSTM,NCIM,NCI,NSFT,NMODE
     &                ,P,R,AP,AR)
      IMPLICIT NONE
C
C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C  -local parameters
      REAL*8     EPSCR
      PARAMETER(EPSCR=1D-12)

C Dummy arguments
      INTEGER N
      REAL*8  X(N),B(N)
      REAL*8  E

      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
   
      INTEGER NMODE

      REAL*8 P(N),R(N),AP(N),AR(N)
C
C Local variables
      INTEGER I,COUNT
      REAL*8 ALF,BTA
      REAL*8 ABSB,ABSR,S,T
C-------------------------------------------------
C initialization
C      WRITE(6,*)' now in MADIM E=',E,' NOP=',NOP
      CALL ADDV(N,X,B)
      CALL MULM(N,R,B,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &         ,NSTM,NCIM,NCI,NSFT,NMODE)
      DO I=1,N
        T=B(I)-R(I)
        R(I)=T
        P(I)=T
      END DO
      CALL MULM(N,AP,P,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &         ,NSTM,NCIM,NCI,NSFT,NMODE)
      CALL ABSV(N,B,ABSB)
C
C main loop
      CALL ABSV(N,R,ABSR)
      COUNT=0
      DO WHILE(ABSR/ABSB.GT.EPSCR.AND.COUNT.LE.N)
        COUNT=COUNT+1
        CALL MULV(N,AP,R,ALF)
        CALL ABSV(N,AP,S)
        ALF= ALF/S**2
        CALL MADV(N,X, P, ALF)
        CALL MADV(N,R,AP,-ALF)
        CALL ABSV(N,R,ABSR)
C   
        CALL MULM(N,AR,R,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,NSTM,NCIM,NCI,NSFT,NMODE)
        CALL MULV(N,AP,AR,BTA)
        BTA=-BTA/S**2
        DO I=1,N
            P(I)= R(I)+BTA* P(I)
           AP(I)=AR(I)+BTA*AP(I)
        END DO
C        WRITE(6,*)' COUNT=',COUNT,' ERR=',ABSR/ABSB,ABSR,ABSB,ALF,BTA,S
      END DO
      WRITE(6,*)'     <<sub CR >>  COUNT=',COUNT,' ERR=',ABSR/ABSB

      END SUBROUTINE MADIM

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C multiply a inverse matrix (H-E) to a vector B 
C and put the result to a vector X.
C  X = 1/(H-E) B
C called by the subroutine EXEIGN.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULIM(N,V1,V2,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NSTM,NCIM,NCI,NSFT,NMODE,P,R,AP,AR)
      IMPLICIT NONE

C Parameter
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)
C
C dummy variables
      INTEGER N
      REAL*8  V1(N),V2(N)
      REAL*8  E
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)

      INTEGER NMODE

      REAL*8 P(N),R(N),AP(N),AR(N)
C
      CALL CLRV (N,V1)
      CALL MADIM(N,V1,V2,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &          ,NSTM,NCIM,NCI,NSFT,NMODE,P,R,AP,AR)
C    
      END SUBROUTINE MULIM

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     solve eign values & vectors of hamiltonian using lanctz method
C        coded by A.Tanaka (at Hiroshima Unv. 1995)
C
C Inputs
C     N            :order of the matix
C     NG           :number of eign values (vectors) you need. 
C     NBSM         :dimemsion of 1st subscript of UG
C
C     H, IHP1,IHP2 :off-diagonal matrix elements and their subscripts 
C     HD           :diagonal part of the matrix 
C     NOP,NHOP,NSTM,NCIM,NCI,NSFT : addtional information of the matrix
C
C     NCPX         :0 real symmmetric , 1 hermite
C   
C Outputs 
C     UG(NBSM,NG)  :eign vectors
C     EG(NG)       :eign values
C
C Work areas   
C     VP(NBSM), VZ(NBSM)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EXEIGN(E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                 ,NSTM,NCIM,NCI,NSFT
     &                 ,NGM,NBGM,NGIN,UG,EG
     &                 ,NBSM,VZ,VP,NMODE,NBMM,TMW1,TMW2,TMW3,TMW4)
      IMPLICIT NONE 

C Parameters 
C  -local parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER NLAN
      PARAMETER(NLAN=5000)
      REAL*8 EPS
      PARAMETER(EPS=1D-10)

C Dummy arguments 
      REAL*8  E

      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)

      INTEGER NGM, NBGM, NGIN
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)
   
      INTEGER NBSM  
      REAL*8  VZ(NBSM),VP(NBSM)

      INTEGER NBMM
      REAL*8  TMW1(NBMM),TMW2(NBMM),TMW3(NBMM),TMW4(NBMM)
      INTEGER NMODE

C Loca, variables
      INTEGER N,NG,NCPX,NEXL
      INTEGER I,J,NL,NN,IG,IG2,IE,IST,IERR,IDSTP

      REAL*8 A(0:NLAN),B(0:NLAN)
      REAL*8 UL(NLAN,2)
      REAL*8 EL(2)
      REAL*8 S,T,R,RR
C Function
      REAL*8 FRAND
C -------------------------------

      WRITE(6,*)'enter EXEIGN'
      NG=NGIN
      IF (NGIN.EQ.0) NG=NGM
C
      N =NSFT(NCI(NOP)+1,NOP)
      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C
      WRITE(6,*)'NOP=',NOP,' NCPX=',NCPX,' NEXL=',NEXL,' N=',N
C
      WRITE(6,*)' E=',E
      IDSTP=1
      DO IG=1,NG
        DO I=1,N
          UG(I,IG)=FRAND()-0.5D0
        END DO 
        NL=NLAN
        DO IST=1,2
          DO I=1,N
            VP(I)=UG(I,IG)
            VZ(I)=0D0
          END DO
          IF (IST.EQ.2) THEN
            DO I=1,N
              UG(I,IG)=0D0
            END DO
          END IF
          DO IE=1,NL
            DO IG2=1,IG-1
              T=0D0
              DO I=1,N
                T=T+VP(I)*UG(I,IG2)
              END DO
              DO I=1,N
                VP(I)=VP(I)-T*UG(I,IG2)
              END DO
              IF (NCPX.NE.0) THEN
                CALL EXCS(N,VP,UG(1,IG2), T)
                CALL EXCV(N,VP,UG(1,IG2),-T)
              END IF
            END DO
C
            CALL ABSV(N,VP,B(IE-1))
C            IF (IE-1.NE.N) T=1D0/B(IE-1)
            IF (B(IE-1).EQ.0D0) THEN
              T=1D30
            ELSE
              T=1D0/B(IE-1)
            END IF
            DO I=1,N
              R=B(IE-1)*VZ(I)
              S=T*VP(I)
              VZ(I)= S
              VP(I)=-R
            END DO
            IF (IST.EQ.2) THEN
              T=UL(IE,1)
              DO I=1,N
                UG(I,IG)=UG(I,IG)+VZ(I)*T
              END DO
            END IF
C
            CALL MADIM(N,VP,VZ,E,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NSTM,NCIM,NCI,NSFT,NMODE,TMW1,TMW2,TMW3,TMW4)
            CALL MULV(N,VP,VZ, A(IE))
            CALL MADV(N,VP,VZ,-A(IE))
C            WRITE(6,*)' A(IE)=',A(IE),' B(IE-1)=',B(IE-1)
            IF ((MOD(IE,IDSTP).EQ.0.OR.IE-1.EQ.N)
     &          .AND.IE.GE.3.AND.IST.EQ.1) THEN
              NN=IE-1
              CALL BISECT(NN,-MIN(2,NN),NLAN,EL,UL,A(1),B(1),IERR)
              IF (IERR.NE.0)THEN
                WRITE(6,*)'Error in subroutine "BISECT"'
              END  IF
              WRITE(6,'('' ITA='',I6,G26.16,G26.16,G16.8)')
     &             IE-1,EL(1),EL(2),B(IE-1)*ABS(UL(IE-1,1))
              IF (ABS(B(IE-1)*UL(IE-1,1)).LT.EPS) THEN
                NL=IE-1
                WRITE(6,*)' NL=',NL
                EG(IG)=E+1D0/EL(1)
                GOTO 10
              END IF
            END IF
          END DO !IE
 10       CONTINUE
        END DO
C
        WRITE(6,'(''  #'',I6,'' NITA='',I6)')IG,IE-1
        WRITE(6,*)' 1st ',EL(1),E+1D0/EL(1)
        WRITE(6,*)' 2nd ',EL(2),E+1D0/EL(2)
        DO I=1,N
          VZ(I)=UG(I,IG)
        END DO
        CALL ABSV(N,VZ,T)
        CALL MULS(N,VZ,1D0/T)
        CALL MULM(N,VP,VZ,0D0,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &           ,NSTM,NCIM,NCI,NSFT,NMODE)
        CALL MULV(N,VP,VZ,T)
        RR=0D0
        DO I=1,N
          RR=RR+(VP(I)-T*VZ(I))**2
        END DO
        WRITE(6,'(''  #'',I6,'' E='',G30.16,'' ERR='',G30.16)')
     &     IG,T,SQRT(RR)
        J=0 
        DO WHILE( J.LT.4 .AND. SQRT(RR).GT.EPS*10D0 ) 
          J=J+1
          T=E+1D0/EL(1) - 1D-5*MIN( 1D0/EL(1), 1D0/EL(2)-1D0/EL(1) )
          CALL MULIM(N,VP,VZ,T,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &              ,NSTM,NCIM,NCI,NSFT,NMODE,TMW1,TMW2,TMW3,TMW4)
          CALL ABSV(N,VP,T)
          CALL MULS(N,VP,1D0/T)
          DO IG2=1,IG-1
            T=0D0
            DO I=1,N
              T=T+VP(I)*UG(I,IG2)
            END DO
            DO I=1,N
              VP(I)=VP(I)-T*UG(I,IG2)
            END DO
            IF (NCPX.NE.0) THEN
              CALL EXCS(N,VP,UG(1,IG2),T)
              CALL EXCV(N,VP,UG(1,IG2),-T)
            END IF
          END DO
          CALL ABSV(N,VP,T)
          CALL MULS(N,VP,1D0/T)
          CALL MOVV(N,VZ,VP)
          CALL MULM(N,VP,VZ,0D0,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &             ,NSTM,NCIM,NCI,NSFT,NMODE)
          CALL MULV(N,VP,VZ,T)
          RR=0D0
          DO I=1,N
            RR=RR+(VP(I)-T*VZ(I))**2
          END DO
          WRITE(6,'(''  #'',I6,'' E='',G30.16,'' ERR='',G30.16)')
     &       IG,T,SQRT(RR)
        END DO

        DO I=1,N
          UG(I,IG)=VZ(I)
        END DO 
C
        IF (IG.GT.1) THEN 
          IF (ABS(EG(IG)-EG(IG-1)).GT.10*EPS) THEN
            E=(EG(IG)+EG(IG-1))/2D0
            IF (NGIN.EQ.0) THEN
              NGIN=IG-1
              RETURN
            END IF 
          END IF
        END IF
C
      END DO 

      END SUBROUTINE EXEIGN

C>>>END RESONANCE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCC   WAVE (solve wave function of the excited state(s)
CCCC
CCCC
CCC      SUBROUTINE WAVE(NOP,NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NELC,NDEG
CCC     &            ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED,NMODE,U,NDLT,DLT
CCC     &            ,EPH,EADJ,MODE,V,X)
CCC      IMPLICIT REAL*8 (A-H,O-Z)
CCC      PARAMETER(LMAX=8,LMAX2=3,KMAX=2*LMAX,NEMAX=4*LMAX2+2
CCC     &,NCIM=13000,NOBTM=120,NSTM=3,NBLK=30,NHM=5000001
CCC     &,NBGM=100001,NBMM=1,NMAX=50000,NGMAX=20,NRECM=2000
CCC     &,NMAXD=50000
CCC     &,NPMAX=4000,NEPHM=1000,NEANGM=20,NDLTM=200,NHOPM=100)
CCCC
CCC      PARAMETER(NDIV=20,DELTAE=0.1D0)
CCC      REAL*8 PI,RHO
CCC      PARAMETER(PI=3.141592653D0,RHO=0.0735D0)
CCC      PARAMETER(EPS=1D-15)
CCCC
CCC      DIMENSION NCI(0:NSTM),NSFT(NCIM+1,NSTM)
CCC     &,NELC(NOBT,NCIM,NSTM),NDEG(NOBTM)
CCC     &,NHOP(0:NHOPM),H(NHM),HD(NMAXD),IHP1(NHM),IHP2(NHM)
CCC     &,ERED(NCIM)
CCC     &,U(NOBTM,NOBTM),NDLT(0:NDLTM*10 ),DLT(NDLTM)
CCC     &,EPH(0:NEPHM)
CCC     &,EADJ(NSTM*3+3)
CCC      CHARACTER*(8) MODE
CCCC
CCC      DIMENSION V(NMAX),X(NMAX)
CCCC
CCCC for test
CCCCvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
CCCC      DIMENSION T1(NMAX)
CCCC^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
CCC      NCPX=MOD(NMODE   ,10)
CCC      NEXL=MOD(NMODE/10,10)
CCCC
CCC      CALL CHKBLK(NCI,NOBT,NOBTX,NELC,NDEG,NHOP,NHM,H,IHP1,IHP2
CCC     &            ,NDLT,DLT,NCI(0),NMODE,IERR) 
CCC      IF (IERR.NE.0) RETURN 
CCCC
CCC      CALL FLHEAD(MODE,NMODE, 0, 1, '        ', 0, 0, NSHL
CCC     &           ,NG,EG,EPH, 0D0)
CCCC
CCC      CALL HGEN(NBSM,V,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
CCC     &         ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
CCC      CALL HELV(NOP,NSTM,NCIM,NCI,NSFT,NOBT,NELC
CCC               ,U,NDLT,DLT,HD,ERED,EADJ,NMODE)
CCCC
CCCC
CCCC  An energy shift of the diagonal elements 
CCCC  ALPHA is defined as ALPHA=3*(R2-R1)-R1
CCCC  where R1 (R2) is the
CCCC  maximum (minimum) value of the diagonal elements.
CCCC
CCC      IF (NEXL.EQ.0) THEN
CCC        R1=HD(1)+ERED(1)
CCC        R2=HD(1)+ERED(1)
CCC        DO IC=1,NCI(NOP)
CCC          DO I=NSFT(IC,NOP)+1,NSFT(IC+1,NOP)
CCC            R1=MIN(R1,HD(I)+ERED(IC)) 
CCC            R2=MAX(R2,HD(I)+ERED(IC)) 
CCC          END DO 
CCC        END DO 
CCC      ELSE
CCC        R1=HD(1)+ERED(1)
CCC        R2=HD(1)+ERED(1)
CCC        DO IC=2,NCI(NOP)
CCC          R1=MIN(R1,HD(IC)+ERED(IC)) 
CCC          R2=MAX(R2,HD(IC)+ERED(IC)) 
CCC        END DO
CCC      END IF
CCC      ALPHA=(R2-R1)*3D0-R1
CCC      IF (ALPHA.LT.EPS) ALPHA=1D0
CCCC
CCC      N=NSFT(NCI(NOP)+1,NOP)
CCC      NEPH=NINT( EPH(0) ) 
CCC      DO IEPH=1,NEPH
CCC        OMEGA=SQRT(EPH(IEPH)+ALPHA)
CCC        TIME=0D0
CCC        DT=2D0*PI/OMEGA/DBLE(NDIV)
CCC        GAMMA=DELTAE/OMEGA
CCC        ITAMAX=NDIV*1000
CCC        DO I=1,N
CCC          X(I)=FRAND()
CCC          V(I)=0D0
CCC        END DO
CCC        DO ITA=0,ITAMAX
CCC          TIME=DT*DBLE(ITA)
CCCC
CCCC V(TIME+DT/2)=V(TIME-DT/2)-DT*GAMMA*V(TIME-DT/2)
CCCC GAMMA: a dumping factor
CCCC
CCC          T=1D0-DT*GAMMA
CCC          DO I=1,N
CCC            V(I)=T*V(I)
CCC          END DO
CCCC
CCCC -DT*(H+ALPHA)
CCCC
CCC          DO I=1,N
CCC            X(I)=-DT*X(I)
CCC          END DO
CCC          IF (NEXL.EQ.0) THEN
CCC            DO I=1,N
CCC              V(I)=V(I)+(HD(I)+ALPHA)*X(I)
CCC            END DO 
CCC          ELSE
CCC            DO IC=1,NCI(NOP)
CCC              T=HD(IC)+ALPHA
CCC              DO I=NSFT(IC,NOP)+1,NSFT(IC+1,NOP)
CCC                V(I)=V(I)+T*X(I)
CCC              END DO
CCC            END DO
CCC          END IF
CCC          CALL OPRT(N,V,N,X,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
CCC          CALL OPRT(N,V,N,X,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
CCCC
CCCC +A*COS(OMEGA*TIME)
CCCC
CCC          T=0.5D0*DT*COS(OMEGA*TIME)
CCC          DO I=1,N
CCC            V(I)=V(I)+T
CCC          END DO
CCCC
CCCC X(TIME+DT)=X(TIME)+DT*V(TIME+DT/2)
CCCC
CCC          DO I=1,N
CCC            X(I)=-X(I)/DT+DT*V(I)
CCC          END DO
CCCC
CCCC OUTPUTS for test
CCCCvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
CCCC          IF (NEXL.EQ.0) THEN
CCCC            DO I=1,N
CCCC              T1(I)=HD(I)*X(I)
CCCC            END DO 
CCCC          ELSE
CCCC            DO IC=1,NCI(NOP)
CCCC              T=HD(IC)
CCCC              DO I=NSFT(IC,NOP)+1,NSFT(IC+1,NOP)
CCCC                T1(I)=T*X(I)
CCCC              END DO
CCCC            END DO
CCCC          END IF
CCCC          CALL OPRT(N,T1,N,X,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
CCCC          CALL OPRT(N,T1,N,X,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
CCCC          CALL MULV(N,T1,X,E)
CCCC          CALL ABSV(N,X,T)
CCCC          E=E/T**2
CCCC          S=0D0
CCCC          DO I=1,N 
CCCC            S=S+(T1(I)-E*X(I))**2
CCCC          END DO
CCCC          R=SQRT(S)/T
CCCC          CALL ABSV(N,V,S)
CCCC          CALL ABSV(N,X,T)
CCCC          WRITE(6,*)' ITA=',ITA,' |V|=',S,' |X|=',T
CCCC          WRITE(6,*)' R=',R,' E=',E
CCCC          WRITE(6,*)ITA,S,T
CCCC^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
CCC        END DO
CCC      END DO
CCC      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===============================================================
C IO control Functions
C===============================================================
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  WRITE DOWN THE CONTENTS OF INTERACTION BLOCKS
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE WRTBLK(NHOP,NHM,H,IHP1,IHP2,NBLK)
      IMPLICIT NONE

C Parameter
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy arguments
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NBLK

C Local variable
      INTEGER I,J,K,ICH
C ------------

      WRITE(6,*)'NHOP=',NHOP(0)
      DO I=1,NHOP(0)
        DO ICH=NHOP(I),NHOP(I+1)-1,NBLK
          WRITE(6,*)'IOP=#',I,'   ICH=#',ICH/NBLK
          DO K=1,NBLK
            J=ICH+K
       WRITE(6,*)' IBLK=#',K,'  ',IHP1(J),'  ',IHP2(J), '  ',H(J)
          END DO
        END DO
      END DO
      END SUBROUTINE WRTBLK
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C The header of the output file  UNIT=02
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE FLHEAD(MODE,NMODE,NNDCR,NDCR,CDCR
     &                 ,NSPN1,NSPN2,NSHL,NG,EG,EPH,EANG )
      IMPLICIT NONE

C Parameters
      INTEGER   NEPHM,     NEANGM,   NDCRM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200)

      INTEGER   IUOUT
      PARAMETER(IUOUT=2)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE

      INTEGER NNDCR
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NSPN1, NSPN2

      INTEGER NSHL

      INTEGER NG
      REAL*8  EG(NG)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)

C Local variables
      INTEGER NEXL,NCPX,NEPH,NEANG,I
C -----------

      NEXL=MOD(NMODE/10,10)
      NCPX=MOD(NMODE,10)
C
      WRITE(IUOUT,*)'additional information for GXTLS'
C
      WRITE(IUOUT,*)'MODE'
      WRITE(IUOUT,*)MODE
      IF (NCPX.EQ.0) THEN
        WRITE(IUOUT,*)'real'
      ELSE
        WRITE(IUOUT,*)'cpx'
      END IF
      IF (NEXL.EQ.0) THEN
        WRITE(IUOUT,*)'norm'
      ELSE
        WRITE(IUOUT,*)'exl'
      END IF 
C
      WRITE(IUOUT,*)'DICRO1'
      IF (NNDCR.GE.1) THEN
        WRITE(IUOUT,*)NDCR(1)
        WRITE(IUOUT,*)'{'
        WRITE(IUOUT,'(A9)')(CDCR(I),I=1,NDCR(1))
        WRITE(IUOUT,*)'}'
      ELSE
        WRITE(IUOUT,*)0
      END IF
C
      WRITE(IUOUT,*)'DICRO2'
      IF (NNDCR.EQ.2) THEN
        WRITE(IUOUT,*)NDCR(2)
        WRITE(IUOUT,*)'{'
        WRITE(IUOUT,'(A9)')(CDCR(I),I=1+NDCR(1),NDCR(2)+NDCR(1))
        WRITE(IUOUT,*)'}'
      ELSE 
        WRITE(IUOUT,*)0
      END IF
C
      WRITE(IUOUT,*)'SPIN'
      WRITE(IUOUT,*)NSPN2-NSPN1+1
      IF      ( NSPN1.EQ.1 .AND. NSPN2.EQ.1 ) THEN
        WRITE(IUOUT,*)'{ d }'
      ELSE IF ( NSPN1.EQ.1 .AND. NSPN2.EQ.2 ) THEN
        WRITE(IUOUT,*)'{ d u }'
      ELSE IF ( NSPN1.EQ.2 .AND. NSPN2.EQ.2 ) THEN
        WRITE(IUOUT,*)'{ u }'
      ELSE IF ( NSPN1.EQ.1 .AND. NSPN2.EQ.3 ) THEN
        WRITE(IUOUT,*)'{ d u du }'
      ELSE IF ( NSPN1.EQ.0 .AND. NSPN2.EQ.0 ) THEN
        WRITE(IUOUT,*)'{ JJ }'
      END IF
C
      WRITE(IUOUT,*)'ENERGY'
      WRITE(IUOUT,*)NG
      WRITE(IUOUT,'(5E15.7/)')(EG(I),I=1,NG)

      WRITE(IUOUT,*)'EPHOTO'
      NEPH=NINT( EPH(0) )
      IF (NEPH.EQ.0) THEN
        WRITE(IUOUT,*)1
        WRITE(IUOUT,*)0D0
      ELSE 
        WRITE(IUOUT,*)NEPH
        WRITE(IUOUT,'(5E15.7/)')(EPH(I),I=1,NEPH)
      END IF

      WRITE(IUOUT,*)'EANGL'
      NEANG=NINT( EANG(0) )
      IF (ABS(NEANG).GT.0) THEN
C on angle resolved mode
        WRITE(IUOUT,*)ABS(NEANG)
      ELSE IF(NEANG.LE.0) THEN
C on angle integrated mode
        WRITE(IUOUT,*)1
        WRITE(IUOUT,*)0D0
      END IF
      WRITE(IUOUT,'(5E15.7/)')(EANG(I),I=1,2*NEANG)
C
      WRITE(IUOUT,*)'NSHELL'
      WRITE(IUOUT,*)NSHL
C
      END SUBROUTINE FLHEAD
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Write IEPH, IG, ISHL, IDCR1, IDCR2, L_EX, M_EX, S_EX
C additional information of a spectrum
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SPHEAD(IEPH,IG,IEANG,ISHL,IDCR1,IDCR2,LEX,IOEX)
      IMPLICIT NONE

C Parameter
      INTEGER IUOUT
      PARAMETER(IUOUT=2)

C Dummy arguments
      INTEGER IEPH,IG,IEANG,ISHL,IDCR1,IDCR2,LEX,IOEX
C ----------------

      WRITE(IUOUT,'('' #E_m='',I5,'' #E_g='',I5,'' #angle='',I5'
     &  // ','' #shell='',I3,  '' #dichro1='',I3,'' #dichro2='',I3'
     &  // ','' l_EX='',I3,'' m_EX='',I3,'' s_EX='',I3)')
     & IEPH,IG,IEANG,ISHL,IDCR1,IDCR2,LEX,IOEX/2-LEX,2*MOD(IOEX,2)-1

      END SUBROUTINE SPHEAD
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Total electron number of the state NSTATE.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION NTOLEL(NSTATE,NSTM,NCIM,NCI,NOBT,NELC)
      IMPLICIT NONE 
      INTEGER NTOLEL

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

C Dummy arguments
      INTEGER NSTATE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)

C Local variable
      INTEGER IO

C Function
      INTEGER NNELC
C --------------

      IF (NSTATE.GT.NCI(0).OR.NSTATE.LT.1) THEN
        WRITE(6,*)'Error! in NTOLEL'
        WRITE(6,*)'A wrong state # was specified: state# =',NSTATE
        STOP
      END IF
C
      NTOLEL=0
      DO IO=1,NOBT
        NTOLEL=NTOLEL+NNELC(IO,1,NSTATE,NSTM,NCIM,NOBT,NELC)
      END DO
C
      END FUNCTION NTOLEL
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      SUBROUTINE ELCNT(NSCV,NSTM,NCIM,NCI,NOBT,NELC)
C      IMPLICIT NONE 
C
CC Parameters
C      INTEGER   NOBTM
C      PARAMETER(NOBTM=120)
C
CC Dummy argumets
C      INTEGER NSCV
C
C      INTEGER NSTM,NCIM
C      INTEGER NCI(0:NSTM)
C      INTEGER NOBT
C      INTEGER NELC(NOBT,NCIM,NSTM)
C
CC Local variables
C      INTEGER NB,IST,N
C      INTEGER NTOLEL
CC ---------
C
C      NSCV=1
C      NB=NTOLEL(1,NOBT,NCI,NELC)
C      DO IST=2,NCI(0)
C        N=NTOLEL(IST,NOBT,NCI,NELC)
C        IF (N.NE.NB) RETURN
C        NSCV=NSCV+1
C      END DO
C      END SUBROUTINE ELCNT
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    read parameters from "xcard"
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RDCD(MODE,NMODE,NSTATE
     &          ,NSTM,NCIM,NCI,NOBT,NOBTX,NELC,NDEG
     &          ,ASTS,ACI,AOBT
     &          ,NHOP,NHM,H,IHP1,IHP2,U,NDLT,DLT,EADJ
     &          ,GAM,GALF,NG,ORBM,EPH,EANG,NDCR,CDCR,NSPN
     &          ,RANGE,GALF2
     &          ,NREC
     &          ,NDMO,NDMG,NDMALF,MATSYB)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX,  LMAX2,  KMAX,       NEMAX
      PARAMETER(LMAX=8,LMAX2=3,KMAX=2*LMAX,NEMAX=4*LMAX2+2)

      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBLKM
      PARAMETER(NBLKM=200)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM,     NEANGM,   NDCRM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200)

      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

      INTEGER   IUNIT 
      PARAMETER(IUNIT=1)

      REAL*8 PI
      PARAMETER(PI=3.141592653897932D0)

      INTEGER NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      CHARACTER*(8) MODE
      INTEGER NMODE
      INTEGER NSTATE

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM2)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10 )
      REAL*8  DLT(NDLTM)
      REAL*8  EADJ(NSTM2*3+3)
      REAL*8  GAM, GALF
      REAL*8  GALF2(0:NSTM2)

      INTEGER NG  
      REAL*8  ORBM(0:NOBTM*7)

      REAL*8  EPH(0:NEPHM)
      REAL*8  EANG(0:NEANGM)
      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)
      INTEGER NSPN(2)

      REAL*8  RANGE(4,NSTM2)

      INTEGER NREC

      INTEGER NDMO
      INTEGER NDMG(0:NSTM2),NDMALF(0:NSTM2)
      CHARACTER*(32) MATSYB


      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Global variables
      INTEGER IERR
      COMMON /SYS/IERR

C Local variables
C  --data IO, X_CODE, GTDAT and GTWRD
      INTEGER ITXMAX, NARGM, NELCBS
      PARAMETER(ITXMAX=128,NARGM=2000,NELCBS=10000)
      REAL*8 DNUM2
      INTEGER            NWRD, NCMD
      CHARACTER*(ITXMAX) AWRD, ACMD
      CHARACTER*(32) ASYMB
      CHARACTER*(2)  DLM

      INTEGER IARG(8)
      INTEGER NARG
      REAL*8  ARG(NARGM)
      INTEGER LIN,LINORG,LINXCD,ITYP
      REAL*8  RR(NBLKM),RI(NBLKM)

C  --for "hyb" command
      REAL*8  VR(LMAX),VI(LMAX)

C  --loop variables
      INTEGER I,J,K,N,IST,NSTS,ICI,ICIM,IO,IO1,IO2,ISB,ICHN
      INTEGER IOM,IOLBL,IORED,NBLK
      INTEGER L1,L2,L3,L4,NERED,NALF,IDCRNMB,NOBTXB,NEPH,NEANG
      INTEGER NTYP,NBDY,NEXL,ICPX,NCPX,NCPXB,NEX,INRM
      REAL*8  T, XA, YA, ZA, ALF, BTA, RED
      LOGICAL CNFCHK
      LOGICAL FOUND
C ----------------------------------------

      WRITE(6,*)'enter RDCD'

      CALL GTWRD(NWRD,AWRD,DLM,LIN,0,IERR,-3)
      CALL X_CODE(0,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
      IF (IERR.EQ.1) STOP

      OPEN(UNIT=IUNIT,FILE='xcard01',STATUS='OLD')
C
C Initialization 
C
      LINORG=0
      NOBT=0
      NOBTX=0
      NSTS=0
      ASTS=' '
      ACMD=' '
      NCMD=0
      DO IST=1,NSTM
        DO ICI=1,NCIM
          ACI(IST,ICI)='ZZZ'
          DO IO=1,NOBT
            NELC(IO,ICI,IST)=0
          END DO
        END DO
      END DO
      DO IST=1,NSTM
        NCI(IST)=0
      END DO
CCC
C   ITEM "XCRD" (start of Xcard, the parameter file)
CCC
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      IF (IERR.EQ.3) THEN
        WRITE(6,*)'end of file'
        STOP
      ELSE IF (AWRD.NE.'xcrd') THEN
        WRITE(6,*)'wrong file'
        STOP
      END IF
      WRITE(6,*)'item =>'//AWRD(:4)//' at line=',LIN
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)

      DO WHILE ( DLM .NE. ': ' )
        DO WHILE (AWRD.EQ.'line') 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINORG=NINT(DNUM2(NWRD,AWRD,ITYP))
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINXCD=LINORG
        END DO 
        IF (LINORG.EQ.0) LINXCD=LIN

        IF (AWRD(1:1).EQ.' '.AND.DLM.EQ.'( ') THEN
          DO
            CALL X_CODE(1,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
            IF (IERR.EQ.1) STOP
            IF (IERR.EQ.2) EXIT 
          END DO
        ELSE IF( AWRD .EQ. 'nhm' .AND. DLM .EQ. '=' ) THEN
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          NHM = NINT( DNUM2(NWRD, AWRD, ITYP ) )
        END IF
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      END DO
C--------------------------------------------------------------
C   ITEM "CNFG:" (definition of orbits & configurations )
C--------------------------------------------------------------
      IF (AWRD.NE.'cnfg') THEN
        WRITE(6,*)'Error! the item "CNFG:" is expected.'
        STOP
      END IF
      WRITE(6,*)'item =>'//AWRD(:4)//' at line=',LIN,LINXCD
C
C read configuration table
C
      DO WHILE ( DLM .NE. '# ' )
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
C
        DO WHILE (AWRD.EQ.'line') 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINORG=NINT(DNUM2(NWRD,AWRD,ITYP))
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINXCD=LINORG
        END DO 
        IF (LINORG.EQ.0) LINXCD=LIN
C
        CALL GTORB(IOLBL,0,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &            ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
      END DO 

      ICIM=0
      DO !UNTIL '# ' appears
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        IST=INDEX(ASTS,AWRD(1:1))
C        WRITE(6,*)'IST=',IST
        IF (IST.LE.0) THEN
          IF (NSTS.EQ.0) THEN
            ASTS=AWRD(1:1)
          ELSE
            ASTS=ASTS(:NSTS)//AWRD(1:1)
          END IF
          NSTS=NSTS+1
          IST=MIN(NSTS,NSTM)
          ICI=0
        END IF
        ICI=ICI+1
        IF (ICI.GT.ICIM) ICIM=ICI
        IF (ICI.LE.NCIM) THEN
          IF (NWRD.LE.1) THEN
            ACI(IST,ICI)=' '
          ELSE 
            ACI(IST,ICI)=AWRD(2:NWRD)
          END IF
        END IF
        NCI(IST)=ICI
        DO IO=1,NOBT
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          IF (ICI.LE.NCIM) NELC(IO,ICI,IST)=NINT(DNUM2(NWRD,AWRD,ITYP))
        END DO
        IF (DLM.NE.'# ') EXIT 
      END DO
      IF (ICIM.GT.NCIM) THEN
        WRITE(6,*)'Error! Too many configurations. The parameter "NCIM"'
        WRITE(6,*)' must be enchanced more than ',ICIM
        STOP
      END IF  
      IF (NSTS.GT.NSTM) THEN
        WRITE(6,*)'Error! Too many states. The parameter "NSTM"'
        WRITE(6,*)' must be enchanced more than ',NSTS
        STOP
      END IF  
      NCI(0)=NSTS
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)

C  ------ dump the data
      WRITE(6,*)'AOBT='
      WRITE(6,'(15A7/)')(AOBT(IO),IO=1,NOBT)
      WRITE(6,*)'ASTS=',ASTS
      WRITE(6,*)'ACI ='
      DO IST=1,NSTS
        WRITE(6,'(15A7)')(ACI(IST,ICI),ICI=1,NCI(IST))
      END DO
      WRITE(6,*)'NCI='
      WRITE(6,'(15I5)')(NCI(IST),IST=0,NCI(0))
      WRITE(6,*)'L='
      WRITE(6,'(15I5)')(NDEG(IO)/4,IO=1,NOBT)
      WRITE(6,*)'NELC='
      DO IST=1,NSTS
        DO ICI=1,NCI(IST) 
          WRITE(6,'(15I5)')(NELC(IO,ICI,IST),IO=1,NOBT)
        END DO
      END DO
C--------------------------------------------------------------
C   ITEM "PARA:" (parameters for system)
C--------------------------------------------------------------
      IF (AWRD.NE.'para')  THEN
	 WRITE(6,*)'Error! the item "PARA:" is expected.'
	 STOP
      END IF
      WRITE(6,*)'item =>'//AWRD(:4)//' at line=',LINXCD,LIN
C
C init parameters
C
      NCPX  =0
C
      NOBTXB=0
      DO IO1=1,NOBT
        DO IO2=1,NOBT
          U(IO1,IO2)=0D0
        END DO
      END DO
C
      NDLT(0)=0
      EPH(0)=0D0
C
      RED =0.8D0
      GAM =0D0
      GALF=0D0
C
      DO I=1,NSTM
        GALF2(I)=0D0
      END DO
C
C Evaluate Commands in the Item "PARA:"
C
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      DO WHILE ( DLM .NE. ': ' )
C
        DO WHILE (AWRD.EQ.'line') 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINORG=NINT(DNUM2(NWRD,AWRD,ITYP))
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINXCD=LINORG
        END DO 
        IF (LINORG.EQ.0) LINXCD=LIN
C
        ACMD=AWRD
        NCMD=NWRD
C
        IF (ACMD.EQ.'u') THEN

          NBDY=1

          CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)

          IF (NARG.NE.1) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          IF ( IARG(5).GT.NOBT .OR. IARG(6).GT.NOBT ) THEN
            WRITE(6,*)
     &        'Error! undefined orbital lebel has been appeared.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          U(IARG(5),IARG(6))=ARG(1)
          U(IARG(6),IARG(5))=ARG(1)
          write(6,*)'  u: '//AOBT(iarg(5))//AOBT(iarg(6)),ARG(1)

        ELSE IF (ACMD(:3).EQ.'dlt') THEN

          NBDY=1
          IF (ACMD(4:4).EQ.'2') NBDY=2
          CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.NE.1) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
C         IF (IARG(5).EQ.IARG(7).AND.IARG(6).EQ.IARG(8)) NBDY=1
          NDLT(0)=NDLT(0)+1
          IF (NDLT(0).GT.NDLTM) THEN
            WRITE(6,*)
     &       'Error! Too many "Dlt" commands. The parameter "NDLTM"'
            WRITE(6,*)' must be enhanced: NDLTM=',NDLTM
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          I=(NDLT(0)-1)*10
          NDLT(I+1)=LINXCD
          NDLT(I+2)=NBDY
          DO K=1,8
            NDLT(I+K+2)=IARG(K)
          END DO
            
          DLT(NDLT(0))=ARG(1)
          IF (INRM-2.GE.0)  DLT(NDLT(0))=-ARG(1)

        ELSE IF (ACMD.EQ.'gam'.OR.ACMD.EQ.'alf'.OR.ACMD.EQ.'alf2'
     &     .OR.ACMD.EQ.'red'.OR.ACMD.EQ.'eph'.OR.ACMD.EQ.'estate')THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)

          IF (ACMD.EQ.'gam') THEN 
             GAM=ARG(1)
          ELSE IF (ACMD.EQ.'red') THEN
             RED=ARG(1)
          ELSE IF (ACMD.EQ.'alf') THEN 
             GALF=ARG(1)
          ELSE IF (ACMD.EQ.'alf2') THEN
            GALF2(0)=DBLE(NARG)
            DO I=1,NARG
             GALF2(I)=ARG(I)
            END DO 
          ELSE IF (ACMD.EQ.'eph'.OR.ACMD.EQ.'estate') THEN
            EPH(0)=DBLE(NARG)
            DO I=1,NARG
              EPH(I)=ARG(I)
            END DO
          END IF
        ELSE IF ( ACMD.EQ.' ' .AND. DLM.EQ.'( ' ) THEN
          DO 
            CALL X_CODE(1,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
            IF (IERR.EQ.1) CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            IF (IERR.EQ.2) EXIT 
          END DO
        ELSE IF (ACMD(1:5).EQ.'exorb') THEN

          DO !until inside the braces
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0.AND.DLM.EQ.'{ ')  THEN
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            END IF
            IF (NWRD.NE.0) THEN 
              CALL GTORB(IOLBL,2,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &                ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
              IF ( IOLBL .LE. NOBT ) THEN
                WRITE(6,*) 'Error! The orbital '//AOBT(IOLBL)(1:5)
     &          // 'has been defined in the item "CNFG:".'
                WRITE(6,*)' The orbital should be for continuum states.'
                CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
              END IF
            END IF
            IF ( DLM.EQ.'} ' ) 
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF ( NWRD.EQ.0 .OR. DLM.NE.', ' ) EXIT 
          END DO
          NOBTXB=NOBTX

        ELSE
          CALL ERR_UCMD(NCMD,ACMD,DLM,'PARA:',LINXCD)
        END IF
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      END DO ! WHILE( DLM .NE. ': ' )

C----------------------------------------------------------
C   ITEM "EXEC:" (conditions of execution)
C----------------------------------------------------------
      IF (AWRD.NE.'exec') THEN
        WRITE(6,*)'Error! the item "EXEC:" is expected.'
        STOP
      END IF
C
C Set Default Parameters for the Item "EXEC"
C
C Without "Dirchro" or "Dichro2" commands 
      NDCR(1)=0
      NDCR(2)=0
C
C "Spin={U,D};" 
      NSPN(1)=1
      NSPN(2)=2
C
C "Mode={NOP,NOCNFCHK};" 
C "Ninit=1;"
C "Nrec=200;" 
      MODE='nop'
      NCPX=0
      NEXL=0
      CNFCHK=.FALSE.
      NG=1
      NREC=200
C
C "Range={1000,-25d0,25d0,0.025d0};"
      DO IST=1,NSTM
        RANGE(1,IST)=0D0
      END DO
      RANGE(1,1)=1000D0
      RANGE(2,1)=-25D0
      RANGE(3,1)= 25D0
      RANGE(4,1)=0.025D0
C
C Without "Eang" command
      EANG(0)=0D0
C
      EADJ(1)=0D0
      EADJ(2)=0D0
      MATSYB='RED'
C
      NSTATE=0
      DO I=0,NOBTM*7
        ORBM(I)=0D0
      END DO
C
C  Evaluate Commands in the Item "EXEC:"
C
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      DO WHILE (DLM.NE.': ')
C
        DO WHILE (AWRD.EQ.'line') 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINORG=NINT(DNUM2(NWRD,AWRD,ITYP))
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          LINXCD=LINORG
        END DO 
        IF (LINORG.EQ.0) LINXCD=LIN
C
        ACMD=AWRD
        NCMD=NWRD
C
        IF (ACMD.EQ.'mode') THEN

          DO !until inside the brace
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0.AND.DLM.EQ.'{ ') 
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            WRITE(6,*)'  mode: '//AWRD(:NWRD)
            IF (AWRD.EQ.'exl') THEN
              NEXL=1
            ELSE IF (AWRD.EQ.'cpx') THEN
              NCPX=1
            ELSE IF (AWRD.EQ.'cnfchk') THEN
              CNFCHK=.TRUE.
            ELSE IF (AWRD.EQ.'nocnfchk') THEN
              CNFCHK=.FALSE.
            ELSE 
              MODE=AWRD(1:8)
              IF (MODE.EQ.'xps'   ) MODE='pes'
              IF (MODE.EQ.'rxps'  ) MODE='rpes'
              IF (MODE.EQ.'rxpsf' ) MODE='rpesf'
              IF (MODE.EQ.'rxpsg' ) MODE='rpesg'
              IF (MODE.EQ.'rxpsfg') MODE='rpesfg'
              IF (MODE.EQ.'bis'   ) MODE='ipes'
              IF (MODE.EQ.'rbis'  ) MODE='ripes'
              IF (MODE.EQ.'rbisf' ) MODE='ripesf'
              IF (MODE.EQ.'tbis'  ) MODE='tipes'
            END IF
            IF ( DLM.EQ.'} ' )
     &         CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0 .OR. DLM.NE.', ' ) EXIT 
          END DO

        ELSE IF (ACMD.EQ.'ninit') THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.EQ.0) CALL  ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          NDMG(0)=NARG
          NG=NINT(ARG(1))
          write(6,*)'  ninit: NG=',NG
          DO I=1,NARG
            NDMG(I)=NINT(ARG(I))
          END DO

        ELSE IF (ACMD.EQ.'nalf') THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.EQ.0) CALL  ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          NDMALF(0)=NARG
          DO I=1,NARG
            NDMALF(I)=NINT(ARG(I))
          END DO

        ELSE IF (ACMD.EQ.'matsyb') THEN

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          MATSYB=AWRD(1:NWRD)

        ELSE IF (ACMD.EQ.'eadj') THEN

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          IST=INDEX(ASTS,AWRD(1:1))
          IF (NWRD.EQ.1) THEN
            AWRD(2:2)=' '
            NWRD=2
          END IF
          IF (IST.EQ.0) THEN
            WRITE(6,*)
     &         'Error! A wrong state "#'//AWRD(1:1)//'" was found.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          FOUND=.FALSE.
          DO ICI=1,NCI(IST)
            FOUND=AWRD(2:NWRD).EQ.ACI(IST,ICI)
            IF (FOUND) EXIT
          END DO
          IF ( .NOT. FOUND ) THEN
            WRITE(6,'(A,I6)')  'Error! A wrong configuration label "'
     &        //AWRD(1:NWRD)//'" was found.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.NE.1) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          WRITE(6,*)'  eadj:  #'//ASTS(IST:IST)//ACI(IST,ICI)
     &            // ' Adjusted energy=',Arg(1)
          EADJ(1)=EADJ(1)+1
          I=NINT(EADJ(1))
          EADJ((I-1)*3+4)=IST
          EADJ((I-1)*3+5)=ICI
          EADJ((I-1)*3+6)=ARG(1)

        ELSE IF (ACMD.EQ.'adjorb') THEN

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTORB(IOLBL,1,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &              ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.NE.1) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          EADJ(2)=DBLE(IOLBL)
          EADJ(3)=ARG(1)

        ELSE IF (ACMD.EQ.'state') THEN

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          I=INDEX(ASTS,AWRD(1:1))
          IF (I.LE.0) THEN
            WRITE(6,*)'Error! Undefined state "#'//AWRD(1:1)//
     &             '" was found.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          NSTATE=I

        ELSE IF (ACMD(1:5).EQ.'dicro'.OR.ACMD(1:6).EQ.'dichro') THEN

          IF ( ACMD(NCMD:NCMD).EQ.'2'.OR.ACMD(NCMD:NCMD).EQ.'''')THEN 
            IDCRNMB=2
            I=NDCR(1)
          ELSE
            IDCRNMB=1
            I=0
          END IF
          DO !until inside the braces
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0.AND.DLM.EQ.'{ ') 
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (IDCRNMB.EQ.1) THEN
              DO J=I+ NDCR(2),I+1,-1
                CDCR(J+1)=CDCR(J)
              END DO
            END IF
            I=I+1
            CDCR(I)=AWRD(1:NWRD)
            write(6,'(A)')'  dichro: '//AWRD(1:NWRD) 
            IF ( DLM.EQ.'} ' )
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0 .OR. DLM.NE.', ') EXIT
          END DO
          NDCR(IDCRNMB)=I
          IF (IDCRNMB.EQ.2) NDCR(2)=NDCR(2) - NDCR(1)

        ELSE IF (ACMD.EQ.'spin') THEN

          NSPN(1)=2
          NSPN(2)=1
          DO !until inside the braces
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0.AND.DLM.EQ.'{ ')
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (AWRD(1:NWRD).EQ.'ud' .OR. AWRD(1:NWRD).EQ.'du' )THEN
              NSPN(1)=1
              NSPN(2)=3
            ELSE IF (AWRD(1:1).EQ.'u') THEN
              NSPN(2)=2
            ELSE IF (AWRD(1:1).EQ.'d') THEN
              NSPN(1)=1
            ELSE IF (AWRD(1:1).EQ.'j') THEN
              NSPN(1)= 0
              NSPN(2)= 0
            ELSE
              WRITE(6,*)
     &         'A wrong parameter "'//AWRD(:NWRD)//'" was found.'
              CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            END IF
            IF ( DLM.EQ.'} ' ) 
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0 .OR. DLM.NE.', ' ) EXIT 
          END DO

        ELSE IF (ACMD.EQ.'eang') THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (MOD(NARG,3).NE.0.OR.NARG.EQ.0) THEN
            CALL  ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          END IF
          DO I=1,NARG/3
            XA=ARG(I*3-2)
            YA=ARG(I*3-1)
            ZA=ARG(I*3  )
            IF (DSQRT(XA**2+YA**2).LT.10D-12) THEN
              ALF=0D0
            ELSE
              ALF=DATAN2(YA,XA)
            END IF
            BTA=-DATAN2(ZA,DSQRT(XA**2+YA**2))+PI/2D0
            EANG(I*2-1)=BTA
            EANG(I*2  )=ALF
            IF (ABS(YA).GT.1D-15) NCPX=1
          END DO 
          EANG(0)=DBLE( NARG/3 )

        ELSE IF (ACMD.EQ.'range') THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (MOD(NARG,4).NE.0.AND.NARG.NE.0) THEN 
            CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          ELSE
            DO I=0,NARG-1,4
              RANGE(1,I/4+1)= ARG(I+1)
              RANGE(2,I/4+1)= ARG(I+2)
              RANGE(3,I/4+1)= ARG(I+3)
              RANGE(4,I/4+1)= MAX(ARG(I+4),(ARG(I+3)-ARG(I+2))/ARG(I+1))
            END DO
          END IF

        ELSE IF (ACMD.EQ.'orbang'.OR.ACMD.EQ.'spinang') THEN

          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)

          IF (MOD(NARG,3).NE.0.AND.NARG.NE.0) THEN 
            CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          ELSE IF (NINT(ORBM(0)).NE.0.AND.NINT(ORBM(0)).NE.NARG/3) THEN
            WRITE(6,'(A,I3)')
     &          'Error! Inconsistant numbers of orbitals among the'
     &        //' "OrbAng", "SpinAng" and "Mag" commands" Norb=',NARG/3
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF 
          ORBM(0)=DBLE(NARG/3)
          N=NARG/3
          IF (ACMD.EQ.'spinang') N=NARG/3+3
          DO J=0,NARG/3-1
            DO I=1,3
              ORBM(I+6*J+N)=ARG(I+3*J)
            END DO
          END DO 

        ELSE IF (ACMD.EQ.'nrec') THEN
          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)
          IF (NARG.NE.1) CALL  ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          NREC=NINT(ARG(1))
          WRITE(6,*)'  nrec: ',NREC
          IF (NREC.GT.NRECM) THEN
            WRITE(6,'(A,I3)')
     &          'Error! The number of the iteration for the recursion'
     &        //' method NREC is too large: NREC=',NREC
            WRITE(6,'(A,I10,A)')'   The parameter NRECM=',NRECM,
     &           ' must be enchanced more than above.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF 

        ELSE IF (ACMD.EQ.'mag') THEN

          IOM=0
          K=0
          DO !until inside the braces
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0.AND. DLM .EQ. '{ ' ) THEN
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            END IF 
            CALL GTORB(IOLBL,1,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &                ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
            IOM=IOM+1
            ORBM(IOM)=DBLE(IOLBL)
            WRITE(6,*)'  mag: '//AOBT(IOLBL)
            IF ( DLM.EQ. '} ') 
     &        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            IF (NWRD.EQ.0 .OR. DLM.NE. ', ' ) EXIT 
          END DO
          IF (NINT(ORBM(0)).NE.0.AND.NINT(ORBM(0)).NE.IOM) THEN
            WRITE(6,'(A,I3)')
     &          'Error! Inconsistant numbers of orbitals among the'
     &        //' "OrbAng", "SpinAng" and "Mag" commands" Norb=',IOM
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          ORBM(0)=DBLE(IOM)

        ELSE IF (ACMD.EQ.'lmlim') THEN

          IF (NINT( EANG(0) ).GT.0) THEN
            WRITE(6,*)'Error! The command "Lmlim" appeared after' 
     &     //' "Eang"  command.'
             CALL  ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTORB(IOLBL,2,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &              ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
          IF ( IOLBL .LE. NOBT ) THEN
            WRITE(6,*) 'Error! The orbital '//AOBT(IOLBL)(1:5)
     &       //'has been defined in the item "CNFG:".'
            WRITE(6,*)' The orbital should be for continuum states.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          CALL GTDAT(0,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &              ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &              ,NCMD, ACMD, LINXCD)

          NEANG=NINT( EANG(0) )
          DO I=1-NEANG,NARG-NEANG
            EANG(I)=IO*100+2*(NINT(ARG(I))+NDEG(IO)/4)
          END DO
          NEANG=NEANG-NARG
          EANG(0)=DBLE( NEANG )

        ELSE IF (ACMD.EQ.'goprt') THEN

          CALL X_CODE(1,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
          IF (IERR.EQ.1) CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          IF (ITYP.NE.0) THEN
            WRITE(6,*)
     &        'Error! The type of the argument must be character.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          ASYMB='__goprt'
          CALL X_CODE(2,IUNIT,ASYMB,NARG,0,ARG,NARGM,LIN,IERR)
          IF (IERR.EQ.1) THEN
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF 

        ELSE

          CALL ERR_UCMD(NCMD,ACMD,DLM,'EXEC:',LINXCD)

        END IF

        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)

      END DO ! WHILE (DLM .NE. ': ')

C--------------------------------------------------------------
C   ITEM "OPRT:" (definition of a operator)
C--------------------------------------------------------------
      IF (AWRD.NE.'oprt') THEN 
        WRITE(6,*)'item "OPRT:" expected'
        STOP
      END IF

      ICHN=1
      NHOP(0)=0
      NHOP(1)=ICHN
      DO WHILE ( AWRD.EQ.'oprt' .AND. DLM.EQ.': ' )
        WRITE(6,*)'item => oprt  NOP=',NHOP(0)+1,' at line=',LINXCD,LIN

        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        DO WHILE (DLM.NE.': ')
          ICPX=0
          DO WHILE (AWRD.EQ.'line')
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            LINORG=NINT(DNUM2(NWRD,AWRD,ITYP))
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
            LINXCD=LINORG
          END DO 
          IF (LINORG.EQ.0) LINXCD=LIN
C
          ACMD=AWRD
          NCMD=NWRD
C
          DO I=1,NBLKM
            RR(I)=0D0
            RI(I)=0D0
          END DO 
C
C         WRITE(6,*)' command =>',ACMD
C command with superscript
          ISB=0
          IF (DLM.EQ.'^ ') THEN
            IDCRNMB=1
            IF(ACMD(NCMD:NCMD).EQ.'2'.OR.ACMD(NCMD:NCMD).EQ.'''') THEN
              IDCRNMB=2 
            END IF
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
C
            K=(IDCRNMB-1)*ABS( NDCR(1) )+1
            FOUND=.FALSE.
            DO ISB=0,ABS( NDCR(IDCRNMB) )- 1 
              FOUND = CDCR(K+ISB) .EQ. AWRD(1:NWRD)
              IF (FOUND) EXIT
            END DO
            IF ( .NOT. FOUND ) THEN
              IF ( NDCR(IDCRNMB) .GT. 0 ) THEN
                WRITE(6,*)
     &   'Error! Undefined superscript '//AWRD(1:NWRD)//' was found.'
                CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
              ELSE 
                NDCR(IDCRNMB)=NDCR(IDCRNMB)-1
                ISB=-NDCR(IDCRNMB)-1
                CDCR(K+ISB)=AWRD(1:NWRD)
              END IF
            END IF
          END IF

          IF ( ACMD.EQ.'rk' .OR. ACMD.EQ.'uj' ) THEN

             NBDY=2
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)
            CALL TWOBDY(NCMD,ACMD,NARG,ARG,IARG,NOBT,NDEG,INRM,RED
     &           ,NTYP,NBLK,RR,NEX,LINXCD)
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)

          ELSE IF ((ACMD(1:1).EQ.'v' .OR.ACMD(1:4).EQ.'10dq'
     &          .OR.ACMD(2:2).EQ.'v' .OR.ACMD(2:5).EQ.'10dq'
     &          .OR.ACMD(1:1).EQ.'q' .OR.ACMD(2:2).EQ.'q'
     &          .OR.ACMD(1:2).EQ.'ak'.OR.ACMD(1:3).EQ.'cak'
     &          .OR.ACMD(2:3).EQ.'ak'.OR.ACMD(2:4).EQ.'cak')
     &         .AND.ACMD(1:4).NE.'vmat'.AND.ACMD(1:5).NE.'cvmat')THEN

            NTYP=2
            NBDY=1
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)
            CALL CRYST(NCMD,ACMD,NARG,ARG,IARG,NOBT,NDEG,NBLK,RR,RI
     &                ,NEX,ICPX,LINXCD)
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)

          ELSE IF ( ACMD.EQ.'hyb' .OR. ACMD.EQ.'chyb' ) THEN

            NTYP=2
            NBDY=1
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)
            CALL ERR_NDEG(NDEG,NARG,IARG,NCMD,ACMD,LINXCD)

            IO1=IARG(5)
            IO2=IARG(6)
            IF ( MOD(NDEG(IO1),2) .NE. 1 ) THEN
              L1=NDEG(IO1)/4
              L2=NDEG(IO2)/4
            ELSE
              L1=NDEG(IO1)/2
              L2=NDEG(IO2)/2
            END IF

            IF (ACMD.EQ.'hyb'.AND.NARG.NE.3+1+MIN(L1,L2)) THEN
              CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
            ELSE IF(ACMD.EQ.'chyb'.AND.NARG.NE.3+2*(1+MIN(L1,L2)))THEN
              CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
            END IF

            IF (ACMD.EQ.'hyb') THEN
              DO I=1,MIN(L1,L2)+1
                VR(I)=ARG(3+I)
                VI(I)=0D0
              END DO
            ELSE 
              DO I=1,MIN(L1,L2)+1
                VR(I)=ARG(3+2*I-1)
                VI(I)=ARG(3+2*I  )
              END DO
            END IF

            CALL HBND(L1,L2,ARG,VR,VI,NBLK,RR,RI,ICPX)
            NEX=1
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)

          ELSE IF (   ACMD.EQ.'zta' .OR. ACMD.EQ.'zeta' 
     &           .OR. ACMD.EQ.'hz'  .OR. ACMD.EQ.'bz'   ) THEN

            NBDY=1
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)
            IF (NARG.NE.1) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
            IF (ACMD.EQ.'zeta' .OR. ACMD.EQ.'zta' ) THEN
              NTYP=3
              RR(1)=ARG(1)
            ELSE IF (ACMD.EQ.'hz') THEN
              NTYP=11
              RR(2)=ARG(1)
            ELSE IF (ACMD.EQ.'bz') THEN
               NTYP=11
               RR(5)=ARG(1)
            END IF
            NEX=1
            NBLK=6
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)

          ELSE IF ( ACMD.EQ.'ha' .OR. ACMD.EQ.'ba' ) THEN

            NTYP=11
            NBDY=1
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)

            IF (NARG.NE.4) CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
            T=1D0/SQRT(ARG(2)**2+ARG(3)**2+ARG(4)**2)
            IF (ABS(ARG(3))/T.GT.1D-10) ICPX=3
            K=0
            IF (ACMD.EQ.'ba') K=3
            RR(1+K)= 0.5D0*ARG(1)*ARG(2)*T
            RR(2+K)=       ARG(1)*ARG(4)*T
            RR(3+K)= 0.5D0*ARG(1)*ARG(2)*T
            RI(1+K)= 0.5D0*ARG(1)*ARG(3)*T
            RI(3+K)=-0.5D0*ARG(1)*ARG(3)*T
            NEX=1
            NBLK=6
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)

          ELSE IF (ACMD(1:2).EQ.'dk') THEN
C
            NTYP=2
            NBDY=1
            CALL GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)

            CALL ERR_NDEG(NDEG,NARG,IARG,NCMD,ACMD,LINXCD)
C
            IDCRNMB=1
            IF ( ACMD(3:3).EQ.'''' .OR. ACMD(3:3).EQ.'2' ) IDCRNMB=2
C
C  "Spin=jj;" command in the item "EXEC:"
            IF ( NSPN(1).EQ.0 .AND. NSPN(2).EQ.0 ) THEN
              IF (IDCRNMB.EQ.2) THEN
                WRITE(6,*)
     & 'Error! "Dk2" command can not be used with "Spin" command.'
                STOP
              END IF
              CDCR(1)='l'
              CDCR(2)='z'
              CDCR(3)='r'
              NDCR(1)=3
              NDCR(2)=0
            ELSE IF ( NDCR(IDCRNMB).EQ.0 ) THEN
              IF (IDCRNMB.EQ.1) THEN
                DO I= ABS( NDCR(2) ), 1, -1
                  CDCR(I+3)=CDCR(I)
                END DO
                CDCR(1)='l'
                CDCR(2)='z'
                CDCR(3)='r'
              ELSE
                K=ABS(NDCR(1))
                CDCR( K+1 )='l'
                CDCR( K+2 )='z'
                CDCR( K+3 )='r'
              END IF
              NDCR(IDCRNMB)=3
            END IF

            IO1=IARG(5)
            IO2=IARG(6)
            IF ( MOD(NDEG(IO1),2) .NE. 1 ) THEN
              L1=NDEG(IO1)/4
              L2=NDEG(IO2)/4
            ELSE
              L1=NDEG(IO1)/2
              L2=NDEG(IO2)/2
            END IF
            NEX=1
            IF (IO1.GT.NOBT) NEX=NDEG(IO1)
            IF (IO2.GT.NOBT) NEX=NDEG(IO2)

            IF (ABS(L1-L2).NE.1) THEN
              WRITE(6,'(A,I3,I3)')
     &         'Incorrect orbital angular momenta l1 & l2 ',L1,L2
              CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            END IF
            T=ARG(1)*DSQRT(3D0/(4D0*3.14159265D0))
            DO ISB=0,NDCR(IDCRNMB)-1
              K=NDCR(1)*(IDCRNMB-1)+1+ISB
C              WRITE(6,*)' ISB=',ISB,' K=',K,' J=',J
              IF (CDCR(K).EQ.'x') THEN
                RR(1)= T/SQRT(2D0)
                RR(2)= 0D0
                RR(3)=-T/SQRT(2D0)
              ELSE IF (CDCR(K).EQ.'y') THEN
                RR(1)= T/SQRT(2D0)
                RR(2)= 0D0
                RR(3)= T/SQRT(2D0)
              ELSE IF (CDCR(K).EQ.'l') THEN
                RR(1)= 0D0 
                RR(2)= 0D0
                RR(3)= T 
              ELSE IF (CDCR(K).EQ.'z') THEN
                RR(1)= 0D0
                RR(2)= T
                RR(3)= 0D0
              ELSE IF (CDCR(K).EQ.'r') THEN
                RR(1)= T 
                RR(2)= 0D0
                RR(3)= 0D0 
              ELSE
                WRITE(6,*)
     &         'Error!  Wrong polarization is specified '//CDCR(K)
                CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
              END IF
              NBLK=3
            CALL PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LINXCD)
            END DO

          ELSE IF (ACMD(1:1).EQ.' '.AND.DLM.EQ.'( ') THEN 

            DO 
              CALL X_CODE(1,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
              IF (IERR.EQ.1) CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
              IF (IERR.EQ.2) EXIT
            END DO
          ELSE
            CALL ERR_UCMD(NCMD,ACMD,DLM,'OPRT:',LINXCD)
          END IF

          NCPX=MAX(NCPX,ICPX-1)
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        END DO ! WHILE (DLM .NE. ': ')
C-----------------------------------------------------------------
        NHOP(0)=NHOP(0)+1
        NHOP(NHOP(0)+1)=ICHN
      END DO ! WHILE (AWRD.EQ.'oprt' .AND. DLM.EQ.': ')
C
      IF ( NDCR(1).LT.0 ) NDCR(1)=-NDCR(1)
      IF ( NDCR(2).LT.0 ) NDCR(2)=-NDCR(2)
      IHP1(1)=ICHN
      IHP2(1)=ICHN

C --- dump the data
C      WRITE(6,*)'  ',ICHN/NBLK+1,'  interaction blocks'
      WRITE(6,*)'  ',NHOP(0),'  operators'
      WRITE(6,*)'Orbitals for continuum state '
      WRITE(6,'(15A5/)')(AOBT(IO),IO=NOBT+1,NOBTX)
C
      IF(NOBTXB.NE.0.AND.NOBTXB.NE.NOBTX) THEN
       WRITE(6,*)'Error! All of orbital(s) for continuum state must be'
     &     //' defined by "Exorb" command or don''t use this command'
     &     //' at line ',LINXCD
       STOP
      END IF

      WRITE(6,*)'Undefined configuration label(s)'
C      DO IST=1,NCI(0)
C        K=NCI(IST)+1
C        J=K
C        DO WHILE(ACI(IST,K).NE.'ZZZ')
C          K=K+1
C        END DO 
C        WRITE(6,'(9A8/)')('#'//ASTS(IST:IST)//ACI(IST,ICI),ICI=J,K-1)
C      END DO
C
      NMODE=NEXL*10+NCPX
      IF (NOBTX.GT.NOBT) THEN
        I=MOD(NDEG(NOBT+1),2)
        DO IO=NOBT+1,NOBTX
          IF( I .NE. MOD(NDEG(IO),2) ) THEN
            WRITE(6,*)
     &      'Error! Inconsistant degeneracies of the orbitals;'
     &    //'One of them is even number but another one is odd number'
            STOP
          END IF
        END DO !IO
        IF ( I.EQ.1 ) THEN
          NSPN(1)=1
          NSPN(2)=1
        END IF
      END IF

      WRITE(6,*)'U='
      DO IO1=1,NOBT
        DO IO2=IO1,NOBT
          IF (DABS(U(IO1,IO2)).GT.1D-7) THEN
            WRITE(6,'(2I6,E15.7)')IO1,IO2,U(IO1,IO2)
          END IF
        END DO
      END DO
      WRITE(6,*)'DLT='
      DO I=1,NDLT(0)
        WRITE(6,'(10I6,E15.7)')(NDLT((I-1)*10+K),K=1,10),DLT(I)
      END DO 
      WRITE(6,*)'EPH='
      NEPH=NINT( EPH(0) )
      WRITE(6,'(5E15.7)')(EPH(I),I=1,NEPH)
C
      END SUBROUTINE RDCD

C--------------------------------------------------
C Transform a variable name in X-code program 
C to a packed format for the interaction block 
C--------------------------------------------------

      SUBROUTINE PKDCHR(NARG,ARG,R)
      IMPLICIT NONE

C Parameters
      INTEGER   NDGT,   NCHRSET,    NBLKM
      PARAMETER(NDGT=8, NCHRSET=63, NBLKM=200)

C Dummy arguments
      INTEGER NARG
      REAL*8  ARG(*)
      REAL*8  R(NBLKM)

C Local variables
      INTEGER I,J,K,IL,IH,NDGT2
      CHARACTER*(NCHRSET) CHRSET
      CHARACTER C
C ----------------------

      CHRSET='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     & // 'abcdefghijklmnopqrstuvwxyz0123456789_' 
      R(1)=DBLE(NARG)
      NDGT2=NDGT/2
      DO I=0,(NARG-1)/NDGT
        IL=0
        DO J=MIN(NDGT2,NARG-NDGT*I),1,-1
          C=CHAR(NINT(ARG(I*NDGT+J)))
          K=INDEX(CHRSET,C)
          IL=IL*NCHRSET+K-1 
        END DO
        IH=0
        DO J=MIN(NDGT,NARG-NDGT*I),NDGT2+1,-1
          C=CHAR(NINT(ARG(I*NDGT+J)))
          K=INDEX(CHRSET,C)
          IH=IH*NCHRSET+K-1 
        END DO
        R(I+2)=DBLE(IH)*DBLE(NCHRSET)**(NDGT2+1)+DBLE(IL)
      END DO

      END SUBROUTINE PKDCHR 

C--------------------------------------------------
C Retrieve a variable name for X-code program 
C from a packed format in the interaction block 
C--------------------------------------------------

      SUBROUTINE RETCHR(R,NSYMB,ASYMB)
      IMPLICIT NONE

C Parameters
      INTEGER   NDGT,   NCHRSET,    NBLKM
      PARAMETER(NDGT=8, NCHRSET=63, NBLKM=200)

C Dummy arguments
      REAL*8         R(NBLKM)
      INTEGER        NSYMB
      CHARACTER*(32) ASYMB

C Local variables
      INTEGER I,J,K,IL,IH,NDGT2
      CHARACTER*(NCHRSET) CHRSET
C ----------------------

      CHRSET='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     & // 'abcdefghijklmnopqrstuvwxyz0123456789_' 

      ASYMB=' '
      NSYMB=ABS(NINT(R(1)))
      NDGT2=NDGT/2
      DO I=0,(NSYMB-1)/NDGT
        IH=NINT( R(I+2)/DBLE(NCHRSET)**(NDGT2+1) )
        IL=NINT( R(I+2)-DBLE(IH)*DBLE(NCHRSET)**(NDGT2+1) )
        DO J=1,MIN(NDGT2,NSYMB-NDGT*I)
          K=MOD(IL,NCHRSET)+1
          ASYMB(I*NDGT+J:I*NDGT+J)=CHRSET(K:K)
          IL=IL/NCHRSET
        END DO
        DO J=NDGT2+1,MIN(NDGT,NSYMB-NDGT*I)
          K=MOD(IH,NCHRSET)+1
          ASYMB(I*NDGT+J:I*NDGT+J)=CHRSET(K:K)
          IH=IH/NCHRSET
        END DO
      END DO

      END SUBROUTINE RETCHR 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C create an interaction block
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE PUTBLK(ICHN,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LIN)

      IMPLICIT NONE

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBLKM,    NBLKSL
      PARAMETER(NBLKM=200,NBLKSL=8)

C Dummy arguments
      INTEGER ICHN

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NTYP, NBDY
      INTEGER NSTM, NCIM, NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER IARG(8)

      INTEGER NBLK
      REAL*8  RR(NBLKM), RI(NBLKM)

      INTEGER NEX, ISB, ICPX
      INTEGER LIN

C Local variables
      INTEGER I,K,N,IEX,IOEX,IST1,IST2,ICI1,ICI2,ICHN1
C ------------

      IST1=IARG(1)
      IST2=IARG(2)
      ICI1=IARG(3)
      ICI2=IARG(4)
      NBLK=MAX(NBLK,NBLKSL)
C
      IF ( ICI1.LT.0 .OR. ICI2.LT.0 ) RETURN
  
      IF (ICHN+NBLK.GT.NHM) THEN
        N=1
        IF (ICPX.EQ.3) N=2
        ICHN=ICHN+NBLK*NEX*N 
        RETURN
      END IF

      IHP1(ICHN+5)=(NBLK*256+NTYP)*8+NBDY-1
      IHP2(ICHN+5)=(NBLK*256+NTYP)*8+NBDY-1
      IHP1(ICHN+6)=(IST2-1)*NSTM+IST1-1
      IHP2(ICHN+6)=LIN
      IHP1(ICHN+7)=ICI1
      IHP2(ICHN+7)=ICI2
      IHP1(ICHN+8)=(IARG(6)-1)*NOBTM+IARG(5)-1
      IHP2(ICHN+8)=(IARG(8)-1)*NOBTM+IARG(7)-1
 
      IOEX=0     
      IF (NBDY.EQ.1.AND.IARG(5).GT.NOBT) THEN
        IOEX=IARG(5)
      ELSE IF (IARG(6).GT.NOBT) THEN
        IOEX=IARG(6)
      ELSE IF (NBDY.EQ.2.AND.IARG(8).GT.NOBT) THEN
        IOEX=IARG(8)
      END IF
C
      IF ( MOD(ICPX,2).EQ.1 .OR. ICPX.EQ.0 ) THEN
        DO I=1,NBLK
          H(ICHN+I)=RR(I)
        END DO
        IHP1(ICHN+5)=IHP1(ICHN+5)+1*2
        IHP2(ICHN+5)=IHP1(ICHN+5)
        IF (ICPX/2.EQ.1) THEN
          DO I=1,NBLK
              H (ICHN+NBLK+I)=RI(I)
            IHP1(ICHN+NBLK+I)=IHP1(ICHN+I)
            IHP2(ICHN+NBLK+I)=IHP2(ICHN+I)
          END DO
          IHP1(ICHN+NBLK+5)=IHP1(ICHN+NBLK+5)+1*2
          IHP2(ICHN+NBLK+5)=IHP1(ICHN+NBLK+5)
        END IF
      ELSE
        IF (ICPX/2.EQ.1) THEN
          DO I=1,NBLK
            H(ICHN+I)=RI(I)
          END DO
          IHP1(ICHN+5)=IHP1(ICHN+5)+2*2
          IHP2(ICHN+5)=IHP1(ICHN+5)
        END IF
      END IF
C
      N=1
      IF (ICPX.EQ.3) N=2
      ICHN1=ICHN
      DO IEX=1,NEX
        DO K=0,(N-1)*NBLK,NBLK
          DO I=1,NBLK
            H(ICHN1+I)=H(ICHN+K+I)
            IHP1(ICHN1+I)=IHP1(ICHN+K+I)
            IHP2(ICHN1+I)=IHP2(ICHN+K+I)
          END DO
          IHP1(ICHN1+2)=ISB+100*(IEX-1)+10000*IOEX
          IHP2(ICHN1+2)=ISB+100*(IEX-1)+10000*IOEX
          ICHN1=ICHN1+NBLK
        END DO
      END DO

      ICHN=ICHN1

      END SUBROUTINE PUTBLK

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Evaluate Commands on the multipole interactions 
C in the Item "OPRT:"
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE TWOBDY(NCMD,ACMD,NARG,ARG,IARG
     &                 ,NOBT,NDEG,INRM,RED
     &                 ,NTYP,NBLK,RR,NEX,LINXCD)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX,  KMAX,       LMAX2
      PARAMETER(LMAX=8,KMAX=2*LMAX,LMAX2=3)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   ITXMAX,    NBLKM 
      PARAMETER(ITXMAX=128,NBLKM=200)

C Dummy arguments
      INTEGER NCMD 
      CHARACTER*(ITXMAX) ACMD

      INTEGER NARG
      REAL*8  ARG(NARG)
      INTEGER IARG(8)

      INTEGER NOBT 
      INTEGER NDEG(NOBTM)
      INTEGER INRM
      REAL*8  RED
      INTEGER NTYP

      INTEGER NBLK
      REAL*8  RR(NBLKM)

      INTEGER NEX,LINXCD

C Local variable
      INTEGER I,K,I1,I2,L1,L2,L3,L4,K1,K2,K3,K4
C -----------

      IF ( ACMD.EQ.'rk' ) THEN

        NTYP=1
        IF(   MOD( NDEG( IARG(5) ), 4 ) .NE.2 
     &   .OR. MOD( NDEG( IARG(6) ), 4 ) .NE.2 
     &   .OR. MOD( NDEG( IARG(7) ), 4 ) .NE.2 
     &   .OR. MOD( NDEG( IARG(8) ), 4 ) .NE.2 ) THEN
          WRITE(6,*)
     &     'Error! The Degeneracy of the orbital should be 4*L+2.'
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          STOP
        END IF

        L1=NDEG(IARG(5))/4
        L2=NDEG(IARG(6))/4
        L3=NDEG(IARG(7))/4
        L4=NDEG(IARG(8))/4

        K1=MAX(ABS(L1-L3),ABS(L2-L4))
        K2=MIN(L1+L3,L2+L4)
        K3=MAX(ABS(L1-L4),ABS(L2-L3))
        K4=MIN(L1+L4,L2+L3)
         
C
C       WRITE(6,*)'L1-L4=',L1,L2,L3,L4
C       WRITE(6,*)'K1-K4=',K1,K2,K3,K4
C       WRITE(6,*)'IO1-IO2=',IARG(5),IARG(6),IARG(7),IARG(8)
C  !!! ommit  k=0
        IF (IARG(5).EQ.IARG(7).AND.IARG(6).EQ.IARG(8)) THEN
          IF (K1.EQ.0) K1=2
          IF (IARG(5).EQ.IARG(6).AND.K3.EQ.0) K3=K3+2
        END IF
        IF ((IARG(5).EQ.IARG(6)).OR.(IARG(7).EQ.IARG(8))) THEN
          I1=0
          I2=0
          IF (NARG.NE.(K2-K1)/2+1.AND.NARG.NE.(K2-K1+K4-K3)/2+2)THEN
            WRITE(6,'(A,I4,A,I4)')
     &    'Error! The number of the arguments must be '
     &    ,(K2-K1)/2+1,' or ',(K2-K1+K4-K3)/2+2
            CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          END IF
        ELSE
          I1=0
          I2=(K2-K1)/2+1
          IF (INRM.EQ.1) THEN
            I1=(K4-K3)/2+1
            I2=0
          END IF
          IF (NARG.NE.(K2-K1+K4-K3)/2+2) THEN
            WRITE(6,'(A,I4)')
     & 'Error! The number of the arguments must be ',(K2-K1+K4-K3)/2+2
            CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
          END IF
        END IF
        I=0
        DO K=K1,K2,2
          I=I+1
          RR(I)=ARG((K-K1)/2+I1+1)*RED
        END DO
        DO K=K3,K4,2
          I=I+1
          RR(I)=ARG((K-K3)/2+I2+1)*RED
        END DO

        NEX=1
        IF (IARG(6).GT.NOBT) NEX=NDEG(IARG(6))
        IF (IARG(8).GT.NOBT) NEX=NDEG(IARG(8))
        NBLK=NARG

      ELSE IF( ACMD.EQ.'uj' ) THEN
      
        NTYP=8
        IF ( IARG(5).NE.IARG(6) .OR. IARG(7).NE.IARG(8) ) THEN
          WRITE(6,*)'Error! More than three orbitals are specified.'
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          STOP
        END IF

        IF (NARG.EQ.2) THEN
          RR(1)=ARG(1)
          RR(2)=ARG(1)
          RR(3)=ARG(2)
          RR(4)=ARG(2)
          IF ( IARG(5).EQ.IARG(6) ) RR(2)=RR(2)-RR(3)*2D0
        ELSE IF ( NARG.EQ.3 .AND. IARG(5).EQ.IARG(6) ) THEN
          RR(1)=ARG(1)
          RR(2)=ARG(2)
          RR(3)=ARG(3)
          RR(4)=ARG(3)
        ELSE IF ( NARG.EQ.4 .AND. IARG(5).EQ.IARG(6) ) THEN
          RR(1)=ARG(1)
          RR(2)=ARG(2)
          RR(3)=ARG(3)
          RR(4)=ARG(4)
        ELSE
          CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
        END IF
        NEX=1
        NBLK=4
      END IF

      END SUBROUTINE TWOBDY

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Evaluate Commands on the Crystal Field,
C Hybridization and Radiative Transition Matrices 
C in the Item "OPRT:"
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CRYST(NCMD,ACMD,NARG,ARG,IARG,NOBT,NDEG,NBLK,RR,RI
     &                ,NEX,ICPX,LINXCD)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX,   KMAX,        LMAX2
      PARAMETER(LMAX=8, KMAX=2*LMAX, LMAX2=3)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   ITXMAX,    NBLKM
      PARAMETER(ITXMAX=128,NBLKM=200)

C Dummy arguments
      INTEGER NCMD
      CHARACTER*(ITXMAX) ACMD

      INTEGER NARG
      REAL*8  ARG(NARG)
      INTEGER IARG(8)

      INTEGER NOBT
      INTEGER NDEG(NOBTM)

      INTEGER NBLK
      REAL*8 RR(NBLKM),RI(NBLKM)

      INTEGER NEX, ICPX, LINXCD

C Local variables
      CHARACTER*(32) SYM 
      LOGICAL QCMD
      REAL*8 VR(NBLKM),VI(NBLKM)
      REAL*8 UR(NBLKM,NBLKM),UI(NBLKM,NBLKM)
      INTEGER I,J,K,N,M,IO1,IO2,L1,L2,IP,NARGCM,KLIM,NCPXB
      REAL*8 THETA, ALF, BTA, GAM, AX, AY, AZ
      REAL*8 T, S, TR, TI
      REAL*8 A00,A20,A22,A40,A42,A43,A44,A60,A62,A63,A64,A66 
      REAL*8 VA1, VA11, VA12, VA2, VA21, V1A, V2A, V12A
      REAL*8 V1A2, V2A2, V12A2
      REAL*8 VB1, VB2, VB3
      REAL*8 VE,  VE1, VE2, V1E, V2E, V12E
      REAL*8 VT1, VT2
C ------------
C
      CALL ERR_NDEG(NDEG,NARG,IARG,NCMD,ACMD,LINXCD)
C
      IO1=IARG(5)
      IO2=IARG(6)
      IF ( MOD(NDEG(IO1),2) .NE. 1 ) THEN
        L1=NDEG(IO1)/4
        L2=NDEG(IO2)/4
      ELSE
        L1=NDEG(IO1)/2
        L2=NDEG(IO2)/2
      END IF
      NEX=1
      IF (IO1.GT.NOBT) NEX=NDEG(IO1)
      IF (IO2.GT.NOBT) NEX=NDEG(IO2)

      IF (L1 .GT. LMAX2 ) THEN
        WRITE(6,'(A,I4)')'Error! Too large anguler momentum l=',L1
        WRITE(6,'(A,I4)')' The maximum value is lmax=',LMAX2
        CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
      END IF
      KLIM=L1+L2

C rotation
      J=1
      IF (ACMD(1:1).EQ.'r') THEN

        ALF=ARG(1) 
        BTA=ARG(2)
        GAM=ARG(3)
        DO I=1,NARG-3
          ARG(I)=ARG(I+3)
        END DO
        NARG=NARG-3
        J=J+1

      ELSE IF (ACMD(1:1).EQ.'x') THEN

        THETA=ARG(1)
        TR=1D0/SQRT(ARG(2)**2+ARG(3)**2+ARG(4)**2)
        AX=ARG(2)*TR
        AY=ARG(3)*TR
        AZ=ARG(4)*TR
        DO I=1,NARG-4
          ARG(I)=ARG(I+4)
        END DO
        NARG=NARG-4
        J=J+1

      END IF
      IF (ACMD(J:J).EQ.'q' .OR. ACMD(J:J).EQ.'v') J=J+1
      SYM=ACMD(J:)
      QCMD=.TRUE.
      IF (ACMD(J:J).EQ.'v') QCMD=.FALSE. 
C
      A00=0D0
      A20=0D0
      A22=0D0
      A40=0D0
      A42=0D0
      A43=0D0
      A44=0D0
      A60=0D0
      A62=0D0
      A63=0D0
      A64=0D0
      A66=0D0
C
      IF(SYM.EQ.'so' .OR. SYM.EQ.'so3') THEN

        A00=ARG(1)
        NARGCM=1
        KLIM=0

      ELSE IF(SYM.EQ.'10dq' .OR. SYM.EQ.'oh' .OR. SYM.EQ.'td') THEN

        IF ( (L1.EQ.0 .OR. L1.EQ.1) .AND. SYM.NE.'10dq' ) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.2) THEN
          IF (NARG.EQ.1) THEN
            A40=2.1D0*ARG(1)
            NARGCM=1
          ELSE IF (SYM.NE.'10dq') THEN
            VE =ARG(1)
            VT2=ARG(2)
            A00=(2D0*VE+3D0*VT2)/5D0
            A40=2.1D0*(VE-VT2)
            NARGCM=2
          END IF
          A44=SQRT(5D0/14D0)*A40
        ELSE IF (L1.EQ.3 .AND. SYM.NE.'10dq' ) THEN
          VA2=ARG(1)
          VT1=ARG(2)
          VT2=ARG(3)
          A00= 1D0/7D0   *(    VA2 + 3D0*VT1 + 3D0*VT2)
          A40=-3D0/4D0   *(2D0*VA2 - 3D0*VT1 +     VT2)
          A60=39D0/280D0 *(4D0*VA2 + 5D0*VT1 - 9D0*VT2)
          A44= SQRT(5D0/14D0)*A40
          A64=-SQRT(7D0/2D0) *A60
          NARGCM=3
        ELSE
          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF (SYM.EQ.'10dq3' .OR. SYM.EQ.'oh3' .OR. SYM.EQ.'td3') THEN

        IF ( (L1.EQ.0 .OR. L1.EQ.1) .AND. SYM.NE.'10dq3' ) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.2) THEN
          IF (NARG.EQ.1) THEN
            A40=-1.4D0*ARG(1)
            NARGCM=1
          ELSE IF (SYM.NE.'10dq3') THEN
            VE =ARG(1)
            VT2=ARG(2)
            A00= ( 2D0*VE + 3D0*VT2 )/5D0
            A40=-1.4D0*(VE-VT2)
            NARGCM=2
          END IF
          A43= SQRT(10D0/7D0)*A40
        ELSE IF ( L1.EQ.3 .AND. SYM.NE.'10dq3' ) THEN
          VA2=ARG(1)
          VT1=ARG(2)
          VT2=ARG(3)
          A00=  1D0/7D0   *(    VA2 + 3D0*VT1 + 3D0*VT2)
          A40=  1D0/2D0   *(2D0*VA2 - 3D0*VT1 +     VT2)
          A60= 26D0/105D0 *(4D0*VA2 + 5D0*VT1 - 9D0*VT2)
          A43= SQRT(10D0/7D0)     *A40
          A63=-SQRT(70D0/3D0)/8D0 *A60
          A66= SQRT(77D0/3D0)/8D0 *A60
          NARGCM=3
        ELSE
          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF ( SYM.EQ.'d3d' .OR. SYM.EQ.'c3v') THEN

        IF (L1.EQ.0) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.1) THEN
          VE =ARG(1)
          VA2=ARG(2)
          NARGCM=2
          A00=(2D0*VE+VA2)/3D0
          A20=5D0/3d0*(VA2-VE)
        ELSE IF (L1.EQ.2) THEN
          V1E =ARG(1) 
          V2E =ARG(2) 
          VA1 =ARG(3) 
          V12E=ARG(4) 

          A00=( 2D0*V1E + 2D0*V2E + VA1 )/5D0 
          A20= VA1 -V2E - SQRT(8D0)*V12E
          T=-1.4D0*( V1E - ( VA1 + 2D0*V2E )/3D0 )
          S=4D0/3D0*(VA1-V2E) + SQRT(8D0)*V12E
          A40=T+S
          A43= SQRT(10D0/7D0)*T - 0.5D0*SQRT(7D0/10D0)*S
          NARGCM=4
        ELSE IF (L1.EQ.3) THEN
          V1A2 =ARG(1) 
          V1E  =ARG(2) 
          V2A2 =ARG(3) 
          V2E  =ARG(4) 
          VA1  =ARG(5) 
          V12A2=ARG(6) 
          V12E =ARG(7) 

          A00=( V1A2 + V2A2 + VA1 + 2D0*V1E + 2D0*V2E )/7D0 
          A20=-5D0/28D0*(V2A2 + 5D0*VA1 + 4D0*SQRT(5D0)*V12A2
     &                  -V1E  - 5D0*V2E - 2D0*SQRT(5D0)*V12E ) 
          A40= 1D0/14D0*(14D0*V1A2 + 13D0*V2A2 - 4D0*sqrt(5D0)*V12A2
     &       + 9D0*VA1 - 34D0*V1E  -  2D0*V2E  +16D0*sqrt(5D0)*V12E)
          A60=13D0/420D0*(32D0*V1A2 + 25D0*V2A2 - 28D0*sqrt(5D0)*V12A2
     &        - 3D0*VA1 + 15D0*V1E -  69D0*V2E  - 42D0*sqrt(5D0)*V12E)
          A43= SQRT(5D0/14D0)*( 2D0*V1A2 -2D0*V2A2 + V12A2/SQRT(5D0)
     &                      - V1E +       V2E -  4D0/sqrt(5D0)*V12E )
          A63=-13D0/(6D0*SQRT(210D0))*( 4D0*V1A2 - 4D0*V2A2 
     &                               + 2D0/SQRT(5D0)*V12A2 
     &             + 9D0*V1E - 9D0*V2E + 36D0/SQRT(5D0)*V12E )
          A66= 143D0/(60D0*SQRT(231D0))*(4D0*V1A2 + 5D0*V2A2 
     &                            + 4D0*SQRT(5D0)*V12A2 - 9D0*VA1 )
          NARGCM=7
        ELSE
          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF ( SYM.EQ.'d3h' ) THEN

        IF (L1.EQ.0) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.1) THEN
          VE1 =ARG(1)
          VA11=ARG(2)
          A00=(2D0*VE1+VA11)/3D0
          A20=5D0/3d0*(VA11-VE1)
          NARGCM=2
        ELSE IF (L1.EQ.2) THEN
          VA11=ARG(1) 
          VE1 =ARG(2) 
          VE2 =ARG(3) 

          A00=( 2D0*VE1 + 2D0*VE2 + VA11 )/5D0 
          A20= VA11+VE2-2D0*VE1 
          A40= 3d0/5d0*(3D0*VA11-4D0*VE2+VE1)
          NARGCM=3
        ELSE IF (L1.EQ.3) THEN
          VA12=ARG(1)
          VE1 =ARG(2)
          VE2 =ARG(3)
          VA11=ARG(4)
          VA21=ARG(5)

          A00= (VA11+VA12+VA21+2D0*VE1+2D0*VE2)/7d0;
          A20= -5D0/28D0*( 5D0*VA11 - 4D0*VA12 + 5D0*VA21 - 6D0*VE1 )
          A40=  3d0/14D0*( 3D0*VA11 + 6D0*VA12 + 3D0*VA21 
     &                   + 2D0*VE1  - 14D0*VE2 )
          A60=-13d0/140d0*(    VA11 - 20D0*VA12 +    VA21 
     &                   + 30D0*VE1  - 12D0*VE2 )
          A66=429D0/(20D0*SQRT(231D0))*(VA11-VA21)
          NARGCM=5
        ELSE

          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF(SYM.EQ.'d4h' .OR. SYM.EQ.'d2d') THEN

        IF (L1.EQ.0) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.1) THEN
          VE =ARG(1)
          VA2=ARG(2)
          NARGCM=2
          A00=(2D0*VE+VA2)/3D0
          A20=5D0/3d0*(VA2-VE)
        ELSE IF (L1.EQ.2) THEN
          VB1=ARG(1)
          VA1=ARG(2)
          VE =ARG(3)
          VB2=ARG(4)
C
          A00=(VA1+VB1+2D0*VE+VB2)/5D0
          A20=-(VB1-VA1) + (VE-VB2)
          T=2.1D0*( (VA1+VB1)/2D0 - (2D0*VE+VB2)/3D0 )
          S=-( 3D0*(VB1-VA1) + 4D0*(VE-VB2) )/4D0
          A40= T + S
          A44= SQRT(5D0/14D0)*T - SQRT(7D0/10D0)*S
          NARGCM=4
        ELSE IF (L1.EQ.3) THEN
          VB1 =ARG(1)
          V1E =ARG(2)
          VA2 =ARG(3)
          V2E =ARG(4)
          VB2 =ARG(5)
          V12E=ARG(6)

          A00= (VA2+VB1+VB2+2D0*V1E+2D0*V2E)/7D0
          A20=  5D0/7D0*( VA2-V1E + SQRT(15D0)*V12E )
          A40=  3D0/28D0 *(12D0*VA2 + 9D0*V1E +  7D0*V2E
     &                    -14D0*VB1 -14D0*VB2 -  2D0*SQRT(15D0)*V12E ) 
          A60= 13D0/280D0*(40D0*VA2 -25D0*V1E - 39D0*V2E
     &                    +12D0*VB1 +12D0*VB2 - 14D0*SQRT(15D0)*V12E )
          A44= sqrt(5D0/14D0)*3D0/4D0  *( 2D0*VB2 - 2D0*VB1
     &         + 3D0*V1E - 3D0*V2E ) +  3D0/2D0 *SQRT(3D0/14D0)*V12E
          A64= sqrt(7D0/2D0)*39D0/280D0*( 4D0*VB2 - 4D0*VB1
     &         - 5D0*V1E + 5D0*V2E ) - 13D0/20D0*SQRT(15D0/14D0)*V12E
          NARGCM=6
        ELSE
          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF(SYM.EQ.'d2h' .OR. SYM.EQ.'d2') THEN

        IF (L1.EQ.0) THEN
          A00=ARG(1)
          NARGCM=1
          KLIM=0
        ELSE IF (L1.EQ.1) THEN
          VB3 =ARG(1)
          VB2 =ARG(2)
          VB1 =ARG(3)
          A00=(VB1+VB2+VB3)/3D0
          A20=5D0/3d0*(VB1 - (VB2+VB3)/2D0 )
          A22=5D0/(2D0*SQRT(6D0))*(VB3-VB2)
          NARGCM=3
        ELSE IF (L1.EQ.2) THEN
          V2A =ARG(1)
          V1A =ARG(2)
          VB3 =ARG(3)
          VB2 =ARG(4)
          VB1 =ARG(5)
          V12A=ARG(6)
C
          A00= (V1A+V2A+VB1+VB2+VB3)/5d0
          A20=  V1A + (VB2+VB3)/2D0 - (V2A+VB1)
          A22=-( SQRT(3D0)*(VB3-VB2) + 4D0*V12A ) / (2D0*SQRT(2D0));
          A40= 3D0*( (V2A+VB1)/2D0 + V1A - 2D0*A00 );
          A42= 3D0*( VB2-VB3 + SQRT(3D0)*V12A ) / SQRT(10D0);
          A44= 21D0*(V2A-VB1) / (2D0*SQRT(70D0));
          NARGCM=6
        ELSE
          WRITE(6,*)'Error! Wrong angular momentum l=',L1
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE IF( SYM.NE.'ak' .AND. SYM.NE.'cak' ) THEN

        WRITE(6,*)'Unknown command "'//ACMD(1:NCMD)
     &   //'" was found at LINE=',LINXCD
        STOP
      END IF

      IF( SYM.EQ.'ak' .OR. SYM.EQ.'cak' ) THEN

        NCPXB=3
        IF (SYM(1:1) .EQ. 'c') NCPXB=4

        IF ( MOD(NARG,NCPXB).NE.0 ) THEN
          CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
        END IF

        KLIM=0
        DO I=0,NARG/NCPXB-1
          K=NINT( ARG(I*NCPXB+1) )
          M=NINT( ARG(I*NCPXB+2) )
          IF ( ABS(M).GT.K .OR. K.LT.0 ) THEN
            WRITE(6,'(A,I3,A,I3)')
     &       'Error! Wrong value for K or M: K=',K,'; M=',M
            WRITE(6,'(A,I3,A,I3)')
     &         'K >= |M| and K >= 0 should be satisfied.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          IF ( MOD(K,2) .EQ. MOD(L1+L2,2) ) KLIM=MAX(KLIM,K)
        END DO

        NBLK=0 
        DO K=ABS(L1-L2),KLIM,2
          NBLK=NBLK+2*K+1
        END DO 

        IF ( NBLK .GT. NBLKM ) THEN
          WRITE(6,'(A,I3)')
     &     'Error! The value of K is too large: K=',K
          WRITE(6,'(A,I3)')
     &     'The value of NBLKM must be enhanced more than NBLKM=',NBLK
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

        DO I=1,NBLK 
          RR(I)=0D0
          RI(I)=0D0
        END DO 

        DO I=0,NARG/NCPXB-1
          K=NINT( ARG(I*NCPXB+1) )
          M=NINT( ARG(I*NCPXB+2) )
          IF ( MOD(K,2) .EQ. MOD(L1+L2,2) ) THEN
            IP=0
            DO N=ABS(L1-L2),K-2,2
              IP=IP+2*N+1
            END DO 
            RR(IP+K+M+1)=ARG(I*NCPXB+3)
            IF (NCPXB.EQ.4) RI(IP+K+M+1)=ARG(I*NCPXB+4)
          END IF
        END DO

      ELSE

        NBLK=0 
        DO K=ABS(L1-L2),KLIM,2
          NBLK=NBLK+2*K+1
        END DO 

        IF ( NBLK .GT. NBLKM ) THEN
          WRITE(6,'(A,I3)')
     &     'Error! The value of K is too large K=',K
          WRITE(6,'(A,I3)')
     &     'The value of NBLKM must be enhanced more than NBLKM=',NBLK
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

        DO I=1,NBLK 
          RR(I)=0D0
          RI(I)=0D0
        END DO 

        IF (NARGCM.NE.NARG) THEN
          CALL ERR_NARG(NARG,NCMD,ACMD,LINXCD)
        END IF

        RR(1) = A00 
        RR(2) = A22
        RR(4) = A20
        RR(6) = A22

        RR(7) = A44 
        RR(8) =-A43 
        RR(9) = A42 
        RR(11)= A40
        RR(13)= A42 
        RR(14)= A43
        RR(15)= A44 

        RR(16)= A66 
        RR(18)= A64
        RR(19)=-A63
        RR(20)= A62
        RR(22)= A60 
        RR(24)= A62
        RR(25)= A63
        RR(26)= A64 
        RR(28)= A66 

      END IF
C
      IF ( ACMD(1:1).EQ.'r' .OR. ACMD(1:1).EQ.'x' ) THEN
        IF ( ACMD(1:1).EQ.'x' ) THEN
          ALF = ATAN2(AZ*SIN(THETA/2D0),COS(THETA/2d0)) - ATAN2(AX,AY)
          BTA = 2d0*ATAN2( SQRT(1D0-AZ**2)*SIN(THETA/2D0)
     &     , SQRT( COS(theta/2D0)**2 + (AZ*SIN(THETA/2D0))**2 ) )
          GAM = ATAN2(AZ*SIN(THETA/2D0),COS(THETA/2d0)) + ATAN2(AX,AY)
        END IF
        IP=0
        DO K=ABS(L1-L2),KLIM,2
          CALL ROTS(2*K+1,NBLKM,UR,UI,ALF,BTA,GAM)
          CALL MOVV(2*K+1,VR,RR(IP+1))
          CALL MOVV(2*K+1,VI,RI(IP+1))
          DO I=1,2*K+1
            TR=0D0
            TI=0D0
            DO J=1,2*K+1
              TR=TR+UR(I,J)*VR(J)-UI(I,J)*VI(J)
              TI=TI+UR(I,J)*VI(J)+UI(I,J)*VR(J)
            END DO
            RR(IP+I)=TR
            RI(IP+I)=TI
          END DO
          IP=IP+2*K+1
        END DO
      END IF

      ICPX=0
      DO I=1,NBLK
        IF (ABS(RR(I)).GT.1D-13) ICPX=2*(ICPX/2)+1
        IF (ABS(RI(I)).GT.1D-13) ICPX=2+MOD(ICPX,2)
      END DO
C
      END SUBROUTINE CRYST

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Evaluation of an orbital label in the X-card.
C 
C     ISTS=0: definition of an orbital label in the intem CNFG:
C     ISTS=1: search an orbital label
C     ISTS=2: search or define an orbital label
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GTORB(IOLBL,ISTS,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &                ,NOBT,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
      IMPLICIT NONE

C Parameters

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

      INTEGER   ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy arguments
      INTEGER IOLBL, ISTS 
      REAL*8 DNUM2

      INTEGER  NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER LIN, IUNIT, IERR, ITYP

      INTEGER NOBT,NOBTX
      CHARACTER*(NLBLM) AOBT(NOBTM)
      INTEGER NDEG(NOBTM)

      INTEGER NCMD
      CHARACTER*(ITXMAX) ACMD 
      INTEGER LINXCD

C Local variable
      INTEGER NL,IO
      CHARACTER*(ITXMAX) AL
C -------

      NL=0

      IOLBL=0
      DO IO=1,NOBTX
        IF ( AOBT(IO) .EQ. AWRD(:NWRD) ) IOLBL=IO
      END DO

      IF ( IOLBL .EQ. 0 ) THEN

        IF ( ISTS .EQ. 1 ) THEN
          WRITE(6,*) 'Error! Undefined orbital level '
     &      //AWRD(:NWRD)//' was found.'
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

        IF (ISTS .EQ. 0) THEN
          IF ( NOBT .EQ. 0 ) THEN
            NOBTX=0
            DO IO=1,NOBTM
              AOBT(IO)='   '
            END DO
          END IF
          NOBT=NOBT+1
          NOBTX=NOBT
        ELSE
          NOBTX=NOBTX+1
        END IF

        IOLBL=NOBTX

        AOBT(IOLBL)=AWRD(:NWRD)
        IF ( NWRD.GE.2 .AND. NL .EQ. 0 ) THEN
          NDEG(IOLBL)=4*INDEX('zspdfghijkl',AWRD(NWRD:NWRD))-6
          IF (NDEG(IOLBL).LE.-6) THEN
            WRITE(6,*)'Error!  A wrong angular momentum'
     &        //' in an orbital label '//AWRD(NWRD:NWRD)
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
        ELSE IF ( NWRD.GE. 3) THEN 
          IF ( ITYP .NE. 2 ) THEN
            WRITE(6,*)'Error! A wrong orbital label '//AWRD(:NWRD)
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          NDEG(IOLBL)=NINT(DNUM2(NL,AL,ITYP))
          IF (NDEG(IOLBL).LE.0) THEN
            WRITE(6,*)
     &      '  ERROR! A wrong orbital degeneracy ->',NDEG(IOLBL)
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
        ELSE
          WRITE(6,*)
     &  'Error! An unidentified symbol appeared: '//AWRD(:NWRD)//DLM
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF

      ELSE
        IF ( ISTS .EQ. 0 ) THEN
          WRITE(6,*) 'Error! A duplicated orbital label '
     &     //AWRD(:NWRD)//' was found.'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF
      END IF  

      END SUBROUTINE GTORB

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Error output for an unknown command 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ERR_UCMD(NCMD,ACMD,DLM,CITEM,LINXCD)
      IMPLICIT NONE

C Parameters
      INTEGER ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy argumets
      INTEGER NCMD
      CHARACTER*(ITXMAX) ACMD
      CHARACTER*(2) DLM
      CHARACTER*(5) CITEM
      INTEGER LINXCD
C ----------

      IF ( NCMD.EQ.0 .AND. DLM.EQ.'  ' ) THEN
        WRITE(6,'(A,I10)')'Error! Wrong format'
     &   //' in the item "'//CITEM//'" at line=',LINXCD
      ELSE 
        WRITE(6,'(A,I10)')'Error! Unknown command '//ACMD(1:NCMD)
     &   //' was found in the item "'//CITEM//'" at line=',LINXCD
      END IF 
      STOP ! Error!!!
      END SUBROUTINE ERR_UCMD
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Error output for the exceeded numbers of the basis functions 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ERR_BSZ(IST,NPARA,CPARA,NSTM,NCIM,NCI,NSFT,IERR
     &,ASTS,ACI,AOBT)
C
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      INTEGER IST,NPARA
      CHARACTER*(5) CPARA

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)

      INTEGER IERR

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variable
      INTEGER N
C ----------

      N=NSFT(NCI(IST)+1,IST)
C!>>>START F90
C      NPARA=MAX(NPARA,N) 
C!>>>END F90 
      IF (N.GT.NPARA) THEN
        WRITE(6,'(A,I10)')'Error! Too many basis functions'
     &    //' in the state #'//ASTS(IST:IST)//' ==>',N
        WRITE(6,'(A,I10,A)')'   The parameter '//CPARA//'=',NPARA,
     &       ' must be enchanced more than above.'
        IERR=1
      ELSE IF (N.LT.1) THEN
        WRITE(6,'(A,I10)')'Error! Too many basis functions'
     &    //'  to express within the integer type.'
     &    //'  in the state #'//ASTS(IST:IST)
        IERR=1
      ELSE
        WRITE(6,'(A,I10,A,I10)')'The number of the basis functions for'
     & //' the state #'//ASTS(IST:IST)//' ==>',N,'  '//CPARA//'=',NPARA
      END IF
      END SUBROUTINE ERR_BSZ

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Error output for wrong number of arguments 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ERR_NARG(NARG,NCMD,ACMD,LINXCD)
      IMPLICIT NONE

C Parameters
      INTEGER ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy arguments
      INTEGER NARG,NCMD
      CHARACTER*(ITXMAX) ACMD
      INTEGER LINXCD
C -------

      WRITE(6,'(A,I6)')'Error in a command "'//ACMD(1:NCMD)
     &        //'" at line ',LINXCD
      WRITE(6,'(A,I6)')'   Incorrect number of arguments NARG=',NARG
      IF (NARG.EQ.0) THEN
        WRITE(6,*)
     & '   Variable on the right hand side of "=" may be not defined.' 
      END IF
      STOP ! Error!!!
      END SUBROUTINE ERR_NARG

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Error output with a command name and the line 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ERR_CMDLIN(NCMD,ACMD,LINXCD)
      IMPLICIT NONE

C Parameters
      INTEGER ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy arguments
      INTEGER NCMD
      CHARACTER*(ITXMAX) ACMD
      INTEGER LINXCD
C ---------

      IF (NCMD.EQ.0) THEN
       WRITE(6,'(A,I6,A)')
     &         '   at the line ',LINXCD,' in your X-card.' 
      ELSE
        WRITE(6,'(A,I6,A)')' in a command "'//ACMD(1:NCMD)
     &        //'" at the line ',LINXCD,' in your X-card.' 
      END IF
      STOP ! Error!!!
      END SUBROUTINE ERR_CMDLIN

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Error output for wrong degeneracies of orbitals for a command
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ERR_NDEG(NDEG,NARG,IARG,NCMD,ACMD,LINXCD)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER NARG 
      INTEGER IARG(8)
      INTEGER NCMD
      CHARACTER*(ITXMAX) ACMD
      INTEGER LINXCD

C Local variables
      INTEGER NDEG1,NDEG2
C ----------

      NDEG1=NDEG( IARG(5) )
      NDEG2=NDEG( IARG(6) )
      IF(  ( MOD(NDEG1,4).NE.2 .OR. MOD(NDEG2,4).NE.2 )
     &.AND.( MOD(NDEG1,2).NE.1 .OR. MOD(NDEG2,2).NE.1 ) ) THEN
        WRITE(6,*)
     & 'The degeneracy of the orbital should be 4*L+2 or 2*L+1.'
        WRITE(6,'(A,I6)')
     & ' for the command "'//ACMD(1:NCMD)//'" at line=',LINXCD
        STOP
      END IF
      END SUBROUTINE ERR_NDEG

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Check for the consistency of the XCARD 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CHKBLK(NSTM,NCIM,NCI,NOBT,NOBTX,NELC,NDEG
     &                 ,NHOP,NHM,H,IHP1,IHP2,NHDM
     &                 ,NDLT,DLT,NHML,NMODE,IERR
     &                 ,ASTS,ACI,AOBT)
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NLBLM,  NDLTM
      PARAMETER(NLBLM=10,NDLTM=200)

C Dummy arguments
      INTEGER NSTM, NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
     
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)

      INTEGER NHML, NMODE, IERR

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variable
      INTEGER NE(NOBTM+10)
      INTEGER IOB(4)
      INTEGER NEXL,N,I,IOP,IST,ICI,IO,IDLT,LINERR,IST1B,IST2B
      INTEGER ICH,NTYP,NBDY,IST1,IST2,ICI1,ICI2,IOEX,LIN,NBLK
C Function
      INTEGER NNELC
C ---------

      NEXL=MOD(NMODE/10,10)
      WRITE(6,*)'Now checking consistancy of your XCARD ....'
C
      I=NHOP(0)
      IF (NHOP(I+1).GT.NHM) THEN
        WRITE(6,*)'The interaction block area overflowed'
     &   //' at the subroutine CHKBLK.'
        WRITE(6,'(A,I10,A,I10)')
     &      'The value of the parameter NHM=',NHM,
     &      ' must be enhanced more than ',NHOP(I+1)
        IERR=1
      END IF

      IF ( NEXL.NE.0 .AND. NHDM.LT.NCIM ) THEN
        WRITE(6,*)'Parameter NHMD must be enhanced.'
        WRITE(6,*)'NHMD>=NCIM for EXL mode.' 
        IERR=1
      END IF
C
C Check for definitions of number of the DMRG's basis functions
C
      DO IST=1,NCI(0)
        DO ICI=1,NCI(IST)
          DO IO=1,NOBT
            IF ( NELC(IO,ICI,IST)*NDEG(IO)/4 .LT. 0 ) THEN
              WRITE(6,*)'Incomplete definition of the number'
              WRITE(6,*)' of the DMRG''s reduced basis functions'
              WRITE(6,*)' orbital #',IO,' config. #',ICI,' state #',IST 
              WRITE(6,*)' # of electrons ',NELC(IO,ICI,IST)
              IERR=1
            END IF
          END DO
        END DO
      END DO
C
C Check for conservation of electron number in NELC 
C
      DO IST=1,NCI(0)
        N=0
        DO IO=1,NOBT
          N=N+NNELC(IO,1,IST,NSTM,NCIM,NOBT,NELC)
        END DO
        DO ICI=2,NCI(IST)
          I=0
          DO IO=1,NOBT 
            I=I+NNELC(IO,ICI,IST,NSTM,NCIM,NOBT,NELC)
          END DO
          IF (I.NE.N) THEN
            WRITE(6,*)'Electron number must be the same in each state'
            WRITE(6,*)' Config. #'//ASTS(IST:IST)//ACI(IST,ICI)
            WRITE(6,*)' The electron numbers specified:',I,N
            IERR=1
          END IF
        END DO
      END DO
      IF (IERR.NE.0) STOP
C
C Check for the consistency of the interaction block in IHP1 & IHP2
C
      LINERR=0
      DO IOP=1,NHOP(0)
        IST1B=0
        IST2B=0
        ICH=NHOP(IOP)
        DO WHILE( ICH .LT. NHOP(IOP+1) )

C --- IHP1(ICH+5)=((NBLK*256+NTYP)*4+ICPX)*2+NBDY-1
          IOEX=IHP1(ICH+2)/10000
          I     =        IHP1(ICH+5)
          NBDY  =MOD(I,2)+1
          I=I/8
          NTYP  =MOD(I,256)
          NBLK  =I/256

          IST1  =MOD(IHP1(ICH+6),NSTM) + 1
          IST2  =    IHP1(ICH+6)/NSTM  + 1
          LIN   =    IHP2(ICH+6)
          ICI1  =    IHP1(ICH+7)
          ICI2  =    IHP2(ICH+7)
          IOB(1)=MOD(IHP1(ICH+8),NOBTM) + 1
          IOB(2)=    IHP1(ICH+8)/NOBTM  + 1
          IOB(3)=MOD(IHP2(ICH+8),NOBTM) + 1
          IOB(4)=    IHP2(ICH+8)/NOBTM  + 1
C
          IF ( IST1.GT.NCI(0) .OR. IST2.GT.NCI(0)
     &     .OR. IST1.LE.0 .OR. IST2.LE.0 ) THEN 
            WRITE(6,*)'<break point> wrong state lebel in CHKBLK' 
            WRITE(6,*)'IST1,IST2=',IST1,IST2
            STOP
          END IF
C
          IF (IST1B.EQ.0) THEN
            IST1B=IST1
            IST2B=IST2
          END IF
          IF( (IST1.NE.IST1B.OR.IST2.NE.IST2B) .AND. LINERR.NE.LIN)THEN
            WRITE(6,*) 'States in the configuration label must' 
     &       //' be the same in each operator'
            IF (IST1.NE.IST1B) THEN
              WRITE(6,'(A,I6)')
     &        '  State in the first label in a command is #'
     &        //ASTS(IST1:IST1)//' at line ',LIN
              WRITE(6,*)
     &        '    whereas that in others are #'//ASTS(IST1B:IST1B)
            ELSE IF (IST2.NE.IST2B) THEN
              WRITE(6,'(A,I6)')
     &        '  State in the second label in a command is #'
     &        //ASTS(IST2:IST2)//' at line ',LIN
              WRITE(6,*)
     &         '    whereas that in others are #'//ASTS(IST2B:IST2B)
            END IF
            LINERR=LIN
            IERR=1
          END IF
          IF( (IOP.LE.NHML.AND.IST1.NE.IST2) .AND. LINERR.NE.LIN) THEN
            WRITE(6,*)
     &      'Two different states are appeared in a Hamiltonian #',IOP
            WRITE(6,*)
     &      '   #'//ASTS(IST1:IST1)//' and #'//ASTS(IST2:IST2) 
            WRITE(6,*)'   at line=',LIN
            LINERR=LIN
            IERR=1
          END IF
C
          IF ( ICI1.GT.NCI(IST1) .OR. ICI2.GT.NCI(IST2)
     &    .OR. ICI1.LE.0 .OR. ICI2.LE.0 ) THEN 
            WRITE(6,*)
     &      '<break point> wrong configuration lebel in CHKBLK' 
            WRITE(6,*)'ICI1,ICI2=',ICI1,ICI2
            STOP
          END IF
C
          DO IO=1,NOBT
            NE(IO)=NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
          END DO
          DO IO=NOBT+1,NOBTX
            NE(IO)=0
          END DO
          DO I=1,NBDY
            IF (IOP.LE.NHML.AND.IOB(I).GT.NOBT.AND.LINERR.NE.LIN) THEN
              WRITE(6,'(A,I3,A)')'Error! An orbital "' //AOBT(IOB(I))
     &        // '" in a Hamiltonian #', IOP, ' is not defined'
              WRITE(6,'(A,I7)') ' in the item "CNFG:" at line=',LIN
              LINERR=LIN
              IERR=1
            END IF
            NE(IOB(I))=NE(IOB(I))-1
            NE(IOB(I+NBDY))=NE(IOB(I+NBDY))+1
          END DO
          DO IO=1,NOBT
C NTYP=5 --- "Mat" command
            IF (NE(IO) .NE. NNELC(IO,ICI2,IST2,NSTM,NCIM,NOBT,NELC)
     &         .AND. LINERR.NE.LIN .AND. NTYP.NE.5 ) THEN
              WRITE(6,*)
     &        'The configuration #'//ASTS(IST2:IST2)//ACI(IST2,ICI2)
     &        //' can not be reached from the configuration #'
     &        //ASTS(IST1:IST1)//ACI(IST1,ICI1)
              IF (NBDY.EQ.1) THEN
                WRITE(6,*)' by removing an electron from an orbital '
     &          //AOBT(IOB(1))(1:5)//'and putting it to '
     &          //AOBT(IOB(2))(1:5)
              ELSE IF (NBDY.EQ.1) THEN
                WRITE(6,*)' by removing two electrons from orbitals '
     &          //AOBT(IOB(1))(1:5)//'and '//AOBT(IOB(2))(1:5)
     &          //' and putting them to '
     &          //AOBT(IOB(3))(1:5)//'and '//AOBT(IOB(4))(1:5)
              END IF
              WRITE(6,'(A,I6)') '   at line=',LIN
              LINERR=LIN
              IERR=1
              GOTO 80 
            END IF
          END DO
          ICH=ICH+NBLK
        END DO ! ICH loop
 80     CONTINUE
      END DO
C
C checking consistency of the delta block in NDLT 
C
      DO IDLT=1,NDLT(0)
        ICH=(IDLT-1)*10
        LIN =NDLT(ICH+1)
        NBDY=NDLT(ICH+2)
        IST1=NDLT(ICH+3)
        IST2=NDLT(ICH+4)
        ICI1=NDLT(ICH+5)
        ICI2=NDLT(ICH+6)
        IOB(1)=NDLT(ICH+7)
        IOB(2)=NDLT(ICH+8)
        IOB(3)=NDLT(ICH+9)
        IOB(4)=NDLT(ICH+10)
C
        IF ( IST1.GT.NCI(0) .OR. IST2.GT.NCI(0)
     &   .OR. IST1.LE.0 .OR. IST2.LE.0 ) THEN 
          WRITE(6,*)'<break point> wrong state lebel in CHKBLK' 
          WRITE(6,*)'IST1,IST2=',IST1,IST2
          STOP
        END IF
        IF ( ICI1.GT.NCI(IST1) .OR. ICI2.GT.NCI(IST2)
     &  .OR. ICI1.LE.0         .OR. ICI2.LE.0 ) THEN
          WRITE(6,*)
     &    'Error! Wrongly specified configurations #'
     &    //ASTS(IST1:IST1)//ACI(IST1,ABS(ICI1))//'and #'
     &    //ASTS(IST2:IST2)//ACI(IST2,ABS(ICI2))
          WRITE(6,'(A,I6)')
     &      ' are found in the "Dlt" command at line=',LIN
          STOP
        END IF
C
        DO IO=1,NOBT
          NE(IO)=NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
        END DO
        
        DO I=1,NBDY
          IF (IOB(I).GT.NOBT) THEN
            WRITE(6,*)' wrong orbit #',IOB(I)
            IERR=1
          END IF
          NE(IOB(I))=NE(IOB(I))-1
          NE(IOB(I+NBDY))=NE(IOB(I+NBDY))+1
        END DO
        DO IO=1,NOBT
          IF ( NE(IO) .NE. NNELC(IO,ICI2,IST2,NSTM,NCIM,NOBT,NELC)
     &       .AND. NTYP.NE.5 .AND. LINERR.NE.LIN ) THEN
            WRITE(6,*)
     &          'The configuration #'//ASTS(IST2:IST2)//ACI(IST2,ICI2)
     &        //' can not be reached from the configuration #'
     &        //ASTS(IST1:IST1)//ACI(IST1,ICI1)
            IF (NBDY.EQ.1) THEN
               WRITE(6,*)' by removing an electron from an orbital '
     &        //AOBT(IOB(1))(1:5)//'and putting it to '
     &        //AOBT(IOB(2))(1:5)//'.'
            ELSE IF (NBDY.EQ.1) THEN
               WRITE(6,*)' by removing two electrons from orbitals '
     &        //AOBT(IOB(1))(1:5)//'and '//AOBT(IOB(2))(1:5)
     &        //' and putting them to '
     &        //AOBT(IOB(3))(1:5)//'and '//AOBT(IOB(4))(1:5)
            END IF
            WRITE(6,'(A,I6)')
     &         ' in the "Dlt" command at line=',LIN
            LINERR=LIN
            IERR=1
            GOTO 130 
          END IF
        END DO
      END DO
 130  CONTINUE
C
      IF (IERR.EQ.0) THEN
        WRITE(6,*)'  No error found in XCARD'
      ELSE
        WRITE(6,*)'  Error(s) found in XCARD'
        WRITE(6,*)'    check your XCARD !!!'
        STOP
      END IF

      END SUBROUTINE CHKBLK
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Check whether the number of the dichroic conditions of an operator 
C is consistant with that in a variable NDCR.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE NSUBS(NSB,NOP,IDCRNMB,NDCR,CDCR,NHOP,NHM,H,IHP1,IHP2)
      IMPLICIT NONE

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER NDCRM
      PARAMETER(NDCRM=1200)

C Dummy arguments
      INTEGER NSB,NOP,IDCRNMB

      INTEGER NDCR(2)
      CHARACTER*(8) CDCR(NDCRM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

C Local variable
      INTEGER ICH,I,NBLK
C ----------

      NSB=0
      ICH=NHOP(NOP)
C ---  IHP1(ICH+2) = ( IO*100 + IEX )*100 + ISUB 
      DO WHILE( ICH .LT. NHOP(NOP+1) )
        NSB= MAX( NSB, MOD( IHP1(ICH+2), 100 ) + 1 )
        ICH=ICH + IHP1(ICH+5)/2048  ! ICH=ICH+NBLK
      END DO

      IF (NSB.EQ.1.AND.NDCR(IDCRNMB).EQ.0) THEN
        NDCR(IDCRNMB)=1
        IF ( IDCRNMB.EQ.1 .AND. NDCR(2).NE.0) THEN
          DO I=NDCR(2),1,-1
            CDCR(I+1)=CDCR(I)
          END DO
          CDCR(1)='0'
        ELSE IF ( IDCRNMB.EQ.2 ) THEN
          CDCR(NDCR(1)+1)='0'
        ELSE
          CDCR(1)='0'
        END IF
      ELSE IF ( NSB .NE. NDCR(IDCRNMB) ) THEN
        WRITE(6,'(A,I3,A)')'Error! The operator #',NOP,' in your'
     & //' X-card must be a transition matrix but it is not!'
        STOP
      END IF

      END SUBROUTINE NSUBS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   remove the interaction blocks for an operator NOP
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RMBLK(NOP,NHOP,NHM,H,IHP1,IHP2)
      IMPLICIT NONE 

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummu arguments
      INTEGER NOP
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

C Local variables
      INTEGER I,K,N,ICH
C --------------

      N=NHOP(0)
      WRITE(6,*)'enter RMBLK'
      IF ( NOP.GT.N .OR. NOP.LT.0 ) THEN
        WRITE(6,*)'Error!! Operator number incorrect! NOP=',NOP
        STOP
      END IF
      IF (NHOP(N+1).NE.IHP1(1)) THEN
        WRITE(6,*)
     &  'The protected area for non-zero elements will be released.'
      END IF
      K=NHOP(NOP+1)-NHOP(NOP)
      DO ICH=NHOP(NOP+1)+1,NHOP(N+1)
        IHP1(ICH-K)=IHP1(ICH)
        IHP2(ICH-K)=IHP2(ICH)
        H   (ICH-K)=H   (ICH)
      END DO
      DO I=NOP+1,N+1
        NHOP(I)=NHOP(I)-K
      END DO
      IHP1(1)=NHOP(N+1)
      IHP2(1)=NHOP(N+1)
      WRITE(6,*)
     & 'The interaction blocks for the operator was removed.  NOP=',NOP
      WRITE(6,*)'Size of the removed area =>',K

      END SUBROUTINE RMBLK

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Evaluate the arguments of a command with one of three 
C   different kinds of forms:
C
C 1) Grobal parameter form
C     ={<arg.1>,<arg.2>,...,<arg.narg>};
C
C 2) The one-body interaction form
C   #<ist1><ici1> [#<ist2><ici2>] <io1> [<io2>] )
C                              ={<arg.1>,<arg.2>,...,<arg.narg>};
C
C 3) The two-body interaction form
C   #<ist1><ici1> [#<ist2><ici2>] <io1> [<io2>] <io3> [<io4>] )
C                              ={<arg.1>,<arg.2>,...,<arg.narg>};
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GTDAT(NBDY,NOBTX,AOBT,NSTM,NCIM,NCI,ACI,ASTS,NDEG
     &                ,INRM,IARG,NARG, ARG, ITYP, IUNIT, CNFCHK
     &                ,NCMD, ACMD, LINXCD)
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NARGM,     NLBLM
      PARAMETER(NARGM=2000,NLBLM=10)

      INTEGER ITXMAX
      PARAMETER(ITXMAX=128)

C Dummy arguments
      INTEGER NBDY

      INTEGER NOBTX
      CHARACTER*(NLBLM) AOBT(NOBTM)

      INTEGER NSTM, NCIM
      INTEGER NCI(0:NSTM)
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)

      CHARACTER*(NSTM2) ASTS

      INTEGER NDEG(NOBTM)

      INTEGER INRM

      INTEGER IARG(8)
      INTEGER NARG
      REAL*8  ARG(NARGM)

      INTEGER ITYP, IUNIT
      LOGICAL CNFCHK
      
      INTEGER NCMD 
      CHARACTER*(ITXMAX) ACMD
      INTEGER LINXCD

C Global variable
      INTEGER IERR
      COMMON /SYS/IERR
C Local variables
      INTEGER    NWRD 
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2) DLM
      CHARACTER*(32) ASYMB
      INTEGER LIN

      INTEGER N,I 
      INTEGER IOB(4)
      INTEGER IST1,IST2,ICI1,ICI2,IO,IOLBL

      LOGICAL FOUND
C ---------------
C
C   initialize
      IST1=0
      IST2=0
      ICI1=0
      ICI2=0
      NARG=0
      DO I=1,8
        IARG(I)=0
      END DO
      DO I=1,4
        IOB(I)=0
      END DO
      DLM='  '
      NWRD=0
C   evaluate
C   "#<ist1><ici1> #<ist2><ici2> <io1> <io2> <io3> <io4> )"
      N=0
      IF (NBDY .NE. 0) THEN 
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        IF (DLM .EQ. '# ' ) THEN 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
          IST1=INDEX(ASTS,AWRD(1:1))
          IF (NWRD.EQ.1) THEN
            AWRD(2:2)=' '
            NWRD=2
          END IF
          N=N+1
          IF (IST1.EQ.0) THEN
            WRITE(6,*)'Undefined state #'//AWRD(1:1)//' was found'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          ICI1=1
          FOUND=.FALSE.
          DO WHILE (ACI(IST1,ICI1).NE.'ZZZ')
            FOUND= AWRD(2:NWRD) .EQ. ACI(IST1,ICI1)
            IF (FOUND) EXIT
            ICI1=ICI1+1
          END DO
          IF (FOUND) THEN
            IF (ICI1.GT.NCI(IST1)) ICI1=-ICI1
          ELSE 
            ICI1=MIN(ICI1,NCIM-1)
            ACI(IST1,ICI1)=AWRD(2:NWRD) 
            ICI1=-ICI1
            IF (NWRD-1.GT.NLBLM) THEN
              WRITE(6,*)'Error! Too long configuration label #'
     &           //AWRD(1:NWRD)//' was found.'
              WRITE(6,'(A,I5)')
     &          'The parameter NLBLM must be enhanced to ',NWRD
              CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            END IF
          END IF
          IF ( CNFCHK .AND. ICI1.LT.0 ) THEN
            WRITE(6,*)'Error! Undefined configuration label #'
     &        //ASTS(IST1:IST1)//ACI(IST1,ABS(ICI1)) //'was found'
            CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
          END IF
          IF ( DLM .EQ. '# ') THEN

            N=N+1
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)

            IST2=INDEX(ASTS,AWRD(1:1))
            IF (NWRD.EQ.1) THEN
              AWRD(2:2)=' '
              NWRD=2
            END IF
            IF (IST2.EQ.0) THEN
              WRITE(6,*)'Undefined state #'//AWRD(1:1)//' was found'
              CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            END IF

            ICI2=1
            FOUND=.FALSE.
            DO WHILE (ACI(IST2,ICI2).NE.'ZZZ')
              FOUND= AWRD(2:NWRD) .EQ. ACI(IST2,ICI2)
              IF (FOUND) EXIT
              ICI2=ICI2+1
            END DO 
            IF (FOUND) THEN
              IF (ICI2.GT.NCI(IST2)) ICI2=-ICI2
            ELSE 
              ICI2=MIN(ICI2,NCIM-1)
              ACI(IST2,ICI2)=AWRD(2:NWRD) 
              ICI2=-ICI2
              IF (NWRD-1.GT.NLBLM) THEN
                WRITE(6,*)'Error! Too long configuration label #'
     &         //AWRD(1:NWRD)//' was found.'
                WRITE(6,'(A,I5)')
     &          'The parameter NLBLM must be enhanced to ',NWRD
                CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
              END IF
            END IF
            IF ( CNFCHK .AND. ICI2.LT.0 ) THEN
              WRITE(6,*)'Error! Undefined configuration label #'
     &          //ASTS(IST2:IST2)//ACI(IST2,ABS(ICI2)) //'was found'
              CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
            END IF
          ELSE
            IST2=IST1
            ICI2=ICI1
          END IF

          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)

C          WRITE(6,*)' IST1 IST2 ICI1 ICI2  ',IST1,IST2,ICI1,ICI2
        END IF
        IO=0
        DO !until ') ' appears
          CALL GTORB(IOLBL,2,NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP
     &              ,0   ,NOBTX,AOBT,NDEG,NCMD,ACMD,LINXCD)
          IO=IO+1
          IOB(IO)=IOLBL
          IF ( DLM .EQ. ') ' ) EXIT 
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        END DO
C
C  !! for ommited form
C  ... <io1> )        -- >  ... <io1> <io1> )
C  ... <io1> <io2> )  -- >  ... <io1> <io2> <io1> <io2> )
        IF (IO.EQ.NBDY) THEN
          DO I=1,NBDY
            IOB(I+NBDY)=IOB(I)
          END DO
        END IF
        IF (N.NE.0.AND.IO.NE.N*NBDY) THEN
          WRITE(6,*)'Error! Incorrect number of orbital labels '
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF
C  !!  order of obital arguments must be
C     io1<=io2 and io3<=io4 and io2<=io4
        INRM=0
        IF (NBDY.EQ.2) THEN
          IF (IOB(1).GT.IOB(2)) THEN
            I=IOB(1)
            IOB(1)=IOB(2)
            IOB(2)=I
            INRM=INRM+1
          END IF
          IF (IOB(3).GT.IOB(4)) THEN
            I=IOB(3)
            IOB(3)=IOB(4)
            IOB(4)=I
            INRM=MOD(INRM+1,2)
          END IF
        END IF
        IF (NBDY.EQ.1) THEN
          IOB(3)=IOB(1)
          IOB(4)=IOB(2)
        END IF
C        WRITE(6,*)'  IO1 IO2 IO3 IO4  ',IOB(1),IOB(2),IOB(3),IOB(4)

C   evaluate "={ <arg.1>, <arg.2>, ... , <arg.narg> }"

        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
        IF (DLM.NE.'= ') THEN
          WRITE(6,*)'Error! Wrong format. ' 
          WRITE(6,*)'"=" is expected instead of DLM='//DLM
          CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)
        END IF
      
      END IF ! NBDY .NE. 0
      CALL X_CODE(1,IUNIT,ASYMB,NARG,ITYP,ARG,NARGM,LIN,IERR)
      IF (IERR.EQ.1) CALL ERR_CMDLIN(NCMD,ACMD,LINXCD)

      IARG(1)=IST1
      IARG(2)=IST2
      IARG(3)=ICI1
      IARG(4)=ICI2
      DO I=1,4
        IARG(4+I)=IOB(I)
      END DO
C
C 1000 FORMAT(' ARG=',10(1X,F7.3))
C      IF (NWRD.GT.0) WRITE(6,*)NWRD,AWRD(1:NWRD)//' '//DLM(1:2),LINXCD
C      WRITE(6,*)'NARG=',NARG
C      WRITE(6,1000)(ARG(I),I=1,NARG)

      END SUBROUTINE GTDAT

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HBND(L1,L2,P,VR,VI,NBLK,RR,RI,ICPX)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX,  KMAX,       NBLKM
      PARAMETER(LMAX=8,KMAX=2*LMAX,NBLKM=200)
  
      REAL*8 PI
      PARAMETER(PI=3.141592653897932D0)

C Dummy arguments
      INTEGER L1,L2
      REAL*8 P(3)
      REAL*8 VR(LMAX),VI(LMAX)
      REAL*8 RR(NBLKM),RI(NBLKM)
      INTEGER ICPX

C Global variables
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)
      COMMON /ANGL/RC

C Local variables
      INTEGER I,N,M,K,IP,NBLK
      REAL*8 TR,TI,ALF,BTA,GAM,XA,YA,ZA
      REAL*8 VTR(LMAX),VTI(LMAX)
      REAL*8 UR(KMAX,KMAX),UI(KMAX,KMAX)
C---------

      DO I=1,NBLKM
        RR(I)=0D0
        RI(I)=0D0
      END DO
C
      N=MIN(L1,L2)
      DO M=0,MIN(L1,L2)
        DO K=ABS(L1-L2),L1+L2,2
          UR(M+1,(K-ABS(L1-L2))/2+1)=RC(K,L2,M,L1,M)
          UI(M+1,(K-ABS(L1-L2))/2+1)=RC(K,L2,M,L1,M)
        END DO
      END DO
      DO K=0,N
        VTR(K+1)=VR(K+1)
        VTI(K+1)=VI(K+1)
      END DO
      CALL LEQ(N+1,KMAX,UR,VTR)
      CALL LEQ(N+1,KMAX,UI,VTI)
C
      XA=P(1)
      YA=P(2)
      ZA=P(3)
      IF (DSQRT(XA**2+YA**2).LT.10D-12) THEN
        ALF=0D0
      ELSE
        ALF=DATAN2(YA,XA)
      END IF
      BTA=-DATAN2(ZA,DSQRT(XA**2+YA**2))+PI/2D0
      GAM=0D0
C
      IP=0
      DO K=ABS(L1-L2),L1+L2,2
        CALL ROTS(2*K+1,KMAX,UR,UI,ALF,BTA,GAM)
        TR=VTR((K-ABS(L1-L2))/2+1)
        TI=VTI((K-ABS(L1-L2))/2+1)
        IF (IP+2*K+1.GT.NBLKM) THEN
          WRITE(6,*)'An error in the command "Hyb".'
          WRITE(6,*)'Too many parameters A_{k,m} needed.'
          WRITE(6,*)
     &  'The parameter NBLKM must be enhanced more than ',IP+2*K+1
          STOP
        END IF
        DO I=1,2*K+1
          RR(IP+I)= UR(I,K+1)*TR-UI(I,K+1)*TI
          RI(IP+I)= UI(I,K+1)*TR+UR(I,K+1)*TI 
        END DO
        IP=IP+2*K+1
      END DO
      NBLK=IP
C
      ICPX=0
*VDIR NOVECTOR
*VCOL LOOP,SCALAR
      DO I=1,IP
C       WRITE(6,*)I,RR(I),RI(I)   
        IF (ABS(RR(I)).GT.1D-13) ICPX=2*(ICPX/2)+1
        IF (ABS(RI(I)).GT.1D-13) ICPX=2+MOD(ICPX,2)
      END DO
      NBLK=IP

      END SUBROUTINE HBND

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Rotational tramsformation of the spherical hamonic functions
C
C     R(alpha,beta,gamma) | l m'> = Sigma_m | l m > U( m, m' )
C
C   INPUTS:
C     ALF ,BTA ,GAM :Euler angles
C     N             :number of basis functions ( =2*l+1 ) 
C     LN            :size of array
C
C   OUTPUTS:
C    UR(LN,N)&UI(LN,N):real and imaginary parts of transformation matrix
C                       subscripts of array     1,   2, ....  N-1,N
C                   magnetic quantum number    -l,-l+1, ....  l-1,l
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ROTS(N,LN,UR,UI,ALF,BTA,GAM)
      IMPLICIT NONE 

C Parameter
      INTEGER   LMAX
      PARAMETER(LMAX=8)

C Dummy arguments
      INTEGER N,LN
      REAL*8  UR(LN,N),UI(LN,N)
      REAL*8  ALF,BTA,GAM

C Local variable
      INTEGER I,J,K
      REAL*8 TR,TI,R,C,S,RL
      REAL*8 T(0:2*LMAX)
      REAL*8 D(2*LMAX)
C------ 

      C=DCOS(-BTA*0.5D0)
      S=DSIN(-BTA*0.5D0)
      RL=DBLE(N-1)*0.5D0
      DO I=1,N
        DO J=0,N
          T(J)=0D0
        END DO
        T(1)=1D0
        R=1.0D0
        DO J=2,N-I+1
          DO K=N,1,-1
            T(K)=S*T(K-1)+C*T(K)
          END DO
          R=R*DBLE(J-1)
        END DO
       
        DO J=N-I+2,N
          DO K=N,1,-1
            T(K)=C*T(K-1)-S*T(K)
          END DO
          R=R*DBLE(N-J+1)
        END DO
        D(I)=DSQRT(R)
        TR=DCOS(-GAM*(DBLE(I-1)-RL))/D(I)
        TI=DSIN(-GAM*(DBLE(I-1)-RL))/D(I)
        DO J=1,N
          UR(J,I)=T(J)*TR
          UI(J,I)=T(J)*TI
        END DO
      END DO
      DO J=1,N
        TR=DCOS(-ALF*(DBLE(J-1)-RL))*D(J)
        TI=DSIN(-ALF*(DBLE(J-1)-RL))*D(J)
        DO I=1,N
          R=UR(J,I)
          UR(J,I)=UR(J,I)*TR-UI(J,I)*TI
          UI(J,I)=R*TI+UI(J,I)*TR
        END DO
      END DO

      END SUBROUTINE ROTS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    solve linear equation       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LEQ(N,NM,A,X)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N,NM
      REAL*8 A(NM,N)
      REAL*8 X(N)

C Local variables
      INTEGER I,J,K
      REAL*8 S,T
C ---------------

      DO J=1,N
        T=1D0/A(J,J)
        DO K=J+1,N
          A(J,K)=A(J,K)*T
        END DO
        X(J)=X(J)*T
        DO I=J+1,N
          T=A(I,J)
          DO K=J+1,N
            A(I,K)=A(I,K)-A(J,K)*T
          END DO
          X(I)=X(I)-X(J)*T
        END DO
      END DO
C
      DO I=N-1,1,-1
        S=0D0
        DO K=I+1,N
          S=S+A(I,K)*X(K)
        END DO
        X(I)=X(I)-S
      END DO
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     set up all coefficient c^k(l1,m1,l2,m2) up to k=lmax*2
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CCOF(RC)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX,KMAX
      PARAMETER(LMAX=8,KMAX=2*LMAX)

C Dummy arguments
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)

C Local variabes
      INTEGER K,L1,L2,M1,M2
      REAL*8 T,T1
C -----------------

      DO K=0,KMAX
        DO L1=0,LMAX
          DO L2=0,LMAX
            DO M1=-LMAX,LMAX
              DO M2=-LMAX,LMAX
                RC(K,L1,M1,L2,M2)=0D0
              END DO
            END DO
          END DO
        END DO
      END DO
C
      RC(0,0,0,0,0)=1D0
      RC(1,0, 0,1,-1)=-1D0/DSQRT(3D0)
      RC(1,0, 0,1, 0)= 1D0/DSQRT(3D0)
      RC(1,0, 0,1, 1)=-1D0/DSQRT(3D0)
      RC(1,1,-1,0, 0)= 1D0/DSQRT(3D0)
      RC(1,1, 0,0, 0)= 1D0/DSQRT(3D0)
      RC(1,1, 1,0, 0)= 1D0/DSQRT(3D0)
      RC(0,1,-1,1,-1)= 1D0
      RC(0,1, 0,1, 0)= 1D0
      RC(0,1, 1,1, 1)= 1D0
      RC(2,1,-1,1,-1)=-     1D0/5D0
      RC(2,1,-1,1, 0)= DSQRT(3D0)/5D0
      RC(2,1,-1,1, 1)=-DSQRT(6D0)/5D0
      RC(2,1, 0,1,-1)=-DSQRT(3D0)/5D0
      RC(2,1, 0,1, 0)=      2D0/5D0
      RC(2,1, 0,1, 1)=-DSQRT(3D0)/5D0
      RC(2,1, 1,1,-1)=-DSQRT(6D0)/5D0
      RC(2,1, 1,1, 0)= DSQRT(3D0)/5D0
      RC(2,1, 1,1, 1)=-     1D0/5D0
C
      DO L2=2,LMAX
      DO L1=0,L2
      DO K=L2-L1,L1+L2,2
      DO M1=-L1,L1
      DO M2=0,L2
        T1=DSQRT(DBLE((2*L2-1)*(2*L2+1))/DBLE((L2+M2-1)*(L2+M2)))
     &     /DBLE(2*K+1)
        T=0D0
        IF (K+1.LE.L1+L2-1) THEN
         T=DSQRT(DBLE((K+M1-M2+1)*(K+M1-M2+2)))*RC(K+1,L1,M1,L2-1,M2-1)
        END IF
        IF (K.GE.1) THEN
         T=T-DSQRT(DBLE((K-M1+M2)*(K-M1+M2-1)))*RC(K-1,L1,M1,L2-1,M2-1)
        END IF
        T=T*T1+DSQRT(DBLE((L2-M2)*(L2-M2-1)*(2*L2+1))
     &    /DBLE((L2+M2)*(L2+M2-1)*(2*L2-3)))*RC(K,L1,M1,L2-2,M2)
        RC(K,L1,M1,L2,M2)=T
        RC(K,L1,-M1,L2,-M2)=T
        IF (MOD(M1-M2,2).NE.0) T=-T
        RC(K,L2,M2,L1,M1)=T
        RC(K,L2,-M2,L1,-M1)=T
      END DO
      END DO
      END DO
      END DO
      END DO

      END SUBROUTINE CCOF

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     The spherical harmonic function    Y_{L,M}(THETA,PHI)
C       YLMR :real part of the value
C       YLMI:imaginary part of the value
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Y(L,M,THETA,PHI,YLMR,YLMI)
      IMPLICIT NONE 

C Parameter
      REAL*8 PI
      PARAMETER(PI=3.141592653897932D0)

C Dummy arguments
      INTEGER L,M
      REAL*8 THETA,PHI,YLMR,YLMI

C Local variables
      INTEGER I,K,MABS
      REAL*8 T,C,R,Z,PLM,PLMB
C ------------

      Z=COS(THETA)
      R=SQRT(ABS(1D0-Z**2))
      PLM=1D0
      MABS=ABS(M)
      DO I=1,2*MABS,2
        PLM=PLM*DBLE(I)*R
      END DO
      PLMB=0D0
      DO K=MABS+1,L
        T=PLMB
        PLMB=PLM
        PLM=(DBLE(2*K-1)*Z*PLM-DBLE(K+MABS-1)*T)/DBLE(K-MABS)
      END DO
C
      C=1D0
      DO I=L-MABS+1,L+MABS
        C=C*DBLE(I)
      END DO
      C=SQRT(DBLE(2*L+1)/(4D0*PI*C))
      IF (M.GT.0.AND.MOD(M,2).EQ.1) C=-C
      YLMR=C*PLM*COS(DBLE(M)*PHI)
      YLMI=C*PLM*SIN(DBLE(M)*PHI)

      END SUBROUTINE Y

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      Create all possible configulations with NE electrons 
C      for an NDEG-th degenerated orbital.
C      NCNF: number of configurations 
C      ICNF: the configurations 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CNFG(NDEG,NE,NCNF,ICNF)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX2,NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

C Dummy arguments
      INTEGER NDEG,NE,NCNF
      INTEGER ICNF(1:*)

C Local variables (static)
      INTEGER NCNFS(-1:NEMAX)
      INTEGER ICNFS(2**NEMAX)
      INTEGER IPLG
      SAVE NCNFS,ICNFS,IPLG
      DATA IPLG/0/

C Local variables (temporal)
      INTEGER I,K,N,IB

C Function
      INTEGER ICMB
C -------------

      IF ((NDEG.GT.NEMAX).OR.(NE.GT.NDEG)) THEN
        WRITE(6,*)'Error in "CNFG"'
        NCNF=0
        RETURN
      END IF
      IF (IPLG.EQ.0) THEN
        IPLG=-1
        NCNFS(-1)=0
        NCNFS(0)=0
        DO I=1,NEMAX
          NCNFS(I)=NCNFS(I-1)+ICMB(NEMAX,I-1) 
        END DO
        DO I=0,2**NEMAX-1
          N=0
          DO IB=0,NEMAX-1
            IF (IAND(2**IB,I).NE.0) N=N+1
          END DO
          NCNFS(N)=NCNFS(N)+1
          ICNFS(NCNFS(N))=I
        END DO
      END IF
      NCNF=ICMB(NDEG,NE)
      K=NCNFS(NE-1)
      DO I=1,NCNF
        ICNF(I)=ICNFS(I+K)
      END DO
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      generate bit functions "IBRN" and "ICNT"
C
C        "IBRN"   position of configuration
C        "ICNT"   number of electrons
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE BITFN(IBRN,ICNT)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX2,NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

C Dummy arguments
      INTEGER IBRN(0:2**NEMAX-1),ICNT(0:2**NEMAX-1)

C Local variables
C -- Note: number of the electronic configuration for f^7 state is 3432
      INTEGER I,K,NE,NCNF
      INTEGER ICNF(3432)
C ------------

      DO NE=0,NEMAX
        K=NE
        CALL CNFG(NEMAX,K,NCNF,ICNF)
        DO I=1,NCNF
          IBRN(ICNF(I))=I-1
          ICNT(ICNF(I))=NE
        END DO
      END DO

      END SUBROUTINE BITFN

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     The energy differences among all configurations are
C     estimated here.
C     U (coulomb repulsion energy) and DLT (chrage transfer energy)
C     are assumed to be global parameters (they does not depend
C     on the configurations).
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HELV(NST,NSTM,NCIM,NCI,NSFT,NOBT,NELC,U,NDLT,DLT
     &               ,NHDM,HD,ERED,EADJ,NMODE)
      IMPLICIT NONE 

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NDLTM,    NLBLM
      PARAMETER(NDLTM=200,NLBLM=10)

C Dummy arguments
      INTEGER NST
 
      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)

      REAL*8  U(NOBTM,NOBTM)
      INTEGER NDLT(0:NDLTM*10)
      REAL*8  DLT(NDLTM)

      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  ERED(NCIM)
      REAL*8  EADJ(NSTM*3+3)
  
      INTEGER NMODE

C Global variables
C      CHARACTER*(NSTM2) ASTS
C      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
C      CHARACTER*(NLBLM) AOBT(NOBTM)

C Local variables
      REAL*8  EAV(NCIM)
      REAL*8  EC(2)
      REAL*8  DLT1(NDLTM)
      REAL*8  EORB(NOBTM)
      INTEGER IORB(NOBTM)
      INTEGER N,I,J,K,IC,IS,IO,IO1,IO2,NEXL,NBDY
      INTEGER IOS,IOS1,IOS2,IOE,IOE1,IOE2,ICOUNT
      REAL*8  T,E0
      LOGICAL HIT
C   
      REAL*8 AO(NOBTM,NOBTM),UO(NOBTM,NOBTM)
      REAL*8 EO(NOBTM),BO(NOBTM)
C-------------

      WRITE(6,*)'enter HELV'
      NEXL=MOD(NMODE/10,10)

      IF (NEXL.EQ.0) THEN
        DO IC=1,NCI(NST)
          T=0D0
          DO I=NSFT(IC,NST)+1,NSFT(IC+1,NST)
            T=T+HD(I)
          END DO
          T=T/DBLE(NSFT(IC+1,NST)-NSFT(IC,NST))-ERED(IC)
          DO I=NSFT(IC,NST)+1,NSFT(IC+1,NST)
            HD(I)=HD(I)-T
          END DO
        END DO
      END IF
C
      IF (NDLT(0).GE.NOBT) THEN
        WRITE(6,*)'ERROR! Too many DLT commands in your'
     &     //' x-card. They must be less than ',NOBT
        STOP
      END IF
C
      DO I=1,NDLT(0)

        IS=NDLT((I-1)*10+3)
        EC(1)=0D0
        EC(2)=0D0
        DLT1(I)=0D0
        DO IO=1,NOBT
          UO(I,IO)=0D0
        END DO

        DO K=1,2
          IC=NDLT((I-1)*10+4+K)
          IF (IC.LE.0) CYCLE 
          DO IO1=1,NOBT
            DO IO2=IO1+1,NOBT
              IF (NELC(IO1,IC,IS).GE.0.AND.NELC(IO2,IC,IS).GE.0) THEN
                EC(K)=EC(K)
     &            +U(IO1,IO2)*DBLE(NELC(IO1,IC,IS)*NELC(IO2,IC,IS))
              END IF
            END DO
          END DO
          DO IO=1,NOBT
            N=NELC(IO,IC,IS)
            IF (N.GE.0) THEN
              EC(K)=EC(K)+U(IO,IO)*DBLE((N-1)*N)*0.5D0
            END IF
          END DO
        END DO
        DLT1(I)=DLT(I)+EC(1)-EC(2)

        NBDY=NDLT((I-1)*10+2)
        IF (NBDY.EQ.1) THEN

          IOS=NDLT((I-1)*10+7)
          IOE=NDLT((I-1)*10+8)
          UO(I,IOS)=UO(I,IOS)-1D0
          UO(I,IOE)=UO(I,IOE)+1D0

        ELSE IF (NBDY.EQ.2) THEN

          IOS1=NDLT((I-1)*10+ 7)
          IOS2=NDLT((I-1)*10+ 8)
          IOE1=NDLT((I-1)*10+ 9)
          IOE2=NDLT((I-1)*10+10)
          UO(I,IOS1)=UO(I,IOS1)-1D0
          UO(I,IOS2)=UO(I,IOS2)-1D0
          UO(I,IOE1)=UO(I,IOE1)+1D0
          UO(I,IOE2)=UO(I,IOE2)+1D0

        ELSE

          WRITE(6,*)'unexpected NBDY=',NBDY
          STOP

        END IF

      END DO
  
      DO IO1=1,NOBT
        DO IO2=IO1,NOBT
          T=0D0
          DO I=1,NDLT(0) 
            T=T+UO(I,IO1)*UO(I,IO2)
          END DO
          AO(IO1,IO2)=T
          AO(IO2,IO1)=T
        END DO
        T=0D0
        DO I=1,NDLT(0) 
          T=T+UO(I,IO1)*DLT1(I)
        END DO
        BO(IO1)=T
      END DO

      CALL JCOB(NOBT,NOBTM,AO,EO,UO)
      DO IO=1,NOBT 
        WRITE(6,*)'IO,EO=',IO,EO(IO)
      END DO

      N=0 
      DO IO=1,NOBT 
        IF ( ABS(EO(IO)).GT.1D-10 ) THEN
          N=N+1  
        END IF
      END DO
      IF ( N.LT.NDLT(0) ) THEN
        WRITE(6,*)'ERROR! Duplicated or wrong definitions of'
     & // ' the orbital energies in DLT or DLT2 commands.'
        STOP
      END IF

      DO IO=1,NOBT
        IORB(IO)=0
        EORB(IO)=0D0
      END DO
      DO IO=1,NOBT
        IF (ABS(EO(IO)).GT.1D-10) THEN
          T = 0D0
          DO IO1=1,NOBT
            T = T + UO(IO1,IO)*BO(IO1)
          END DO
          T = T/EO(IO)
          DO IO1=1,NOBT
            EORB(IO1)=EORB(IO1) + T*UO(IO1,IO)
          END DO
          DO IO1=1,NOBT
            IF (ABS(UO(IO1,IO)).GT.1D-7) THEN
              IORB(IO1)=1
            END IF
          END DO 
        END IF
      END DO

      DO IO=1,NOBT
        IF (IORB(IO).EQ.0) THEN 
          N=NELC(IO,1,NST)
          DO IC=1,NCI(NST)
            IF ( N .NE. NELC(IO,IC,NST) ) THEN
         WRITE(6,'(A,I5,A)')'ERROR! The energy of an orbital #',IO,
     & ' must be defined using the DLT or DLT2 command.'
              STOP
            END IF
          END DO
        END IF
      END DO
              
        
C the one-body energy adjustment (This process only necessary for
C inter cluster charge transfer.)
C EADJ(2): the orbital, EADJ(3): the energy 
      IF (NINT(EADJ(2)).NE.0) THEN
        T=EADJ(3)-EORB(NINT(EADJ(2)))
        DO IO=1,NOBT
          EORB(IO)=EORB(IO)+T
        END DO
      END IF
C
C The average energy of each configuration
      CALL CLRV(NCI(NST),EAV)
      DO IO=1,NOBT
        IF (NELC(IO,1,NST).GE.0) THEN
          DO IC=1,NCI(NST)
            N=NELC(IO,IC,NST) 
            EAV(IC)=EAV(IC)+EORB(IO)*DBLE(N)
     &             +U(IO,IO)*DBLE((N-1)*N)*0.5D0
          END DO
        END IF
      END DO
C
      DO IO1=1,NOBT
        DO IO2=IO1+1,NOBT
          IF (NELC(IO1,1,NST).GE.0 .AND. NELC(IO2,1,NST).GE.0) THEN
            DO IC=1,NCI(NST)
              EAV(IC)=EAV(IC)
     &          +U(IO1,IO2)*DBLE(NELC(IO1,IC,NST)*NELC(IO2,IC,NST))
            END DO
          END IF
        END DO
      END DO
C
C the zero-energy adjustment
      IF (NINT(EADJ(1)).EQ.NCI(0) .AND. NINT(EADJ(2)).EQ.0 ) THEN
        DO I=1,NINT( EADJ(1) )
          IS=  NINT( EADJ((I-1)*3+4) )
          HIT= NST.EQ.IS
          IF (HIT) EXIT 
        END DO
        IF (HIT) THEN
          IC=NINT( EADJ((I-1)*3+5) ) 
          E0=      EADJ((I-1)*3+6)
        ELSE
          WRITE(6,*)'ERROR! The adjustment energy did not defined'
     &            //' for a state: State #=',NST
          STOP
        END IF
      ELSE IF ( NINT( EADJ(1) ) .EQ. 0 ) THEN
        IS=NST
        IC=1
        E0=0D0
      ELSE IF ( NINT(EADJ(1)).EQ.1 .AND. NINT(EADJ(2)).NE.0 ) THEN
        IS=NINT( EADJ(4) )
        IC=NINT( EADJ(5) )
        E0=      EADJ(6)
      ELSE
        WRITE(6,*)'Error! A wrong usage for the Eadj or the AdjOrb'
     &          //' commands.  in HELV'
        STOP
      END IF
C
      T=0D0
      DO IO1=1,NOBT
        DO IO2=IO1+1,NOBT
          IF (NELC(IO1,IC,IS).GE.0.AND.NELC(IO2,IC,IS).GE.0) THEN
            T=T+U(IO1,IO2)*DBLE(NELC(IO1,IC,IS)*NELC(IO2,IC,IS))
          END IF
        END DO
      END DO
      DO IO=1,NOBT
        N=NELC(IO,IC,IS)
        IF (N.GE.0) THEN
          T=T+EORB(IO)*DBLE(N)+U(IO,IO)*DBLE((N-1)*N)*0.5D0
        END IF
      END DO
C
      T=E0-T
      DO IC=1,NCI(NST)
        EAV(IC)=EAV(IC)+T
      END DO
C
C add the average energies of the configulations
C to the diagonal elements of the matrix. 
      IF (NEXL.EQ.0) THEN 
        DO IC=1,NCI(NST)
          T=0D0
          DO I=NSFT(IC,NST)+1,NSFT(IC+1,NST)
            HD(I)=HD(I)+EAV(IC)
            T=T+HD(I)
          END DO
C          WRITE(6,*)IC,T/(NSFT(IC+1,NST)-NSFT(IC,NST))
        END DO
      ELSE
        DO IC=1,NCI(NST)
          HD(IC)=EAV(IC)
        END DO
      END IF
C      DO IC=1,NCI(NST)
C        WRITE(6,*) '    CI=#'//ASTS(NST:NST)//ACI(NST,IC)//'  '
C     &     ,EAV(IC),EAV(IC)+ERED(IC)
C      END DO
C
      WRITE(6,*)'EAV='
      WRITE(6,'(5E15.7/)')(EAV(IC),IC=1,NCI(NST))

      END SUBROUTINE HELV
C
C  Diagonarization of a real symmetric matrix 
C  by means of the Jcob's method
C  (This routine is invoked by the 
C  subroutine HELV)
C
      SUBROUTINE JCOB(N,NMAX,A,E,UN)
      IMPLICIT NONE
C Parameters
      INTEGER ITMAX
      PARAMETER(ITMAX=10000)
      REAL*8 EPS
      PARAMETER(EPS=1.0D-15)
C Dummy arguments
      INTEGER N,NMAX
      REAL*8 A(NMAX,N),UN(NMAX,N)
      REAL*8 E(N)
C Local variables
      REAL*8 C,S,T,U
      INTEGER I,J,K,NCNT,NITA
C------------------
      DO I=1,N
        DO J=1,N
          UN(I,J)=0D0
        END DO
      END DO
      DO I=1,N
        UN(I,I)=1D0
      END DO
C
      NCNT=1
      NITA=0
      DO WHILE(NCNT.GT.0.AND.NITA.LT.ITMAX)
        NCNT=0
        DO I=1,N
          DO J=I+1,N
            IF (ABS(A(I,J)).GT.EPS) THEN
              NCNT=NCNT+1
              U=(A(J,J)-A(I,I))/(2D0*A(I,J))
              U=U+SIGN(1D0,U)*SQRT(1D0+U*U)
              S=1D0/SQRT(1D0+U*U)
              C=U*S
C
              DO K=1,N
                T=A(K,I)
                U=A(K,J)
                A(K,I)= C*T-S*U
                A(K,J)= S*T+C*U
              END DO

              DO K=1,N
                T=A(I,K)
                U=A(J,K)
                A(I,K)= C*T-S*U
                A(J,K)= S*T+C*U
              END DO
C
              DO K=1,N
                T=UN(K,I)
                U=UN(K,J)
                UN(K,I)= C*T-S*U
                UN(K,J)= S*T+C*U
              END DO
            END IF
          END DO
        END DO
        NITA=NITA+1
      END DO !WHILE
      IF (NITA.EQ.ITMAX) THEN
        WRITE(6,*)'DID NOT CONVERGE'
      END IF
C
      DO I=1,N
        E(I)=A(I,I)
      END DO
C
      END


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Estimate left top position of each configuration block IC
C     in the Hamiltonian for a state IST and save them in NSFT(IC,IST).
C     Note: value of NSFT(NCI(IST)+1,IST) equal to size of
C     the whole matrix size of the Hamiltonian for the state IST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SFTS(NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

C Dummy arguments
      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NMODE

C Local variables
      INTEGER NCPX,IST,ICI,IO,K

C Function
      INTEGER IBSIZE
C ----------------

      NCPX=MOD(NMODE,10)
      DO IST=1,NCI(0)
        NSFT(1,IST)=0
        DO ICI=1,NCI(IST)
          K=1
          DO IO=1,NOBT
            K=K*IBSIZE(IO,ICI,IST,NSTM,NCIM,NOBT,NELC,NDEG)
          END DO
          IF (NCPX.NE.0) K=K*2
          NSFT(ICI+1,IST)=NSFT(ICI,IST)+K
        END DO
      END DO

      END SUBROUTINE SFTS

C+++++++++++++++++++++++++++++++++++
C     combination  _nC_m = N!/(M!(N-M)!)
C+++++++++++++++++++++++++++++++++++
      FUNCTION ICMB(N,M)
      IMPLICIT NONE 
      INTEGER  ICMB

C Dummy arguments
      INTEGER N,M

C Local variables
      INTEGER I,K
      REAL*8 T

      IF ((N.LT.M).OR.(M.LT.0)) THEN
        WRITE(6,*)'wrong usage in "ICMB"'
        ICMB=1
        RETURN
      END IF
      T=1D0
      K=MIN(N-M,M)
      DO I=1,K
        T=T*DBLE(N-K+I)/DBLE(I)
      END DO
      ICMB=NINT(T)
      IF (ICMB.LT.1) THEN
        WRITE(6,*)'Error! too large number for combination' 
     &   //' _nC_m. N,M=',N,M
        STOP
      END IF

      END FUNCTION ICMB

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     The number of basis functions for the orbital IO
C     of a configuration IC in a state IST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION IBSIZE(IO,ICI,IST,NSTM,NCIM,NOBT,NELC,NDEG)
      IMPLICIT NONE 
      INTEGER  IBSIZE

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER NELCBS
      PARAMETER(NELCBS=10000)

C Dummy arguments
      INTEGER IO, ICI, IST
      INTEGER NSTM,NCIM
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

C Function 
      INTEGER ICMB

      IF (NDEG(IO).LT.0) THEN
        IBSIZE=NINT(DBLE(-NELC(IO,ICI,IST))/DBLE(NELCBS))
      ELSE
        IBSIZE=ICMB(NDEG(IO),NELC(IO,ICI,IST))
      END IF

      END FUNCTION IBSIZE

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     The number of electron on the orbital IO
C     of a configuration IC in a state IST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION NNELC(IO,ICI,IST,NSTM,NCIM,NOBT,NELC)
      IMPLICIT NONE
      INTEGER  NNELC

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NELCBS
      PARAMETER(NELCBS=10000)

C Dummy arguments
      INTEGER IO, ICI, IST

      INTEGER NSTM, NCIM, NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)

C Local variable
      INTEGER I
C -------------

      IF (IO.GT.NOBT) THEN
        NNELC=0
        RETURN
      ELSE IF (NELC(IO,ICI,IST).LT.-NELCBS/2) THEN
        I=NINT(DBLE(-NELC(IO,ICI,IST))/DBLE(NELCBS))
        NNELC=NELC(IO,ICI,IST)+I*NELCBS
      ELSE
        NNELC=NELC(IO,ICI,IST)
      END IF

      END FUNCTION NNELC
      
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Generate non-zero matrix elements of operators
C    and the elements are stored in the HD, H, IHP1 and IHP2.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HGEN(NBSM,T1,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      IMPLICIT NONE
C
C Parameters
      INTEGER LMAX2,NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBLKM
      PARAMETER(NBLKM=200)

      INTEGER   INTMAX,NLPM
      PARAMETER(INTMAX=10000,NLPM=5)

      INTEGER   NRWM
      PARAMETER(NRWM=10000)

      REAL*8 EPS
      PARAMETER(EPS=1D-12)
C
C Dummy arguments
      INTEGER NBSM
      REAL*8  T1(NBSM)
C - Operator number
      INTEGER NOP
C - Matrix elements
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NSTM, NCIM
      REAL*8  ERED(NCIM)

      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)
     
      INTEGER NMODE
C
C Global variables
      INTEGER IBRN(0:2**NEMAX-1),ICNT(0:2**NEMAX-1)
      COMMON /BIT/IBRN,ICNT
      INTEGER IERR
      COMMON /SYS/IERR
      INTEGER NBMPK
      INTEGER NBWRK(NHOPM)
      COMMON /NBM/NBMPK,NBWRK
C
C Local variables
C -- Note: number of the electronic configuration for f^7 state is 3432
      REAL*8  RW(NRWM)
      INTEGER IOB(5)
      INTEGER IOS(2*NOBTM)

      INTEGER IP1S(0:6),IP2S(0:6),ISGS(0:6),ICFS(0:6)
      INTEGER NBAS1(2*NOBTM),NBAS2(2*NOBTM)
      INTEGER ICNF(4*3432)
      INTEGER NCNF(0:2*NOBTM)
      INTEGER NCNT(0:NLPM),NSTP1(0:NLPM),NSTP2(0:NLPM)

      REAL*8  HIN(INTMAX)
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)
      LOGICAL HMLTN

      INTEGER I,J,K,N,N1,N2,ICH,IO,ISG,NE1,NE2,ISTS,IFL
      INTEGER NCPX,ICPX,NEXL,ISP,NOS,NLP,NBLK
      INTEGER NTYP,NBDY,LIN,IST1,IST2,ICI1,ICI2,IBS
      INTEGER NPTR,NHIN,NDIN,NNIN,IHIN,NNZ,INZ
      INTEGER IHP,IHPB,IHPBD
      REAL*8 S
      INTEGER NLP1, NLP2, NLP3
      REAL*8 ELLP1,ELLP2,ELLP3,AVLI1,AVLI2,AVLI3,AVLO1,AVLO2
C
C Function
      INTEGER NNELC, IBSIZE
C ------------
C
      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C
      NLP1=0
      NLP2=0
      NLP3=0
      ELLP1=0D0
      ELLP2=0D0
      ELLP3=0D0
      AVLI1=0D0
      AVLI2=0D0
      AVLI3=0D0
      AVLO1=0D0
      AVLO2=0D0
C
      WRITE(6,*)'enter HGEN NOP=',NOP,' NCPX=',NCPX,' NEXL=',NEXL
      IF ( NHOP(NOP) .NE. NHOP(NOP+1) ) THEN
        ICH=NHOP(NOP)
        IST1  =MOD(IHP1(ICH+6),NSTM) + 1
        IST2  =    IHP1(ICH+6)/NSTM  + 1
        IF ( NOP.LE.NCI(0) .AND. IST1.EQ.NOP .AND. IST2.EQ.NOP ) THEN
          CALL CLRV(NCI(NOP),ERED) 
          HMLTN=.TRUE.
        ELSE
          HMLTN=.FALSE.
        END IF
        IF ( NEXL.EQ.0 .AND. HMLTN ) THEN
          N=NSFT(NCI(NOP)+1,NOP)
          IF ( N .GT. NHDM ) THEN
            WRITE(6,'(A,I10,A)')
     &  'Error! NHDM must be enhanced more than ',N,' or use EXL mode'
            WRITE(6,*)
     &  ' if you want to save memory (NHDM value equal to NCIM).'
            STOP
          END IF
          CALL CLRV(N,HD)
        END IF
      ELSE
        WRITE(6,*)' Warning!  Null matrix'
        RETURN
      END IF

C  !! IHP1(1)  pointer to last element of H,IHP1,IHP2
C --At NHOP(NOP), number of matrix-elements segments is kept in H 
C   and pointer to the first segment is stored in IHP1 and IHP2 
      IHP=IHP1(1)
      ICH=NHOP(NOP)
      DO WHILE( ICH .LT. NHOP(NOP+1) )
C
C get a interaction block
C  
        IOB(5)=IHP1(ICH+2)   ! ( IO*100 + IEX )*100 + ISUB 
C --- IHP1(ICH+5)=((NBLK*256+NTYP)*4+ICPX)*2+NBDY-1
        I     =        IHP1(ICH+5)
        NBDY  =MOD(I,2)+1
        I=I/2
        ICPX  =MOD(I,4)
        I=I/4
        NTYP  =MOD(I,256)
        NBLK  =I/256

        IST1  =MOD(IHP1(ICH+6),NSTM) + 1
        IST2  =    IHP1(ICH+6)/NSTM  + 1 
        LIN   =    IHP2(ICH+6)
        ICI1  =    IHP1(ICH+7)
        ICI2  =    IHP2(ICH+7)
        IOB(1)=MOD(IHP1(ICH+8),NOBTM) + 1
        IOB(2)=    IHP1(ICH+8)/NOBTM  + 1
        IOB(3)=MOD(IHP2(ICH+8),NOBTM) + 1
        IOB(4)=    IHP2(ICH+8)/NOBTM  + 1

C        WRITE(6,*)'LIN=',LIN,' NBDY=',NBDY,' NTYP=',NTYP,' ICPX=',ICPX
C        WRITE(6,*)'NBLK=',NBLK,' ICH=',ICH
C        WRITE(6,*)'IST1,IST2=',IST1,IST2
C        WRITE(6,*)'ICI1,ICI2=',ICI1,ICI2

        IF ( IST1.GT.NCI(0) .OR. IST2.GT.NCI(0)
     &   .OR. IST1.LE.0 .OR. IST2.LE.0 ) THEN 
          WRITE(6,*)'<break point> wrong state lebel in HGEN' 
          WRITE(6,*)'IST1,IST2=',IST1,IST2
          STOP
        ELSE IF ( ICI1.GT.NCI(IST1) .OR. ICI2.GT.NCI(IST2)
     &   .OR. ICI1.LE.0 .OR. ICI2.LE.0 ) THEN 
          WRITE(6,*)'<break point> wrong configuration lebel in HGEN' 
          WRITE(6,*)'ICI1,ICI2=',ICI1,ICI2
          STOP
        END IF

        DO I=1,NBLK
          RW(I)=H(ICH+I)
        END DO

C find interacting orbitals
        NOS=0
        NCNF(0)=0
        N1=1
        N2=1
*POPTION NOPARALLEL
        DO IO=1,NOBT
          K=0
*VDIR NOVECTOR
*VOCL LOOP,SCALAR
*VOPTION NOVEC
          DO I=1,NBDY*2
            IF (IOB(I).EQ.IO) THEN
              IOS(I)=NOS+1
              K=K+1
            END IF
          END DO !I
          NE1=NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
          NE2=NNELC(IO,ICI2,IST2,NSTM,NCIM,NOBT,NELC)
          IF (K.NE.0) THEN
            NOS=NOS+1
            N=NCNF(NOS-1)+1
            IF (NDEG(IO).GT.0) THEN
              CALL CNFG(NDEG(IO),NE1,K,ICNF(N))
            ELSE
              K=IBSIZE(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC,NDEG)
            END IF
            NCNF(NOS)=NCNF(NOS-1)+K
            NBAS1(NOS)=N1
            NBAS2(NOS)=N2
C            WRITE(6,*)'NBAS1&2=',NBAS1(NOS),NBAS2(NOS)
          END IF
          N1=N1*IBSIZE(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC,NDEG)
          N2=N2*IBSIZE(IO,ICI2,IST2,NSTM,NCIM,NOBT,NELC,NDEG)
        END DO !IO

        IF (IOB(NBDY*2).GT.NOBT) THEN
          IOS(NBDY*2)=NOS+1
        ELSE IF (IOB(NBDY).GT.NOBT) THEN
          IOS(NBDY  )=NOS+1
        END IF
C estimation of direct production loop parameters
C       WRITE(6,*)(ICNF(I),I=NCNF(0)+1,NCNF(NOS))
C        WRITE(6,*)'NOS=',NOS
        ISTS=1
        NLP=0
        N1=1
        N2=1
        DO IO=1,NOBT
          IFL=0
*VOPTION NOVEC
          DO J=1,NBDY*2
            IF (IOB(J).EQ.IO) IFL=2
          END DO
          IF ((ISTS+IFL).EQ.1) THEN
            NLP=NLP+1
            IF (NLP.GT.5) THEN
              WRITE(6,*)'<break point> Number of the direct production'
              WRITE(6,*)' loops exceed the limit (max. 5).  '
              STOP
            END IF
            NSTP1(NLP)=N1
            NSTP2(NLP)=N2
            ISTS=0
          ELSE IF ((ISTS+IFL).EQ.2) THEN
            NCNT(NLP)=N1/NSTP1(NLP)
            ISTS=1
            IF (NCNT(NLP).EQ.1) NLP=NLP-1
          END IF
          N1=N1*IBSIZE(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC,NDEG)
          N2=N2*IBSIZE(IO,ICI2,IST2,NSTM,NCIM,NOBT,NELC,NDEG)
        END DO
        IF (ISTS.EQ.0) THEN
          NCNT(NLP)=N1/NSTP1(NLP)
          IF (NCNT(NLP).EQ.1) NLP=NLP-1
        END IF
C
C shift value for complex matrix
C
        IF (NCPX.NE.0.AND.ICPX.EQ.0) THEN
          ICPX=1
        ELSE IF (NCPX.EQ.0.AND.ICPX.EQ.1) THEN
          ICPX=0
        ELSE IF (ICPX.GT.2) THEN
          WRITE(6,*)'<break point> irregular value for ICPX in HGEN'
          STOP
        END IF

C --Create the header of a segment of matrix elements 
C   corresponding to an interaction block
        IF (ICPX.EQ.1.AND.NLP.GE.1.AND.NSTP1(1)*NSTP2(1).EQ.1) THEN
          IHP1(ICH+3) =  0 
          IHP2(ICH+3) =  0 
        ELSE IF (ICPX.EQ.2) THEN
          IHP1(ICH+3) =  2 
          IHP2(ICH+3) =  3 
        ELSE
          IHP1(ICH+3) = ICPX 
          IHP2(ICH+3) = ICPX 
        END IF
        IF (NCPX.NE.0) THEN
          DO I=1,NOS
            NBAS1(I)=NBAS1(I)*2
            NBAS2(I)=NBAS2(I)*2
          END DO
          DO I=1,NLP
            NSTP1(I)=NSTP1(I)*2
            NSTP2(I)=NSTP2(I)*2
          END DO
          IF ( IHP1(ICH+3) .EQ. 0 ) THEN
            NCNT (1)=NCNT (1)*2
            NSTP1(1)=1
            NSTP2(1)=1
          END IF
        END IF
C
C store number of loops,shifts,steps&counts for direct production loops
C

        INZ=1
        NPTR=IHP+1
*VDIR NOVECTOR
*VOCL LOOP,SCALAR
*VOPTION NOVEC
C        WRITE(6,*)'NLP=',NLP 
        DO I=1,NLP
          INZ=INZ*NCNT(I)
          H   (NPTR+I)  =DBLE( NCNT(I) )
          IHP1(NPTR+I-1)=NSTP1(I)
          IHP2(NPTR+I-1)=NSTP2(I)
C        WRITE(6,*)'ILP NSTP1 NSTP2 NCNT= ',I,NSTP1(I),NSTP2(I),NCNT(I)
        END DO
        H(NPTR)= DBLE( NLP )
        IF ( NCPX.NE.0 .AND. IHP1(ICH+3).EQ.0 ) INZ=INZ/2
        NPTR=NPTR+NLP

        ISG=0
        IF (NBDY.EQ.1) THEN
*VOPTION NOVEC
          DO IO=MIN(IOB(1),IOB(2)),MAX(IOB(1),IOB(2))-1
            ISG=ISG+NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
          END DO
          IF (IOB(1).LT.IOB(2))ISG=ISG+1
        ELSE IF (NBDY.EQ.2) THEN
*VOPTION NOVEC
          DO IO=MIN(IOB(1),IOB(3)),MAX(IOB(1),IOB(3))-1
            ISG=ISG+NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
          END DO
*VOPTION NOVEC
          DO IO=MIN(IOB(2),IOB(4)),MAX(IOB(2),IOB(4))-1
            ISG=ISG+NNELC(IO,ICI1,IST1,NSTM,NCIM,NOBT,NELC)
          END DO
        END IF
C
        NDIN=0
        NNIN=0

C  creation of "bit operators"
C        WRITE(6,*)'NTYP',NTYP
        SELECT CASE(NTYP)
        CASE(1)
          CALL MLTP(NDEG,NOBT,IOB,IOS,RW,HIN,NHIN
     &             ,ICMP,ITST,IMSK,ITRN,HMLTN)
        CASE(2)
          CALL HYBR(NDEG,NOBT,IOB,IOS,NBLK,RW,HIN,NHIN
     &             ,ICMP,ITST,IMSK,ITRN,HMLTN)
        CASE(3)
          CALL SOIN(NDEG,IOB,IOS,RW,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
        CASE(4)
          CALL CCOR(NDEG,IOS,RW,HIN,NHIN,ICMP,ITST,IMSK,ITRN)
        CASE(11)
          CALL MFLD(NDEG,IOB,IOS,RW,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
        CASE DEFAULT 
          WRITE(6,*)'<break point> wrong type of operator in HGEN'
          WRITE(6,*)'  NTYP=',NTYP
          STOP
        END SELECT 
C
        IF (NOS.GT.0) THEN
          IP1S(NOS+1)=NSFT(ICI1,IST1)
          IP2S(NOS+1)=NSFT(ICI2,IST2)
          ISGS(NOS+1)=ISG
          IF( IST1.EQ.IST2 )THEN
            DO I=NSFT(ICI1,IST1)+1,NSFT(ICI1+1,IST1),NBAS1(1)
              T1(I)=0D0
            END DO
          END IF

          IHP=NPTR-1
C  Create  non-zero matrix elements and store them to H,IHP1&IHP2
          DO IHIN=1,NHIN
            IHPB=IHP
            IHP=IHP+1
            ISP=NOS
 200        ICFS(ISP)=NCNF(ISP-1)+1
 210        K=ICFS(ISP)-NCNF(ISP-1)-1
            I=ICNF(ICFS(ISP))
            J=4*(IHIN-1)+ISP
            IF (IAND(I,ICMP(J)).NE.ITST(J)) GOTO 220
            IP1S(ISP)=IP1S(ISP+1)+NBAS1(ISP)*K
            IP2S(ISP)=IP2S(ISP+1)+NBAS2(ISP)*IBRN(IEOR(I,ITRN(J)))
            ISGS(ISP)=ISGS(ISP+1)+ICNT(IAND(I,IMSK(J)))
            ISP=ISP-1
            IF (ISP.NE.0) GOTO 200
            IHP=IHP+1
            IF (IHP.LT.NHM) THEN
              IHP1(IHP)=IP1S(1)+1
              IHP2(IHP)=IP2S(1)+1
              H(IHP)=(-1)**ISGS(1)*HIN(IHIN)
            END IF
            ISP=ISP+1
 220        ICFS(ISP)=ICFS(ISP)+1
            IF (ICFS(ISP).LE.NCNF(ISP)) GOTO 210
            ISP=ISP+1
            IF (ISP.LE.NOS) GOTO  220
            IHP1(IHPB+1)=IHP-IHPB-1
            IHP2(IHPB+1)=IHP-IHPB-1
C            WRITE(6,*)'NDEC=',IHP-IHPB-1
            IF (IHP.EQ.IHPB+1) THEN
              IHP=IHPB
              NNIN=NNIN+1
            END IF
C            WRITE(6,*)'end HIN EX'
C  diagonal part of Hamiltonian
            K=4*(IHIN-1)
            IF ( IST1.EQ.IST2 .AND. ITRN(K+1)+ITRN(K+2)
     &         +ITRN(K+3)+ITRN(K+4).EQ.0 .AND. IHP.GT.IHPB ) THEN
C              WRITE(6,*)'DIAGONAL'
C When the operator is a Hamiltomian and the system is not
C in the EXL mode, the diagonal part of the matrix is saved
C in the array HD.
              NDIN=NDIN+1
              DO I=IHPB+2,IHP
                J=IHP1(I)
                T1(J)=T1(J)+H(I)
              END DO
              IHP=IHPB
            END IF
          END DO !IHIN
C
          NHIN=NHIN-NDIN-NNIN

          IF (NHIN.GT.0) THEN
C-----------------
C            WRITE(6,*)'Before OPTPAL or OPTSCR----'
C            WRITE(6,*)
C     &    '==== NTYP=',NTYP,' NPTR=',NPTR,' NHIN=',NHIN
C            DO I=1,5
C            WRITE(6,*)
C     & 'NCNT=',NCNT(I),' NSTP1=',NSTP1(i),' NSTP2=',NSTP2(I)
C            END DO
C            IBS=NPTR
C            DO J=1,NHIN
C              N=IHP1(IBS)
C              WRITE(6,*)'   NC=',N,' IHIN=',J
C              DO I=IBS+1,IBS+N
C                WRITE(6,'(2I8,F15.7)')IHP1(I),IHP2(I),H(I)
C              END DO
C              IBS=IBS+N+1
C            END DO
C-----------------
C>>>START SCALAR
            CALL OPTSCR(NHM,H,IHP1,IHP2,NPTR,NHIN,IHP)
C>>>END SCALAR
C-----------------
C            WRITE(6,*)' NTYP=',NTYP,' NPTR=',NPTR,' NHIN=',NHIN
C            WRITE(6,*)'After OPTPAL or OPTSCR----'
C            IBS=NPTR
C            DO J=1,NHIN
C              N=IHP1(IBS)
C              WRITE(6,*)'   N=',N,' I=',J
C              DO I=IBS+1,IBS+N
C                WRITE(6,*)IHP1(I),IHP2(I),H(I)
C              END DO
C              IBS=IBS+N+1
C            END DO
C-----------------
          END IF
C-----------------------------------------------------------------
C          IF (NHIN-NDIN-NNIN.GT.1) THEN
C            N=IHP1(NPTR)
C            IHP=NPTR+N+1
C            K=NHIN-NDIN-NNIN
C            DO J=1,NHIN-NDIN-NNIN-1
C              N  =IHP1(IHP)
C              IHP1(NPTR)=IHP1(NPTR)+N
C              IHP2(NPTR)=IHP2(NPTR)+N
C              DO I=IHP+1,IHP+N
C                IHP1(I-J)=IHP1(I)
C                IHP2(I-J)=IHP2(I)
C                H   (I-J)=H   (I)
C              END DO
C              IHP=IHP+N+1
C              NNIN=NNIN+1
C            END DO
C            IHP=IHP-K
C            WRITE(6,*)'Size N=',IHP1(NPTR)
C            WRITE(6,*)'  NNIN=',NNIN,' ',NHIN-NDIN-NNIN 
C            DO I=1,IHP1(NPTR)
C              J=NPTR+I 
C              WRITE(6,*)' I=',I,' IHP1=',IHP1(J),' IHP2=',IHP2(J)
C     &           ,' H=',H(J)
C            END DO
C          END IF
C-----------------------------------------------------------------
          IF ( NDIN.NE.0 ) THEN
            S=0D0
C Energy adjustment needed for a Hamiltonian
            IF (HMLTN) THEN 
              DO I=NSFT(ICI1,IST1)+1,NSFT(ICI1+1,IST1),NBAS1(1)
                S=S+T1(I)
              END DO
              S=S/DBLE(N1/INZ)
            END IF
            IHPBD=IHP
            IHP=IHP+1
            IF (NOS.EQ.1) THEN
              DO I=0,NCNF(1)-NCNF(0)-1
                J=NBAS1(1)*I+NSFT(ICI1,IST1)+1
                IF (ABS(T1(J)-S).GT.EPS) THEN
                  IHP=IHP+1
                  IHP1(IHP)=J
                  IHP2(IHP)=J
                   H  (IHP)=T1(J)-S
                END IF
C                WRITE(6,*)'IHP;T1-S=',IHP+I,T1(J)-S
              END DO
            ELSE IF (NOS.EQ.2) THEN
              DO J=0,NCNF(2)-NCNF(1)-1
                DO I=0,NCNF(1)-NCNF(0)-1
                  K=NBAS1(1)*I+NBAS1(2)*J+NSFT(ICI1,IST1)+1
                  IF (ABS(T1(K)-S).GT.EPS) THEN
                    IHP=IHP+1
                    IHP1(IHP)=K
                    IHP2(IHP)=K
                     H  (IHP)=T1(K)-S
                  END IF
                END DO
              END DO
            ELSE
              WRITE(6,*)'<break point> NOS >2 with EXL mode.'
              WRITE(6,*)'Too complicate interaction in HGEN'
              STOP
            END IF
            IF (IHP.GT.IHPBD+1) THEN
              IF ( NEXL.EQ.0 .AND. HMLTN ) THEN
                IHP1(IHPBD+1)=IHP-IHPBD-1
                IHP2(IHPBD+1)=0
                I=IHP1(ICH+3) !ICPX
                CALL HDIAG(NHM,H,IHP1,NHDM,HD,NLP,NSTP1,NCNT
     &                    ,IHPBD+1,I,NBAS1(NOS) )
                IHP=IHPBD
                NDIN=0
              ELSE
                IHP1(IHPBD+1)=IHP-IHPBD-1
                IHP2(IHPBD+1)=0
              END IF
            ELSE
              IHP=IHPBD
              NDIN=0
            END IF
          END IF
        END IF

        IHP1(ICH+4)=NHIN
        IHP2(ICH+4)=NHIN
        IF ( .NOT.( NEXL.EQ.0 .AND. HMLTN ) .AND. NDIN.NE.0 ) THEN
          IHP1(ICH+4)=IHP1(ICH+4)+1
          IF (.NOT.HMLTN) IHP2(ICH+4)=IHP2(ICH+4)+1
        END IF

        N=MAX( IHP1(ICH+4), IHP2(ICH+4) ) 
        S=DBLE(IHP-NPTR+1-N)*DBLE(INZ)
        IF (NSTP1(1)*NSTP2(1).EQ.1.AND.NLP.NE.0) THEN
          NLP1=NLP1+1
          ELLP1=ELLP1+S
          AVLI1=AVLI1+NCNT(1)
          AVLO1=AVLO1+(N*INZ)/NCNT(1)
        ELSE IF (NLP.NE.0) THEN
          NLP2=NLP2+1
          ELLP2=ELLP2+S
          AVLI2=AVLI2+NCNT(1)
          AVLO2=AVLO2+(N*INZ)/NCNT(1)
        ELSE
          NLP3 =NLP3+1
          ELLP3=ELLP3+S
          AVLI3=AVLI3+N
        END IF
C  --IHP1,2(ICH+4)=NHIN1,2
        IF ( IHP1(ICH+4).NE.0 .OR. IHP2(ICH+4).NE.0 ) THEN
          IHP1(ICH+1)=NPTR-NLP
          IHP2(ICH+1)=NPTR-NLP
          IF (NOS.GE.1) THEN
            I=MAX( NBAS1(NOS), NBAS2(NOS) )
C  -- 4*MAX(NBAS1,NBAS2)+ICPX
            IHP1(ICH+3)=IHP1(ICH+3)+4*I 
            IHP2(ICH+3)=IHP2(ICH+3)+4*I 
          END IF
        ELSE
          IHP=NPTR-NLP-1
        END IF
        ICH = ICH + NBLK
      END DO !ICH
C
      IF ( IHP1(1).EQ.IHP ) THEN
        WRITE(6,*)' Warning! Operator with no matrix element NOP=',NOP
      END IF

      CALL MATSTR(NOP,NLP1,ELLP1,AVLI1,AVLO1
     &               ,NLP2,ELLP2,AVLI2,AVLO2
     &               ,NLP3,ELLP3,AVLI3)
      NBWRK(NOP)=IHP-IHP1(1)
      NBMPK=MAX(IHP,NBMPK)

      IHP1(1)=IHP
      IHP2(1)=IHP


      IF (IHP.GE.NHM) THEN
        WRITE(6,*)'Error !!  Too many non-zero matrix elements.'
        WRITE(6,'(A,I10,A,I10)')
     &      'The value of the parameter NHM=',NHM,
     &      ' must be enhanced more than ',IHP
        WRITE(6,*)'The operator number =',NOP
        WRITE(6,*)'Total non-zero elements =',ELLP1+ELLP2+ELLP3
        STOP
      END IF
      WRITE(6,*)'The operator number =',NOP
      WRITE(6,'(A,I10,A,I10)')'The matrix elements storage: used=',
     &      IHP1(1),' maximum=',NHM
      WRITE(6,*)'Total non-zero elements =',ELLP1+ELLP2+ELLP3

      END SUBROUTINE HGEN

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Optimize structure of the matrix-elements blocks
C  for parallel processing machines
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPTPAL(NHM,H,IHP1,IHP2,NPTR,NHIN,IHP)
      IMPLICIT NONE

C Parameters
      INTEGER NPAL
      PARAMETER(NPAL=8)
      INTEGER NHM

C Dummy arguments
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NPTR,NHIN,IHP

C Local variables
      INTEGER I,J,N,I1,I2,N1,N2,IC1,IC2,IBS,IBS1,IBS2
      REAL*8 T 
      LOGICAL HIT
C
C----------------------------------------------------------------
C NPAL:     The number of processing units
C NPTR:     Porinter to the starting address of matrix elements 
C           storage for an interaction-block.
C           The matrix elements are further grouped into smaller
C           blocks called "non-collision-elements block";
C           All the indeces within a block are different,
C           which is necessary for vector or parallel processing
C           so to avoid simulteneous writing to any address.
C NHIN:     number of non-collision-elements blocks.
C IBS1(or IBS2): Base pointer for a non-collision-elements block
C           which shows the starting address of the block.
C           The number of the elements N1 in the block
C           is stored in IHP1(IBS1) and IHP2(IBS1). 
C           The matrix elements H(*) and their indecies 
C           IHP1(*) and IHP2(*) are saved at addresses
C           IBS1+1,IBS1+2,...,IBS1+N1.
C---------------------------------------------------------------
C Combine small element-blocks into a large one 
C
C      WRITE(6,*)' In optpal'
      N1  = IHP1(NPTR)
      IBS1= NPTR
      DO IC1=1,NHIN-1
        IC2 = IC1+1
        N2  = N1
        IBS2= IBS1
        DO WHILE ( N1.LT.NPAL .AND. N1.GE.1 .AND. IC2.LE.NHIN )
          IBS2=IBS2+N2+1
          N2=IHP1(IBS2)
          IF ( N2.LT.NPAL .AND. N2.GE.1 ) THEN
            DO I1=1,N1
              J=IBS1+N1
              HIT=.FALSE.
              DO I=IBS2+1,IBS2+N2
                HIT= IHP1(I).EQ.IHP1(J) .OR. IHP2(I).EQ.IHP2(J) 
                IF (HIT) EXIT
              END DO
C
              IF (HIT) THEN
                J=IBS1+N1
                I=IHP1(IBS1+1) 
                IHP1(IBS1+1)= IHP1(J)
                IHP1(J)     = I
                I=IHP2(IBS1+1)
                IHP2(IBS1+1)= IHP2(J)
                IHP2(J)     = I
                T=H(IBS1+1)
                H(IBS1+1)   = H(J)
                H(J)        = T
              ELSE
                IHP1(IBS2)=IHP1(IBS1+N1) 
                IHP2(IBS2)=IHP2(IBS1+N1) 
                H   (IBS2)=H   (IBS1+N1) 
C shift elements blocks between 
C those IBS1 and IBS2 to one lower address
                IBS=IBS1+N1+1
                N=IHP1(IBS)
                DO WHILE (IBS2.GT.IBS+N)
                  IHP1(IBS)=IHP1(IBS+N)
                  IHP2(IBS)=IHP2(IBS+N)
                  H   (IBS)=H   (IBS+N)
                  IHP1(IBS-1)=N
                  IHP2(IBS-1)=N
                  IBS=IBS+N+1
                  N=IHP1(IBS)
                END DO
                N1=N1-1
                IHP1(IBS1)=N1
                IHP2(IBS1)=N1
                N2=N2+1
                IBS2=IBS2-1
                IHP1(IBS2)=N2
                IHP2(IBS2)=N2
                IF (N2.EQ.NPAL) EXIT 
              END IF
            END DO
          END IF
C
          IC2=IC2+1
        END DO
        IBS1=IBS1+N1+1
        N1=IHP1(IBS1)
      END DO
      IHP=IBS1+N1
C-------------------------------------------------------
C Remove zero-size blocks
      IBS1=NPTR
      N =NHIN
      DO IC1=1,NHIN
        N1=IHP1(IBS1)
        IF (N1.EQ.0) THEN
C          WRITE(6,*)' Block removed!'
          N2=0
          IBS2=IBS1
          DO IC2=IC1+1,NHIN
            IBS2=IBS2+N2+1
            N2=IHP1(IBS2)
            IHP1(IBS2-1)=N2
            IHP2(IBS2-1)=N2
            IHP1(IBS2)=IHP1(IBS2+N2)
            IHP2(IBS2)=IHP2(IBS2+N2)
            H   (IBS2)=H   (IBS2+N2)
          END DO
          N=N-1
        ELSE
          IBS1=IBS1+N1+1
        END IF
      END DO
      NHIN=N
      IHP=IBS1-1
C------------------------------------------------------------------
C  Change the size of element blocks to make their sizes
C  being multiples of NPAL 
      IBS1=NPTR
      DO IC1=1,NHIN-1
        N1=IHP1(IBS1)
        IBS2=IBS1
        N2=N1
        DO IC2=IC1+1,NHIN
          IBS2=IBS2+N2+1
          N2=IHP1(IBS2)
              
          I1=MOD(N1,NPAL)
          I2=MOD(N2,NPAL)
          IF ( I1.EQ.0 .OR. I2.EQ.0 ) GOTO 220
          IF ( N1/NPAL.GT.100 .AND. N2/NPAL.GT.100 ) GOTO 220
          IF ( MIN(NPAL-I2,I1) .LE. MIN(NPAL-I1,I2) ) THEN
            I2=MIN(NPAL-I2,I1)
            I1=N1
            DO WHILE( I1.GE.1 .AND. I2.GE.1 ) 

              J=IBS1+N1
              HIT=.FALSE.
              DO I=IBS2+1,IBS2+N2
                HIT= IHP1(I).EQ.IHP1(J) .OR. IHP2(I).EQ.IHP2(J)
                IF (HIT) EXIT
              END DO
              IF (HIT) THEN
                J=IBS1+N1
                I=IHP1(IBS1+1) 
                IHP1(IBS1+1)= IHP1(J)
                IHP1(J)   = I
                I=IHP2(IBS1+1)
                IHP2(IBS1+1)= IHP2(J)
                IHP2(J)   = I
                T=H(IBS1+1)
                H(IBS1+1)   = H(J)
                H(J)      = T
              ELSE
                IHP1(IBS2)=IHP1(IBS1+N1) 
                IHP2(IBS2)=IHP2(IBS1+N1) 
                H   (IBS2)=H   (IBS1+N1) 
C shift one element in each block between IBS1 and IBS2
                IBS=IBS1+N1+1
                N=IHP1(IBS)
                DO WHILE (IBS2.GT.IBS+N)
                  IHP1(IBS)=IHP1(IBS+N)
                  IHP2(IBS)=IHP2(IBS+N)
                  H   (IBS)=H   (IBS+N)
                  IHP1(IBS-1)=N
                  IHP2(IBS-1)=N
                  IBS=IBS+N+1
                  N=IHP1(IBS)
                END DO
                N1=N1-1
                IHP1(IBS1)=N1
                IHP2(IBS1)=N1 
                N2=N2+1
                IBS2=IBS2-1
                IHP1(IBS2)=N2
                IHP2(IBS2)=N2
                I2=I2-1
              END IF
              I1=I1-1
            END DO
          ELSE
            I1=MIN(NPAL-I1,I2)
            I2=N2
            DO WHILE( I2.GE.1 .AND. I1.GE.1 ) 
              J=IBS2+1
              HIT=.FALSE.
              DO I=IBS1+1,IBS1+N1
                HIT= IHP1(I).EQ.IHP1(J) .OR. IHP2(I).EQ.IHP2(J)
                IF (HIT) EXIT
              END DO
              IF (HIT) THEN 
                J=IBS2+N2
                I=IHP1(IBS2+1) 
                IHP1(IBS2+1)= IHP1(J)
                IHP1(J)   = I
                I=IHP2(IBS2+1)
                IHP2(IBS2+1)= IHP2(J)
                IHP2(J)   = I
                T=H(IBS2+1)
                H(IBS2+1)   = H(J)
                H(J)      = T
              ELSE
                IBS=IBS1+N1+1
                N=IHP1(IBS)
                IHP1(IBS)=IHP1(IBS2+1) 
                IHP2(IBS)=IHP2(IBS2+1) 
                H   (IBS)=H   (IBS2+1) 
C shift one element in each block between IBS1 and IBS2
                DO WHILE (IBS2.GE.IBS+N+1)
                  J=IHP1(IBS+N+1)
                  IHP1(IBS+N+1)=IHP1(IBS+1)
                  IHP2(IBS+N+1)=IHP2(IBS+1)
                  H   (IBS+N+1)=H   (IBS+1)
                  IHP1(IBS+1)=N
                  IHP2(IBS+1)=N
                  IBS=IBS+N+1
                  N=J
                END DO
                N1=N1+1
                IHP1(IBS1)=N1
                IHP2(IBS1)=N1 
                N2=N2-1
                IBS2=IBS2+1
                IHP1(IBS2)=N2
                IHP2(IBS2)=N2
                I1=I1-1
              END IF
              I2=I2-1
            END DO
          END IF
 220      CONTINUE
        END DO
        IBS1=IBS1+N1+1
      END DO
C-------------------------------------------------------
C Remove zero-size blocks
      IBS1=NPTR
      N =NHIN
      DO IC1=1,NHIN
        N1=IHP1(IBS1)
        IF (N1.EQ.0) THEN
C          WRITE(6,*)' Block removed!'
          N2=0
          IBS2=IBS1
          DO IC2=IC1+1,NHIN
            IBS2=IBS2+N2+1
            N2=IHP1(IBS2)
            IHP1(IBS2-1)=N2
            IHP2(IBS2-1)=N2
            IHP1(IBS2)=IHP1(IBS2+N2)
            IHP2(IBS2)=IHP2(IBS2+N2)
            H   (IBS2)=H   (IBS2+N2)
          END DO
          N=N-1
        ELSE
          IBS1=IBS1+N1+1
        END IF
      END DO
      NHIN=N
      IHP=IBS1-1
C
      END SUBROUTINE OPTPAL

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Optimize structure of the matrix-elements blocks
C  for scalar machines
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPTSCR(NHM,H,IHP1,IHP2,NPTR,NHIN,IHP)
      IMPLICIT NONE

C Dummy arguments
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NPTR,NHIN,IHP

C Local variables
      INTEGER N,NC,IC,IPTR,IHIN
C----------------------------------------------------------------
C NPTR:     Pointer to the starting address of matrix elements 
C           storage for an interaction-block.
C           The matrix elements are further grouped into smaller
C           blocks called "non-collision-elements block";
C           All the indeces within a block are different,
C           which is necessary for vector or parallel processing
C           so to avoid simulteneous writing to any address.
C NHIN:     number of non-collision-elements blocks.
C---------------------------------------------------------------
C Combine non-collision-element-blocks into one 
C
      IF ( NHIN.GT.0 ) THEN
        NC=IHP1(NPTR)
        N=NC
        IPTR=NPTR+NC+1
C        WRITE(6,*)'NHIN=',NHIN
        DO IHIN=2,NHIN
          NC=IHP1(IPTR)
          N=N+NC
C          WRITE(6,*)'NC=',NC,' IHIN=',IHIN
          DO IC=1,NC
            IHP1(IPTR+IC-IHIN+1)=IHP1(IPTR+IC)
            IHP2(IPTR+IC-IHIN+1)=IHP2(IPTR+IC)
            H   (IPTR+IC-IHIN+1)=H   (IPTR+IC)
C            WRITE(6,'(2I6,F15.7)') IHP1(IPTR+IC-IHIN+1),
C     &                             IHP2(IPTR+IC-IHIN+1),
C     &                             H   (IPTR+IC-IHIN+1)
          END DO
          IPTR=IPTR+NC+1
        END DO
        IF (IHP1(NPTR).EQ.IHP2(NPTR)) THEN
          IHP1(NPTR)=N
          IHP2(NPTR)=N
        ELSE
          IHP1(NPTR)=N
        END IF
        IHP=IHP-(NHIN-1)
        NHIN=1
C        DO IC=1,N
C          WRITE(6,'(2I6,F15.7)') IHP1(NPTR+IC),
C     &                           IHP2(NPTR+IC),
C     &                           H   (NPTR+IC)
C        END DO
      END IF
C
      END SUBROUTINE OPTSCR

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Generate a set of bit operators for a multipole interation.
C     Called from the subroutine HGEN as NTYP=1 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MLTP(NDEG,NOBT,IOB,IOS,R
     &               ,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
      IMPLICIT NONE 
C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   INTMAX,      NBLKM
      PARAMETER(INTMAX=10000,NBLKM=200)
      INTEGER LMAX, KMAX  
      PARAMETER(LMAX=8,KMAX=LMAX*2)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER NOBT
      INTEGER IOB(5)
      INTEGER IOS(*)
      REAL*8 R(NBLKM)
      REAL*8 HIN(INTMAX)
      INTEGER NHIN
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)
      LOGICAL HMLTN
C Global variable 
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)
      COMMON /ANGL/RC
C Local variables 
      INTEGER IBMAX(4)
      REAL*8  RDR(0:KMAX),REX(0:KMAX)
      REAL*8  TR,TD,TEX
C  -loop param.
      INTEGER I,I1,I2,I3,I4,IBS1,IBS2,IBD1,IBD2
      INTEGER IBS2S,IBS2E,IBD2S,IBD2E
      INTEGER IO1,IO2,IO3,IO4,L1,L2,L3,L4,K,K1,K2,K3,K4 
      INTEGER IH,M1,M2,M3,M4,MS1,MS2,MS3,MS4
C
C      WRITE(6,*)'enter MLTP'
      DO I=1,4
        IBMAX(I)= NDEG( IOB(I) ) -1
      END DO
      DO K=0,KMAX
        RDR(K)=0
        REX(K)=0
      END DO
C
      IO1=IOB(1)
      IO2=IOB(2)
      IO3=IOB(3)
      IO4=IOB(4)
      L1=NDEG(IO1)/4
      L2=NDEG(IO2)/4
      L3=NDEG(IO3)/4
      L4=NDEG(IO4)/4
      K1=MAX(ABS(L1-L3),ABS(L2-L4))
      K2=MIN(L1+L3,L2+L4)
      K3=MAX(ABS(L1-L4),ABS(L2-L3))
      K4=MIN(L1+L4,L2+L3)

      IF ( IO1.EQ.IO3 .AND. IO2.EQ.IO4 ) THEN
        IF ( K1.EQ.0 ) K1=2 
        IF ( IO1.EQ.IO2 .AND. K3.EQ.0 ) K3=2
      END IF
      IF ( IO1.EQ.IO2 .AND. IO3.EQ.IO4 ) THEN
        I=0
      ELSE
        I=(K2-K1)/2+1
      END IF

C      WRITE(6,*)'L1-L4',K1,K2,K3,K4,'IO1-IO2',IO1,IO2,IO3,IO4
*POPTION NOPARALLEL
      DO K=K1,K2,2
        RDR(K)=R((K-K1)/2+1)
      END DO
      DO K=K3,K4,2
        REX(K)=R((K-K3)/2+I+1)
      END DO

      DO I=1,INTMAX*4
        ICMP(I)=0
        ITST(I)=0
        IMSK(I)=0
        ITRN(I)=0
      END DO

      IH=0
      IBD2S=0
      IBD2E=IBMAX(4)
C ---  IOB(5)=IHP1(ICH+2)   ! ( IO*100 + IEX )*100 + ISUB 
      IF (IO4.GT.NOBT) THEN
        IBD2S=MOD(IOB(5),10000)/100
        IBD2E=MOD(IOB(5),10000)/100
      END IF
      IBS2S=0
      IBS2E=IBMAX(2)
      IF (IO2.GT.NOBT) THEN
        IBS2S=MOD(IOB(5),10000)/100
        IBS2E=MOD(IOB(5),10000)/100
      END IF
C      WRITE(6,*)'IBD2E,IBD2S=',IBD2S,IBD2E
      DO IBD2=IBD2S,IBD2E
        DO IBD1=0,IBMAX(3)
          DO IBS2=IBS2S,IBS2E
*VDIR NOVECTOR
*VOCL LOOP,SCALAR
            DO IBS1=0,IBMAX(1)
              TR=0D0
              M1=IBS1/2-L1
              M2=IBS2/2-L2
              M3=IBD1/2-L3
              M4=IBD2/2-L4
              MS1=MOD(IBS1,2)
              MS2=MOD(IBS2,2)
              MS3=MOD(IBD1,2)
              MS4=MOD(IBD2,2)
              IF (M1+M2.EQ.M3+M4.AND.MS1+MS2.EQ.MS3+MS4) THEN
                IF (MS1.EQ.MS3) THEN
*VOPTION NOVEC
                  DO K=K1,K2,2
                    TR=TR+RC(K,L3,M3,L1,M1)*RC(K,L2,M2,L4,M4)*RDR(K)
                  END DO
                END IF
                TD=TR
                IF (MS1.EQ.MS4) THEN
*VOPTION NOVEC
                  DO K=K3,K4,2
                    TR=TR-RC(K,L4,M4,L1,M1)*RC(K,L2,M2,L3,M3)*REX(K)
                  END DO
                END IF
                TEX=TR
              END IF
              IF ( (IO1.NE.IO2.OR.IBS1.LT.IBS2) .AND. 
     &             (IO3.NE.IO4.OR.IBD1.LT.IBD2) .AND.
     &             ( IO2.NE.IO4 .OR. IO1.NE.IO3
     &               .OR. IBS2.LE.IBD2 .OR. (.NOT.HMLTN) )
     &                          .AND.(ABS(TR).GT.1.0D-6)    ) THEN
                IH=IH+1
                I1=4*(IH-1)+IOS(1)
                I2=4*(IH-1)+IOS(2)
                I3=4*(IH-1)+IOS(3)
                I4=4*(IH-1)+IOS(4)
                ICMP(I1)=2**IBS1
                ICMP(I2)=IOR(ICMP(I2),2**IBS2)
                ICMP(I3)=IOR(ICMP(I3),2**IBD1)
                ICMP(I4)=IOR(ICMP(I4),2**IBD2)
                ITST(I1)=2**IBS1
                ITST(I2)=IOR(ITST(I2),2**IBS2)
                IMSK(I1)=2**(IBS1+1)-1
                IMSK(I2)=IEOR(IMSK(I2),2**IBS2-1)
                IMSK(I3)=IEOR(IMSK(I3),2**(IBD1+1)-1)
                IMSK(I4)=IEOR(IMSK(I4),2**IBD2-1)
                IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS1))
                IMSK(I2)=IAND(IMSK(I2),IEOR(-1,2**IBS2))
                ITRN(I1)=2**IBS1
                ITRN(I2)=IEOR(ITRN(I2),2**IBS2)
                ITRN(I3)=IEOR(ITRN(I3),2**IBD1)
                ITRN(I4)=IEOR(ITRN(I4),2**IBD2)
                HIN(IH)=TR
              END IF
C    WRITE(6,*)'M1-M4 MS1-MS4  ',M1,M2,M3,M4,'   ',MS1,MS2,MS3,MS4
C    WRITE(6,*)'TR=',TR
            END DO
          END DO
        END DO
      END DO
C
      NHIN=IH

      END SUBROUTINE MLTP

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Generates a set of bit operators for a one-body interaction
C   specified using the command in the X-card for the crystal field, 
C   hybridization and radiative transition matrix
C   such as "Ak", "CAk", "10Dq", "VOh", "VD4h", "Dk".
C   Called from the subroutione HGEN as NTYP=2.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HYBR(NDEG,NOBT,IOB,IOS,NBLK,R
     &                ,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   INTMAX,      NBLKM
      PARAMETER(INTMAX=10000,NBLKM=200)
      INTEGER   LMAX, KMAX
      PARAMETER(LMAX=8,KMAX=LMAX*2)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER NOBT
      INTEGER IOB(5)
      INTEGER IOS(*)
      INTEGER NBLK
      REAL*8  R(NBLKM)
      REAL*8  HIN(INTMAX)
      INTEGER NHIN
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)
      LOGICAL HMLTN

C Global variable 
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)
      COMMON /ANGL/RC

C Local variables 
      INTEGER IBMAX(4)
      REAL*8 A(0:KMAX,-KMAX:KMAX)
      INTEGER I,I1,I2,IO1,IO2,IH,IP,K,KLIM,L1,L2,M,M1,M2,MS1,MS2
      INTEGER IBS,IBD,IBSS,IBSE,IBDS,IBDE
      REAL*8 TR
C -------------
C      WRITE(6,*)'enter HYBR'

      DO I=1,2
        IBMAX(I)=NDEG( IOB(I) )-1
      END DO

      IO1=IOB(1)
      IO2=IOB(2)
      IF ( MOD(NDEG(IO1),2) .NE. 1 ) THEN
        L1=NDEG(IO1)/4
        L2=NDEG(IO2)/4
      ELSE
        L1=NDEG(IO1)/2
        L2=NDEG(IO2)/2
      END IF

      DO K=ABS(L1-L2),L1+L2
        DO M=-K,K
          A(K,M)=0D0
        END DO
      END DO

      IP=0
C      WRITE(6,*)'L1,L2=',L1,L2
      DO K=ABS(L1-L2),L1+L2,2
        KLIM=K
        IF ( IP+2*K+1 .GT. NBLK ) EXIT 
        DO M=-K,K
          A(K,M)=R(IP+K+M+1)
C          WRITE(6,*)'K,M,Ak,m=',K,M,A(K,M)
        END DO
        IP=IP+2*K+1
      END DO

C      WRITE(6,*)'IO1,IO2,L1,L2=',IO1,IO2,L1,L2
      DO I=1,INTMAX*4
        ICMP(I)=0
        ITST(I)=0
        IMSK(I)=0
        ITRN(I)=0
      END DO 
C
      IH=0
      IBDS=0
      IBDE=IBMAX(2)
C ---  IOB(5)=IHP1(ICH+2)   ! ( IO*100 + IEX )*100 + ISUB 
      IF (IO2.GT.NOBT) THEN
        IBDS=MOD(IOB(5),10000)/100
        IBDE=MOD(IOB(5),10000)/100
      END IF
      IBSS=0
      IBSE=IBMAX(1)
      IF (IO1.GT.NOBT) THEN
        IBSS=MOD(IOB(5),10000)/100
        IBSE=MOD(IOB(5),10000)/100
      END IF
C      WRITE(6,*)'IBSS-IBDS',IBSS,IBSE,IBDE,IBDS
C
      IF ( MOD(NDEG(IO1),2).NE.1) THEN

        DO IBD=IBDS,IBDE
C          WRITE(6,*)' '
C          WRITE(6,'(I4,I4)')IBD/2-L2, 2*MOD(IBD,2)-1
*VDIR NOVECTOR
*VOCL LOOP,SCALAR
          DO IBS=IBSS,IBSE
            TR=0D0
            M1=IBS/2-L1
            MS1=MOD(IBS,2)
            M2=IBD/2-L2
            MS2=MOD(IBD,2)
            IF (MS1.EQ.MS2) THEN
              DO K=KLIM,ABS(M1-M2),-2
                TR=TR+RC(K,L2,M2,L1,M1)*A(K,M2-M1)
              END DO 
            END IF
C            WRITE(6,'(F10.5,$)')TR
            IF ( ( IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &         .AND. (ABS(TR).GT.10D-6)   ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO

      ELSE 

        DO IBD=IBDS,IBDE
C          WRITE(6,*)' '
C          WRITE(6,'(I4,I4)')IBD/2-L2, 2*MOD(IBD,2)-1
*VDIR NOVECTOR
*VOCL LOOP,SCALAR
          DO IBS=IBSS,IBSE
            TR=0D0
            M1=IBS-L1
            M2=IBD-L2
            DO K=KLIM,ABS(M1-M2),-2
              TR=TR+RC(K,L2,M2,L1,M1)*A(K,M2-M1)
            END DO 
C            WRITE(6,'(F10.5,$)')TR
            IF ( ( IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &           .AND. (ABS(TR).GT.10D-6) ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO

      END IF

      NHIN=IH

      END SUBROUTINE HYBR

C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     generate a set of bit operators for
C     a < a_{i}^dagger a_{j} >
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CCOR(NDEG,IOS,R
     &                ,HIN,NHIN,ICMP,ITST,IMSK,ITRN)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   INTMAX,      NBLKM
      PARAMETER(INTMAX=10000,NBLKM=200)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER IOS(*)
      REAL*8  R(NBLKM)
      REAL*8  HIN(INTMAX)
      INTEGER NHIN
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)

C Local variables 
      INTEGER I,I1,I2,IBS,IBD
C ------------
C      WRITE(6,*)'enter CCOR'
C
      DO I=1,4
        ICMP(I)=0
        ITST(I)=0
        IMSK(I)=0
        ITRN(I)=0
      END DO 
C
      IBS=NINT(R(1))
      IBD=NINT(R(2))
      I1=IOS(1)
      I2=IOS(2)
C
      ICMP(I1)=2**IBS
      ICMP(I2)=IOR(ICMP(I2),2**IBD)
      ITST(I1)=2**IBS
      IMSK(I1)=2**(IBS+1)-1
      IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
      IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
      ITRN(I1)=2**IBS
      ITRN(I2)=IEOR(ITRN(I2),2**IBD)
      HIN(1)=1D0
      NHIN=1

      END SUBROUTINE CCOR

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Generate a set of bit operators for spin-orbit interaction
C    Called from the subroutione HGEN as NTYP=3.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SOIN(NDEG,IOB,IOS,R
     &                ,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX,  KMAX,       INTMAX,      NBLKM
      PARAMETER(LMAX=8,KMAX=LMAX*2,INTMAX=10000,NBLKM=200)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER IOB(5)
      INTEGER IOS(*)
      REAL*8  R(NBLKM)
      REAL*8  HIN(INTMAX)
      INTEGER NHIN
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)
      LOGICAL HMLTN

C Global variable 
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)
      COMMON /ANGL/RC

C Local variables 
      INTEGER IBMAX(4)
      REAL*8 T(-1:1)
      INTEGER I,I1,I2,IBS,IBD,IO1,IO2,IH,L1,L2,M,M1,M2,MS1,MS2
      REAL*8 TR,ZTA
C ----------------

      DO I=1,2
        IBMAX(I)= NDEG( IOB(I) ) -1
      END DO
C
      IO1=IOB(1)
      IO2=IOB(2)
      ZTA  =R(1)
      T(-1)=R(2)
      T( 0)=R(3)
      T( 1)=R(4)
C
      DO I=1,INTMAX*4
        ICMP(I)=0
        ITST(I)=0
        IMSK(I)=0
        ITRN(I)=0
      END DO
C
      IH=0
      IF ( MOD( NDEG(IO1), 4) .EQ. 2) THEN
C
        L1=NDEG(IO1)/4
        L2=NDEG(IO2)/4
C
        DO IBD=0,IBMAX(2)
          DO IBS=0,IBMAX(1)

            TR=0D0
            M1=IBS/2-L1
            M2=IBD/2-L2
            MS1=MOD(IBS,2)
            MS2=MOD(IBD,2)
            IF ((M1.EQ.M2).AND.(MS1.EQ.MS2)) THEN
              TR=TR+(DBLE(MS1)-0.5D0) * ( ZTA*DBLE(M1)  
     &                      -2D0*T(0)*RC(2,L2,M2,L1,M1) ) 
            ELSE IF ((M1.EQ.M2).AND.(MS1.NE.MS2)) THEN
              TR=TR +T(MS2-MS1)*RC(2,L2,M2,L1,M1)
            ELSE IF ( ( M1+1.EQ.M2 .OR. M1.EQ.M2+1  )
     &                  .AND.   (MS1.EQ.MS2)   ) THEN
              M=(M2-M1)*M1
              TR=TR +(DBLE(MS1)-0.5D0)*DSIGN(T(M2-M1),DBLE(M2-M1))
     &                   *DSQRT(6D0)*RC(2,L2,M2,L1,M1)
            ELSE IF ( ( M1+2.EQ.M2 .AND. MS1.EQ.MS2+1 )
     &            .OR. ( M1.EQ.M2+2 .AND. MS1+1.EQ.MS2 ) ) THEN
              TR=TR-T(MS1-MS2)*DSQRT(6D0)*RC(2,L2,M2,L1,M1)
            ELSE IF ((M1+MS1).EQ.(M2+MS2)) THEN
              M=(2*MS2-1)*M2
              TR=TR+0.5D0*ZTA*DSQRT(DBLE((L1-M)*(L1+M+1)))
     &        +DSQRT(1.5D0)*DSIGN(T(0),DBLE(M2-M1))*RC(2,L2,M2,L1,M1)
            END IF
            IF ( ( IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &            .AND. (ABS(TR).GT.10D-10) ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO

      END IF
      NHIN=IH

      END SUBROUTINE SOIN

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Generase a set of bit operators for 
C    molecular or external magnetic fields specified 
C    using "Hz", "Ha", "Bz" or "Ba" commands in the X-card.
C    Called from the subroutione HGEN as NTYP=11.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MFLD(NDEG,IOB,IOS,R
     &               ,HIN,NHIN,ICMP,ITST,IMSK,ITRN,HMLTN)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX,  KMAX,       INTMAX
      PARAMETER(LMAX=8,KMAX=LMAX*2,INTMAX=10000)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NBLKM
      PARAMETER(NBLKM=200)

C Dummy arguments
      INTEGER NDEG(NOBTM)
      INTEGER IOB(5)
      INTEGER IOS(*)
      REAL*8  R(NBLKM)
      REAL*8  HIN(INTMAX)
      INTEGER NHIN
      INTEGER ICMP(4*INTMAX),ITST(4*INTMAX)
      INTEGER IMSK(4*INTMAX),ITRN(4*INTMAX)
      LOGICAL HMLTN

C Global variable 
      REAL*8 RC(0:KMAX,0:LMAX,-LMAX:LMAX,0:LMAX,-LMAX:LMAX)
      COMMON /ANGL/RC

C Local variables 
      INTEGER IBMAX(4)
      REAL*8 H(-1:1),B(-1:1)
      INTEGER I,I1,I2,IBS,IBD,IO1,IO2,IH,L1,L2,M,M1,M2,MS1,MS2
      REAL*8 TR
C ----------------

      DO I=1,2
        IBMAX(I)= NDEG( IOB(I) ) -1
      END DO
C
      IO1=IOB(1)
      IO2=IOB(2)
      H(-1)=R(1)
      H( 0)=R(2)
      H( 1)=R(3)
      B(-1)=R(4)
      B( 0)=R(5)
      B( 1)=R(6)
C
      DO I=1,INTMAX*4
        ICMP(I)=0
        ITST(I)=0
        IMSK(I)=0
        ITRN(I)=0
      END DO
C
      IH=0
      IF ( MOD( NDEG(IO1), 4) .EQ. 2) THEN
C
        L1=NDEG(IO1)/4
        L2=NDEG(IO2)/4
C
        DO IBD=0,IBMAX(2)
          DO IBS=0,IBMAX(1)

            TR=0D0
            M1=IBS/2-L1
            M2=IBD/2-L2
            MS1=MOD(IBS,2)
            MS2=MOD(IBD,2)
            IF ((M1.EQ.M2).AND.(MS1.EQ.MS2)) THEN
              TR=TR+(DBLE(MS1)-0.5D0)*(H(0)+2D0*B(0) )+DBLE(M1)*B(0)
            ELSE IF ((M1.EQ.M2).AND.(MS1.NE.MS2)) THEN
              TR=TR+H(MS2-MS1)+2D0*B(MS2-MS1)
            ELSE IF ( ( M1+1.EQ.M2 .OR. M1.EQ.M2+1  )
     &                  .AND.   (MS1.EQ.MS2)   ) THEN
              M=(M2-M1)*M1
              TR=TR+B(M2-M1)*DSQRT(DBLE(L1-M)*(L1+M+1))
            END IF
            IF ( ( IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &            .AND. (ABS(TR).GT.10D-10) ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO

      ELSE IF ( MOD( NDEG(IO1), 2) .EQ. 0) THEN
 
        DO IBD=0,IBMAX(2)
          DO IBS=0,IBMAX(1)
            TR=0D0
            M1=IBS/2
            M2=IBD/2
            MS1=MOD(IBS,2)
            MS2=MOD(IBD,2)
            IF ((M1.EQ.M2).AND.(MS1.EQ.MS2)) THEN
              TR=TR + (DBLE(MS1)-0.5D0) * (H(0) +2d0*B(0))
            ELSE IF ((M1.EQ.M2).AND.(MS1.NE.MS2)) THEN
              TR=TR + H(MS2-MS1) + 2D0*B(MS2-MS1)
            END IF
            IF ( (IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &          .AND. (ABS(TR).GT.10D-10) ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO
    
      ELSE IF ( MOD( NDEG(IO1), 2) .EQ. 1) THEN
 
        L1=NDEG(IO1)/2
        L2=NDEG(IO2)/2
        DO IBD=0,IBMAX(2)
          DO IBS=0,IBMAX(1)
            TR=0D0
            M1=IBS-L1
            M2=IBD-L2
            IF ( M1.EQ.M2 ) THEN
              TR=TR + DBLE(M1) * B(0)
            ELSE IF ( M1+1.EQ.M2 .OR. M1.EQ.M2+1 )THEN
             M=(M2-M1)*M1
             TR=TR+B(M2-M1)*DSQRT(DBLE(L1-M)*(L1+M+1))
            END IF
            IF ( (IO1.NE.IO2 .OR. IBS.LE.IBD .OR. (.NOT.HMLTN) )
     &           .AND. (ABS(TR).GT.10D-10) ) THEN
              IH=IH+1
              I1=4*(IH-1)+IOS(1)
              I2=4*(IH-1)+IOS(2)
              ICMP(I1)=2**IBS
              ICMP(I2)=IOR(ICMP(I2),2**IBD)
              ITST(I1)=2**IBS
              IMSK(I1)=2**(IBS+1)-1
              IMSK(I2)=IEOR(IMSK(I2),2**(IBD+1)-1)
              IMSK(I1)=IAND(IMSK(I1),IEOR(-1,2**IBS))
              ITRN(I1)=2**IBS
              ITRN(I2)=IEOR(ITRN(I2),2**IBD)
C              IF (I1.EQ.I2.AND.IBS.GT.IBD) TR=-TR
              HIN(IH)=TR
            END IF
          END DO
        END DO

      END IF
      NHIN=IH

      END SUBROUTINE MFLD 


C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RMDIAG(NOP,NHOP,NHM,H,IHP1,IHP2)
      IMPLICIT NONE 

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy arguments
      INTEGER NOP
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

C Local variables

      INTEGER ICH, NBLK, NIN1, NIN2, NIN
C ---------------------------
C
C      WRITE(6,*)'enter RMDIAG'
C      WRITE(6,*)'NOP IO IEX ISB',NOP,IO,IEX,ISB

      ICH=NHOP(NOP)
      DO WHILE( ICH .LT. NHOP(NOP+1) )
        NIN1 = IHP1(ICH+4)
        NIN2 = IHP2(ICH+4)
        NBLK = IHP1(ICH+5)/2048
        IF (NIN1.NE.NIN2) THEN
          NIN=MIN(NIN1,NIN2)
          IHP1(ICH+4)=NIN
          IHP2(ICH+4)=NIN
        END IF
        ICH=ICH+NBLK
      END DO !ICH

      END SUBROUTINE RMDIAG


C>>>START SCALAR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Approximated diagoral part of squared Hamiltonial NOP
C    can be calculated using this subroutine.
C     
C      CALL CLRV(N,HD2) 
C      CALL HDG2(N,HD2,NOP,NHOP,NHM,H,IHP1,IHP2)
C      CALL HDG2(N,HD2,NOP,NHOP,NHM,H,IHP2,IHP1)
C      DO I=1,N
C        HD2(I)=HD2(I)+HD(I)**2
C      END DO 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HDG2(N,HD2,NOP,NHOP,NHM,H,IHP1,IHP2)
      IMPLICIT NONE

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBASLIM,      NCLIM
      PARAMETER(NBASLIM=10000,NCLIM=100)

C Dummy arguments
      INTEGER N
      REAL*8  HD2(N)
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

C Local variables
      INTEGER NSTP1(5), NSTP2(5), NCNT(5)
      INTEGER NLP,NIN1,ICPX,NBLK,NPTR,NPTR0
      INTEGER NBASM
      INTEGER I,I1,I2,I3,I4,I5,INI,K1,K01,NC,IC,ICH
      REAL*8  T1
C----------------
C
C      WRITE(6,*)'enter HDG2(Scalar version)'

      ICH=NHOP(NOP)
      DO WHILE ( ICH .LT. NHOP(NOP+1) )

        NPTR = IHP1(ICH+1)
        ICPX = MOD( IHP1(ICH+3), 4 )
        NBASM=      IHP1(ICH+3)/4
        NIN1 = IHP1(ICH+4)
        NBLK = IHP1(ICH+5)/2048

        NLP  =NINT( H(NPTR) )
        DO I=1,NLP
          NSTP1(I)= IHP1(NPTR+I-1)
          NSTP2(I)= IHP2(NPTR+I-1)
          NCNT (I)= NINT( H(NPTR+I) )
        END DO
        DO I=NLP+1,5
          NSTP1(I)=1
          NSTP2(I)=1
          NCNT (I)=1
        END DO

        IF ( NSTP1(1)*NSTP2(1).EQ.4 .AND. NLP.GE.1 
     &     .AND. ICPX.GE.2 ) THEN
          NCNT (1)=2*NCNT(1)
          NSTP1(1)=1
          NSTP2(1)=1
        END IF

        NPTR0=NPTR+NLP
        IF ( NSTP1(1)*NSTP2(1).EQ.1 .AND. NLP.GE.1 ) THEN
          DO I5=0,NCNT(5)-1
            DO I4=0,NCNT(4)-1
              DO I3=0,NCNT(3)-1
                DO I2=0,NCNT(2)-1
                  K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
                  NPTR=NPTR0
                  DO INI=1,NIN1
                    NC=IHP1(NPTR)
                    DO IC=NPTR+1,NPTR+NC
                      K1=K01+IHP1(IC)
                      T1=H(IC)**2
!IBM* ASSERT (NODEPS,ITERCNT(100))
                      DO I1=K1,K1+NCNT(1)-1
                        HD2(I1)=HD2(I1)+T1
                      END DO
                    END DO
                    NPTR=NPTR+NC+1
                  END DO
                END DO
              END DO
            END DO 
          END DO
        ELSE IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR0).LT.NCLIM )
     &          .AND. NLP.GE.1 ) THEN
          DO I5=0,NCNT(5)-1
            DO I4=0,NCNT(4)-1
              DO I3=0,NCNT(3)-1
                DO I2=0,NCNT(2)-1
                  K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
                  NPTR=NPTR0
                  IF (ICPX.EQ.0) THEN
                    DO INI=1,NIN1
                      NC=IHP1(NPTR)
                      DO IC=NPTR+1,NPTR+NC
                        K1=K01+IHP1(IC)
                        T1=H(IC)**2
!IBM* ASSERT (ITERCNT(100))
                        DO I1=K1,K1+(NCNT(1)-1)*NSTP1(1),NSTP1(1)
                          HD2(I1)=HD2(I1) + T1
                        END DO
                      END DO
                      NPTR=NPTR+NC+1
                    END DO
                  ELSE !ICPX=1, 2 or 3
                    DO INI=1,NIN1
                      NC=IHP1(NPTR)
                      DO IC=NPTR+1,NPTR+NC
                        K1=K01+IHP1(IC)
                        T1=H(IC)**2
!IBM* ASSERT (ITERCNT(100))
                        DO I1=K1,K1+(NCNT(1)-1)*NSTP1(1),NSTP1(1)
                          HD2(I1)  =HD2(I1)  +T1
                          HD2(I1+1)=HD2(I1+1)+T1
                        END DO
                      END DO
                      NPTR=NPTR+NC+1
                    END DO
                  END IF !ICPX
                END DO
              END DO
            END DO
          END DO
        ELSE 
          DO I5=0,NCNT(5)-1
            DO I4=0,NCNT(4)-1
              DO I3=0,NCNT(3)-1
                DO I2=0,NCNT(2)-1
                  DO I1=0,NCNT(1)-1
                    K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)
     &                 +I2*NSTP1(2)+I1*NSTP1(1)
                    NPTR=NPTR0
                    IF (ICPX.EQ.0) THEN
                      DO INI=1,NIN1
                        NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                        DO IC=NPTR+1,NPTR+NC
                          HD2(IHP1(IC)+K01)=HD2(IHP1(IC)+K01)+H(IC)**2
                        END DO
                        NPTR=NPTR+NC+1
                      END DO
                    ELSE !ICPX=1, 2 or 3
                      DO INI=1,NIN1
                        NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                        DO IC=NPTR+1,NPTR+NC
                          HD2(IHP1(IC)+K01)=HD2(IHP1(IC)+K01)+H(IC)**2
                          HD2(IHP1(IC)+K01+1)
     &                                     =HD2(IHP1(IC)+K01+1)+H(IC)**2
                        END DO
                        NPTR=NPTR+NC+1
                      END DO
                    END IF !ICPX
                  END DO
                END DO
              END DO
            END DO
          END DO 
        END IF

        ICH = ICH + NBLK

      END DO ! ICH loop
C
      END SUBROUTINE HDG2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    This subroutine creates the diagonal part of Hamiltonian
C    (optimized for SCALAR machines)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE HDIAG(NHM,H,IHP1,NHDM,HD,NLP,NSTP1,NCNT
     &                ,NPTR,ICPX,NBASM)
      IMPLICIT NONE

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NLPM 
      PARAMETER(NLPM=5)
      INTEGER   NBASLIM,      NCLIM
      PARAMETER(NBASLIM=10000,NCLIM=100)

C Dummy arguments
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NLP
      INTEGER NCNT(0:NLPM),NSTP1(0:NLPM)
       
      INTEGER NPTR, ICPX, NBASM

C Local variables
      INTEGER I,I1,I2,I3,K1,K01,IC,NC
      REAL*8  T1
C----------------

      DO I=NLP+1,3
        NSTP1(I)=1
        NCNT (I)=1
      END DO
      NC=IHP1(NPTR)

      IF (ICPX.EQ.0) THEN
        IF ( NSTP1(1).EQ.1 .AND. NLP.GE.1 ) THEN

          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I3*NSTP1(3)+I2*NSTP1(2)
              DO IC=NPTR+1,NPTR+NC
                K1=K01+IHP1(IC)
                T1=H(IC)
!IBM* ASSERT (NODEPS,ITERCNT(100))
                DO I1=K1,K1+NCNT(1)-1
                  HD(I1)=HD(I1)+T1
                END DO
              END DO
            END DO
          END DO 

        ELSE IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR).LT.NCLIM )
     &    .AND. NLP.GE.1 ) THEN

          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I3*NSTP1(3)+I2*NSTP1(2)
              DO IC=NPTR+1,NPTR+NC
                K1=K01+IHP1(IC)
                T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
                DO I1=K1,K1+(NCNT(1)-1)*NSTP1(1),NSTP1(1)
                  HD(I1)=HD(I1)+T1
                END DO
              END DO
            END DO
          END DO

        ELSE 

          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              DO I1=0,NCNT(1)-1
                K01=I3*NSTP1(3)+I2*NSTP1(2)+I1*NSTP1(1)
!IBM* ASSERT (ITERCNT(100))
                DO IC=NPTR+1,NPTR+NC
                  HD(IHP1(IC)+K01)=HD(IHP1(IC)+K01)+H(IC)
                END DO
              END DO
            END DO
          END DO 

        END IF
      ELSE
        IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR).LT.NCLIM )
     &    .AND. NLP.GE.1 ) THEN

          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I3*NSTP1(3)+I2*NSTP1(2)
              DO IC=NPTR+1,NPTR+NC
                K1=K01+IHP1(IC)
                T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
                DO I1=K1,K1+(NCNT(1)-1)*NSTP1(1),NSTP1(1)
                  HD(I1)  =HD(I1)  +T1
                  HD(I1+1)=HD(I1+1)+T1
                END DO
              END DO
            END DO
          END DO

        ELSE

          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              DO I1=0,NCNT(1)-1
                K01=I3*NSTP1(3)+I2*NSTP1(2)+I1*NSTP1(1)
!IBM* ASSERT (ITERCNT(100))
                DO IC=NPTR+1,NPTR+NC
                  HD(IHP1(IC)+K01)  =HD(IHP1(IC)+K01)  + H(IC)
                  HD(IHP1(IC)+K01+1)=HD(IHP1(IC)+K01+1)+ H(IC)
                END DO
              END DO
            END DO
          END DO

        END IF
      END IF
C
      END SUBROUTINE HDIAG 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    This subroutine "OPRT" is for SCALAR machines.
C     V2=V2+ Op(nop,io,iex,isub) V1
C     V2,V1    : VECTORS
C     Op       : OPERATOR (Hamiltonian, Transtion matrix etc.)
C     nop      : OPERATOR NUMBER
C     io & iex : ORBIT & BASE NUMBER OF A CONTINUUM STATE
C     isub     : SUBSCRIPT OF THE OPERATOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPRT(N2,V2,N1,V1,NOP,IO,IEX,ISB,NHOP,NHM,H,IHP1,IHP2)
      IMPLICIT NONE

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBASLIM,      NCLIM
      PARAMETER(NBASLIM=10000,NCLIM=100)

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)
      INTEGER NOP,IO,IEX,ISB

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

C Global variable
      INTEGER NFREQ(NHOPM)
      COMMON /FREQ/NFREQ

C Local variables
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)
      INTEGER I,NBLK,NLP,NIN1,NIN2,ICPX,ICH,NPTR,NBASM
C----------------
C
C      WRITE(6,*)'enter OPRT(Scalar version)'
C      WRITE(6,*)'NOP IO IEX ISB',NOP,IO,IEX,ISB

      NFREQ(NOP)=NFREQ(NOP)+1
      ICH=NHOP(NOP)
      DO WHILE( ICH .LT. NHOP(NOP+1) )
        NIN1 = IHP1(ICH+4)
        NBLK = IHP1(ICH+5)/2048
        IF ( IHP1(ICH+2) .EQ. 10000*IO+100*IEX+ISB 
     &    .AND.   NIN1.NE.0 ) THEN

          NPTR = IHP1(ICH+1)
          ICPX =MOD( IHP1(ICH+3), 4 )
          NBASM=     IHP1(ICH+3)/4
          NIN2 = IHP2(ICH+4)

          NLP=NINT( H(NPTR) )
          DO I=1,NLP
            NSTP1(I)=   IHP1(NPTR+I-1)
            NSTP2(I)=   IHP2(NPTR+I-1)
            NCNT (I)=NINT( H(NPTR+I) )
          END DO
          DO I=NLP+1,5
            NSTP1(I)=1
            NSTP2(I)=1
            NCNT (I)=1
          END DO

Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
C          write(6,*)' NPTR=',NPTR
C          write(6,*)' NIN1=',IHP1(ICH+4)
C          write(6,*)' NIN2=',IHP2(ICH+4)
C          write(6,*)' ICPX=',ICPX
C          write(6,*)' LIN =',IHP2(ICH+6)
C          write(6,*)' NBLK=',IHP1(ICH+5)/2048 ! ICH=ICH+NBLK
C          write(6,*)' ICH=',ICH,' NHOP(NOP+1)=',NHOP(NOP+1)
C          write(6,*)' NLP=',NLP
C          DO I=1,NLP
C            write(6,*)' loop #',I
C             write(6,*)' NSTP1=',IHP1(NPTR+I-1)
C             write(6,*)' NSTP2=',IHP2(NPTR+I-1)
C             write(6,*)' NCNT =',NINT( H(NPTR+I) )
C          END DO
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          NPTR=NPTR+NLP
          IF (ICPX.EQ.0) THEN
            IF ( NSTP1(1)*NSTP2(1).EQ.1 .AND. NLP.GE.1 ) THEN
              CALL LOOP1(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                  ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
            ELSE IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR).LT.NCLIM )
     &               .AND. NLP.GE.1 ) THEN
              CALL LOOP2(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                  ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
            ELSE 
              CALL LOOP3(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                  ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
            END IF
          ELSE IF (ICPX.EQ.1) THEN
            IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR).LT.NCLIM )
     &        .AND. NLP.GE.1 ) THEN
              CALL LOOP2R(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                   ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
            ELSE
              CALL LOOP3R(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                   ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
            END IF
          ELSE
            IF (NSTP1(1)*NSTP2(1).EQ.4.AND.NLP.GE.1) THEN
              CALL LOOP1I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                   ,NIN1,NPTR,NSTP1,NSTP2,NCNT,ICPX)
            ELSE IF ( ( NBASM.GT.NBASLIM .OR. IHP1(NPTR).LT.NCLIM )
     &              .AND. NLP.GE.1 ) THEN
              CALL LOOP2I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                   ,NIN1,NPTR,NSTP1,NSTP2,NCNT,ICPX)
            ELSE 
              CALL LOOP3I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                   ,NIN1,NPTR,NSTP1,NSTP2,NCNT,ICPX)
            END IF
          END IF
        END IF
        ICH =ICH + NBLK 
      END DO ! ICH loop
C
      END SUBROUTINE OPRT

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH STEP ONE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP1(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)
 
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN1, NIN2, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K1,K2,K01,K02,NC,IC,NPTR0
      REAL*8 T1
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I5*NSTP1(5)+I4*NSTP1(4)
     &           +I3*NSTP1(3)+I2*NSTP1(2)
              K02=I5*NSTP2(5)+I4*NSTP2(4)
     &           +I3*NSTP2(3)+I2*NSTP2(2)
              NPTR=NPTR0
              DO INI=1,MIN(NIN1,NIN2)
                NC=IHP1(NPTR)
                DO IC=NPTR+1,NPTR+NC
                  K2=K02+IHP2(IC)
                  K1=K01+IHP1(IC)-K2
                  T1=H(IC)
!IBM* ASSERT (NODEPS,ITERCNT(100))
                  DO I1=K2,K2+NCNT(1)-1
                    V2(I1)=V2(I1)+T1*V1(I1+K1)
                  END DO
                END DO
                NPTR=NPTR+NC+1
              END DO
            END DO
          END DO
        END DO 
      END DO
      IF (NIN1.GT.NIN2) THEN
        NC=IHP1(NPTR)
        DO I3=0,NCNT(3)-1
          DO I2=0,NCNT(2)-1
            K01=I3*NSTP1(3)+I2*NSTP1(2)
            DO IC=NPTR+1,NPTR+NC
              K1=K01+IHP1(IC)
              T1=H(IC)
!IBM* ASSERT (NODEPS,ITERCNT(100))
              DO I1=K1,K1+NCNT(1)-1
                V2(I1)=V2(I1)+T1*V1(I1)
              END DO
            END DO
          END DO
        END DO 
      END IF

      END SUBROUTINE LOOP1

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH STEP MORE THEN ONE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP2(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)
 
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN1, NIN2, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K1,K2,K01,K02,NC,IC,NPTR0
      REAL*8 T1
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
              K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
              NPTR=NPTR0
              DO INI=1,MIN(NIN1,NIN2)
                NC=IHP1(NPTR)
                DO IC=NPTR+1,NPTR+NC
                  K2=K02+IHP2(IC)
                  K1=K01+IHP1(IC)
                  T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
                  DO I1=0,NCNT(1)-1
                    V2(K2)=V2(K2)+T1*V1(K1)
                    K2=K2+NSTP2(1)
                    K1=K1+NSTP1(1)
                  END DO
                END DO
                NPTR=NPTR+NC+1
              END DO
            END DO
          END DO
        END DO
      END DO
      IF (NIN1.GT.NIN2) THEN
        NC=IHP1(NPTR)
        DO I3=0,NCNT(3)-1
          DO I2=0,NCNT(2)-1
            K01=I3*NSTP1(3)+I2*NSTP1(2)
            DO IC=NPTR+1,NPTR+NC
              K1=K01+IHP1(IC)
              T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
              DO I1=0,NCNT(1)-1
                V2(K1)=V2(K1)+T1*V1(K1)
                K1=K1+NSTP1(1)
              END DO
            END DO
          END DO
        END DO
      END IF

      END SUBROUTINE LOOP2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH IRREGULAR INTERVALS 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP3(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)

      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN1, NIN2, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K01,K02,NC,IC,NPTR0
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              DO I1=0,NCNT(1)-1
                K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
     &             +I1*NSTP1(1)
                K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
     &             +I1*NSTP2(1)
                NPTR=NPTR0
                DO INI=1,MIN(NIN1,NIN2)
                  NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                  DO IC=NPTR+1,NPTR+NC
                    V2(IHP2(IC)+K02)=V2(IHP2(IC)+K02) 
     &                              +H(IC)*V1(IHP1(IC)+K01)
                  END DO
                  NPTR=NPTR+NC+1
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO 
      IF (NIN1.GT.NIN2) THEN
        NC=IHP1(NPTR)
        DO I3=0,NCNT(3)-1
          DO I2=0,NCNT(2)-1
            DO I1=0,NCNT(1)-1
              K01=I3*NSTP1(3)+I2*NSTP1(2)+I1*NSTP1(1)
!IBM* ASSERT (ITERCNT(100))
              DO IC=NPTR+1,NPTR+NC
                V2(IHP1(IC)+K01)=V2(IHP1(IC)+K01) 
     &                              +H(IC)*V1(IHP1(IC)+K01)
              END DO
            END DO
          END DO
        END DO 
      END IF

      END SUBROUTINE LOOP3
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH STEP MORE THAN ONE (COMPLEX,REAL PART)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP2R(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                 ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)

      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN1, NIN2, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K1,K2,K01,K02,NC,IC,NPTR0
      REAL*8 T1
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
              K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
              NPTR=NPTR0
              DO INI=1,MIN(NIN1,NIN2)
                NC=IHP1(NPTR)
                DO IC=NPTR+1,NPTR+NC
                  K2=K02+IHP2(IC)
                  K1=K01+IHP1(IC)
                  T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
                  DO I1=0,NCNT(1)-1
                    V2(K2)  =V2(K2)  +T1*V1(K1)
                    V2(K2+1)=V2(K2+1)+T1*V1(K1+1)
                    K2=K2+NSTP2(1)
                    K1=K1+NSTP1(1)
                  END DO
                END DO
                NPTR=NPTR+NC+1
              END DO
            END DO
          END DO
        END DO 
      END DO
      IF (NIN1.GT.NIN2) THEN
        NC=IHP1(NPTR)
        DO I3=0,NCNT(3)-1
          DO I2=0,NCNT(2)-1
            K01=I3*NSTP1(3)+I2*NSTP1(2)
            DO IC=NPTR+1,NPTR+NC
              K1=K01+IHP1(IC)
              T1=H(IC)
!IBM* ASSERT (ITERCNT(100))
              DO I1=0,NCNT(1)-1
                V2(K1)  =V2(K1)  +T1*V1(K1)
                V2(K1+1)=V2(K1+1)+T1*V1(K1+1)
                K1=K1+NSTP1(1)
              END DO
            END DO
          END DO
        END DO
      END IF

      END SUBROUTINE LOOP2R

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH IRREGULAR INTERVALS (COMPLEX,REAL PART)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP3R(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                 ,NIN1,NIN2,NPTR,NSTP1,NSTP2,NCNT)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)

      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN1, NIN2, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K01,K02,NC,IC,NPTR0
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              DO I1=0,NCNT(1)-1
                K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
     &             +I1*NSTP1(1)
                K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
     &             +I1*NSTP2(1)
                NPTR=NPTR0
                DO INI=1,MIN(NIN1,NIN2)
                  NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                  DO IC=NPTR+1,NPTR+NC
                    V2(IHP2(IC)+K02)  =V2(IHP2(IC)+K02)  
     &                                +H(IC) * V1(IHP1(IC)+K01)
                    V2(IHP2(IC)+K02+1)=V2(IHP2(IC)+K02+1) 
     &                                +H(IC) * V1(IHP1(IC)+K01+1)
                  END DO
                  NPTR=NPTR+NC+1
                END DO
              END DO
            END DO
          END DO
        END DO 
      END DO
      IF (NIN1.GT.NIN2) THEN
        NC=IHP1(NPTR)
        DO I3=0,NCNT(3)-1
          DO I2=0,NCNT(2)-1
            DO I1=0,NCNT(1)-1
              K01=I3*NSTP1(3)+I2*NSTP1(2)+I1*NSTP1(1)
!IBM* ASSERT (ITERCNT(100))
              DO IC=NPTR+1,NPTR+NC
                V2(IHP1(IC)+K01)  =V2(IHP1(IC)+K01)  
     &                            +H(IC) * V1(IHP1(IC)+K01)
                V2(IHP1(IC)+K01+1)=V2(IHP1(IC)+K01+1) 
     &                            +H(IC) * V1(IHP1(IC)+K01+1)
              END DO
            END DO
          END DO
        END DO
      END IF

      END SUBROUTINE LOOP3R

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH STEP ONE (COMPLEX,IMAG PART)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP1I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                 ,NIN,NPTR,NSTP1,NSTP2,NCNT,ICPX)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)

      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)
      INTEGER ICPX

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K1,K2,K01,K02,NC,IC,NPTR0
      REAL*8 T1
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I5*NSTP1(5)+I4*NSTP1(4)
     &           +I3*NSTP1(3)+I2*NSTP1(2)
              K02=I5*NSTP2(5)+I4*NSTP2(4)
     &           +I3*NSTP2(3)+I2*NSTP2(2)
              NPTR=NPTR0
              DO INI=1,NIN
                NC=IHP1(NPTR)
                DO IC=NPTR+1,NPTR+NC
                  T1=H(IC)
                  IF (ICPX.EQ.3) T1=-T1
                  K2=K02+IHP2(IC)
                  K1=K01+IHP1(IC)-K2
!IBM* ASSERT (NODEPS,ITERCNT(100))
                  DO I1=K2,K2+(NCNT(1)-1)*2,2
                    V2(I1+1)=V2(I1+1)+T1*V1(I1+K1)
                    V2(I1)  =V2(I1)  -T1*V1(I1+K1+1)
                  END DO
                END DO
                NPTR=NPTR+NC+1
              END DO
            END DO
          END DO
        END DO 
      END DO

      END SUBROUTINE LOOP1I

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH STEP MORE THAN ONE (COMPLEX,IMAG PART)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP2I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                 ,NIN,NPTR,NSTP1,NSTP2,NCNT,ICPX)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)
      
      INTEGER NHM 
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)
      INTEGER ICPX

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K1,K2,K01,K02,NC,IC,NPTR0
      REAL*8 T1
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
              K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
              NPTR=NPTR0
              DO INI=1,NIN
                NC=IHP1(NPTR)
                DO IC=NPTR+1,NPTR+NC
                  T1=H(IC)
                  IF (ICPX.EQ.3) T1=-T1 
                  K2=K02+IHP2(IC)
                  K1=K01+IHP1(IC)
!IBM* ASSERT (ITERCNT(100))
                  DO I1=0,NCNT(1)-1
                    V2(K2+1)=V2(K2+1)+T1*V1(K1)
                    V2(K2)  =V2(K2)  -T1*V1(K1+1)
                    K2=K2+NSTP2(1)
                    K1=K1+NSTP1(1)
                  END DO
                END DO
                NPTR=NPTR+NC+1
              END DO
            END DO
          END DO
        END DO 
      END DO

      END SUBROUTINE LOOP2I

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     LOOP WITH IRREGULAR INTERVALS (COMPLEX,IMAG PART)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LOOP3I(N2,V2,N1,V1,NHM,H,IHP1,IHP2
     &                 ,NIN,NPTR,NSTP1,NSTP2,NCNT,ICPX)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N1,N2
      REAL*8  V1(N1)
      REAL*8  V2(N2)

      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NIN, NPTR
      INTEGER NSTP1(5),NSTP2(5),NCNT(5)
      INTEGER ICPX

C Local variable
      INTEGER I1,I2,I3,I4,I5,INI,K01,K02,NC,IC,NPTR0
C -----

      NPTR0=NPTR
      DO I5=0,NCNT(5)-1
        DO I4=0,NCNT(4)-1
          DO I3=0,NCNT(3)-1
            DO I2=0,NCNT(2)-1
              DO I1=0,NCNT(1)-1
                K01=I5*NSTP1(5)+I4*NSTP1(4)+I3*NSTP1(3)+I2*NSTP1(2)
     &             +I1*NSTP1(1)
                K02=I5*NSTP2(5)+I4*NSTP2(4)+I3*NSTP2(3)+I2*NSTP2(2)
     &             +I1*NSTP2(1)
                NPTR=NPTR0
                IF (ICPX.EQ.2) THEN
                  DO INI=1,NIN
                    NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                    DO IC=NPTR+1,NPTR+NC
                      V2(IHP2(IC)+K02+1)=V2(IHP2(IC)+K02+1) 
     &                                  +H(IC)*V1(IHP1(IC)+K01)
                      V2(IHP2(IC)+K02)  =V2(IHP2(IC)+K02)   
     &                                  -H(IC)*V1(IHP1(IC)+K01+1)
                    END DO
                    NPTR=NPTR+NC+1
                  END DO
                ELSE 
                  DO INI=1,NIN
                    NC=IHP1(NPTR)
!IBM* ASSERT (ITERCNT(100))
                    DO IC=NPTR+1,NPTR+NC
                      V2(IHP2(IC)+K02+1)=V2(IHP2(IC)+K02+1) 
     &                                  -H(IC)*V1(IHP1(IC)+K01)
                      V2(IHP2(IC)+K02)  =V2(IHP2(IC)+K02) 
     &                                  +H(IC)*V1(IHP1(IC)+K01+1)
                    END DO
                    NPTR=NPTR+NC+1
                  END DO
                END IF
              END DO
            END DO
          END DO
        END DO 
      END DO
      END SUBROUTINE LOOP3I

C>>>END SCALAR

      BLOCK DATA
      IMPLICIT NONE 

      INTEGER NTSKM, NHOPM
      PARAMETER(NTSKM=30,NHOPM=100)

      INTEGER NTSK, NTST
      CHARACTER*(8) TSK(NTSKM)
      REAL*8  TTSK(NTSKM)
      INTEGER ITSK(NTSKM)
      COMMON /WTM/NTSK,NTST,TSK,TTSK,ITSK

      INTEGER NBMPK
      INTEGER NBWRK(NHOPM) 
      COMMON /NBM/NBMPK,NBWRK

      INTEGER NFREQ(NHOPM)
      COMMON /FREQ/NFREQ
      SAVE /WTM/,/NBM/,/FREQ/
C
      DATA NFREQ/NHOPM*0/
      DATA NTSK/0/,NTST/NTSKM/
      DATA NBMPK/0/
      END
C
      SUBROUTINE MATSTR(NOP,NLP1I,ELLP1I,AVLI1I,AVLO1I
     &                     ,NLP2I,ELLP2I,AVLI2I,AVLO2I
     &                     ,NLP3I,ELLP3I,AVLI3I)
      IMPLICIT NONE 

C Parameter
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)
 
C Dummy arguments
      INTEGER NOP
      INTEGER NLP1I,  NLP2I,  NLP3I
      REAL*8  ELLP1I, ELLP2I, ELLP3I
      REAL*8  AVLI1I, AVLI2I, AVLI3I
      REAL*8  AVLO1I, AVLO2I

      INTEGER NLP1 (NHOPM), NLP2 (NHOPM), NLP3 (NHOPM)
      REAL*8  ELLP1(NHOPM), ELLP2(NHOPM), ELLP3(NHOPM)
      REAL*8  AVLI1(NHOPM), AVLI2(NHOPM), AVLI3(NHOPM)
      REAL*8  AVLO1(NHOPM), AVLO2(NHOPM)

      COMMON /OPRSTS/NLP1 ,NLP2, NLP3
     &              ,ELLP1,ELLP2,ELLP3
     &              ,AVLI1,AVLI2,AVLI3
     &              ,AVLO1,AVLO2

      IF (NOP.GE.1.AND.NOP.LE.NHOPM) THEN
        NLP1(NOP) =NLP1I
        NLP2(NOP) =NLP2I
        NLP3(NOP) =NLP3I
        ELLP1(NOP)=ELLP1I
        ELLP2(NOP)=ELLP2I
        ELLP3(NOP)=ELLP3I
        AVLI1(NOP)=AVLI1I/MAX(DBLE(NLP1I),1D0)
        AVLI2(NOP)=AVLI2I/MAX(DBLE(NLP2I),1D0)
        AVLI3(NOP)=AVLI3I/MAX(DBLE(NLP3I),1D0)
        AVLO1(NOP)=AVLO1I/MAX(DBLE(NLP1I),1D0)
        AVLO2(NOP)=AVLO2I/MAX(DBLE(NLP2I),1D0)
      END IF

      END SUBROUTINE MATSTR

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     solve eign values & vectors of hamiltonian using lanctz method
C        coded by A.Tanaka (at Hiroshima Unv. 1995)
C
C Inputs
C     NGIN         :number of eign values (vectors) you need. 
C
C     H, IHP1,IHP2 :off-diagonal matrix elements and their subscripts 
C     HD           :diagonal part of the matrix 
C     NOP          :matrix number you want to diagonalize
C     NHOP,NSTM,NCIM,NCI,NSFT:addtional information on the matrix
C
C     NMODE  = 10 * NCPX + NEXL
C     NCPX         :0 real symmmetric, 1 hermite
C     NEXL         :0 normal mode, 1 extremely-large-size mode
C   
C Outputs 
C     UG(NBGM,NGM) :eign vectors
C     EG(NGM)       :eign values
C
C Temporary variables 
C     VP(NBSM), VZ(NBSM): vectors for temporary use 
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EIGN(NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &,NSTM,NCIM,NCI,NSFT
     &,NGM,NBGM,NGIN,UG,EG,NBSM,VZ,VP,NMODE)
      IMPLICIT NONE 
C
C global parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C local parameters
      INTEGER NLAN
      PARAMETER(NLAN=5000)
      REAL*8 EPS
      PARAMETER(EPS=1D-15)
C
C Dummy variables
      INTEGER NOP
C --Matrix elements
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NSTM, NCIM 
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
C
      INTEGER NGM, NBGM, NGIN
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)
   
      INTEGER NBSM
      REAL*8  VZ(NBSM),VP(NBSM)
      INTEGER NMODE
C
C local variables
      REAL*8  A(0:NLAN),B(0:NLAN)
      REAL*8  UL(NLAN,2)
      REAL*8  EL(2)
      INTEGER N,ISTM,NCPX,NEXL,NG
      INTEGER IG,IG2,IST,IDSTP,IE,ICI,IRET,IERR,NL,NN
      INTEGER I,J,K
      REAL*8  S,T,R,RR
C
C Main 
C
      WRITE(6,*)'enter EIGN'
      N=NSFT(NCI(NOP)+1,NOP)
      ISTM=2
      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
      WRITE(6,*)'NCPX=',NCPX,' NEXL=',NEXL
C
      NG=NGIN
      IF (NCPX.EQ.0) THEN
        NG=MIN(NG,N)
      ELSE
        NG=MIN(NG,N/2)
      END IF
      IF (NG.NE.NGIN) THEN
        WRITE(6,*)'Warning! The number of the eigen state NG=',NGIN
        WRITE(6,*)'is larger than the matrix size N=',NG
        NGIN=NG
      END IF

      IF (NG.GT.NGM) THEN
        WRITE(6,*)'Error! The number of the eigen state NG=',NGIN
        WRITE(6,*)' exceeds the maximum limit NGM=',NGM
        STOP
      ELSE IF(NGIN.EQ.0) THEN
        NG=1
        ISTM=1
      END IF
      IDSTP=5
      IF (N.LT.100) THEN 
        IDSTP=1
      ELSE IF (N.LT.500) THEN  
        IDSTP=2
      END IF
      DO IG=1,NG
      IRET=0 
  5   DO IST=1,ISTM
        IF (ISTM.EQ.1) THEN
          CALL RNDV(N,IG,VP)
          DO I=1,N
            VZ(I)=0D0
          END DO
        ELSE 
          IF (IST.EQ.1.AND.IRET.EQ.0) THEN
            CALL RNDV(N,IG,UG(1,IG))
          END IF
!IBM* ASSERT(NODEPS)
          DO I=1,N
            VP(I)=UG(I,IG)
            VZ(I)=0D0
          END DO
        END IF
        IF (IST.EQ.2) THEN
          DO I=1,N
            UG(I,IG)=0D0
          END DO
        END IF
        IF (IST.EQ.1) NL=NLAN
        IF (IST.EQ.2) NL=IE-1
        DO IE=1,NL
          DO IG2=1,IG-1
            S=0D0
            DO I=1,N
              S=S+VP(I)*UG(I,IG2)
            END DO
!IBM* ASSERT(NODEPS)
            DO I=1,N
              VP(I)=VP(I)-S*UG(I,IG2)
            END DO
            IF (NCPX.NE.0) THEN
              CALL EXCS(N,VP,UG(1,IG2),S)
              CALL EXCV(N,VP,UG(1,IG2),-S)
            END IF
          END DO !IG2
          IF (IG.EQ.N) GOTO 155 
C
          T=0D0
          DO I=1,N
            T=T+VP(I)**2
          END DO
          B(IE-1)=DSQRT(T)
C          WRITE(6,*)' IE,T=',IE,T
          T=0D0
C          IF (IE-1.NE.N) T=1D0/B(IE-1)
          IF (B(IE-1).EQ.0D0) THEN
            T=1D30
          ELSE
            T=1D0/B(IE-1)
          END IF
!IBM* ASSERT(NODEPS) 
          DO I=1,N
            R=B(IE-1)*VZ(I)
            S=T*VP(I)
            VZ(I)=S
            VP(I)=-R
          END DO
          IF (IST.EQ.2) THEN
            T=UL(IE,1)
!IBM* ASSERT(NODEPS)
            DO I=1,N
              UG(I,IG)=UG(I,IG)+VZ(I)*T
            END DO
          END IF
C
          IF (NEXL.EQ.0) THEN
!IBM* ASSERT(NODEPS)
            DO I=1,N
              VP(I)=VP(I)+HD(I)*VZ(I)
            END DO
          ELSE
            DO ICI=1,NCI(NOP)
              T=HD(ICI)
!IBM* ASSERT(NODEPS)
              DO I=NSFT(ICI,NOP)+1,NSFT(ICI+1,NOP)
                VP(I)=VP(I)+T*VZ(I)
              END DO
            END DO
          END IF
          CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
          CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C
          T=0D0
          DO I=1,N
            T=T+VP(I)*VZ(I)
          END DO
          A(IE)=T
!IBM* ASSERT(NODEPS)
          DO I=1,N
            VP(I)=VP(I)-A(IE)*VZ(I)
          END DO
          IF ((MOD(IE,IDSTP).EQ.0.OR.IE-1.EQ.N)
     &     .AND.IE.GE.3.AND.IST.EQ.1) THEN
            NN=IE-1
            CALL BISECT(NN,MIN(2,NN),NLAN,EL,UL,A(1),B(1),IERR)
            IF (IERR.NE.0) THEN
              WRITE(6,*)'Error in subroutine "BISECT"'
            END  IF 
            WRITE(6,'('' ITA='',I6,G26.16,G26.16,G16.8)')
     &           IE-1,EL(1),EL(2),B(IE-1)*ABS(UL(IE-1,1))
            IF (NGIN.EQ.0) THEN
              WRITE(2,'('' ITA='',I6,G26.16,G26.16,G16.8)')
     &             IE-1,EL(1),EL(2),B(IE-1)*ABS(UL(IE-1,1))
            END IF
            IF ( ABS( B(IE-1)*UL(IE-1,1) )
     &            .LT.  EPS*10D0*SQRT(DBLE(N)) ) GOTO 155
          END IF
        END DO !IE
        IF (IST.EQ.1) THEN
          WRITE(6,*)' DID NOT CONVERGE!!'
          WRITE(6,*)' NLAN MUST BE ENHANCED'
        END IF
 155    IF (IST.EQ.2.OR.IG.EQ.N) THEN
          IF ( IG .NE. N ) THEN
            WRITE(6,*)' ITA=',IE-1
            write(6,*)' 1st ',EL(1)
            write(6,*)' 2nd ',EL(2)
          ELSE
            DO I=1,N
              UG(I,N)=VP(I)
            END DO
          END IF
          T=0D0
          DO I=1,N
            T=T+UG(I,IG)**2
          END DO
          IF (IG.NE.N) WRITE(6,*)' |Y|=',DSQRT(T)
          T=1D0/DSQRT(T)
!IBM* ASSERT(NODEPS)
          DO I=1,N
            UG(I,IG)=T*UG(I,IG)
          END DO
          DO I=1,N
            VZ(I)=UG(I,IG)
          END DO
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          IF (NEXL.EQ.0) THEN
!IBM* ASSERT(NODEPS)
            DO I=1,N
              VP(I)=HD(I)*VZ(I)
            END DO
          ELSE
            DO ICI=1,NCI(NOP)
              T=HD(ICI)
!IBM* ASSERT(NODEPS)
              DO I=NSFT(ICI,NOP)+1,NSFT(ICI+1,NOP)
                VP(I)=T*VZ(I)
              END DO
            END DO
          END IF
          CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
          CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          T=0D0
          DO I=1,N
            T=T+VZ(I)*VP(I)
          END DO
          EG(IG)=T
          RR=0D0
          DO I=1,N
            RR=RR+(VP(I)-T*VZ(I))**2
          END DO
          R=DSQRT(RR)
          WRITE(6,'(''  #'',I6,'' E='',G30.16,'' ERROR='',G30.16)')
     &      IG,T,R
          IF (IRET.GT.10) THEN
            WRITE(6,*)'Warninig!! did not coverge in lanctzos method.'
          ELSE IF (R.GT.EPS*1D4*SQRT(DBLE(N))) THEN
            IRET=IRET+1
            IF (IG.NE.N) THEN
              GOTO 5
            ELSE
              GOTO 235
            END IF
          END IF
        END IF
      END DO !IST
      END DO !IG

 235  CONTINUE 
      IF (NCPX.NE.0) THEN
        DO IG=1,NG
          DO I=1,N
            VZ(I)=UG(I,IG)
          END DO
          CALL EXCV(N,VZ,UG(1,IG),1D0)
          T=0D0
          DO I=1,N
            T=T+VZ(I)**2
          END DO
          T=1D0/DSQRT(T)
          DO I=1,N
            UG(I,IG)=VZ(I)*T
          END DO
        END DO !IG
      END IF 
C========== sort
      IF (NG.GT.1) THEN
        R= EPS * 100D0 * SQRT( DBLE(N) )
        DO I=1,NG-1
          DO J=I+1,NG
            IF (EG(I).GT.EG(J)+R) THEN
              T=EG(I)
              EG(I)=EG(J)
              EG(J)=T
              DO K=1,N
                T=UG(K,I)
                UG(K,I)=UG(K,J)
                UG(K,J)=T
              END DO !K
            END IF
          END DO !J
        END DO !I
      END IF
C
      END SUBROUTINE EIGN
CCC
CCC
CCC    solve high-energy eigenvalues & eingenvectors of hamiltonian 
CCC    using lanctz method
CCC        coded by A.Tanaka (at Hiroshima Unv. 1995)
CCC
CCC Inputs
CCC     N            :the order of the matix
CCC     NG           :number of eign values (vectors) you need. 
CCC     NMAX         :the dimemsion of 1st subscript of UG
CCC
CCC     H, IHP1,IHP2 :off-diagonal matrix elements and their subscripts 
CCC     HD           :the diagonal part of the matrix 
CCC     ESFT         :a shift value of the diagonal elements 
CCC     NOP,NHOP,NSTM,NCIM,NCI,NSFT : addtional information on the matrix
CCC
CCC     NMODE  = 10 * NCPX + NEXL
CCC     NCPX         :0 real symmmetric, 1 hermite
CCC     NEXL         :0 normal mode, 1 extremely-large-size mode
CCC   
CCC Outputs 
CCC     UG(NMAX,NG)  :eigenvectors
CCC     EG(NG)       :eigenvalues
CCC
CCC Work areas   
CCC     VP(NMAX), VZ(NMAX), UG(NMAX,NG+1)
CCC
CC      SUBROUTINE EIGN2(NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ESFT,NSTM,NCIM,NCI,NSFT
CC     &,NGIN,UG,EG,VZ,VP,T1,NMODE)
CC      IMPLICIT REAL*8 (A-H,O-Z)
CC      PARAMETER(NBGM=100001,NCIM=13000
CC     &,NGMAX=20,NLAN=10000,EPS=1D-7)
CC      DIMENSION NHOP(0:*),H(*),HD(*),IHP1(*),IHP2(*)
CC     &,NCI(0:*),NSFT(NCIM+1,*)
CC     &,EG(*),VZ(*),VP(*),T1(*),EL(NLAN),UG(NBGM,*)
CC     &,A(0:NLAN),B(0:NLAN),UL(NLAN,2)
CCC       DIMENSION SD(NLAN),D(NLAN)
CCC
CC      WRITE(6,*)'enter EIGN2'
CC      NG=NGIN
CC      N=NSFT(NCI(NST)+1,NST)
CC      IF (N.GT.NBGM) THEN
CC        WRITE(6,*)' Too many basis functions' 
CC        WRITE(6,*)' The value of the NBGM should satisfy NBGM >=',N
CC        WRITE(6,*)' The present value of NBGM is ',NBGM
CC      END IF 
CC      ISTM=2
CC      NCPX=MOD(NMODE   ,10)
CC      NEXL=MOD(NMODE/10,10)
CC      WRITE(6,*)'NCPX=',NCPX,' NEXL=',NEXL
CCC
CC      IF (NG.GT.NGMAX) THEN
CC        WRITE(6,*)' TOO LARGE NG=',NG
CC        WRITE(6,*)' NGMAX MUST BE ENHANCED'
CC        WRITE(6,*)' EIGN2 requires NG<=NGMAX'
CC        STOP
CC      ELSE IF(NGIN.EQ.0) THEN
CC        NG=1
CC        ISTM=1
CC      END IF
CC      IDSTP=5
CC      IF (N.LT.100) THEN 
CC         IDSTP=1
CC      ELSE IF (N.LT.500) THEN  
CC         IDSTP=2
CC      END IF
CCC
CCC main loop
CCC
CC      DO IG=1,NG
CC        IRET=0 
CC  5     DO IST=1,ISTM
CC          IF (ISTM.EQ.1) THEN
CC            CALL RNDV(N,IG,VP)
CC            DO I=1,N
CC              VZ(I)=0D0
CC            END DO
CC          ELSE 
CC            IF (IST.EQ.1.AND.IRET.EQ.0) THEN
CC              CALL RNDV(N,IG,UG(1,IG))
CC            END IF
CC            DO I=1,N
CC              VP(I)=UG(I,IG)
CC              VZ(I)=0D0
CC            END DO
CC          END IF
CC          IF (IST.EQ.2) CALL CLRV(N,UG(1,IG))
CC          IF (IST.EQ.1) NL=NLAN
CC          IF (IST.EQ.2) NL=IE-1
CC          DO IE=1,NL
CC            DO IG2=1,IG-1
CC              CALL MULV(N,VP,UG(1,IG2),S)
CC              CALL MADS(N,VP,UG(1,IG2),-S)
CC              IF (NCPX.NE.0) THEN
CC                CALL EXCS(N,VP,UG(1,IG2),S)
CC                CALL EXCV(N,VP,UG(1,IG2),-S)
CC              END IF
CC            END DO 
CCC
CC            CALL ABSV(N,VP,B(IE-1))
CCC            IF (IE-1.NE.N) T=1D0/B(IE-1)
CC            IF (B(IE-1).LT.EPS) THEN
CC              T=1D30
CC            ELSE
CC              T=1D0/B(IE-1)
CC            END IF
CCC
CC            DO I=1,N
CC              R=B(IE-1)*VZ(I)
CC              S=T*VP(I)
CC              VZ(I)=S
CC              VP(I)=-R
CC            END DO
CC            IF (IST.EQ.2) CALL MADS(N,UG(1,IG),VZ,UL(IE,1)) 
CCC
CC            CALL MULM(N,T1,VZ,ESFT,NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
CC     &               ,NSTM,NCIM,NCI,NSFT,NMODE)
CC            CALL MADM(N,VP,T1,ESFT,NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
CC     &               ,NSTM,NCIM,NCI,NSFT,NMODE)
CCC
CC            CALL MULV(N,VP,VZ,A(IE))
CC            CALL MADS(N,VP,VZ,-A(IE))
CC            IF ((MOD(IE,IDSTP).EQ.0.OR.IE-1.EQ.N.OR.IE.EQ.NL)
CC     &          .AND.IE.GE.3.AND.IST.EQ.1) THEN
CC              NN=IE-1
CC              IF (NN.EQ.2) THEN
CC                S=SQRT((A(1)-A(2))**2+4D0*B(1)**2) 
CC                EL(1)=(A(1)+A(2)-S)/2D0
CC                EL(2)=(A(1)+A(2)+S)/2D0
CC                UL(1,1)=B(1)/SQRT(B(1)**2+(A(1)-EL(1))**2)
CC                UL(2,1)=-(A(1)-EL(1))/SQRT(B(1)**2+(A(1)-EL(1))**2)
CC              END IF
CC              CALL BISECT(NN,MIN(1,NN),NLAN,EL,UL,A(1),B(1),IERR)
CC              IF (IERR.NE.0)THEN
CC                WRITE(6,*)'Error in subroutine "BISECT"'
CC              END  IF 
CC              WRITE(6,'('' ITA='',I6,G26.16,G26.16,G16.8)')
CC     &             IE-1,EL(1),EL(2),B(IE-1)*ABS(UL(IE-1,1))
CC              IF (NGIN.EQ.0) THEN
CC                WRITE(2,'('' ITA='',I6,G26.16,G26.16,G16.8)')
CC     &               IE-1,EL(1),EL(2),B(IE-1)*ABS(UL(IE-1,1))
CC              END IF
CC              IF (ABS(B(IE-1)*UL(IE-1,1)).LT.EPS) GOTO 155
CC            END IF
CC          END DO
CC          IF (IST.EQ.1) THEN
CC            WRITE(6,*)' DID NOT CONVERGE!!'
CC            WRITE(6,*)' NLAN MUST BE ENHANCED'
CC          END IF
CC 155      IF (IST.EQ.2) THEN
CC            WRITE(6,*)' ITA=',IE-1
CC            write(6,*)' 1st ',EL(1)
CC            write(6,*)' 2nd ',EL(2)
CC            CALL ABSV(N,UG(1,IG),T)
CC            WRITE(6,*)' |Y|=',T
CC            CALL MULS(N,UG(1,IG),1D0/T)
CC            CALL MOVV(N,VZ,UG(1,IG))
CC            CALL MULM(N,VP,VZ,0D0,NST,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
CC     &               ,NSTM,NCIM,NCI,NSFT,NMODE)
CC            CALL MULV(N,VZ,VP,T)
CC            EG(IG)=T
CC            RR=0D0
CC            DO I=1,N
CC              RR=RR+(VP(I)-T*VZ(I))**2
CC            END DO
CC            R=DSQRT(RR)
CC            WRITE(6,'(''  #'',I6,'' E='',G30.16,'' ERROR='',G30.16)')
CC     &        IG,T,R
CC            RETURN
CC            IF (IRET.GT.1000000) THEN
CC            WRITE(6,*)'Warninig!! did not coverge in lanctzos method.'
CC            ELSE IF (R.GT.10*EPS) THEN
CC              IRET=IRET+1
CC              GOTO 5
CC            END IF
CC          END IF
CC        END DO
CC      END DO
CCC
CCC The renormarization of the eignvectors
CCC
CC      IF (NCPX.NE.0) THEN
CC        DO IG=1,NG
CC          CALL MOVV(N,VZ,UG(1,IG))
CC          CALL EXCV(N,VZ,UG(1,IG),1D0)
CC          CALL ABSV(N,VZ,T)
CC          CALL MULV(N,UG(1,IG),VZ,1D0/T)
CC        END DO
CC      END IF 
CC      END
C
C 
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  diagonalize the tri-diagonal matrix by bisection method.
C      coded by A. Tanaka  (at Hiroshima Unv. 1995)
C
C  input  
C         integer  N           :  order of the matrix
C         integer  NE          :  number of eigen values (vectors) 
C                                 which you need.
C         integer  NMAX        :  dimension for 1st subscript
C                                 of the array U  
C         real*8   A(N)        :  diagonal elements  
C         real*8   B(N)        :  sub-diagonal elements (1 to N-1)
C  output 
C         real*8   E(NE)       :  eigen values
C         real*8   U(NMAX,NE)  :  eigen vectors
C         integer  IERR        :  error value (no errors IERR=0 ) 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION RLIM(X,R)
      IMPLICIT NONE

      REAL*8    EPS_BS
      PARAMETER(EPS_BS=1D-13)

      REAL*8 RLIM,X,R

      RLIM=SIGN( MAX( ABS(X), R*EPS_BS ), X )

      END 

      SUBROUTINE BISECT(N,NEIN,NMAX,E,U,A,B,IERR)
      IMPLICIT NONE

C Parameters
      INTEGER NLAN
      PARAMETER(NLAN=10000)
      REAL*8    EPS_BS
      PARAMETER(EPS_BS=1D-13)

C Dummy arguments
      INTEGER N, NEIN, NMAX
      REAL*8  E(*)
      REAL*8  U(NMAX,*)
      REAL*8  A(N), B(N)
      INTEGER IERR

      REAL*8  Y(NLAN), R1(NLAN), R2(NLAN), R3(NLAN), RM(NLAN)
      INTEGER IEXCG(NLAN)

C Local variables
      INTEGER I, J, K, IE, JE, ICNT, NE, NEG, IS
      REAL*8 EMIN, EMAX, EMDL, EMDLB, EMIN0, EMAX0, GMIN, GMAX, GR, GI
      REAL*8 S, T, R, EPSR

C Function
      REAL*8 RLIM, FRAND
C ----
      IERR=0
      NE=ABS(NEIN)
      IF (N.GT.NMAX) THEN
        WRITE(6,*)'Error in "BISECT" too large N or NMAX'
        IERR=1
        STOP
      ELSE IF(NE.GT.N) THEN
        WRITE(6,*)'Error in "BISECT" too large  NE ',NE
        IERR=1
        STOP
      ELSE IF(N.EQ.1) THEN
        E(1)=A(1)
        U(1,1)=1D0
        RETURN
      END IF
C
      R=MAX(ABS(A(1))+ABS(B(1)),ABS(B(N-1))+ABS(A(N)))
      DO I=2,N-1
        R=MAX(R,ABS(B(I-1))+ABS(A(I))+ABS(B(I)))
      END DO
C====== bisection method
      IE=N
      EMAX0= 2D0*R
      EMIN0=-2D0*R
   20 CONTINUE
        EMIN=EMIN0
        EMAX=EMAX0
        GMIN=0D0
        GMAX=0D0
        IF (R.LT.EPS_BS) R=EPS_BS
        EPSR=EPS_BS*R
        JE=0
        ICNT=0
   30   CONTINUE
          EMDL=0.5D0*(EMAX+EMIN)
          NEG=0
          K=1
          GR=A(1)-EMDL
          GI=EPSR
   40     CONTINUE 
            IF (GR.GT.0D0) NEG=NEG+1
            K=K+1
            T =B(K-1)**2/(GR**2+GI**2)
            GR=A(K)-EMDL-T*GR
            GI=EPSR+T*GI
          IF (K.LT.N) GOTO 40
          IF (GR.GT.0D0) NEG=NEG+1
C
          IF (NEIN.GT.0) THEN
            IF (NEG.LT.IE) THEN
              EMAX=EMDL
              GMAX=GR
              JE=MAX(JE,NEG)
            ELSE
              EMIN=EMDL
              GMIN=GR
            END IF
          ELSE
            IF (N-NEG.LT.IE) THEN
              EMIN=EMDL
              GMIN=GR
              JE=MAX(JE,N-NEG)
            ELSE
              EMAX=EMDL
              GMAX=GR
            END IF
          END IF
          ICNT=ICNT+1
        IF (DABS(EMAX-EMIN).GT.R*EPS_BS.AND.ICNT.LE.100) GOTO 30
        IF (GMAX*GMIN.LT.0D0) EMDL=EMIN-GMIN/(GMAX-GMIN)*(EMAX-EMIN)
        IF (ICNT.GE.100) THEN
          WRITE(6,*)' Warning!! Did not converge in bisection method'
          WRITE(6,*)'   after 100 itaration (SUB. BISECT).'
          WRITE(6,*)' E(',N-IE+1,')=',EMDL,' Ediff=',EMAX-EMIN
          IERR=N-IE+1
        END IF
        JE=MAX(N-NE,JE)
C        WRITE(6,*)' JE=',JE,' IE=',IE
C======= inverse itaration
C
        DO J=IE,JE+1,-1
          DO I=1,N
            Y(I)=FRAND()-0.5D0
          END DO
C
          ICNT=0
   70     CONTINUE
*SOPTION NOUNROLL
*VOPTION NOVEC
            DO IS=J+1,N
              IF (ABS(E(N-IS+1)-EMDL).GT.R*EPS_BS*1D5) GOTO 77
            END DO
   77       CONTINUE
            IS=IS-1
            DO K=IS,J+1,-1
              T=0D0
              DO I=1,N
                T=T+U(I,N-K+1)*Y(I)
              END DO
              DO I=1,N
                Y(I)=Y(I)-U(I,N-K+1)*T
              END DO
            END DO
C
            R1(1)=RLIM(A(1)-EMDL,R)
            R2(1)=B(1)
            R3(1)=0D0
*SOPTION NOUNROLL
*VOPTION NOVEC
            DO I=1,N-1
              IF (ABS(R1(I)).LT.ABS(B(I))) THEN
                IEXCG(I)=-1
                T=R1(I)/RLIM(B(I),R)
                R1(I+1)=RLIM(R2(I)-T*(A(I+1)-EMDL),R)
                IF (I.LE.N-2) R2(I+1)=R3(I)-T*B(I+1)
                R3(I+1)=0D0
                R1(I)=RLIM(B(I),R)
                R2(I)=A(I+1)-EMDL
                IF (I.LE.N-2) R3(I)=B(I+1)
                RM(I)=T
              ELSE
                IEXCG(I)=0
                T=B(I)/R1(I)
                R1(I+1)=RLIM(A(I+1)-EMDL-T*R2(I),R)
                IF (I.LE.N-2) R2(I+1)=B(I+1)-T*R3(I)
                R3(I+1)=0D0
                RM(I)=T
              END IF
            END DO
C
            IF (ICNT.GT.0) THEN
*SOPTION NOUNROLL
*VOPTION NOVEC
              DO I=1,N-1
                IF (IEXCG(I).NE.0) THEN
                  T=Y(I)
                  Y(I)=Y(I+1)
                  Y(I+1)=T-RM(I)*Y(I+1)
                ELSE
                  Y(I+1)=Y(I+1)-RM(I)*Y(I)
                END IF
              END DO
            END IF
C
            Y(N)=Y(N)/R1(N)
            Y(N-1)=(Y(N-1)-R2(N-1)*Y(N))/R1(N-1)
*SOPTION NOUNROLL
*VOPTION NOVEC
            DO I=N-2,1,-1
              Y(I)=(Y(I)-R2(I)*Y(I+1)-R3(I)*Y(I+2))/R1(I)
            END DO
C
            T=0D0
            DO I=1,N
              T=T+Y(I)**2
            END DO
            T=1D0/SQRT(T)
            DO I=1,N
              Y(I)=Y(I)*T
            END DO
C
            EMDLB=EMDL
            EMDL=Y(1)*(A( 1 )*Y( 1 )+B(1)*Y(2))
     &          +Y(N)*(B(N-1)*Y(N-1)+A(N)*Y(N))
*SOPTION NOUNROLL
*VOPTION NOVEC
            DO I=2,N-1
              EMDL=EMDL+Y(I)*(B(I-1)*Y(I-1)+A(I)*Y(I)+B(I)*Y(I+1))
            END DO

            S=ABS(EMDL*Y(1)-(A(1)  *Y(1)  +B(1)*Y(2)))
     &       +ABS(EMDL*Y(N)-(B(N-1)*Y(N-1)+A(N)*Y(N)))
*SOPTION NOUNROLL
*VOPTION NOVEC
            DO I=2,N-1
              S=S+ABS(EMDL*Y(I)-(B(I-1)*Y(I-1)+A(I)*Y(I)+B(I)*Y(I+1)))
            END DO

            ICNT=ICNT+1
          IF( ( S+ABS(EMDL-EMDLB) .GT. R*EPS_BS*10D0 .AND. ICNT.LT.5 )
     &        .OR. ICNT.LT.2 ) GOTO 70
          IF (ICNT.EQ.5) THEN
            WRITE(6,*)' Warning!! Did not converge in inverse itaration'
            WRITE(6,*)'  after 5 itarations (SUB. BISECT).'
            WRITE(6,*)' E(',N-J+1,')=',EMDL,' RERR=',S/RLIM(EMDL,R)
            IERR=N-J+1
          END IF
C=====> renormalization & reorthogonalization 
          E(N-J+1)=EMDL 
*SOPTION NOUNROLL
*VOPTION NOVEC
          DO IS=J+1,N
            IF ( ABS(E(N-IS+1)-EMDL) .GT. R*EPS_BS*1D5 ) EXIT 
          END DO
          IS=IS-1
          DO K=IS,J+1,-1
            T=0D0
            DO I=1,N
              T=T+U(I,N-K+1)*Y(I)
            END DO
            DO I=1,N
              Y(I)=Y(I)-U(I,N-K+1)*T
            END DO
          END DO
          T=0D0
          DO I=1,N
            T=T+Y(I)**2
          END DO
          IF (SQRT(T).LT.EPS_BS*100d0) THEN
            WRITE(6,*)'Warning!! Did not converge (SUB. BISECTION).'  
            IF (NEIN.GT.0) THEN
              EMIN0=EMDL
            ELSE
              EMAX0=EMDL
            END IF
            JE=JE+IE-J+1
            GOTO 195
          END IF   
          T=1D0/SQRT(T)
          DO I=1,N
            U(I,N-J+1)=Y(I)*T
          END DO
        END DO 
  195   IE=JE 
      IF (IE.GE.N-NE+1) GOTO 20
      END SUBROUTINE BISECT
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    QUASI-RANDAM VECTOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RNDV(N,ISFT,V)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N,ISFT
      REAL*8  V(N)

C Local variable
      REAL*8  RT2, T
      INTEGER I, J
C ------
      RT2=SQRT(2D0)
      J=N*(ISFT-1)
      DO I=1,N
        T=DBLE(I+J)*RT2
        V(I)=T-NINT(T)
      END DO

      END SUBROUTINE RNDV

C+++++++++++++++++++++++++++++++++++++++++
C     Generate uniform random number
C     using congruential method
C+++++++++++++++++++++++++++++++++++++++++
      FUNCTION FRAND()
      IMPLICIT NONE
      REAL*8 FRAND
C Parameter
      REAL*8 RNRM
      PARAMETER(RNRM=1D0/2147483647D0)
      INTEGER I
      SAVE I
      DATA I/1234567/
C
      I=I*48828125
      IF (I.LT.0) I=(I+2147483647)+1
      FRAND=RNRM*DBLE(I)
      END FUNCTION FRAND
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Reduce a complex symmetric matrix (Harmiltonian)
C    to  tri-diagonal matrix by  the Lanctzos method
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LANCZ(N,VP,VZ,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                ,NREC,NR,A,B,NSTM,NCIM,NCI,NSFT,NMODE)
      IMPLICIT NONE 

C Parameters

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NRECM
      PARAMETER(NRECM=2000)

C Dummy arguments
      INTEGER N
      REAL*8  VP(N),VZ(N)
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NREC,NR
      REAL*8  A(0:NRECM),B(0:NRECM)
C
      INTEGER NSTM, NCIM 
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NMODE

C Local variables
      INTEGER NCPX,NEXL
      INTEGER I,IE,ICI
      REAL*8  S,T,R,RR
C ---------------

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C      WRITE(6,*)'enter LANCZ NEXL=',NEXL,' NCPX=',NCPX
      RR=0D0
      DO I=1,N
        VZ(I)=0D0
        RR=RR+VP(I)**2
      END DO
      IF(SQRT(RR).LT.1D-5) THEN
        B(0)=0D0
        WRITE(6,*)'  initial vec. is zero !!!'
        NR=0
        RETURN
      END IF
      DO IE=1,NREC
        T=0D0
        DO I=1,N
          T=T+VP(I)*VP(I)
        END DO
        B(IE-1)=DSQRT(T)
        IF (B(IE-1).GT.1.0E-30)  THEN
          T=1D0/B(IE-1)
        ELSE
          WRITE(6,*)'  very small b(i) at i=',IE
          NR=IE-1
          RETURN
        ENDIF
        IF (NEXL.EQ.0) THEN
!IBM* ASSERT (NODEPS) 
          DO I=1,N
            R=B(IE-1)*VZ(I)
            S=T*VP(I)
            VZ(I)=S
            VP(I)=-R
          END DO
!IBM* ASSERT (NODEPS)
          DO I=1,N
            VP(I)=VP(I)+HD(I)*VZ(I)
          END DO
        ELSE
!IBM* ASSERT (NODEPS) 
          DO I=1,N
            R=B(IE-1)*VZ(I)
            S=T*VP(I)
            VZ(I)=S
            VP(I)=-R
          END DO
          DO ICI=1,NCI(NOP)
            T=HD(ICI)
!IBM* ASSERT (NODEPS)
            DO I=NSFT(ICI,NOP)+1,NSFT(ICI+1,NOP)
              VP(I)=VP(I)+T*VZ(I)
            END DO
          END DO
        END IF
        CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VP,N,VZ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
        T=0D0
        DO I=1,N
          T=T+VP(I)*VZ(I)
        END DO
        A(IE)=T
!IBM* ASSERT (NODEPS)
        DO I=1,N
          VP(I)=VP(I)-A(IE)*VZ(I)
        END DO
      END DO
      NR=NREC

      END SUBROUTINE LANCZ

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Write spectural weight to the file with unit number IUNIT 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SPOUT(ISHL,SPD,NSTM,RANGE)
      IMPLICIT NONE

C Parameter 
      INTEGER   NPMAX
      PARAMETER(NPMAX=4000)

      INTEGER   IUOUT
      PARAMETER(IUOUT=2)

C Dummy arguments

      INTEGER IUNIT
      INTEGER ISHL
      REAL*8  SPD(0:NPMAX)
      INTEGER NSTM 

      REAL*8  RANGE(4,NSTM) 

C Local variable 
      INTEGER I, NPX
      REAL*8  EMIN, EMAX, GAM2
C --------

      NPX =NINT(RANGE(1,ISHL))
      EMIN=     RANGE(2,ISHL)
      EMAX=     RANGE(3,ISHL)
      GAM2=     RANGE(4,ISHL)
      
      WRITE(IUOUT,*)'SPCD'
      WRITE(IUOUT,'(I5,3E15.7)') NPX, EMIN, EMAX, GAM2
      WRITE(IUOUT,'(5E15.7/)')(REAL(SPD(I)),I=0,NPX)

      END SUBROUTINE SPOUT
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     recursion method for constant life time 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RECUR(ISHL,SPR,SPI,E,NSTM,RANGE,NREC,A,B)
      IMPLICIT NONE

C Parameters
      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

C Dummy arguments
      INTEGER ISHL 
      REAL*8  SPR(0:NPMAX), SPI(0:NPMAX)
      REAL*8  E

      INTEGER NSTM 
      REAL*8  RANGE(4,NSTM) 

      INTEGER NREC
      REAL*8  A(0:NRECM), B(0:NRECM)

C Local variables
      INTEGER I, IR
      REAL*8  DE, T
      INTEGER NPX
      REAL*8  EMIN, EMAX, GAM
C -------------

      NPX =NINT(RANGE(1,ISHL))
      EMIN=     RANGE(2,ISHL)
      EMAX=     RANGE(3,ISHL)
      GAM =     RANGE(4,ISHL)

      DO I=0,NPX
        SPR(I)=0D0
        SPI(I)=0D0
      END DO
      IF (NREC.EQ.0) RETURN 
C
      DE=(EMAX-EMIN)/DBLE(NPX)
      DO IR=NREC,1,-1
        DO I=0,NPX
          SPR(I)=DBLE(I)*DE+EMIN+E-A(IR)-SPR(I)
          SPI(I)=-GAM-SPI(I)
        END DO
        DO I=0,NPX
          T=B(IR-1)**2/(SPR(I)**2+SPI(I)**2)
          SPR(I)=T*SPR(I)
          SPI(I)=-T*SPI(I)
        END DO
      END DO
C
      END SUBROUTINE RECUR

C>>>START RESONANCE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Reduce a complex symmetric matrix (Harmiltonian + Self-energy term)
C    to  tri-diagonal matrix by  the Lanctzos method
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LANCZ2(N,VPR,VPI,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NREC,NR,AR,AI,BR,BI
     &,VZR,VZI,HDD)

      IMPLICIT NONE 

C Parameters

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

C Dummy arguments
      INTEGER N
      REAL*8  VPR(N),VPI(N)
      INTEGER NOP
C
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
C
      REAL*8  GALF

      INTEGER NF(*)
      INTEGER NOP2, NSHL
      INTEGER NDEG(NOBTM)
      INTEGER NOBT, NOBTX, ISTS
C
      INTEGER NREC, NR
      REAL*8  AR(0:NRECM), AI(0:NRECM), BR(0:NRECM), BI(0:NRECM)
C
      REAL*8  VZR(N), VZI(N), HDD(N)

C Local variables
      INTEGER I,IE
      REAL*8  SR,SI,TR,TI,RR,RI
C ---------

C      WRITE(6,*)'enter LANCZ2'
      CALL MOVV(N,HDD,HD)
      CALL CLRV(N,VZR)
      CALL CLRV(N,VZI)
      CALL ABSV(N,VPR,RR)
      CALL ABSV(N,VPI,RI)
      RR=(RR+RI)*(RR-RI)
      CALL MULV(N,VPR,VPI,RI)
      RI=2D0*RI
      IF(SQRT(RR**2+RI**2).LT.1D-5) THEN
        BR(0)=0D0
        BI(0)=0D0
        NR=0
        WRITE(6,*)' the initial vector is zero !!!'
        RETURN
      END IF
      DO IE=1,NREC
        CALL ABSV(N,VPR,RR)
        CALL ABSV(N,VPI,RI)
        RR=(RR+RI)*(RR-RI)
        CALL MULV(N,VPR,VPI,RI)
        RI=2D0*RI
        SI=ATAN2(RI,RR)
        SR=SQRT(RR**2+RI**2)
        BR(IE-1)=COS(0.5D0*SI)*SQRT(SR)
        BI(IE-1)=SIN(0.5D0*SI)*SQRT(SR)
        IF (SR.GT.1D-10) THEN
          TR= COS(-0.5D0*SI)/SQRT(SR)
          TI= SIN(-0.5D0*SI)/SQRT(SR)
        ELSE
          WRITE(6,*)'  very small b(i) at i=',IE
          NR=IE-1
          CALL MOVV(N,HD,HDD)
          RETURN
        ENDIF
        DO I=1,N
          RR=BR(IE-1)*VZR(I)-BI(IE-1)*VZI(I)
          RI=BR(IE-1)*VZI(I)+BI(IE-1)*VZR(I)
          SR=TR      *VPR(I)-TI      *VPI(I)
          SI=TR      *VPI(I)+TI      *VPR(I)
          VZR(I)= SR
          VZI(I)= SI
          VPR(I)=-RR
          VPI(I)=-RI
        END DO
C        WRITE(6,*)'before madcm2'
        CALL MADCM2(N,VPR,VPI,VZR,VZI,0D0,0D0,NOP
     &             ,NHOP,NHM,H,IHP1,IHP2,N,HDD
     &             ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
        RR=0D0
        RI=0D0
        DO I=1,N
          RR=RR+VZR(I)*VPR(I)-VZI(I)*VPI(I)
          RI=RI+VZR(I)*VPI(I)+VZI(I)*VPR(I)
        END DO
        AR(IE)=RR
        AI(IE)=RI
C        WRITE(6,*)'ie ar, ai & b',IE,AR(IE),AI(IE),BR(IE-1),BI(IE-1)
        CALL MADCV2(N,VPR,VPI,-RR,-RI,VZR,VZI)
      END DO
      CALL MOVV(N,HD,HDD)
      NR=NREC

      END SUBROUTINE LANCZ2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Recursion method for term dependent life time 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE RECUR2(ISHL,SPR,SPI,E,NSTM,RANGE,NREC,AR,AI,BR,BI)
      IMPLICIT NONE

C Parameters
      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

C Dummy arguments
      INTEGER ISHL
      REAL*8  SPR(0:NPMAX),SPI(0:NPMAX)
      REAL*8  E

      INTEGER NSTM 
      REAL*8  RANGE(4,NSTM) 

      INTEGER NREC
      REAL*8  AR(0:NRECM), AI(0:NRECM), BR(0:NRECM), BI(0:NRECM)

C Local variables
      INTEGER I,IR
      REAL*8  DE,RR,RI,TR,TI

      INTEGER NPX
      REAL*8  EMIN, EMAX, GAM
C -------------

      NPX =NINT(RANGE(1,ISHL))
      EMIN=     RANGE(2,ISHL)
      EMAX=     RANGE(3,ISHL)
      GAM =     RANGE(4,ISHL)

      DO I=0,NPX
        SPR(I)=0D0
        SPI(I)=0D0
      END DO
C
      IF (NPX.GT.0) THEN
        DE=(EMAX-EMIN)/DBLE(NPX)
      ELSE
        DE=0D0
      END IF
C
      DO IR=NREC,1,-1
        DO I=0,NPX
          SPR(I)=DBLE(I)*DE+EMIN+E-AR(IR)-SPR(I)
          SPI(I)=-(-AI(IR))-GAM-SPI(I)
        END DO
        RR=(BR(IR-1)+(-BI(IR-1)))*(BR(IR-1)-(-BI(IR-1)))
        RI=2D0*BR(IR-1)*(-BI(IR-1))
        DO I=0,NPX
          TR=SPR(I)
          TI=SPI(I)
          SPR(I)=( RR*TR+RI*TI)/(TR**2+TI**2)
          SPI(I)=(-RR*TI+RI*TR)/(TR**2+TI**2)
        END DO
      END DO

      END SUBROUTINE RECUR2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Calculate Matrix inversion 1/{H-Er-i(Gamma+Ei)}
C     using conjugate gradient method
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE INVM(N,XR,XI,NOP
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,ER,EI,GALF
     &               ,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NMODE
     &               ,NBMM,TR,TI,HPR,HPI,HDD,RR,RI,PR,PI,DG,BR,BI)
      IMPLICIT NONE

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   ITAMAX
      PARAMETER(ITAMAX=10000)

      REAL*8    EPS,     GAM1
      PARAMETER(EPS=1D-7,GAM1=1D0)

C Dummy arguments 
      INTEGER N
      REAL*8  XR(N),XI(N)

      INTEGER NOP
 
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  ER, EI, GALF 
  
      INTEGER NOP2, NSHL
      INTEGER NF(NSHL)
      INTEGER NDEG(NOBTM)
      INTEGER NOBT, NOBTX, ISTS
      INTEGER NMODE

C Global variables
      INTEGER NBMM
      REAL*8  TR(NBMM),TI(NBMM),HPR(NBMM),HPI(NBMM),HDD(NBMM)
      REAL*8  RR(NBMM),RI(NBMM),PR(NBMM),PI(NBMM),DG(NBMM)
      REAL*8  BR(NBMM),BI(NBMM)

C Local variables
Cvvvvvvvvvvvvvvvv
C^^^^^^^^^^^^^^^
      INTEGER I, ITA, ISHL, NCPX, NEXL
      REAL*8 S,T, TTR, TTI, RERR, RERRB, RERRT
      REAL*8 ALFR, ALFI, ATR, ABR, ATI, ABI
      REAL*8 BBR, BBI, BTR, BTI, BTAR, BTAI
C ------------------------

      WRITE(6,*)'enter INVM'

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
      IF (N.GT.NBMM) THEN
        WRITE(6,*)'Too many basis in intermediate state ',N
        WRITE(6,*)'The parameter "NBMM" must be enhanced.'
        STOP
      END IF
      IF (NEXL.NE.0) THEN
        WRITE(6,*)'EXL mode can not used for the second order'
        WRITE(6,*)' optical process. Xtls will be stoped.'
        STOP
      END IF
C
      CALL CLRV(N,DG)
      CALL HDG2(N,DG,NOP,NHOP,NHM,H,IHP1,IHP2)
      CALL HDG2(N,DG,NOP,NHOP,NHM,H,IHP2,IHP1)
      
      DO I=1,N
        DG(I)=1D0/DSQRT(DG(I)+(HD(I)-ER)**2+GAM1**2)
      END DO
Cvvvvvvvvvvvvvvvvvvv
      CALL MOVV(N,BR,XR)
      CALL MOVV(N,BI,XI)
C^^^^^^^^^^^^^^^^^^^
C
      CALL MOVV(N,HDD,HD)

      DO I=1,N
        HPR(I)=XR(I)
        HPI(I)=XI(I)
      END DO
      DO I=1,N
        T=DG(I)*DG(I)
        XR(I)=XR(I)*T
        XI(I)=XI(I)*T
      END DO
C
      ITA=0
      RERRB=0D0
      DO I=1,N
        RERRB=RERRB+(HPR(I)**2+HPI(I)**2)*DG(I)**2
      END DO
      WRITE(6,*)'RERRB=',RERRB
      IF ( SQRT(RERRB) .LT. 1D-12 ) THEN
        WRITE(6,*)' Size zero!! |B|=',SQRT(RERRB)
        RETURN
      END IF
      CALL MULS(N,XI,-1D0)

      CALL MULCM2(N,TR,TI,XR,XI,ER,EI,NOP
     &           ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &           ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
      CALL MULS(N,XI,-1D0)
      CALL MULS(N,TI,-1D0)
      CALL MULCM2(N,RR,RI,TR,TI,ER,EI,NOP
     &           ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &           ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
      DO I=1,N
        TTR=DG(I)*(HPR(I)-RR(I))
        TTI=DG(I)*(HPI(I)-RI(I))
        RR (I)=TTR
        RI (I)=TTI
        PR (I)=TTR
        PI (I)=TTI
        XR(I)=XR(I)/DG(I)
        XI(I)=XI(I)/DG(I)
      END DO
      DO !ITA LOOP
        RERRT=0D0
        DO I=1,N
          RERRT=RERRT+RR(I)**2+RI(I)**2
        END DO
        RERR=DSQRT(RERRT/(2*N*RERRB))
        IF ((ITA.GT.ITAMAX).OR.(RERR.LT.EPS)) EXIT !ITA LOOP
        DO I=1,N
          HPR(I)=DG(I)*PR(I)
          HPI(I)=DG(I)*PI(I)
        END DO
        CALL MULS(N,HPI,-1D0)
        CALL MULCM2(N,TR,TI,HPR,HPI,ER,EI,NOP
     &             ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &             ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
        CALL MULS(N,TI,-1D0)
        CALL MULCM2(N,HPR,HPI,TR,TI,ER,EI,NOP
     &             ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &             ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
        DO I=1,N
          HPR(I)=DG(I)*HPR(I)
          HPI(I)=DG(I)*HPI(I)
        END DO
        CALL MULV(N,PR, RR,ATR)
        CALL MULV(N,PR,HPR,ABR)
        CALL MULV(N,PI, RI,ATI)
        CALL MULV(N,PI,HPI,ABI)

        IF (ISTS.EQ.1) THEN
          ALFR=ATR/ABR
          IF (ABI.EQ.0D0) THEN
            ALFI=0D0
          ELSE
            ALFI=ATI/ABI
          END IF
        ELSE
          ALFR=(ATR+ATI)/(ABR+ABI)
          ALFI=ALFR
        END IF
        DO I=1,N
          XR(I)=XR(I)+ALFR*PR(I)
          XI(I)=XI(I)+ALFI*PI(I)
        END DO
        DO I=1,N
          RR(I)=RR(I)-ALFR*HPR(I)
          RI(I)=RI(I)-ALFI*HPI(I)
        END DO
        BBR=ABR
        BBI=ABI
        CALL MULV(N,RR,HPR,BTR)
        CALL MULV(N,RI,HPI,BTI)
        WRITE(6,*)'BBR=',BBR,' BBI=',BBI
        IF (ISTS.EQ.1) THEN
          BTAR=BTR/BBR
          IF (BBI.EQ.0D0) THEN
            BTAI=0D0
          ELSE
            BTAI=BTI/BBI
          END IF
        ELSE
          BTAR=(BTR+BTI)/(BBR+BBI)
          BTAI=BTAR
        END IF
        DO I=1,N
          PR(I)=RR(I)-BTAR*PR(I)
          PI(I)=RI(I)-BTAI*PI(I)
        END DO
        ITA=ITA+1
        WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR
      END DO !ITA LOOP
      WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR
      DO I=1,N
        XR(I)=XR(I)*DG(I)
        XI(I)=XI(I)*DG(I)
      END DO
      CALL MOVV(N,TR,XR)
      CALL MOVV(N,TI,XI)
       CALL MULS(N,TI,-1D0)
      CALL MULCM2(N,XR,XI,TR,TI,ER,EI,NOP
     &           ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &           ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
      CALL MULS(N,XI,-1D0)
Cvvvvvvvvvvvvvvvvvvvvvvvvvvv
      CALL MULCM2(N,TR,TI,XR,XI,ER,EI,NOP
     &           ,NHOP,NHM,H,IHP1,IHP2,NBMM,HDD
     &           ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NHDM,HD)
      RERRT=0D0
      S=0D0
      DO I=1,N
        RERRT=RERRT+(TR(I)-BR(I))**2+(TI(I)-BI(I))**2
        S=S+TR(I)**2+TI(I)**2
      END DO
      RERR=DSQRT(RERRT/(2*N*RERRB))
      WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR,DSQRT(RERRT/(2*N*S))
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      CALL MOVV(N,HD,HDD)

      END SUBROUTINE INVM

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Calculate Matrix inversion 1/{H-Er-i(Gamma+Ei)}
C     using conjugate gradient method
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      SUBROUTINE MODINVM(N,XR,XI,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
C     &,ER,EI,GALF
C     &,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NMODE)
C      IMPLICIT REAL*8 (A-H,O-Z)
C      PARAMETER(NBMM=1,ITAMAX=NBMM,NCIM=13000)
C      PARAMETER(EPS=1D-7,GAM1=1D0)
C      DIMENSION XR(N),XI(N)
C     &,NHOP(0:*),H(N),HD(N),IHP1(*),IHP2(*),NDEG(*),NF(*)
C      DIMENSION TR(NBMM),TI(NBMM),HPR(NBMM),HPI(NBMM),HDD(NBMM)
C     &,RR(NBMM),RI(NBMM),PR(NBMM),PI(NBMM),DG(NBMM)
C      COMMON /IMWRK/TR,TI,HPR,HPI,HDD,RR,RI,PR,PI,DG
CCvvvvvvvvvvvvvvvv
C      DIMENSION BR(NBMM),BI(NBMM)
C      WRITE(6,*)'enter INVM'
CC^^^^^^^^^^^^^^^
CC
C      NCPX=MOD(NMODE   ,10)
C      NEXL=MOD(NMODE/10,10)
C      IF (N.GT.NBMM) THEN
C        WRITE(6,*)'Too many basis in intermediate state ',N
C        WRITE(6,*)'The parameter "NBMM" must be enhanced.'
C        STOP
C      END IF
C      IF (NEXL.NE.0) THEN
C        WRITE(6,*)'EXL mode can not used for the second order'
C        WRITE(6,*)' optical process. Xtls will be stoped.'
C        STOP
C      END IF
CC
C      CALL CLRV(N,DG)
C      CALL HDG2(N,DG,NOP,NHOP,NHM,H,IHP1,IHP2)
C      CALL HDG2(N,DG,NOP,NHOP,NHM,H,IHP2,IHP1)
CC      
C      DO I=1,N
C        DG(I)=1D0/DSQRT(DG(I)+(HD(I)-ER)**2+GAM1**2)
C      END DO
CCvvvvvvvvvvvvvvvvvvv
C      CALL MOVV(N,BR,XR)
CC      CALL MOVV(N,BI,XI)
CC^^^^^^^^^^^^^^^^^^^
CC
C      CALL MOVV(N,HDD,HD)
C      DO I=1,N
C        HPR(I)=XR(I)
C        HPI(I)=XI(I)
C      END DO
CC      DO I=1,N
C        T=DG(I)*DG(I)
C        XR(I)=XR(I)*T
C        XI(I)=XI(I)*T
C      END DO
CC
C      ITA=0
C      RERRB=0D0
C      DO I=1,N
C        RERRB=RERRB+(HPR(I)**2+HPI(I)**2)*DG(I)**2
C      END DO
C      CALL MULS(N,XI,-1D0)
C      CALL MULM2(N,TR,TI,XR,XI,ER,EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C      IF (ISTS.EQ.2) THEN
C        DO ISHL=1,NSHL
C          NN=NF(ISHL)
C          CALL MODGAMM2(N,NN,TR,TI,XR,XI, NOP2+ISHL-1, NHOP,H,IHP1,IHP2
C     &             ,NDEG,NOBT,NOBTX,GALF,HD)
C        END DO
C      END IF
C      CALL MULS(N,XI,-1D0)
C      CALL MULS(N,TI,-1D0)
C      CALL MULM2(N,RR,RI,TR,TI,ER,EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C      IF (ISTS.EQ.2) THEN
C        DO ISHL=1,NSHL
C          NN=NF(ISHL)
C          CALL MODGAMM2(N,NN,RR,RI,TR,TI, NOP2+ISHL-1, NHOP,H,IHP1,IHP2
C     &             ,NDEG,NOBT,NOBTX,GALF,HD)
C        END DO
C      END IF
C      DO I=1,N
C        TTR=DG(I)*(HPR(I)-RR(I))
C        TTI=DG(I)*(HPI(I)-RI(I))
C        RR (I)=TTR
C        RI (I)=TTI
CC        PR (I)=TTR
C        PI (I)=TTI
C        XR(I)=XR(I)/DG(I)
C        XI(I)=XI(I)/DG(I)
C      END DO
C      DO !ITA LOOP
C        RERRT=0D0
C        DO I=1,N
C          RERRT=RERRT+RR(I)**2+RI(I)**2
C        END DO
C        RERR=DSQRT(RERRT/(2*N*RERRB))
C        IF ((ITA.GT.ITAMAX).OR.(RERR.LT.EPS)) EXIT !ITA LOOP
C        DO I=1,N
C          HPR(I)=DG(I)*PR(I)
C          HPI(I)=DG(I)*PI(I)
C        END DO
C        CALL MULS(N,HPI,-1D0)
C        CALL MULM2(N,TR,TI,HPR,HPI,ER, EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C        IF (ISTS.EQ.2) THEN
C          DO ISHL=1,NSHL
C            NN=NF(ISHL)
C            CALL MODGAMM2(N,NN,TR,TI,HPR,HPI, NOP2+ISHL-1
C     &                   ,NHOP,H,IHP1,IHP2,NDEG,NOBT,NOBTX,GALF,HD)
C          END DO
C        END IF
C        CALL MULS(N,TI,-1D0)
C        CALL MULM2(N,HPR,HPI,TR,TI,ER, EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C        IF (ISTS.EQ.2) THEN
C          DO ISHL=1,NSHL
C            NN=NF(ISHL)
C            CALL MODGAMM2(N,NN,HPR,HPI,TR,TI, NOP2+ISHL-1
C     &                   ,NHOP,H,IHP1,IHP2,NDEG,NOBT,NOBTX,GALF,HD)
C          END DO
C        END IF
C        DO I=1,N
C          HPR(I)=DG(I)*HPR(I)
C          HPI(I)=DG(I)*HPI(I)
C        END DO
C        CALL MULV(N,PR, RR,ATR)
C        CALL MULV(N,PR,HPR,ABR)
C        CALL MULV(N,PI, RI,ATI)
C        CALL MULV(N,PI,HPI,ABI)
C        WRITE(6,*)'ABR=',ABR,' ABI=',ABI
C        IF (ISTS.EQ.1) THEN
C          ALFR=ATR/ABR
C          IF (ABI.EQ.0D0) THEN
C            ALFI=0D0
C          ELSE
C            ALFI=ATI/ABI
C          END IF
C        ELSE
C          ALFR=(ATR+ATI)/(ABR+ABI)
C          ALFI=ALFR
C        END IF
C        DO I=1,N
C          XR(I)=XR(I)+ALFR*PR(I)
C          XI(I)=XI(I)+ALFI*PI(I)
C        END DO
C        DO I=1,N
C          RR(I)=RR(I)-ALFR*HPR(I)
C          RI(I)=RI(I)-ALFI*HPI(I)
C        END DO
C        BBR=ABR
C        BBI=ABI
C        CALL MULV(N,RR,HPR,BTR)
C        CALL MULV(N,RI,HPI,BTI)
C        WRITE(6,*)'BBR=',BBR,' BBI=',BBI
C        IF (ISTS.EQ.1) THEN
C          BTAR=BTR/BBR
C          IF (BBI.EQ.0D0) THEN
C            BTAI=0D0
C          ELSE
C            BTAI=BTI/BBI
C          END IF
C        ELSE
C          BTAR=(BTR+BTI)/(BBR+BBI)
CC          BTAI=BTAR
C        END IF
C        DO I=1,N
C          PR(I)=RR(I)-BTAR*PR(I)
C          PI(I)=RI(I)-BTAI*PI(I)
C        END DO
C        ITA=ITA+1
C        WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR
C      END DO !ITA LOOP
C      WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR
CC      DO I=1,N
C        XR(I)=XR(I)*DG(I)
C        XI(I)=XI(I)*DG(I)
C      END DO
C      CALL MOVV(N,TR,XR)
C      CALL MOVV(N,TI,XI)
C      CALL MULS(N,TI,-1D0)
C      CALL MULM2(N,XR,XI, TR, TI,ER,EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C      IF (ISTS.EQ.2) THEN
C        DO ISHL=1,NSHL
C          NN=NF(ISHL)
C          CALL MODGAMM2(N,NN,XR,XI,TR,TI, NOP2+ISHL-1
C     &                 ,NHOP,H,IHP1,IHP2,NDEG,NOBT,NOBTX,GALF,HD)
C        END DO
C      END IF
C      CALL MULS(N,XI,-1D0)
CCvvvvvvvvvvvvvvvvvvvvvvvvvvv
C      CALL MULM2(N,TR,TI, XR, XI,ER,EI,NOP,NHOP,H,HDD,IHP1,IHP2,2)
C      IF (ISTS.EQ.2) THEN
C        DO ISHL=1,NSHL
C          NN=NF(ISHL)
C          CALL MODGAMM2(N,NN,TR,TI,XR,XI, NOP2+ISHL-1
C     &                 ,NHOP,H,IHP1,IHP2,NDEG,NOBT,NOBTX,GALF,HD)
C        END DO
C      END IF
C      RERRT=0D0
C      S=0D0
C      DO I=1,N
C        RERRT=RERRT+(TR(I)-BR(I))**2+(TI(I)-BI(I))**2
C        S=S+TR(I)**2+TI(I)**2
C      END DO
C      RERR=DSQRT(RERRT/(2*N*RERRB))
C      WRITE(6,*)'  ITA=',ITA,'  RERR=',RERR,DSQRT(RERRT/(2*N*S))
CC^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C      CALL MOVV(N,HD,HDD)
C      END
C>>>END RESONANCE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Configuration mixing  
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CNFMIX(NST,NSTM,NCIM,NCI,NSFT,WAV,ASTS,ACI,AOBT)
      IMPLICIT NONE

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      INTEGER NST

      INTEGER NSTM, NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      REAL*8  WAV(*)

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Global variables
      INTEGER IERR
      COMMON /SYS/IERR

C Local variable
      INTEGER IOU(2)
      DATA IOU/2,6/
      INTEGER I,N,IC,IO 
      REAL*8  RN, T
C ---------------------

      N=NSFT(NCI(NST)+1,NST)
      CALL ABSV(N,WAV,RN)
      DO IC=1,NCI(NST)
        T=0D0
        DO I=NSFT(IC,NST)+1,NSFT(IC+1,NST)
          T=T+WAV(I)**2
        END DO
        DO I=1,2
          IO=IOU(I)
          WRITE(IO,*)
     & '    CI=#'//ASTS(NST:NST)//ACI(NST,IC)//'  ',T*100/RN**2,' %'
        END DO
      END DO 
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Write information on electron occupations of 
C     eigen function(s) of the state NST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CHRD(NST,ORBM,NGM,NBGM,NG,EG,UG
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &               ,NMODE,NBSM,T1,T2,ISFT
     &               ,ASTS,ACI,AOBT)
      IMPLICIT NONE 

C Parameters
      INTEGER LMAX2,NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)

C Dummy arguments
      INTEGER NST
      REAL*8  ORBM(0:NOBTM*7)

      INTEGER NGM, NBGM, NG
      REAL*8  EG(NGM)
      REAL*8  UG(NBGM,NGM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
 
      INTEGER NSTM, NCIM
      REAL*8  ERED(NCIM)

      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NMODE
  
      INTEGER NBSM
      REAL*8  T1(NBSM), T2(NBSM)
      INTEGER ISFT

      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)

C Global variables
      INTEGER IERR
      COMMON /SYS/IERR

C Local variables
      REAL*8  ELCR(0:NEMAX-1,0:NEMAX-1), ELCI(0:NEMAX-1,0:NEMAX-1)
      REAL*8  OHB(0:NEMAX-1), RJB(0:NEMAX-1)
      INTEGER NORBM(0:NOBTM)
      REAL*8  E(NEMAX)
      REAL*8  UR(NEMAX,NEMAX), UI(NEMAX,NEMAX)

      INTEGER IOU(2) 
      DATA IOU/2,6/

      INTEGER I, J, K, IO, IB, IB1, IB2, IM, NCPX, IOM, IG ,N1
      REAL*8 S,T,PI,TR,TI
C -----------------------------------
      NCPX=MOD(NMODE   ,10)
      DO IOM=0,NINT( ORBM(0) )
        NORBM(IOM)=NINT( ORBM(IOM) )
      END DO

      N1=NSFT(NCI(NST)+1,NST)
C
      DO IG=1,NG
      WRITE(6,*)'====== state #'//ASTS(NST:NST)//' ========='
      WRITE(2,*)'====== state #'//ASTS(NST:NST)//' ========='
      WRITE(6,*)'  #',ISFT+IG,'  E=',EG(IG)
      WRITE(2,*)'  #',ISFT+IG,'  E=',EG(IG)
      PI=4D0*ATAN(1D0)
      CALL OPRT_C(NST,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,PI/2D0,N1,UG(1,IG),T1,T2,TR,TI,NMODE)
      WRITE(6,'(''  C4'',2F8.4)')TR,TI
      WRITE(2,'(''  C4'',2F8.4)')TR,TI
      CALL OPRT_C(NST,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,2D0*PI/3D0,N1,UG(1,IG),T1,T2,TR,TI,NMODE)
      WRITE(6,'(''  C3'',2F8.4)')TR,TI
      WRITE(2,'(''  C3'',2F8.4)')TR,TI
      CALL OPRT_C(NST,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,PI,       N1,UG(1,IG),T1,T2,TR,TI,NMODE)
      WRITE(6,'(''  C2'',2F8.4)')TR,TI
      WRITE(2,'(''  C2'',2F8.4)')TR,TI
      DO IM=1,NORBM(0)
        IOM=NORBM(IM)
        CALL CHRG(NST,IOM,IG,ELCR,ELCI,NGM, NBGM, UG
     &           ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &           ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &           ,NMODE,N1,T1,T2)
        K=NORBM(0)+6*(IM-1)+1
        CALL ORBROT( NDEG(IOM), ELCR, ELCI, ORBM(K) )
        S=0D0
        DO IB=0,NDEG(IOM)-1,2
          S=S+ELCR(IB,IB)
        END DO
        T=0D0
        DO IB=1,NDEG(IOM)-1,2
          T=T+ELCR(IB,IB)
        END DO
        CALL S2J ( NDEG(IOM), ELCR, RJB )
        CALL S2OH( NDEG(IOM), ELCR, OHB )
        DO J=1,2
          IO=IOU(J)
          WRITE(IO,'(A)')' Orbit => '//AOBT(IOM)
          WRITE(IO,*)'               ----- electron occupation ----'
          IF (NDEG(IOM).EQ.2) THEN

            WRITE(IO,'(''   m   down     up             down     up  '
     &              // '   Jz   J=1/2  '')')
            WRITE(IO,'(''   0'',2F8.4,''   A1g  '',2F8.4'
     &            //',''   1/2        '',F8.4)')
     &             ELCR(0,0),ELCR(1,1),OHB(0),OHB(1),       RJB(1)
            WRITE(IO,'('' sum'',2F8.4,''                        '''
     &            //',''  -1/2        '',F8.4)')
     &              S,       T,                             RJB(0)
            WRITE(IO,'('' total  '',F8.4)')S+T

          ELSE IF (NDEG(IOM).EQ.6) THEN

            WRITE(IO,'(''   m   down     up             down     up  '
     &              // '   Jz   J=1/2   J=3/2  '')')
            WRITE(IO,'(''   1'',2F8.4,''   T1u x'',2F8.4'
     &            //',''   3/2        '',F8.4)')
     &             ELCR(4,4),ELCR(5,5),OHB(0),OHB(1),       RJB(5)
            WRITE(IO,'(''   0'',2F8.4,''   T1u y'',2F8.4'
     &            //',''   1/2'',2F8.4)')
     &             ELCR(2,2),ELCR(3,3),OHB(2),OHB(3),RJB(1),RJB(4)
            WRITE(IO,'(''  -1'',2F8.4,''   T1u z'',2F8.4'
     &            //',''  -1/2'',2F8.4)')
     &             ELCR(0,0),ELCR(1,1),OHB(4),OHB(5),RJB(0),RJB(3)
            WRITE(IO,'('' sum'',2F8.4,''                        '''
     &            //',''  -3/2        '',F8.4)')
     &              S,       T,                             RJB(2)
            WRITE(IO,'('' total  '',F8.4)')S+T

          ELSE IF (NDEG(IOM).EQ.10) THEN

            WRITE(IO,'(''   m   down     up             down     up  '
     &              // '   Jz   J=3/2   J=5/2  '')')
            WRITE(IO,'(''   2'',2F8.4,''   Eg  u'',2F8.4'
     &            //',''   5/2        '',F8.4)')
     &             ELCR(8,8),ELCR(9,9),OHB(0),OHB(1),       RJB(9)
            WRITE(IO,'(''   1'',2F8.4,''   Eg  v'',2F8.4'
     &            //',''   3/2'',2F8.4)')
     &             ELCR(6,6),ELCR(7,7),OHB(2),OHB(3),RJB(3),RJB(8)
            WRITE(IO,'(''   0'',2F8.4,''   T2g a'',2F8.4'
     &            //',''   1/2'',2F8.4)')
     &             ELCR(4,4),ELCR(5,5),OHB(4),OHB(5),RJB(2),RJB(7)
            WRITE(IO,'(''  -1'',2F8.4,''   T2g b'',2F8.4'
     &            //',''  -1/2'',2F8.4)')
     &             ELCR(2,2),ELCR(3,3),OHB(6),OHB(7),RJB(1),RJB(6)
            WRITE(IO,'(''  -2'',2F8.4,''   T2g c'',2F8.4'
     &            //',''  -3/2'',2F8.4)')
     &             ELCR(0,0),ELCR(1,1),OHB(8),OHB(9),RJB(0),RJB(5)
            WRITE(IO,'('' sum'',2F8.4,''                        '''
     &            //',''  -5/2        '',F8.4)')
     &              S,       T,                             RJB(4)
            WRITE(IO,'('' total  '',F8.4)')S+T

          ELSE IF (NDEG(IOM).EQ.14) THEN

            WRITE(IO,'(''   m   down     up             down     up  '
     &            // '   Jz   J=5/2   J=7/2  '')')
            WRITE(IO,'(''   3'',2F8.4,''   A2u  '',2F8.4'
     &          //',''   7/2        '',F8.4)')
     &           ELCR(12,12),ELCR(13,13),OHB( 0),OHB( 1),      RJB(13)
            WRITE(IO,'(''   2'',2F8.4,''   T1u x'',2F8.4'
     &          //',''   5/2'',2F8.4)')
     &         ELCR(10,10),ELCR(11,11),OHB( 2),OHB( 3),RJB( 5),RJB(12)
            WRITE(IO,'(''   1'',2F8.4,''   T1u y'',2F8.4'
     &          //',''   3/2'',2F8.4)')
     &         ELCR( 8, 8),ELCR( 9, 9),OHB( 4),OHB( 5),RJB( 4),RJB(11)
            WRITE(IO,'(''   0'',2F8.4,''   T1u z'',2F8.4'
     &          //',''   1/2'',2F8.4)')
     &         ELCR( 6, 6),ELCR( 7, 7),OHB( 6),OHB( 7),RJB( 3),RJB(10)
            WRITE(IO,'(''  -1'',2F8.4,''   T2u a'',2F8.4'
     &          //',''  -1/2'',2F8.4)')
     &         ELCR( 4, 4),ELCR( 5, 5),OHB( 8),OHB( 9),RJB( 2),RJB( 9)
            WRITE(IO,'(''  -2'',2F8.4,''   T2u b'',2F8.4'
     &          //',''  -3/2'',2F8.4)')
     &         ELCR( 2, 2),ELCR( 3, 3),OHB(10),OHB(11),RJB( 1),RJB( 8)
            WRITE(IO,'(''  -3'',2F8.4,''   T2u c'',2F8.4'
     &          //',''  -5/2'',2F8.4)')
     &         ELCR( 0, 0),ELCR( 1, 1),OHB(12),OHB(13),RJB( 0),RJB( 7)
            WRITE(IO,'('' sum'',2F8.4,''                        '''
     &          //',''  -7/2        '',F8.4)')
     &            S,       T,                                  RJB( 6)
            WRITE(IO,'('' total  '',F8.4)')S+T

          END IF
        END DO
      END DO !IM
      END DO !IG
      DO IG=1,NG
        WRITE(2,*)'====== state #'//ASTS(NST:NST)//' ========='
        WRITE(2,*)'  #',ISFT+IG,'  E=',EG(IG)
        DO IM=1,NORBM(0)
          IOM=NORBM(IM)
          CALL CHRG(NST,IOM,IG,ELCR,ELCI,NGM,NBGM,UG
     &             ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &             ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &             ,NMODE, N1,T1,T2)
          WRITE(2,'(A)')' Orbit => '//AOBT(IOM)
          WRITE(2,*)'-- Expectation value of the number operator --'
          DO IB2=0,NDEG(IOM)-1
            WRITE(2,'(28F15.7)')
     &        (ELCR(IB1,IB2),ELCI(IB1,IB2),IB1=0,NDEG(IOM)-1)
          END DO
  
          IF (NCPX.NE.0) THEN
            CALL CJACOB(NDEG(IOM),NEMAX,ELCR,ELCI,E,UR,UI)
          ELSE
            CALL JACOB(NDEG(IOM),NEMAX,ELCR,E,UR)
          END IF
          WRITE(2,*)'-- Diagonalized --'
          DO J=NDEG(IOM),1,-1
            WRITE(2,'(''  Weight='',F15.7)')E(J)
            IF (NCPX.NE.0) THEN
              WRITE(2,'(14F15.7)')(UR(I,J),I=1,NDEG(IOM))
              WRITE(2,'(14F15.7)')(UI(I,J),I=1,NDEG(IOM))
            ELSE
              WRITE(2,'(14F15.7)')(UR(I,J),I=1,NDEG(IOM))
            END IF
          END DO

        END DO
      END DO
C
      END SUBROUTINE CHRD
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Rotate quantization axis of the orbital.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ORBROT(NDEG,ELCR,ELCI,ANGL)
      IMPLICIT NONE

C Parameters
      INTEGER    LMAX2,   NEMAX 
      PARAMETER( LMAX2=3, NEMAX=4*LMAX2+2 )

C Dummy arguments
      INTEGER NDEG
      REAL*8  ELCR (NEMAX,NEMAX), ELCI (NEMAX,NEMAX)
      REAL*8  ANGL(6) 

C Local variables
      REAL*8  ELCRN(NEMAX,NEMAX), ELCIN(NEMAX,NEMAX)
      REAL*8  UOR(NEMAX/2,NEMAX/2), UOI(NEMAX/2,NEMAX/2)
      REAL*8  USR(2,2), USI(2,2)
      REAL*8  UR(NEMAX,NEMAX), UI(NEMAX,NEMAX)
      INTEGER I,J,K,L,NSDEG,IS,JS,IO,JO
      REAL*8  TR,TI, ALFO, BTAO, GAMO, ALFS, BTAS, GAMS
C -------

      IF (MOD(NDEG,2).EQ.1) THEN
        L=NDEG/2
        NSDEG=1
      ELSE IF (MOD(NDEG,4).EQ.2) THEN
        L=NDEG/4
        NSDEG=2
      ELSE
        L=0
        NSDEG=2 
      END IF
C
      ALFO=ANGL(1)
      BTAO=ANGL(2)
      GAMO=ANGL(3)
      ALFS=ANGL(4)
      BTAS=ANGL(5)
      GAMS=ANGL(6)
C      WRITE(6,*)'ALF,BTA,GAM=',ALFO,BTAO,GAMO
C      WRITE(6,*)'ALF,BTA,GAM=',ALFS,BTAS,GAMS
C
      IF (L.GT.0) CALL ROTS(2*L+1,NEMAX/2,UOR,UOI,ALFO,BTAO,GAMO)
      IF (NSDEG.EQ.2) CALL ROTS(2    ,2      ,USR,USI,ALFS,BTAS,GAMS)
C
      IF (NSDEG.GE.2 .AND. L .GT. 0) THEN
        DO IS=1,2
          DO JS=1,2
            DO IO=1,2*L+1
              DO JO=1,2*L+1
                UR(2*IO+IS-2,2*JO+JS-2)=
     &                 UOR(IO,JO)*USR(IS,JS)-UOI(IO,JO)*USI(IS,JS)
                UI(2*IO+IS-2,2*JO+JS-2)=
     &                 UOR(IO,JO)*USI(IS,JS)+UOI(IO,JO)*USR(IS,JS)
              END DO
            END DO
          END DO
        END DO
      ELSE IF (NSDEG.GE.2) THEN
        DO I=1,NDEG
          DO J=1,NDEG
            UR(I,J)=0D0
            UI(I,J)=0D0
          END DO
        END DO
        DO I=0,NDEG-1,2
          DO IS=1,2
            DO JS=1,2
              UR(I+IS,I+JS)=USR(IS,JS)
              UI(I+IS,I+JS)=USI(IS,JS)
            END DO
          END DO
        END DO
      ELSE IF (L.GT.0 ) THEN
        DO I=1,NDEG
          DO J=1,NDEG
            UR(I,J)=UOR(I,J)
            UI(I,J)=UOI(I,J)
          END DO
        END DO
      ELSE 
        RETURN
      END IF

      DO I=1,NDEG
        DO J=1,NDEG
          TR=0D0
          TI=0D0
          DO K=1,NDEG
            TR=TR + ELCR(J,K)*UR(K,I)-ELCI(J,K)*UI(K,I)
            TI=TI + ELCR(J,K)*UI(K,I)+ELCI(J,K)*UR(K,I)
C            TR=TR + ELCR(K,J)*UR(K,I)-ELCI(K,J)*UI(K,I)
C            TI=TI + ELCR(K,J)*UI(K,I)+ELCI(K,J)*UR(K,I)
          END DO
          ELCRN(J,I)=TR
          ELCIN(J,I)=TI
        END DO
      END DO

      DO I=1,NDEG
        DO J=1,NDEG
          TR=0D0
          TI=0D0
          DO K=1,NDEG
            TR=TR + ELCRN(K,I)*UR(K,J)+ELCIN(K,I)*UI(K,J)
            TI=TI - ELCRN(K,I)*UI(K,J)+ELCIN(K,I)*UR(K,J)
C            TR=TR + ELCRN(I,K)*UR(K,J)+ELCIN(I,K)*UI(K,J)
C            TI=TI - ELCRN(I,K)*UI(K,J)+ELCIN(I,K)*UR(K,J)
          END DO
          ELCR(J,I)=TR
          ELCI(J,I)=TI
        END DO
      END DO
    
      END SUBROUTINE ORBROT

C--------------------------------------------------
C Diagonalization of a real symmetric matrix by Jacob's method.
C          coded by Arata Tanaka on 2005.1.6
C
C  input   
C         integer    N           :  dimension of the matrix
C         integer    NMAX        :  dimension for the 1st subscript
C                                 of the arrays A and U
C         real*8     A(NMAX,N)   :  matrix elements       
C  output  
C         real*8     E(N)        :  eigen values
C         real*8     U(NMAX,N)   :  eigen vectors 
C
C--------------------------------------------------
      SUBROUTINE JACOB(N,NMAX,A,E,U)
      IMPLICIT NONE 

C Parameters
      INTEGER ITAMAX
      PARAMETER(ITAMAX=10000)
      REAL*8  EPS
      PARAMETER(EPS=1D-15)

C Dummy arguments
      INTEGER N, NMAX
      REAL*8  A(NMAX,N)
      REAL*8  E(N)
      REAL*8  U(NMAX,N)

C Local variables
      INTEGER I,J,K,ITA,NCNT
      REAL*8 T,R,C,S,A_II,A_JJ,A_IJ,TI,TJ,AMAX
C --------------

      DO I=1,N
        DO J=1,N
          U(I,J)=0D0
        END DO
      END DO
      DO I=1,N
        U(I,I)=1D0
      END DO
      AMAX=A(1,1)
      DO I=1,N
        DO J=1,N
          AMAX=MAX( ABS( A(I,J) ), AMAX )
        END DO
      END DO
    
      NCNT=1
      ITA=0 

      DO WHILE( NCNT.GT.0 .AND. ITA.LT.ITAMAX )
        NCNT=0
        DO I=1,N
          DO J=I+1,N

            A_IJ=A(I,J)
            IF ( ABS(A_IJ) .GT. EPS*AMAX ) THEN

              NCNT=NCNT+1 

              A_II=A(I,I)
              A_JJ=A(J,J)

              R=( A_JJ - A_II )/( 2D0 * A_IJ )
              R=R+SIGN(1D0,R)*SQRT(1D0+R**2)
              S=1D0/SQRT(1D0+R**2)
              C=R*S

              DO K=1,N
                TI=A(K,I)
                TJ=A(K,J)
                A(K,I)=C*TI-S*TJ
                A(K,J)=S*TI+C*TJ
              END DO
              DO K=1,N
                A(I,K)= A(K,I)
                A(J,K)= A(K,J)
              END DO
              A(I,I)=A_II*C**2 + A_JJ*S**2 - 2d0*A_IJ*C*S
              A(J,J)=A_II*S**2 + A_JJ*C**2 + 2d0*A_IJ*C*S
              A(I,J)=0D0 
              A(J,I)=0D0 

              DO K=1,N
                TI= U(K,I)
                TJ= U(K,J)
                U(K,I)=C*TI-S*TJ
                U(K,J)=S*TI+C*TJ
              END DO
    
            END IF
       
          END DO
        END DO
        ITA=ITA+1
      END DO

      IF ( ITA .EQ. ITAMAX ) THEN
        WRITE(6,*)'Did not converge'
      END IF
      
      DO I=1,N
        E(I)=A(I,I)
      END DO

      IF (N.GT.1) THEN
        R= EPS * AMAX 
        DO I=1,N-1
          DO J=I+1,N
            IF (E(I).GT.E(J)+R) THEN
              T=E(I)
              E(I)=E(J)
              E(J)=T
              DO K=1,N
                T=U(K,I)
                U(K,I)=U(K,J)
                U(K,J)=T
              END DO
            END IF
          END DO
        END DO
      END IF
    
      END SUBROUTINE JACOB

C--------------------------------------------------
C Diagonalization of a Helmite matrix by Jacob's method.
C          coded by Arata Tanaka on 2005.1.6
C
C  input   
C         integer    N           :  dimension of the matrix
C         integer    NMAX        :  dimension for the 1st subscript
C                                 of the arrays AR, AI, UR and UI
C         real*8     AR(NMAX,N)  :  real parts of matrix elements       
C         real*8     AI(NMAX,N)  :  imaginaly parts of matrix elements       
C  output  
C         real*8     E(N)        :  eigen values
C         real*8     UR(NMAX,N)  :  real parts of eigen vectors 
C         real*8     UI(NMAX,N)  :  imaginaly parts of eigen vectors 
C
C--------------------------------------------------
      SUBROUTINE CJACOB(N,NMAX,AR,AI,E,UR,UI)
      IMPLICIT NONE 

C Parameters
      INTEGER ITAMAX
      PARAMETER(ITAMAX=10000)
      REAL*8  EPS
      PARAMETER(EPS=1D-15)

C Dummy arguments
      INTEGER N, NMAX
      REAL*8  AR(NMAX,N),AI(NMAX,N)
      REAL*8  E(N)
      REAL*8  UR(NMAX,N),UI(NMAX,N)

C Local variables
      INTEGER I,J,K,ITA,NCNT
      REAL*8 T,R,C,S,A_II,A_JJ,A_IJ,TIR,TII,TJR,TJI,AMAX
      REAL*8 CPHI,SPHI,PHI
C --------
     
      DO I=1,N
        DO J=1,N
          UR(I,J)=0D0
          UI(I,J)=0D0
        END DO
      END DO
      DO I=1,N
        UR(I,I)=1D0
      END DO
      AMAX=AR(1,1)
      DO I=1,N
        DO J=1,N
          AMAX=MAX( ABS( AR(I,J) ), ABS( AI(I,J) ), AMAX)
        END DO
      END DO
    
      NCNT=1
      ITA=0 

      DO WHILE( NCNT.GT.0 .AND. ITA.LT.ITAMAX )
        NCNT=0
        DO I=1,N
          DO J=I+1,N

            A_IJ=SQRT( AR(I,J)**2 + AI(I,J)**2 )
            IF ( A_IJ .GT. EPS*AMAX ) THEN

              NCNT=NCNT+1 

              A_II=AR(I,I)
              A_JJ=AR(J,J)

              PHI=ATAN2( AI(I,J), AR(I,J) )
              CPHI=COS(-PHI/2D0)
              SPHI=SIN(-PHI/2D0) 
            
              R=( A_JJ - A_II )/( 2D0 * A_IJ )
              R=R+SIGN(1D0,R)*SQRT(1D0+R**2)
              S=1D0/SQRT(1D0+R**2)
              C=R*S

              DO K=1,N
                TIR= CPHI*AR(K,I)+SPHI*AI(K,I)
                TII=-SPHI*AR(K,I)+CPHI*AI(K,I)
                TJR= CPHI*AR(K,J)-SPHI*AI(K,J)
                TJI= SPHI*AR(K,J)+CPHI*AI(K,J)
                AR(K,I)=C*TIR-S*TJR
                AR(K,J)=S*TIR+C*TJR
                AI(K,I)=C*TII-S*TJI
                AI(K,J)=S*TII+C*TJI
              END DO
              DO K=1,N
                AR(I,K)= AR(K,I)
                AI(I,K)=-AI(K,I)
                AR(J,K)= AR(K,J)
                AI(J,K)=-AI(K,J)
              END DO
              AR(I,I)=A_II*C**2 + A_JJ*S**2 - 2d0*A_IJ*C*S
              AR(J,J)=A_II*S**2 + A_JJ*C**2 + 2d0*A_IJ*C*S
              AR(I,J)=0D0 
              AR(J,I)=0D0 
              AI(I,J)=0D0 
              AI(J,I)=0D0 

              DO K=1,N
                TIR= CPHI*UR(K,I)+SPHI*UI(K,I)
                TII=-SPHI*UR(K,I)+CPHI*UI(K,I)
                TJR= CPHI*UR(K,J)-SPHI*UI(K,J)
                TJI= SPHI*UR(K,J)+CPHI*UI(K,J)
                UR(K,I)=C*TIR-S*TJR
                UR(K,J)=S*TIR+C*TJR
                UI(K,I)=C*TII-S*TJI
                UI(K,J)=S*TII+C*TJI
              END DO
    
            END IF
       
          END DO
        END DO
        ITA=ITA+1
      END DO

      IF ( ITA .EQ. ITAMAX ) THEN
        WRITE(6,*)'Did not converge'
      END IF
      
      DO I=1,N
        E(I)=AR(I,I)
      END DO

      IF (N.GT.1) THEN
        R= EPS * AMAX 
        DO I=1,N-1
          DO J=I+1,N
            IF (E(I).GT.E(J)+R) THEN
              T=E(I)
              E(I)=E(J)
              E(J)=T
              DO K=1,N
                T=UR(K,I)
                UR(K,I)=UR(K,J)
                UR(K,J)=T
                T=UI(K,I)
                UI(K,I)=UI(K,J)
                UI(K,J)=T
              END DO
            END IF
          END DO
        END DO
      END IF
    
      END SUBROUTINE CJACOB

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Electron occupation on orbitals |j=l+1/2,j_z> and |j=l-1/2,j_z>
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE S2J(NDEG,SBR,RJB)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX2,  NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

C Dummy arguments
      INTEGER NDEG
      REAL*8  SBR(0:NEMAX-1,0:NEMAX-1)
      REAL*8  RJB(0:NEMAX-1)

C Local variables
      INTEGER L, M, IB1, IB2
C ---------------

      IF ( MOD(NDEG,4) .NE. 2 ) RETURN
      L=NDEG/4
C
      DO M=-L+1 ,L
        IB1=2*(M-1)+1+2*L
        IB2=2*M+2*L
        RJB(M+L-1)=1D0/DBLE(2*L+1)*(
     &                 DBLE(L-M+1)*SBR(IB1,IB1)+DBLE(L+M)*SBR(IB2,IB2)
     &                -2D0*SQRT(DBLE((L-M+1)*(L+M)))*SBR(IB1,IB2)
     &             )
      END DO
C
      DO M=-L-1,L
        IB1=MAX(2*M+1+2*L,0)
        IB2=MIN(2*(M+1)+2*L,4*L+1)
        RJB(M+3*L+1)=1D0/DBLE(2*L+1)*(
     &                 DBLE(L+M+1)*SBR(IB1,IB1)+DBLE(L-M)*SBR(IB2,IB2)
     &                +2D0*SQRT(DBLE((L+M+1)*(L-M)))*SBR(IB1,IB2)
     &             )
      END DO

      END SUBROUTINE S2J

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Electron occupation on the orbitals with octahedral symmetry
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE S2OH(NDEG,SBR,OHB)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX2,  NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

C Dummy arguments
      INTEGER NDEG
      REAL*8  SBR(0:NEMAX-1,0:NEMAX-1)
      REAL*8  OHB(0:NEMAX-1)

C Local variable
      INTEGER L, NSDEG
C -------------------

      IF (MOD(NDEG,2).EQ.1) THEN
        L=NDEG/2
        NSDEG=1
      ELSE IF (MOD(NDEG,4).EQ.2) THEN
        L=NDEG/4
        NSDEG=2
      ELSE
        L=0
        NSDEG=2 
      END IF
C
      IF (NSDEG.EQ.2) THEN
        IF (L.EQ.0) THEN
          OHB(0)=SBR(0,0)
          OHB(1)=SBR(1,1)
        ELSE IF (L.EQ.1) THEN
          OHB(0)=0.5D0*(SBR(4,4)+SBR(0,0)-2D0*SBR(0,4))
          OHB(1)=0.5D0*(SBR(5,5)+SBR(1,1)-2D0*SBR(1,5))
          OHB(2)=0.5D0*(SBR(4,4)+SBR(0,0)+2D0*SBR(0,4))
          OHB(3)=0.5D0*(SBR(5,5)+SBR(1,1)+2D0*SBR(1,5))
          OHB(4)=SBR(2,2)
          OHB(5)=SBR(3,3)
        ELSE IF (L.EQ.2) THEN
          OHB(0)=SBR(4,4)
          OHB(1)=SBR(5,5)
          OHB(2)=0.5D0*(SBR(8,8)+SBR(0,0)+2D0*SBR(0,8))
          OHB(3)=0.5D0*(SBR(9,9)+SBR(1,1)+2D0*SBR(1,9))
          OHB(4)=0.5D0*(SBR(6,6)+SBR(2,2)+2D0*SBR(2,6))
          OHB(5)=0.5D0*(SBR(7,7)+SBR(3,3)+2D0*SBR(3,7))
          OHB(6)=0.5D0*(SBR(6,6)+SBR(2,2)-2D0*SBR(2,6))
          OHB(7)=0.5D0*(SBR(7,7)+SBR(3,3)-2D0*SBR(3,7))
          OHB(8)=0.5D0*(SBR(8,8)+SBR(0,0)-2D0*SBR(0,8))
          OHB(9)=0.5D0*(SBR(9,9)+SBR(1,1)-2D0*SBR(1,9))
        ELSE IF (L.EQ.3) THEN
          OHB(0)=0.5D0*(SBR(10,10)+SBR(2,2)-2D0*SBR(2,10))
          OHB(1)=0.5D0*(SBR(11,11)+SBR(3,3)-2D0*SBR(3,11))
          OHB(2)=1D0/16D0*(5D0*(SBR(12,12)+SBR(0,0)-2D0*SBR(0,12))
     &                    +3D0*(SBR( 8, 8)+SBR(4,4)-2D0*SBR(4, 8))
     &                    -2D0*SQRT(15D0)*(SBR(12, 8)+SBR(0,4))
     &                    +2D0*SQRT(15D0)*(SBR(12, 4)+SBR(0,8))     
     &                    )
          OHB(3)=1D0/16D0*(5D0*(SBR(13,13)+SBR(1,1)-2D0*SBR(1,13))
     &                    +3D0*(SBR( 9, 9)+SBR(5,5)-2D0*SBR(5, 9))
     &                    -2D0*SQRT(15D0)*(SBR(13, 9)+SBR(1,5))
     &                    +2D0*SQRT(15D0)*(SBR(13, 5)+SBR(1,9))     
     &                    )
          OHB(4)=1D0/16D0*(5D0*(SBR(12,12)+SBR(0,0)+2D0*SBR(0,12))
     &                    +3D0*(SBR( 8, 8)+SBR(4,4)+2D0*SBR(4, 8))
     &                    +2D0*SQRT(15D0)*(SBR(12, 8)+SBR(0,4))
     &                    +2D0*SQRT(15D0)*(SBR(12, 4)+SBR(0,8))     
     &                    )
          OHB(5)=1D0/16D0*(5D0*(SBR(13,13)+SBR(1,1)+2D0*SBR(1,13))
     &                    +3D0*(SBR( 9, 9)+SBR(5,5)+2D0*SBR(5, 9))
     &                    +2D0*SQRT(15D0)*(SBR(13, 9)+SBR(1,5))
     &                    +2D0*SQRT(15D0)*(SBR(13, 5)+SBR(1,9))  
     &                    )
          OHB(6)=SBR(6,6)
          OHB(7)=SBR(7,7)
          OHB(8)=1D0/16D0*(3D0*(SBR(12,12)+SBR(0,0)-2D0*SBR(0,12))
     &                    +5D0*(SBR( 8, 8)+SBR(4,4)-2D0*SBR(4, 8))
     &                    +2D0*SQRT(15D0)*(SBR(12, 8)+SBR(0,4))
     &                    -2D0*SQRT(15D0)*(SBR(12, 4)+SBR(0,8))     
     &                    )
          OHB(9)=1D0/16D0*(3D0*(SBR(13,13)+SBR(1,1)-2D0*SBR(1,13))
     &                    +5D0*(SBR( 9, 9)+SBR(5,5)-2D0*SBR(5, 9))
     &                    +2D0*SQRT(15D0)*(SBR(13, 9)+SBR(1,5))
     &                    -2D0*SQRT(15D0)*(SBR(13, 5)+SBR(1,9))     
     &                    )
          OHB(10)=1D0/16D0*(3D0*(SBR(12,12)+SBR(0,0)+2D0*SBR(0,12))
     &                     +5D0*(SBR( 8, 8)+SBR(4,4)+2D0*SBR(4, 8))
     &                     -2D0*SQRT(15D0)*(SBR(12, 8)+SBR(0,4))
     &                     -2D0*SQRT(15D0)*(SBR(12, 4)+SBR(0,8))     
     &                     )
          OHB(11)=1D0/16D0*(3D0*(SBR(13,13)+SBR(1,1)+2D0*SBR(1,13))
     &                     +5D0*(SBR( 9, 9)+SBR(5,5)+2D0*SBR(5, 9))
     &                     -2D0*SQRT(15D0)*(SBR(13, 9)+SBR(1,5))
     &                     -2D0*SQRT(15D0)*(SBR(13, 5)+SBR(1,9))     
     &                     )
          OHB(12)=0.5D0*(SBR(10,10)+SBR(2,2)+2D0*SBR(2,10))
          OHB(13)=0.5D0*(SBR(11,11)+SBR(3,3)+2D0*SBR(3,11))
        END IF
      ELSE
        IF (L.EQ.0) THEN
          OHB(0)=SBR(0,0)
        ELSE IF (L.EQ.1) THEN
          OHB(0)=0.5D0*(SBR(2,2)+SBR(0,0)-2D0*SBR(0,2))
          OHB(1)=0.5D0*(SBR(2,2)+SBR(0,0)+2D0*SBR(0,2))
          OHB(2)=SBR(1,1)
        ELSE IF (L.EQ.2) THEN
          OHB(0)=SBR(2,2)
          OHB(1)=0.5D0*(SBR(4,4)+SBR(0,0)+2D0*SBR(0,4))
          OHB(2)=0.5D0*(SBR(3,3)+SBR(1,1)+2D0*SBR(1,3))
          OHB(3)=0.5D0*(SBR(3,3)+SBR(1,1)-2D0*SBR(1,3))
          OHB(4)=0.5D0*(SBR(4,4)+SBR(0,0)-2D0*SBR(0,4))
        ELSE IF (L.EQ.3) THEN
          OHB(0)=0.5D0*(SBR(5,5)+SBR(1,1)-2D0*SBR(1,5))
          OHB(1)=1D0/16D0*(5D0*(SBR(6,6)+SBR(0,0)-2D0*SBR(0,6))
     &                    +3D0*(SBR(4,4)+SBR(2,2)-2D0*SBR(2,4))
     &                    -2D0*SQRT(15D0)*(SBR(6,4)+SBR(0,2))
     &                    +2D0*SQRT(15D0)*(SBR(6,2)+SBR(0,4))     
     &                    )
          OHB(2)=1D0/16D0*(5D0*(SBR(6,6)+SBR(0,0)+2D0*SBR(0,6))
     &                    +3D0*(SBR(4,4)+SBR(2,2)+2D0*SBR(2,4))
     &                    +2D0*SQRT(15D0)*(SBR(6,4)+SBR(0,2))
     &                    +2D0*SQRT(15D0)*(SBR(6,2)+SBR(0,4))     
     &                    )
          OHB(3)=SBR(3,3)
          OHB(4)=1D0/16D0*(3D0*(SBR(6,6)+SBR(0,0)-2D0*SBR(0,6))
     &                    +5D0*(SBR(4,4)+SBR(2,2)-2D0*SBR(2,4))
     &                    +2D0*SQRT(15D0)*(SBR(6,4)+SBR(0,2))
     &                    -2D0*SQRT(15D0)*(SBR(6,2)+SBR(0,4))     
     &                    )
          OHB(5)=1D0/16D0*(3D0*(SBR(6,6)+SBR(0,0)+2D0*SBR(0,6))
     &                    +5D0*(SBR(4,4)+SBR(2,2)+2D0*SBR(2,4))
     &                    -2D0*SQRT(15D0)*(SBR(6,4)+SBR(0,2))
     &                    -2D0*SQRT(15D0)*(SBR(6,2)+SBR(0,4))     
     &                    )
          OHB(6)=0.5D0*(SBR(5,5)+SBR(1,1)+2D0*SBR(1,5))
        END IF
      END IF
      END SUBROUTINE S2OH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    This routine
C    calculates <IG| a^dagger_IB1 a^dagger_IB2 |IG> 
C    |IG>: IG-th eigen function of state NST
C    a^dagger_IB1: creation operator of orbital IOM with a
C                  spin and orbital index IB1=2 + sigma + 2m
C    The real and imaginal parts will be saved 
C    in ELCR(IB1,IB2) and ELCI(IB1,IB2).
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CHRG(NST,IOM,IG,ELCR,ELCI
     &               , NGM, NBGM, UG
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &               ,NMODE, NBSM, T1,T2)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX2,  NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBLKM
      PARAMETER(NBLKM=200)

C Dummy arguments
      INTEGER NST, IOM, IG
      REAL*8  ELCR(0:NEMAX-1,0:NEMAX-1), ELCI(0:NEMAX-1,0:NEMAX-1)

      INTEGER NGM, NBGM
      REAL*8  UG(NBGM,NGM)

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
 
      INTEGER NSTM,NCIM
      REAL*8  ERED(NCIM)
 
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NMODE
 
      INTEGER NBSM
      REAL*8  T1(NBSM), T2(NBSM)

C Local variable
      INTEGER IARG(8)
     
      REAL*8  RR(NBLKM), RI(NBLKM)
      INTEGER NBLK

      INTEGER I, N, IHP, IHPB, NOP, IB1, IB2, ICI, NCPX, NEXL
      INTEGER NTYP,NBDY,ICPX,NEX,ISB,LIN
      REAL*8 SR, SI
C -------------
      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
C
      NOP=NHOP(0)+2
      IHPB=IHP1(1)
      NHOP(NOP)=IHPB
      N  =NSFT(NCI(NST)+1,NST)

      NTYP=4
      NBDY=1
      ICPX=0
      NEX=1
      ISB=0
      LIN=0

      IARG(1)=NST 
      IARG(2)=NST 
      IARG(5)=IOM
      IARG(6)=IOM
      IARG(7)=IOM
      IARG(8)=IOM

      NBLK=2
      DO I=1,NBLKM
        RR(I)=0D0
        RI(I)=0D0
      END DO
C
      DO IB1=0,NDEG(IOM)-1
        DO IB2=IB1,NDEG(IOM)-1

          IHP=IHPB
          RR(1)=DBLE(IB2)
          RR(2)=DBLE(IB1)

          DO ICI=1,NCI(NST)
            IARG(3)=ICI
            IARG(4)=ICI
            CALL PUTBLK(IHP,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,ISB,ICPX,LIN)
          END DO !ICI

          NHOP(NOP+1)=IHP
          IHP1(1)=IHP
          IHP2(1)=IHP

          CALL HGEN (NBSM,T1,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &              ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)

          CALL CLRV(N,T2)
          CALL OPRT(N,T2,N,UG(1,IG),NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N,UG(1,IG),T2,SR,SI,NCPX)
C          WRITE(6,*)' SR=',SR,' SI=',SI

          ELCR(IB1,IB2)= SR
          ELCI(IB1,IB2)= SI
          ELCR(IB2,IB1)= SR
          ELCI(IB2,IB1)=-SI

        END DO !IB2
      END DO !IB1
      IHP1(1)=IHPB
      IHP2(1)=IHPB

      END SUBROUTINE CHRG
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Write information on electron occupations of 
C     eigen function(s) of the state NST.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MAGT(NST,ORBM
     &               ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &               ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &               ,NBGM,VS,NMODE, NBSM,T1,T2
     &               ,ASTS,ACI,AOBT)
      IMPLICIT NONE 

C Parameters
      INTEGER   LMAX2,  NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NLBLM
      PARAMETER(NLBLM=10)
C -- Note: These operator number must be continuous.
      INTEGER N_SZ,N_SP,N_LZ,N_LP,N_TZ,N_TP,N_JZ,N_JP
      PARAMETER(N_SZ=50,N_SP=51,N_LZ=52,N_LP=53
     &         ,N_TZ=54,N_TP=55,N_JZ=56,N_JP=57)

C Dummy arguments
      INTEGER NST
      REAL*8  ORBM(0:NOBTM*7)
 
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      INTEGER NSTM,NCIM
      REAL*8  ERED(NCIM)

      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      INTEGER NBGM
      REAL*8  VS(NBGM)

      INTEGER NMODE

      INTEGER NBSM 
      REAL*8  T1(NBSM), T2(NBSM)

C Global variables
      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)
C
      INTEGER IERR
      COMMON /SYS/IERR

C Local varialbes (static
      INTEGER   NBLKM
      PARAMETER(NBLKM=200)
      REAL*8  R(NBLKM)
      INTEGER IOU(2)
      DATA IOU/2,6/
C
      INTEGER I,I1,I2,NOP,NHPTOP,N1,NCPX,NEXL,IO,IOM1,IOM2,NBLK
      REAL*8 S,T,TSS,TLL,TJJ,RN,ROOT3
      REAL*8  SS,  SX,  SY,  SZ,  S2,  SX2,  SZ2,  SXY,  SZX,  SYZ
      REAL*8 RLL, RLX, RLY, RLZ, RL2, RLX2, RLZ2, RLXY, RLZX, RLYZ
      REAL*8 RJJ, RJX, RJY, RJZ, RJ2, RJX2, RJZ2, RJXY, RJZX, RJYZ
      REAL*8  TX,  TY,  TZ
C -------------------
 
      WRITE(6,*)'enter MAGT' 

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)

      N1=NSFT(NCI(NST)+1,NST)
      CALL ABSV(N1,VS,RN)
      IF ( RN .LT. 1D-12 ) THEN
        WRITE(6,*)' Size zero!! |VS|=',RN
        RETURN
      END IF
      RN=1D0/RN**2
      ROOT3=SQRT(3D0)
C-------------------------------------------------------------
C  Set up Operators L, S and T
C-------------------------------------------------------------
      NHPTOP=IHP1(1)
      NBLK=6
C
      CALL CLRV(NBLK,R)
      R(2)=1D0
      CALL OP1GEN(N_SZ,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(3)=1D0
      CALL OP1GEN(N_SP,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(2)=-2D0
      R(5)= 1D0
      CALL OP1GEN(N_LZ,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(3)=-2D0
      R(6)= 1D0
      CALL OP1GEN(N_LP,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(3)= 1D0
      CALL OP1GEN(N_TZ, 3,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(4)= 1D0
      CALL OP1GEN(N_TP, 3,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(2)=-1D0
      R(5)= 1D0
      CALL OP1GEN(N_JZ,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL CLRV(NBLK,R)
      R(3)=-1D0
      R(6)= 1D0
      CALL OP1GEN(N_JP,11,NST,ORBM,NBLK,R,NHOP,NHM,H,IHP1,IHP2
     &           ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
C
      CALL WATCH('HGEN    ')
      DO NOP=N_SZ,N_JP
        CALL HGEN (NBSM,T1,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ERED
     &            ,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG,NMODE)
      END DO
      CALL WATCH('END     ')
C
      CALL WATCH('MAGCRR  ')
      TSS=0D0
      TLL=0D0
      TJJ=0D0
      DO I1=1,NINT( ORBM(0) )
        IOM1= NINT ( ORBM(I1) )
C
C-----------------------------------
C  Calc. <Sx>, <Sy> and <Sz>
C-----------------------------------
        IF (  MOD( NDEG(IOM1),2) .EQ. 0 ) THEN
          CALL CLRV(N1,T1)
          CALL OPRT(N1,T1,N1,VS,N_SP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)

          CALL MULCV(N1,T1,VS,SX,SY,NCPX)
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_SP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,SX2,SXY,NCPX)

          CALL CLRV (N1,T2)
C !Cation S^-
          CALL  OPRT(N1,T2,N1,T1,N_SP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
          CALL MULCV(N1,T2,VS,S2,S,NCPX)

          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_SZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,SZX,SYZ,NCPX)
C----
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_SZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,SZ,S,NCPX)
          S2=S2+SZ
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_SZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          SZ2=(2D0*S-S2)/ROOT3
          S2 =S2+S
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_SP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          SZX=SZX+S
          SYZ=SYZ+T
          TSS=TSS+S2
        END IF
C-----------------------------------
C  Calc. <Lx>, <Ly> and <Lz>
C-----------------------------------
        IF (  MOD( NDEG(IOM1),4) .EQ. 2
     &   .OR. MOD( NDEG(IOM1),2) .EQ. 1 ) THEN
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_LP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,RLX,RLY,NCPX)
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_LP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,RLX2,RLXY,NCPX)
          CALL CLRV (N1,T2)
C !Cation L^-
          CALL  OPRT(N1,T2,N1,T1,N_LP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
          CALL MULCV(N1,T2,VS,RL2,S,NCPX)

          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_LZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,RLZX,RLYZ,NCPX)
C----
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_LZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,RLZ,S,NCPX)
          RL2=RL2+RLZ
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_LZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          RLZ2=(2D0*S-RL2)/SQRT(3D0)
          RL2 =RL2+S
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_LP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          RLZX=RLZX+S
          RLYZ=RLYZ+T
          TLL=TLL+RL2
        END IF
C-----------------------------------
C  Calc. <Jx>, <Jy> and <Jz>
C-----------------------------------
        IF ( MOD( NDEG(IOM1),4) .EQ. 2) THEN
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_JP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,RJX,RJY,NCPX)
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_JP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,RJX2,RJXY,NCPX)
          CALL CLRV (N1,T2)
C !Cation J^-
          CALL  OPRT(N1,T2,N1,T1,N_JP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
          CALL MULCV(N1,T2,VS,RJ2,S,NCPX)

          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_JZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,RJZX,RJYZ,NCPX)
C----
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_JZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,RJZ,S,NCPX)
          RJ2=RJ2+RJZ
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_JZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          RJZ2=(2D0*S-RJ2)/ROOT3
          RJ2 =RJ2+S
          CALL  CLRV(N1,T2)
          CALL  OPRT(N1,T2,N1,T1,N_JP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T2,VS,S,T,NCPX)
          RJZX=RJZX+S
          RJYZ=RJYZ+T
          TJJ=TJJ+RJ2
        END IF
C-----------------------------------
C  Calc. <Tx>, <Ty> and <Tz>
C-----------------------------------
        IF ( MOD( NDEG(IOM1),4) .EQ. 2 ) THEN
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_TP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,TX,TY,NCPX)
C
          CALL  CLRV(N1,T1)
          CALL  OPRT(N1,T1,N1,VS,N_TZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
          CALL MULCV(N1,T1,VS,TZ,T,NCPX)
        END IF
C
        DO I=1,2
          IO=IOU(I)

            WRITE(IO,*)
     &    '-----------------------------------------------------------'
          WRITE(IO,*)' Orbit ==>  '//AOBT(IOM1)
          IF ( MOD( NDEG(IOM1),2) .EQ. 0 ) THEN
            WRITE(IO,'(3(A5,G15.7))')
     &        ' Sx=',SX *RN,' Sy=',SY *RN,' Sz=',SZ *RN
          END IF

          IF ( MOD( NDEG(IOM1),4) .EQ. 2 
     &   .OR. MOD( NDEG(IOM1),2) .EQ. 1 ) THEN
            WRITE(IO,'(3(A5,G15.7))')
     &      ' Lx=',RLX*RN,' Ly=',RLY*RN,' Lz=',RLZ*RN
          END IF

          IF ( MOD( NDEG(IOM1),4) .EQ. 2 ) THEN
            WRITE(IO,'(3(A5,G15.7))')
     &      ' Mx=',-(2D0*SX+RLX)*RN,' My=',-(2D0*SY+RLY)*RN
     &     ,' Mz=',-(2D0*SZ+RLZ)*RN
            WRITE(IO,'(3(A5,G15.7))')
     &      ' Tx=',TX *RN,' Ty=',TY *RN,' Tz=',TZ *RN
          END IF

          IF ( MOD( NDEG(IOM1),2) .EQ. 0 ) THEN
            WRITE(IO,*)  '   '
            WRITE(IO,'(3(A6,G15.7))')
     &      ' S^2 =',S2 *RN
            WRITE(IO,'(3(A12,G15.7))')
     & ' SySz+SzSy=',SYZ*RN,' SzSx+SxSz=',SZX*RN,' SxSy+SySx=',SXY*RN
            WRITE(IO,'(3(A12,G15.7))')
     &      '3Sx^2-S^2=', 0.5D0*(ROOT3*SX2-SZ2)*RN * ROOT3
     &     ,' 3Sy^2-S^2=',-0.5D0*(ROOT3*SX2+SZ2)*RN * ROOT3
     &     ,' 3Sz^2-S^2=',                  SZ2 *RN * ROOT3
            WRITE(IO,'(3(A12,G15.7))')
     &      ' Sy^2-Sz^2=',-0.5D0*(SX2+ROOT3*SZ2)*RN
     &     ,' Sz^2-Sx^2=',-0.5D0*(SX2-ROOT3*SZ2)*RN
     &     ,' Sx^2-Sy^2=',        SX2*RN
          END IF

          IF ( MOD( NDEG(IOM1),4) .EQ. 2
     &   .OR. MOD( NDEG(IOM1),2) .EQ. 1 ) THEN
            WRITE(IO,*)  '   '
            WRITE(IO,'(3(A6,G15.7))')
     &      ' L^2 =',RL2 *RN
            WRITE(IO,'(3(A12,G15.7))')
     &' LyLz+LzLy=',RLYZ*RN,' LzLx+LxLz=',RLZX*RN,' LxLy+LyLx=',RLXY*RN
            WRITE(IO,'(3(A12,G15.7))')
     &      ' 3Lx^2-L^2=', 0.5D0*(ROOT3*RLX2-RLZ2)*RN * ROOT3
     &     ,' 3Ly^2-L^2=',-0.5D0*(ROOT3*RLX2+RLZ2)*RN * ROOT3
     &     ,' 3Lz^2-L^2=',                   RLZ2 *RN * ROOT3
            WRITE(IO,'(3(A12,G15.7))')
     &      ' Ly^2-Lz^2=',-0.5D0*(RLX2+ROOT3*RLZ2)*RN
     &     ,' Lz^2-Lx^2=',-0.5D0*(RLX2-ROOT3*RLZ2)*RN
     &     ,' Lx^2-Ly^2=',        RLX2*RN
          END IF

          IF ( MOD( NDEG(IOM1),4) .EQ. 2 ) THEN
            WRITE(IO,*)  '   '
            WRITE(IO,'(3(A6,G15.7))')
     &      ' J^2 =',RJ2 *RN
            WRITE(IO,'(3(A12,G15.7))')
     &' JyJz+JzJy=',RJYZ*RN,' JzJx+JxJz=',RJZX*RN,' JxJy+JyJx=',RJXY*RN
            WRITE(IO,'(3(A12,G15.7))')
     &      ' 3Jx^2-J^2=', 0.5D0*(ROOT3*RJX2-RJZ2)*RN * ROOT3
     &     ,' 3Jy^2-J^2=',-0.5D0*(ROOT3*RJX2+RJZ2)*RN * ROOT3
     &     ,' 3Jz^2-J^2=',                   RJZ2 *RN * ROOT3
            WRITE(IO,'(3(A12,G15.7))')
     &      ' Jy^2-Jz^2=',-0.5D0*(RJX2+ROOT3*RJZ2)*RN
     &     ,' Jz^2-Jx^2=',-0.5D0*(RJX2-ROOT3*RJZ2)*RN
     &     ,' Jx^2-Jy^2=',        RJX2*RN
          END IF

        END DO
      END DO
C---------------------------------------------------------
C  Magnetic Correlation Functions
C---------------------------------------------------------
      DO I1=1,NINT( ORBM(0) )
        IOM1=NINT( ORBM(I1) )
        DO I2=I1+1,NINT ( ORBM(0) )
          IOM2=NINT( ORBM(I2) )
C---------------------------------
C  Calculate < S(2) dot S(1) >
C---------------------------------
          IF( MOD(NDEG(IOM1),2).EQ.0 .AND. MOD(NDEG(IOM2),2).EQ.0 )THEN
            CALL CLRV(N1,T1)
C <S+(2)S-(1)>     
            CALL CLRV(N1,T2)
C  !Cation S^-
            CALL OPRT(N1,T2,N1,VS,N_SP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
            CALL OPRT(N1,T1,N1,T2,N_SP,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
C <S-(2)S+(1)>     
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_SP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
C  !Cation S^-
            CALL OPRT(N1,T1,N1,T2,N_SP,0,0,IOM2,NHOP,NHM,H,IHP2,IHP1)
            CALL MULS(N1,T1,0.5D0)
C <Sz(2)Sz(1)>
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_SZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
            CALL OPRT(N1,T1,N1,T2,N_SZ,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
            CALL MULCV(N1,T1,VS,SS,T,NCPX)
          END IF
C---------------------------------
C  Calculate < L(2) dot L(1) >
C---------------------------------
          IF( MOD(NDEG(IOM1),4).EQ.2 .AND. MOD(NDEG(IOM2),4).EQ.2
     &  .OR.MOD(NDEG(IOM1),2).EQ.1 .AND. MOD(NDEG(IOM2),2).EQ.1 )THEN
            CALL CLRV(N1,T1)
C <L+(2)L-(1)>     
            CALL CLRV(N1,T2)
C  !Cation L^-
            CALL OPRT(N1,T2,N1,VS,N_LP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
            CALL OPRT(N1,T1,N1,T2,N_LP,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
C <L-(2)L+(1)>     
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_LP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
C  !Cation L^-
            CALL OPRT(N1,T1,N1,T2,N_LP,0,0,IOM2,NHOP,NHM,H,IHP2,IHP1)
            CALL MULS(N1,T1,0.5D0)
C <Lz(2)Lz(1)>
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_LZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
            CALL OPRT(N1,T1,N1,T2,N_LZ,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
            CALL MULCV(N1,T1,VS,RLL,T,NCPX)
          END IF
C---------------------------------
C  Calculate < J(2) dot J(1) >
C---------------------------------
          IF( MOD(NDEG(IOM1),4).EQ.2 .AND. MOD(NDEG(IOM2),4).EQ.2 )THEN
            CALL CLRV(N1,T1)
C <J+(2)J-(1)>     
            CALL CLRV(N1,T2)
C  !Cation J^-
            CALL OPRT(N1,T2,N1,VS,N_JP,0,0,IOM1,NHOP,NHM,H,IHP2,IHP1)
            CALL OPRT(N1,T1,N1,T2,N_JP,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
C <J-(2)J+(1)>     
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_JP,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
C  !Cation J^-
            CALL OPRT(N1,T1,N1,T2,N_JP,0,0,IOM2,NHOP,NHM,H,IHP2,IHP1)
            CALL MULS(N1,T1,0.5D0)
C <Jz(2)Jz(1)>
            CALL CLRV(N1,T2)
            CALL OPRT(N1,T2,N1,VS,N_JZ,0,0,IOM1,NHOP,NHM,H,IHP1,IHP2)
            CALL OPRT(N1,T1,N1,T2,N_JZ,0,0,IOM2,NHOP,NHM,H,IHP1,IHP2)
            CALL MULCV(N1,T1,VS,RJJ,T,NCPX)
          END IF
C    
          IF( MOD(NDEG(IOM1),4).EQ.2 .AND. MOD(NDEG(IOM2),4).EQ.2 )THEN

            DO I=1,2
              IO=IOU(I)
              WRITE(IO,*)
     &     '----------------------------------------------------------'
              WRITE(IO,*)' Orbit #1 ==>'//AOBT(IOM1)
     &                 //' Orbit #2 ==>'//AOBT(IOM2)
              WRITE(IO,'(3(A10,G15.7))')
     &     '<S_2 S_1>=',SS*RN,'<L_2 L_1>=',RLL*RN,'<J_2 J_1>=',RJJ*RN
            END DO
            TLL=TLL+2D0*RLL
            TJJ=TJJ+2D0*RJJ
            TSS=TSS+2D0*SS

          ELSE 

            IF(   MOD(NDEG(IOM1),2).EQ.0 
     &      .AND. MOD(NDEG(IOM2),2).EQ.0 )THEN
              DO I=1,2
                IO=IOU(I)
                WRITE(IO,*)
     &     '----------------------------------------------------------'
                WRITE(IO,*)' Orbit #1 ==>'//AOBT(IOM1)
     &                   //' Orbit #2 ==>'//AOBT(IOM2)
                WRITE(IO,'(3(A10,G15.7))') '<S_2 S_1>=',SS*RN
              END DO
              TSS=TSS+2D0*SS
            END IF

            IF(   MOD(NDEG(IOM1),2).EQ.1 
     &      .AND. MOD(NDEG(IOM2),2).EQ.1 )THEN
              DO I=1,2
                IO=IOU(I)
                WRITE(IO,*)
     &     '----------------------------------------------------------'
                WRITE(IO,*)' Orbit #1 ==>'//AOBT(IOM1)
     &                   //' Orbit #2 ==>'//AOBT(IOM2)
                WRITE(IO,'(3(A10,G15.7))') '<L_2 L_1>=',RLL*RN
              END DO
              TLL=TLL+2D0*RLL
            END IF

          END IF
C      
        END DO
      END DO
C
      DO I=1,2
        IO=IOU(I)
        WRITE(IO,*)
     &  '----------------------------------------------------------'
        WRITE(IO,*)
     &  'Total moments over the orbitals specified in "Mag" command.'
        WRITE(IO,'(3(A10,G15.7))')
     &  '<S_2 S_1>=',TSS*RN,'<L_2 L_1>=',TLL*RN,'<J_2 J_1>=',TJJ*RN
      END DO
C
      IHP1(1)=NHPTOP
      IHP2(1)=NHPTOP
      CALL WATCH('END     ')
C
      END SUBROUTINE MAGT

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Create one-body operator
C    
C   After you use this subroutine, do not forget remove matrix-elements
C   created by this routine in H, IHP1 and IHP2 
C   if you want to save memory.
C   To do this, keep the maxium pointer value stored in IHP1(1)
C   ( or IHP2(1) ) before using this subroutine and after the 
C   execution of this subroutine, restore the value to both the 
C   IHP1(1) and IHP2(1).
C
C    NOP:  operator number
C    NTYP: type of the operator (defined in HGEN)
C    NST:  state number
C    IOM:  orbital where the operator act
C    ISB:  subscript of the operator
C    R(NBLK): parameters for the operator
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OP1GEN(NOP,NTYP,NST,ORBM,NBLK,RR
     &                 ,NHOP,NHM,H,IHP1,IHP2
     &                 ,NSTM,NCIM,NCI,NOBT,NELC,NDEG)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX2,  NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NBLKM
      PARAMETER(NBLKM=200)

C Dummy arguments
      INTEGER NOP, NTYP, NST
      REAL*8  ORBM(0:NOBTM*7)
      INTEGER NBLK
      REAL*8  RR(NBLKM) 
      
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

C Local arguments
      REAL*8  RI(NBLKM)
      INTEGER IARG(8)
      INTEGER I,ICI,IOM,NEL,NELMAX,IHP
      INTEGER NBDY,ICPX,NEX,LIN
C ----------------
C
      IHP=IHP1(1)
      NHOP(NOP)=IHP

      NBDY=1
      ICPX=0
      NEX=1
      LIN=0

      IARG(1)=NST
      IARG(2)=NST

      DO I=1,NBLK
        RI(I)=0D0
      END DO

      DO I=1,NINT( ORBM(0) )
        IOM= NINT ( ORBM(I) )
        NELMAX=NDEG(IOM)
        IARG(5)=IOM
        IARG(6)=IOM
        IARG(7)=IOM
        IARG(8)=IOM
        DO ICI=1,NCI(NST)
          NEL=NELC(IOM,ICI,NST)
          IF ( ( NTYP.NE.3 .AND. NTYP.NE.11 )
     &    .OR. ( NEL.NE.0 .AND. NEL.NE.NELMAX ) ) THEN
            IARG(3)=ICI
            IARG(4)=ICI
            CALL PUTBLK(IHP,NHOP,NHM,H,IHP1,IHP2
     &                 ,NTYP,NBDY,NSTM,NCIM,NOBT,NELC
     &                 ,IARG,NBLK,RR,RI,NEX,IOM,ICPX,LIN)
          END IF
        END DO
      END DO
      IF (IHP.GT.NHM) THEN
        WRITE(6,*)'The interaction block area overflowed'
     &   //' at the subroutine OP1GEN.'
        WRITE(6,'(A,I10,A,I10)')
     &      'The value of the parameter NHM=',NHM,
     &      ' must be enhanced more than ',IHP
        STOP
      END IF
      NHOP(NOP+1)=IHP
      IHP1(1)=IHP
      IHP2(1)=IHP

      END SUBROUTINE OP1GEN

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  The expectation value of the 
C  rotation operator C(PHI) around the z-axis
C  with a rotation angle PHI 
C  <T1| C(PHI) |T1>
C  This would be useful for analying the symmetry of 
C  wavefunctions.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPRT_C(NOP,NSTM,NCIM,NCI,NSFT,NOBT,NELC,NDEG
     &                 ,PHI,NBSM,T1,T2,T3,TR,TI,NMODE)
      IMPLICIT NONE

C Parameters
      INTEGER   LMAX2, NEMAX
      PARAMETER(LMAX2=3,NEMAX=4*LMAX2+2)
 
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

C Dummy arguments
      INTEGER NOP

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT
      INTEGER NELC(NOBTM,NCIM,NSTM)
      INTEGER NDEG(NOBTM)

      REAL*8  PHI

      INTEGER NBSM
      REAL*8  T1(NBSM),T2(NBSM),T3(NBSM)

      REAL*8  TR,TI
      INTEGER NMODE

C Global varaiable
      INTEGER IBRN(0:2**NEMAX-1),ICNT(0:2**NEMAX-1)
      COMMON /BIT/IBRN,ICNT  

C Local variable
      INTEGER ICNF(4000)
      REAL*8 CR(0:4*LMAX2+1), CI(0:4*LMAX2+1)
      INTEGER N,I,J,K,I1,I2,ICI,IO,IOBT,IS,ID,L1,M1,N1,N2,IBITS
      INTEGER NCNF,NSTP1,NSTP2,NCPX,NEXL,NC,IBS
      REAL*8 T

C Function
      INTEGER IBSIZE
C --------------

      DO IOBT=1,NOBT
        IF (MOD( NDEG(IOBT), 4) .NE. 2 ) THEN
          TR=0D0
          TI=0D0
          RETURN
        END IF
      END DO

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
      N=NSFT(NCI(NOP)+1,NOP)
      IF (NCPX.NE.0) N=N/2
C
      IF (NCPX.EQ.0) THEN
        CALL MOVV(N,T2,T1)
        CALL CLRV(N,T3)
      ELSE 
        DO I=1,N
          T2(I)=T1(I*2-1) 
          T3(I)=T1(I*2)
        END DO
      END IF
C
      DO IOBT=1,NOBT
        L1=NDEG(IOBT)/4
        DO M1=-L1,L1
          DO IS=0,1
            CR(2*(M1+L1)+IS)=COS( PHI*(DBLE(M1)+DBLE(IS)-0.5D0) )
            CI(2*(M1+L1)+IS)=SIN( PHI*(DBLE(M1)+DBLE(IS)-0.5D0) )
          END DO
        END DO
        DO ICI=1,NCI(NOP)
C
          CALL CNFG(4*L1+2,NELC(IOBT,ICI,NOP),NCNF,ICNF)
          N1=1
          DO IO=1,IOBT-1
            N1=N1*IBSIZE(IO,ICI,NOP,NSTM,NCIM,NOBT,NELC,NDEG)
          END DO
          NC=N1
          NSTP1=1
C
          N2=1
          DO IO=IOBT+1,NOBT
            N2=N2*IBSIZE(IO,ICI,NOP,NSTM,NCIM,NOBT,NELC,NDEG)
          END DO
          NSTP2=N1*NCNF
C
          IF (N2/2.GT.N1) THEN
            I=N2
            J=NSTP2
            N2=N1
            NSTP2=NSTP1
            N1=I
            NSTP1=J
          END IF
C
          DO I=1,NCNF
            K=ICNF(I)
            TR=1D0
            TI=0D0
*POPTION NOPARALLEL
*VOPTION NOVEC
            DO IBS=0,4*L1+1
              IBITS=2**IBS
              IF (IAND(K,IBITS).EQ.IBITS) THEN
                T =TR
                TR=TR*CR(IBS)-TI*CI(IBS)
                TI=T *CI(IBS)+TI*CR(IBS)
              END IF
            END DO
            ID=(I-1)*NC
            K=NSFT(ICI,NOP)
            IF (NCPX.NE.0) K=K/2
            DO I2=K+1,K+(N2-1)*NSTP2+1,NSTP2
              DO I1=I2+ID,I2+ID+(N1-1)*NSTP1,NSTP1
                T=T2(I1)
                T2(I1)=TR*T-TI*T3(I1)
                T3(I1)=TI*T+TR*T3(I1)
              END DO 
            END DO
          END DO 
        END DO
      END DO 

      IF (NCPX.EQ.0) THEN
        CALL MULV(N,T1,T2,TR)
        CALL MULV(N,T1,T3,TI)
      ELSE
        DO I=N,1,-1
          T2(2*I-1)=T2(I)
          T2(2*I)  =T3(I)
        END DO
        CALL MULCV(2*N,T2,T1,TR,TI,NCPX)
      END IF
C
      END SUBROUTINE OPRT_C

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  System status
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SSTAT(NSTM,NCIM,NCI,NSFT,NOBT,NOBTX,NHOP
     &                ,NGM,NG,NHM,NHDM,NBGM,NBMM,NBFM,NBSM,NMODE,MODE
     &                ,ASTS,ACI,AOBT)
      IMPLICIT NONE

C Parameters
      INTEGER   NSTM2
      PARAMETER(NSTM2=100)

      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      INTEGER   NDLTM
      PARAMETER(NDLTM=200)

      INTEGER   NEPHM,     NEANGM,   NDCRM
      PARAMETER(NEPHM=1000,NEANGM=20,NDCRM=1200)

      INTEGER   NPMAX,     NRECM
      PARAMETER(NPMAX=4000,NRECM=2000)

      INTEGER   LMAX,  LMAX2,  KMAX,       NEMAX
      PARAMETER(LMAX=8,LMAX2=3,KMAX=2*LMAX,NEMAX=4*LMAX2+2)

      INTEGER    NALFM,   NMAXR 
      PARAMETER(NALFM=1,NMAXR=1)

C Dummy arguments
      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM) 
      INTEGER NSFT(NCIM+1,NSTM)
      INTEGER NOBT, NOBTX
      INTEGER NHOP(0:NHOPM)
      INTEGER NGM, NG
      INTEGER NHM,NHDM,NBGM,NBMM,NBFM,NBSM
      CHARACTER*(8) MODE
      INTEGER NMODE

C Global variables 
C -- orbital & configuration lebels
      INTEGER NLBLM
      PARAMETER(NLBLM=10)
      CHARACTER*(NSTM2) ASTS
      CHARACTER*(NLBLM) ACI(NSTM,NCIM)
      CHARACTER*(NLBLM) AOBT(NOBTM)
C -- CPU time for each tasks
      INTEGER NTSKM
      PARAMETER(NTSKM=30)
      INTEGER NTSK, NTST
      CHARACTER*(8) TSK(NTSKM)
      REAL*8  TTSK(NTSKM)
      INTEGER ITSK(NTSKM)
      COMMON /WTM/NTSK,NTST,TSK,TTSK,ITSK
C -- Statistical infomation of loops 
      INTEGER NLP1 (NHOPM), NLP2 (NHOPM), NLP3 (NHOPM)
      REAL*8  ELLP1(NHOPM), ELLP2(NHOPM), ELLP3(NHOPM)
      REAL*8  AVLI1(NHOPM), AVLI2(NHOPM), AVLI3(NHOPM)
      REAL*8  AVLO1(NHOPM), AVLO2(NHOPM)

      COMMON /OPRSTS/NLP1 ,NLP2, NLP3
     &              ,ELLP1,ELLP2,ELLP3
     &              ,AVLI1,AVLI2,AVLI3
     &              ,AVLO1,AVLO2

      INTEGER NBMPK
      INTEGER NBWRK(NHOPM) 
      COMMON /NBM/NBMPK,NBWRK

      INTEGER NFREQ(NHOPM)
      COMMON /FREQ/NFREQ
      SAVE /WTM/,/NBM/,/FREQ/

C Local variables
      INTEGER I, NCPX, NEXL, IO, IOP
      REAL*8  T
C -----

      NCPX=MOD(NMODE   ,10)
      NEXL=MOD(NMODE/10,10)
      WRITE(2,*)
      WRITE(2,*)'=============== System parameters ================ '
      WRITE(2,
     &'(''NHM: size of matrices data storage                 ='',I10)'
     &   )NHM
      WRITE(2,
     &'(''               actual consumption of the storage  =>'',I10)'
     &   )NBMPK
      WRITE(2,
     &'(''NBGM: max. number of basis for initial state       ='',I10)'
     &   )NBGM
      WRITE(2,
     &'(''NBSM:  max. number of basis for all state          ='',I10)'
     &   )NBSM
      WRITE(2,
     &'(''NBMM: max. number of basis for intermediate stat   ='',I10)'
     &   )NBMM
      WRITE(2,
     &'(''NBFM: max. number of basis for final state         ='',I10)'
     &   )NBFM
      WRITE(2,*)'         (RPESg or RPESfg modes only)'
      WRITE(2,
     &'(''NHDM: max. number of diagonal elements             ='',I10)'
     &   )NHDM
      WRITE(2,
     &'(''NGM: max. number of initial vectors                ='',I10)'
     &   )NGM
      WRITE(2,
     &'(''NCIM:  max. number of configurations               ='',I10)'
     &   )NCIM
      WRITE(2,
     &'(''NOBTM: max. number of orbitals                     ='',I10)' 
     &   )NOBTM
      WRITE(2,
     &'(''NSTM:  max. number of states                       ='',I10)'
     &   )NSTM
      WRITE(2,
     &'(''NRECM: max. number of iteration in recursion method='',I10)' 
     &   )NRECM
      WRITE(2,
     &'(''NPMAX: max. number of points in spectral data      ='',I10)'
     &   )NPMAX
      WRITE(2,
     &'(''NLBLM: max. length of configuration label          ='',I10)'
     &   )NLBLM
      WRITE(2,
     &'(''NMAXR: max. number of DM basis                     ='',I10)'
     &   )NMAXR
      WRITE(2,
     &'(''NALFM: max. number of reduced basis                ='',I10)'
     &   )NALFM
      WRITE(2,*)'================ Present status ================'

      IO=NOBT
C>>>START F77
      IO=NOBTM 
C>>>END F77
      T=DBLE(NHM)*2d0+DBLE(NHDM)+DBLE(NBSM)*2d0+DBLE(NGM)*DBLE(NBGM)
     & +DBLE(NBMM)*14d0+DBLE(NBFM)*3d0
     & +0.5d0*(DBLE(IO)+1d0+DBLE(NLBLM)/8d0)*DBLE(NCIM)*DBLE(NSTM)
     & +DBLE(NPMAX)*4d0+DBLE(NRECM)*4d0
      T=T*8d0/1024d0**2 + 7d0
      IF ( T .GT. 1024D0 ) THEN
        T=T/1020D0
        WRITE(2,'(A,F10.3,A)')
     & 'Estimeted memory consumption for calculations: ',T,' gigabyte'
      ELSE
        WRITE(2,'(A,F7.1,A)')
     & 'Estimeted memory consumption for calculations: ',T,' megabyte'
      END IF

      WRITE(2,*)'  photo process      ===> '//MODE
      WRITE(2,'(''  complex(NCPX>1) or real(NCPX=0) NCPX='',I3)')NCPX
      WRITE(2,
     &   '(''extremely large(NEXL>1) or normal(NEXL=0) NEXL='',I3)')
     &     NEXL
      WRITE(2,'(''  number of orbitals (in solid )='',I3)') NOBT
      WRITE(2,'(''  number of orbitals (continuum)='',I3)') NOBTX-NOBT
      WRITE(2,'(''  number of states              ='',I3)') NCI(0)
      WRITE(2,'(''  number of basis(NBS) & of configurations(NCF) '')')
      DO IO=1,NCI(0)
        WRITE(2,'(''    #'',A1,''  NBS='',I10,''  NCF='',I8)')
     &          ASTS(IO:IO),NSFT(NCI(IO)+1,IO),NCI(IO)
      END DO
      WRITE(2,'('' number of initial vectors    =>'',I4)') NG
      WRITE(2,*)' max. use of matrices data storage  =>',NBMPK
      WRITE(2,*)' SIB: size of interaction blocks'
     &        //', SEL: size of elements area'
      WRITE(2,*)' NNZ: number of non-zero elements'
      DO IOP=1,NHOP(0)
        WRITE(2,'(''   Op.#'',I3,''  SIB='',I10,''  SEL='',I10'
     &     //   ',''   NNZ='',F16.0)')
     &     IOP,NHOP(IOP+1)-NHOP(IOP),NBWRK(IOP)
     &     ,ELLP1(IOP)+ELLP2(IOP)+ELLP3(IOP)
      END DO
      WRITE(2,*)' execution freqency of multification loops'
      WRITE(2,*)'  LIN1: linear, step=1, LIN2: linear, step>1'
     &    //' ,NLIN: non-linear'
      WRITE(2,*)'   NELM1, 2 & 3: number of non-zero elements'
      WRITE(2,*)'  INLEN1, 2 & 3: averaged length of inner loops'
      WRITE(2,*)' OUTLEN1  & 2: averaged length of outer loops'
      DO IOP=1,NHOP(0)
        WRITE(2,'(''   Op.#'',I3,''  FREQ='',I10)')IOP,NFREQ(IOP)
        WRITE(2,'(''            LIN1='',I11,''    LIN2='',I11'
     &    //  ',''    NLIN='',I11)')
     &     NLP1(IOP),NLP2(IOP),NLP3(IOP)
        WRITE(2,'(''           NELM1='',F11.0,''   NELM2='',F11.0'
     &    //  ',''   NELM3='',F11.0)') ELLP1(IOP),ELLP2(IOP),ELLP3(IOP)
        WRITE(2,'(''          INLEN1='',F11.1,''  INLEN2='',F11.1'
     &    //  ',''  INLEN3='',F11.1)') AVLI1(IOP),AVLI2(IOP),AVLI3(IOP)
        WRITE(2,'(''         OUTLEN1='',F11.1,'' OUTLEN2='',F11.1)')
     &     AVLO1(IOP),AVLO2(IOP)
      END DO
C
      WRITE(2,*)' CPU time of each task'
      DO I=1,NTSK
        WRITE(2,'(''   TASK =>'',A8,'' TIME='',F16.7'
     &     //   ',''(sec)  COUNT='',I6)') 
     &     TSK(I),TTSK(I),ITSK(I)   
      END DO

      END SUBROUTINE SSTAT

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   CPU time 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE WATCH(COM)
      IMPLICIT NONE 

C Parameter
      INTEGER   NTSKM
      PARAMETER(NTSKM=30)

C Dummy arguments 
      CHARACTER*(8) COM

C Global variables
      INTEGER NTSK, NTST
      CHARACTER*(8) TSK(NTSKM)
      REAL*8  TTSK(NTSKM)
      INTEGER ITSK(NTSKM)
      COMMON /WTM/NTSK,NTST,TSK,TTSK,ITSK

C Local variables
      INTEGER I
      REAL*8 T
      LOGICAL HIT
C If you don't want to use 'COMMON' statement, use these two lines
C below instead of above COMMON statement.
C  vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
C      SAVE NTSK,NTST,TSK,TTSK,ITSK
C      DATA NTSK/0/,NTST/NTSKM/
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      IF (COM.EQ.'END') THEN
        CALL CPU_TIME(T)
        WRITE(6,'('' end of '',A8,''  time='',F16.7,''sec'')')
     &         TSK(NTST),T-TTSK(NTST)
        HIT=.FALSE.
        DO I=1,NTSK
          HIT = TSK(I).EQ.TSK(NTST)
          IF (HIT) EXIT
        END DO
        IF (HIT) THEN 
          TTSK(I)=TTSK(I)+T-TTSK(NTST)
          ITSK(I)=ITSK(I)+1
          NTST=NTST+1
        ELSE 
          NTSK=NTSK+1
          TSK(NTSK)=TSK(NTST)
          ITSK(NTSK)=1
          TTSK(NTSK)=T-TTSK(NTST)
          NTST=NTST+1
        END IF
      ELSE
        NTST=NTST-1
        TSK(NTST)=COM
        WRITE(6,'('' start of '',A8)')COM
        CALL CPU_TIME(TTSK(NTST))
      END IF

      END SUBROUTINE WATCH

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   get CPU time 
C    (this subroutine depends machine)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C////////////////////////////////////////
C//   complex vector lib.   (type I)   //
C////////////////////////////////////////
C
C
      SUBROUTINE EXCS(N,V,U,S)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  V(N),U(N)
      REAL*8  S

C Local variable
      INTEGER I
C -------

      S=0D0
      DO I=1,N,2
        S=S+V(I)*U(I+1)-V(I+1)*U(I)
      END DO
      END SUBROUTINE EXCS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  add the vector  U
C  multiplied by scalar factor -i S
C  to the vector V.
C  V = V + (i S) * U^*
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EXCV(N,V,U,S)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  V(N),U(N)
      REAL*8  S
C Local variables
      INTEGER I
C
!IBM* ASSERT (NODEPS)
      DO I=1,N,2
        V(I)  =V(I)  +S*U(I+1)
        V(I+1)=V(I+1)-S*U(I)
      END DO
      END SUBROUTINE EXCV

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Complex conjugate of the vector V
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CNJV(N,V,NCPX)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  V(N)
      INTEGER NCPX

C Local variable
      INTEGER I
C -------

      IF (NCPX.NE.0) THEN
        DO I=2,N,2
          V(I)=-V(I)
        END DO
      END IF
      END SUBROUTINE CNJV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  multiply complex constant (SR,SI) to the type I complex vector V
C  and save the result to the vector U 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADCS(N,V,U,SR,SI,NCPX)
      IMPLICIT NONE

C Dummy argumets
      INTEGER N
      REAL*8  V(N),U(N)
      REAL*8  SR,SI
      INTEGER NCPX

C Local variable
      INTEGER I
C -------

      IF (NCPX.EQ.0.OR.SI.EQ.0D0) THEN
        CALL MADS(N,V,U,SR)
      ELSE IF(SR.EQ.0D0) THEN
*VDIR NODEP(V)
cdir$ ivdep
        DO I=1,N,2
          U(I)  =U(I)  -V(I+1)*SI
          U(I+1)=U(I+1)+V(I)  *SI
        END DO 
      ELSE
*VDIR NODEP(V)
cdir$ ivdep
        DO I=1,N,2
          U(I)  =U(I)  + V(I)*SR-V(I+1)*SI
          U(I+1)=U(I+1)+ V(I)*SI+V(I+1)*SR
        END DO
      END IF
      END SUBROUTINE MADCS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  multiply complex constant (SR,SI) to the type I complex vector V
C  and save the result to the V
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULCS(N,V,SR,SI,NCPX)
      IMPLICIT NONE 

C Dummy argumets
      INTEGER N
      REAL*8  V(N)
      REAL*8  SR, SI
      INTEGER NCPX

C Local variables
      INTEGER I
      REAL*8  T
C -------

      IF (NCPX.EQ.0.OR.SI.EQ.0D0) THEN
        CALL MULS(N,V,SR)
      ELSE IF(SR.EQ.0D0) THEN
*VDIR NODEP(V)
cdir$ ivdep
        DO I=1,N,2
          T=V(I)
          V(I)  =-V(I+1)*SI
          V(I+1)=      T*SI
        END DO
      ELSE
*VDIR NODEP(V)
cdir$ ivdep
        DO I=1,N,2
          T=V(I)
          V(I)  = T*SR-V(I+1)*SI
          V(I+1)= T*SI+V(I+1)*SR
        END DO
      END IF
      END SUBROUTINE MULCS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  inner product of type I complex (NCPX=1) or real (NCPX=0) 
C   vector U and V 
C   result: real part ->SR =Re <U|V>, imaginaly part -> SI =Im <U|V> 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULCV(N,V,U,SR,SI,NCPX)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V(N),U(N)
      REAL*8  SR,SI
      INTEGER NCPX

C Local variable
      INTEGER I
C -------

      CALL MULV(N,V,U,SR)
      SI=0D0
      IF (NCPX.NE.0) THEN
        DO I=1,N,2
          SI=SI-V(I)*U(I+1)+V(I+1)*U(I)
        END DO
      END IF
      END SUBROUTINE MULCV

C>>>START RESONANCE
C
C////////////////////////////////////////
C//   complex vector lib.   (type II)  //
C////////////////////////////////////////
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Multiply hamiltonian to a complex vector VS
C and save the result to complex vector VD
C     ists=1---> real    ists=2---> complex
C     er & ei----> energy shift
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULM2(N,VD,VDI,VS,VSI,E,EI
     &             ,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD,ISTS)
      IMPLICIT NONE

C Parameter 
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy argumets
      INTEGER N
      REAL*8  VS(N),VSI(N),VD(N),VDI(N)
      REAL*8  E,EI
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER ISTS

C Local variable
      INTEGER I
C --------------
  
      IF (ISTS.EQ.1) THEN
        DO I=1,N
          VD(I)=(HD(I)-E)*VS(I)
        END DO
        CALL OPRT(N,VD,N,VS,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VD,N,VS,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
      ELSE IF (ISTS.GE.2) THEN
        DO I=1,N
          VD (I)=(HD(I)-E)*VS (I)+EI*VSI(I)
          VDI(I)=(HD(I)-E)*VSI(I)-EI*VS (I)
        END DO
        CALL OPRT(N,VD ,N,VS ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VD ,N,VS ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
        CALL OPRT(N,VDI,N,VSI,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VDI,N,VSI,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
      ELSE
        WRITE(6,*)'  ERROR IN MULM'
      END IF

      END SUBROUTINE MULM2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Multiply hamiltonian + self-energy term to a complex vector 
C |U>=(UR,UI) and *save* it to a complex vector |V>=(VR,VI)
C
C  |V> = ( H - ER - i EI ) |U>
C       - i pi*rho*(1 - i GALF / pi)*(Sum_k T_Ak^dagger T_Ak ) |U>
C
C  NOP:   The operator number of the Hamiltonian H.
C  NOP2:  The (first) operator number of  Auger transtion T_A1.
C  NSHL:  Number of Auger transtion operators T_Ak (k=1,2,...,NSHL)
C  NF(k): The number of the basis functions of the state
C         which is reached by the k-th Auger transtion.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULCM2(N,VR,VI,UR,UI,ER,EI,NOP
     &                 ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                 ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NBSM,T2)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      REAL*8 PI,RHO,EPS
      PARAMETER(PI=3.1415926535D0,RHO=0.0735D0,EPS=1D-8)

C Dummy arguments 
      INTEGER N
      REAL*8  VR(N),VI(N),UR(N),UI(N)
      REAL*8  ER, EI

      INTEGER NOP
 
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)
     
      REAL*8  GALF 
  
      INTEGER NOP2, NSHL
      INTEGER NF(NSHL)
      INTEGER NDEG(NOBTM)
      INTEGER NOBT, NOBTX, ISTS
  
      INTEGER NBSM
      REAL*8  T2(NBSM)

C Local variable
      INTEGER I,ISHL,IO,IO1,IEX,IEX1,N2
      REAL*8 R,T
C ------------------
C write(6,*)'before real part of the self-energy'

      CALL CLRV(N,VR)
      CALL CLRV(N,VI)

      R=RHO*PI
      IF (ISTS.EQ.2) THEN
        DO ISHL=1,NSHL
          N2=NF(ISHL)
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              IO1=IO
              IEX1=IEX
              CALL CLRV(N2,T2)
              CALL OPRT(N2,T2,N,UI, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL MULS(N2,T2,R)
              CALL OPRT(N,VR,N2,T2, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
            END DO
          END DO
        END DO
      END IF

C      write(6,*)'before imaginary part of the self-energy'

      IF (ISTS.EQ.2) THEN
        DO ISHL=1,NSHL
          N2=NF(ISHL)
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              IO1=IO
              IEX1=IEX
              CALL CLRV(N2,T2)
              CALL OPRT(N2,T2,N,UR, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL MULS(N2,T2,-R)
              CALL OPRT(N,VI,N2,T2, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
            END DO
          END DO
        END DO
      END IF

      IF (ABS(GALF).GT.EPS) THEN
         R=GALF/PI
         DO I=1,N
           T=VR(I)
           VR(I)=VR(I)+R*VI(I)
           VI(I)=VI(I)-R*T
         END DO
      END IF

      DO I=1,N
        VR(I)=VR(I)+(HD(I)-ER)*UR(I)+EI*UI(I)
      END DO
      CALL OPRT(N, VR ,N,UR ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N, VR ,N,UR ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
      DO I=1,N
        VI(I)=VI(I)+(HD(I)-ER)*UI(I)-EI*UR(I)
      END DO
      CALL OPRT(N, VI ,N,UI, NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N, VI ,N,UI, NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C
      END SUBROUTINE MULCM2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Multiply hamiltonian + self-energy term to a complex vector 
C |U>=(UR,UI) and add it to a complex vector |V>=(VR,VI)
C
C  |V> =|V>  + ( H - ER - i EI ) |U>
C       - i pi*rho*(1 - i GALF / pi)*(Sum_k T_Ak^dagger T_Ak ) |U>
C
C  NOP:   The operator number of the Hamiltonian H.
C  NOP2:  The (first) operator number of  Auger transtion T_A1.
C  NSHL:  Number of Auger transtion operators T_Ak (k=1,2,...,NSHL)
C  NF(k): The number of the basis functions of the state
C         which is reached by the k-th Auger transtion.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADCM2(N,VR,VI,UR,UI,ER,EI,NOP
     &                 ,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &                 ,GALF,NF,NOP2,NSHL,NDEG,NOBT,NOBTX,ISTS,NBSM,T2)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      REAL*8    PI,               RHO,         EPS
      PARAMETER(PI=3.1415926535D0,RHO=0.0735D0,EPS=1D-8)

C Dummy arguments 
      INTEGER N
      REAL*8  VR(N),VI(N),UR(N),UI(N)
      REAL*8  ER, EI

      INTEGER NOP
 
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      REAL*8  GALF 
  
      INTEGER NOP2, NSHL
      INTEGER NF(NSHL)
      INTEGER NDEG(NOBTM)
      INTEGER NOBT, NOBTX, ISTS
 
      INTEGER NBSM
      REAL*8  T2(NBSM)

C Local variable
      INTEGER I,ISHL,IO,IO1,IEX,IEX1,N2
      REAL*8 R
C ------------------

      DO I=1,N
        VR(I)=VR(I)+(HD(I)-ER)*UR(I)+EI*UI(I)
      END DO
      CALL OPRT(N, VR ,N,UR ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N, VR ,N,UR ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
      DO I=1,N
        VI(I)=VI(I)+(HD(I)-ER)*UI(I)-EI*UR(I)
      END DO
      CALL OPRT(N, VI ,N,UI, NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N, VI ,N,UI, NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)

C write(6,*)'before real part of the self-energy'

      R=RHO*PI
      IF (ABS(GALF).GT.EPS) CALL MADV(N,UI,UR,-GALF/PI) 
      IF (ISTS.EQ.2) THEN
        DO ISHL=1,NSHL
          N2=NF(ISHL)
C         WRITE(6,*)'N2=',N2
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              IO1=IO
              IEX1=IEX
              CALL CLRV(N2,T2)
              CALL OPRT(N2,T2,N,UI, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL MULS(N2,T2,R)
              CALL OPRT(N,VR,N2,T2, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
C             WRITE(6,*)'IO,IEX,ISHL',IO,IEX,ISHL
            END DO
          END DO
        END DO
      END IF
C
      IF (ABS(GALF).GT.EPS) CALL MADV(N,UI,UR,GALF/PI) 

C write(6,*)'before imaginary part of the self-energy'

      IF (ABS(GALF).GT.EPS) CALL MADV(N,UR,UI,GALF/PI) 
      IF (ISTS.EQ.2) THEN
        DO ISHL=1,NSHL
          N2=NF(ISHL)
          DO IO=NOBT+1,NOBTX
            DO IEX=0,NDEG(IO)-1
              IO1=IO
              IEX1=IEX
              CALL CLRV(N2,T2)
              CALL OPRT(N2,T2,N,UR, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP1,IHP2)
              CALL MULS(N2,T2,-R)
              CALL OPRT(N,VI,N2,T2, NOP2 + ISHL-1, IO1,IEX1,0
     &                 ,NHOP,NHM,H,IHP2,IHP1)
            END DO
          END DO
        END DO
      END IF
      IF (ABS(GALF).GT.EPS)  CALL MADV(N,UR,UI,-GALF/PI) 
C
      END SUBROUTINE MADCM2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Multiply imaginary self-energy term to a complex vector VS
C  and add it to a complex vector VD (type II)
C  |VD> =|VD> -i pi rho (1 - i G_alf / pi)  T_A dagger T_A |VS>
C
C  NOP2: operator number of Auger transtion T_A.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GAMM2(N2,N3,VD,VDI,VS,VSI,NOP2,NHOP,NHM,H,IHP1,IHP2
     &                ,NDEG,NOBT,NOBTX,GALF,T1)
      IMPLICIT NONE 

C Parameters
      INTEGER   NOBTM
      PARAMETER(NOBTM=120)

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

      REAL*8    PI,               RHO,         EPS
      PARAMETER(PI=3.1415926535D0,RHO=0.0735D0,EPS=1D-8)

C Dummy arguments 
      INTEGER N2,N3
      REAL*8  VD(N2),VDI(N2),VS(N2),VSI(N2)

      INTEGER NOP2
 
      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)

      INTEGER NDEG(NOBTM)
      INTEGER NOBT, NOBTX
      REAL*8  GALF 
  
      REAL*8 T1(N3)

C Local variable
      INTEGER IO,IO1,IEX,IEX1
C ------------------
C
      DO IO=NOBT+1,NOBTX
        DO IEX=0,NDEG(IO)-1
          IO1=IO
          IEX1=IEX
          CALL CLRV(N3,T1)
          CALL OPRT(N3,T1 ,N3,VS ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP1,IHP2)
          CALL MULS(N3,T1,-RHO*PI)
          CALL OPRT(N2,VDI,N2,T1 ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
          IF (ABS(GALF).GT.EPS) THEN
            CALL MULS(N3,T1, GALF/PI)
            CALL OPRT(N2,VD,N2,T1,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
          END IF
C 
          CALL CLRV(N3,T1)
          CALL OPRT(N3,T1 ,N3,VSI,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP1,IHP2)
          CALL MULS(N3,T1, RHO*PI)
          CALL OPRT(N2,VD ,N2,T1 ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
          IF (ABS(GALF).GT.EPS) THEN
            CALL MULS(N3,T1,-GALF/PI)
            CALL OPRT(N2,VDI,N2,T1,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
          END IF
        END DO !IEX
      END DO !IO

      END SUBROUTINE GAMM2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Multiply imaginary self-energy term to a complex vector VS
C  and add it to a complex vector VD (type II)
C  |VD> =|VD> -i pi rho (1 - i G_alf / pi)  T_A dagger T_A |VS>
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      SUBROUTINE MODGAMM2(N2,N3,VDR,VDI,VSR,VSI,NOP2
C     &                   ,NHOP,NHM,H,IHP1,IHP2
C     &                   ,NDEG,NOBT,NOBTX,GALF,VWRK)
C      IMPLICIT NONE 
C
CC Parameters
C      INTEGER   NOBTM
C      PARAMETER(NOBTM=120)
C
C      INTEGER   NHOPM
C      PARAMETER(NHOPM=100)
C
C      REAL*8    PI,RHO,EPS
C      PARAMETER(PI=3.1415926535D0,RHO=0.0735D0,EPS=1D-8)
C
CC Dummy arguments 
C      INTEGER N2
C      REAL*8  VDR(N2),VDI(N2),VSR(N2),VSI(N2)
C
C      INTEGER NOP2
C 
C      INTEGER NHOP(0:NHOPM)
C      INTEGER NHM
C      REAL*8  H(NHM)
C      INTEGER IHP1(NHM),IHP2(NHM)
C
C      INTEGER NDEG(NOBTM)
C      INTEGER NOBT, NOBTX
C      REAL*8  GALF 
C      REAL*8  VWRK(N2)
C  
C Local variable
C      INTEGER IO,IO1,IOC,IEX,IEX1, N3, NOP
C      INTEGER NBMM
C      REAL*8  T1(NBMM),T2(NBMM)
C      REAL*8 S
CC -----
C      NOP=NHOP(0)
C      WRITE(6,*)'NOP=',NOP
C      WRITE(6,*)'NOP2=',NOP2
C      WRITE(6,*)'N2=',N2
C      WRITE(6,*)'N3=',N3
C      IOC=IHP1(NHOP(NOP)+7)
C      S=DBLE(NDEG(IOC)-2)/DBLE(2*NDEG(IOC))
C      DO IO=NOBT+1,NOBTX
C        DO IEX=0,NDEG(IO)-1
C          IO1=IO
C          IEX1=IEX
CC P_j=5/2
C          CALL CLRV(N2,VWRK)
C          CALL OPRT(N2,VWRK ,N2,VSR ,NOP,0,0,IOC,NHOP,NHM,H,IHP1,IHP2) 
C          CALL OPRT(N2,VWRK ,N2,VSR ,NOP,0,0,IOC,NHOP,NHM,H,IHP2,IHP1)
C          CALL MADS(N2,VWRK, VSR, S )
CC T_Auger
C          CALL CLRV(N3,VSR)
C          CALL OPRT(N3,VSR ,N3,VWRK ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP1,IHP2)
CC T_Auger^dagger^
C          CALL CLRV(N2,VWRK)
C          CALL OPRT(N2,VWRK,N2, VSR ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
CC P_j=5/2
C          CALL CLRV(N3,VSR)
C          CALL OPRT(N2,VDI,N2,T1 ,NOP,0,0,IOC,NHOP,NHM,H,IHP1,IHP2)
C          CALL OPRT(N2,VDI,N2,T1 ,NOP,0,0,IOC,NHOP,NHM,H,IHP2,IHP1)
C          CALL MADS(N2,VDI,T1,S)
CCCCCCCC 
CC P_j=5/2
C          CALL CLRV(N2,T1)
C          CALL OPRT(N2,T1,N2, VSI,NOP,0,0,IOC,NHOP,NHM,H,IHP1,IHP2)
C          CALL OPRT(N2,T1,N2, VSI,NOP,0,0,IOC,NHOP,NHM,H,IHP2,IHP1)
C          CALL MADS(N2, T1,VSI,S)
CC T_Auger
C          CALL CLRV(N3,T2)
C          CALL OPRT(N3,T2,N3, T1,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP1,IHP2)
C          CALL MULS(N3,T2, RHO*PI)
CC T_Auger^dagger^
C          CALL CLRV(N2,T1)
C          CALL OPRT(N2,T1,N2, T2 ,NOP2,IO1,IEX1,0,NHOP,NHM,H,IHP2,IHP1)
CC P_j=5/2
C          CALL OPRT(N2,VDR,N2,T1 ,NOP,0,0,IOC,NHOP,NHM,H,IHP1,IHP2)
C          CALL OPRT(N2,VDR,N2,T1 ,NOP,0,0,IOC,NHOP,NHM,H,IHP2,IHP1)
C          CALL MADS(N2,VDR,T1,S)
C
C        END DO !IEX
C      END DO !IO
C
C      END SUBROUTINE MODGAMM2

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Multiply hamiltonian to a complex vector VS and add it 
C  to a complex vector VD (type II) 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADM2(N,VD,VDI,VS,VSI,E,EI
     &                ,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD)
      IMPLICIT NONE 

C Parameters
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy arguments
      INTEGER N
      REAL*8  VD(N),VDI(N), VS(N),VSI(N)
      REAL*8  E, EI
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

C Local variable
      INTEGER I
C -----------------

        DO I=1,N
          VD (I)=VD (I)+(HD(I)-E)*VS (I)+EI*VSI(I)
          VDI(I)=VDI(I)+(HD(I)-E)*VSI(I)-EI*VS (I)
        END DO

        CALL OPRT(N,VD ,N,VS ,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VD ,N,VS ,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
        CALL OPRT(N,VDI,N,VSI,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
        CALL OPRT(N,VDI,N,VSI,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C
      END SUBROUTINE MADM2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Inner product of two complex vectors V & U (type II)
C   S = (V, U) 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE PRDCV2(N,VR,VI,UR,UI,SR,SI)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  VR(N), VI(N), UR(N), UI(N)
      REAL*8  SR, SI

C Local variable
      INTEGER I
C -------------

      SR=0D0
      SI=0D0
      DO I=1,N
        SR=SR+VR(I)*UR(I)+VI(I)*UI(I)
        SI=SI-VI(I)*UR(I)+VR(I)*UI(I)
      END DO

      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Multiply a complex number S to a complex vector V (type II)
C   V = S * V
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULCS2(N,VR,VI,SR,SI)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  VR(N), VI(N)
      REAL*8  SR, SI

C Local variable
      INTEGER I
      REAL*8  T
C
      DO I=1,N
        T     = SR*VR(I)-SI*VI(I)
        VI(I) = SR*VI(I)+SI*VR(I)
        VR(I) = T
      END DO
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Multiply a complex number S to a complex vector U
C   and add the result to V  (type II)
C   V = V + S * U 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADCV2(N,VR,VI,SR,SI,UR,UI)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  VR(N), VI(N), UR(N), UI(N)
      REAL*8  SR, SI

C Local variable
      INTEGER I

      DO I=1,N
        VR(I) = VR(I)+SR*UR(I)-SI*UI(I)
        VI(I) = VI(I)+SR*UI(I)+SI*UR(I)
      END DO
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   The absolute value of a complex vector (type II)
C   S = |V| 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ABSCV2(N,VR,VI,S)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  VR(N),VI(N)
      REAL*8  S

C Local variable
      INTEGER I

      S=0D0
      DO I=1,N
        S = S + VR(I)**2 + VI(I)**2
      END DO
      S = SQRT(S)
C
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   multify real vector U to a complex vector V (type II)
C   V = U * V 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULV2(N,VR,VI,U)
      IMPLICIT NONE
      INTEGER N
      REAL*8  VR(N),VI(N),U(N)

C Local variable
      INTEGER I

      DO I=1,N
        VR(I)=U(I)*VR(I)
        VI(I)=U(I)*VI(I)
      END DO
C
      END
C>>>END RESONANCE
C//////////////////////////
C//  real vector lib.    //
C//////////////////////////

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  The absorlute value of a vector V
C  T=|V|
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ABSV(N,V,T)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V(N)

C Local variable
      INTEGER I
      REAL*8  T

      T=0D0
      DO I=1,N
        T=T+V(I)**2
      END DO
      T=SQRT(T)

      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Load the IP-th low vector of the 
C  matrix UM to the vector V.
C   V(1:N)=UG(1:N,IP)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE LDV(N,V,NGM,NBGM,UG,IP)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  V(N)
      INTEGER NGM, NBGM
      REAL*8  UG(NBGM,NGM)
      INTEGER IP

C Local variable
      INTEGER I

      DO I=1,N
        V(I)=UG(I,IP)
      END DO
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Store the value of the vector V
C  to the IP-th low vector of the 
C  matrix UM.
C   UG(1:N,IP)=V(1:N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE STV(N,V,NGM,NBGM,UG,IP)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER  N
      REAL*8   V(N)
      INTEGER  NGM,NBGM
      REAL*8   UG(NBGM,NGM)
      INTEGER  IP

C Local variable
      INTEGER  I

      DO I=1,N
        UG(I,IP)=V(I)
      END DO
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Add the vector V2 to the vector V1
C   V1(1:N)=V1(1:N)+V2(1:N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ADDV(N,V1,V2)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8 V1(N),V2(N)

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=V1(I)+V2(I)
      END DO
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Subtract the vector V2 from the vector V1 
C   V1(1:N)=V1(1:N)-V2(1:N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SUBV(N,V1,V2)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V1(N),V2(N)

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=V1(I)-V2(I)
      END DO
      END SUBROUTINE SUBV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   multiply real scalar S to the vector V1
C   V1(1:N)=S*V1(1:N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULS(N,V1,S)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V1(N)
      REAL*8  S 

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=S*V1(I)
      END DO
      END SUBROUTINE MULS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   V1(1:N)=V1(1:N)+S*V2(1:N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADS(N,V1,V2,S)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V1(N),V2(N)
      REAL*8  S

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=V1(I)+S*V2(I)
      END DO
      END SUBROUTINE MADS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   The inner product of vectors V1 and V2
C    S=sum_{I=1}^N V1(I)*V2(I)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULV(N,V1,V2,S)
      IMPLICIT NONE
      INTEGER N
      REAL*8  V1(N),V2(N)
      REAL*8  S

C Local variable
      INTEGER I

      S=0D0
      DO I=1,N
        S=S+V1(I)*V2(I)
      END DO
      END SUBROUTINE MULV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Set a vector V to zero: V(1:N)=0 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CLRV(N,V1)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V1(N)

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=0D0
      END DO
      END SUBROUTINE CLRV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Move the value of a vector V2 to V1
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MOVV(N,V1,V2)
      IMPLICIT NONE

C Dummy arguments
      INTEGER N
      REAL*8  V1(N), V2(N)

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=V2(I)
      END DO
      END SUBROUTINE MOVV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Multiply real scalar S to a vector V2
C  and add it to a vector V1
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADV(N,V1,V2,S)
      IMPLICIT NONE 

C Dummy arguments
      INTEGER N
      REAL*8  V1(N), V2(N)
      REAL*8  S

C Local variable
      INTEGER I

      DO I=1,N
        V1(I)=V1(I)+S*V2(I)
      END DO
      END SUBROUTINE MADV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Operate Hamiltonian #NOP to a vector V1
C  and store the result to a vector V2
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MULM(N,V2,V1,ESFT,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,NSTM,NCIM,NCI,NSFT,NMODE)
      IMPLICIT NONE 

C Parameter 
      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy arguments
      INTEGER N
      REAL*8  V1(N), V2(N)
      REAL*8  ESFT
      INTEGER NOP

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)

      INTEGER NMODE

C Local variables
      INTEGER NEXL,I,ICI
      REAL*8 T

      NEXL=MOD(NMODE/10,10)
      IF (NEXL.EQ.0) THEN
        DO I=1,N
          V2(I)=(HD(I)-ESFT)*V1(I)
        END DO
      ELSE
        DO ICI=1,NCI(NOP)
          T=HD(ICI)-ESFT
          DO I=NSFT(ICI,NOP)+1,NSFT(ICI+1,NOP)
            V2(I)=T*V1(I)
          END DO
        END DO
      END IF
      CALL OPRT(N,V2,N,V1,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N,V2,N,V1,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C
      END SUBROUTINE MULM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Operate Hamiltonian #NOP to a vector V1
C  and add the result to a vector V2.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MADM(N,V2,V1,ESFT,NOP,NHOP,NHM,H,IHP1,IHP2,NHDM,HD
     &               ,NSTM,NCIM,NCI,NSFT,NMODE)
      IMPLICIT NONE 

C Parameter

      INTEGER   NHOPM
      PARAMETER(NHOPM=100)

C Dummy arguments
      INTEGER N
      REAL*8  V1(N), V2(N)
      REAL*8  ESFT
      INTEGER NOP 

      INTEGER NHOP(0:NHOPM)
      INTEGER NHM
      REAL*8  H(NHM)
      INTEGER IHP1(NHM),IHP2(NHM)
      INTEGER NHDM
      REAL*8  HD(NHDM)

      INTEGER NSTM,NCIM
      INTEGER NCI(0:NSTM)
      INTEGER NSFT(NCIM+1,NSTM)

      INTEGER NMODE

C Local variables 
      INTEGER NEXL,I,ICI
      REAL*8 T
C
      NEXL=MOD(NMODE/10,10)
      IF (NEXL.EQ.0) THEN
        DO I=1,N
          V2(I)=V2(I)+(HD(I)-ESFT)*V1(I)
        END DO
      ELSE
        DO ICI=1,NCI(NOP)
          T=HD(ICI)-ESFT
          DO I=NSFT(ICI,NOP)+1,NSFT(ICI+1,NOP)
            V2(I)=V2(I)+T*V1(I)
          END DO
        END DO
      END IF
      CALL OPRT(N,V2,N,V1,NOP,0,0,0,NHOP,NHM,H,IHP1,IHP2)
      CALL OPRT(N,V2,N,V1,NOP,0,0,0,NHOP,NHM,H,IHP2,IHP1)
C
      END SUBROUTINE MADM
