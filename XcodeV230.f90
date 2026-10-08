 
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
!   X-code, a simple but powerfull interpreter for data processing
!     Ver. 1.00  coded by Arata Tanaka (1993.08 at Hiroshima Univ.)
!     Ver. 2.00  coded by Arata Tanaka (1997.10 at Hiroshima Univ.)
!     Ver. 2.20  coded by Arata Tanaka (2002.05 at Hiroshima Univ.)
!     Ver. 2.30  coded by Arata Tanaka (2004.01 at Hiroshima Univ.)
!
!     latest modification date: 2005.10.5 (Ver. 2.30)
!
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!>>>START MAIN
      PROGRAM X_SYS
      IMPLICIT NONE
 
! Paremeters
      INTEGER, PARAMETER ::NVAL=1000,ITXMAX=128
 
! Global variables
      REAL*8 :: VAL(NVAL)
      CHARACTER(32)  :: ASYMB
      INTEGER :: I,IUNIT,ISZ,ITYP,LIN,IERR
!
! init Xcode
!
      IUNIT=5
      CALL X_CODE(0,IUNIT,ASYMB,ISZ,ITYP,VAL &
                 ,NVAL,LIN,IERR)
!
! main loop
!
      DO !main loop
        CALL X_CODE(1,IUNIT,ASYMB,ISZ,ITYP,VAL &
                   ,NVAL,LIN,IERR)
!       WRITE(6,*)(VAL(I),I=1,ISZ)
!       WRITE(6,*)ITYP
!
! end of the program file
!
        IF (IERR.EQ.3) THEN
          STOP
!
! case of extra ")"
!
        ELSE IF (IERR.EQ.2) THEN
          WRITE(6,*)'Are you sure to quit Xcode ? (y/n)'
          READ(5,'(A1)')ASYMB
          IF ( ASYMB.EQ.'Y' .OR. ASYMB.EQ.'y' ) STOP
        END IF
      END DO !main loop
!
      END
!>>>END MAIN
!------------------------------------------------------------------
!
!   ISTAT=0 -- The initialzation of the system.
!        =1 -- The evaluation and execution of the input program.
!        =2 -- Set value(s) to a variable in the input program.
!        =3 -- Get value(s) from a variable in the input program.
!
!   IUNIT: The unit number of the device through which
!          the input program is received.
!
!   ASYMB: The name of a variable in the input program you want
!          to access in ISTAT=2,3.
!    ISZ:  The size of the variable.
!    ITYP: The type of the variable.
!    VAL:  The value(s) of the variable.
!   NVAL:  The number of the element(s) of the variable.
!    (For ISTAT=1, ISZ,ITYP,VAL and NVAL are also the same but
!     those for the top of the system stack at NVARW)
!
!    AWRD: The last word extracted from the input program in ISTAT=1
!    NWRD: The size of the word.
!     DLM: The last delimiter extracted from the input program
!          in ISTAT=1
!
!     LIN: current line number.
!
!    IERR=0: A normal termination of the execution.
!            (Note that this does not meam the end of the program file
!             is reached, when IBREAK=1; In this case this subroutine
!             stops every expression ending with a semicolon
!             or a definition of a user function.)
!    IERR=1: The interpreter encountered an error and the execution
!            has been suspended.
!    IERR=2: An extra ")"  is found.
!    IERR=3: The end of the input program file is reached or
!            the control-D key is pressed in the interactive mode.
!
!-------------------------------------------------------------------
! Internal variables
!
!     SVAR  --  the names of variables
!     ISIZE --  the sizes(number of elements) of variables & constants
!     ITYPE --  the types of variables & constants
!     IVALP --  pointers to their values stored in RVAL
!     RVAL  --  the values (elements of variables & constants)
!     NVAR  --- the number of variables
!
!     NVARM  --- the maxmum number of variables
!     NCNSM  --- the maxmum number of constans
!     NRVALM --- the maxmum number of values
!     NRVALX     separates elements of RVAL into two areas
!                1 to NRVLAX-1     values for variables
!                NRVALX to NRVALM  values for constants
!
!     SFNC  --- name of user functions
!     IFNCP --- top address of internal code for user functions
!     NFNC  --- the number of user functions
!
!     NFNCM --  the maxmum number of user functions
!
!     ICD     -- internal codes are stored this variable
!     ICDM    -- the maxmum number of internal codes
!
!-----------------------------------------------------------------
! The variables to control the system behavior.
!
!     IBREAK=0 -- Trys to continue the execution of
!                 program even with errrors and shows
!                 prompt "Xcode>". This is for interactive mode.
!                 Sys_batch = 0 (default)
!
!     IBREAK=1 -- The interpreter stops execution of every single
!                 expression or the definition of an user function,
!                 which are separeted by semicorons. This mode is
!                 usefull when your xcode program is writte in a file
!                 and fed through the redirection of the standard
!                 input (unit number 5).
!                 Sys_batch = 1
!
!     ITRACE=1 -- Creates internal code with line numbers
!                 of the source code to get detailed information
!                 for debugging.
!                 Sys_trace =1  (default)
!
!     ITRACE=2 -- In addition to above, the internal code created
!                 and the status of the system resouces are shown
!                 every execution of the expression in the xcode
!                 program. This is for debugging of this program
!                 itself.
!                 Sys_trace =2
!
!     ITRACE=0 -- No detailed information of errors
!                 when the program crash but lesser memory
!                 and better peformance than above two.
!                 Sys_trace =0
!
!     ISYSDB=1 -- The types of variables starting with the letters
!                 from a to h and o to z with no type declaration
!                 are implicitly defined to be double precision
!                 (the type double).
!                 Sys_dble =1 (default)
!
!     ISYSDB=0 -- The same to above but the variables are implicitly
!                 defined to be single precision (the type real).
!                 Sys_dble =0
!
!     ISYSIO   -- The default input and output unit number
!                 for the functions read and write with
!                 no explicit opening of the file using the
!                 function open, i.e., read(file_name, arg2, ...)
!                 or write(file_name, arg2, ..).
!                 ISYSIO = 10 is the default value. (Sys_io=10)
!
!
      SUBROUTINE X_CODE(ISTAT,IUNIT,ASYMB,ISZ,ITYP,VAL &
      ,NVAL,LIN,IERR)
      IMPLICIT NONE
 
! Paremeters
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NRVALX=195000&
      ,NCNSM=2000,NCNSB=NVARM,NFNCM=100,ICDM=40000
 
      INTEGER, PARAMETER ::NBTM=2000
 
      INTEGER, PARAMETER ::ITXMAX=128
 
! Dummy arguments
      INTEGER :: ISTAT, IUNIT
      CHARACTER(32)  :: ASYMB
      INTEGER :: ISZ, ITYP
      INTEGER :: NVAL
      REAL*8 ::  VAL(NVAL)
      INTEGER :: LIN, IERR
 
! Local variables
      INTEGER :: ICD(ICDM)
      INTEGER :: IBT(NBTM,0:2)
      INTEGER :: IFNCP(NFNCM)
      INTEGER :: IVALP(NVARM+NCNSM), ISIZE(NVARM+NCNSM), ITYPE(NVARM+NCNSM)
      REAL*8 ::  RVAL(NRVALM)
      REAL*8 ::  SYSVAL(10)
!
      CHARACTER(32)  :: SVAR(NVARM)
      CHARACTER(32)  :: SFNC(NFNCM)
 
      INTEGER ::            NWRD
      CHARACTER(ITXMAX)  :: AWRD
      CHARACTER(2)  ::      DLM
 
      INTEGER :: ISYSIO, IBREAK, ISYSDB, ITRACE
      INTEGER :: NCNS, NVAR, NVARW, NFNC, NRVAL, NRVALC
      INTEGER :: IUNIT0
      INTEGER :: I,J,K,IPS,IT,IS,IE,ICDS,ICDE
      INTEGER :: NCNST,NRVALT,NFNCT
      INTEGER :: IBTP
 
      SAVE ICD,IFNCP,IVALP,ISIZE,ITYPE,RVAL
      SAVE SVAR,SFNC,ICDS,IUNIT0
      SAVE ISYSIO,IBREAK,ISYSDB,ITRACE
 
      SAVE NCNS,NVAR,NFNC,NRVAL,NRVALC
 
      DATA ISYSIO/10/,IBREAK/0/,ISYSDB/1/,ITRACE/1/
!------------------------------------------------------------------
! Initialization
!------------------------------------------------------------------
      IF (ISTAT.EQ.0) THEN
! initialization of system variables and constants
        IERR=0
        NCNS=0
        NVAR=0
        NFNC=0
        NRVAL=1
        NRVALC=NRVALX+1
        ICDS=0
        IUNIT0=IUNIT
!
! Define system constants
!
! set the size zero constant "{}"
! This has the undefinite type (-1). When the substitution, the type
! is automatically much to that of the variable on the left side
! of the operator "=".
!
        ISIZE(NCNSB+1)=0
        ITYPE(NCNSB+1)=-1
        IVALP(NCNSB+1)=NRVALC
        RVAL(NRVALC)=0
        NRVALC=NRVALC+1
!
! set logical global constant "false"
!
        ISIZE(NCNSB+2)=1
        ITYPE(NCNSB+2)=4
        IVALP(NCNSB+2)=NRVALC
        RVAL(NRVALC)=0
        NRVALC=NRVALC+1
!
! set logical global constant "true"
!
        ISIZE(NCNSB+3)=1
        ITYPE(NCNSB+3)=4
        IVALP(NCNSB+3)=NRVALC
        RVAL(NRVALC)=1
        NRVALC=NRVALC+1
        NCNS=3
!
! Define system variables and their initial values
!
        ASYMB='sys_io'
        SYSVAL(1)=DBLE(ISYSIO)
        CALL X_UI(1,ASYMB,SYSVAL,1,1,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
 
        ASYMB='sys_batch'
        SYSVAL(1)=DBLE(IBREAK)
        CALL X_UI(1,ASYMB,SYSVAL,1,1,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
 
        ASYMB='sys_dble'
        SYSVAL(1)=DBLE(ISYSDB)
        CALL X_UI(1,ASYMB,SYSVAL,1,1,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
 
        ASYMB='sys_trace'
        SYSVAL(1)=DBLE(ITRACE)
        CALL X_UI(1,ASYMB,SYSVAL,1,1,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
 
!>>>START IOFNC
        ASYMB='sys_xcodelib'
        AWRD='/home/tanaka/Xcode/lib/'
        DO I=ITXMAX,1,-1
          IF (AWRD(I:I).NE.' ') EXIT
        END DO
        NWRD=I
        DO I=1,NWRD
          VAL(I)=ICHAR(AWRD(I:I))
        END DO
        CALL X_UI(1,ASYMB,VAL,NWRD,0,NVAR,NRVAL,SVAR,RVAL &
                   ,IVALP,ISIZE,ITYPE,IE)
!>>>END IOFNC
!------------------------------------------------------------------
! Show prompt (when in the interactive mode)
!------------------------------------------------------------------
!>>>START MAIN
        IF (IBREAK.EQ.0) THEN
          WRITE(6,*) &
       '  Xcode interpreter V2.30 coded by Arata Tanaka (Jan. 2004)'
          WRITE(6,'(A,$)')'Xcode>'
          AWRD=';'
          CALL GTWRD(NWRD,AWRD,DLM,LIN,0,IERR,-4)
        ELSE
!>>>END MAIN
          AWRD=' '
          CALL GTWRD(NWRD,AWRD,DLM,LIN,0,IERR,-4)
!>>>START MAIN
        END IF
!>>>END MAIN
      ELSE IF (ISTAT.EQ.1) THEN
!------------------------------------------------------------------
! interpretation & execution of the input program
!------------------------------------------------------------------
        IERR=0
!
! create a binary tree from an input program for syntax analysis
!
        CALL X_BTRE( &
             NCNS,NCNST,NRVALC,NRVALT &
            ,NVAR,NVARW,SVAR &
            ,NFNC,NFNCT,SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,IBT,IBTP &
            ,LIN,IUNIT,IBREAK,IERR)
!
! transform the binary tree to internal codes
!
        IF (IERR.EQ.0) THEN
        CALL X_INTR( &
             NCNS,NCNST,NRVALC,NRVALT &
            ,NVAR,NVARW,NRVAL        ,SVAR &
            ,NFNC,NFNCT,IFNCP        ,SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,IBT,IBTP &
            ,ICDS,ICDE,ICD &
            ,ISYSDB,ITRACE,IBREAK,IERR)
        ELSE IF (IBREAK.NE.0.AND.IERR.NE.3.AND.IERR.NE.2) THEN
          STOP
        END IF
        IF (ITRACE.EQ.2) THEN
          WRITE(6,*)'> Code area: ICDS, ICDE, ICDM=',ICDS,ICDE,ICDM
          WRITE(6,*)'> Number of Variables NVAR, NVARM=',NVAR,NVARM
          WRITE(6,*)'> Number of Functions NFNC, NFNCM=',NFNC,NFNCM
          WRITE(6,*)'> Number of Constants NCNS, NCNSM=',NCNS,NCNSM
          WRITE(6,*)'> Data Storage (variable) NRVAL, NRVALM=' &
                    ,NRVAL,NRVALM
          WRITE(6,*)'>              (constant) NRVALC,NRVALX=' &
                    ,NRVALC,NRVALX
        END IF
!        CALL X_DASM(ICDS+1,ICDE,ICD)
!
! execute the internal codes
!
        IF (IERR.EQ.0) THEN
        CALL X_EXEC( &
             NCNS,NRVALC &
            ,NVAR,NVARW,NRVAL       ,SVAR &
            ,NFNC,IFNCP             ,SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,ICDS,ICDE,ICD &
            ,IUNIT,ISYSIO,ITRACE,IERR,IBREAK)
        ELSE IF(IBREAK.NE.0.AND.IERR.NE.3.AND.IERR.NE.2) THEN
          STOP
        END IF
        IF (NVARW.GT.0.AND.IERR.EQ.0) THEN
          ISZ=ISIZE(NVARW)
          ITYP=ITYPE(NVARW)
          K=IVALP(NVARW)
!          WRITE(6,*)'in output:size,type valp value',ISZ,ITYP,K,RVAL(K)
          DO I=1,MIN(ISZ,NVAL)
            VAL(I)=RVAL(I+K-1)
          END DO
        END IF
!
! set system variables
!
        ASYMB='sys_io'
        CALL X_UI(2,ASYMB,SYSVAL,I,J,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
        ISYSIO=NINT(SYSVAL(1))
 
        ASYMB='sys_batch'
        CALL X_UI(2,ASYMB,SYSVAL,I,J,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
        IBREAK=NINT(SYSVAL(1))
 
        ASYMB='sys_dble'
        CALL X_UI(2,ASYMB,SYSVAL,I,J,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
        ISYSDB=NINT(SYSVAL(1))
 
        ASYMB='sys_trace'
        CALL X_UI(2,ASYMB,SYSVAL,I,J,NVAR,NRVAL,SVAR,RVAL &
                 ,IVALP,ISIZE,ITYPE,IE)
        ITRACE=NINT(SYSVAL(1))
!
! In the interactive mode (IBREAK=0), a semicolon is inserted
! in the end of every line.
!
!>>>START MAIN
        IF (IBREAK.EQ.0) THEN
          AWRD=';'
          CALL GTWRD(NWRD,AWRD,DLM,LIN,0,IERR,-4)
        ELSE
!>>>END MAIN
          AWRD=' '
          CALL GTWRD(NWRD,AWRD,DLM,LIN,0,IERR,-4)
!>>>START MAIN
        END IF
!>>>END MAIN
!
! Show the top of the system stack at NVARW
! in the interactive mode (IBREAK=0)
!
!>>>START MAIN
        IF (IBREAK.EQ.0.AND.IERR.EQ.0) THEN
          IT=ITYPE(NVARW)
          IPS=IVALP(NVARW)
          IS=ISIZE(NVARW)
          IF (IT.EQ.3) THEN
            WRITE(6,*)(RVAL(IPS+I),I=0,IS-1)
          ELSE IF (IT.EQ.2) THEN
            WRITE(6,*)(REAL(RVAL(IPS+I)),I=0,IS-1)
! 2018       FORMAT(5E15.7/)
          ELSE IF (IT.EQ.1.OR.IT.EQ.4) THEN
            WRITE(6,*)(NINT(RVAL(IPS+I)),I=0,IS-1)
! 3018       FORMAT(7I8/)
          ELSE
            WRITE(6,4018)(CHAR(NINT(RVAL(IPS+I))),I=0,IS-1)
 4018       FORMAT(80A1/)
          END IF
          WRITE(6,'(A,$)')'Xcode>'
        END IF
!>>>END MAIN
! The end of the input file is reached.
        IF (IUNIT.EQ.0.AND.IERR.EQ.3) THEN
          IUNIT=IUNIT0
          RETURN
! The end of a file specified by the function include is reached.
        ELSE IF (IERR.EQ.3) THEN
!>>>START SILENT
          WRITE(6,*)'  end of inclusion'
!>>>END SILENT
          IERR=0
        END IF
!
      ELSE IF (ISTAT.EQ.2) THEN
! set value(s) to the required variable
        CALL X_UI(1,ASYMB,VAL,ISZ,ITYP &
                 ,NVAR,NRVAL,SVAR,RVAL,IVALP,ISIZE,ITYPE,IERR)
      ELSE IF (ISTAT.EQ.3) THEN
! get value(s) from the required variable
        ISZ=NVAL
        CALL X_UI(2,ASYMB,VAL,ISZ,ITYP &
                 ,NVAR,NRVAL,SVAR,RVAL,IVALP,ISIZE,ITYPE,IERR)
      END IF
 
      END SUBROUTINE X_CODE
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  User interface for Xcode interpreter
!  to get or put value(s) of a variable specified.
!
!  IMODE: 1-Put or 2-Get
!  USYM: the name of the variable
!  UVAL: the value of the variable
!  IUSZ: the size of the variable
!  IUTY: the type of the variable
!  ( IUTY=0: character, 1: integer, 2: real, 3: double and 4: logical )
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE X_UI(IMODE,USYM,UVAL,IUSZ,IUTY &
                ,NVAR,NRVAL,SVAR,RVAL,IVALP,ISIZE,ITYPE,IERR)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::NBTM=2000
 
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NRVALX=195000&
      ,NCNSM=2000,NCNSB=NVARM,NFNCM=100,ICDM=40000
 
! Dummy arguments
      INTEGER :: IMODE
      CHARACTER(32)  :: USYM
      REAL*8 :: UVAL(*)
      INTEGER :: IUSZ, IUTY
      INTEGER :: NVAR, NRVAL
      CHARACTER(32)  :: SVAR(NVARM)
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: IVALP(NVARM+NCNSM), ISIZE(NVARM+NCNSM), ITYPE(NVARM+NCNSM)
 
! Local variables
      INTEGER :: ICD(ICDM)
      INTEGER :: IBT(NBTM,0:2)
      INTEGER :: IFNCP(NFNCM)
      REAL*8 ::  SYSVAL(10)
!
      INTEGER :: NVL(1), NVLW(1)
      INTEGER :: NRVALW, NUVALM, NVARW
      INTEGER :: I,K,IERR,IP,ISD,IPD
! -----------------------------
      CALL LWCASE(USYM)
!
      IERR=0
!
!   IMODE=1
!   put value(s) to a variable specified by USYM
!
      IF (IMODE.EQ.1) THEN
        DO I=1,NVAR
          IF (USYM.EQ.SVAR(I)) THEN
            IF (IUTY.LT.0) THEN
              IUTY=ITYPE(I)
            ELSE IF (ITYPE(I).NE.IUTY) THEN
              WRITE(6,*)' type mismatch in X_UI'
              WRITE(6,*)' symbol of variable ==>'//USYM
              IERR=1
              RETURN
            END IF
            ISD=ISIZE(I)
            IPD=IVALP(I)
! extend size of variable
            NVARW=NVAR
            NRVALW=NRVAL
            NVL(1)=1
            NVLW(1)=1
            CALL RSZVAR(IUSZ,ISD,IPD &
                      ,NVAR,NVARW,ISIZE,IVALP &
                      ,NRVAL,NRVALW,RVAL &
                      ,0,NVL,NVLW)
            DO K=1,IUSZ
              RVAL(IPD+K-1)=UVAL(K)
            END DO
            RETURN
          END IF
        END DO
        NVAR=NVAR+1
        SVAR(NVAR)=USYM
        ITYPE(NVAR)=IUTY
        ISIZE(NVAR)=IUSZ
        IVALP(NVAR)=NRVAL
        DO I=NRVAL,NRVAL+IUSZ-1
          RVAL(I)=UVAL(I-NRVAL+1)
        END DO
        NRVAL=NRVAL+IUSZ
        RETURN
!
!   IMODE=2
!   Get value(s) from a variable specified by USYM
!
      ELSE IF(IMODE.EQ.2) THEN
        NUVALM=IUSZ
        DO I=1,NVAR
          IF (USYM.EQ.SVAR(I)) THEN
            IUSZ=ISIZE(I)
            IUTY=ITYPE(I)
            IP=IVALP(I)
            DO K=1,MIN(NUVALM,IUSZ)
              UVAL(K)=RVAL(IP+K-1)
            END DO
            RETURN
          END IF
        END DO
        WRITE(6,*)'the required variable did not found in X_UI'
        WRITE(6,*)'  variable ==>',USYM
        IERR=1
        RETURN
      ELSE
        WRITE(6,*)'wrong mode in X_UI  mode=',IMODE
        IERR=1
        RETURN
      END IF
 
      END SUBROUTINE X_UI
!
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     Xcode to binary tree for syntax analysis Ver.2.00
!                        coded by A.Tanaka 1993.08 (at Hiroshima Univ.)
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      SUBROUTINE X_BTRE( &
             NCNS,NCNST,NRVALC,NRVALT &
            ,NVAR,NVARW        ,SVAR &
            ,NFNC,NFNCT        ,SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,IBT,IBTP &
            ,LIN,IUNIT,IBREAK,IERR)
!
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::NSYS=50
      INTEGER, PARAMETER ::NBTM=2000
      INTEGER, PARAMETER ::NOPM=39,NVARB=900 ,NCLS=10000
 
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NRVALX=195000&
      ,NCNSM=2000,NCNSB=NVARM,NFNCM=100,ICDM=40000
 
      INTEGER, PARAMETER ::ITXMAX=128
 
! Dummy argumets
      INTEGER :: NCNS,NCNST,NRVALC,NRVALT
 
      INTEGER :: NVAR, NVARW
      CHARACTER(32)  :: SVAR(NVARM)
 
      INTEGER :: NFNC,NFNCT
      CHARACTER(32)  :: SFNC(NFNCM)
 
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: ISIZE(NVARM+NCNSM),ITYPE(NVARM+NCNSM),IVALP(NVARM+NCNSM)
 
      INTEGER :: IBT(NBTM,0:2)
      INTEGER :: IBTP
 
      INTEGER :: LIN,IUNIT,IBREAK,IERR
 
! Local variables
      INTEGER :: NOPPR(NOPM),NOPRND(NOPM)
      INTEGER :: IFAC(30)
      INTEGER :: IFNCP(NFNCM)
      INTEGER :: NSSZ(NSYS),NSTY(NSYS),NLIN(NSYS)
      INTEGER :: I,K,N,ISTP,LIN2,NFAC,NASZ,NATY,NSZ,NTY,IT,IOP,IOPC
      INTEGER :: IADD,IPLSEQ,IBRN
 
      CHARACTER(2)  ::  SOPR(NSYS)
      CHARACTER(2)  ::  AOPR
      CHARACTER(256)  :: TOPR,SYSFNC,IOFNC,EXFNC
      CHARACTER(ITXMAX)  :: SARG(NSYS)
      CHARACTER(ITXMAX)  :: AARG, AT
! Function
      REAL*8 :: DNUM2
 
!
! op #            1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
! op #           16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
! op #           31 32 33 34 35 36 37 38 39 40
!  priority of operators
      DATA NOPPR / 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5 &
                 , 5, 5, 5, 6, 7, 9, 9, 9, 9, 9, 9,10,10,11,11 &
                 ,12,14,14,14, 8,13,13, 5,14/
!  number of operands  0---indefinite
      DATA NOPRND/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2 &
                 , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 &
                 , 2, 2, 1, 1, 1, 1, 1, 1 ,2/
!  parameters IADD, IPLSEQ  etc. in "X_INTR" depend variable TOPR.
!  Check them IF you change TOPR.
!  symbols for operators
            TOPR=') e} e] e( e{ e[ e??e? e: e# e, e; e= e+=e-=e' &
               //'*=e/=e%=e||e&&e< e> e==e!=e<=e>=e+ e- e* e/ e' &
               //'% e**e++e--e! e-@e+@e^ e@ e" '
!  system functions (minimum set)
!             1       2       3       4       5
!             0123456701234567012345670123456701234567
      SYSFNC=' sqrt    exp     cos     sin     tan    ' &
           //' log     atan    sign    abs     min    ' &
           //' max     int     cosh    sinh    tanh   ' &
      	   //' sizeof  typeof  print   real    nint   ' &
           //' sum     clear   dble    val     str    ' &
           //' char    stop    log10   fac     cmb    ' &
           //' printf  '
!>>>START IOFNC
!  IO functions
      IOFNC =' open    close   include write   writeln' &
           //' read    readln  input   eof     search ' &
           //' index   rewind  nextln  writef  setdlm '
!>>>END IOFNC
!>>>START EXFNC
!  Graphic functions
      EXFNC =' drfstr  drfend  setpen  line1   line2  ' &
           //' line4   atype   axis    arrow1  circle ' &
           //' rect    arc0    setlin  bear    symbol ' &
           //' mark    lgaxis  dimen1  arrow3  origin ' &
           //' rotate  incheg  factor  degree  radian '
!>>>END EXFNC
!>>>START SUBSYSTEM
!  IO functions
      IOFNC ='          '
!  Graphic functions
      EXFNC ='          '
!>>>END SUBSYSTEM
!
!------------------------------------------------------------
! initialization
!------------------------------------------------------------
      ISTP=1
      SARG(1)=' '
      SOPR(1)='; '
      NSTY(1)=0
      NSSZ(1)=0
      NLIN(1)=1
      AT=' '
      IBTP=0
      AOPR='+='
      IPLSEQ=INDEX(TOPR,AOPR)/3+1
      AOPR='+ '
      IADD=INDEX(TOPR,AOPR)/3+1
      NFAC=0
      IERR=0
      LIN=0
      LIN2=0
!------------------------------------------------------------
!  create a binary tree for the syntax analysis
!------------------------------------------------------------
      NVARW=0
      NCNST=NCNS
      NRVALT=NRVALC
      NFNCT=NFNC
!== Recursive entry
!-- Get a pair of operator & operand from the input program
 
      DO !MAIN LOOP
        CALL GTWRD(NASZ,AARG,AOPR,LIN2,IUNIT,IERR,NATY)
        IF (IERR.EQ.3) THEN
          IF (ISTP.GT.1) THEN
            WRITE(6,*)'Error! Abnormal termination of the program file.'
            WRITE(6,*)'  ; or ) or } or ] or " may be missing'
            STOP
          ELSE
            RETURN
          END IF
        END IF
        IF (AOPR.EQ.'  ') THEN
          IF (NASZ.NE.0) THEN
            WRITE(6,'(A,I8)') 'Error! Missing operator after "' &
             //AARG(:NASZ)//'" at the line=',LIN2
          ELSE
            WRITE(6,'(A,I8)')'Error! Missing operator at the line=',LIN2
          END IF
          WRITE(6,'(A)') 'A semicolon at the end of the line or a comma' &
               // ' in parentheses may be forgotten.'
          IF (IBREAK.NE.0) STOP
          IERR=1
          LIN=MAX(LIN,LIN2)
          RETURN
        END IF
        IOP=INDEX(TOPR,SOPR(ISTP))/3+1
        IOPC=INDEX(TOPR,AOPR)/3+1
        IF(IOPC.LT.1) THEN
          WRITE(6,*)'Error! Unknown operator =>'//AOPR
          WRITE(6,*)'    at the line=',LIN2
          IF (IBREAK.NE.0) STOP
          IERR=1
          LIN=MAX(LIN,LIN2)
          RETURN
        END IF
        IF (NOPPR(IOPC).GT.NOPPR(IOP).OR.NOPPR(IOPC).EQ.2) THEN
!--  Push a pair of operator & operand  to the stack
          ISTP=ISTP+1
          SOPR(ISTP)=AOPR
          SARG(ISTP)=AARG
          NSSZ(ISTP)=NASZ
          NSTY(ISTP)=NATY
          NLIN(ISTP)=LIN2
        ELSE
!==  estimate operands & create one node
!
          DO !IBTP LOOP
            IBTP=IBTP+1
            N=2
            DO IBRN=2,1,-1
              IF (IBRN.EQ.2) THEN
                AT=AARG
                NTY=NATY
                NSZ=NASZ
                LIN=LIN2
              ELSE
                AT=SARG(ISTP)
                NTY=NSTY(ISTP)
                NSZ=NSSZ(ISTP)
                LIN=NLIN(ISTP)
              END IF
              IF (IBRN.EQ.1.AND.NTY.EQ.4 .AND. SOPR(ISTP).EQ.'( ') THEN
                AT=AT(1:NSZ)
!-- Functions
                IF (INDEX(SYSFNC,' '//AT(1:NSZ)//' ').NE.0) THEN
                  IBT(IBTP,IBRN) &
                     = INDEX(SYSFNC,' '//AT(1:NSZ))/8 + 1  + NCLS*4
!>>>START IOFNC
                ELSE IF (INDEX(IOFNC,' '//AT(1:NSZ)//' ').NE.0) THEN
                  IBT(IBTP,IBRN) &
                     = INDEX(IOFNC,' '//AT(1:NSZ))/8 + 101 + NCLS*4
!>>>END IOFNC
!>>>START EXFNC
                ELSE IF (INDEX(EXFNC,' '//AT(1:NSZ)//' ').NE.0) THEN
                  IBT(IBTP,IBRN) &
                    =  INDEX(EXFNC,' '//AT(1:NSZ))/8 + 201 + NCLS*4
!>>>END EXFNC
                ELSE
! The name of a function must not the same to a variable
                  DO I=1,NVAR
                    IF (AT.EQ.SVAR(I) ) EXIT
                  END DO
                  IF (I.LE.NVAR) THEN
                    WRITE(6,'(A)') AT(1:NSZ)// &
                       ' already has been defined as a global variable'
                    WRITE(6,*)'    at line=',LIN
                    IERR=1
                    LIN=MAX(LIN,LIN2)
                    RETURN
                  END IF
                  DO I=1,NVARW
                    IF (AT.EQ.SVAR(NVARB+I)) EXIT
                  END DO
                  IF (I.LE.NVARW) THEN
                    WRITE(6,'(A)') AT(1:NSZ)// &
                       ' already has been defined as a variable'
                    WRITE(6,*)'    at line=',LIN
                    IERR=1
                    LIN=MAX(LIN,LIN2)
                    RETURN
                  ELSE
                    DO I=1,NFNCT
                      IF (AT.EQ.SFNC(I)) GOTO 40
                    END DO
                      SFNC(I)=AT(1:NSZ)
                      NFNCT=I
 40                 IBT(IBTP,IBRN)=I+NCLS*3
                  END IF
                END IF
              ELSE IF (SOPR(ISTP).EQ.'@ '.AND.IBRN.EQ.1) THEN
!-- Types
                I=1
                RVAL(NRVALT)=0
                IF (AT(1:7).EQ.'global_')THEN
                  I=8
                  RVAL(NRVALT)=10
                ELSE IF (AT(1:6).EQ.'const_')THEN
                  I=7
                  RVAL(NRVALT)=20
                END IF
                IF     (AT(I:NSZ).EQ.'c'.OR.AT(I:NSZ).EQ.'char')THEN
                  RVAL(NRVALT)=RVAL(NRVALT)+0
                ELSE IF (AT(I:NSZ).EQ.'i'.OR.AT(I:NSZ).EQ.'int')THEN
                  RVAL(NRVALT)=RVAL(NRVALT)+1
                ELSE IF (AT(I:NSZ).EQ.'r'.OR.AT(I:NSZ).EQ.'real')THEN
                  RVAL(NRVALT)=RVAL(NRVALT)+2
                ELSE IF (AT(I:NSZ).EQ.'d'.OR.AT(I:NSZ).EQ.'double')THEN
                  RVAL(NRVALT)=RVAL(NRVALT)+3
                ELSE IF(AT(I:NSZ).EQ.'l'.OR.AT(I:NSZ).EQ.'logical')THEN
                  RVAL(NRVALT)=RVAL(NRVALT)+4
                ELSE IF (AT(1:NSZ).EQ.'hidden') THEN
                  RVAL(NRVALT)=30
                ELSE
                  WRITE(6,'(A)')'Error! Unknown type =>',AT(1:NSZ)
                  WRITE(6,*)'    at line=',LIN
                  IERR=1
                  LIN=MAX(LIN,LIN2)
                  RETURN
                END IF
!
                NCNST=NCNST+1
                IVALP(NCNSB+NCNST)=NRVALT
                ISIZE(NCNSB+NCNST)=1
                ITYPE(NCNSB+NCNST)=1
                NRVALT=NRVALT+1
                IBT(IBTP,IBRN)=NCNST+NCLS*2
              ELSE IF (NTY.EQ.1) THEN
!-- Constants (array of character)
                NCNST=NCNST+1
                IVALP(NCNSB+NCNST)=NRVALT
                ITYPE(NCNSB+NCNST)=0
                DO K=1,NSZ
                  RVAL(NRVALT)=ICHAR(AT(K:K))
                  NRVALT=NRVALT+1
                END DO
                ISIZE(NCNSB+NCNST)=NSZ
                IBT(IBTP,IBRN)=NCNST+NCLS*2
              ELSE IF (NTY.EQ.2.OR.NTY.EQ.3) THEN
!-- Constants (numbers)
                RVAL(NRVALT)=DNUM2(NSZ,AT(1:32),IT)
                IF (IBREAK.NE.0.AND.IT.LT.0) THEN
                  WRITE(6,*)'Error! A wrong data type: '//AT(1:32)
                  WRITE(6,*)'    at line=',LIN
                  STOP
                END IF
                NCNST=NCNST+1
                IVALP(NCNSB+NCNST)=NRVALT
                ISIZE(NCNSB+NCNST)=1
!               IF (IT.EQ.2.AND.ISYSDB.NE.0) IT=3
                ITYPE(NCNSB+NCNST)=IT
                NRVALT=NRVALT+1
                IBT(IBTP,IBRN)=NCNST+NCLS*2
              ELSE IF (NTY.EQ.4) THEN
!
! The logical global constants "false" and "true".
! The constants No.2 (false) and No.3 (true)
! have beed already defined and reserved for the system
! at this stage.
!
                IF (AT(1:NSZ).EQ.'false') THEN
                  IBT(IBTP,IBRN)=2+NCLS*2
                ELSE IF (AT(1:NSZ).EQ.'true') THEN
                  IBT(IBTP,IBRN)=3+NCLS*2
                ELSE
!-- Variables
                  I=INDEX(SYSFNC,' '//AT(1:NSZ)//' ')
!>>>START IOFNC
                  I=MAX(I,INDEX(IOFNC,' '//AT(1:NSZ)//' '))
!>>>END IOFNC
!>>>START EXFNC
                  I=MAX(I,INDEX(EXFNC,' '//AT(1:NSZ)//' '))
!>>>END EXFNC
                  IF (I.NE.0) THEN
                    WRITE(6,'(A)')AT(1:NSZ)// &
                  ' already has been defined as a intrinsic function'
                    WRITE(6,*)'     at the line=',LIN
                    IERR=1
                    LIN=MAX(LIN,LIN2)
                    RETURN
                  END IF
                  DO I=1,NFNCT
                    IF (AT.EQ.SFNC(I)) EXIT
                  END DO
                  IF (I.LE.NFNCT) THEN
                    WRITE(6,'(A)') AT(1:NSZ)// &
                      ' already has been defined as a user function'
                    WRITE(6,*)'        at the line=',LIN
                    IERR=1
                    LIN=MAX(LIN,LIN2)
                    RETURN
                  ELSE
                    DO I=1,NVARW
                      IF (AT.EQ.SVAR(NVARB+I)) GOTO 60
                    END DO
                    NVARW=NVARW+1
                    SVAR (NVARB+NVARW)=AT
                    ITYPE(NVARB+NVARW)=-1
                    ISIZE(NVARB+NVARW)=1
 60                 IBT(IBTP,IBRN)=I+NCLS
                  END IF
                END IF
              ELSE IF (NTY.EQ.6) THEN
!-- Factors
                IBT(IBTP,IBRN)=IFAC(NFAC)
                NFAC=NFAC-1
              ELSE IF (NTY.EQ.0) THEN
!-- No operands
                N=N-1
                IBT(IBTP,IBRN)=-1
              ELSE
!-- Errors
                WRITE(6,'(A)')'Error! Wrong syntax: '//AT(1:NSZ)
                WRITE(6,*)'      at the line=',LIN
                IERR=1
                LIN=MAX(LIN,LIN2)
                RETURN
              END IF
              IF (IBT(IBTP,IBRN).GT.NCLS) THEN
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
                I=LIN
                IF (I.GT.20000) I=20000
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                IBT(IBTP,IBRN)=IBT(IBTP,IBRN)+10*NCLS*I
              END IF
            END DO !IBRN LOOP
!
            AARG=' FACTOR'
            NATY=6
            IF (NOPRND(IOP).NE.0.AND.NOPRND(IOP).NE.N) THEN
              WRITE(6,*)' Incorrect number of operands for ' &
              //'the operator ',TOPR(3*IOP-2:3*IOP-1)
              IF(IBREAK.NE.0) WRITE(6,*)'      at the line=',LIN
              IERR=1
              LIN=MAX(LIN,LIN2)
              RETURN
            END IF
            IBT(IBTP,0)=IOP
            NFAC=NFAC+1
            IFAC(NFAC)=IBTP
!-- Pop the stack
            ISTP=ISTP-1
            IF (ISTP.LT.1) THEN
              IF ( AOPR.EQ.': ' .OR. AOPR.EQ.'# ' &
              .OR. AOPR.EQ.'? ' .OR. AOPR.EQ.'??' ) THEN
                 WRITE(6,*)'The operator '//AOPR//' is not' &
              // ' enclosed by parenthese or brackets at the line=',LIN2
                IF (IBREAK.NE.0) STOP
                IERR=1
              ELSE IF (AOPR.EQ.') ') THEN
                IERR=2
              END IF
              LIN=MAX(LIN,LIN2)
              RETURN
            END IF
!-- Drop bra & kets IF they meet
            IF (NOPPR(IOPC).EQ.1.AND.NOPPR(IOP).EQ.2) THEN
              IF (IOP.NE.IOPC+3) THEN
                 WRITE(6,*)'Error! Unbalancing "{", "[" or "("'
                 WRITE(6,*)'   at the line=',LIN
                 IERR=1
                 RETURN
              END IF
              CALL GTWRD(NSZ,AT,AOPR,LIN,IUNIT,IERR,NTY)
              IF (AOPR.EQ.'  ') THEN
                WRITE(6,*)'Error! no operator at the line=',LIN
                IF (IBREAK.NE.0) STOP
                IERR=1
                LIN=MAX(LIN,LIN2)
                RETURN
              END IF
              IF (NSZ.NE.0) THEN
                WRITE(6,*)'Error! Missing oprator after ")", "}" or "]"'
                WRITE(6,*)'   at the line=',LIN
                IERR=1
                LIN=MAX(LIN,LIN2)
                RETURN
              END IF
              IOPC=(INDEX(TOPR,AOPR))/3+1
            END IF
            IOP=INDEX(TOPR,SOPR(ISTP))/3+1
            IF (NOPPR(IOPC).GT.NOPPR(IOP).OR.NOPPR(IOPC).EQ.2) EXIT
          END DO !IBTP LOOP
!-- Push operator & operand to the stack
          ISTP=ISTP+1
          SOPR(ISTP)=AOPR
          SARG(ISTP)=AARG
          NSSZ(ISTP)=NASZ
          NSTY(ISTP)=NATY
          NLIN(ISTP)=LIN2
        END IF
      END DO !MAIN LOOP
 
      END SUBROUTINE X_BTRE
!
! ??????????????????????????????????????????????????????????????????????
!  trace binary tree & internal code generation for "Xcode"
!                        coded by A.Tanaka 1993.08 (at Hiroshima Univ.)
! ??????????????????????????????????????????????????????????????????????
!
! IBT(IBTP,IBRN): the binary tree
!     IBTP: Node number
!     IBRN=0--Operator number
!     IBRN=1--Left operand or node number of the left branch
!     IBRN=2--Right operand or node number of the right branch
!
!   The classification of the value of IBT(IBTP,IBRN=1 or 2)
!
!             IBT( ... ) <   NCLS: node number
!      NCLS < IBT( ... ) < 2*NCLS: variable
!    2*NCLS < IBT( ... ) < 3*NCLS: constant
!    3*NCLS < IBT( ... ) < 4*NCLS: user function
!    4*NCLS < IBT( ... )         : system function
!
! -------------------------------------------------------------
! IBRA(IBRAP): The kind of brackets in which the code currently
!               processing is enclosed.
!       1-- ( ), 2-- { } and 3-- [ ]
! -------------------------------------------------------------
! The value of IVMD(IVMDP) can be:
!    ISTA--  store a variable
!    ILDA--  load a variable
!    IDEFF-  definition of a user function
!    IDEFV-  dummy arguments
!    IDEFT-  definition of a type
!    IBRS--  branching to a system/user function
! -------------------------------------------------------------
! NVLOC=1: Now in a definintion of a user function and
!          the variables appears should be either a dummy
!          arguments or a local variable.
! NVLOC=0: Variables are global ones.
! -------------------------------------------------------------
! NMLTSTA=1: in a multi-assignment ( , , , ... )=( , , , ... )
!
      SUBROUTINE X_INTR( &
             NCNS,NCNST,NRVALC,NRVALT &
            ,NVAR,NVARW,NRVAL        ,SVAR &
            ,NFNC,NFNCT,IFNCP        ,SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,IBT,IBTP &
            ,ICDS,ICDE,ICD &
            ,ISYSDB,ITRACE,IBREAK,IERR)
      IMPLICIT NONE
 
      INTEGER, PARAMETER ::NBTM=2000
      INTEGER, PARAMETER ::NOPM=39,NVARB=900 ,NCLS=10000
 
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NRVALX=195000&
      ,NCNSM=2000,NCNSB=NVARM,NFNCM=100,ICDM=40000
 
      INTEGER, PARAMETER ::ITXMAX=128
 
      INTEGER, PARAMETER ::NEST=1000
! internal codes
      INTEGER, PARAMETER ::IBRS=1,ISFN=2
      INTEGER, PARAMETER ::IPSH=3,IMRG=4,IDRP=5,IPOP=6
      INTEGER, PARAMETER ::ISTA=7, ISTC=8, ISTD1=9, ISTD2=10,ISTD3=11
      INTEGER, PARAMETER ::ILDA=12,ILDC=13,ILDD1=14,ILDD2=15,ILDD3=16,IALC=17
      INTEGER, PARAMETER ::INOP=37,IARY=48 ,ILDCI=39,ILDSZ=40,ILNA=41
      INTEGER, PARAMETER ::IBRE=51,IBZR=52,IBNZ=53 ,ICYC=54, IRTS=38,ILIN=60
!
      INTEGER, PARAMETER ::IDEF=100,IDEFV=101,IDEFF=102,IDEFT=103
 
!   parameter ISMCLN,ICMM,ICLN,INMB,IQES,IADD & IPLSEQ
!         depends variable TOPR in "X_BTRE"
!   these are the operator number of ;  ,  :  #  ?   +   +=
      INTEGER, PARAMETER ::ISMCLN=12,ICMM=11,ICLN=9,INMB=10,IQES=8&
      ,IADD=27,IPLSEQ=14
 
! Dummy argumets
      INTEGER :: NCNS,NCNST,NRVALC,NRVALT
 
      INTEGER :: NVAR, NVARW, NRVAL
      CHARACTER(32)  :: SVAR(NVARM)
 
      INTEGER :: NFNC, NFNCT
      INTEGER :: IFNCP(NFNCM)
      CHARACTER(32)  :: SFNC(NFNCM)
 
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: ISIZE(NVARM+NCNSM), ITYPE(NVARM+NCNSM), IVALP(NVARM+NCNSM)
 
      INTEGER :: IBT(NBTM,0:2)
      INTEGER :: IBTP
 
      INTEGER :: ICDS,ICDE
      INTEGER :: ICD(ICDM)
      INTEGER :: ISYSDB,ITRACE,IBREAK,IERR
 
! Local variables
      INTEGER :: IST(0:NEST),ITRE(0:NEST)
      INTEGER :: IBRA(NEST),IVMD(NEST),ICNT(NEST)
      INTEGER :: ICC1(0:NEST),ICC2(0:NEST)
!
      CHARACTER(32)  :: AT
      INTEGER :: IBROOT,ISTP,ISTS,IVMDP,IBRAP,ICNTP,NVLOC
      INTEGER :: IBRN, IMLTSTA, IMD, LIN, NARGS, NFTOP
      INTEGER :: IADR, ITYDEF, IADRLOC
      INTEGER :: I,J,K,IOP,ICDT
 
!----------------------------------------------------------------
! Initalization
!----------------------------------------------------------------
      IBROOT=IBTP
      ICDT=ICDS
      ICDE=ICDS
      ISTP=0
      ISTS=1
      IVMDP=1
      IBRAP=1
      ICNTP=1
      ITYDEF=-1
      NVLOC=0
      IBRA(IBRAP)=0
      IVMD(IVMDP)=ILDA
!----------------------------------------------------------------
! Tracing the binary tree
!----------------------------------------------------------------
!== Recursive entry of tracing
      DO
        IF (MOD(ISTS,2).EQ.1) THEN
!== Operators
          IF (ISTS.EQ.1) ICC1(ISTP)=ICDT
          IF (ISTS.EQ.3) ICC2(ISTP)=ICDT
          IOP=IBT(IBTP,0)
!----------------------------------------------------------------
! operators
!----------------------------------------------------------------
          SELECT CASE(IOP)
!===================
!  op #    4
!  operator (
!===================
          CASE(4)
            IF (ISTS.EQ.1) THEN
              IBRAP=IBRAP+1
              IBRA(IBRAP)=1
              ICNTP=ICNTP+1
              ICNT(ICNTP)=1
              IF (IBT(IBTP,2).LT.0) ICNT(ICNTP)=0
              I=IBT(IBTP,1)
              IBT(IBTP,1)=IBT(IBTP,2)
              IBT(IBTP,2)=I
! IDEFV: left hand side of "=" of the definition of a user function
              IF (IVMD(IVMDP).EQ.IDEF) THEN
                IVMDP=IVMDP+1
                IVMD(IVMDP)=IDEFV
              END IF
            ELSE IF (ISTS.EQ.3) THEN
! IDEFF: right hand side of "=" of the definition of a user function
              IF (IVMD(IVMDP).EQ.IDEFV) THEN
                IVMD(IVMDP)=IDEFF
              ELSE
! IBRS: system/user function call
                IVMDP=IVMDP+1
                IVMD(IVMDP)=IBRS
              END IF
            ELSE
              IVMDP=IVMDP-1
              IBRAP=IBRAP-1
              ICNTP=ICNTP-1
            END IF
!===================
!   op #    5
!  operator {
!===================
          CASE(5)
            IF (ISTS.EQ.1) THEN
              I=IBT(IBTP,1)
              IBT(IBTP,1)=IBT(IBTP,2)
              IBT(IBTP,2)=I
! the size zero constant "{}"
              IF ( IBT(IBTP,1).EQ.-1.AND.IBT(IBTP,2).EQ.-1) THEN
                IBT(IBTP,1)=1+NCLS*2
              END IF
              IBRAP=IBRAP+1
              IBRA(IBRAP)=2
              ICNTP=ICNTP+1
              ICNT(ICNTP)=1
            ELSE IF(ISTS.EQ.3) THEN
              IVMDP=IVMDP+1
              IVMD(IVMDP)=ILDA
            ELSE IF (ISTS.EQ.5) THEN
              IF (IBT(IBTP,2).GT.0) THEN
                ICDT=ICDT+1
                ICD(ICDT)=ICYC
                ICDT=ICDT+1
                ICD(ICDT)=0
              END IF
              IVMDP=IVMDP-1
              IBRAP=IBRAP-1
              ICNTP=ICNTP-1
            END IF
!===================
!   op #    6
!  operator [
!===================
          CASE(6)
            IF (ISTS.EQ.1) THEN
              I=IBT(IBTP,1)
              IBT(IBTP,1)=IBT(IBTP,2)
              IBT(IBTP,2)=I
              IBRAP=IBRAP+1
              IBRA(IBRAP)=3
              ICNTP=ICNTP+1
              ICNT(ICNTP)=1
              IVMDP=IVMDP+1
              IVMD(IVMDP)=ILDA
              J=IBT(IBTP,1)
              IF (J.GT.0.AND.J.LT.NCLS.AND.I.GT.0) THEN
                IF(IBT(J,0).EQ.ICLN.AND.IBT(J,2).LT.0)IBT(J,2)=-I
                K=IBT(J,1)
                IF(K.GT.0.AND.K.LT.NCLS) THEN
                 IF(IBT(K,0).EQ.ICLN.AND.IBT(K,2).LT.0)IBT(K,2)=-I
                END IF
              END IF
            ELSE IF (ISTS.EQ.3) THEN
              IVMD(IVMDP)=IVMD(IVMDP-1)+ICNT(ICNTP)+1
            ELSE
              IF (IBT(IBTP,2).LT.0) THEN
                ICDT=ICDT+1
                ICD(ICDT)=IARY+ICNT(ICNTP)-1
                ICDT=ICDT+1
                ICD(ICDT)=0
              END IF
              IVMDP=IVMDP-1
              IBRAP=IBRAP-1
              ICNTP=ICNTP-1
            END IF
!===================
!   op #    7
!  operator ??
!===================
          CASE(7)
            IF (ISTS.EQ.1) THEN
              I=IBT(IBTP,1)
              IF (I.GT.0.AND.I.LT.NCLS) THEN
                IF (IBT(I,0).EQ.ICLN.OR.IBT(I,0).EQ.INMB) THEN
                  J=IBT(I,1)
                  IF (J.GT.0.AND.J.LT.NCLS) THEN
                    IF (IBT(J,0).EQ.ICLN.OR.IBT(J,0).EQ.INMB) THEN
!  convert "(<1>#<2>#<3>??<4>)" into "<1>;(<2>??<4>;<3>)"
                      IBT(J,0)=ISMCLN
                      IBT(IBTP,1)=IBT(J,2)
                      IBT(J,2)=IBTP
                      IBT(I,0)=ISMCLN
                      IBT(I,1)=IBT(IBTP,2)
                      IBT(IBTP,2)=I
                      IBTP=J
                      ISTS=0
                    ELSE
!  convert "(<1>#<2>??<3>)" into "<1>;(<2>??<3>)"
                      IBT(I,0)=ISMCLN
                      IBT(IBTP,1)=IBT(I,2)
                      IBT(I,2)=IBTP
                      IBTP=I
                      ISTS=0
                    END IF
                  END IF
                END IF
              END IF
            ELSE IF (ISTS.EQ.3) THEN
              ICDT=ICDT+2
            ELSE IF (ISTS.EQ.5) THEN
!
              ICDT=ICDT+1
              ICD(ICDT)=IDRP
              ICDT=ICDT+1
!
              ICDT=ICDT+1
              ICD(ICDT)=IBRE
              ICDT=ICDT+1
              ICD(ICDT)=ICC1(ISTP)-ICDT+2
!
              K=ICC2(ISTP)+1
              ICD(K)=IBZR
              ICD(K+1)=ICDT-ICC2(ISTP)
!
            END IF
!===================
!  op #     8
!  operator ?
!===================
          CASE(8)
            IF (ISTS.EQ.1) THEN
              K=IBT(IBTP,1)
              IF (K.EQ.0) THEN
                WRITE(6,*)'Error! "?" operator with no condition.'
                IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                IERR=1
                ICDE=ICDS
              ELSE IF (K.LT.NCLS) THEN
                IF (IBT(K,0).EQ.ICLN.OR.IBT(K,0).EQ.INMB) THEN
                  IBT(IBTP,1)=IBT(K,2)
                  IBT(K,2)=IBTP
                  IBTP=K
                  ISTS=0
                ELSE IF (IBT(K,0).EQ.IQES) THEN
                  WRITE(6,*) &
                 'Error! The position of "?" operator is not correct.'
                  IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                  IERR=1
                  ICDE=ICDS
                END IF
              END IF
            ELSE IF (ISTS.EQ.3) THEN
              ICDT=ICDT+2
            ELSE IF (ISTS.EQ.5) THEN
              K=ICC2(ISTP)+1
              ICDT=ICDT+2
              ICD(ICDT-1)=INOP
              ICD(K)=IBZR
              ICD(K+1)=ICDT-ICC2(ISTP)
            END IF
!===================
!  op #      9     10
!  operator "#" or ":"
!===================
          CASE(9:10)
            IF (ISTS.EQ.1) THEN
              IF (IBRA(IBRAP).EQ.1) THEN
                K=IBT(IBTP,1)
                IF (K.GT.0.AND.K.LT.NCLS) THEN
                  IF (IBT(K,0).EQ.IQES) THEN
                    J=IBT(K,1)
                    IF (J.GT.0.AND.J.LT.NCLS) THEN
                      IF (IBT(J,0).EQ.ICLN.OR.IBT(J,0).EQ.INMB) THEN
                        IBT(K,1)=IBT(J,2)
                        IBT(J,2)=IBTP
                        IBTP=J
                        ISTS=0
                      END IF
                    END IF
                  END IF
                ELSE
                  WRITE(6,*) &
                  'A lonely "#" or ":" operator without "?" operator.'
                  IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                  IERR=1
                  ICDE=ICDS
                  RETURN
                END IF
              ELSE IF (IBRA(IBRAP).EQ.3) THEN
! set the 1st argument of a partial variable if it is ommited
                IF (ICNT(ICNTP).EQ.1.AND.IBT(IBTP,1).LT.0) THEN
                  ICDT=ICDT+1
                  ICD(ICDT)=ILDCI
                  ICDT=ICDT+1
                  ICD(ICDT)=1
                END IF
              END IF
            ELSE IF (ISTS.EQ.3) THEN
              IF (IBRA(IBRAP).EQ.3) THEN
                ICNT(ICNTP)=ICNT(ICNTP)+1
! set the 2nd argument of a partial variable if it is ommited
                IF (ICNT(ICNTP).EQ.2.AND.IBT(IBTP,2).LT.0) THEN
                  ICDT=ICDT+1
                  ICD(ICDT)=ILDSZ
                  ICDT=ICDT+1
                  ICD(ICDT)=IBT(IBTP,2)
                END IF
              END IF
            ELSE IF(ISTS.EQ.5) THEN
! : block of ? statement
              K=IBT(IBTP,1)
              IF (K.GT.0.AND.K.LT.NCLS) THEN
                IF (IBT(K,0).EQ.IQES) THEN
                  K=ICC2(ISTP)-1
                  ICD(K)=IBRE
                  ICD(K+1)=ICDT-K+1
                END IF
              END IF
            END IF
!===================
!  op #    11
!  operator ,
!===================
          CASE(11)
            IF (ISTS.EQ.1) THEN
              IF (IVMD(IVMDP).EQ.ISTA) THEN
                I=IBT(IBTP,1)
                IBT(IBTP,1)=IBT(IBTP,2)
                IBT(IBTP,2)=I
              END IF
            ELSE IF (ISTS.EQ.3) THEN
! Right side of a multi assignment, the arguments of a function
! return values
              K=IBT(IBTP,1)
              IF (IBRA(IBRAP).EQ.2.AND. &
              (K.NE.MOD(K,NCLS).OR.IBT(MOD(K,NCLS),0).NE.ICMM)) THEN
! the first element of "{ , , , ... }"
                ICDT=ICDT+2
                ICD(ICDT-1)=IPSH
                ICD(ICDT)=0
              END IF
              IF (K.LT.0) THEN
                WRITE(6,*) &
               'A operand is missing on the left side of a comma.'
                IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                IERR=1
                ICDE=ICDS
                RETURN
              END IF
              IF (IBRA(IBRAP).EQ.1.AND.IVMD(IVMDP).EQ.ISTA) THEN
! Left side of a multi-assignment "( , , , ... )=( , , , ... )"
                ICDT=ICDT+2
                ICD(ICDT-1)=IPOP
                ICD(ICDT)=0
              END IF
              IF (IBRA(IBRAP).EQ.1.AND.IVMD(IVMDP).EQ.ILDA &
               .AND.IMLTSTA.EQ.1) THEN
! Right side of a multi-assignment "( , , , ... )=( , , , ... )"
                ICDT=ICDT+2
                ICD(ICDT-1)=IPSH
                ICD(ICDT)=0
              END IF
              ICNT(ICNTP)=ICNT(ICNTP)+1
            ELSE
! An element of "{ , , , ... }" other than the first one
              IF (IBT(IBTP,2).LT.0) THEN
                WRITE(6,*) &
                'A operand is missing on the right side of a comma.'
                IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                IERR=1
                ICDE=ICDS
                RETURN
              END IF
              IF (IBRA(IBRAP).EQ.2) THEN
              ICDT=ICDT+2
                ICD(ICDT-1)=IPSH
                ICD(ICDT)=0
                ICDT=ICDT+2
                ICD(ICDT-1)=IMRG
                ICD(ICDT)=0
              END IF
            END IF
!===================
!  op #    12
!  operator ;
!===================
          CASE(12)
            IF (ISTS.EQ.3) THEN
              ICNT(ICNTP)=ICNT(ICNTP)+1
              ICDT=ICDT+2
              ICD(ICDT-1)=IDRP
            END IF
!===================
!  op #     13
!  operator =
!===================
          CASE(13)
            IF (ISTS.EQ.1) THEN
              K=IBT(IBTP,1)
              I=MOD(K,NCLS)
              IF (K.EQ.I.AND.IBT(I,0).EQ.4.AND.IBT(I,1).GT.NCLS*3) THEN
! Definition of user function
                IVMDP=IVMDP+1
                IVMD(IVMDP)=IDEF
                IF (IBT(K,1)-NCLS*3.NE.NFNC+1.AND.IBT(K,1).LT.NCLS*4) &
                  THEN
                  WRITE(6,*)'Warning! FUNCTION IS REDEFINED.'
                ELSE
                  NFNC=NFNC+1
                END IF
              ELSE
                I=IBT(IBTP,1)
                IBT(IBTP,1)=IBT(IBTP,2)
                IBT(IBTP,2)=I
                IVMDP=IVMDP+1
                IF(IVMD(IVMDP-1).EQ.ISTA) THEN
                  IVMD(IVMDP)=ISTA
                ELSE
                  IVMD(IVMDP)=ILDA
                  IMLTSTA=1
                END IF
              END IF
!            write(6,*)'ivmd',ivmd(ivmdp)
            ELSE IF (ISTS.EQ.3) THEN
              IMLTSTA=0
              IF (IVMD(IVMDP).EQ.IDEF) THEN
! On the right side of "=" in a definition of user function,
! variables are dummy argumets or local variables NVLOC=1.
                IVMD(IVMDP)=ILDA
                NVLOC=1
              ELSE
                IVMD(IVMDP)=ISTA
              END IF
!            write(6,*)'ivmd',ivmd(ivmdp)
            ELSE IF (ISTS.EQ.5) THEN
              IF (IVMD(IVMDP).EQ.ILDA) THEN
! At the end of a definition of user function.
                NARGS=ICD(NFTOP)
                DO I=1,NVARW
                  ICD(NFTOP+I*2+2)=ISIZE(NVARB+I)
                  ICD(NFTOP+I*2+3)=ITYPE(NVARB+I)
                END DO
                NVLOC=0
                ICDT=ICDT+2
                ICD(ICDT-1)=IRTS
                ICD(ICDT)=0
                ICDS=ICDT
                ICD(NFTOP+2)=ICDT
                NCNS=NCNST
                NRVALC=NRVALT
              END IF
              ICDE=ICDT
              IVMDP=IVMDP-1
            END IF
!===================
!  op #     14  15  16  17  18
!  oeprator +=  -=  *=  /=  %=
!===================
          CASE(14:18)
            IF (ISTS.EQ.1) THEN
              IVMDP=IVMDP+1
              IVMD(IVMDP)=ISTA
            ELSE IF (ISTS.EQ.3) THEN
              IVMD(IVMDP)=ILDA
            ELSE
              ICDT=ICDT+2
              ICD(ICDT-1)=IALC
              ICD(ICDT)=0
              ICDT=ICDT+2
              ICD(ICDT-1)=IOP-IPLSEQ+IADD
              ICD(ICDT)=0
              K=ICC1(ISTP)
              DO I=1,ICC2(ISTP)-ICC1(ISTP),2
                ICD(I+ICDT)=ICD(I+K)
                ICD(I+ICDT+1)=ICD(I+K+1)
                IF (ICD(I+K).GE.ISTA.AND.ICD(I+K).LE.ISTD3) &
                ICD(I+K)=ICD(I+K)+ILDA-ISTA
              END DO
              ICDT=ICC2(ISTP)-ICC1(ISTP)+ICDT
              IVMDP=IVMDP-1
            END IF
!===================
!  op #     19 20 21 22 23 24 25 26 27 28 29 30 31 32
!  operator || && <  >  <= >= != == +  -  *  /  %  **
!===================
          CASE(19:32)
            IF (ISTS.EQ.1) THEN
              IVMDP=IVMDP+1
              IVMD(IVMDP)=ILDA
            ELSE IF (ISTS.EQ.5) THEN
              ICDT=ICDT+2
              ICD(ICDT-1)=IALC
              ICD(ICDT)=0
              ICDT=ICDT+2
              ICD(ICDT-1)=IOP
              ICD(ICDT)=0
              IVMDP=IVMDP-1
            END IF
!===================
!  op #     33 34
!  operator ++ --
!===================
          CASE(33:34)
            IF (ISTS.EQ.3) THEN
              IF (IBT(IBTP,1).GT.0) THEN
                ICDT=ICDT+2
                ICD(ICDT-1)=IPSH
              END IF
              ICDT=ICDT+2
              ICD(ICDT-1)=IOP
              ICC2(ISTP)=ICDT
            ELSE IF (ISTS.EQ.5) THEN
              ICD(ICC2(ISTP))=IADR
            END IF
!===================
!  op #    35 36 37 38
!  operator !  @- @+ ^
!===================
          CASE(35:38)
            IF (ISTS.EQ.5.AND.IOP.NE.37) THEN
              ICDT=ICDT+1
              ICD(ICDT)=IOP
              ICDT=ICDT+1
              ICD(ICDT)=0
            END IF
!===================
!  op #      39
!  operator  @
!===================
          CASE(39)
            IF (ISTS.EQ.1) THEN
              IVMDP=IVMDP+1
              IVMD(IVMDP)=IDEFT
            ELSE IF (ISTS.EQ.3) THEN
              IVMDP=IVMDP-1
            ELSE IF (ISTS.EQ.5) THEN
              ITYDEF=-1
            END IF
          END SELECT
!=========================================================
        ELSE
          IBRN=ISTS/2
          K=IBT(IBTP,IBRN)
          IF (K.GT.0.AND.K.LT.NCLS) THEN
!== Trace next node
!           write(6,*)'push',ibtp,ibrn,ibt(ibtp,ibrn)
            ISTP=ISTP+1
            ITRE(ISTP)=IBTP
            IST(ISTP)=ISTS
            IBTP=IBT(IBTP,IBRN)
            ISTS=0
          ELSE IF(K.GT.NCLS) THEN
!== Operands
            IADR=IBT(IBTP,IBRN)
            IF (IADR.GT.10*NCLS) THEN
              LIN=IADR/(10*NCLS)
              IADR=MOD(IADR,10*NCLS)
            END IF
!----------------------------------------------------------------
!  operands
!----------------------------------------------------------------
            IMD=IVMD(IVMDP)
            IF (IMD.EQ.IDEFF) THEN
!== Definition of a user function
              IADR=IADR-3*NCLS
              IF (IADR.GE.NCLS)THEN
                WRITE(6,*) &
                'You can not redefine the intrinsic functions.'
!               WRITE(6,*)'The function => '//SFNC(IADR)
                IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                IERR=1
                ICDE=ICDS
                RETURN
              END IF
!>>>START SILENT
              WRITE(6,*)'defined function => '//SFNC(IADR)
!>>>END SILENT
              IFNCP(IADR)=ICDT+1
              NFTOP=ICDT+1
              ICDT=ICDT+1
!-- the number of dummy arguments
              ICD(ICDT)=ICNT(ICNTP)
              ICDT=ICDT+1
!-- the number of dummy arguments+local variables
              ICD(ICDT)=NVARW
              ICDT=ICDT+NVARW*2+2
            ELSE IF (IMD.EQ.IDEFV) THEN
              ICDT=ICDT
            ELSE IF (IMD.EQ.IDEFT) THEN
!== Definition of type
              ITYDEF=RVAL(IVALP(NCNSB+IADR-2*NCLS))
            ELSE IF (IMD.EQ.IBRS) THEN
!== Call a intrinsic function
              IF (IADR.GT.4*NCLS) THEN
                IADR=IADR-4*NCLS
                ICDT=ICDT+1
                ICD(ICDT)=ISFN
                ICDT=ICDT+1
                ICD(ICDT)=IADR
                ICDT=ICDT+1
                ICD(ICDT)=-LIN
                ICDT=ICDT+1
                ICD(ICDT)=ICNT(ICNTP)
              ELSE
!== Call a user function
                IADR=IADR-3*NCLS
                IF (IADR.GT.NFNC) THEN
                  WRITE(6,*)'Undefined function =>'//SFNC(IADR)
                  WRITE(6,*)'  at line ',LIN
                  IERR=1
                  ICDE=ICDS
                  RETURN
                ELSE
                  IF (ICD(IFNCP(IADR)).NE.ICNT(ICNTP)) THEN
                    WRITE(6,*) &
           'Number of arguments mismatch for the function '//SFNC(IADR)
!             write(6,*)IADR,IFNCP(IADR),ICD(IFNCP(IADR)),ICNT(ICNTP)
                    IF(IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                    IERR=1
                    ICDE=ICDS
                    RETURN
                  END IF
                END IF
                K=IFNCP(IADR)
!            write(6,*)icd(i),icd(i+1)
                IF (ITRACE.NE.0) THEN
                  ICDT=ICDT+1
                  ICD(ICDT)=ILIN
                  ICDT=ICDT+1
                  ICD(ICDT)=LIN
                END IF
                ICDT=ICDT+1
                ICD(ICDT)=IBRS
                ICDT=ICDT+1
                ICD(ICDT)=IADR
              END IF
            ELSE IF (IADR.GT.2*NCLS) THEN
!== Load a constant
              IF (IMD.EQ.ISTA) THEN
                WRITE(6,*)'Error! A constant appeared on the left'
                WRITE(6,*)'hand size of the operator "=" at line ',LIN
                IERR=1
                ICDE=ICDS
                RETURN
              END IF
              IADR=IADR-2*NCLS
              ICDT=ICDT+1
              ICD(ICDT)=ILDC
              ICDT=ICDT+1
              ICD(ICDT)=IADR
            ELSE
!== Load or store a variable
              IADR=IADR-NCLS
              IF (NVLOC.NE.0) THEN
!-- Dummy argument or local variable for a user function
                AT=SVAR(NVARB+IADR)
                IF(ICD(NFTOP).GE.2.AND.IADR.EQ.1) THEN
                  I=2
                ELSE IF(ICD(NFTOP).GE.2.AND.IADR.EQ.2)THEN
                  I=1
                ELSE
                  I=IADR
                END IF
                IF (ITYPE(NVARB+I).LT.0) THEN
                  IF (ITYDEF.GE.10.AND.I.LE.ICD(NFTOP)) THEN
                    WRITE(6,*) &
         'The variable already has been defined as a dummy argument'
                    WRITE(6,*)'and can not be a global variable =>',AT
                    IF(IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                    IERR=1
                    ICDE=ICDS
                    RETURN
                  ELSE IF (ITYDEF.GE.0) THEN
                    ITYPE(NVARB+I)=MOD(ITYDEF,10)
                  ELSE
                    IF (AT(1:1).GE.'i'.AND.AT(1:1).LE.'n') THEN
                      ITYPE(NVARB+I)=1
                    ELSE
                      ITYPE(NVARB+I)=2
                      IF (ISYSDB.NE.0) ITYPE(NVARB+I)=3
                    END IF
                  END IF
                ELSE
                  IF (ITYDEF.GE.0) THEN
                    WRITE(6,*)'The variable already has been defined' &
                    //' as a local variable =>',AT
                    IF (IBREAK.NE.0) WRITE(6,*)'  at line ',LIN
                    IERR=1
                    ICDE=ICDS
                    RETURN
                  END IF
                END IF
                IADRLOC=I
              END IF
              IF (NVLOC.EQ.0.OR.ITYDEF/10.EQ.1) THEN
!-- Global variable
                AT=SVAR(NVARB+IADR)
                DO I=1,NVAR
                  IF (AT.EQ.SVAR(I)) GOTO 1030
                END DO
!                write(6,*)'defined variable =>',at
                NVAR=NVAR+1
                I=NVAR
                SVAR(I)=AT
                RVAL(NRVAL)=0
                IVALP(I)=NRVAL
                ISIZE(I)=0
                ITYPE(I)=-1
                NRVAL=NRVAL+1
 1030           IF (ITYPE(I).LT.0) THEN
                  IF (ITYDEF.GE.0) THEN
                    ITYPE(I)=MOD(ITYDEF,10)
                  ELSE
                    IF (AT(1:1).GE.'i'.AND.AT(1:1).LE.'n') THEN
                      ITYPE(I)=1
                    ELSE
                      ITYPE(I)=2
                      IF (ISYSDB.NE.0) ITYPE(I)=3
                    END IF
                  END IF
                ELSE
                  IF (ITYDEF.EQ.30) THEN
                    SVAR(I)=' '//AT(1:31)
                  ELSE IF (ITYDEF.GE.0) THEN
                    IF (ITYDEF.LT.10) THEN
                      WRITE(6,*)'The variable already has been defined'
                      WRITE(6,*)' as a global variable  =>',AT
                      WRITE(6,*)'  at line ',LIN
                      IERR=1
                      ICDE=ICDS
                      RETURN
                    ELSE IF (MOD(ITYDEF,10).NE.ITYPE(I)) THEN
                      WRITE(6,*) &
                      'Inconsistant type for a global variable'
                      WRITE(6,*)'  the variable  =>',AT
                      WRITE(6,*)'  at line ',LIN
                      IERR=1
                      ICDE=ICDS
                      RETURN
                    END IF
                  END IF
                END IF
              END IF
!
              DO K=ICC1(ISTP)+1,ICDT,2
                IF (ICD(K).EQ.ILDSZ.AND.IBT(IBTP,IBRN).EQ.-ICD(K+1)) &
                  ICD(K+1)=I
              END DO
!
              IADR=I
              IF (ITYDEF/10.EQ.1) THEN
! For a global variable in a function, the address IADR of the global
! variable is linked to the local variable with an address IADRLOC by
! the internal-code command LNA and then load or store statement
! on this local variable at IADRLOC is executed.
                ICDT=ICDT+1
                ICD(ICDT)=ILNA
                ICDT=ICDT+1
                ICD(ICDT)=IADRLOC
                ICDT=ICDT+1
                ICD(ICDT)=INOP
                ICDT=ICDT+1
                ICD(ICDT)=IADR
                ICDT=ICDT+1
                ICD(ICDT)=IMD
                ICDT=ICDT+1
                ICD(ICDT)=IADRLOC
              ELSE
!-- Load or store to the variable with an address IADR
                IADR=I
                ICDT=ICDT+1
                ICD(ICDT)=IMD
                ICDT=ICDT+1
                ICD(ICDT)=IADR
              END IF
            END IF
!           write(6,*)'pass sym cns'
            IF (ITRACE.NE.0) THEN
              ICDT=ICDT+1
              ICD(ICDT)=ILIN
              ICDT=ICDT+1
              ICD(ICDT)=LIN
            END IF
          END IF
        END IF
        ISTS=ISTS+1
        IF (ISTS.LE.5) CYCLE
        IF (IBTP.EQ.IBROOT) THEN
          ICDE=ICDT
          RETURN
        END IF
!== Back to the parent node
        IBTP=ITRE(ISTP)
        ISTS=IST(ISTP)+1
!      write(6,*)'pop'
        ISTP=ISTP-1
      END DO !Recursive entry of tracing
      END
!
!  trace back the program from the current address to all functions
!  called at present and show the line numbers of their locations.
!
      SUBROUTINE X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC &
                        ,ITRACE,IBREAK)
      IMPLICIT NONE
 
! Paremeters
      INTEGER, PARAMETER ::ISFN=2,ILIN=60
      INTEGER, PARAMETER ::NFNCM=100,ICDM=40000
 
! Dummy arguments
! --internal code and pointer
      INTEGER :: ICDP
      INTEGER :: ICD(ICDM)
! --system stack
      INTEGER :: ISTP
      INTEGER :: NCS(200)
! --user functions
      INTEGER :: NFNC
      INTEGER :: IFNCP(NFNCM)
      CHARACTER(32)  :: SFNC(NFNCM)
! --system varaibles
      INTEGER :: ITRACE, IBREAK
 
! Local variables
      INTEGER :: I, IFNC, IP
! -------------------------
 
      IF (ITRACE.NE.0) THEN
        IF (ISTP.LE.0) THEN
          IF (IBREAK.NE.0) THEN
            WRITE(6,*)' in the main program'
            IF (ICD(ICDP).EQ.ISFN) THEN
              WRITE(6,*)' at line=',-ICD(ICDP+2)
            ELSE IF (ICD(ICDP-2).EQ.ILIN) THEN
              WRITE(6,*)' at line=',ICD(ICDP-1)
            ELSE IF (ICD(ICDP-4).EQ.ILIN) THEN
              WRITE(6,*)' at line=',ICD(ICDP-3)
            ELSE IF (ICD(ICDP-6).EQ.ILIN) THEN
              WRITE(6,*)' at line=',ICD(ICDP-5)
            ELSE IF (ICD(ICDP+2).EQ.ILIN) THEN
              WRITE(6,*)' at line=',ICD(ICDP+3)
            END IF
          END IF
        ELSE
          DO IFNC=1,NFNC
            IP=IFNCP(IFNC)
            IF (IP.LT.ICDP.AND.ICDP.LE.ICD(IP+2)) THEN
              WRITE(6,*)' in the function '//SFNC(IFNC)
              IF (ICD(ICDP).EQ.ISFN) THEN
                WRITE(6,*)' at line=',-ICD(ICDP+2)
              ELSE IF (ICD(ICDP-2).EQ.ILIN) THEN
                WRITE(6,*)' at line=',ICD(ICDP-1)
              ELSE IF (ICD(ICDP-4).EQ.ILIN) THEN
                WRITE(6,*)' at line=',ICD(ICDP-3)
              ELSE IF (ICD(ICDP-6).EQ.ILIN) THEN
                WRITE(6,*)' at line=',ICD(ICDP-5)
              ELSE IF (ICD(ICDP+2).EQ.ILIN) THEN
                WRITE(6,*)' at line=',ICD(ICDP+3)
              END IF
            END IF
          END DO
          DO I=ISTP,2,-1
            DO IFNC=1,NFNC
              IP=IFNCP(IFNC)
              IF (IP.LT.NCS(I).AND.NCS(I).LE.ICD(IP+2)) THEN
                WRITE(6,*)'   called from the function '//SFNC(IFNC)
                IF (ICD(ICDP).EQ.ISFN) THEN
                  WRITE(6,*)' at line=',-ICD(ICDP+2)
                ELSE IF (ICD(NCS(I)-2).EQ.ILIN) THEN
                  WRITE(6,*)'   at line=',ICD(NCS(I)-1)
                ELSE IF (ICD(NCS(I)-4).EQ.ILIN) THEN
                  WRITE(6,*)'   at line=',ICD(NCS(I)-3)
                ELSE IF (ICD(NCS(I)-6).EQ.ILIN) THEN
                  WRITE(6,*)'   at line=',ICD(NCS(I)-5)
                END IF
              END IF
            END DO
          END DO
          IF (IBREAK.NE.0) THEN
            IP=NCS(1)
            IF (ICD(ICDP).EQ.ISFN) THEN
              WRITE(6,*)' at line=',-ICD(ICDP+2)
            ELSE IF (ICD(IP-2).EQ.ILIN) THEN
              WRITE(6,*)'     called from the main program'
              WRITE(6,*)'       at line=',ICD(IP-1)
            ELSE IF (ICD(IP-4).EQ.ILIN) THEN
              WRITE(6,*)'     called from the main program'
              WRITE(6,*)'       at line=',ICD(IP-1)
            ELSE IF (ICD(IP-6).EQ.ILIN) THEN
              WRITE(6,*)'     called from the main program'
              WRITE(6,*)'       at line=',ICD(IP-1)
            ELSE IF (ICD(IP+2).EQ.ILIN) THEN
              WRITE(6,*)'     called from the main program'
              WRITE(6,*)'       at line=',ICD(IP-1)
            END IF
          END IF
        END IF
      END IF
      IF (IBREAK.NE.0) STOP
      RETURN
      END SUBROUTINE X_TRACE
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     execute internal codes of "Xcode"
!                      coded by A.Tanaka 1993.08 (at Hiroshima Univ.)
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! Important variables when executing the internal code.
!
!      ICDP:   The address of the code executing now.
!      ICDS:   The address where the code start.
!      ICDE:   The address where the code end.
!      NVARW:  The top address of the working variable area
!      NVAR:   The top address of the protected variable area
!               (including global variables and the arguments
!                and local variables of functions)
!      NVARB:  The base of variable address for the currently
!              executing program (NVARB=0 for global variables)
!              or a function (arguments and local variables)
!              The addresses for the arguments and local variables
!              for the executing function are asigned relative to this
!              value so to protect global variables and arguments
!              and local variables for previously called (and not
!              finished) functions located below this value.
!      NRVAL:  The top address +1 of the data storage RVAL for
!              the variables.
!      NRVALW: The top address +1 of the data storage RVAL for
!              the working data.
!
!      When a function called, the current values of above variable
!      are stacked untill the execution of the function is finished.
!
!      ISTP:   The stack pointer
!      NCS(ISTP)=ICDP
!      NVW(ISTP)=NVARW
!      NV(ISTP)=NVAR
!      NVB(ISTP)=NVARB
!      NVL(ISTP)=NRVAL
!
      SUBROUTINE X_EXEC( &
             NCNS,NRVALC &
            ,NVAR,NVARW,NRVAL, SVAR &
            ,NFNC,IFNCP,       SFNC &
            ,RVAL,IVALP,ISIZE,ITYPE &
            ,ICDS,ICDE,ICD &
            ,IUNIT,ISYSIO,ITRACE,IERR,IBREAK)
 
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::NBTM=2000
 
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NRVALX=195000&
      ,NCNSM=2000,NCNSB=NVARM,NFNCM=100,ICDM=40000
 
! Dummy argumets
      INTEGER :: NCNS, NRVALC
 
      INTEGER :: NVAR, NVARW, NRVAL
      CHARACTER(32)  :: SVAR(NVARM)
 
      INTEGER :: NFNC,NFNCT
      INTEGER :: IFNCP(NFNCM)
      CHARACTER(32)  :: SFNC(NFNCM)
 
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: ISIZE(NVARM+NCNSM), ITYPE(NVARM+NCNSM), IVALP(NVARM+NCNSM)
 
      INTEGER :: ICDS,ICDE
      INTEGER :: ICD(ICDM)
 
      INTEGER :: IUNIT,ISYSIO,ITRACE,IERR,IBREAK
 
! Local variables
      INTEGER :: NCS(200),NV(200),NVW(200),NVB(200),NVL(200),NVLW(200)
      INTEGER :: I, J, K, N
      INTEGER :: IOPR, IOPRND, ICDP, ISTP, NVARB, NARG, NFN, IADR, NRVALW
      INTEGER :: IPS, IPS1, IPS2, ISS1, ISS2, IPD, ISD, ITD, ISS, ITS
      INTEGER :: IS, IS1, IS2, IT, IT1, IT2, IPA, ISA, ITA, IDS, IDE, IDD
      REAL*8 :: T
 
! ------------
 
      ISTP=0
      NVARW=NVAR
      NVARB=0
      NRVALW=NRVAL
!
      ICDP=ICDS+1
      IF (ICDP.GT.ICDE) RETURN
!
!               1    2    3    4    5    6    7    8    9   10
!         +00 CALL SFN  PSH  MRG  DRP  POP  STA  STC  STD1 STD2
!         +10 STD3 LDA  LDC  LDD1 LDD2 LDD3 ALC  @@@  LOR  LAND
!         +20 LLT  LGT  LEQ  LNE  LLE  LGE  ADD  SUB  MUL  DIV
!         +30 MOD  POW  INC  DEC  NOT  MNS  NOP  RTS  LDCI LDSZ
!         +40 LNA  @@@  @@@  @@@  @@@  @@@  @@@  ARY1 ARY2 ARY3
!         +50 BRE  BZR  BNZ  CYC  @@@  @@@  @@@  @@@  @@@  LIN
!
!               1    2    3    4    5    6    7    8    9   10
!        GOTO (101, 102, 103, 104, 105, 106, 107, 200, 109, 110
!     &       ,111, 112, 113, 114, 115, 116, 117, 118, 119, 120
!     &       ,121, 122, 123, 124, 125, 126, 127, 128, 129, 130
!     &       ,131, 132, 133, 134, 135, 136, 137, 138, 139, 200
!     &       ,141, 200, 200, 200, 200, 200, 200, 148, 149, 150
!     &       ,151, 152, 153, 154, 200, 200, 200, 200, 200, 160
!     &       ),ICD(ICDP)
      DO !main loop
      IOPRND=ICD(ICDP+1)
      IF (ITRACE.EQ.2) CALL X_DASM(ICDP,ICDP+1,ICD)
      IOPR=ICD(ICDP)
      SELECT CASE(IOPR)
!===================
!  op #       1
!  operator call (call a user function )
!===================
      CASE(1)
        ISTP=ISTP+1
        NCS(ISTP)=ICDP
        NVW(ISTP)=NVARW
        NV(ISTP)=NVAR
        NVB(ISTP)=NVARB
        NVL(ISTP)=NRVAL
!        WRITE(6,*)'call NRVAL=',NRVAL
        NVLW(ISTP)=NRVALW
        ICDP=IFNCP(IOPRND)
        NVARB=NVARW-ICD(ICDP)
        NVAR=ICD(ICDP+1)+NVARB
        NRVAL=NRVALW
!        WRITE(6,*)'call NRVAL=',NRVAL
!  check the types of the arguments
        DO I=1,ICD(ICDP)
          IF (ICD(ICDP+2+2*I+1).NE.ITYPE(NVARB+I)) THEN
            IF (ITYPE(NVARB+I).EQ.0.OR.ICD(ICDP+2+2*I+1).EQ.0 &
            .OR.ITYPE(NVARB+I).EQ.4.OR.ICD(ICDP+2+2*I+1).EQ.4) THEN
              WRITE(6,*)'Error! An incompatible type in the argument.'
              WRITE(6,*)'  The function called: '//SFNC(IOPRND)
              WRITE(6,*)'  The argument number: ',I
              CALL X_TRACE(ICD(NCS(ISTP)),ICD,ISTP,NCS,NFNC,IFNCP,SFNC &
                           ,ITRACE,IBREAK)
              IERR=1
              RETURN
            END IF
          END IF
          ITYPE(NVARB+I)=ICD(ICDP+2+2*I+1)
        END DO
!
!  initialization of local variables
        DO I=ICDP+ICD(ICDP)*2+4,ICDP+ICD(ICDP+1)*2+2,2
          NVARW=NVARW+1
          IVALP(NVARW)=NRVAL
          ISIZE(NVARW)=0
          ITYPE(NVARW)=ICD(I+1)
!          write(6,*)icd(i),icd(i+1)
          NRVAL=NRVAL+1
        END DO
        NRVALW=NRVAL
        ICDP=ICDP+ICD(ICDP+1)*2+2
!===================
!  op #       2
!  operator sfn (call a system function)
!===================
      CASE(2)
        NARG=ICD(ICDP+3)
        IF (IOPRND.GT.200) THEN
!>>>START EXFNC
!--  Graphic functions (needs to compiled with the Pig graphic
!    library only usefull for old versions 1.2 or before)
          NFN=IOPRND-200
          CALL X_EXFN(NFN,NARG &
                     ,IVALP,ITYPE,ISIZE,RVAL &
                     ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                     ,NVL,NVLW,ISTP &
                     ,IUNIT,IERR)
!>>>END EXFNC
        ELSE IF (IOPRND.GT.100) THEN
!>>>START IOFNC
!-- IO functions
          NFN=IOPRND-100
          CALL X_IOFN(NFN,NARG &
                     ,IVALP,ITYPE,ISIZE,RVAL &
                     ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                     ,NVL,NVLW,ISTP &
                     ,IUNIT,ISYSIO,IERR)
!>>>END IOFNC
        ELSE
!-- System functions (minimum set)
          NFN=IOPRND
          CALL X_SYFN(NFN,NARG &
                     ,IVALP,ITYPE,ISIZE,RVAL &
                     ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                     ,NVL,NVLW,ISTP &
                     ,IUNIT,ISYSIO,IERR)
        END IF
        IF (IERR.NE.0) THEN
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC &
                      ,ITRACE,IBREAK)
          RETURN
        END IF
        ICDP=ICDP+2
!===================
!  op #        3
!  operator push (push a variable to the system stack)
!===================
      CASE(3)
        IPS=IVALP(NVARW)
        IF (IPS.LT.NRVAL) THEN
          K=NRVALW-IPS
          DO I=NRVALW,NRVALW+ISIZE(NVARW)-1
            RVAL(I)=RVAL(I-K)
          END DO
          IVALP(NVARW)=NRVALW
          NRVALW=NRVALW+MAX(ISIZE(NVARW),1)
        END IF
!===================
!  op #        4
!  operator mrg  (marge the top & 2nd stack)
!===================
      CASE(4)
        IPS2=IVALP(NVARW)
        IPS1=IVALP(NVARW-1)
        ISS2=ISIZE(NVARW)
        ISS1=ISIZE(NVARW-1)
        IF (IPS2.GT.IPS1+ISS1) THEN
          K=IPS2-IPS1-ISS1
          DO I=IPS2,IPS2+ISS2-1
            RVAL(I-K)=RVAL(I)
          END DO
        END IF
        NVARW=NVARW-1
        ISIZE(NVARW)=ISS1+ISS2
        ITYPE(NVARW)=MAX(ITYPE(NVARW),ITYPE(NVARW+1))
!===================
!  op #        5
!  operator drp  (drop whole system stack)
!===================
      CASE(5)
        NVARW=NVAR
        NRVALW=NRVAL
!===================
!  op #        6
!  operator pop  (pop the top stack)
!===================
      CASE(6)
        IF (IVALP(NVARW).GE.NRVAL) NRVALW=IVALP(NVARW)
        NVARW=NVARW-1
!===================
!  op #       7
!  operator sta  (store by address)
!===================
      CASE(7)
        IPD=IVALP(IOPRND+NVARB)
        ISD=ISIZE(IOPRND+NVARB)
        ITD=ITYPE(IOPRND+NVARB)
!
        ISS=ISIZE(NVARW)
        ITS=ITYPE(NVARW)
        IF (NVARW.EQ.NVAR) THEN
          WRITE(6,*)'Error! Nothing on the system stack (sta).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
!          ISS=0
!          ITS=ITD
        END IF
!
        IF (ITS.EQ.-1) ITS=ITD
        IF (ITS.EQ.ITD.OR. &
          (ITS.NE.0.AND.ITD.NE.0.AND.ITS.NE.4.AND.ITD.NE.4)) THEN
          CALL RSZVAR(ISS,ISD,IPD &
                     ,NVAR,NVARW,ISIZE,IVALP &
                     ,NRVAL,NRVALW,RVAL &
                     ,ISTP,NVL,NVLW)
          IPS=IVALP(NVARW)
          ISIZE(IOPRND+NVARB)=ISS
          DO I=0,ISS-1
            RVAL(IPD+I)=RVAL(IPS+I)
          END DO
        ELSE
          WRITE(6,*)'Error! Type mismatch in store operation (sta).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       9
!  operator std1  (store with one subscript)
!===================
      CASE(9)
        IPD=IVALP(IOPRND+NVARB)
        ISD=ISIZE(IOPRND+NVARB)
        ITD=ITYPE(IOPRND+NVARB)
        ITS=ITYPE(NVARW-1)
!
        IPA=IVALP(NVARW)
        ISA=ISIZE(NVARW)
        ITA=ITYPE(NVARW)
        IF (NVARW.EQ.NVAR) THEN
          WRITE(6,*)'Error! Nothing on the system stack (std1).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        IF (ITS.EQ.-1) ITS=ITD
        IF (ITS.EQ.ITD.OR. &
        (ITS.NE.0.AND.ITD.NE.0.AND.ITS.NE.4.AND.ITD.NE.4)) THEN
          N=NINT(RVAL(IPA))
          DO I=1,ISA-1
            IF(N.LT.NINT(RVAL(IPA+I))) N=NINT(RVAL(IPA+I))
          END DO
          IF (N.LT.1) THEN
            WRITE(6,*) &
            'Error! Subscript out of range in store operation (std1)'
            CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC &
                        ,ITRACE,IBREAK)
            IERR=1
            RETURN
          END IF
          IF (ISD.LT.N) THEN
            CALL RSZVAR(N,ISD,IPD &
                       ,NVAR,NVARW,ISIZE,IVALP &
                       ,NRVAL,NRVALW,RVAL &
                       ,ISTP,NVL,NVLW)
          END IF
          IPA=IVALP(NVARW)
          ISA=ISIZE(NVARW)
          IPS=IVALP(NVARW-1)
          ISS=ISIZE(NVARW-1)
          IF (ISS.NE.0) THEN
            IF (ISA.EQ.1) THEN
              RVAL(IPD+NINT(RVAL(IPA))-1)=RVAL(IPS)
            ELSE
              IF (IPS.LT.NRVAL) THEN
                DO I=0,ISS
                  RVAL(NRVALW+I)=RVAL(IPS+I)
                END DO
                IPS=NRVALW
              END IF
              DO I=0,ISA-1
                RVAL(IPD+NINT(RVAL(IPA+I))-1)=RVAL(IPS+MOD(I,ISS))
              END DO
            END IF
          END IF
          NVARW=NVARW-1
        ELSE
          WRITE(6,*)'Error! Type mismatch in store operation (std1).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       10
!  operator std2 (store with two subscripts)
!===================
      CASE(10)
        IDS=NINT(RVAL(IVALP(NVARW-1)))-1
        IDE=NINT(RVAL(IVALP(NVARW)))
        IF (NVARW.EQ.NVAR) THEN
          WRITE(6,*)'Error! Nothing on the system stack (std2).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!
        ITS=ITYPE(NVARW-2)
!
        IPD=IVALP(IOPRND+NVARB)
        ISD=ISIZE(IOPRND+NVARB)
        ITD=ITYPE(IOPRND+NVARB)
        IF (ITS.EQ.-1) ITS=ITD
        IF (ITS.EQ.ITD.OR. &
        (ITS.NE.0.AND.ITD.NE.0.AND.ITS.NE.4.AND.ITD.NE.4)) THEN
          IF (IDS+1.GT.IDE.OR.IDS.LT.0.OR.IDE.LT.1) THEN
            WRITE(6,*) &
            'Error! Subscript out of range in store operation (std2)'
            CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC &
                        ,ITRACE,IBREAK)
            IERR=1
            RETURN
          END IF
          IF (IDE.GT.ISD) THEN
            CALL RSZVAR(IDE,ISD,IPD &
                       ,NVAR,NVARW,ISIZE,IVALP &
                       ,NRVAL,NRVALW,RVAL &
                       ,ISTP,NVL,NVLW)
          END IF
          IPS=IVALP(NVARW-2)
          ISS=ISIZE(NVARW-2)
          IF (ISS.NE.0) THEN
            IF (IPD+IDS.GT.IPS.AND.IPD+IDS.LE.IPS+ISS) THEN
              DO I=IDE-IDS-1,0,-1
                RVAL(IPD+IDS+I)=RVAL(IPS+MOD(I,ISS))
              END DO
            ELSE
              DO I=0,IDE-IDS-1
                RVAL(IPD+IDS+I)=RVAL(IPS+MOD(I,ISS))
              END DO
            END IF
          END IF
          NVARW=NVARW-2
        ELSE
          WRITE(6,*)'Error! Type mismatch in store opration (std2).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
! op #        11
!  operator std3  (store with three subscripts)
!===================
      CASE(11)
        IDS=NINT(RVAL(IVALP(NVARW-2)))
        IDE=NINT(RVAL(IVALP(NVARW-1)))
        IDD=NINT(RVAL(IVALP(NVARW)))
!
        IPS=IVALP(NVARW-3)
        ISS=ISIZE(NVARW-3)
        ITS=ITYPE(NVARW-3)
!
        IPD=IVALP(IOPRND+NVARB)
        ISD=ISIZE(IOPRND+NVARB)
        ITD=ITYPE(IOPRND+NVARB)
!
        IF (NVARW.EQ.NVAR) THEN
          WRITE(6,*)'Error! Nothing on the system stack (std3).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!
        IF (ITS.EQ.-1) ITS=ITD
        IF (ITS.EQ.ITD.OR. &
        (ITS.NE.0.AND.ITD.NE.0.AND.ITS.NE.4.AND.ITD.NE.4)) THEN
!
          IF (IDE.LT.IDS) THEN
            I=IDE
            IDE=IDS
            IDS=I
          END IF
          IDS=IDS-1
          IF (IPS.LT.NRVAL) THEN
            DO I=0,ISS-1
              RVAL(NRVALW+I)=RVAL(IPS+I)
            END DO
            IPS=NRVALW
            IVALP(NVARW-3)=IPS
            NRVALW=NRVALW+ISS
          END IF
          IF (IDE.GT.ISD) THEN
            CALL RSZVAR(IDE,ISD,IPD &
                       ,NVAR,NVARW,ISIZE,IVALP &
                       ,NRVAL,NRVALW,RVAL &
                       ,ISTP,NVL,NVLW)
          END IF
          IPS=IVALP(NVARW-3)
          IF (ISS.NE.0)THEN
            N=(IDE-IDS-1)/ABS(IDD)+1
!            WRITE(6,*)'IDS,IDE,IDD,N',IDE,IDS,IDD,N
            DO I=0,N-1
              RVAL(IPD+IDS+I*IDD)=RVAL(IPS+MOD(I,ISS))
            END DO
          END IF
          NVARW=NVARW-3
        ELSE
          WRITE(6,*)'Error! Type mismatch in store operation (std3).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
! op #        12
!  operator lda  (load by address)
!===================
      CASE(12)
        NVARW=NVARW+1
        IVALP(NVARW)=IVALP(IOPRND+NVARB)
        ISIZE(NVARW)=ISIZE(IOPRND+NVARB)
        ITYPE(NVARW)=ITYPE(IOPRND+NVARB)
!===================
! op #        13
!  operator ldc  (load constant)
!===================
      CASE(13)
        NVARW=NVARW+1
        IPS=IVALP(NCNSB+IOPRND)
        IS=ISIZE(NCNSB+IOPRND)
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=IS
        ITYPE(NVARW)=ITYPE(NCNSB+IOPRND)
        K=IPS-NRVALW
        DO I=NRVALW,NRVALW+IS-1
          RVAL(I)=RVAL(I+K)
        END DO
        NRVALW=NRVALW+MAX(IS,1)
!        write(6,*)'cnst=',RVAL(NRVALW-1)
!===================
! op #        14
!  operator ldd1  (load with one subscript)
!===================
      CASE(14)
        IPS=IVALP(IOPRND+NVARB)
        IPA=IVALP(NVARW)
        ISS=ISIZE(IOPRND+NVARB)
        ISA=ISIZE(NVARW)
        N=NINT(RVAL(IPA))
        IF ( N.GT.ISS .OR. N.LT.1 ) THEN
          WRITE(6,*) &
          'Error! Subscript out of range in load operation (ldd1).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        IF (ISA.EQ.1) THEN
          IVALP(NVARW)=IPS+NINT(RVAL(IPA))-1
          ISIZE(NVARW)=1
          ITYPE(NVARW)=ITYPE(IOPRND+NVARB)
        ELSE
          DO I=0,ISA-1
            RVAL(NRVALW+I)=RVAL(IPS+NINT(RVAL(IPA+I)-1))
          END DO
          IVALP(NVARW)=NRVALW
          ISIZE(NVARW)=ISA
          ITYPE(NVARW)=ITYPE(IOPRND+NVARB)
          NRVALW=NRVALW+ISA
        END IF
!===================
! op #        15
!  operator ldd2  (load with two subscripts)
!===================
      CASE(15)
        IDS=NINT(RVAL(IVALP(NVARW-1)))
        IDE=NINT(RVAL(IVALP(NVARW)))
        ISS=ISIZE(IOPRND+NVARB)
        IF ( IDS.GT.IDE .OR. IDE.GT.ISS .OR. IDS.LT.1 ) THEN
          WRITE(6,*) &
          'Error! Subscript out of range in load operation (ldd2)'
          WRITE(6,*)' IDS=',IDS,' IDE=',IDE,' ISS=',ISS
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        NVARW=NVARW-1
        IVALP(NVARW)=IVALP(IOPRND+NVARB)+IDS-1
        ISIZE(NVARW)=IDE-IDS+1
        ITYPE(NVARW)=ITYPE(IOPRND+NVARB)
!===================
! op #        16
!  operator ldd3  (load with three subscripts)
!===================
      CASE(16)
        IDS=NINT(RVAL(IVALP(NVARW-2)))
        IDE=NINT(RVAL(IVALP(NVARW-1)))
        IDD=NINT(RVAL(IVALP(NVARW)))
        IPS=IVALP(IOPRND+NVARB)
        ISS=ISIZE(IOPRND+NVARB)
        IF ( (IDE-IDS+1)*IDD.LT.0 .OR. MAX(IDE,IDS).GT.ISS &
          .OR. MIN(IDS,IDE).LT.1 ) THEN
          WRITE(6,*) &
          'Error! Subscripts out of range in load operation (ldd3).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        N=(IDE-IDS)/IDD+1
        J=MIN(IDS,IDE)
        DO I=0,N-1
          RVAL(NRVALW+I)=RVAL(IPS+IDS+I*IDD-1)
        END DO
        NVARW=NVARW-2
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=N
        ITYPE(NVARW)=ITYPE(IOPRND+NVARB)
        NRVALW=NRVALW+N
!===================
! op #        17
!  operator alc  (memory reallocation)
!===================
      CASE(17)
        IS2 =ISIZE(NVARW)
        IS1 =ISIZE(NVARW-1)
        IPS2=IVALP(NVARW)
        IPS1=IVALP(NVARW-1)
        IT2 =ITYPE(NVARW)
        IT1 =ITYPE(NVARW-1)
        IF (IT1.EQ.-1) IT1=IT2
        IF (IT2.EQ.-1) IT2=IT1
        IF (IT1.EQ.IT2.OR. &
        ( IT1.NE.0 .AND. IT2.NE.0 .AND. IT1.NE.4 .AND. IT2.NE.4) )THEN
          IT  =MAX(IT1,IT2)
        ELSE
        WRITE(6,*) &
          'Error! Type mismatch in arithmetic operation (alc).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        IF ((IS1.EQ.0.OR.IS2.EQ.0).AND.IT.NE.0) THEN
          WRITE(6,*) &
          'Size zero! The value is not defined for a variable (alc).'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!
        IVALP(NVARW+1)=NRVALW
        K=0
        IF (IPS2.GE.NRVAL) K=1
        IF (IPS1.GE.NRVAL) K=2
        IPD=IVALP(NVARW-K+1)
        IF (IT.NE.0) THEN
          IS=1
!  cyclic extension
          IF (IS1.EQ.IS2)  THEN
            IS=IS1
          ELSE IF (IS2.GT.IS1) THEN
            DO J=0,IS1-1
              T=RVAL(IPS1+J)
              DO I=NRVALW+J,NRVALW+J+IS2-1,IS1
                RVAL(I)=T
              END DO
            END DO
            IS=IS2
            IPS1=NRVALW
          ELSE
            DO J=0,IS2-1
              T=RVAL(IPS2+J)
              DO I=NRVALW+J,NRVALW+J+IS1-1,IS2
                RVAL(I)=T
              END DO
            END DO
            IS=IS1
            IPS2=NRVALW
          END IF
        ELSE
          IS=IS1+IS2
        END IF
        NVARW=NVARW-1
        ISIZE(NVARW)=IS
        IVALP(NVARW)=IPD
        ITYPE(NVARW)=IT
        NRVALW=IPD+MAX(IS,1)
!===================
!  op #       19
!  operator lor  (  ||  logical or)
!===================
      CASE(19)
        IF (IT.EQ.4) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I))+NINT(RVAL(IPS2+I)).EQ.0) THEN
              RVAL(IPD+I)=0D0
            ELSE
              RVAL(IPD+I)=1D0
            END IF
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "||" !!'
          WRITE(6,*)'Type of the operands must be logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       20
!  operator land (  &&  logical and)
!===================
      CASE(20)
        IF (IT.EQ.4) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=NINT(RVAL(IPS1+I)*RVAL(IPS2+I))
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "&&" !!'
          WRITE(6,*)'The type of the operands must be logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       21
!  operator llt   ( < )
!===================
      CASE(21)
        IF (IT.EQ.1) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).LT.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            IF (RVAL(IPS1+I).LT.RVAL(IPS2+I)) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          DO I=0,MIN(IS1,IS2)-1
            IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
              IF (NINT(RVAL(IPS1+I)).LT.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=1D0
              ELSE
                RVAL(IPD)=0D0
              END IF
              GOTO 520
            END IF
          END DO
          IF(IS1.LT.IS2) THEN
            RVAL(IPD)=1D0
          ELSE
            RVAL(IPD)=0D0
          END IF
        ELSE
          WRITE(6,*)'Type mismatch for the operator "<" !!'
          WRITE(6,*)'The operands should not be type logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
 520    ITYPE(NVARW)=4
!===================
!  op #       22
!  operator lgt   ( > )
!===================
      CASE(22)
        IF (IT.EQ.1) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).GT.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            IF (RVAL(IPS1+I).GT.RVAL(IPS2+I)) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          DO I=0,MIN(IS1,IS2)-1
            IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
              IF (NINT(RVAL(IPS1+I)).GT.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=1D0
              ELSE
                RVAL(IPD)=0D0
              END IF
              GOTO 521
            END IF
          END DO
          IF(IS1.GT.IS2) THEN
            RVAL(IPD)=1D0
          ELSE
            RVAL(IPD)=0D0
          END IF
        ELSE
          WRITE(6,*)'Type mismatch for the operator ">" !!'
          WRITE(6,*)'The operands should not be type logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
 521    ITYPE(NVARW)=4
!===================
!  op #       23
!  operator leq   ( == )
!===================
      CASE(23)
        IF (IT.EQ.1.OR.IT.EQ.4) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).EQ.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            IF (RVAL(IPS1+I).EQ.RVAL(IPS2+I)) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
            RVAL(IPD+I)=0D0
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          IF (IS1.EQ.IS2) THEN
            DO I=0,IS1-1
              IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=0D0
                GOTO 522
              END IF
            END DO
            RVAL(IPD)=1D0
          ELSE
            RVAL(IPD)=0D0
          END IF
        END IF
 522    ITYPE(NVARW)=4
!===================
!  op #       24
!  operator lne   ( !=   not equal)
!===================
      CASE(24)
        IF (IT.EQ.1.OR.IT.EQ.4) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            IF (RVAL(IPS1+I).NE.RVAL(IPS2+I)) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
            RVAL(IPD+I)=1D0
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          IF (IS1.EQ.IS2) THEN
            DO I=0,IS1-1
              IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=1D0
                GOTO 523
              END IF
            END DO
            RVAL(IPD)=0D0
          ELSE
            RVAL(IPD)=1D0
          END IF
        END IF
 523    ITYPE(NVARW)=4
!===================
!  op #       25
!  operator lle   ( <= )
!===================
      CASE(25)
        IF (IT.EQ.1) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).LE.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            IF (RVAL(IPS1+I).LE.RVAL(IPS2+I)) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          DO I=0,MIN(IS1,IS2)-1
            IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
              IF (NINT(RVAL(IPS1+I)).LT.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=1D0
              ELSE
                RVAL(IPD)=0D0
              END IF
              GOTO 524
            END IF
          END DO
          IF(IS1.LE.IS2) THEN
            RVAL(IPD)=1D0
          ELSE
            RVAL(IPD)=0D0
          END IF
        ELSE
          WRITE(6,*)'Type mismatch for the operator "<=" !!'
          WRITE(6,*)'The operands should not be type logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
 524    ITYPE(NVARW)=4
!===================
!  op #       26
!  operator lge ( >= )
!===================
      CASE(26)
        IF (IT.EQ.1) THEN
          DO I=0,IS-1
            IF (NINT(RVAL(IPS1+I)).GE.NINT(RVAL(IPS2+I))) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=SIGN(0.5D0,RVAL(IPS1+I)-RVAL(IPS2+I))+0.5
          END DO
        ELSE IF (IT.EQ.0) THEN
          ISIZE(NVARW)=1
          DO I=0,MIN(IS1,IS2)-1
            IF (NINT(RVAL(IPS1+I)).NE.NINT(RVAL(IPS2+I))) THEN
              IF (NINT(RVAL(IPS1+I)).GT.NINT(RVAL(IPS2+I))) THEN
                RVAL(IPD)=1D0
              ELSE
                RVAL(IPD)=0D0
              END IF
              GOTO 525
            END IF
          END DO
          IF(IS1.GE.IS2) THEN
            RVAL(IPD)=1D0
          ELSE
            RVAL(IPD)=0D0
          END IF
        ELSE
          WRITE(6,*)'Type mismatch for the operator ">=" !!'
          WRITE(6,*)'The operands should not be type logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
 525    ITYPE(NVARW)=4
!===================
!  op #       27
!  operator add  ( + )
!===================
      CASE(27)
        IF (IT.NE.0.AND.IT.NE.4) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)+RVAL(IPS2+I)
          END DO
        ELSE IF (IT.EQ.0) THEN
          IF (IPD+IS1.LE.IPS2) THEN
            DO I=IS1,IS1+IS2-1
              RVAL(IPD+I)=RVAL(IPS2+I-IS1)
            END DO
          ELSE
            DO I=IS1+IS2-1,IS1,-1
              RVAL(IPD+I)=RVAL(IPS2+I-IS1)
            END DO
          END IF
          DO I=0,IS1-1
            RVAL(IPD+I)=RVAL(IPS1+I)
          END DO
          ISIZE(NVARW)=IS1+IS2
          NRVALW=IPD+MAX(IS1+IS2,1)
        ELSE
          WRITE(6,*)'Type mismatch for the operator "+" !!'
          WRITE(6,*)'The operands should not be type logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       28
!  operator sub  ( - )
!===================
      CASE(28)
        IF (IT.EQ.1.OR.IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)-RVAL(IPS2+I)
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "-" !!'
          WRITE(6,*) &
           'The operand must not be type character or logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       29
!  operator mul  ( * )
!===================
      CASE(29)
        IF (IT.EQ.1.OR.IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)*RVAL(IPS2+I)
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "*" !!'
          WRITE(6,*) &
           'The operands should not be type character or logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #      30
!  operator div  ( / )
!===================
      CASE(30)
        IF (IT.EQ.2.OR.IT.EQ.3) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)/RVAL(IPS2+I)
          END DO
        ELSE IF (IT.EQ.1) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=NINT(RVAL(IPS1+I))/NINT(RVAL(IPS2+I))
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "/" !!.'
          WRITE(6,*) &
           'The operands should not be type character or logical.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       31
!  operator mod  ( % )
!===================
      CASE(31)
        IF (IT.EQ.1) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=MOD(NINT(RVAL(IPS1+I)),NINT(RVAL(IPS2+I)))
          END DO
        ELSE
          WRITE(6,*)'Type mismatch for the operator "%" !!'
          WRITE(6,*)'The operand must be integer.'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       32
!  operator pow  ( ** power)
!===================
      CASE(32)
        IF (IT2.EQ.1) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)**NINT(RVAL(IPS2+I))
          END DO
        ELSE
          DO I=0,IS-1
            RVAL(IPD+I)=RVAL(IPS1+I)**RVAL(IPS2+I)
          END DO
        END IF
!===================
!  op #       33
!  operator inc ( ++  increment)
!===================
      CASE(33)
        ITD=ITYPE(NVARB+IOPRND)
        ISD=ISIZE(NVARB+IOPRND)
        IF(ITD.NE.0.AND.ITD.NE.4.AND.ISD.NE.0) THEN
          IPD=IVALP(NVARB+IOPRND)
          DO I=0,ISD-1
            RVAL(IPD+I)=RVAL(IPD+I)+1D0
          END DO
        ELSE
          IF (ISD.NE.0.AND.ITD.NE.-1) THEN
            WRITE(6,*)'Type mismatch for the operator "++" !!'
            WRITE(6,*) &
           'The operand should not be type character or logical.'
          ELSE
           WRITE(6,*) &
            'Size zero! The value is not defined for "++" operator.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       34
!  operator dec   ( -- decrement)
!===================
      CASE(34)
        ITD=ITYPE(NVARB+IOPRND)
        ISD=ISIZE(NVARB+IOPRND)
        IF(ITD.NE.0.AND.ITD.NE.4.AND.ISD.NE.0) THEN
          IPD=IVALP(NVARB+IOPRND)
          DO I=0,ISD-1
            RVAL(IPD+I)=RVAL(IPD+I)-1D0
          END DO
        ELSE
          IF (ISD.NE.0.AND.ITD.NE.-1) THEN
            WRITE(6,*)'Type mismatch for the operator "--" !!'
            WRITE(6,*) &
         'The operand should not be the type "character" or "logical".'
          ELSE
            WRITE(6,*) &
            'Size zero! The value is not defined for "--" operator.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       35
!  operator not   ( ! logical not)
!===================
      CASE(35)
        ITS=ITYPE(NVARW)
        ISS=ISIZE(NVARW)
        IF (ITS.EQ.4.AND.ISS.NE.0) THEN
          IPS=IVALP(NVARW)
          IF (IPS.LT.NRVAL) THEN
            IPD=NRVALW
            IVALP(NVARW)=IPD
            NRVALW=NRVALW+ISIZE(NVARW)
          ELSE
            IPD=IPS
          END IF
          DO I=0,ISIZE(NVARW)-1
            IF (NINT(RVAL(IPS+I)).EQ.0) THEN
              RVAL(IPD+I)=1D0
            ELSE
              RVAL(IPD+I)=0D0
            END IF
          END DO
        ELSE
          IF (ISS.NE.0.AND.ITS.NE.-1) THEN
            WRITE(6,*)'Type mismatch for the operator not(!) !!'
            WRITE(6,*)'The operand must be type logical.'
          ELSE
            WRITE(6,*) &
         'Size zero! The value is not defined for the not(!) operator.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       36
!  operator mns  (sign -)
!===================
      CASE(36)
        ITS=ITYPE(NVARW)
        ISS=ISIZE(NVARW)
        IF(ITS.NE.0.AND.ITS.NE.4.AND.ISS.NE.0) THEN
          IPS=IVALP(NVARW)
          IF (IPS.LT.NRVAL) THEN
            IPD=NRVALW
            IVALP(NVARW)=IPD
            NRVALW=NRVALW+ISIZE(NVARW)
          ELSE
            IPD=IPS
          END IF
          DO I=0,ISIZE(NVARW)-1
            RVAL(IPD+I)=-RVAL(IPS+I)
          END DO
        ELSE
          IF (ISS.NE.0.AND.ITS.NE.-1) THEN
            WRITE(6,*)'Type mismatch for the minus sign operator!!'
            WRITE(6,*) &
             'The operand should not be type character or logical.'
          ELSE
            WRITE(6,*)'Size zero! '// &
               'The value is not defined for the minus sign operator.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       37
!  operator nop   (no operation)
!===================
      CASE(37)
!===================
!  op #        38
!  operator rts  (return from subroutine)
!===================
      CASE(38)
        IPD=NVLW(ISTP)
        DO I=NVARB+1,NVW(ISTP)
          K=IVALP(I)
          IF (K.LT.IPD.AND.K.GE.NVL(ISTP)) IPD=K
        END DO
        N=NRVALW
        DO I=NVAR+1,NVARW
          K=N-IVALP(I)
          DO J=N,N+ISIZE(I)-1
            RVAL(J)=RVAL(J-K)
          END DO
          K=I-NVAR+NVARB
          IVALP(K)=N-NRVALW+IPD
          ISIZE(K)=ISIZE(I)
          ITYPE(K)=ITYPE(I)
          N=N+ISIZE(I)
        END DO
        K=NRVALW-IPD
        DO I=IPD,IPD+N-NRVALW-1
          RVAL(I)=RVAL(I+K)
        END DO
        NRVALW=I
        NVARW=NVARW-NVAR+NVARB
        NVAR=NV(ISTP)
        NVARB=NVB(ISTP)
        NRVAL=NVL(ISTP)
        ICDP=NCS(ISTP)
        ISTP=ISTP-1
!        WRITE(6,*)'at RTS'
!        WRITE(6,*)'NRVAL=',NRVAL
!        WRITE(6,*)' RVAL(NRVAL)=',RVAL(NRVAL)
!        WRITE(6,*)' RVAL(NRVAL-1)=',RVAL(NRVAL-1)
!        WRITE(6,*)' RVAL(NRVAL-2)=',RVAL(NRVAL-2)
!        WRITE(6,*)'NRVALW=',NRVALW
!        WRITE(6,*)' RVAL(NRVALW)=',RVAL(NRVALW)
!        WRITE(6,*)' RVAL(NRVALW-1)=',RVAL(NRVALW-1)
!        WRITE(6,*)' RVAL(NRVALW-2)=',RVAL(NRVALW-2)
!===================
! op #        39
!  operator ldci  (load integer constant)
!===================
      CASE(39)
        NVARW=NVARW+1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=1
        ITYPE(NVARW)=1
        RVAL(NRVALW)=DBLE(IOPRND)
        NRVALW=NRVALW+1
!===================
! op #        40
!  operator ldsz  (load size of a variable)
!===================
      CASE(40)
        NVARW=NVARW+1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=1
        ITYPE(NVARW)=1
        RVAL(NRVALW)=DBLE(ISIZE(IOPRND+NVARB))
        NRVALW=NRVALW+1
!===================
! op #        41
!  operator lna  (make a link to a global variable )
!===================
      CASE(41)
        IADR=ICD(ICDP+3)
        IVALP(IOPRND+NVARB)=IVALP(IADR)
        ISIZE(IOPRND+NVARB)=ISIZE(IADR)
        ITYPE(IOPRND+NVARB)=ITYPE(IADR)
!===================
!  op #       48
!  operator ary1
!===================
      CASE(48)
        IPS=IVALP(NVARW)
        ISS=ISIZE(NVARW)
        DO I=0,ISS-1
          RVAL(I+NRVALW)=NINT(RVAL(I+IPS))
        END DO
        IVALP(NVARW)=NRVALW
        ITYPE(NVARW)=1
        NRVALW=NRVALW+ISS
!===================
!  op #       49
!  operator ary2
!===================
      CASE(49)
        IDS=NINT(RVAL(IVALP(NVARW-1)))
        IDE=NINT(RVAL(IVALP(NVARW)))
        IF (IDE-IDS.LT.0) THEN
          WRITE(6,*)'Error! IE >= IS must be satisfied for an' &
          // ' expression with a structure [IS:IE].'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        DO I=0,IDE-IDS
          RVAL(I+NRVALW)=I+IDS
        END DO
        NVARW=NVARW-1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=IDE-IDS+1
        ITYPE(NVARW)=1
        NRVALW=NRVALW+IDE-IDS+1
!===================
!  op#        50
!  operator ary3
!===================
      CASE(50)
        IDS=NINT(RVAL(IVALP(NVARW-2)))
        IDE=NINT(RVAL(IVALP(NVARW-1)))
        IDD=NINT(RVAL(IVALP(NVARW)))
        IF (IDD.EQ.0 .OR. (IDE-IDS)*IDD.LT.0 ) THEN
          WRITE(6,*)'Error! ID >0 and (IE-IS)*ID>=0 are required for' &
          // ' an expresion with a structure [IS:IE:ID].'
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
        N=(IDE-IDS)/IDD+1
        DO I=0,N-1
          RVAL(I+NRVALW)=I*IDD+IDS
        END DO
        NVARW=NVARW-2
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=N
        ITYPE(NVARW)=1
        NRVALW=NRVALW+N
!===================
!  op #       51
!  operator bre   (branch ever)
!===================
      CASE(51)
        ICDP=ICDP+IOPRND-2
!===================
!  op #       52
!  operator bze   (branch  IF the value of stack top is zero)
!===================
      CASE(52)
        IT=ITYPE(NVARW)
        IS=ISIZE(NVARW)
        IF (IT.EQ.4.AND.IS.EQ.1) THEN
          IF (NINT(RVAL(IVALP(NVARW))).EQ.0) ICDP=ICDP+IOPRND-2
          IF (IVALP(NVARW).GE.NRVAL) NRVALW=IVALP(NVARW)
          NVARW=NVARW-1
        ELSE
          IF(IT.NE.4) THEN
            WRITE(6,*)'Type mismatch for the "?" or "??" operator'
            WRITE(6,*)'The type of the operand must be logical.'
          ELSE IF(IS.NE.1) THEN
            WRITE(6,*) &
          'Incorrect size of the operand for the "?" or "??" operator.'
            WRITE(6,*)'The size must be one.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       53
!  operator bnz   (branch IF the value of stack top is not zero)
!===================
      CASE(53)
        IT=ITYPE(NVARW)
        IS=ISIZE(NVARW)
        IF (IT.EQ.4.AND.IS.EQ.1) THEN
          IF (NINT(RVAL(IVALP(NVARW))).NE.0) ICDP=ICDP+IOPRND-2
          IF (IVALP(NVARW).GE.NRVAL) NRVALW=IVALP(NVARW)
          NVARW=NVARW-1
        ELSE
          IF(IT.NE.4) THEN
            WRITE(6,*)'Type mismatch for the "?" or "??" operator'
            WRITE(6,*)'The type of the operand must be logical.'
          ELSE IF(IS.NE.1) THEN
            WRITE(6,*) &
       'Incorrect size of the operand for the "?" or "??" operator.'
            WRITE(6,*)'The size must be one.'
          END IF
          CALL X_TRACE(ICDP,ICD,ISTP,NCS,NFNC,IFNCP,SFNC,ITRACE,IBREAK)
          IERR=1
          RETURN
        END IF
!===================
!  op #       54
!  operator cyc   (cyclic extension)
!===================
      CASE(54)
        N=NINT(RVAL(IVALP(NVARW)))
        ISS=ISIZE(NVARW-1)
        IPS=IVALP(NVARW-1)
        IPD=IPS
        IF (IPS.LT.NRVAL) IPD=NRVALW
        IVALP(NVARW-1)=IPD
        DO J=1,N
          DO I=0,ISS-1
            RVAL(IPD+I)=RVAL(IPS+I)
          END DO
          IPD=IPD+ISS
        END DO
        NRVALW=IPD
        NVARW=NVARW-1
        ISIZE(NVARW)=ISS*N
!===================
!  op #       60
!  operator lin   (no operation but for showing line number)
!===================
      CASE(60)
      END SELECT
 
      IF (NRVALW.GT.NRVALX) THEN
        WRITE(6,*)'out of memory'
        IERR=1
        IF (IBREAK.NE.0) STOP
        RETURN
      END IF
      ICDP=ICDP+2
      IF (ICDP.GT.ICDE) EXIT
      END DO !main loop
      RETURN
 
      END SUBROUTINE X_EXEC
 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     Resize a variable
!
!    IPD:   The pointer to the 1st element of the variable
!    ISD:   The size of the variable
!    ISRSZ: New size of the variable
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE RSZVAR(ISRSZ,ISD,IPD &
                       ,NVAR,NVARW,ISIZE,IVALP &
                       ,NRVAL,NRVALW,RVAL &
                       ,ISTP,NVL,NVLW)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NCNSM=2000
 
! Dummy argumets
      INTEGER :: ISRSZ, ISD, IPD
      INTEGER :: NVAR, NVARW
      INTEGER :: ISIZE(NVARM+NCNSM), IVALP(NVARM+NCNSM)
      INTEGER :: NRVAL, NRVALW
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: ISTP
      INTEGER :: NVL(*), NVLW(*)
 
! Local variable
      INTEGER :: I, K, IERR, IPD2, ISD2, IVLP, ISZ, IVLPNXT
!
! Since the varialbe can be a partial variable of some other
! variable, it is needed to find the original variable.
!
      DO I=1,NVAR
        IF (IVALP(I).LE.IPD.AND.IVALP(I)+MAX(ISIZE(I),1) &
         .GE.IPD+MAX(ISD,1)) GOTO 310
      END DO
      WRITE(6,*)'Error! Variable area is broken.'
      IERR=1
      RETURN
 310  IPD2=IVALP(I)
      ISD2=ISIZE(I)
      IVLPNXT=NRVAL
      DO I=1,NVAR
        IF (IVALP(I).LT.IVLPNXT.AND.IVALP(I).GE.IPD2+MAX(1,ISD2)) THEN
          IVLPNXT=IVALP(I)
        END IF
      END DO
!      WRITE(6,*)'IPD2=',IPD2,' ISD2=',ISD2
!
! When there is no room to increase the size of the variable,
! data for variables positioned above must shifted higher
! addresses
!
      K=ISRSZ-ISD - ( IVLPNXT -(IPD2+ISD2) )
      IF (K.GT.0) THEN
        DO I=1,NVARW
          IF (IVALP(I).GE.IVLPNXT) IVALP(I)=IVALP(I)+K
        END DO
        DO I=1,ISTP
          IF (NVL (I).GE.IVLPNXT) THEN
            NVL (I)=NVL (I)+K
          ELSE IF (NVL (I).GE.IPD+MAX(ISD,1)) THEN
            NVL (I)=NVL(I)+ISRSZ-MAX(ISD,1)
          END IF
          IF (NVLW(I).GE.IVLPNXT) THEN
            NVLW(I)=NVLW(I)+K
          ELSE IF (NVLW(I).GE.IPD+MAX(ISD,1)) THEN
            NVLW(I)=NVLW(I)+ISRSZ-MAX(ISD,1)
          END IF
        END DO
        DO I=NRVALW-1,IPD2+MAX(ISD2,1),-1
          RVAL(I+K)=RVAL(I)
        END DO
        NRVALW=NRVALW+K
        IF (NRVAL.GE.IVLPNXT) THEN
          NRVAL =NRVAL+K
        ELSE IF (NRVAL.GE.IPD+MAX(ISD,1)) THEN
          NRVAL =NRVAL+ISRSZ-MAX(ISD,1)
        END IF
      END IF
!
      IF (ISRSZ.GT.ISD.AND.ISD2.NE.0) THEN
        DO I=IPD2+ISD2-1,IPD+ISD,-1
          RVAL(I+ISRSZ-ISD)=RVAL(I)
        END DO
      ELSE IF (ISRSZ.LT.ISD) THEN
        DO I=IPD+ISD,IPD2+ISD2-1
          RVAL(I+ISRSZ-ISD)=RVAL(I)
        END DO
      END IF
!
! Since the variable of the interest can be a partial variable of
! some other variables and also can be reffered as the arguments or
! gobal variables in functions, care must be needed for all variables,
! which have accesses to the same memory area, to keep consistancy.
!
      IF (ISRSZ.LT.ISD) THEN
        DO I=1,NVAR
          IVLP=IVALP(I)
          ISZ =ISIZE(I)
          IF (IVLP.GE.IPD2.AND.IVLP+MAX(ISZ,1).LE.IPD2+MAX(ISD2,1))THEN
            IF (IVLP.LE.IPD+ISRSZ) THEN
              IF (IVLP+ISZ.GE.IPD+ISD) THEN
                ISIZE(I)=ISZ+ISRSZ-ISD
!                WRITE(6,*)'pass 1'
              ELSE
                ISIZE(I)=IPD+ISZ-IVLP
!                WRITE(6,*)'pass 2'
              END IF
            ELSE
              IF (IVLP.GT.IPD+ISRSZ) THEN
                IVALP(I)=IPD+ISRSZ
                ISIZE(I)=IVLP+ISZ-(IPD+ISD)
!                WRITE(6,*)'pass 3'
              ELSE
                WRITE(6,*) &
                  'Error! Unable to change the size of a variable.'
                IERR=1
                RETURN
              END IF
            END IF
          END IF
        END DO
      ELSE IF(ISRSZ.GT.ISD) THEN
        DO I=1,NVAR
          IVLP=IVALP(I)
          ISZ =ISIZE(I)
          IF (IVLP.GE.IPD2.AND.IVLP+MAX(ISZ,1).LE.IPD2+MAX(ISD2,1))THEN
            IF(IVLP.LE.IPD+ISD.AND.IVLP+ISZ.GE.IPD+ISD) THEN
              ISIZE(I)=ISZ +ISRSZ-ISD
            ELSE IF(IVLP.GT.IPD+ISD) THEN
!              IVALP(I)=IVLP+ISRSZ-ISD
            END IF
          END IF
        END DO
      END IF
!      WRITE(6,*)'NRVAL =',NRVAL
!      WRITE(6,*)'  IPD =',IPD
!      WRITE(6,*)'  ISD =',ISD
!      WRITE(6,*)'  IPD2=',IPD2
!      WRITE(6,*)'  ISD2=',ISD2
!      WRITE(6,*)' ISRSZ=',ISRSZ
 
      END SUBROUTINE RSZVAR
 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! convert upcase character to downcase one
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      SUBROUTINE LWCASE(ASYM)
      IMPLICIT NONE
 
! Dummy argument
      CHARACTER(32)  :: ASYM
 
! Local variables
      INTEGER :: I,J
      CHARACTER(26)  :: LWRCASE
! ---------------------------
 
      LWRCASE='abcdefghijklmnopqrstuvwxyz'
      DO I=1,32
        J=INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',ASYM(I:I))
        IF (J.NE.0) THEN
          ASYM(I:I)=LWRCASE(J:J)
        END IF
      END DO
 
      END SUBROUTINE LWCASE
 
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!   get a word and delimiter from text  (for "Xcode" interpreter)
!
!               coded by Arata Tanaka 1993.08 (at Hiroshima Univ.)
!
!               last modification 2004.1.16
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  INPUT VALUES
!    IUNIT>0:  The unit number of a device for an input file
!    IUNIT=0:  Modification of the parameters or closing of
!              the input file.
!                ITPY=-1 -- set the value of OP2 via AWRD
!                ITPY=-2 -- set the value of CMT via AWRD
!                ITPY=-4 -- set the value of CRN via AWRD
!                 others -- close the file
!    IUNIT<0:  This retrieves the text buffer from the stack.
!              For IUNIT=-1, the text data at the stack top is
!              retrieved and for IUNIT=-n, that at the n-th stack is
!              recovered. If you change the value of IUNIT before
!              closing the file, the text buffer is saved to the stack.
!              When the end of the file is reached, the text data
!              at the stack top is recovered unless the stack is empty.
!              If the stack is empty, the value of IUNIT is change to
!              be zero.
!
!  OUTPUT VALUES
!     AWRD:   a word
!     NWRD:   size of the word
!     DLM :   delimiter
!             consisting from one character
!               + - ! # $ % & ( ) * , / : ; < = > ? @ [ ] ^ { | } ~
!             consisiting from two characters
!                (these are defined in variable "OP2")
!               += -= *= /= %= ?? || && == >= <= != ** ++ --
!             consisting from only space(s)
!
!     ITYP:   type of the word
!              1 -- array of character between "
!              2 -- numeral (integer)
!              3 -- numeral (floting point)
!              4 -- symbol started from alphabet (_ is also included)
!              5 -- others
!
!     LIN :   line number of text
!
!     IERR:   error value
!              0 -- normal end
!              3 -- end of file
!
!  PARAMETERS
!
!     CMT:  characters after this delimiter are comments (default //)
!
      SUBROUTINE GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::ITXMAX=128,MAXST=3
 
! Dummy arguments
      INTEGER ::            NWRD
      CHARACTER(ITXMAX)  :: AWRD
      CHARACTER(2)  :: DLM
      INTEGER :: LIN, IUNIT, IERR, ITYP
 
! Local variables
      INTEGER :: IATMT(15,6)
      INTEGER :: ITXPST(MAXST),IUST(MAXST),LINST(MAXST)
      CHARACTER(ITXMAX)  :: TXTST(MAXST)
      CHARACTER(ITXMAX)  :: TXT
 
      INTEGER :: LIN0, IUNIT0
      CHARACTER(2)  :: DLMBF
      CHARACTER(128)  :: CHR, OP2
      CHARACTER(1)  :: AT
      CHARACTER(2)  :: CMT
      CHARACTER(1)  :: CRN, TAB, CR
      INTEGER :: IOPR,ISGN,INUM,IALF,IDQT,NCHR
      INTEGER :: ISTP,ICLS,ISTS,ITXP,NSTS15
      INTEGER :: I,K
 
! Function
      REAL*8 :: DNUM2
!
      SAVE TXT,ITXP,LIN0,IUNIT0,DLMBF,OP2,CMT,TAB
      SAVE CHR,IOPR,ISGN,INUM,IALF,IDQT,NCHR
      SAVE ISTP,TXTST,ITXPST,IUST,LINST
!
!       sp1 nm1 nm2 nm3 nm4 nm5 nm6 nme sym sp2 op1 op2 ca1 ca2 ca3
!         1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
      DATA IATMT &
        / 1, 16, 10, 10,  8,  8, 10, 17, 10, 10, 16, 16, 13, 16, 10 &
        ,11, 12, 11, 11,  8,  8, 11, 17, 11, 11, 12, 16, 13, 16, 11 &
        , 2, 12, 11, 11,  6,  6, 11, 11, 11, 11, 12, 16, 13, 16, 11 &
        , 3,  3,  3,  4,  7,  7,  7,  9,  9, 16, 16, 16, 13, 16, 16 &
        , 9, 16,  8,  8,  8,  8,  8, 17,  9, 16, 16, 16, 13, 16, 16 &
        ,13, 16, 16,  8,  8,  8,  8, 17, 16, 16, 16, 16, 15, 16, 15 /
!
      DATA  OP2/'+= -= *= /= %= ?? || && == >= <= != ** ++ --'/
      DATA  CMT/'//'/,CRN/' '/
      DATA  IUNIT0/0/,ISTP/0/,LIN0/0/
!
! change the parameter OP2, CMT or CRN or close the input file
!
      IF (IUNIT.EQ.0) THEN
        IF (ITYP.EQ.-1) THEN
          OP2=AWRD
        ELSE IF (ITYP.EQ.-2) THEN
          CMT=AWRD(1:2)
        ELSE IF (ITYP.EQ.-4) THEN
          CRN=AWRD(1:1)
        ELSE
          IF ( IUNIT0.NE.5 .AND. IUNIT0.NE.0 ) CLOSE(IUNIT0)
          IUNIT0=0
        END IF
        RETURN
      ELSE IF (IUNIT.LT.0) THEN
!
! pop the (-IUNIT)-th stack to the text buffer
!
        IF (ISTP.GT.-IUNIT) THEN
          IF (IUNIT0.NE.5) CLOSE(IUNIT0)
          TXT   = TXTST (ISTP+IUNIT+1)
          ITXP  = ITXPST(ISTP+IUNIT+1)
          IUNIT0= IUST  (ISTP+IUNIT+1)
          LIN0  = LINST (ISTP+IUNIT+1)
          DO 2 I=ISTP+IUNIT+2,ISTP,-1
            TXTST (I-1)=TXTST (I)
            ITXPST(I-1)=ITXPST(I)
            IUST  (I-1)=IUST  (I)
            LINST (I-1)=LINST (I)
 2        CONTINUE
          ISTP=ISTP-1
          IUNIT=IUNIT0
          LIN=LIN0
          RETURN
        ELSE
          WRITE(6,*)'<<GTWRD>> Error! stack underflow!!'
          WRITE(6,*)' The maxium stack level: MAXST=',MAXST
          STOP
        END IF
        RETURN
      ELSE IF (IUNIT.NE.IUNIT0.AND.IUNIT0.NE.0) THEN
        ISTP=ISTP+1
        IF (ISTP.GT.MAXST) THEN
          WRITE(6,*)'<<GTWRD>> Error! stack overflow!! '
          WRITE(6,*)' The maxium stack level: MAXST=',MAXST
          STOP
        END IF
        TXTST(ISTP) = TXT
        ITXPST(ISTP)= ITXP
        IUST(ISTP)  = IUNIT0
        LINST(ISTP) = LIN0
        LIN0=0
      END IF
!
! initialization
!
      IF (IUNIT.NE.IUNIT0) THEN
 
        TAB=CHAR(9)
        CR=CHAR(13)
 
        CHR=' '//TAB//CR//'!#$%&()*,/:;<=>?@[]^{|}~\\+-0123456789.' &
          //'_abcdefghijklmnopqrstuvwxyz' &
          //'ABCDEFGHIJKLMNOPQRSTUVWXYZ"'
        IOPR=INDEX(CHR,'!')
        ISGN=INDEX(CHR,'+')
        INUM=INDEX(CHR,'0')
        IALF=INDEX(CHR,'_')
        IDQT=INDEX(CHR,'"')
        NCHR=INDEX(CHR,'"')
        IUNIT0=IUNIT
!
        IERR=0
        AWRD=' '
        NWRD=0
        DLM='  '
!CCCCC
        DLMBF='; '
        ISTS=1
        ITYP=0
        GOTO 20
      END IF
!
! main loop
!
      IERR=0
      AWRD=' '
      NWRD=0
      DLM='  '
      ISTS=1
      ITYP=0
 
!
 5    DO 10 I=ITXP,ITXMAX
        AT=TXT(I:I)
        K=INDEX(CHR,AT)
        IF (K.GE.1.AND.K.LT.IOPR) THEN
          ICLS=1
        ELSE IF (K.LT.ISGN) THEN
          ICLS=2
        ELSE IF (K.LT.INUM) THEN
          ICLS=3
        ELSE IF  (K.LT.IALF) THEN
          ICLS=4
        ELSE IF  (K.LT.IDQT) THEN
          ICLS=5
        ELSE IF  (K.LE.NCHR) THEN
          ICLS=6
        ELSE
          GOTO 20
        END IF
!
        IF (AT.EQ.'.'.AND.(ISTS.EQ.3.OR.ISTS.EQ.2)) THEN
          ISTS=4
        ELSE IF (AT.EQ.'.'.AND.ISTS.EQ.4) THEN
          ISTS=8
        ELSE IF ( INDEX('dDeE',AT).NE.0 &
                     .AND.ISTS.GE.3.AND.ISTS.LE.4)THEN
          ISTS=5
        ELSE
          ISTS=IATMT(ISTS,ICLS)
        END IF
!
        IF (ISTS.EQ.2) THEN
          IF (INDEX('  e) e] e} ',DLMBF).NE.0)THEN
!    operator  + or -
            DLM=AT//' '
            ISTS=11
          ELSE
!    sign  + or -
            DLM=AT//'@'
            ISTS=11
          END IF
        ELSE IF (ISTS.EQ.11) THEN
          DLM=AT//' '
          IF (DLM.EQ.CMT) THEN
            DLM='  '
            ISTS=1
            IF (NWRD.NE.0) ISTS=10
            GOTO 20
          END IF
        ELSE IF (ISTS.EQ.12) THEN
          IF (DLM(1:1)//AT.EQ.CMT) THEN
            DLM='  '
            ISTS=1
            IF (NWRD.NE.0) ISTS=10
            GOTO 20
          END IF
          IF (INDEX(OP2,DLM(1:1)//AT).NE.0) THEN
            DLM(2:2)=AT
            ITXP=I+1
          ELSE
            ITXP=I
          END IF
          DLMBF=DLM
          LIN=LIN0
          RETURN
        ELSE IF (ISTS.EQ.8) THEN
          ITXP=I-NWRD
          NWRD=0
          AWRD=' '
          ITYP=5
          GOTO 5
        ELSE IF (ISTS.GE.3.AND.ISTS.LE.9) THEN
          IF (DLM(2:2).EQ.'@') THEN
            AWRD=DLM(1:1)
            NWRD=1
            DLM='  '
          END IF
          IF (ISTS.EQ.3) THEN
            ITYP=2
          ELSE IF (ISTS.LE.7) THEN
            ITYP=3
          ELSE IF (ISTS.EQ.9) THEN
            ITYP=MAX(ITYP,4)
          END IF
          IF (ICLS.EQ.5.AND.K.GE.IALF+27) K=K-26
          NWRD=NWRD+1
          AWRD(NWRD:NWRD)=CHR(K:K)
        ELSE IF (ISTS.EQ.13) THEN
          ITYP=1
          NSTS15=0
          IF (AT.NE.'"') THEN
            NWRD=NWRD+1
            AWRD(NWRD:NWRD)=AT
          END IF
        ELSE IF (ISTS.EQ.14) THEN
          IF (NWRD.EQ.0) AWRD(NWRD+1:NWRD+1)=AT
          ISTS=13
        ELSE IF (ISTS.EQ.15) THEN
          NSTS15=NSTS15+1
          IF (NSTS15.EQ.2) THEN
            NWRD=NWRD+1
            AWRD(NWRD:NWRD)='"'
            ISTS=13
          END IF
        ELSE IF (ISTS.EQ.16) THEN
          ITXP=I
          DLMBF=DLM
          LIN=LIN0
          RETURN
        ELSE IF (ISTS.EQ.17) THEN
          WRITE(6,*)'Error! wrong syntax: '//AWRD(:NWRD)
          IERR=1
          RETURN
        END IF
 10   CONTINUE
!
!  read a new line from the input file
!
 20   ITXP=1
      LIN0=LIN0+1
      READ(IUNIT,'(A)',END=100)TXT
      TXT(ITXMAX-1:ITXMAX-1)=CRN
      GOTO 5
!
!  EOF (the end of file is reached)
!
 100  CONTINUE
      IERR=3
! close currently opened input file
      IF (IUNIT0.NE.5) CLOSE(IUNIT0)
! retrieve the text data from the stack to the text buffer
      IF (ISTP.GT.0) THEN
        TXT   = TXTST(ISTP)
        ITXP  = ITXPST(ISTP)
        IUNIT = IUST(ISTP)
        LIN0  = LINST(ISTP)
        ISTP=ISTP-1
        IUNIT0= IUNIT
      ELSE
! if the stack is empty IUNIT is set to be zero.
        IUNIT = 0
        IUNIT0= 0
      END IF
      LIN=LIN0
      RETURN
!
      END
! ==================================================================
! character to numeral
! ==================================================================
      FUNCTION DNUM2(NWRD,AWRD,IT)
      IMPLICIT NONE
 
      REAL*8 :: DNUM2
 
! Parameter
      INTEGER, PARAMETER ::NWRDM=32
 
! Dummy arguments
      INTEGER :: NWRD
      CHARACTER(NWRDM)  :: AWRD
      INTEGER :: IT
 
! Local variables
      CHARACTER(NWRDM)  :: CHR
 
      INTEGER :: I, K, ISTS, ICLS
      REAL*8 :: SF, SE, FL, EX, FP
 
      INTEGER :: IATMT(4,0:7)
      SAVE IATMT
 
      DATA  IATMT/1,3,8,2 &
                 ,8,3,8,2 &
                 ,8,3,5,2 &
                 ,8,8,5,4 &
                 ,8,8,5,4 &
                 ,6,8,8,7 &
                 ,8,8,8,7 &
                 ,8,8,8,7/
      CHR=' +-.edED0123456789'
      ISTS=0
      SF=1
      SE=1
      FL=0D0
      EX=0
      FP=0
      IT=1
      DO 10 I=1,NWRD
        K=INDEX(CHR,AWRD(I:I))
        IF(K.EQ.0.OR.K.GT.18) THEN
          GOTO 20
        ELSE IF (K.EQ.1) THEN
          GOTO 10
        ELSE IF (K.LE.3) THEN
          ICLS=1
        ELSE IF (K.LE.4) THEN
          ICLS=2
          IT=2
        ELSE IF (K.LE.8) THEN
          ICLS=3
          IT=2
          IF (K.EQ.6.OR.K.EQ.8) IT=3
        ELSE
          ICLS=4
        END IF
        ISTS=IATMT(ICLS,ISTS)
        IF (ISTS.EQ.1) THEN
          IF (K.EQ.3) SF=-1D0
        ELSE IF (ISTS.EQ.2) THEN
          FL=FL*10D0+DBLE(K-9)
        ELSE IF (ISTS.EQ.4) THEN
          FP=FP+1
          FL=FL*10D0+DBLE(K-9)
        ELSE IF (ISTS.EQ.6) THEN
          IF (K.EQ.3) SE=-1
        ELSE IF (ISTS.EQ.7) THEN
          EX=EX*10+DBLE(K-9)
        ELSE IF (ISTS.EQ.8) THEN
!          WRITE(6,*)'non-numeral datum'
          IT=-1
        END IF
 10   CONTINUE
 20   IF (ISTS.LE.1.OR.ISTS.EQ.5.OR.ISTS.EQ.6.OR.I.LE.NWRD) THEN
!        WRITE(6,*)'non-numeral datum'
        IT=-1
      END IF
      DNUM2=SF*FL*10D0**(SE*EX-FP)
      RETURN
      END
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     disassembler for internal code of "Xcode"
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE X_DASM(ICDS,ICDE,ICD)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::ICDM=40000
 
! Dummy arguments
      INTEGER :: ICDS, ICDE
      INTEGER :: ICD(ICDM)
 
! Local variables
      INTEGER :: I,J,K,N,IBUF,IPD,IPS,ISS,IT,IS,IS1,IS2,IPS1,IPS2
      CHARACTER(5)  :: AOP
      CHARACTER(300)  :: TOP
!               1    2    3    4    5    6    7    8    9    0
!               12345123451234512345123451234512345123451234512345
           TOP='CALL SFN  PSH  MRG  DRP  POP  STA  STC  STD1 STD2 ' &
             //'STD3 LDA  LDC  LDD1 LDD2 LDD3 ALC  @@@  LOR  LAND ' &
             //'LLT  LGT  LEQ  LNE  LLE  LGE  ADD  SUB  MUL  DIV  ' &
             //'MOD  POW  INC  DEC  NOT  MNS  NOP  RTS  LDCI LDSZ ' &
             //'LNA  @@@  @@@  @@@  @@@  @@@  @@@  ARY1 ARY2 ARY3 ' &
             //'BRE  BZR  BNZ  CYC  @@@  @@@  @@@  @@@  @@@  LIN  '
!
 1000 FORMAT(A2,I5,5X,A5,I5)
      DO I=ICDS,ICDE,2
        K=ICD(I)*5-4
        IF (K.LT.0.OR.K.GT.300-4) THEN
          AOP(1:5)='***  '
        ELSE
          AOP(1:5)=TOP(K:K+4)
        END IF
        WRITE(6,1000)'> ',I,AOP,ICD(I+1)
      END DO
      RETURN
      END
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!     system functions
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
!    NFN -- The function number
!    NARG -- The number of the arguments
!
      SUBROUTINE X_SYFN(NFN,NARG &
                        ,IVALP,ITYPE,ISIZE,RVAL &
                        ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                        ,NVL,NVLW,ISTP &
                        ,IUNIT,ISYSIO,IERR)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::ITXMAX=128,NBUF=256
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NCNSM=2000
 
! Dummy argumets
      INTEGER :: NFN, NARG
      INTEGER :: IVALP(NVARM+NCNSM), ITYPE(NVARM+NCNSM), ISIZE(NVARM+NCNSM)
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: NVAR, NVARW
      INTEGER :: NRVAL, NRVALW, NRVALX
      INTEGER :: NVL(*),NVLW(*)
      INTEGER :: ISTP
      INTEGER :: IUNIT,ISYSIO,IERR
 
! Local variables
      CHARACTER(ITXMAX)  :: AT
      CHARACTER(32)  :: AFMT
      CHARACTER(1)  ::  AFORM
      CHARACTER(NBUF+1)  :: ABUF
      LOGICAL :: LRET, LFORM
      INTEGER :: I,J,K,M,N,IBUF,IFORM,IARG,IPF,ITF
      INTEGER :: IPS, IPS1, IPS2, ISS1, ISS2, IPD, ISD, ITD, ISS, ITS
      INTEGER :: IS, IS1, IS2, IT, IT1, IT2, IPA, ISA, ITA, IDS, IDE, IDD
      INTEGER :: NFORM, NFLD, NFLDMLT, NFMT, NVARB
      REAL*8 ::  T, S
 
! Function
      REAL*8 :: DNUM2
! -------------------
 
      IPS=IVALP(NVARW)
      IT =ITYPE(NVARW)
      IS =ISIZE(NVARW)
      IF (IPS.GE.NRVAL) THEN
        IPD=IPS
      ELSE
        IPD=NRVALW
      END IF
! system functions
!              1       2       3       4       5
!       +00    sqrt    exp     cos     sin     tan
!       +05    log     atan    sign    abs     min
!       +10    max     int     cosh    sinh    tanh
!       +15    sizeof  typeof  print   real    nint
!       +20    sum     clear   dble    val     str
!       +25    char    stop    log10   fac     cmb
!       +30    printf
!
!       GOTO (  1001,   1002,   1003,   1004,   1005
!     &      ,  1006,   1007,   1008,   1009,   1010
!     &      ,  1011,   1012,   1013,   1014,   1015
!     &      ,  1016,   1017,   1018,   1019,   1020
!     &      ,  1021,   1022,   1023,   1024,   1025
!     &      ,  1026,   1027,   1028,   1029,   1030
!     &      ,  1031 ),NFN
      SELECT CASE(NFN)
!-------------------------------
!  fn #1
!  function sqrt
!-------------------------------
      CASE(1)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function sqrt.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=SQRT(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #2
!  function exp
!-------------------------------
      CASE(2)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function exp.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=EXP(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #3
!  function cos
!-------------------------------
      CASE(3)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function cos.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=COS(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #4
!  function sin
!-------------------------------
      CASE(4)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function sin.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=SIN(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #5
!  function tan
!-------------------------------
      CASE(5)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function tan.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=TAN(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #6
!  function log
!-------------------------------
      CASE(6)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function log.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=LOG(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #7
!  function atan
!-------------------------------
      CASE(7)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function atan.'
          IERR=1
          RETURN
        END IF
        IF (NARG.EQ.1) THEN
          DO I=0,IS-1
            RVAL(IPD+I)=ATAN(RVAL(IPS+I))
          END DO
        ELSE IF (NARG.EQ.2) THEN
          N=NINT(RVAL(IPS))
          IPS2=IPS
          IS2 =IS
          IPS1=IVALP(NVARW-1)
          IS1 =ISIZE(NVARW-1)
          IF (IPS1.LT.IPD.AND.IPS1.GE.NRVAL) IPD=IPS1
          DO I=0,MAX(IS1,IS2)-1
            RVAL(IPD+I) &
             =ATAN2(RVAL(IPS1+MOD(I,IS1)),RVAL(IPS2+MOD(I,IS2)))
          END DO
          NVARW=NVARW-1
          IS=MAX(IS1,IS2)
        ELSE
          WRITE(6,*)'Error! The number of the arguments mismatch'
          WRITE(6,*)'for the function atan (Narg <=2). '
          IERR=1
          RETURN
        END IF
        IT=MAX(2,IT)
!-------------------------------
!  fn #8
!  function sign
!-------------------------------
      CASE(8)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function sign.'
          IERR=1
          RETURN
        END IF
        IF (NARG.EQ.1) THEN
          IF (IT.EQ.1) THEN
            DO I=0,IS-1
              J=NINT(RVAL(IPS+I))
              RVAL(IPD+I)=SIGN(1,J)
            END DO
          ELSE
            DO I=0,IS-1
              T=RVAL(IPS+I)
              RVAL(IPD+I)=SIGN(1D0,T)
            END DO
          END IF
        ELSE IF (NARG.EQ.2) THEN
          IPS2=IPS
          IS2 =IS
          IPS1=IVALP(NVARW-1)
          IS1 =ISIZE(NVARW-1)
          IF (IPS1.LT.IPD.AND.IPS1.GE.NRVAL) IPD=IPS1
          IF (IT.EQ.1) THEN
            DO I=0,MAX(IS1,IS2)-1
              RVAL(IPD+I)=SIGN( NINT(RVAL(IPS1+MOD(I,IS1))) &
                              , NINT(RVAL(IPS2+MOD(I,IS2))) )
            END DO
          ELSE
            DO I=0,MAX(IS1,IS2)-1
              RVAL(IPD+I)=SIGN( RVAL(IPS1+MOD(I,IS1) ) &
                              , RVAL(IPS2+MOD(I,IS2)) )
            END DO
          END IF
          NVARW=NVARW-1
          IS=MAX(IS,IS2)
        ELSE
          WRITE(6,*)'Error! The number of the arguments mismatch'
          WRITE(6,*)'for the function sign.'
          IERR=1
          RETURN
        END IF
!-------------------------------
!  fn #9
!  function abs
!-------------------------------
      CASE(9)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function abs.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=ABS(RVAL(IPS+I))
        END DO
!-------------------------------
!  fn #10
!  function min
!-------------------------------
      CASE(10)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function min.'
          IERR=1
          RETURN
        END IF
        T=RVAL(IPS)
        DO J=1,NARG
          IPS=IVALP(NVARW+1-J)
          IS =ISIZE(NVARW+1-J)
          DO I=0,IS-1
            IF (T.GT.RVAL(IPS+I)) T=RVAL(IPS+I)
          END DO
          IF (IPD.GT.IPS.AND.IPS.GE.NRVAL) IPD=IPS
        END DO
        NVARW=NVARW-NARG+1
        RVAL(IPD)=T
        IS=1
!-------------------------------
!  fn #11
!  function max
!-------------------------------
      CASE(11)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function max.'
          IERR=1
          RETURN
        END IF
        T=RVAL(IPS)
        DO J=1,NARG
          IPS=IVALP(NVARW+1-J)
          IS =ISIZE(NVARW+1-J)
          DO I=0,IS-1
            IF (T.LT.RVAL(IPS+I)) T=RVAL(IPS+I)
          END DO
          IF (IPD.GT.IPS.AND.IPS.GE.NRVAL) IPD=IPS
        END DO
        NVARW=NVARW-NARG+1
        RVAL(IPD)=T
        IS=1
!-------------------------------
!  fn #12
!  function int
!-------------------------------
      CASE(12)
        IF(IS.EQ.0) THEN
           WRITE(6,*) &
         'Error! The size of argument is zero for the function int.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=INT(RVAL(IPS+I))
        END DO
        IT=1
!-------------------------------
!  fn #13
!  function cosh
!-------------------------------
      CASE(13)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function cosh.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=COSH(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #14
!  function sinh
!-------------------------------
      CASE(14)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function sinh.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=SINH(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #15
!  function tanh
!-------------------------------
      CASE(15)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function tanh.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=TANH(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #16
!  function sizeof
!-------------------------------
      CASE(16)
        RVAL(IPD)=IS
        IS=1
        IT=1
!-------------------------------
!  fn #17
!  function typeof
!-------------------------------
      CASE(17)
        RVAL(IPD)=IT
        IS=1
        IT=1
!-------------------------------
!  fn #18
!  function print
!-------------------------------
      CASE(18)
        NVARB=NVARW-NARG
        N=0
        K=0
        DO IARG=1,NARG
          IF (ITYPE(NVARB+IARG).NE.0) THEN
            ISS=ISIZE(NVARB+IARG)
            IF(ISS.EQ.0) THEN
              WRITE(6,*)'Warning in the function print!'
              WRITE(6,*) &
             'The size of argument is zero: argument #',IARG
              K=1
            END IF
            N=MAX(N,ISS)
          ELSE
            N=MAX(N,1)
          END IF
          IPS=IVALP(NVARB+IARG)
          IF (IPS.LT.IPD.AND.IPS.GE.NRVAL) IPD=IPS
        END DO
        IF (K.EQ.1) GOTO 6018
!
        DO I=0,N-1
          IBUF=0
          DO IARG=1,NARG
            IT =ITYPE(NVARB+IARG)
            ISS=ISIZE(NVARB+IARG)
            IPS=IVALP(NVARB+IARG)
            IF (IT.EQ.0) THEN
              DO K=1,ISS
                IF (IBUF.EQ.NBUF) THEN
                  WRITE(6,'(A,$)')ABUF(1:NBUF)
                  IBUF=0
                END IF
                IBUF=IBUF+1
                ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPS+K-1)))
              END DO
              ISS=0
            ELSE IF (IT.EQ.1) THEN
              WRITE(AT,'(I12)')NINT(RVAL(IPS+MOD(I,ISS)))
              ISS=12
            ELSE IF (IT.EQ.2) THEN
              WRITE(AT,'(G15.7)')RVAL(IPS+MOD(I,ISS))
              ISS=17
            ELSE IF (IT.EQ.3) THEN
              WRITE(AT,'(G24.16)')RVAL(IPS+MOD(I,ISS))
              ISS=26
            ELSE IF (IT.EQ.4) THEN
              WRITE(AT,'(I2)')NINT(RVAL(IPS+MOD(I,ISS)))
              ISS=2
            END IF
            IF (IBUF+ISS.LE.NBUF) THEN
              IF (ISS.GT.0) ABUF(IBUF+1:IBUF+ISS)=AT(1:ISS)
              IBUF=IBUF+ISS
            ELSE
              IF (IBUF.LT.NBUF) ABUF(IBUF+1:NBUF)=AT(1:NBUF-IBUF)
              WRITE(6,'(A,$)')ABUF(1:NBUF)
              IF (ISS.GE.NBUF-IBUF+1) &
                   ABUF(1:ISS+IBUF-NBUF)=AT(NBUF-IBUF+1:ISS)
              IBUF=ISS+IBUF-NBUF
            END IF
          END DO
          IF (IBUF.NE.0) THEN
            WRITE(6,'(A)')ABUF(1:IBUF)
          ELSE
            WRITE(6,*)''
          END IF
        END DO
!
 6018   IT =ITYPE(NVARB+1)
        IS =ISIZE(NVARB+1)
        IPS=IVALP(NVARB+1)
        DO I=0,IS-1
          RVAL(IPD+I)=RVAL(IPS+I)
        END DO
        NVARW=NVARB+1
!-------------------------------
!  fn #19
!  function real
!-------------------------------
      CASE(19)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function real.'
          IERR=1
          RETURN
        END IF
        IT=2
        DO I=0,IS-1
          RVAL(IPD+I)=RVAL(IPS+I)
        END DO
!-------------------------------
!  fn #20
!  function nint
!-------------------------------
      CASE(20)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function nint.'
          IERR=1
          RETURN
        END IF
        IT=1
        DO I=0,IS-1
          RVAL(IPD+I)=NINT(RVAL(IPS+I))
        END DO
!-------------------------------
!  fn #21
!  function sum
!-------------------------------
      CASE(21)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function sum.'
          IERR=1
          RETURN
        END IF
        IF (NARG.EQ.1) THEN
          S=0D0
          DO I=IPS,IPS+IS-1
            S=S+RVAL(I)
          END DO
          RVAL(IPD)=S
          IS=1
        ELSE IF (NARG.EQ.2) THEN
          N=NINT(RVAL(IPS))
          IPS=IVALP(NVARW-1)
          IS=ISIZE(NVARW-1)
          IF (IPS.LT.IPD.AND.IPS.GT.NRVAL) IPD=IPS
          DO I=IPS,IPS+IS-1,N
            S=0D0
            DO J=I,I+N-1
              S=S+RVAL(J)
            END DO
            RVAL(IPD+(I-IPS+1)/N)=S
          END DO
          NVARW=NVARW-1
          IS=IS/N
        ELSE
          WRITE(6,*)'Error! The number of arguments for '
          WRITE(6,*)'the function sum is not correct (Narg<=2).'
          IERR=1
        END IF
!-------------------------------
!  fn #22
!  function clear
!-------------------------------
      CASE(22)
        IF (IS.GT.1) THEN
          K=1-IS
          DO I=1,NVARW
            J=IVALP(I)
            IF (J.GT.IPS) IVALP(I)=J+K
            IF (IPS.GE.J.AND.IPS.LT.J+ISIZE(I) &
             .AND.I.NE.NVARW) THEN
              ISIZE(I)=ISIZE(I)+K
            END IF
          END DO
          DO I=1,ISTP
            IF (NVL(I).GT.IPS) NVL(I)=NVL(I)+K
            IF (NVLW(I).GT.IPS) NVLW(I)=NVLW(I)+K
          END DO
          DO I=IPS+IS,NRVALW-1
            RVAL(I+K)=RVAL(I)
          END DO
          NRVALW=NRVALW+K
          NRVAL=NRVAL+K
        END IF
        RVAL(IPS)=0D0
        ITYPE(IPS)=-1
!-------------------------------
!  fn #23
!  function dble
!-------------------------------
      CASE(23)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function dble.'
          IERR=1
          RETURN
        END IF
        IT=3
        DO I=0,IS-1
          RVAL(IPD+I)=RVAL(IPS+I)
        END DO
!-------------------------------
!  fn #24
!  function val
!-------------------------------
      CASE(24)
        IF (IT.EQ.0) THEN
          DO I=0,MIN(IS,32)-1
            AT(I+1:I+1)=CHAR(NINT(RVAL(IPS+I)))
          END DO
          T=DNUM2(IS,AT(1:32),IT)
          IF (IT.LT.0) THEN
            RVAL(IPD)=0D0
            IT=2
          ELSE
            RVAL(IPD)=T
          END IF
          IS=1
        ELSE
          WRITE(6,*)'The type of argument must be character ' &
                  //'for the function val.'
          IERR=1
        END IF
!-------------------------------
!  fn #25
!  function str
!-------------------------------
      CASE(25)
        IF (IT.EQ.0) THEN
          WRITE(6,*)'The type of argument must not be character' &
                  //' for the function str.'
          IERR=1
          RETURN
        END IF
        IF (NARG.EQ.1) THEN
          ISD=0
          DO I=1,IS
            IF (IT.EQ.1) THEN
              WRITE(AT,'(I12)')NINT(RVAL(IPS+I-1))
              NFLD=12
            ELSE IF (IT.EQ.2) THEN
              WRITE(AT,'(G15.7)')REAL(RVAL(IPS+I-1))
              NFLD=15
            ELSE IF (IT.EQ.3) THEN
               WRITE(AT,'(G24.16)')RVAL(IPS+I-1)
               NFLD=24
            ELSE IF (IT.EQ.4) THEN
              WRITE(AT,'(I2)')NINT(RVAL(IPS+I-1))
              NFLD=2
            END IF
!
            IF (ISD.NE.0) THEN
              ISD=ISD+1
              RVAL(NRVALW+ISD-1)=ICHAR(' ')
            END IF
!
            DO J=1,NFLD
              IF (AT(J:J).NE.' ') THEN
                ISD=ISD+1
                RVAL(NRVALW+ISD-1)=ICHAR(AT(J:J))
              END IF
            END DO
          END DO
          IS=ISD
        ELSE IF (NARG.EQ.2) THEN
          IPS2=IPS
          IS2=IS
          IPS1=IVALP(NVARW-1)
          IS1 =ISIZE(NVARW-1)
          IF (IPS1.LT.IPD.AND.IPS1.GT.NRVAL) IPD=IPS1
!
          DO I=1,IS1
            AT(I:I)=CHAR(NINT(RVAL(IPS1+I-1)))
          END DO
          IF (IS1.EQ.1) THEN
            IF (INDEX('lL',AT(1:1)).NE.0) THEN
              AT='I2'
            ELSE IF (INDEX('iI',AT(1:1)).NE.0) THEN
              AT='I12'
            ELSE IF (INDEX('rR',AT(1:1)).NE.0) THEN
              AT='G15.7'
            ELSE IF (INDEX('dD',AT(1:1)).NE.0) THEN
              AT='G24.16'
            ELSE
              WRITE(6,*)'A wrong format in the function str!'
              WRITE(6,*)'An unknown type is specified:'//AT(1:IS1)
              IERR=1
              RETURN
            END IF
          END IF
          AFMT(1:32)=AT(2:IS1)
          NFLD=NINT(DNUM2(32,AFMT,N))
          AFMT(1:32)='('//AT(1:IS1)//')'
!
          IS=0
          DO I=1,IS2
            IF (IT.EQ.1.OR.IT.EQ.4) THEN
              WRITE(AT,AFMT,ERR=8025)NINT(RVAL(IPS2+I-1))
            ELSE
              WRITE(AT,AFMT,ERR=8025)RVAL(IPS2+I-1)
            END IF
            DO J=1,NFLD
              IS=IS+1
              RVAL(NRVALW+IS-1)=ICHAR(AT(J:J))
            END DO
          END DO
          NVARW=NVARW-1
        END IF
        IF (IPD.LT.NRVALW) THEN
          DO I=1,IS
            RVAL(IPD+I-1)=RVAL(NRVALW+I-1)
          END DO
        END IF
        IT=0
        GOTO 1025
!
!  error trap for wrong format
 8025   WRITE(6,*) &
         'Error! A wrong format in the function str: '//AFMT(1:NFMT)
        IERR=1
        RETURN
 1025   CONTINUE
!-------------------------------
!  fn #26
!  function char
!-------------------------------
      CASE(26)
        IT=0
        DO I=0,IS-1
          RVAL(IPD+I)=NINT(RVAL(IPS+I))
        END DO
!-------------------------------
!  fn #27
!  function stop
!-------------------------------
      CASE(27)
        STOP
!-------------------------------
!  fn #28
!  function log10
!-------------------------------
      CASE(28)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function log10.'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          RVAL(IPD+I)=LOG10(RVAL(IPS+I))
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #29
!  function fac  (factorial)
!-------------------------------
      CASE(29)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function fac.'
          IERR=1
          RETURN
        END IF
        IF (NARG.NE.1) THEN
          WRITE(6,*)'number of argument is wrong for function fac(n)'
          IERR=1
          RETURN
        END IF
        DO I=0,IS-1
          N=NINT(RVAL(IPS+I))
          IF (N.LT.0) THEN
            WRITE(6,*) &
           'The argument should be positive integer for function fac'
            IERR=1
            RETURN
          END IF
          T=1D0
          DO J=1,N
            T=T*DBLE(J)
          END DO
          RVAL(IPD+I)=T
        END DO
        IT=MAX(2,IT)
!-------------------------------
!  fn #30
!  function cmb  (combination)
!-------------------------------
      CASE(30)
        IF(IS.EQ.0) THEN
          WRITE(6,*) &
         'Error! The size of argument is zero for the function cmb.'
          IERR=1
          RETURN
        END IF
        IF (NARG.NE.2) THEN
          WRITE(6,*)'number of argument is wrong for function cmb(n,m)'
          IERR=1
          RETURN
        END IF
        N=NINT(RVAL(IPS))
        IPS2=IPS
        IS2=IS
        IPS1=IVALP(NVARW-1)
        IS1=ISIZE(NVARW-1)
        IF (IPS1.LT.IPD.AND.IPS1.GT.NRVAL) IPD=IPS1
        DO J=0,MAX(IS1,IS2)-1
          N=NINT( RVAL( IPS1+MOD(J,IS1) ) )
          M=NINT( RVAL( IPS2+MOD(J,IS2) ) )
          T=1D0
          IF ((N.LT.M).OR.(M.LT.0)) THEN
            WRITE(6,*)'Condition n>=m and m>0 should be satisfied for'
            WRITE(6,*)'function cmb(n,m)'
            IERR=1
            RETURN
          ELSE
            K=MIN(N-M,M)
            DO I=1,K
              T=T*DBLE(N-K+I)/DBLE(I)
            END DO
          END IF
          RVAL(IPD+J)=T
        END DO
        NVARW=NVARW-1
        IS=MAX(IS1,IS2)
!-------------------------------
!  fn #31
!  function printf
!-------------------------------
      CASE(31)
        NVARB=NVARW-NARG
        N=0
        K=0
        DO IARG=2,NARG
          IF (ITYPE(NVARB+IARG).NE.0) THEN
            ISS=ISIZE(NVARB+IARG)
            IF(ISS.EQ.0) THEN
              WRITE(6,*)'Warning in the function printf!'
              WRITE(6,*) &
             'The size of argument is zero: argument #',IARG
              K=1
            END IF
            N=MAX(N,ISS)
          ELSE
            N=MAX(N,1)
          END IF
          IPS=IVALP(NVARB+IARG)
          IF (IPS.LT.IPD.AND.IPS.GE.NRVAL) IPD=IPS
        END DO
        IF (K.EQ.1) GOTO 7031
!
        ITF  =ITYPE(NVARB+1)
        NFORM=ISIZE(NVARB+1)
        IPF  =IVALP(NVARB+1)
!
        IF (ITF.NE.0) THEN
          WRITE(6,*) &
      'Error! type mismatch in the function printf at the argument #1.'
          GOTO 9031
        END IF
!
        IBUF=0
        IFORM=1
        LFORM=.FALSE.
        NFLDMLT=0
!
        DO I=0,N-1
          DO IARG=2,NARG
            IT =ITYPE(NVARB+IARG)
            ISS=ISIZE(NVARB+IARG)
            IPS=IVALP(NVARB+IARG)
!
            IF ( NFLDMLT.EQ.0 ) THEN
              IF ( .NOT. LFORM ) THEN
!
! transfer characters which does not enclosed by "#"
! to the text buffer as they are
 131            CONTINUE
                DO WHILE( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#' &
                        .AND. IFORM.LT.NFORM )
                  IF (IBUF.EQ.NBUF) THEN
                    WRITE(6,'(A,$)')ABUF(1:NBUF)
                    IBUF=0
                  END IF
                  IBUF=IBUF+1
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IFORM=IFORM+1
                END DO
!
                IF ( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#') THEN
                  WRITE(6,*)'A wrong format in the function printf!'
                  WRITE(6,*)'A delimiter "#" may be missing.'
                  GOTO 9031
                ELSE
                  IFORM=IFORM+1
                  AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IF (AFORM.EQ.'#') THEN
                    IF (IBUF.EQ.NBUF) THEN
                      WRITE(6,'(A,$)')ABUF(1:NBUF)
                      IBUF=0
                    END IF
                    IBUF=IBUF+1
                    ABUF(IBUF:IBUF)=AFORM
                    IFORM=IFORM+1
                    GOTO 131
                  END IF
                  LFORM=.TRUE.
                END IF
              END IF
!
! get the multiplier of a format
              LRET=.TRUE.
              NFLDMLT=1
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              IF (INDEX('0123456789',AFORM).NE.0) THEN
                NFLDMLT=0
                K=INDEX('0123456789',AFORM)
                DO WHILE (K.NE.0)
                  NFLDMLT=NFLDMLT*10+(K-1)
                  IFORM=IFORM+1
                  IF (IFORM.LE.NFORM) THEN
                    AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                    K=INDEX('0123456789',AFORM)
                  ELSE
                    WRITE(6,*)'A wrong format in the function printf!'
                    WRITE(6,*)'A wrongly placed multiplier is found.'
                    GOTO 9031
                  END IF
                END DO
              END IF
!
! cut out one format specifier
              NFMT=0
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              DO WHILE( INDEX('#/+,',AFORM).EQ.0 .AND. IFORM.LE.NFORM )
                NFMT=NFMT+1
                AFMT(NFMT:NFMT)=AFORM
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) THEN
                  AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                ELSE
                  WRITE(6,*)'A wrong format in the function printf!'
                  WRITE(6,*)'A delimiter "#" may be missing.'
                  GOTO 9031
                END IF
              END DO
!
! default formats for each type
              IF (NFMT.EQ.1) THEN
                IF (INDEX('aAcC',AFMT(1:1)).NE.0) THEN
                  AFMT='A'
                  NFMT=1
                ELSE IF (INDEX('lL',AFMT(1:1)).NE.0) THEN
                  AFMT='I2'
                  NFMT=2
                ELSE IF (INDEX('iI',AFMT(1:1)).NE.0) THEN
                  AFMT='I12'
                  NFMT=3
                ELSE IF (INDEX('rR',AFMT(1:1)).NE.0) THEN
                  AFMT='G15.7'
                  NFMT=5
                ELSE IF (INDEX('dD',AFMT(1:1)).NE.0) THEN
                  AFMT='G24.16'
                  NFMT=6
                ELSE
                  GOTO 8031
                END IF
              END IF
            END IF
!
! set the size of a field: NFLD
            IF (AFMT.EQ.'A') THEN
              NFLD=ISS
            ELSE
              AT=AFMT(2:NFMT)
              NFLD=INT(DNUM2(NFMT-1,AT(:32),K))
            END IF
!
! transform a value to the format specified by AFMT and
! write the field to the text buffer
            IF (IT.EQ.0) THEN
              DO K=1,NFLD
                IF (IBUF.EQ.NBUF) THEN
                  WRITE(6,'(A,$)')ABUF(1:NBUF)
                  IBUF=0
                END IF
                IBUF=IBUF+1
                IF (K.LE.ISS) THEN
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPS+K-1)))
                ELSE
                  ABUF(IBUF:IBUF)=' '
                END IF
              END DO
              NFLD=0
            ELSE IF (IT.EQ.1.OR.IT.EQ.4) THEN
              WRITE(AT,'('//AFMT(1:NFMT)//')',ERR=8031) &
                                 NINT(RVAL(IPS+MOD(I,ISS)))
            ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
              WRITE(AT,'('//AFMT(1:NFMT)//')',ERR=8031) &
                                  RVAL(IPS+MOD(I,ISS))
            END IF
!
            IF (IBUF+NFLD.LE.NBUF) THEN
              IF (NFLD.GT.0) ABUF(IBUF+1:IBUF+NFLD)=AT(1:NFLD)
              IBUF=IBUF+NFLD
            ELSE
              IF (IBUF.LT.NBUF) ABUF(IBUF+1:NBUF)=AT(1:NBUF-IBUF)
              WRITE(6,'(A,$)')ABUF(1:NBUF)
              IF (NFLD.GE.NBUF-IBUF+1) &
                 ABUF(1:NFLD+IBUF-NBUF)=AT(NBUF-IBUF+1:NFLD)
              IBUF=NFLD+IBUF-NBUF
            END IF
            NFLDMLT=NFLDMLT-1
!
! handling delimiters ("+", "/" and "#")
            IF (NFLDMLT.EQ.0) THEN
!
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              IF (AFORM.EQ.'+') THEN
                LRET=.FALSE.
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              ELSE IF (AFORM.EQ.'/') THEN
                IF (IBUF.NE.0) THEN
                  WRITE(6,'(A)')ABUF(1:IBUF)
                  IBUF=0
                ELSE
                  WRITE(6,*)''
                END IF
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              END IF
!
              IF (AFORM.EQ.'#') THEN
                IFORM=IFORM+1
!
! transfer characters which does not enclosed by "#"
! to the text buffer as they are
 231          CONTINUE
                DO WHILE( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#' &
                      .AND. IFORM .LE. NFORM )
                  IF (IBUF.EQ.NBUF) THEN
                    WRITE(6,'(A,$)')ABUF(1:NBUF)
                    IBUF=0
                  END IF
                  IBUF=IBUF+1
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IFORM=IFORM+1
                END DO
                IF ( CHAR(NINT(RVAL(IPF+IFORM-1))) .EQ. '#') THEN
                  IFORM=IFORM+1
                  AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IF (AFORM.EQ.'#') THEN
                    IF (IBUF.EQ.NBUF) THEN
                      WRITE(6,'(A,$)')ABUF(1:NBUF)
                      IBUF=0
                    END IF
                    IBUF=IBUF+1
                    ABUF(IBUF:IBUF)=AFORM
                    IFORM=IFORM+1
                    GOTO 231
                  END IF
                  LFORM=.TRUE.
                END IF
              END IF
!
              IF (IFORM.GT.NFORM) THEN
                IFORM=1
                LFORM=.FALSE.
                IF (IBUF.NE.0.AND.LRET) THEN
                  WRITE(6,'(A)')ABUF(1:IBUF)
                  IBUF=0
                ELSE IF (LRET) THEN
                  WRITE(6,*)''
                END IF
              END IF
!
            END IF
          END DO !IARG
        END DO !I
!
        IF (IBUF.NE.0) THEN
          IF (LRET) THEN
            WRITE(6,'(A)')ABUF(1:IBUF)
          ELSE
            WRITE(6,'(A,$)')ABUF(1:IBUF)
          END IF
        END IF
!
 7031   IT =ITYPE(NVARB+2)
        IS =ISIZE(NVARB+2)
        IPS=IVALP(NVARB+2)
        DO I=0,IS-1
          RVAL(IPD+I)=RVAL(IPS+I)
        END DO
        NVARW=NVARB+1
        RETURN
!
!  error trap for a wrong format
 8031   WRITE(6,*) &
         'Error! A wrong format in the function printf: '//AFMT(1:NFMT)
 9031   IERR=1
        IT =ITYPE(NVARB+2)
        IS =ISIZE(NVARB+2)
        IPS=IVALP(NVARB+2)
        DO I=0,IS-1
          RVAL(IPD+I)=RVAL(IPS+I)
        END DO
        NVARW=NVARB+1
        RETURN
      END SELECT
!
      NRVALW=IPD+IS
      IVALP(NVARW)=IPD
      ISIZE(NVARW)=IS
      ITYPE(NVARW)=IT
!
      END SUBROUTINE X_SYFN
 
!>>>START IOFNC
! ######################################################################
!    IO functions for "Xcode"
! ######################################################################
      SUBROUTINE X_IOFN(NFN,NARG &
                       ,IVALP,ITYPE,ISIZE,RVAL &
                       ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                       ,NVL,NVLW,ISTP &
                       ,IUNIT,ISYSIO,IERR)
      IMPLICIT NONE
 
! Parameters
      INTEGER, PARAMETER ::NVARM=1000,NRVALM=200000,NCNSM=2000
 
      INTEGER, PARAMETER ::NBUF=256,NAT=128,NSPC=128,ITXMAX=128
 
      INTEGER, PARAMETER ::IOPEN=1,IREAD=2,IWRITE=3,ICLOSE=0,IEOF=4
 
! Dummy argumets
      INTEGER :: NFN, NARG
      INTEGER :: IVALP(NVARM+NCNSM),ITYPE(NVARM+NCNSM),ISIZE(NVARM+NCNSM)
      REAL*8 ::  RVAL(NRVALM)
      INTEGER :: NVAR, NVARW
      INTEGER :: NRVAL, NRVALW, NRVALX
      INTEGER :: NVL(*),NVLW(*)
      INTEGER :: ISTP
      INTEGER :: IUNIT,ISYSIO,IERR
 
! Local variables
 
      CHARACTER(NAT)  :: AT
      CHARACTER(NBUF+1)  :: ABUF
      CHARACTER(32)  :: AFMT
      CHARACTER(1)  ::  AFORM
      CHARACTER(6)  ::  ASFT
      CHARACTER(ITXMAX)  :: AWRD
      CHARACTER(2)  ::  DLM
      CHARACTER(32)  :: UDLM
      CHARACTER(64)  :: FNAME
      CHARACTER(32)  :: FSTATE
!
      INTEGER :: IOU(100),ITXPBF(100),ISFTBF(100)
      LOGICAL :: LOPEN
      LOGICAL :: LRET, LFORM
 
      INTEGER :: I,J,K,M,N,IBUF,IFORM,IARG,IPF,ITF
      INTEGER :: IPS, IPS1, IPS2, ISS1, ISS2, IPD, ISD, ITD, ISS, ITS
      INTEGER :: IS, IS1, IS2, IT, IT1, IT2, IPA, ISA, ITA, IDS, IDE, IDD
      INTEGER :: IU, ITXP, ITXPB, ITXPBB
      INTEGER :: ICNT, IP, ISFT, ISFTB, NFNAME, NFSTATE, NUDLM
      INTEGER :: NFORM, NFLD, NFLDMLT, NFMT, NVARB
      REAL*8 ::  T, S
 
! Function
      REAL*8 :: DNUM2
 
      SAVE IOU,ITXPBF,ISFTBF,ABUF,NUDLM,UDLM
      DATA IOU/100*0/,ITXPBF/100*0/,ISFTBF/100*0/,NUDLM/0/
!
      IF (NUDLM.EQ.0) THEN
        UDLM=' '//CHAR(9)//CHAR(13)
        NUDLM=3
      END IF
      NVARB=NVARW-NARG
      DO IARG=1,NARG
        K=IVALP(NVARB+IARG)
        IF (K.GE.NRVAL.AND.NRVALW.GT.K) NRVALW=K
      END DO
      SELECT CASE(NFN)
!-------------------------------
!  iofn #1
!   function open
!-------------------------------
! unit number (argumet 1)
      CASE(1)
        IU=NINT(RVAL(IVALP(NVARB+1)))
        IF (ITYPE(NVARB+1).EQ.0) THEN
          WRITE(6,*) &
          'Type mismach for the argument #1 of the function open!'
          IERR=1
          RETURN
        ELSE IF (ISIZE(NVARB+1).EQ.0) THEN
          WRITE(6,*) &
      'Variable size is zero at the argument #1 of the function open!'
          NVARW=NVARB
          IERR=1
          RETURN
        ELSE IF(NARG.LE.1.OR.NARG.GT.3) THEN
          WRITE(6,*)'Number of arguments mismatch in the function open!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
!
        INQUIRE(IU,OPENED=LOPEN)
        IF (LOPEN) THEN
          WRITE(6,*)'File already open!'
          IF (IOU(IU).EQ.ICLOSE) THEN
            WRITE(6,*)'The unit number is used by system '
            WRITE(6,*)'          Uint#= ',IU
          END IF
          IERR=1
          RETURN
        ELSE
          IOU(IU)=IOPEN
        END IF
!
! file name (argument 2)
        IF (NARG.GE.2) THEN
          IPS   =IVALP(NVARB+2)
          NFNAME=ISIZE(NVARB+2)
          IF (ITYPE(NVARB+2).NE.0) THEN
            WRITE(6,*) &
            'Type mismach for the argument #2 of the function open!'
            NVARW=NVARB
            IERR=1
            RETURN
          ELSE IF (NFNAME.EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #2 of' &
                   // ' the function open!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          DO I=1,NFNAME
            FNAME(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
        END IF
!
! status of file (argument 3 )
        IF(NARG.GE.3) THEN
          IPS    =IVALP(NVARB+3)
          NFSTATE=ISIZE(NVARB+3)
          IF (ITYPE(NVARB+3).NE.0) THEN
          WRITE(6,*) &
          'Type mismach for the argument #3 of the function open!'
            NVARW=NVARB
            IERR=1
            RETURN
          ELSE IF (NFSTATE.EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #3 of' &
                   // ' the function open!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          FSTATE=' '
          DO I=1,NFSTATE
            FSTATE(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
          CALL LWCASE(FSTATE)
        ELSE
          FSTATE ='UNKNOWN'
          NFSTATE=7
        END IF
!
! file open
        OPEN(UNIT=IU,FILE=FNAME(1:NFNAME),STATUS=FSTATE(1:NFSTATE) &
            ,ERR=401)
        ITXPBF(IU)=0
        ISFTBF(IU)=0
        NVARW=NVARB
        RETURN
!
! error trap
 401    NVARW=NVARB
        CLOSE(IU)
        IF (FSTATE.EQ.'old') THEN
          WRITE(6,*) &
        ' The file requested by the function open does not exist.'
        ELSE IF (FSTATE.EQ.'new') THEN
          WRITE(6,*) &
        ' The file requested by the function open aleady exists.'
        ELSE
          WRITE(6,*)' IO error in the function open!'
        END IF
        WRITE(6,*)'            Unit#=',IU
        WRITE(6,*)'        File name=',FNAME(1:NFNAME)
        IERR=1
        RETURN
!-------------------------------
!  iofn #2
!   function close
!-------------------------------
      CASE(2)
        IU=NINT(RVAL(IVALP(NVARB+1)))
        IF (ISIZE(NVARB+1).EQ.0) THEN
          WRITE(6,*)'Variable size is zero at the argument #1' &
                 // ' of the function close!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IF (IOU(IU).NE.ICLOSE) THEN
          CLOSE(IU)
          IOU(IU)=ICLOSE
          ITXPBF(IU)=0
          ISFTBF(IU)=0
          NVARW=NVARB
        ELSE
          WRITE(6,*)'File already closed: Unit# =',IU
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        RETURN
!-------------------------------
!  iofn #3
!   function include
!-------------------------------
      CASE(3)
        IU=ISYSIO
        DO !IU
          INQUIRE(IU,OPENED=LOPEN)
          IF (.NOT.LOPEN) EXIT
          IU=IU+1
        END DO
!
        IF (NARG.EQ.1) THEN
          IPS   =IVALP(NVARB+1)
          NFNAME=ISIZE(NVARB+1)
          IF (ITYPE(NVARB+1).NE.0) THEN
            WRITE(6,*) &
            'Type mismach for the argument of the function include!'
            NVARW=NVARB
            IERR=1
            RETURN
          ELSE IF (NFNAME.EQ.0) THEN
            WRITE(6,*)'Variable size is zero in the argument of' &
                   // ' the function include!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          DO I=1,NFNAME
            FNAME(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
          OPEN(UNIT=IU,FILE=FNAME(1:NFNAME),STATUS='OLD',ERR=403)
          NVARW=NVARB
          IUNIT=IU
        ELSE
      WRITE(6,*)'Number of arguments mismatch in "include" (must be 1)'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        RETURN
! error trap
 403    NVARW=NVARB
        WRITE(6,*)' IO error in the function include'
        WRITE(6,*)'            Unit#=',IU
        WRITE(6,*)'        File name=',FNAME(1:NFNAME)
        IERR=1
        RETURN
!-------------------------------
!  iofn #4
!   function write
!  iofn #5
!   function writeln
!-------------------------------
      CASE(4:5)
        NFNAME=0
        IF (ITYPE(NVARB+1).EQ.0) THEN
! file open (case of quick write)
          IU=ISYSIO
          DO !IU
            INQUIRE(IU,OPENED=LOPEN)
            IF (.NOT.LOPEN) EXIT
            IU=IU+1
          END DO
          IPS   =IVALP(NVARB+1)
          NFNAME=ISIZE(NVARB+1)
          IF (NFNAME.EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1 of' &
                   // ' the function write or writeln!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          DO I=1,NFNAME
            FNAME(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
          OPEN(UNIT=IU,FILE=FNAME(1:NFNAME),STATUS='UNKNOWN',ERR=1504)
        ELSE
! io errors
          IU=NINT(RVAL(IVALP(NVARB+1)))
          IF (ISIZE(NVARB+1).EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1 of' &
                   // ' the function write or writeln'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IF (IOU(IU).EQ.IREAD) THEN
            WRITE(6,*)'Can not write after the function read!'
            WRITE(6,*)'    Unit#=',IU
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IF (IOU(IU).EQ.ICLOSE.AND.IU.NE.5.AND.IU.NE.6) THEN
            WRITE(6,*)'File not open!'
            WRITE(6,*)'    Unit#=',IU
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IOU(IU)=IWRITE
        END IF
! main loop
        N=0
        DO IARG=2,NARG
          IF (ITYPE(NVARB+IARG).NE.0.AND.NFN.EQ.4) THEN
            ISS=ISIZE(NVARB+IARG)
            IF(ISS.EQ.0) THEN
              WRITE(6,*)'Error in the function print!'
              WRITE(6,*) &
               'The size of argument is zero: argument #',IARG
              NVARW=NVARB
              IERR=1
              RETURN
            END IF
            N=MAX(N,ISS)
          ELSE
            N=MAX(N,1)
          END IF
        END DO
!
        DO I=0,N-1
          IBUF=0
          DO IARG=2,NARG
            IT =ITYPE(NVARB+IARG)
            ISS=ISIZE(NVARB+IARG)
            IPS=IVALP(NVARB+IARG)
            IF (IT.EQ.0) THEN
              DO K=1,ISS
                IF (IBUF.EQ.NBUF) THEN
                  WRITE(IU,'(A,$)')ABUF(1:NBUF)
                  IBUF=0
                END IF
                IBUF=IBUF+1
                ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPS+K-1)))
              END DO
              ISS=0
            ELSE IF (IT.EQ.1) THEN
              WRITE(AT,'(I12)')NINT(RVAL(IPS+MOD(I,ISS)))
              ISS=12
            ELSE IF (IT.EQ.2) THEN
              WRITE(AT,'(G15.7)')RVAL(IPS+MOD(I,ISS))
              ISS=17
            ELSE IF (IT.EQ.3) THEN
              WRITE(AT,'(G24.16)')RVAL(IPS+MOD(I,ISS))
              ISS=26
            ELSE IF (IT.EQ.4) THEN
              WRITE(AT,'(I2)')NINT(RVAL(IPS+MOD(I,ISS)))
              ISS=2
            END IF
            IF (IBUF+ISS.LE.NBUF) THEN
              IF (ISS.GT.0) ABUF(IBUF+1:IBUF+ISS)=AT(1:ISS)
              IBUF=IBUF+ISS
            ELSE
              IF (IBUF.LT.NBUF) ABUF(IBUF+1:NBUF)=AT(1:NBUF-IBUF)
              WRITE(IU,'(A,$)')ABUF(1:NBUF)
              IF (ISS.GE.NBUF-IBUF+1) &
                   ABUF(1:ISS+IBUF-NBUF)=AT(NBUF-IBUF+1:ISS)
              IBUF=ISS+IBUF-NBUF
            END IF
          END DO !IARG
          IF (IBUF.NE.0) THEN
            WRITE(IU,'(A)')ABUF(1:IBUF)
          ELSE
            WRITE(IU,*)''
          END IF
        END DO !I
        NVARW=NVARB
! close file (case of quick write)
        IF (NFNAME.NE.0) CLOSE(IU)
        RETURN
!  error trap
 1504   CLOSE(IU)
        WRITE(6,*)' IO error in the function write or writeln!'
        WRITE(6,*)'          Unit#=',IU
        WRITE(6,*)'     File name=',FNAME
        IOU(IU)=0
        IERR=1
        RETURN
!---------------------------------
!  iofn #6
!   function read
!  iofn #7
!   function readln
!  iofn #8
!   function input
!---------------------------------
      CASE(6:8)
        NFNAME=0
        IF (NFN.EQ.8) THEN
          IU=5
          DO I=NVARW,NVARB+1,-1
            IVALP(I+1)=IVALP(I)
            ISIZE(I+1)=ISIZE(I)
            ITYPE(I+1)=ITYPE(I)
          END DO
          NVARW=NVARW+1
          NARG=NARG+1
          IF (IOU(5).NE.0) THEN
            WRITE(6,*)'IO error in the function input'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
        ELSE IF (ITYPE(NVARB+1).EQ.0) THEN
!  quick read
!        write(6,*)'quick read'
          IU=ISYSIO
          DO !IU
            INQUIRE(IU,OPENED=LOPEN)
            IF (.NOT.LOPEN) EXIT
            IU=IU+1
          END DO
          IPS   =IVALP(NVARB+1)
          NFNAME=ISIZE(NVARB+1)
          IF (NFNAME.EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1 of' &
                   // ' the function read or readln!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          DO I=1,NFNAME
            FNAME(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
          OPEN(UNIT=IU,FILE=FNAME(1:NFNAME),STATUS='OLD',ERR=1506)
        ELSE
! io errors
          IU=NINT(RVAL(IVALP(NVARB+1)))
          IF (ISIZE(NVARB+1).EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1' &
                   // ' of the function read or readln!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IF (IOU(IU).EQ.IWRITE) THEN
            WRITE(6,*) &
            'Can not read after the function write or writeln!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IF (IOU(IU).EQ.IEOF) THEN
            WRITE(6,*)'End of file: Unit#=',IU
            DO IARG=2,NARG
              IF (IVALP(NVARB+IARG).LT.NRVAL) THEN
                ISD=ISIZE(NVARB+IARG)
                IPD=IVALP(NVARB+IARG)
                ISS=0
                CALL RSZVAR(ISS,ISD,IPD &
                        ,NVAR,NVARW,ISIZE,IVALP &
                        ,NRVAL,NRVALW,RVAL &
                        ,ISTP,NVL,NVLW)
              END IF
            END DO
            NVARW=NVARB+1
            IVALP(NVARB+1)=NRVALW
            ITYPE(NVARB+1)=4
            ISIZE(NVARB+1)=1
            RVAL(NRVALW)=1d0
            NRVALW=NRVALW+1
            IERR=0
            RETURN
          END IF
          IF (IOU(IU).EQ.ICLOSE.AND.IU.NE.5.AND.IU.NE.6) THEN
            WRITE(6,*)'File not open: Unit#=',IU
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          IOU(IU)=IREAD
        END IF
!  make working area
        N=NINT(DBLE(NRVALX-NRVALW)/DBLE(NARG))
        DO I=2,NARG
          IVALP(NVARW+I-1)=N*(I-1)+NRVALW
          ISIZE(NVARW+I-1)=0
        END DO
! init.
        ISS=0
        IARG=2
        ICNT=0
        IF (ITXPBF(IU).NE.0.AND.NFNAME.EQ.0) THEN
           ITXP=ITXPBF(IU)
          ISFT=ISFTBF(IU)
          BACKSPACE(IU)
        ELSE
          ITXP=1
          ISFT=0
        ENDIF
 
        AFMT(1:18)='(         A000000)'
        WRITE(ASFT,'(I6)')NBUF
        AFMT(12:17)=ASFT
 606    CONTINUE
! read a line from the file
          IF (ISFT.EQ.0) THEN
            AFMT(2:10)='         '
          ELSE
            WRITE(ASFT,'(I6)')ISFT
            AFMT(2:10)='TR'//ASFT(1:6)//','
          END IF
          READ(IU,AFMT(1:18),ERR=1506,END=3006)ABUF
          ABUF(NBUF+1:NBUF+1)='*'
 706      ITXPB=ITXP
! skip spaces
          DO WHILE(INDEX(UDLM,ABUF(ITXP:ITXP)).NE.0)
            ITXP=ITXP+1
          END DO
          ICNT=ICNT+ITXP-ITXPB
          IF (ICNT.GT.NSPC) THEN
            ITXP=1
            ISFT=0
            ICNT=0
            GOTO 606
          END IF
          IF (ITXP.GT.NBUF) THEN
            ISFT=ISFT+NBUF
            ITXP=1
            BACKSPACE(IU)
            GOTO 606
          END IF
! get a datum
          ISFTB=ISFT
          ITXPBB=ITXPB
          N=0
 906      ITXPB=ITXP
!          ITXP=INDEX(ABUF(ITXPB:),' ')+ITXPB-1
          DO ITXP=ITXPB,NBUF+1
            IF (INDEX(UDLM(1:NUDLM),ABUF(ITXP:ITXP)).NE.0) EXIT
          END DO
          IF (ITXP.EQ.NBUF+1) THEN
            IF (N+NBUF-ITXPB+1.LE.NAT) THEN
              AT(N+1:N+NBUF-ITXPB+1)=ABUF(ITXPB:NBUF)
            END IF
            N=N+NBUF-ITXPB+1
            ITXP=1
            ISFT=ISFT+NBUF
            BACKSPACE(IU)
            WRITE(ASFT,'(I6)')ISFT
            AFMT(2:10)='TR'//ASFT(1:6)//','
            READ(IU,AFMT(1:18),ERR=1506,END=3006)ABUF
            ABUF(NBUF+1:NBUF+1)='*'
            GOTO 906
          ELSE
            AT(N+1:MIN(N+ITXP-ITXPB,NAT))=ABUF(ITXPB:ITXP-1)
            N=N+ITXP-ITXPB
          END IF
          IF (N.GT.NAT) THEN
            WRITE(6,*)'Too long word; text buffer overflow!'
            WRITE(6,*)'The word is truncated: size=',NAT
            N=NAT
          END IF
! evaluate and save the datum to the working area
          IP=IVALP(NVARW+IARG-1)
          IT=ITYPE(NVARB+IARG)
          IF (IT.EQ.0) THEN
            DO I=0,N
              RVAL(IP+I)=ICHAR(AT(I+1:I+1))
            END DO
            ISIZE(NVARW+IARG-1)=N
          ELSE IF (IT.GE.1) THEN
            RVAL(IP+ISS)=DNUM2(N,AT(1:32),IT)
            IF (IT.LT.0) THEN
              ISIZE(NVARW+IARG-1)=ISS
              ISFT=ISFTB
              ITXP=ITXPBB
              GOTO 2006
            ELSE
              ISIZE(NVARW+IARG-1)=ISS+1
            END IF
          END IF
          IARG=IARG+1
          IF (IARG.GT.NARG) THEN
            IARG=2
            ISS=ISS+1
            IF (NFN.EQ.7.OR.NFN.EQ.8) GOTO 2006
          END IF
          ICNT=0
        GOTO 706
!
 3006   IOU(IU)=IEOF
 2006   IF (IOU(IU).NE.IEOF.AND.NFN.NE.8) THEN
          ITXPBF(IU)=ITXP
          ISFTBF(IU)=ISFT
        END IF
! move data from work area to variable area
 5006   DO IARG=2,NARG
          IF (IVALP(NVARB+IARG).LT.NRVAL) THEN
            ISD=ISIZE(NVARB+IARG)
            IPD=IVALP(NVARB+IARG)
            ISS=ISIZE(NVARW+IARG-1)
            CALL RSZVAR(ISS,ISD,IPD &
                       ,NVAR,NVARW,ISIZE,IVALP &
                       ,NRVAL,NRVALW,RVAL &
                       ,ISTP,NVL,NVLW)
            IPS=IVALP(NVARW+IARG-1)
            IF (ISS.GT.0) THEN
              DO I=0,ISS-1
                RVAL(IPD+I)=RVAL(IPS+I)
              END DO
            ELSE
              WRITE(6,*) &
       '  Warning! type mismatch in the function read: argument #',IARG
            END IF
          END IF
        END DO
        NVARW=NVARB+1
        IVALP(NVARB+1)=NRVALW
        ITYPE(NVARB+1)=4
        ISIZE(NVARB+1)=1
        RVAL(NRVALW)=0d0
        NRVALW=NRVALW+1
        IF (IU.EQ.5) IOU(IU)=ICLOSE
! close file (case of quick read)
        IF (NFNAME.NE.0) CLOSE(IU)
        RETURN
!
!  error trap
 1506   CLOSE(IU)
        WRITE(6,*)' IO error in the function read or readln'
        WRITE(6,*)'        Unit No.=',IU
        WRITE(6,*)'       File name=',FNAME(1:NFNAME)
        IOU(IU)=ICLOSE
        IERR=1
        RETURN
!-------------------------------
!   iofn #9
!   function eof
!-------------------------------
      CASE(9)
        IF (ISIZE(NVARB+1).EQ.0) THEN
          WRITE(6,*)'Variable size is zero in the argument of' &
                 // ' the function eof!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IU=NINT(RVAL(IVALP(NVARW)))
        IF (IOU(IU).EQ.IEOF) THEN
          RVAL(NRVALW)=1D0
        ELSE
          RVAL(NRVALW)=0D0
        END IF
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=1
        ITYPE(NVARW)=4
        NRVALW=NRVALW+1
!-------------------------------
!  iofn #10
!   function search
!-------------------------------
      CASE(10)
        IF (ISIZE(NVARB+1).EQ.0) THEN
          WRITE(6,*)'Variable size is zero at the argument #1 of' &
                 // ' the function search!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IU=NINT(RVAL(IVALP(NVARB+1)))
        IF (IOU(IU).EQ.IWRITE) THEN
          WRITE(6,*)'Can not read after the function write!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IF (IOU(IU).EQ.ICLOSE.AND.IU.NE.5.AND.IU.NE.6) THEN
          WRITE(6,*)'File not open: Unit#=',IU
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IF (IOU(IU).EQ.IEOF) THEN
          WRITE(6,*)'End of file: Unit#=',IU
          IERR=0
          RETURN
        END IF
        IF (IOU(IU).EQ.IEOF) GOTO 1509
        IPS=IVALP(NVARB+2)
        ISS=ISIZE(NVARB+2)
        DO I=0,ISS-1
          AT(I+1:I+1)=CHAR(NINT(RVAL(IPS+I)))
        END DO
        IF (ITXPBF(IU).NE.0.AND.IU.NE.ISYSIO+1) THEN
          ITXP=ITXPBF(IU)
          ISFT=ISFTBF(IU)
          BACKSPACE(IU)
        ELSE
          ITXP=1
          ISFT=0
        ENDIF
!
        AFMT(1:18)='(         A000000)'
        WRITE(ASFT,'(I6)')NBUF
        AFMT(12:17)=ASFT
 409    IF (ISFT.EQ.0) THEN
          AFMT(2:10)='         '
        ELSE
          WRITE(ASFT,'(I6)')ISFT
          AFMT(2:10)='TR'//ASFT(1:6)//','
        END IF
        READ(IU,AFMT(1:18),END=1509,ERR=1519)ABUF
        ABUF(NBUF+1:NBUF+1)='*'
        ITXPB=ITXP
        ITXP=INDEX(ABUF(ITXPB:),AT(1:ISS))+ITXPB-1
        IF (ITXP.LT.ITXPB) THEN
          I=NBUF
          DO WHILE (ABUF(I:I).EQ.' ')
            I=I-1
          END DO
          IF (NBUF-I.GT.NSPC) THEN
            ITXP=1
            ISFT=0
            GOTO 409
          ELSE
            ISFT=ISFT+NBUF
            ITXP=1
            BACKSPACE(IU)
            GOTO 409
          END IF
        END IF
        ITXP=ITXP+ISS
!
        ITXPBF(IU)=ITXP
        ISFTBF(IU)=ISFT
!
        IPD=NRVALW
        DO I=0,NBUF-1
          RVAL(IPD+I)=ICHAR(ABUF(I+1:I+1))
        END DO
        IS=NBUF
        NVARW=NVARB+1
        IVALP(NVARW)=IPD
        ISIZE(NVARW)=IS
        ITYPE(NVARW)=0
        NRVALW=NRVALW+IS
        RETURN
 1509   IS=1
        WRITE(6,*)'End of file: Unit#=',IU
        IOU(IU)=IEOF
        NVARW=NVARB+1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=IS
        ITYPE(NVARW)=0
        NRVALW=NRVALW+1
        RETURN
!
!  error trap
 1519   CLOSE(IU)
        WRITE(6,*)' IO error in the function search'
        WRITE(6,*)'        Unit No.=',IU
        WRITE(6,*)'       File name=',FNAME(1:NFNAME)
        IOU(IU)=ICLOSE
        IERR=1
        RETURN
!----------------------------------
!  iofn #11
!   function index
!----------------------------------
      CASE(11)
        IPS1=IVALP(NVARB+1)
        IPS2=IVALP(NVARB+2)
        ISS1=ISIZE(NVARB+1)
        ISS2=ISIZE(NVARB+2)
        IF (ISS1*ISS2.NE.0.AND.ISS1.GE.ISS2) THEN
          DO I=1,ISS1-ISS2+1
            DO J=1,ISS2
              IF (NINT(RVAL(IPS1+J+I-2)).NE.NINT(RVAL(IPS2+J-1))) &
                GOTO 211
            END DO
            GOTO 311
 211        CONTINUE
          END DO
          I=0
        ELSE
          I=0
        END IF
 311    NVARW=NVARW-1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=1
        ITYPE(NVARW)=1
        RVAL(NRVALW)=I
        NRVALW=NRVALW+1
        RETURN
!----------------------------------
!  iofn #12
!   function rewind
!----------------------------------
      CASE(12)
        IF (ISIZE(NVARW).EQ.0) THEN
          WRITE(6,*)'Variable size is zero in the argument of' &
                 // ' the function rewind!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
!
        IU=NINT(RVAL(IVALP(NVARW)))
        IF (IOU(IU).NE.0) THEN
          REWIND(IU)
          IOU(IU)=1
          ITXPBF(IU)=0
          ISFTBF(IU)=0
        ELSE
          WRITE(6,*)'File not open: Unit#=',IU
          IERR=1
          RETURN
        END IF
        RETURN
!----------------------------------
!  iofn #13
!   function nextln
!----------------------------------
      CASE(13)
        IF (ISIZE(NVARW).EQ.0) THEN
          WRITE(6,*)'Variable size is zero in the argument of' &
                 // ' the function nextln!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IU=NINT(RVAL(IVALP(NVARW)))
        IF (IOU(IU).EQ.IWRITE) THEN
          WRITE(6,*)'Can not read after the function write!'
          IERR=1
          RETURN
        END IF
        IF (IOU(IU).EQ.ICLOSE.AND.IU.NE.5.AND.IU.NE.6) THEN
          WRITE(6,*)'File not open: Unit#=',IU
          IERR=1
          RETURN
        END IF
        AFMT(1:18)='(         A000000)'
        WRITE(ASFT,'(I6)')NBUF
        AFMT(12:17)=ASFT
        READ(IU,AFMT(1:18),END=1013)ABUF
        DO I=NBUF,1,-1
          IF  (ABUF(I:I).NE.' ') EXIT
        END DO
        N=I
        NVARW=NVARB+1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=N
        ITYPE(NVARW)=0
        DO I=1,N
          RVAL(NRVALW+I-1)=ICHAR(ABUF(I:I))
        END DO
        NRVALW=NRVALW+N
!
        ITXPBF(IU)=1
        ISFTBF(IU)=0
        RETURN
! end of file
 1013   WRITE(6,*)'End of file: Unit#=',IU
        NVARW=NVARB+1
        IVALP(NVARW)=NRVALW
        ISIZE(NVARW)=0
        ITYPE(NVARW)=0
        NRVALW=NRVALW+1
        IOU(IU)=IEOF
        RETURN
!----------------------------------
!  iofn #14
!   function writef
!----------------------------------
      CASE(14)
        NFNAME=0
        IF(NARG.LE.2) THEN
        WRITE(6,*)'Number of arguments mismatch in the function writef!'
          NVARW=NVARB
          IERR=1
          RETURN
        END IF
        IF (ITYPE(NVARB+1).EQ.0) THEN
! file open (case of quick writef)
          IU=ISYSIO
          DO !IU
            INQUIRE(IU,OPENED=LOPEN)
            IF (.NOT.LOPEN) EXIT
            IU=IU+1
          END DO
          IPS   =IVALP(NVARB+1)
          NFNAME=ISIZE(NVARB+1)
          IF (NFNAME.EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1 of' &
                   // ' the function writef!'
            NVARW=NVARB
            IERR=1
            RETURN
          END IF
          DO I=1,NFNAME
            FNAME(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
          END DO
          OPEN(UNIT=IU,FILE=FNAME(1:NFNAME),STATUS='UNKNOWN',ERR=1514)
        ELSE
!
! io errors
          IU=NINT(RVAL(IVALP(NVARB+1)))
          IF (ISIZE(NVARB+1).EQ.0) THEN
            WRITE(6,*)'Variable size is zero at the argument #1 of' &
                   // ' the function writef!'
            IERR=1
            NVARW=NVARB
            RETURN
          END IF
          IF (IOU(IU).EQ.IREAD) THEN
            WRITE(6,*)'Can not write after the function read!'
            WRITE(6,*)'    Unit#=',IU
            IERR=1
            NVARW=NVARB
            RETURN
          END IF
          IF (IOU(IU).EQ.ICLOSE.AND.IU.NE.5.AND.IU.NE.6) THEN
            WRITE(6,*)'File not open!'
            WRITE(6,*)'    Unit#=',IU
            IERR=1
            NVARW=NVARB
            RETURN
          END IF
          IOU(IU)=IWRITE
        END IF
! main loop
        N=0
        DO IARG=3,NARG
          IF (ITYPE(NVARB+IARG).NE.0) THEN
            ISS=ISIZE(NVARB+IARG)
            IF(ISS.EQ.0) THEN
              WRITE(6,*)'Error in the function writef!'
              WRITE(6,*) &
               'The size of argument is zero: argument #',IARG
              IERR=1
              NVARW=NVARB
              RETURN
            END IF
            N=MAX(N,ISS)
          ELSE
            N=MAX(N,1)
          END IF
          IPS=IVALP(NVARB+IARG)
          IF (IPS.LT.IPD.AND.IPS.GE.NRVAL) IPD=IPS
        END DO
!
        ITF  =ITYPE(NVARB+2)
        NFORM=ISIZE(NVARB+2)
        IPF  =IVALP(NVARB+2)
!
        IF (ITF.NE.0) THEN
          WRITE(6,*) &
      'Error! Type mismatch in the function writef at the argument #2.'
          IERR=1
          NVARW=NVARB
          RETURN
        END IF
!
        IBUF=0
        IFORM=1
        LFORM=.FALSE.
        NFLDMLT=0
!
        DO I=0,N-1
          DO IARG=3,NARG
            IT =ITYPE(NVARB+IARG)
            ISS=ISIZE(NVARB+IARG)
            IPS=IVALP(NVARB+IARG)
!
            IF ( NFLDMLT.EQ.0 ) THEN
              IF ( .NOT. LFORM ) THEN
!
! transfer characters which does not enclosed by "#"
! to the text buffer
 214            CONTINUE
                DO WHILE( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#' &
                        .AND. IFORM.LT.NFORM )
                  IF (IBUF.EQ.NBUF) THEN
                    WRITE(IU,'(A,$)')ABUF(1:NBUF)
                    IBUF=0
                  END IF
                  IBUF=IBUF+1
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IFORM=IFORM+1
                END DO
 
!
                IF ( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#') THEN
                  WRITE(6,*)'A wrong format in the function writef!'
                  WRITE(6,*)'A delimiter "#" may be missing.'
                  IERR=1
                  NVARW=NVARB
                  RETURN
                ELSE
                  IFORM=IFORM+1
                  AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IF (AFORM.EQ.'#') THEN
                    IF (IBUF.EQ.NBUF) THEN
                      WRITE(IU,'(A,$)')ABUF(1:NBUF)
                      IBUF=0
                    END IF
                    IBUF=IBUF+1
                    ABUF(IBUF:IBUF)=AFORM
                    IFORM=IFORM+1
                    GOTO 214
                  END IF
                  LFORM=.TRUE.
                END IF
              END IF
!
! get the multiplyer of a format
              LRET=.TRUE.
              NFLDMLT=1
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              IF (INDEX('0123456789',AFORM).NE.0) THEN
                NFLDMLT=0
                K=INDEX('0123456789',AFORM)
                DO WHILE (K.NE.0)
                  NFLDMLT=NFLDMLT*10+(K-1)
                  IFORM=IFORM+1
                  IF (IFORM.LE.NFORM) THEN
                    AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                    K=INDEX('0123456789',AFORM)
                  ELSE
                    WRITE(6,*)'A wrong format in the function writef!'
                    WRITE(6,*)'A wrongly placed multiplyer is found.'
                    IERR=1
                    NVARW=NVARB
                    RETURN
                  END IF
                END DO
              END IF
!
! cut out one format specifyer
              NFMT=0
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              DO WHILE( INDEX('#/+,',AFORM).EQ.0 .AND. IFORM.LE.NFORM )
                NFMT=NFMT+1
                AFMT(NFMT:NFMT)=AFORM
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) THEN
                  AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
                ELSE
                  WRITE(6,*)'A wrong format in the function writef!'
                  WRITE(6,*)'A delimiter "#" may be missing.'
                  IERR=1
                  NVARW=NVARB
                  RETURN
                END IF
              END DO
!
! default formats for each type
              IF (NFMT.EQ.1) THEN
                IF (INDEX('aAcC',AFMT(1:1)).NE.0) THEN
                  AFMT='A'
                  NFMT=1
                ELSE IF (INDEX('lL',AFMT(1:1)).NE.0) THEN
                  AFMT='I2'
                  NFMT=2
                ELSE IF (INDEX('iI',AFMT(1:1)).NE.0) THEN
                  AFMT='I12'
                  NFMT=3
                ELSE IF (INDEX('rR',AFMT(1:1)).NE.0) THEN
                  AFMT='G15.7'
                  NFMT=5
                ELSE IF (INDEX('dD',AFMT(1:1)).NE.0) THEN
                  AFMT='G24.16'
                  NFMT=6
                ELSE
                  WRITE(6,*)'A wrong format in the function writef!'
                  WRITE(6,*)'An unknown type is specified:' &
                           //AFMT(1:NFMT)
                  IERR=1
                  NVARW=NVARB
                  RETURN
                END IF
              END IF
            END IF
!
! get the size of a field: NFLD
            IF (AFMT.EQ.'A') THEN
              NFLD=ISS
            ELSE
              AT=AFMT(2:NFMT)
              NFLD=INT(DNUM2(NFMT-1,AT(:32),K))
            END IF
!
! transform a value to the format specified by AFMT and
! write the field to the text buffer
            IF (IT.EQ.0) THEN
              DO K=1,NFLD
                IF (IBUF.EQ.NBUF) THEN
                  WRITE(IU,'(A,$)')ABUF(1:NBUF)
                  IBUF=0
                END IF
                IBUF=IBUF+1
                IF (K.LE.ISS) THEN
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPS+K-1)))
                ELSE
                  ABUF(IBUF:IBUF)=' '
                END IF
              END DO
              NFLD=0
            ELSE IF (IT.EQ.1.OR.IT.EQ.4) THEN
              WRITE(AT,'('//AFMT(1:NFMT)//')',ERR=8014) &
                                    NINT(RVAL(IPS+MOD(I,ISS)))
            ELSE IF (IT.EQ.2.OR.IT.EQ.3) THEN
              WRITE(AT,'('//AFMT(1:NFMT)//')',ERR=8014) &
                                         RVAL(IPS+MOD(I,ISS))
            END IF
!
            IF (IBUF+NFLD.LE.NBUF) THEN
              IF (NFLD.GT.0) ABUF(IBUF+1:IBUF+NFLD)=AT(1:NFLD)
              IBUF=IBUF+NFLD
            ELSE
              IF (IBUF.LT.NBUF) ABUF(IBUF+1:NBUF)=AT(1:NBUF-IBUF)
              WRITE(IU,'(A,$)')ABUF(1:NBUF)
              IF (NFLD.GE.NBUF-IBUF+1) &
                 ABUF(1:NFLD+IBUF-NBUF)=AT(NBUF-IBUF+1:NFLD)
              IBUF=NFLD+IBUF-NBUF
            END IF
            NFLDMLT=NFLDMLT-1
!
! handling delimiters
            IF (NFLDMLT.EQ.0) THEN
!
              AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              IF (AFORM.EQ.'+') THEN
                LRET=.FALSE.
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              ELSE IF (AFORM.EQ.'/') THEN
                IF (IBUF.NE.0) THEN
                  WRITE(IU,'(A)')ABUF(1:IBUF)
                  IBUF=0
                ELSE
                WRITE(IU,*)''
                END IF
                IFORM=IFORM+1
                IF (IFORM.LE.NFORM) AFORM=CHAR(NINT(RVAL(IPF+IFORM-1)))
              END IF
!
              IF (AFORM.EQ.'#') THEN
                IFORM=IFORM+1
!
! transfer characters which does not enclosed by "#"
! to the text buffer
 314            CONTINUE
                DO WHILE( CHAR(NINT(RVAL(IPF+IFORM-1))) .NE. '#' &
                      .AND. IFORM .LE. NFORM )
                  IF (IBUF.EQ.NBUF) THEN
                    WRITE(IU,'(A,$)')ABUF(1:NBUF)
                    IBUF=0
                  END IF
                  IBUF=IBUF+1
                  ABUF(IBUF:IBUF)=CHAR(NINT(RVAL(IPF+IFORM-1)))
                  IFORM=IFORM+1
                END DO
                IF ( CHAR(NINT(RVAL(IPF+IFORM-1))).EQ. '#') THEN
                  IFORM=IFORM+1
                  IF (CHAR(NINT(RVAL(IPF+IFORM-1))).EQ.'#') THEN
                    IF (IBUF.EQ.NBUF) THEN
                      WRITE(IU,'(A,$)')ABUF(1:NBUF)
                      IBUF=0
                    END IF
                    IBUF=IBUF+1
                    ABUF(IBUF:IBUF)=AFORM
                    IFORM=IFORM+1
                    GOTO 314
                  END IF
                  LFORM=.TRUE.
                END IF
              END IF
!
              IF (IFORM.GT.NFORM) THEN
                IFORM=1
                LFORM=.FALSE.
                IF (IBUF.NE.0.AND.LRET) THEN
                  WRITE(IU,'(A)')ABUF(1:IBUF)
                  IBUF=0
                ELSE IF (LRET) THEN
                  WRITE(IU,*)''
                END IF
              END IF
!
            END IF
          END DO !IARG
        END DO !I
!
        IF (IBUF.NE.0) THEN
          IF (LRET) THEN
            WRITE(IU,'(A)')ABUF(1:IBUF)
          ELSE
            WRITE(IU,'(A,$)')ABUF(1:IBUF)
          END IF
        END IF
!
        NVARW=NVARB
!
! close file (case of quick write)
        IF (NFNAME.NE.0) CLOSE(IU)
        RETURN
!
!  error trap
 1514   CLOSE(IU)
        WRITE(6,*)' IO error in the function writef!'
        WRITE(6,*)'          Unit#=',IU
        WRITE(6,*)'     File name=',FNAME
        IOU(IU)=0
        IERR=1
        RETURN
!
!  error trap for wrong format
 8014   WRITE(6,*) &
         'Error! A wrong format in the function writef: '//AFMT(1:NFMT)
        IERR=1
        RETURN
!----------------------------------
!  iofn #15
!   function setdlm
!   (set delimiters other than spaces)
!----------------------------------
      CASE(15)
        IPS  =IVALP(NVARW)
        NUDLM=ISIZE(NVARW)
        DO I=1,NUDLM
          UDLM(I:I)=CHAR(NINT(RVAL(IPS+I-1)))
        END DO
        NUDLM=NUDLM+1
        UDLM(NUDLM:NUDLM)=' '
        NUDLM=NUDLM+1
        UDLM(NUDLM:NUDLM)=CHAR(9)
        NUDLM=NUDLM+1
        UDLM(NUDLM:NUDLM)=CHAR(13)
        NVARW=NVARB
        RETURN
      END SELECT
      END SUBROUTINE X_IOFN
!>>>END IOFNC
!>>>START EXFNC
!//////////////////////////////////////////////////////////////////
!   Graphic functions (The graphic library Pig is needed.)
!   These functions are obosolate for the versions 2.0 or later.
!   Instead, use the Postscript library pslib.x to make figures.
!//////////////////////////////////////////////////////////////////
      SUBROUTINE X_EXFN(NFN,NARG &
                       ,IVALP,ITYPE,ISIZE,RVAL &
                       ,NVAR,NVARW,NRVAL,NRVALW,NRVALX &
                       ,NVL,NVLW,ISTP &
                       ,IUNIT,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(ITXT=80)
      CHARACTER(14)  ::  FNAME
      CHARACTER(160)  :: BUF
      DIMENSION IVALP(*),ISIZE(*),ITYPE(*),RVAL(*),NVL(*),NVLW(*)
      SAVE FNAME
!
      NVARB=NVARW-NARG
      DO 10 IARG=1,NARG
        K=IVALP(NVARB+IARG)
        IF (K.GE.NRVAL.AND.NRVALW.GT.K) NRVALW=K
 10   CONTINUE
      GOTO (101,102,103,104,105,106,107,108,109,110,111,112 &
      ,113,114,115,116,117,118,119,120,121,122,123,124,125),NFN
 200  RETURN
!
!     drfstr
!
 101  IU=40
      IF (NARG.GE.2) THEN
        IPS=IVALP(NVARB+2)
        ISS=ISIZE(NVARB+2)
        IF (ITYPE(NVARB+2).EQ.0) THEN
          DO  201 I=0,ISS-1
            FNAME(1:I+1)=FNAME(1:I)//CHAR(NINT(RVAL(IPS+I)))
 201      CONTINUE
          FNAME=FNAME(1:ISS)
          OPEN(UNIT=40,FILE=FNAME,STATUS='UNKNOWN',ERR=301)
        ELSE
          IU=NINT(RVAL(IPS))
        END IF
      ELSE
        FNAME=' '
      END IF
      CALL DRFSTR(NINT(RVAL(IVALP(NVARB+1))))
      CALL OFLCHG(IU)
      NVARW=NVARW-NARG
      RETURN
 301  WRITE(6,*)'IO ERR !!'
      IERR=1
      RETURN
!
!     drfend
!
 102  CALL DRFEND
      IF (FNAME.NE.' ') CLOSE(40)
      NVARW=NVARW-NARG
      RETURN
!
!     setpen
!
 103  CALL SETPEN(NINT(RVAL(IVALP(NVARB+1))))
      NVARW=NVARW-NARG
      RETURN
!
!     line1
!
 104  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
 
      IF (NARG.EQ.4) THEN
        DO  204 I=0,MAX(N1,N2,N3,N4)-1
          XS=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          YS=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          XF=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          YF=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          CALL LINE1(REAL(XS),REAL(YS),REAL(XF),REAL(YF))
 204    CONTINUE
      ELSE IF (NARG.EQ.7) THEN
        N5=ISIZE(NVARB+5)
        N6=ISIZE(NVARB+6)
        N7=ISIZE(NVARB+7)
        DO  304 I=0,MAX(N1,N2,N3,N4,N5,N6,N7)-1
          XS=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          YS=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          XF=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          YF=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          S1=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          S2=RVAL(IVALP(NVARB+6)+MOD(I,N6))
           K=NINT(RVAL(IVALP(NVARB+7)+MOD(I,N7)))
           CALL LINE3(REAL(XS),REAL(YS),REAL(XF),REAL(YF) &
                    ,REAL(S1),REAL(S2),K)
 304    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in line1'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     line2
!
 105  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
 
      IF (NARG.EQ.2) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        CALL LINE2(REAL(X),REAL(Y),2)
        DO  205 I=1,MAX(N1,N2)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          CALL LINE2(REAL(X),REAL(Y),1)
 205    CONTINUE
      ELSE IF (NARG.EQ.3) THEN
        N3=ISIZE(NVARB+3)
        DO  305 I=0,MAX(N1,N2,N3)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          K=NINT(RVAL(IVALP(NVARB+3)+MOD(I,N3)))
          CALL LINE2(REAL(X),REAL(Y),K)
 305    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in line2'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     line4
!
 106  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
 
      IF (NARG.EQ.2) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        CALL LINE4(REAL(X),REAL(Y),2)
        DO  206 I=1,MAX(N1,N2)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          CALL LINE4(REAL(X),REAL(Y),1)
 206    CONTINUE
      ELSE IF (NARG.EQ.3) THEN
        N3=ISIZE(NVARB+3)
        DO  306 I=0,MAX(N1,N2,N3)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          K=NINT(RVAL(IVALP(NVARB+3)+MOD(I,N3)))
          CALL LINE4(REAL(X),REAL(Y),K)
 306    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in line4'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     atype
!
 107  IF (NARG.GE.4.AND.NARG.LE.8) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        H=RVAL(IVALP(NVARB+3))
        IPS=IVALP(NVARB+4)
        ISS=ISIZE(NVARB+4)
        DO  207 I=0,ISS-1
          BUF(I+1:I+1)=CHAR(NINT(RVAL(IPS+I)))
 207    CONTINUE
        BUF=BUF(1:ISS)
        S=0D0
        IF (NARG.GE.5) S=RVAL(IVALP(NVARB+5))
        L=0
        IF (NARG.GE.6) L=NINT(RVAL(IVALP(NVARB+6)))
        M=0
        IF (NARG.GE.7) M=NINT(RVAL(IVALP(NVARB+7)))
        T=-1D0
        IF (NARG.GE.8) T=RVAL(IVALP(NVARB+8))
        CALL BTYPE(REAL(X),REAL(Y),REAL(H),BUF,REAL(S),ISS,L,M,REAL(T))
      ELSE
        WRITE(6,*)'the number of arguments mismatch in atype'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     axis
!
 108  IF (NARG.GE.6.AND.NARG.LE.12) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        A1=RVAL(IVALP(NVARB+3))
        A2=RVAL(IVALP(NVARB+4))
        A3=RVAL(IVALP(NVARB+5))
        D=RVAL(IVALP(NVARB+6))
        I=0
        IF (NARG.GE.7) I=NINT(RVAL(IVALP(NVARB+7)))
        S=0D0
        IF (NARG.GE.8) S=RVAL(IVALP(NVARB+8))
        H=-1D0
        IF (NARG.GE.9) H=RVAL(IVALP(NVARB+9))
        N=0
        IF (NARG.GE.10) N=NINT(RVAL(IVALP(NVARB+10)))
        M=1
        IF (NARG.GE.11) M=NINT(RVAL(IVALP(NVARB+11)))
        KD=0
        IF (NARG.GE.12) KD=NINT(RVAL(IVALP(NVARB+12)))
        CALL AXIS2(REAL(X),REAL(Y),REAL(A1),REAL(A2),REAL(A3),REAL(D) &
                   ,I,REAL(S),REAL(H),N,M,KD)
      ELSE
        WRITE(6,*)'the number of arguments mismatch in axis'
        IERR=1
      END IF
      NVARW=NVARW-NARG
 
      RETURN
!
!     arrow1
!
 109  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
      N5=ISIZE(NVARB+5)
 
      IF (NARG.EQ.5) THEN
        DO  209 I=0,MAX(N1,N2,N3,N4,N5)-1
          XS=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          YS=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          XF=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          YF=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          AL=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL ARROW1(REAL(XS),REAL(YS),REAL(XF),REAL(YF),REAL(AL),3,3)
 209    CONTINUE
      ELSE IF (NARG.EQ.6) THEN
        N6=ISIZE(NVARB+6)
        DO  309 I=0,MAX(N1,N2,N3,N4,N5,N6)-1
          XS=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          YS=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          XF=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          YF=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          AL=RVAL(IVALP(NVARB+5)+MOD(I,N5))
           J=NINT(RVAL(IVALP(NVARB+6)+MOD(I,N6)))
          CALL ARROW1(REAL(XS),REAL(YS),REAL(XF),REAL(YF),REAL(AL),J,3)
 309    CONTINUE
      ELSE IF (NARG.EQ.7) THEN
        N6=ISIZE(NVARB+6)
        N7=ISIZE(NVARB+7)
        DO  409 I=0,MAX(N1,N2,N3,N4,N5,N6)-1
          XS=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          YS=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          XF=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          YF=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          AL=RVAL(IVALP(NVARB+5)+MOD(I,N5))
           J=NINT(RVAL(IVALP(NVARB+6)+MOD(I,N6)))
           N=NINT(RVAL(IVALP(NVARB+7)+MOD(I,N7)))
          CALL ARROW1(REAL(XS),REAL(YS),REAL(XF),REAL(YF),REAL(AL),J,N)
 409    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in arrow1'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     circle
!
 110  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
 
      DO  210 I=0,MAX(N1,N2,N3)-1
        X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
        Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
        R=RVAL(IVALP(NVARB+3)+MOD(I,N3))
        CALL CIRCLE(REAL(X),REAL(Y),REAL(R))
 210  CONTINUE
      NVARW=NVARW-NARG
      RETURN
!
!     RECT
!
 111  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
 
      IF (NARG.EQ.4) THEN
        DO  211 I=0,MAX(N1,N2,N3,N4)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          W=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          CALL RECT(REAL(X),REAL(Y),REAL(H),REAL(W),0.0)
 211    CONTINUE
      ELSE IF (NARG.EQ.5) THEN
        N5=ISIZE(NVARB+5)
        DO  311 I=0,MAX(N1,N2,N3,N4,N5)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          W=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL RECT(REAL(X),REAL(Y),REAL(H),REAL(W),REAL(S))
 311    CONTINUE
      ELSE IF (NARG.EQ.7) THEN
        N5=ISIZE(NVARB+5)
        N6=ISIZE(NVARB+6)
        N7=ISIZE(NVARB+7)
        DO  411 I=0,MAX(N1,N2,N3,N4,N5,N6,N7)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          W=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          T=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          F=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL HATCH(REAL(X),REAL(Y),REAL(H),REAL(W),REAL(S) &
                    ,REAL(T),REAL(F),0)
 411    CONTINUE
      ELSE IF (NARG.EQ.8) THEN
        N5=ISIZE(NVARB+5)
        N6=ISIZE(NVARB+6)
        N7=ISIZE(NVARB+7)
        N8=ISIZE(NVARB+8)
        DO  511 I=0,MAX(N1,N2,N3,N4,N5,N6,N7,N8)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          W=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          T=RVAL(IVALP(NVARB+5)+MOD(I,N6))
          F=RVAL(IVALP(NVARB+5)+MOD(I,N7))
          J=NINT(RVAL(IVALP(NVARB+5)+MOD(I,N8)))
          CALL HATCH(REAL(X),REAL(Y),REAL(H),REAL(W),REAL(S) &
                    ,REAL(T),REAL(F),J)
 511    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in rect'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     arc0
!
 112  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
      N5=ISIZE(NVARB+5)
 
      IF (NARG.EQ.5) THEN
        DO  312 I=0,MAX(N1,N2,N3,N4,N5)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          R=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          TS=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          TF=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL ARC0(REAL(X),REAL(Y),REAL(R),REAL(TS),REAL(TF))
 312    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     setlin
!
 113  S1=RVAL(IVALP(NVARB+1))
      S2=RVAL(IVALP(NVARB+2))
      IF (NARG.EQ.3) THEN
        K=NINT(RVAL(IVALP(NVARB+3)))
        CALL SETLIN(REAL(S1),REAL(S2),K)
      ELSE IF (NARG.EQ.4) THEN
        S3=RVAL(IVALP(NVARB+3))
        K=NINT(RVAL(IVALP(NVARB+4)))
        CALL SETLN2(REAL(S1),REAL(S2),REAL(S3),K)
      ELSE
        WRITE(6,*)'the number of arguments mismatch in setlin'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     bear
!
 114  X=RVAL(IVALP(NVARB+1))
      Y=RVAL(IVALP(NVARB+2))
      H=RVAL(IVALP(NVARB+3))
      R=RVAL(IVALP(NVARB+4))
      IF (NARG.EQ.3) THEN
        CALL BEAR(REAL(X),REAL(Y),REAL(H),0.0)
      ELSE IF (NARG.EQ.4) THEN
        CALL BEAR(REAL(X),REAL(Y),REAL(H),REAL(R))
      ELSE
        WRITE(6,*)'the number of arguments mismatch bear'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     symbol
!
 115  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
 
      IF (NARG.EQ.4) THEN
        DO  215 I=0,MAX(N1,N2,N3,N4)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          CALL SYMBOL(REAL(X),REAL(Y),REAL(H),N,0.0)
 215    CONTINUE
      ELSE IF (NARG.EQ.5) THEN
        N5=ISIZE(NVARB+5)
        DO  315 I=0,MAX(N1,N2,N3,N4,N5)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL SYMBOL(REAL(X),REAL(Y),REAL(H),N,REAL(S))
 315    CONTINUE
      ELSE IF (NARG.EQ.6) THEN
        N5=ISIZE(NVARB+5)
        N6=ISIZE(NVARB+6)
        DO  415 I=0,MAX(N1,N2,N3,N4,N5,N6)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          XY=RVAL(IVALP(NVARB+6)+MOD(I,N6))
          CALL SYMBLS(REAL(X),REAL(Y),REAL(H),N,REAL(S),REAL(XY),0.0)
 415    CONTINUE
      ELSE IF (NARG.EQ.7) THEN
        N5=ISIZE(NVARB+5)
        N6=ISIZE(NVARB+6)
        N7=ISIZE(NVARB+7)
        DO  515 I=0,MAX(N1,N2,N3,N4,N5,N6,N7)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          XY=RVAL(IVALP(NVARB+6)+MOD(I,N6))
          T=RVAL(IVALP(NVARB+7)+MOD(I,N7))
          CALL SYMBLS(REAL(X),REAL(Y),REAL(H),N,REAL(S),REAL(XY) &
                     ,REAL(T))
 515    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in symbol'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     mark
!
 116  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
 
      IF (NARG.EQ.4) THEN
        DO  216 I=0,MAX(N1,N2,N3,N4)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          CALL MARK(REAL(X),REAL(Y),REAL(H),N)
 216    CONTINUE
      ELSE IF (NARG.EQ.5) THEN
        N5=ISIZE(NVARB+5)
        DO  316 I=0,MAX(N1,N2,N3,N4,N5)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          H=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          N=NINT(RVAL(IVALP(NVARB+4)+MOD(I,N4)))
          S=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          CALL MARK2(REAL(X),REAL(Y),REAL(H),N,REAL(S))
 316    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in mark'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     lgaxis
!
 117  IF (NARG.GE.5.AND.NARG.LE.10) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        IS=NINT(RVAL(IVALP(NVARB+3)))
        IF=NINT(RVAL(IVALP(NVARB+4)))
        D=RVAL(IVALP(NVARB+5))
        S=0D0
        IF (NARG.GE.6) S=RVAL(IVALP(NVARB+6))
        H=4D0
        IF (NARG.GE.7) H=RVAL(IVALP(NVARB+7))
        N=0
        IF (NARG.GE.8) N=NINT(RVAL(IVALP(NVARB+8)))
        M=1
        IF (NARG.GE.8) M=NINT(RVAL(IVALP(NVARB+9)))
        F=1D0
        IF (NARG.GE.8) F=RVAL(IVALP(NVARB+10))
        CALL LGAXS2(REAL(X),REAL(Y),IS,IF,REAL(D),N,REAL(S),REAL(H),N &
                   ,M,REAL(F))
      ELSE
        WRITE(6,*)'the number of arguments mismatch in lgaxis'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     dimen1
!
 118  IF (NARG.EQ.6) THEN
        XS=RVAL(IVALP(NVARB+1))
        YS=RVAL(IVALP(NVARB+2))
        XF=RVAL(IVALP(NVARB+3))
        YF=RVAL(IVALP(NVARB+4))
         H=RVAL(IVALP(NVARB+5))
        S=0D0
        IPS=IVALP(NVARB+6)
        ISS=ISIZE(NVARB+6)
        DO  218 I=0,ISS-1
          BUF(I+1:I+1)=CHAR(NINT(RVAL(IPS+I)))
 218    CONTINUE
        BUF=BUF(1:ISS)
        CALL DIMEN1(REAL(XS),REAL(YS),REAL(XF),REAL(YF),REAL(H),BUF,ISS)
      ELSE
        WRITE(6,*)'the number of arguments mismatch in dimen1'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     arrow3
!
 119  N1=ISIZE(NVARB+1)
      N2=ISIZE(NVARB+2)
      N3=ISIZE(NVARB+3)
      N4=ISIZE(NVARB+4)
      N5=ISIZE(NVARB+5)
      N6=ISIZE(NVARB+6)
 
      IF (NARG.EQ.6) THEN
        DO  219 I=0,MAX(N1,N2,N3,N4,N5,N6)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          R=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          TS=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          TF=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          AL=RVAL(IVALP(NVARB+6)+MOD(I,N6))
          CALL ARROW3(REAL(X),REAL(Y),REAL(R),REAL(TS),REAL(TF) &
                     ,REAL(AL),3,3)
 219    CONTINUE
      ELSE IF (NARG.EQ.7) THEN
        N7=ISIZE(NVARB+7)
        DO  319 I=0,MAX(N1,N2,N3,N4,N5,N6,N7)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          R=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          TS=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          TF=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          AL=RVAL(IVALP(NVARB+6)+MOD(I,N6))
          J=NINT(RVAL(IVALP(NVARB+7)+MOD(I,N7)))
          CALL ARROW3(REAL(X),REAL(Y),REAL(R),REAL(TS),REAL(TF) &
                     ,REAL(AL),J,3)
 319    CONTINUE
      ELSE IF (NARG.EQ.8) THEN
        N7=ISIZE(NVARB+7)
        N8=ISIZE(NVARB+8)
        DO  419 I=0,MAX(N1,N2,N3,N4,N5,N6,N7,N8)-1
          X=RVAL(IVALP(NVARB+1)+MOD(I,N1))
          Y=RVAL(IVALP(NVARB+2)+MOD(I,N2))
          R=RVAL(IVALP(NVARB+3)+MOD(I,N3))
          TS=RVAL(IVALP(NVARB+4)+MOD(I,N4))
          TF=RVAL(IVALP(NVARB+5)+MOD(I,N5))
          AL=RVAL(IVALP(NVARB+6)+MOD(I,N6))
          J=NINT(RVAL(IVALP(NVARB+7)+MOD(I,N7)))
          N=NINT(RVAL(IVALP(NVARB+8)+MOD(I,N8)))
          CALL ARROW3(REAL(X),REAL(Y),REAL(R),REAL(TS),REAL(TF) &
                     ,REAL(AL),J,N)
 419    CONTINUE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in arrow3'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     origin
!
 120  IF (NARG.EQ.2) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=RVAL(IVALP(NVARB+2))
        CALL ORIGIN(REAL(X),REAL(Y))
      ELSE
        WRITE(6,*)'the number of arguments mismatch in origin'
        IERR=1
      END IF
      RETURN
!
!     rotate
!
 121  IF (NARG.EQ.1) THEN
        S=RVAL(IVALP(NVARB+1))
        CALL ROTATE(REAL(S))
      ELSE
        WRITE(6,*)'the number of arguments mismatch in rotate'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     incheg
!
 122  IF (NARG.EQ.1) THEN
        N=NINT(RVAL(IVALP(NVARB+1)))
        CALL INCHEG(N)
      ELSE
        WRITE(6,*)'the number of arguments mismatch in incheg'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     factor
!
 123  IF (NARG.EQ.2.OR.NARG.EQ.1) THEN
        X=RVAL(IVALP(NVARB+1))
        Y=X
        IF (NARG.EQ.2) Y=RVAL(IVALP(NVARB+2))
        N=NINT(RVAL(IVALP(NVARB+1)))
        CALL FACTOR(REAL(X),REAL(Y))
      ELSE
        WRITE(6,*)'the number of arguments mismatch in factor'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     degree
!
 124  IF (NARG.EQ.0) THEN
        CALL DEGREE
      ELSE
        WRITE(6,*)'the number of arguments mismatch in degree'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!     radian
!
 125  IF (NARG.EQ.0) THEN
        CALL RADIAN
      ELSE
        WRITE(6,*)'the number of arguments mismatch in radian'
        IERR=1
      END IF
      NVARW=NVARW-NARG
      RETURN
!
!
!
      END
!>>>END EXFNC
 
 
