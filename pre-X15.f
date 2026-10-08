C
C    pre-X preprocessor for "X-card" Ver. 1.4 revision 2
C        for Xtls version 8.4 or later
C
C    This program expands multi-configuration expressions in "X-card" to
C     single-configuration ones.
C     Such as
C       [A-C]**    --> AAA AAB AAC ABB ABC ACC BBB BBC BCC CCC
C       [A-C][D,E] --> AD AE BD BE CD CE
C       [L1-L3]    --> L1 L2 L3
C       1[A,E]    --> 1A 1E
C     
C     (last modification 2005.10.2) 
C     
C   Version 1.3
C   -This version adds "line" command to its converted x-card file 
C   to inform line numbers in the original X-card file to Xtls ver. 8.4.
C
C
      PROGRAM PRE_X
      IMPLICIT NONE
C
      INTEGER NCNFM, NORDM
      PARAMETER(NCNFM=500000,NORDM=200)

C Unit number for Input/output 
      INTEGER IUIN, IUOUT
      PARAMETER(IUIN=3,IUOUT=1)

      INTEGER ITXMAX, LBLM 
      PARAMETER(ITXMAX=128,LBLM=32)

      INTEGER          NCNFLV 
      INTEGER          ICNFLV(NCNFM)
      CHARACTER*(LBLM) CNFLV(NCNFM)
C
      INTEGER          NORD
      INTEGER          IORD(NORDM)
      CHARACTER*(LBLM) AORD(NORDM)
      INTEGER          LIMN(NORDM)
C
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM

      INTEGER LIN,IERR,ITYP
C-----------------
     
      WRITE(6,*)'pre-X ver. 14'
      LIN=0
      OPEN(UNIT=3,FILE='xcard03')
      OPEN(UNIT=1,FILE='xcard01')
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      DO WHILE ( AWRD.EQ.'xcrd' .AND. DLM.EQ.':' )

        WRITE(6,*)'enter xcrd: line=',LIN
        WRITE(IUOUT,*)'xcrd: '

        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        CALL XCRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
C
        IF ( AWRD.EQ.'cnfg' .AND. DLM.EQ.':' ) THEN
          CALL CNFG(NCNFLV,ICNFLV,CNFLV,NORD,IORD,AORD
     &             ,LIMN,NWRD,AWRD,DLM,LIN)
        ELSE
          WRITE(6,*)'Error! item "CNFG:" expected !! at line=',LIN
          STOP
        END IF
C
        IF ( AWRD.EQ.'para' .AND. DLM.EQ.':' ) THEN
          CALL OPRT(NCNFLV,ICNFLV,CNFLV,NORD,IORD,AORD
     &             ,LIMN,NWRD,AWRD,DLM,LIN)
        ELSE
          WRITE(6,*)'Error! item "PARA:" expected !! at line=',LIN
          STOP
        END IF
C
        IF ( AWRD.EQ.'exec' .AND. DLM.EQ.':' ) THEN
           CALL EXEC(NWRD,AWRD,DLM,LIN)
        ELSE
          WRITE(6,*)'Error! item "EXEC:" expected !! at line=',LIN
          STOP
        END IF
C
        DO WHILE ( AWRD.EQ.'oprt' .AND. DLM.EQ.':' ) 
          CALL OPRT(NCNFLV,ICNFLV,CNFLV,NORD,IORD,AORD
     &             ,LIMN,NWRD,AWRD,DLM,LIN)
        END DO
C
        IF ( AWRD.EQ.'xend' .AND. DLM.EQ.':' ) THEN
          WRITE(IUOUT,*)'xend:'
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        ELSE
          WRITE(6,*)'Error! item "XEND:" expected !! at line=',LIN
          STOP
        END IF
C
        IF ( AWRD.EQ.'stop' .AND. DLM.EQ.':' ) THEN
          WRITE(IUOUT,*)'stop:'
          STOP
        END IF
      END DO
      IF (IERR.EQ.3) THEN 
        WRITE(6,*)'Warning! item "STOP:" expected !! at line=',LIN
        WRITE(IUOUT,*)'stop: '
        WRITE(6,*)'"stop:" has been inserted. I hope it works.'
        STOP
      ELSE
        WRITE(6,*)'Error! item "XCRD:" expected !! at line=',LIN
        STOP
      END IF
      WRITE(6,*)'normal termination of pre-X'
      CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      END PROGRAM PRE_X

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   Definition of Orbitals and Configuration labels
C   in the item "CNFG:" 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CNFG(NCNFLV,ICNFLV,CNFLV,NORD,IORD,AORD
     &               ,LIMN,NWRD,AWRD,DLM,LIN)
      IMPLICIT NONE

C Parameters
      INTEGER   NCNFM,       NORDM
      PARAMETER(NCNFM=500000,NORDM=200)
      INTEGER   IUIN,  IUOUT
      PARAMETER(IUIN=3,IUOUT=1)
      INTEGER   ITXMAX,    LBLM,   NTXBFM
      PARAMETER(ITXMAX=128,LBLM=32,NTXBFM=256)

C Dummy arguments
      INTEGER         NCNFLV
      INTEGER         ICNFLV(NCNFM)
      CHARACTER*(LBLM) CNFLV(NCNFM)
C
      INTEGER          NORD
      INTEGER          IORD(NORDM)
      CHARACTER*(LBLM) AORD(NORDM)
      INTEGER          LIMN(NORDM)
C
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER            LIN

C Local variables
      INTEGER          NCNF
      CHARACTER*(LBLM) ACNF(NCNFM)
      INTEGER          ICNF(NCNFM)

      INTEGER            NWRDB
      CHARACTER*(ITXMAX) AWRDB

      INTEGER            ITXP
      CHARACTER*(NTXBFM) TXBF, AT

      INTEGER   NCOLM
      PARAMETER(NCOLM=10)
      CHARACTER*(LBLM) AORB(NCOLM)
      INTEGER  ICOL(NCOLM),IORB(NCOLM),NORB(NCOLM),ISG(NCOLM)
      INTEGER  NE(NCOLM),NEMAX(NCOLM),NEMIN(NCOLM)
      
      INTEGER I,J,K,L,N,IC, IERR, ITYP, IT

C Functions
      INTEGER ICNT
      REAL*8  DNUM2
C --------------------------
      WRITE(6,*)'enter cnfg: at line=',LIN
      WRITE(IUOUT,*)'cnfg:'
      WRITE(IUOUT,'(A9,I8,A1)')'line=',LIN,';'
C
C definition of orbital lebels
C
      WRITE(6,*)'  Now expanding orbital labels...'
      N=0
      NORD=0
      NCNFLV=0
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
 20   CONTINUE
        IF (NWRD.NE.0 ) THEN
          N=N+1
          NORD=NORD+1
          IF (NORD.GT.NORDM) THEN
            WRITE(6,*)'  ERROR! too many orbital labels.'
            WRITE(6,*)'    The parameter "NORDM" must be enhanced.' 
            WRITE(6,*)'                        at line=',LIN
            STOP
          END IF
          ICOL(N)=NORD
          IF ( DLM .EQ. '/ ' ) THEN
            AORD(NORD)=AWRD(1:LBLM)
            IORD(NORD)=NWRD
            NWRD=0
          ELSE
            AORD(NORD)=AWRD(1:LBLM)
            IORD(NORD)=NWRD-1
            AWRD(1:1)=AWRD(NWRD:NWRD)
            NWRD=1
          END IF
        ELSE IF (NWRD.EQ.0 .AND. DLM.EQ.'[ ') THEN
          AT='0123456789abcdefghijklmnopqrstuvwxyz'
          N=N+1
          ICOL(N)=NORD+1
          DO !until '] ' appears
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            IF (DLM.EQ.', '.OR.DLM.EQ.'] ') THEN
              NORD=NORD+1
              AORD(NORD)=AWRD(1:LBLM)
              IORD(NORD)=NWRD
            ELSE IF (DLM.EQ.'- ') THEN
              AWRDB=AWRD
              NWRDB=NWRD 
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
              IF (NWRD.GT.1) THEN
                I=NINT(DNUM2(NWRDB,AWRDB(2:)//' ',IT))
                J=NINT(DNUM2(NWRD ,AWRD (2:)//' ',IT))
                IF (AWRDB(1:1).NE.AWRD(1:1).OR.NWRDB.NE.NWRD) THEN
                  WRITE(6,*)
     &             '  A syntax ERROR was found inside [ ] at line=',LIN
                  STOP
                END IF
                DO L=I,J 
                  WRITE(AWRDB(1:6),'(I6)')100000+L
C                  WRITE(6,*)'AWRDB=',AWRDB(1:6),'NWRD=',NWRD
                  NORD=NORD+1
                  AORD(NORD)=AWRD(1:1)//AWRDB(6-NWRD+2:6)
                  IORD(NORD)=NWRD
                END DO
              ELSE
                I=INDEX(AT,AWRDB(1:1)) 
                J=INDEX(AT,AWRD (1:1)) 
                DO L=I,J 
                  NORD=NORD+1
                  AORD(NORD)=AT(L:L)
                  IORD(NORD)=1
                END DO
              END IF
            END IF
            IF (DLM.EQ.'] ') EXIT
          END DO
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        END IF
C
        IF (NWRD.EQ.1) THEN
          IORB(N)=4*INDEX('zspdfghijkl',AWRD(1:1))-6
          AORB(N)=AWRD(1:1)
          NORB(N)=1
          IF (IORB(N).LE.-6) THEN
            WRITE(6,*)
     &      '  ERROR! wrong orbital symmetry ->',AWRD(1:1)
            WRITE(6,*)'                        at line=',LIN
            STOP
          END IF
        ELSE
          WRITE(6,*)
     &  '  Error! Wrong syntax in the definition of an orbital lebel'
          WRITE(6,*)'              at line=',LIN
          STOP
        END IF
C  
C  electron number limilter
C
        IF ( NWRD.EQ.1 .AND. DLM.EQ.'( ' ) THEN
        ELSE IF (NWRD.EQ.1) THEN
          NEMIN(N)=0
          NEMAX(N)=IORB(N)
          ISG(N)=1
          NE(N)=0
          NWRD=0
        END IF
C
        IF (DLM.NE.'  ' .AND. DLM.NE.'# ' .AND. DLM.NE.'[ ' 
     &     .AND. DLM .NE. ') ' ) THEN
          WRITE(6,*)
     & '  Error! Wrong delimiter in the definition of an orbital lebel.'
            WRITE(6,*)'              at line=',LIN
          STOP
        END IF
        IF (DLM.NE.'# ' .AND. DLM.NE.'[ ' ) THEN
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          IF (DLM.NE.'# ' .OR. NWRD.NE.0 ) GOTO 20
        END IF
      IF (DLM.NE.'# ') GOTO 20 
      ICOL(N+1)=NORD+1
C
      ITXP=0
      TXBF=' '
      AT='spdfghijklz'
      DO J=1,N
        DO I=ICOL(J),ICOL(J+1)-1
          IF (ISG(J).EQ.1) THEN
            LIMN(I)=NEMAX(J)-NE(J)
          ELSE IF (ISG(J).EQ.-1) THEN
            LIMN(I)=NE(J)-NEMIN(J)
          ELSE
            LIMN(I)=IORB(J)
          END IF
          IF (ITXP+IORD(I)+2.GE.ITXMAX-2) THEN
            WRITE(IUOUT,'(A)')TXBF(1:ITXP)
            ITXP=0
          END IF
          K=NORB(J)
          TXBF(ITXP+1:ITXP+IORD(I)+K+1)
     &                    =' '//AORD(I)(:IORD(I))//AORB(J)(:K)
          ITXP=ITXP+IORD(I)+K+1
        END DO !I
      END DO !J
      WRITE(IUOUT,'(A)')TXBF(1:ITXP)
      WRITE(6,*)'  Orbital labels were sucessfully expanded.'
C
C   definition of configuration labels
C
      DO !while '# ' exist
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        AWRDB=AWRD
        IF (NWRD.GE.1) THEN
          DO I=1,NWRD-1
            AWRD(I:I)=AWRD(I+1:I+1)
          END DO
          AWRD(NWRD:NWRD)=' '
        END IF
        NWRD=NWRD-1
        CALL EXPND(NWRD,AWRD,DLM,LIN,IUIN,NCNF,ACNF,ICNF
     &            ,NORD,AORD,IORD,LIMN)
        DO I=1,N
          IF (NWRD.NE.0.AND.DLM.EQ.'* ' ) THEN
            NWRD=0
          ELSE
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          END IF
          IF (DLM.EQ.'- ') THEN
            ISG(I)=-1
            NE(I)=DNUM2(NWRD,AWRD,IT)
          ELSE IF (DLM.EQ.'+ ') THEN
            ISG(I)=1
            NE(I)=DNUM2(NWRD,AWRD,IT)
          ELSE IF(NWRD.NE.0.AND.(DLM.EQ.'  ' .OR. DLM.EQ.', ' 
     &        .OR. DLM.EQ.'* ' .OR. DLM.EQ.'# ') )THEN
            ISG(I)=0
            NE(I)=DNUM2(NWRD,AWRD,IT)
          ELSE IF (NWRD.EQ.0 .AND. DLM .EQ. '* ' ) THEN
          ELSE IF (.NOT.(DLM.EQ.'# '.AND.I.EQ.N)) THEN
            WRITE(6,*)'  Error! Inconsistant number of colums.'
            WRITE(6,*)'              at line=',LIN
            STOP
          ELSE
            WRITE(6,*)'  Error! Wrong configulation table'
            WRITE(6,*)'              at line=',LIN
            STOP
          END IF
        END DO !I
        WRITE(6,*)
     &   ' Number of the configurations for #'//AWRDB(1:1),NCNF
C
        DO IC=1,NCNF
          TXBF='#'//AWRDB(1:1)//ACNF(IC)(1:ICNF(IC))
          ITXP=2+ICNF(IC)
          DO J=1,N
            DO I=ICOL(J),ICOL(J+1)-1
              K=ICNT(ICNF(IC),ACNF(IC),IORD(I),AORD(I))
              L=NE(J)+ISG(J)*K
              IF ( (NEMIN(J)-L)*(L-NEMAX(J)) .LT. 0 ) GOTO 120
            END DO !I
          END DO !J
          NCNFLV=NCNFLV+1
          CNFLV (NCNFLV)=AWRDB(1:1)//ACNF(IC) 
          ICNFLV(NCNFLV)=ICNF(IC)
          DO J=1,N
            DO I=ICOL(J),ICOL(J+1)-1
              K=ICNT(ICNF(IC),ACNF(IC),IORD(I),AORD(I))
              IF (ITXP+4.GE.ITXMAX-2) THEN
                WRITE(IUOUT,'(A)')TXBF(1:ITXP)
                ITXP=0
              END IF
              WRITE(TXBF(ITXP+1:ITXP+4),'(I3)') NE(J)+ISG(J)*K
              ITXP=ITXP+4
            END DO !I
          END DO !J
          WRITE(IUOUT,'(A)')TXBF(1:ITXP)
 120      CONTINUE
        END DO !IC
        IF (DLM.EQ.'+ '.OR.DLM.EQ.'- '.OR.DLM.EQ.'] '.OR.DLM.EQ.'* ') 
     &    CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        IF ( DLM.EQ.'; ' .OR. DLM.EQ.'  ' ) 
     &    CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        IF (DLM.NE.'# ') EXIT
      END DO 
      WRITE(6,*)'  The configuration table has been created.'
C
      END SUBROUTINE CNFG

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  WRITE DATA TO THE TEXT BUFFER
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
      IMPLICIT NONE 

C Parameters
      INTEGER   NCNFM
      PARAMETER(NCNFM=500000)
      INTEGER   IUIN,  IUOUT
      PARAMETER(IUIN=3,IUOUT=1)
      INTEGER   ITXMAX,    NTXBFM
      PARAMETER(ITXMAX=128,NTXBFM=256)

C Dummy arguments
      CHARACTER*(NTXBFM) TXBF
      INTEGER            ITXP
 
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER            ITYP

C Local variables
      INTEGER NDLM
C --------------------------------

      NDLM=2
      IF (DLM.EQ.'+@'.OR.DLM.EQ.'-@') DLM(2:2)=' '
      IF (DLM(2:2).EQ.' ') NDLM=1
      IF (ITYP.EQ.1) THEN
        IF (ITXP+1+NWRD+1+NDLM.GE.ITXMAX-2) THEN
          WRITE(IUOUT,*)TXBF(1:ITXP)
          ITXP=2
          IF (DLM.EQ.': ')ITXP=0 
          TXBF='  '
        END IF
        
        TXBF(ITXP+1:ITXP+1+NWRD+1+NDLM)='"'//AWRD(1:NWRD)//'"'
     &                               //DLM(1:NDLM)
        ITXP=ITXP+1+NWRD+1+NDLM
      ELSE
        IF (ITXP+NWRD+NDLM.GE.ITXMAX-2) THEN
          WRITE(IUOUT,*)TXBF(1:ITXP)
          ITXP=2
          IF (DLM.EQ.': ')ITXP=0 
          TXBF='  '
        END IF
        
        TXBF(ITXP+1:ITXP+NWRD+NDLM)=AWRD(1:NWRD)//DLM(1:NDLM)
        ITXP=ITXP+NWRD+NDLM
      END IF

      END SUBROUTINE WBUFF

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   ITEM "EXEC"  
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EXEC(NWRD,AWRD,DLM,LIN)
      IMPLICIT NONE

C Parameters
      INTEGER   NCNFM
      PARAMETER(NCNFM=500000)
      INTEGER   IUIN,  IUOUT
      PARAMETER(IUIN=3,IUOUT=1)
      INTEGER   ITXMAX,    NTXBFM
      PARAMETER(ITXMAX=128,NTXBFM=256)

C Dummy arguments
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER            LIN

C Local variables
      INTEGER            LINB
     
      INTEGER            ITXP
      CHARACTER*(NTXBFM) TXBF

      INTEGER IERR, ITYP 
C -----------------------------

      WRITE(6,*)'enter '//AWRD(1:NWRD)//': line=',LIN
      WRITE(IUOUT,*)AWRD(1:NWRD)//': '
      WRITE(IUOUT,'(A9,I8,A1)')'line=',LIN,';'
C
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      ITXP=2
      TXBF='  '
      LINB=LIN
      DO WHILE (DLM.NE.': ')
        CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        IF (LINB.NE.LIN) THEN
          WRITE(IUOUT,*)TXBF(1:ITXP)
          IF ( TXBF(ITXP:ITXP).EQ.';'.AND.DLM.NE.': ')
     &     WRITE(IUOUT,'(A9,I8,A1)')'line=',LIN,';'
          ITXP=2
          IF (DLM.EQ.': ')ITXP=0 
          TXBF='  '
          LINB=LIN
        END IF
      END DO
      IF (ITXP.GT.2) THEN
       WRITE(IUOUT,*)TXBF(1:ITXP)
       ITXP=0
       TXBF='  '
      END IF
C
      END SUBROUTINE EXEC

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   ITEM "OPRT:" and "PARA:"
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE OPRT(NCNFLV,ICNFLV,CNFLV,NORD,IORD,AORD
     &               ,LIMN,NWRD,AWRD,DLM,LIN)
      IMPLICIT NONE 

C Parameters
      INTEGER   NCNFM,       NORDM
      PARAMETER(NCNFM=500000,NORDM=200)
      INTEGER   IUIN,  IUOUT
      PARAMETER(IUIN=3,IUOUT=1)
      INTEGER   ITXMAX,    LBLM,   NTXBFM
      PARAMETER(ITXMAX=128,LBLM=32,NTXBFM=256)

C Dummy arguments
      INTEGER         NCNFLV
      INTEGER         ICNFLV(NCNFM)
      CHARACTER*(LBLM) CNFLV(NCNFM)
C
      INTEGER          NORD
      INTEGER          IORD(NORDM)
      CHARACTER*(LBLM) AORD(NORDM)
      INTEGER          LIMN(NORDM)
C
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER            LIN

C Local variables
      INTEGER          NCNF,         NCNF1
      CHARACTER*(LBLM) ACNF(NCNFM) , ACNF1(NCNFM) ,ACNF2(NCNFM)
      INTEGER          ICNF(NCNFM), ICNF1(NCNFM)  ,ICNF2(NCNFM)

      INTEGER          NTRN
      CHARACTER*(LBLM) ATRN(NCNFM)
      INTEGER          ITRN(NCNFM)

      SAVE ACNF,ACNF1,ACNF2,ATRN,ICNF,ICNF1,ICNF2,ITRN
C
      INTEGER            LINB

      INTEGER            ITXP
      CHARACTER*(NTXBFM) TXBF, AT, AU
      INTEGER I, K, L, N, IC, IP, IERR, ITYP, IPLG, IERRCNT
C -------------------

      WRITE(6,*)'enter '//AWRD(1:NWRD)//': line=',LIN
      WRITE(IUOUT,*)AWRD(1:NWRD)//': '
C
      CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      DO WHILE(DLM.NE.': ') 
        WRITE(IUOUT,'(A9,I8,A1)')'line=',LIN,';'
        TXBF='  '
        ITXP=0
        IPLG=0
        IERRCNT=0
        IF (DLM.EQ.'= ') THEN
C
C  <command>=  
C
          ITXP=2
          TXBF='  '
          LINB=0
          DO !until '; ' appears
            CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
            LINB=LIN
            IF (DLM.EQ.'; ') EXIT 
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            IF (LINB.NE.LIN) THEN
              WRITE(IUOUT,*)TXBF(1:ITXP)
              ITXP=2
              TXBF='  '
            END IF
          END DO
          IF (ITXP.GT.2) WRITE(IUOUT,*)TXBF(1:ITXP)
        ELSE IF (NWRD.NE.0.AND.(DLM.EQ.'( '.OR.DLM.EQ.'^ ')) THEN
C
C  <command>[^<superscript>]( #<state1><config.1>[,]  
C  or  <command>(<orbit1>,
       
          IF (NWRD.NE.0) THEN
            TXBF(1:2+NWRD+1)='  '//AWRD(1:NWRD)//DLM(1:1)
          ELSE
            TXBF(1:3)='  '//DLM(1:1)
          END IF
          ITXP=ITXP+2+NWRD+1
          IF (DLM.EQ.'^ ') THEN
            DO !before '( '
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
              IF (DLM.NE.'( ' .AND. DLM.NE.'$ ') THEN
                WRITE(6,*)'Error! wrong format 1 at line=',LIN 
                STOP
              END IF
              IF (NWRD.NE.0) THEN
                TXBF(ITXP+1:ITXP+NWRD+1)=AWRD(1:NWRD)//DLM(1:1)
                ITXP=ITXP+NWRD+1
              ELSE IF (NWRD.EQ.0 .AND. DLM.EQ.'$ ') THEN
                ITXP=ITXP+1
                TXBF(ITXP:ITXP)='$'
              ELSE 
                WRITE(6,*)'Error! wrong format 2 at line=',LIN 
                STOP
              END IF
              IF ( DLM.EQ.'( ' .OR. DLM.NE.'$ ' ) EXIT 
            END DO !before '( '
          END IF
           
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          IF (DLM.EQ.'# ') THEN
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            TXBF(ITXP+1:ITXP+5)='#'//AWRD(1:1)//'$c '
            ITXP=ITXP+5
            IF ( NWRD.EQ.1 .AND. DLM.EQ.'* ' ) THEN
            ELSE
              IF (NWRD.GE.1) THEN
                DO I=1,NWRD-1
                  AWRD(I:I)=AWRD(I+1:I+1)
                END DO
                AWRD(NWRD:NWRD)=' '
              END IF
              NWRD=NWRD-1
              CALL EXPND(NWRD,AWRD,DLM,LIN,IUIN
     &                  ,NCNF,ACNF,ICNF,NORD,AORD,IORD,LIMN)
            END IF
            IF (NCNF.EQ.0) THEN
              WRITE(6,*)'Error! wrong format in the 1st configuration'
     &      //' label at line=',LIN
              STOP
            END IF
            IF (DLM.EQ.', '.OR.DLM.EQ.'  ') 
     &             CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          ELSE IF( DLM.EQ.', ' .OR. DLM.EQ.'/ ' ) THEN
            NCNF=1
            NCNF1=1
          END IF
C
C #<state2><config.2> or #<state2>$<trans.> or $<trans.>
C 
          IF (DLM.EQ.'# ') THEN
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            IF (DLM.EQ.'$ ') THEN
              IF (NWRD.NE.1) THEN
                WRITE(6,*)'Error! wrong format 4 on line=',LIN
                STOP
              END IF
              TXBF(ITXP+1:ITXP+5)='#'//AWRD(1:1)//'$e '
              ITXP=ITXP+5
            ELSE IF (DLM.EQ.'  '.OR.DLM.EQ.', ') THEN
              IF (NWRD.NE.0) THEN
                TXBF(ITXP+1:ITXP+1+NWRD+1)='#'//AWRD(1:NWRD)//' '
              ELSE
                TXBF(ITXP+1:ITXP+1+1)='# '
              END IF
              ITXP=ITXP+1+NWRD+1
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            ELSE
              WRITE(6,*)'Error! wrong format 5 on line=',LIN
              STOP
            END IF
          END IF
C
          NTRN=1
          IF (DLM.EQ.'$ ') THEN
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            IF (AWRD(1:1).EQ.' '.AND.DLM.EQ.'[ ') THEN
              CALL EXPND(NWRD,AWRD,DLM,LIN,IUIN
     &                  ,NTRN,ATRN,ITRN,NORD,AORD,IORD,LIMN)
              IPLG=1
              IF (DLM.EQ.', '.OR.DLM.EQ.'  ') 
     &          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            ELSE IF (AWRD(1:1).EQ.'c' ) THEN
              IF (NWRD.NE.0) THEN
                TXBF(ITXP-1:)=AWRD(1:NWRD)//' '
              ELSE 
                TXBF(ITXP-1:)=' '
              END IF
              ITXP=ITXP+NWRD-1 
              IPLG=0
              CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            ELSE
              WRITE(6,*)'Error! wrong format 6 on line=',LIN
              STOP
            END IF
          END IF
C
C <orbit1>[[,]<orbit2>[[,]<orbit3>[,]<orbit4>]])={<arg.1>,<arg.2>,...};
C
          DO !until '; ' appears
            IF (DLM.EQ.': ' .AND.
     &        ( AWRD(:NWRD).EQ.'exec' .OR. AWRD(:NWRD).EQ.'xend'
     &        .OR.AWRD(:NWRD).EQ.'oprt') ) EXIT 
            CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
            IF (DLM.EQ.'; ') EXIT 
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          END DO
C        
C $c -><config.1> $e -><config.2>
C $s ->left side of | in <trans.> $d -> right side of | in <trans.>
C
          AT=TXBF
          IP=ITXP
          IF (IPLG.EQ.0) THEN
            DO I=1,NCNF
              TXBF=AT
              ITXP=IP
              DO !while '$c' exist
                K=INDEX(TXBF(1:ITXP),'$c')
                IF (K.EQ.0) EXIT 
                AU=ACNF(I)(1:ICNF(I))//TXBF(K+2:)
                TXBF(K:)=AU
                ITXP=ITXP-2+ICNF(I)
              END DO
              CALL OUTPUT(ITXP,TXBF)
            END DO !I
          ELSE 
            DO L=1,NTRN
              CALL TRNS(NCNF,ACNF,ICNF,ITRN(L),ATRN(L)
     &         ,NORD,AORD,IORD,LIMN,NCNF1,ACNF1,ICNF1,ACNF2,ICNF2)
              DO I=1,NCNF1
                TXBF=AT
                ITXP=IP
                DO !while '$' exist
                  K=INDEX(TXBF(1:ITXP),'$')
                  IF (K.EQ.0) THEN
                    CALL OUTPUT(ITXP,TXBF)
                    EXIT
                  ELSE IF (TXBF(K+1:K+1).EQ.'c') THEN
                    N=ICNF1(I)
                    AWRD=ACNF1(I)(:N)
                  ELSE IF (TXBF(K+1:K+1).EQ.'e') THEN
                    N=ICNF2(I)
                    AWRD=ACNF2(I)(:N)
                  ELSE IF (TXBF(K+1:K+1).EQ.'s') THEN
                    N=INDEX(ATRN(L)(1:ITRN(L)),'|')-1
                    IF (N.GT.0) AWRD=ATRN(L)(:N)
                  ELSE IF (TXBF(K+1:K+1).EQ.'d') THEN
                    N=ITRN(L)-INDEX(ATRN(L)(1:ITRN(L)),'|')
                    IF (N.GT.0) AWRD=ATRN(L)(ITRN(L)-N+1:)
                  ELSE IF (TXBF(K+1:K+1).EQ.'n') THEN
                    N=INT(LOG10(DBLE(L)))+1
                    SELECT CASE(N)
                    CASE(1)
                      WRITE(AWRD(1:1),'(I1)')L
                    CASE(2)
                      WRITE(AWRD(1:2),'(I2)')L
                    CASE(3)
                      WRITE(AWRD(1:3),'(I3)')L
                    CASE(4)
                      WRITE(AWRD(1:4),'(I4)')L
                    CASE(5)
                      WRITE(AWRD(1:5),'(I5)')L
                    CASE(6)
                      WRITE(AWRD(1:6),'(I6)')L
                    CASE DEFAULT
                      WRITE(AWRD(1:10),'(I10)')L
                      N=10
                    END SELECT
                  ELSE
                    WRITE(6,*)'Error! unknown expr.-> $',TXBF(K+1:K+1)
                    IERRCNT=IERRCNT+1
                    IF (IERRCNT.GT.1) STOP 
                  END IF
                  IF(N.GT.0) THEN
                    AU=AWRD(1:N)//TXBF(K+2:)
                  ELSE
                    AU=TXBF(K+2:)
                  END IF
                  TXBF(K:)=AU
                  ITXP=ITXP-2+N
                END DO !while '$' exist
              END DO !I
            END DO !L
          END IF
        END IF
C       WRITE(6,*)'pass 4',TXBF(1:ITXP)
        IF (DLM.EQ.': ') RETURN 
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      END DO 
C
      END SUBROUTINE OPRT

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Xcode commands enclosed by "(" and ")"
C and the "NHM" command in the item "XCRD:" 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE XCRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      IMPLICIT NONE

C Parameters
      INTEGER IUOUT
      PARAMETER(IUOUT=1)
      INTEGER ITXMAX, NTXBFM
      PARAMETER(ITXMAX=128,NTXBFM=256)

C Dummy arguments
      INTEGER            NWRD 
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER   LIN, IUIN, IERR, ITYP


C Local variables
      INTEGER            ITXP
      CHARACTER*(NTXBFM) TXBF

      INTEGER I, LINB
C -------------------------------------

      WRITE(6,*)'enter VXCD'
      ITXP=2
      TXBF='  '
      I=0
      LINB=LIN
      DO WHILE (DLM.NE.': ') 
        IF (DLM.EQ.'( ') THEN
          DO !until I=0
            IF (DLM.EQ.'( ') THEN
              I=I+1
            ELSE IF (DLM.EQ.') ') THEN
              I=I-1
            ELSE IF (DLM.EQ.'+@'.OR.DLM.EQ.'-@') THEN
              DLM(2:2)=' '
            END IF
            CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
            IF (I.EQ.0) EXIT 
            IF (IERR.EQ.3) THEN
              WRITE(6,*)'end of file!'
              RETURN
            END IF
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            IF (LINB.NE.LIN) THEN
              WRITE(IUOUT,'(A)')TXBF(1:ITXP)
              ITXP=2
              TXBF='  '
              LINB=LIN
            END IF
          END DO !until I=0 
        ELSE IF( AWRD .EQ. 'nhm' .AND. DLM .EQ. '=' ) THEN
          CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          CALL WBUFF(TXBF,ITXP,NWRD,AWRD,DLM,ITYP)
        END IF
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
      END DO
      IF (ITXP.NE.2) WRITE(IUOUT,'(A)')TXBF(1:ITXP)
C
      END SUBROUTINE XCRD 
C
C
C
      SUBROUTINE OUTPUT(ITXP,TXBF)
      IMPLICIT NONE
 
C Parameters
      INTEGER   IUOUT
      PARAMETER(IUOUT=1)
      INTEGER   ITXMAX,    NTXBFM
      PARAMETER(ITXMAX=128,NTXBFM=256)

C Dummy arguments 
      INTEGER            ITXP
      CHARACTER*(NTXBFM) TXBF

C Local variables
      INTEGER I, IS, IE
      LOGICAL HIT
C ---------------------

      IS=1
      DO
        IE=ITXP
        IF (IE-IS+1.GE.ITXMAX-2) THEN
          IE=78+IS
          HIT=.FALSE.
          DO I=IE,IS,-1
            HIT= TXBF(I:I).EQ.','.OR.TXBF(I:I).EQ.' '
            IF (HIT) EXIT
          END DO
          IF (HIT) THEN
            WRITE(IUOUT,'(A)')TXBF(IS:I)
            IS=I+1
          ELSE
            WRITE(6,*)'Error! Too long word in the X-card.'
            WRITE(6,*)'Pre-X stoped at subroutine "OUTPUT"'
            STOP
          END IF
        ELSE
          WRITE(IUOUT,'(A)')TXBF(IS:IE)
          RETURN
        END IF
      END DO
      RETURN
      END SUBROUTINE OUTPUT
C
C
C
      SUBROUTINE TRNS(NCNF ,ACNF,ICNF,NTRN,ATRN,NORD,AORD,IORD,LIMN
     &               ,NCNF1,ACNF1,ICNF1,ACNF2,ICNF2)
      IMPLICIT NONE
C Parameters
      INTEGER   NCNFM,       NORDM
      PARAMETER(NCNFM=500000,NORDM=200)
      INTEGER   LBLM
      PARAMETER(LBLM=32)

C Dummy arguments
      INTEGER          NCNF
      CHARACTER*(LBLM) ACNF (NCNFM)
      INTEGER          ICNF(NCNFM)

      INTEGER          NTRN
      CHARACTER*(LBLM) ATRN

      INTEGER          NORD
      CHARACTER*(LBLM) AORD (NORDM)
      INTEGER          IORD(NORDM), LIMN(NORDM)

      INTEGER          NCNF1
      CHARACTER*(LBLM) ACNF1(NCNFM), ACNF2(NCNFM)
      INTEGER          ICNF1(NCNFM), ICNF2(NCNFM)

C Local variable
      INTEGER          NT, NC, NS 
      CHARACTER*(LBLM) AT, AC, AS
      INTEGER I, J, K, M, IBAR
      LOGICAL HIT
C ------------------------

      IBAR=INDEX(ATRN(1:NTRN),'|')
      IF (IBAR.EQ.0 ) THEN
        IBAR=NTRN+1
        AT=ATRN(1:NTRN)//'|'//ATRN(1:NTRN)
        ATRN=AT
        NTRN=2*NTRN+1
      END IF
      IF (NTRN.EQ.1) THEN
        NCNF1=NCNF
        DO I=1,NCNF
          ACNF1(I)=ACNF(I)
          ICNF1(I)=ICNF(I)
          ACNF2(I)=ACNF(I)
          ICNF2(I)=ICNF(I)
        END DO
        RETURN
      END IF
      NCNF1=1
C
      DO I=1,NCNF
        M=IBAR-1
        AT=ATRN(1:IBAR-1)
        NC=ICNF(I)
        AC=ACNF(I)(1:NC)//'*'
        DO WHILE (M.NE.0)
          HIT=.FALSE.
          DO K=1,NORD
            HIT= AT(:IORD(K)).EQ.AORD(K)(:IORD(K)) 
            IF (HIT) EXIT
          END DO
          IF (HIT) THEN
            K=IORD(K)
            J=INDEX(AC(1:NC),AT(1:K))
            IF (J.EQ.0) GOTO 40
            AC(J:)=AC(J+K:)
            AT(1:)=AT(1+K:)
            NC=NC-K
            M=M-K
          ELSE
            WRITE(6,*)'Error! wrong config. expr. "'//AT(1:IBAR-1)//'"'
            WRITE(6,*)' in subroutine TRNS'
            STOP
          END IF
        END DO
C
        AT=ATRN(IBAR+1:LBLM) 
        NT=NTRN-IBAR
        CALL APPND(NS,AS,NC,AC,NT,AT
     &            ,NORD,AORD,IORD,LIMN)
        IF (NS.NE.0) THEN 
          ACNF2(NCNF1)=AS
          ICNF2(NCNF1)=NS
          ACNF1(NCNF1)=ACNF(I)
          ICNF1(NCNF1)=ICNF(I)
          NCNF1=NCNF1+1
        END IF
 40     CONTINUE
      END DO !I
      NCNF1=NCNF1-1
C
      END SUBROUTINE TRNS
C
C
C
      FUNCTION ICNT(NW,AW,NC,AC)
      IMPLICIT NONE
      INTEGER ICNT
C Parameter
      INTEGER   LBLM
      PARAMETER(LBLM=32)

C Dummy arguments
      INTEGER NW,NC
      CHARACTER*(LBLM) AW, AC

C Local variables
      INTEGER I,J
C ---------- 
      ICNT=0
      J=1
      DO WHILE (J.LE.NW-NC+1) 
        I=INDEX(AW(J:NW),AC(1:NC))
        IF (I.EQ.0) RETURN 
        ICNT=ICNT+1
        J=J+NC+I-1
      END DO
      RETURN
      END FUNCTION ICNT
C
C
C
      SUBROUTINE EXPND(NWRD,AWRD,DLM,LIN,IUIN
     &                ,NCNF,ACNF,ICNF,NORD,AORD,IORD,LIMN)
      IMPLICIT NONE 

C Parameters
      INTEGER   NCNFM,       NORDM
      PARAMETER(NCNFM=500000,NORDM=200)
      INTEGER   ITXMAX,    LBLM
      PARAMETER(ITXMAX=128,LBLM=32)

C Dummy arguments
      INTEGER            NWRD
      CHARACTER*(ITXMAX) AWRD
      CHARACTER*(2)      DLM
      INTEGER            LIN, IUIN

      INTEGER          NCNF
      CHARACTER*(LBLM) ACNF(NCNFM)
      INTEGER          ICNF(NCNFM)

      INTEGER          NORD
      CHARACTER*(LBLM) AORD(NORDM)
      INTEGER          IORD(NORDM)
      INTEGER          LIMN(NORDM)

C Local variables
      CHARACTER*(LBLM) ACU, ACV

      INTEGER            NWRDB
      CHARACTER*(ITXMAX) AWRDB
      CHARACTER*(2)      DLMB
    
      INTEGER          NCT,        NCS 
      CHARACTER*(LBLM) ACT(NCNFM), ACS(NCNFM)
      INTEGER          ICT(NCNFM), ICS(NCNFM)

      SAVE ACT,ACS,ICT,ICS

      INTEGER             NARC
      INTEGER IBRA(0:500),IARC(0:500)

      INTEGER I,J,K, IP, NCV, IERR, ITYP

      LOGICAL FOUND
C-----------------

C  init
      NARC=0
      IARC(0)=0
      IBRA(0)=1
      NCNF=0
      IF (INDEX(',] #$',DLM(1:1)).NE.0) THEN
        NCNF=1
        ACNF(1)=AWRD(1:LBLM)
        ICNF(1)=NWRD
        RETURN
      END IF
      DO !main loop
        NWRDB=NWRD
        AWRDB=AWRD
        DLMB=DLM
        CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
        IF (DLM.EQ.'| ') THEN
          ACU=AWRD(1:LBLM)
          K=NWRD
          CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
          ACU(K+1:LBLM)='|'//AWRD(1:LBLM-K-1)
          AWRD=ACU(1:LBLM)
          NWRD=NWRD+K+1
        END IF
CCCC bra
        IF (DLMB.EQ.'[ ') THEN
C
          IF (NWRDB.NE.0) THEN
            NCNF=NCNF+1
            ACNF(NCNF)=AWRDB(1:LBLM)
            ICNF(NCNF)=NWRDB
          END IF
          NARC=NARC+1
          IARC(NARC)=NCNF
          IBRA(NARC)=1
CCCC ket
        ELSE IF (DLMB.EQ.'] ') THEN
C
          IF (NWRDB.NE.0) THEN
            NCNF=NCNF+1
            ACNF(NCNF)=AWRDB(1:LBLM)
            ICNF(NCNF)=NWRDB
          END IF

          DO WHILE (IBRA(NARC).EQ.0)
            NARC=NARC-1
            IF (NARC.LT.0) THEN
              WRITE(6,*)'Error! wrong format at line=',LIN
              STOP
            END IF
          END DO
          IP=IARC(NARC)
          IF (DLM.EQ.'* '.OR.DLM.EQ.'**') THEN
            DO I=IP+1,NCNF
              ACS(I-IP)=ACNF(I)
              ICS(I-IP)=ICNF(I)
            END DO
            NCS=NCNF-IARC(NARC)
          ELSE
            IF (NWRD.NE.0) THEN
              J=IP
              DO I=IP+1,NCNF
                CALL APPND (NCV,ACV,ICNF(I),ACNF(I),NWRD,AWRD
     &                     ,NORD,AORD,IORD,LIMN)
                IF (NCV.NE.0) THEN
                  J=J+1
                  ICNF(J)=NCV
                  ACNF(J)=ACV
                END IF
              END DO
              NCNF=J
              NWRD=0
            END IF
C
            IF (IARC(NARC).NE.IARC(NARC-1)) THEN

              IP=IARC(NARC)
              DO I=IP+1,NCNF
                ACS(I-IP)=ACNF(I)
                ICS(I-IP)=ICNF(I)
              END DO
              NCS=NCNF-IP

              IP=IARC(NARC-1)
              DO I=IP+1,IARC(NARC)
                ACT(I-IP)=ACNF(I)
                ICT(I-IP)=ICNF(I)
              END DO
              NCT=IARC(NARC)-IP
              NCNF=IP

              DO J=1,NCS
                DO I=1,NCT
                  CALL APPND(NCV,ACV
     &                      ,ICT(I),ACT(I),ICS(J),ACS(J)
     &                      ,NORD,AORD,IORD,LIMN)
                  IF (NCV.NE.0) THEN
                    FOUND=.FALSE.
                    DO K=IP+1,NCNF
                      FOUND= ACV.EQ.ACNF(K)
                      IF (FOUND) EXIT
                    END DO
                    IF ( .NOT. FOUND ) THEN 
                      NCNF=NCNF+1
                      ICNF(NCNF)=NCV
                      ACNF(NCNF)=ACV
                    END IF
                  END IF
                END DO
              END DO
              WRITE(6,*)'IP=',IP,' NCNF=',NCNF
              WRITE(6,*)'NCT=',NCT,' NCS=',NCS
            END IF
            NARC=NARC-1
          END IF 
CCCC ,
        ELSE IF (DLMB.EQ.', ') THEN
          IF (NWRDB.NE.0) THEN
            NCNF=NCNF+1
            ACNF(NCNF)=AWRDB(1:LBLM)
            ICNF(NCNF)=NWRDB
          END IF
          NARC=NARC+1
          IARC(NARC)=NCNF
          IBRA(NARC)=0
CCCC -
        ELSE IF (DLMB.EQ.'- ') THEN
          FOUND=.FALSE.
          DO I=1,NORD
            FOUND= AWRDB .EQ. AORD(I)(1:IORD(I))
            IF (FOUND) EXIT
          END DO
          IF ( .NOT. FOUND ) THEN
            WRITE(6,*)
     &       'Error! wrong config. expr. "'//AWRDB(1:NWRDB)//'"'
            WRITE(6,*)' in subroutine EXPND  text at line',LIN
            STOP
          END IF
          FOUND=.FALSE.
          DO J=NORD,1,-1
            FOUND = AWRD .EQ. AORD(J)(1:IORD(J))
            IF (FOUND) EXIT
          END DO
          IF ( .NOT. FOUND ) THEN
            WRITE(6,*)
     &      'Error! wrong config. expr. "'//AWRD(1:NWRD)//'"'
            WRITE(6,*)' in subroutine EXPND  text at line',LIN
            STOP
          END IF
          DO K=I,J
            ACNF(NCNF+K-I+1)=AORD(K)
            ICNF(NCNF+K-I+1)=IORD(K)
          END DO
          NWRD=0
          NCNF=NCNF+J-I+1
CCCC *
        ELSE IF (DLMB.EQ.'* '.OR.DLMB.EQ.'**') THEN

 35       IP=IARC(NARC)
          DO I=IP+1,NCNF
            ACT(I-IP)=ACNF(I)
            ICT(I-IP)=ICNF(I)
          END DO
          NCT=NCNF-IP

          NCNF=IP
          DO J=1,NCS
            DO I=1,NCT
              CALL APPND(NCV,ACV
     &                  ,ICT(I),ACT(I),ICS(J),ACS(J)
     &                  ,NORD,AORD,IORD,LIMN)
              IF (NCV.NE.0) THEN
                FOUND=.FALSE.
                DO K=IP+1,NCNF
                  FOUND= ACV .EQ. ACNF(K)
                  IF (FOUND) EXIT 
                END DO
                IF ( .NOT. FOUND ) THEN
                  NCNF=NCNF+1
                  ICNF(NCNF)=NCV
                  ACNF(NCNF)=ACV
                END IF
              END IF
            END DO
          END DO
          WRITE(6,*)'IP=',IP,' NCNF=',NCNF
          WRITE(6,*)'NCT=',NCT,' NCS=',NCS

          IF (DLMB.EQ.'**') THEN
            DLMB='* '
            GOTO 35
          END IF
          IF (DLM.EQ.'* '.OR.DLM.EQ.'**') THEN 
            NWRDB=NWRD
            AWRDB=AWRD
            DLMB=DLM
            CALL GTWRD(NWRD,AWRD,DLM,LIN,IUIN,IERR,ITYP)
            GOTO 35
          END IF

          DO WHILE (IBRA(NARC).EQ.0)
            NARC=NARC-1
            IF (NARC.LT.0) THEN
              WRITE(6,*)'Error! wrong format at line=',LIN
              STOP
            END IF
          END DO

          IF (NWRD.NE.0) THEN
            J=IARC(NARC)
            DO I=IP+1,NCNF
              CALL APPND (NCV,ACV,ICNF(I),ACNF(I),NWRD,AWRD
     &                   ,NORD,AORD,IORD,LIMN)
              IF (NCV.NE.0) THEN
                J=J+1
                ICNF(J)=NCV
                ACNF(J)=ACV
              END IF
            END DO
            NCNF=J
            NWRD=0
          END IF

          IF (IARC(NARC).NE.IARC(NARC-1)) THEN
            IP=IARC(NARC)
            DO I=IP+1,NCNF
              ACS(I-IP)=ACNF(I)
              ICS(I-IP)=ICNF(I)
            END DO
            NCS=NCNF-IP
            IP=IARC(NARC-1)
            DO I=IP+1,IARC(NARC)
              ACT(I-IP)=ACNF(I)
              ICT(I-IP)=ICNF(I)
            END DO
            NCT=IARC(NARC)-IP
            NCNF=IP
            DO J=1,NCS
              DO I=1,NCT
                CALL APPND(NCV,ACV
     &                    ,ICT(I),ACT(I),ICS(J),ACS(J)
     &                    ,NORD,AORD,IORD,LIMN)
                IF (NCV.NE.0) THEN
                  FOUND=.FALSE.
                  DO K=IP+1,NCNF
                    FOUND= ACV.EQ.ACNF(K)
                    IF (FOUND) EXIT
                  END DO
                  IF ( .NOT. FOUND ) THEN
                    NCNF=NCNF+1
                    ICNF(NCNF)=NCV
                    ACNF(NCNF)=ACV
                  END IF
                END IF
              END DO
            END DO
            WRITE(6,*)'IP=',IP,' NCNF=',NCNF
            WRITE(6,*)'NCT=',NCT,' NCS=',NCS
          END IF
          NARC=NARC-1
CCCCCCCCCCCCCCCCCCCCCCCCCCC
        ELSE
          WRITE(6,*)'Error! wrong syntax in "EXPND" on line=',LIN
          STOP
        END IF
        IF (INDEX(',] #$',DLM(1:1)).NE.0) THEN
          IF (NARC.EQ.0) RETURN
        END IF
      END DO !main loop

      END SUBROUTINE EXPND
C
C
C
      SUBROUTINE APPND(NW,AW,NW1,AW1,NW2,AW2
     &,NORD,AORD,IORD,LIMN)
      IMPLICIT NONE 

C Parameters
      INTEGER   LBLM
      PARAMETER(LBLM=32)
      INTEGER   NORDM 
      PARAMETER(NORDM=200)

C Dummy arguments
      INTEGER          NW, NW1, NW2
      CHARACTER*(LBLM) AW, AW1, AW2
 
      INTEGER          NORD
      CHARACTER*(LBLM) AORD(NORDM)
      INTEGER          IORD(NORDM)
      INTEGER          LIMN(NORDM)

C Local variables
      CHARACTER*(LBLM) AT
      INTEGER I,J,K,K1,K2, IE2, NAT
C
      IF (NW1*NW2.EQ.0) THEN
        IF (NW1.NE.0) THEN
          AW=AW1
          NW=NW1
          RETURN
        ELSE IF (NW2.NE.0) THEN
          AW=AW2
          NW=NW2
          RETURN
        ELSE 
          WRITE(6,*)'Error! append two null config. expr. !?' 
          STOP
        END IF
      END IF
C      
      AW=AW1(1:NW1)//AW2(1:NW2) 
      NW=NW1+NW2
      K1=INDEX(AW1,'|')
      K2=0
      IE2=INDEX(AW2(1:NW2)//'|','|')-1
      K=K1
      DO I=1,NORD
        J=0
        AT =AORD(I)
        NAT=IORD(I) 
        DO WHILE (K1 .LT. NW1 .AND. AW1(K1+1:K1+NAT) .EQ. AT(1:NAT) )
          AW(K+1:K+NAT)=AT(1:NAT)
          K1=K1+NAT
          K =K +NAT
          J=J+1
        END DO
        DO WHILE ( K2 .LT. IE2 .AND. AW2(K2+1:K2+NAT) .EQ. AT(1:NAT) ) 
          AW(K+1:K+NAT)=AT(1:NAT)
          K2=K2+NAT
          K =K +NAT
          J=J+1
        END DO
        IF (J.GT.LIMN(I)) THEN
          NW=0 
          RETURN
        END IF
      END DO
      IF (K.NE.NW1+IE2) THEN
        WRITE(6,*)
     &    'Error! Wrong config. expr. "'//AW(1:MAX(IE2,1))//'"'
        WRITE(6,*)' in function APPND'
        STOP
      END IF

      END SUBROUTINE APPND
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     
C   get a word and delimiter from text  (for "Xcode" interpreter)
C
C               coded by Arata Tanaka 1993.08 (at Hiroshima Univ.)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  INPUT VALUES 
C     IUNIT:   unit number of a device for input file 
C
C  OUTPUT VALUES  
C      AWRD:   a word    
C      NWRD:   size of the word
C      DLM :   delimiter
C                 one character type 
C                    ! # $ % & ( ) * , / : ; < = > ? @ [ ] ^ { | }~
C                 two character type
C                    (these are defined in variable "OP2")
C                   += -= *= /= %= ?? || && == >= <= != ** ++ --
C                 only space(s) 
C      ITYP:   type of the word     
C                  1 ---- array of character between "
C                  2 ---- numeral (integer)
C                  3 ---- numeral (floting point)
C                  4 ---- symbol started from alphabet (_ is also included)
C                  5 ---- others 
C      LIN :   line number of text
C      IERR:   error value
C                  0 ---- normal end
C                  3 ---- end of file
C
C  PARAMETERS
C      
C     CMT:     characters after this delimiter are comments (default //)
C     ESC:     escape character of "                        (default @ )
C                        
      SUBROUTINE GTWRD(NWRD,AWRD,DLM,LIN,IUNIT,IERR,ITYP)
      PARAMETER(ITXMAX=128)
      DIMENSION IATMT(15,6)
      REAL*8 DNUM2
C
      CHARACTER*(ITXMAX) TXT, AWRD, UWRD
      CHARACTER*(2) DLM, UDLM, CMT
      CHARACTER*(128) CHR, OP2
      CHARACTER*(1) ESC, AT
C
      SAVE TXT,ITXP,LIN0,IUNIT0,UWRD,UDLM
      SAVE CHR,IOPR,ISGN,INUM,IALF,IDQT,NCHR
C
C       sp1 nm1 nm2 nm3 nm4 nm5 nm6 nme sym sp2 op1 op2 ca1 ca2 ca3
C         1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
      DATA IATMT
     &  / 1, 16, 10, 10,  8,  8, 10, 17, 10, 10, 16, 16, 13, 16, 10
     &  ,11, 12, 11, 11,  8,  8, 11, 17, 11, 11, 12, 16, 13, 16, 11
     &  , 2, 12, 11, 11,  6,  6, 11, 11, 11, 11, 12, 16, 13, 16, 11
     &  , 3,  3,  3,  4,  7,  7,  7,  9,  9, 16, 16, 16, 13, 16, 16
     &  , 9, 16,  8,  8,  8,  8,  8, 17,  9, 16, 16, 16, 13, 16, 16
     &  ,13, 16, 16,  8,  8,  8,  8, 17, 16, 16, 16, 16, 15, 16, 16 / 
C
      DATA  OP2/'+= -= *= /= %= ?? || && == >= <= != ** ++ --'/
      DATA  CMT/'//'/,ESC/'\\'/
      DATA  IUNIT0/0/
C
C init
C
c      write(6,*)'enter GTWRD'
      IF (IUNIT.NE.IUNIT0) THEN
        CHR=' !#$%&()*,/:;<=>?@[]^{|}~\\+-0123456789.'
     &    //'_abcdefghijklmnopqrstuvwxyz'
     &    //'ABCDEFGHIJKLMNOPQRSTUVWXYZ"'
        IOPR=2
        ISGN=INDEX(CHR,'+')
        INUM=INDEX(CHR,'0')
        IALF=INDEX(CHR,'_')
        IDQT=INDEX(CHR,'"')
        NCHR=INDEX(CHR,'"')
        IUNIT0=IUNIT
        LIN0=0
        UDLM='  '
        UWRD=' '
C
        IERR=0
        AWRD=' '
        NWRD=0
        DLM='  '
        ISTS=1
        ITYP=0
        GOTO 20
      END IF
C
C   UNDO
C
c     IF (IMODE.NE.0) THEN
c       AWRD=UWRD
c       DLM=UDLM
c       RETURN
c     END IF
C
C main
C
      IERR=0
      AWRD='                            '
      NWRD=0
      DLM='  '
      ISTS=1
      ITYP=0
C
 5    DO I=ITXP,ITXMAX
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
C
        IF (AT.EQ.'.'.AND.(ISTS.EQ.3.OR.ISTS.EQ.2)) THEN
          ISTS=4
        ELSE IF (INDEX('dDeE',AT).NE.0 
     &  .AND.ISTS.GE.3.AND.ISTS.LE.4)THEN
          ISTS=5 
        ELSE 
          ISTS=IATMT(ISTS,ICLS)
        END IF
C
        IF (ISTS.EQ.2) THEN
          IF (INDEX('  e) e] e} ',UDLM).NE.0)THEN 
            DLM=AT//' '
            ISTS=11
          ELSE
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
          UWRD=AWRD
          UDLM=DLM
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
          IF (NWRD.EQ.0) AWRD=CHR(K:K)
          IF (NWRD.GE.1) AWRD=AWRD(:NWRD)//CHR(K:K)
          NWRD=NWRD+1
        ELSE IF (ISTS.EQ.13) THEN 
          ITYP=1
          IF (AT.NE.'"') THEN
            IF (NWRD.EQ.0) AWRD=AT
            IF (NWRD.GE.1) AWRD=AWRD(:NWRD)//AT
            NWRD=NWRD+1
          END IF
        ELSE IF (ISTS.EQ.14) THEN
          IF (NWRD.EQ.1) AWRD=AT
          IF (NWRD.GE.1) AWRD=AWRD(:NWRD)//AT
          ISTS=13
        ELSE IF (ISTS.EQ.15) THEN
          IF (AT.EQ.'"'.AND.NWRD.NE.0) THEN
            IF (AWRD(NWRD:NWRD).EQ.ESC) THEN
              ISTS=13
              AWRD(NWRD:NWRD)='"'
            END IF
          END IF
        ELSE IF (ISTS.EQ.16) THEN
          UWRD=AWRD
          UDLM=DLM
          ITXP=I
          RETURN
        ELSE IF (ISTS.EQ.17) THEN
          WRITE(6,*)'"GTWRD" wrong syntax'
          IERR=1
          RETURN
        END IF
      END DO !I
C
C    read a line from text
C
 20   ITXP=1
      LIN0=LIN0+1
      READ(IUNIT,'(A128)',END=100)TXT
c      WRITE(2,1000)TXT
C   skip a line number
C6    K=INDEX(CHR,TXT(ITXP:ITXP))
C     IF (K.LT.INUM.OR.K.GE.IALF) GOTO 30
C     ITXP=ITXP+1
C     GOTO 6
C 30   CONTINUNE 
      IF (ITXP.EQ.1) THEN
        LIN=LIN0
      ELSE
        LIN=NINT(DNUM2(ITXP-1,TXT(:ITXP-1),IERR))
      END IF
      GOTO 5
C
 100  IERR=3
      UWRD=AWRD
      UDLM=DLM
      RETURN
      END
C ==================================================================
C character to numeral
C ==================================================================
      FUNCTION DNUM2(NWRD,AWRD,IT)
      IMPLICIT NONE 

      REAL*8 DNUM2

C Dummy arguments
      INTEGER        NWRD 
      CHARACTER*(32) AWRD
      INTEGER        IT

C Local variables
      INTEGER IATMT(4,0:7)
      CHARACTER*(32) CHR
      INTEGER I, K, ICLS, ISTS
      REAL*8 SF, SE, FL, EX, FP
      SAVE IATMT
      DATA  IATMT/1,3,8,2
     &           ,8,3,8,2
     &           ,8,3,5,2
     &           ,8,8,5,4
     &           ,8,8,5,4
     &           ,6,8,8,7
     &           ,8,8,8,7
     &           ,8,8,8,7/

C --------
      CHR=' +-.edED0123456789'
      ISTS=0
      SF=1D0
      SE=1D0
      FL=0D0
      EX=0D0
      FP=0D0
      IT=1
      DO I=1,NWRD
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
          FL=FL*10D0 + DBLE(K-9)
        ELSE IF (ISTS.EQ.4) THEN
          FP=FP+1D0
          FL=FL*10D0 + DBLE(K-9)
        ELSE IF (ISTS.EQ.6) THEN
          IF (K.EQ.3) SE=-1D0
        ELSE IF (ISTS.EQ.7) THEN
          EX=EX*10D0 + DBLE(K-9)
        ELSE IF (ISTS.EQ.8) THEN
          WRITE(6,*)'irregular description in "DNUM2"'
          IT=-1
        END IF
 10     CONTINUE
      END DO !I
 20   IF (ISTS.LE.1.OR.ISTS.EQ.5.OR.ISTS.EQ.6) THEN
        WRITE(6,*)'irregular description  in "DNUM2"'
        IT=-1
      END IF
      DNUM2=SF*FL*10D0**(SE*EX-FP)
      RETURN
      END

