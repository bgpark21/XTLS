#
#  Makefile for Xtls 
#  (last modification 2009.10.3) 
#
#  Five executable files xtls900, pre-X15, x900, xc and xcs  
#  are created by default in the directory "bin" in your
#  home directory.
#
#  You can describe electronic configurations, Hamiltonians, 
#  the photoprocess and conditions of spectra, and parameters
#  needed for the calculations in a file with a special form 
#  called "X-card" as input for Xtls. 
#  Examples of them can be found in the directory Xtls/xcards/.
#  
#  x900 is a c-shell script for initiating and scheduling
#  the calculations of spectra described by "X-card" files.
#  The executables xtls900 and pre-X15 are invoked automatically
#  by this command.
#   
#  xtls900 is the main program and do actual calculations
#  of spectra discribed by the X-card files.
#
#  pre-X15 is a preprocessor of the X-card, which permit 
#  using multi-configuration expressions (a kind of wildcards) 
#  in the X-card. 
#
#  xc is the interpreter of X-code the programming languge.
#  The output data of Xtls can be processed by the programs
#  written with X-code found in the directory Xtls/xc/spc/.
#
#  xcs is the same to xc but for non-interactive use
#  (such as embedded in shell scripts).
#
# =====================================================
# Set a directory where you wish to put executable files
BINDIR = $(HOME)/bin
# =====================================================
# For Fortran90 or later compilers set "F90" below, 
# For FORTRAN77 compilers, character other than above
# (for instance "G") will do. In the case of FORTRAN77 
# compilation, if you set "G", correspoding configuration 
# file is x900G.sed and the executable files created are 
# named xtls900G and x900G. If you are working on different 
# kinds of spectroscopies or models at the same time, having 
# executable files compiled with different sets of array 
# sizes is very convenient (since their size cannot be
# changed after compilation with FORTRAN77). 
#MODE= G
# =====================================================
# Version of Xcode interpreter
#    Ver. 2.0 for Xtls ver.8.3 or before
#    Ver. 2.3 for Xtls ver.8.4 or later 
VERXCODE= 230
# =====================================================
# Version of Xtls 
VERXTLS= 900
# =====================================================
# Version of pre-X preprocessor for Xtls
#    Ver. 1.2 for Xtls ver.8.3 or before
#    Ver. 1.3 for Xtls ver.8.4 
#    Ver. 1.4 for Xtls ver.8.5
#    Ver. 1.5 for Xtls ver.8.6 or later 
VERPREX= 15
#=========================================================
# Specify a directory where postscript libraries must be placed.
XCODELIB = `pwd`/../lib/
# =====================================================
# Shell script to run jobs  
# 
SCRIPT= x9.csh
# =====================================================
# Specify a fortran compiler and its options which you 
# want to build with. 
#   FC: command name of the fortran compiler and options for optimization 
#  FRM: free format option of the compiler
# Examples are listed below.
# ----------------------------------------------------- 
# GNU gfortran / Mac OS X / Intel Core 2 Duo 2.16GHz
#FC =/usr/bin/gfortran -O3 -W -Wall -pedantic -march=nocona -mtune=nocona -funroll-loops -m64
#FC =/usr/bin/gfortran -O3 -W -Wall -pedantic -march=core2 -mtune=core2 -funroll-loops -m64
#FRM =-ffree-form
# -----------------------------------------------------
# Intel Fortran for ESLab
FC =ifort -O2 -xSSE2 -axSSE2 -funroll-loops -static -vec -vec-report1
FRM =-free
# ----------------------------------------------------- 
# GNU gfortran / Mac OS X / PowerPC G5 2.5GHz
# FC =/usr/local/bin/gfortran -O3 -W -Wall -pedantic -falign-labels=16 -falign-loops=16 -falign-jumps=16 -falign-functions=16  -mdynamic-no-pic -mcpu=970 -mtune=970 -mpowerpc64 -mpowerpc-gpopt -funroll-loops -m64 
# FRM =-ffree-form
# ----------------------------------------------------- 
# Intel fortran 10.1
# FC =ifort -O2 -xT -ipo
# FRM =-free
# ----------------------------------------------------- 
# Intel fortran / RedHat linux / Intel Xeon 3.06 GHz
# FC =/opt/intel/compiler70/ia32/bin/ifc -O3 -tpp7 -xW -pad -ipo 
# FRM =-FR
# ----------------------------------------------------- 
# PGI fortran / SUSE linux 9.0 / AMD Opteron 2.2GHz
# FC =pgf95 -fastsse -mcmodel=medium -Mipa=fast -Minfo 
# FRM =-Mfree
# ----------------------------------------------------- 
# PGI fortran / RedHat linux / Intel Xeon 3.06GHz
# FC =pgf90 -tpp7 -fastsse -Wl,-Bstatic -Minfo 
# ----------------------------------------------------- 
# IBM XLF fortran / Mac OS X / PowerPC G5 2.5GHz
# FC =xlf -O3 -qarch=ppc970 -qcache=auto -qhot -qzerosize
# FRM =-qfree
# ----------------------------------------------------- 
# NAGWare f95 / Mac OS X / PowerPC G5 2.5GHz
# FC =/usr/local/bin/f95 -O3 -ieee=full -save 
# ----------------------------------------------------- 
# Hitach SR8000 super computer 
# FC =xf90 -64 -W0,'OPT(O(ss)),PVEC(DIAG(2),PVFUNC(1)),MP(P(4),PROCNUM(8))'
# ----------------------------------------------------- 
# Hitach SR11000 super computer 
# FC =xf90 -64 -Oss 
# ----------------------------------------------------- 
# GNU g77 / Mac OS X / PowerPC G5 dual 2.5GHz
# FC =g77 -O3 -falign-loops=16 -falign-jumps=16 -falign-functions=16 -malign-natural -ffast-math -funroll-loops -mcpu=G5 -mpowerpc-gpopt -mtune=G5 -mpowerpc64 -fprefetch-loop-arrays
# FRM =-ffree-form
# ----------------------------------------------------- 
# GNU g77 / SUSE linux 9.0 / AMD Opteron 2.2GHz
# FC =g77 -O3 -msse2 -m3dnow -funroll-loops 
# FRM =-ffree-form
# ----------------------------------------------------- 
# GNU g77 / Mac OS X / PowerPC G4 800MHz
# FC=g77 -O3 -falign-loops=16 -falign-jumps=16 -falign-functions=16 -malign-natural -ffast-math -funroll-loops -mcpu=7450 -mpowerpc-gpopt -mtune=G4 -fsched-interblock -fgcse-sm -fprefetch-loop-arrays
# FRM =-ffree-form
# ----------------------------------------------------- 
# Compaq fortran / Kondara MNU/linux 2000 / Alpha 21264(EV6) 667MHz 
# FC =fort -fast -tune host -arch host -O4
# FRM =-free
# ----------------------------------------------------- 
# HP
# FC =f77 -O -K 
# -----------------------------------------------------
BINDIR = /opt/xtls900/bin
# =====================================================
# Do not change below unless you know what you are doing.

all:clean xtls$(VERXTLS) pre-X$(VERPREX) xc
XcodeV2xx.o:XcodeV$(VERXCODE).f 
	test -d "$(BINDIR)" || mkdir "$(BINDIR)" || exit 1
	sed s%xcs%$(BINDIR)/xcs% f77to90.sh > f77to90
	chmod u+x f77to90
	if ! test -e "$(BINDIR)/xcs" ; then \
	  if test $(MODE) = F90  ; then \
	    sed -f xcs$(VERXCODE).sed XcodeV$(VERXCODE).f90 | sed s%/home/tanaka/Xcode/lib/%"$(XCODELIB)"% >XcodeV2xx.f ;\
	    $(FC) $(FRM) XcodeV2xx.f -o "$(BINDIR)"/xcs ;\
	  else \
	    sed -f xcs$(VERXCODE).sed XcodeV$(VERXCODE).f | sed s%/home/tanaka/Xcode/lib/%"$(XCODELIB)"% >XcodeV2xx.f ;\
	    $(FC) XcodeV2xx.f -o "$(BINDIR)"/xcs ;\
	  fi \
	fi 
	if test $(MODE) = F90  ; then \
	  sed -f xclt$(VERXCODE).sed XcodeV$(VERXCODE).f90 >XcodeV2xx.f ;\
	  $(FC) $(FRM) -c XcodeV2xx.f ;\
	else \
	  sed -f xclt$(VERXCODE).sed XcodeV$(VERXCODE).f >XcodeV2xx.f ;\
	  $(FC) -c XcodeV2xx.f ;\
	fi
xtls$(VERXTLS):XcodeV2xx.o
	test -d "$(BINDIR)" || mkdir "$(BINDIR)" || exit 1
	if test $(MODE) = F90  ; then \
	  sed -f x$(VERXTLS).sed xtls$(VERXTLS).f >xtls9xx.f ;\
	  ./f77to90 xtls9xx.f ;\
	  sed "/>>>START F90/,/>>>END F90/s/^\!//" xtls9xx.f90 > xtls9xx.f ;\
	  $(FC) $(FRM) xtls9xx.f XcodeV2xx.o -o "$(BINDIR)"/xtls$(VERXTLS) ;\
	else \
	  sed -f x$(VERXTLS)$(MODE).sed xtls$(VERXTLS).f >xtls9xx.f ;\
	  $(FC) xtls9xx.f XcodeV2xx.o -o "$(BINDIR)"/xtls$(VERXTLS)$(MODE) ;\
	fi
	if test $(MODE) = F90  ; then \
	  sed s/xtls9/xtls$(VERXTLS)/ $(SCRIPT) | sed s/pre-X/pre-X$(VERPREX)/ > "$(BINDIR)"/x$(VERXTLS) \
	  || exit 1 ;\
	  chmod u+x "$(BINDIR)"/x$(VERXTLS) ;\
	else \
	  sed s/xtls9/xtls$(VERXTLS)$(MODE)/ $(SCRIPT) | sed s/pre-X/pre-X$(VERPREX)/ > "$(BINDIR)"/x$(VERXTLS)$(MODE) || exit 1 ;\
	  chmod u+x "$(BINDIR)"/x$(VERXTLS)$(MODE) ;\
	fi
xc:
	if ! test -e "$(BINDIR)/xc" ; then \
	  test -d "$(BINDIR)" || mkdir "$(BINDIR)" || exit 1 ;\
	  if test $(MODE) = F90  ; then \
	    sed -f xc$(VERXCODE).sed XcodeV$(VERXCODE).f90 | sed s%/home/tanaka/Xcode/lib/%"$(XCODELIB)"% >XcodeV2xx.f ;\
	    $(FC) $(FRM) XcodeV2xx.f -o "$(BINDIR)"/xc ;\
	  else \
	    sed -f xc$(VERXCODE).sed XcodeV$(VERXCODE).f | sed s%/home/tanaka/Xcode/lib/%"$(XCODELIB)"% >XcodeV2xx.f ;\
	    $(FC) XcodeV2xx.f -o "$(BINDIR)"/xc ;\
	  fi \
	fi
pre-X$(VERPREX):
	test -d "$(BINDIR)" || mkdir "$(BINDIR)" || exit 1
	if test $(MODE) = F90  ; then \
	  ./f77to90 pre-X$(VERPREX).f ;\
	  mv pre-X$(VERPREX).f90 pre-X1x.f ;\
	  $(FC) $(FRM) pre-X1x.f -o "$(BINDIR)"/pre-X$(VERPREX) ;\
	else \
	  $(FC) pre-X$(VERPREX).f -o "$(BINDIR)"/pre-X$(VERPREX) ;\
	fi
retxcards:
	cp retxcards.csh "$(BINDIR)"/retxcards
	chmod u+x "$(BINDIR)"/retxcards
cleanxobjs:
	cp cleanxobjs.csh "$(BINDIR)"/cleanxobjs
	chmod u+x "$(BINDIR)"/cleanxobjs
xtlssize:
	cp xtlssize$(VERXTLS).csh "$(BINDIR)"/xtlssize
	chmod u+x "$(BINDIR)"/xtlssize
test:
	if test $(MODE) = F90  ; then \
	(cd ../test/xcards/; "$(BINDIR)"/x$(VERXTLS) Ni2+L_2pXPS; cd ../xcodes/; $(BINDIR)/xc <spc.x; cd ../../src/) ;\
	else \
	(cd ../test/xcards/; "$(BINDIR)"/x$(VERXTLS)$(MODE) Ni2+L_2pXPS; cd ../xcodes/; $(BINDIR)/xc <spc.x; cd ../../src/) ;\
	fi
clean:
	rm -f XcodeV2xx* pre-X*.o xtls9xx* pre-X1x*
