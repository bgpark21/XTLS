# -----------------------------------------------------------------
# A configuration of the source code of the Xcode version 2.3
# interpreter to combine the interpreter with other program
# and make it work as a part of command interpreter of the system.
# The modification to the source can be applied such as
#  sed -f xclt230.sed XcodeV230.f > XcodeV230mod.f
#
# Last update: 2009.10.1
# -----------------------------------------------------------------
# For detailed explanations of the parameters below, see the comments 
# in the source file "XcodeV230.f".
#
s/NRVALM=[0-9][0-9]*/NRVALM=100000/g
s/NRVALX=[0-9][0-9]*/NRVALX=95000/g
  s/ICDM=[0-9][0-9]*/ICDM=10000/g
s/NVARM=[0-9][0-9]*/NVARM=1000/g
s/NCNSM=[0-9][0-9]*/NCNSM=5000/g
s/NFNCM=[0-9][0-9]*/NFNCM=100/g
 s/NSYS=[0-9][0-9]*/NSYS=2000/g
 s/NCLS=[0-9][0-9]*/NCLS=1000/g

# ==========================================================
# Modification for NAGWare f95 fortran and GNU gfortran
# Comment out below, for g77 and old Intel fortran compilers
# Either works for Intel fortran ver.10
 s/'(A,$)'/'(A)',ADVANCE='no'/g
#
# ==========================================================
# If you want Xcode as a stand-alone system, comment out
# the line with "START MAIN" and following two lines.
# If Xcode must be compiled with another program as a subsystem,
# then comment out the line with "START SUBSYSTEM" only.
#
/>>>START MAIN/,/>>>END MAIN/d
s/.*WRITE(6,\*)' *at the line=',LIN.*//
s/at the line=',.*LIN.*/ '/
#/>>>START SUBSYSTEM/,/>>>END SUBSYSTEM/d
#
# ==========================================================
# If you need to use file input/output functions such as
# read, write, include open and close, then comment out
# the line with "START IOFNC".
#
# If you have made extentional build-in functions
# in the subroutine EXFNC, comment out the line with
# "START EXFNC".
#
/>>>START IOFNC/,/>>>END IOFNC/d
/>>>START EXFNC/,/>>>END EXFNC/d
#
# ==========================================================
# If you want use Xcode non-interactive way, e.g.,
# embedded in a shell script, comment out the two lines below.
#
#/>>>START SILENT/,/>>>END SILENT/d
#s?IBREAK/0/?IBREAK/1/?


