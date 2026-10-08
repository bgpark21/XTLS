#=================================================
# The sizes of arrays in the source file xtls900.f
# (settings for the FORTRAN77 compiler).
# 
# If there are matrix elements with complex numbers,
# required values for the array sizes NBSM, NHDM, NBGM, 
# NBMM, NBFM NALFM and NMAXR are doubled.
# Matrix elements with complex numbers can be appeared
# when you use command such as "CAk", "rAk", "r10Dq", 
# "Ha", "Ba" and "Hyb" in your X-card.
#
# NBSM:  max. number of basis functions for all
#        (the initial, final and intermediate) states
# NBGM:  max. number of basis functions for the initial state
# NBMM:  max. number of basis functions for the intermediate state
# NHDM:  The value of this must be the same to NMAX for the normal mode,
#        but if you specify the EXL mode by "Mode" command 
#        in your X-card, the value should be the same to that
#        of NCIM.
# NBFM:  max. number of basis functions for the final state 
#          (only for RPESG or RPESFG mode)
#
# NGM:   max. number of the initial state.
# NHM:   max. size of matrices data storage.
# NOBTM: max. number of orbitals
# NSTM:  max. number of states
# NCIM:  max. number of the electron configurations
# NRECM: number of iteration for the recursion method
# NPMAX: max. number of sampling points for a spectrum
# NLBLM: max. length of configuration labels
#
# NALFM: max. number of reduced basis (only for DMRG mode)
# NMAXR: max. number of DM basis (only for DMRG mode) 
#------------------------------------------------
  s/NBSM=[0-9][0-9]*/NBSM=500001/
  s/NBGM=[0-9][0-9]*/NBGM=100001/
  s/NBMM=[0-9][0-9]*/NBMM=1/
  s/NHDM=[0-9][0-9]*/NHDM=500001/
  s/NBFM=[0-9][0-9]*/NBFM=1/
  s/NGM=[0-9][0-9]*/NGM=20/
  s/NHM=[0-9][0-9]*/NHM=5000001/
  s/NOBTM=[0-9][0-9]*/NOBTM=120/
  s/NSTM=[0-9][0-9]*/NSTM=3/
  s/NCIM=[0-9][0-9]*/NCIM=13000/
  s/NLBLM=[0-9][0-9]*/NLBLM=10/
  s/NRECM=[0-9][0-9]*/NRECM=2000/
  s/NPMAX=[0-9][0-9]*/NPMAX=4000/
# For DMRG mode
  s/NMAXR=[0-9][0-9]*/NMAXR=1/
  s/NALFM=[0-9][0-9]*/NALFM=1/
# For EXEIG mode
  s/NMAXE=[0-9][0-9]*/NMAXE=1/
#=================================================
#/>>>START RESONANCE/,/>>>END RESONANCE/d
#/>>>START SPECIAL/,/>>>END SPECIAL/d
#=================================================
# for cray super computer
#------------------------------------------------
#s/EPS_BS=1D-[0-9][0-9]*/EPS_BS=1D-14/
#s/DCOS(/COS(/g
#s/DSIN(/SIN(/g
#s/DEXP(/EXP(/g
#s/DSIGN(/SIGN(/g
#s/DSQRT(/SQRT(/g
#s/DABS(/ABS(/g
#s/DATAN2(/ATAN2(/g
#s/DATAN(/ATAN(/g
#s/DBLE(/REAL(/g
#s/\(=.*\)FRAND(/\1RANF(/g
#s/\([\.1234567890]\)D\([-+1234567890]\)/\1E\2/g
#=================================================
# Vector-Parallel or Scalar machine
# Choose one of the lines below and put "#" at the begining 
# of the line. 
#------------------------------------------------
#/>>>START SCALAR/,/>>>END SCALAR/d
/>>>START VECTOR/,/>>>END VECTOR/d
#=================================================
# For FORTRAN77 compilers put "#" at the begining
# of the first line below and remove "#" for the next line.
#------------------------------------------------
#/>>>START F77/,/>>>END F77/d
s/^ *INTEGER *NELC(NOBT,NCIM,NSTM)/      INTEGER NELC(NOBTM,NCIM,NSTM)/
#=================================================
# Function to obtain CPU time depends on the compiler.
# Choose one of the lines below and put "#" at the begining 
# of the line. 
# 
#  GNU   ---- for GNU g77, IBM XLF, NAGWare, Compaq compilers
#  INTEL ---- for INTEL compiler
#  PGI ------ for PGI or Compaq compilers 
#  SR8000 ----for Hitachi SR8000 super computer 
#  VPP    ----for Fujitu VPP super computer 
#  UNKNOWN ---in case you can not identify which
#             line is suited to your system, choose
#             this line.
#------------------------------------------------
#/>>>START GNU/,/>>>END GNU/d
/>>>START INTEL/,/>>>END INTEL/d
/>>>START PGI/,/>>>END PGI/d
/>>>START SR8000/,/>>>END SR8000/d
/>>>START VPP/,/>>>END VPP/d
/>>>START CRAY/,/>>>END CRAY/d
/>>>START HP9000/,/>>>END HP9000/d
/>>>START RS6000/,/>>>END RS6000/d
/>>>START UNKNOWN/,/>>>END UNKNOWN/d


