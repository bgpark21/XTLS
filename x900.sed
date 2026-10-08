#=================================================
# The sizes of arrays in the source file xtls900.f
# (settings for the FORTRAN90 or later compiler).
#  
# NHM:   max. size of matrices data storage (default value)
# NOBTM: max. number of orbitals
# NSTM2: max. number of states
# NRECM: number of iteration for the recursion method
# NPMAX: max. number of sampling points for a spectrum
# NLBLM: max. length of the configuration label
#
# NALFM: max. number of reduced basis (only for DMRG mode)
# NMAXR: max. number of DM basis (only for DMRG mode) 
#------------------------------------------------
  s/NHM=[0-9][0-9]*/NHM=5000000/
  s/NOBTM=[0-9][0-9]*/NOBTM=120/
  s/NSTM2=[0-9][0-9]*/NSTM2=20/
  s/NRECM=[0-9][0-9]*/NRECM=4000/
  s/NPMAX=[0-9][0-9]*/NPMAX=4000/
  s/NLBLM=[0-9][0-9]*/NLBLM=16/
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
/>>>START F77/,/>>>END F77/d
#s/^ *INTEGER *NELC(NOBT,NCIM,NSTM)/      INTEGER NELC(NOBTM,NCIM,NSTM)/
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


