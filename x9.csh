#!/bin/csh  
if ( $1 == "--help" ) then
cat <<CAT
  This cshell script file is for initiating and scheduling
  the calculations of the spectra described by the X-card files. 

  The directories "xcards", "xobjs", "xcodes" and "xwrk" must be made 
  within the same directory before using this command and X-card files 
  are required to be placed in "xcards". The command must be executed 
  in one of these directories. Files having the same name to the 
  corresponding X-card with the extension ".obj" will be created in 
  the directory "xobjs" and the results of the calculations are saved 
  in these files. Similarly, the log files with extension ".err" are 
  kept in the directory "xwrk".

  Usage:
  1) Feed jobs describe in X-card files to a queue number N (1 to 4)
    If the queue number is omitted, the queue #1 is assumed.

   x9.sh [-N]  <X-card file1> [<X-card file2> [<X-card file3>...]
 
  2) Cancel all jobs in the queue number N
    If the queue number is omitted, the queue #1 is assumed.

   x9.sh [-N]  --clear

  3) Show this message. 

   x9.sh --help
 
CAT
exit;exit
endif
#set count=`ps aux|grep -c "xtls6 "`
#while ($count > 1) 
#  set count=`ps aux|grep -c "xtls6 "`
#  sleep 20 
#end
cd ../xcards/
if ( $1 == "-1" ) then
  cd ../xwrk/
  shift
else if ( $1 == "-2" ) then
  if ( ! -e ../xwrk2 ) mkdir ../xwrk2
  cd ../xwrk2/
  shift
else if ( $1 == "-3" ) then
  if ( ! -e ../xwrk3 ) mkdir ../xwrk3
  cd ../xwrk3/
  shift
else if ( $1 == "-4" ) then
  if ( ! -e ../xwrk4 ) mkdir ../xwrk4
  cd ../xwrk4/
  shift
else
  cd ../xwrk/
endif

if ( $1 == "--clear" ) then
  if ( -e lock ) then
    set pid = `cat lock | awk '{print $3}'`
    set kpid = $pid
    foreach chpid (`ps -eo pid,ppid | awk '$2 == '$pid' { print $1 }'`) 
      set kpid = ($kpid $chpid) 
      foreach chchpid (`ps -eo pid,ppid | awk '$2 == '$chpid' { print $1 }'`) 
        set kpid = ($kpid $chchpid)
      end
    end
    kill -9 $kpid
    echo PID $kpid killed
  endif
  exit;exit
endif

set args=($*)
if ( -e lock ) then
  set pid = `cat lock | awk '{print $3}'`
  set active = `ps -eo pid | grep -c $pid`
  if ( $active > 0 ) echo Waiting for another job pid=$pid
  while ( $active > 0 && -e lock )
    sleep 10
    if ( -e lock ) set pid = `cat lock | awk '{print $3}'`
    set active = `ps -eo pid | grep -c $pid`
  end
endif

nohup echo Start calculation
csh -f <<JOB > /dev/null &
foreach xcard ($args)
  if ( -e ../xcards/\$xcard ) then
    nohup echo  start of \$xcard  > /dev/tty
    if ( -e xcard01 ) rm xcard01
    if ( -e xobj02 ) mv xobj02 xobj02.bak
    if ( -e xcard03 ) mv xcard03 xcard03.bak
    cp ../xcards/\$xcard xcard03
    echo $HOST `date` > ../xobjs/\$xcard.obj
    cat xcard03 >> ../xobjs/\$xcard.obj
    pre-X > \$xcard.err
    (time xtls9 )>> \$xcard.err
    if ( -e xobj02 ) then
      cat xobj02 >> ../xobjs/\$xcard.obj
    else   
      nohup echo Error! No output file for the job: \$xcard > /dev/tty
    endif
    eval "set normalend = \`tail -n 2 \$xcard.err | grep -c 'end of TOTAL'\`"
    if ( \$normalend == 0 ) then
      nohup echo Error! Irregular termination of the job: \$xcard > /dev/tty
      nohup tail -n 20 \$xcard.err > /dev/tty
    endif   
  else
    nohup echo Error! Unable to find the X-card file: \$xcard > /dev/tty
  endif
  nohup echo  end of \$xcard ^G^G  > /dev/tty
end
JOB
jobs -l > lock
wait
if ( -e lock ) rm lock
