#!/bin/csh  
xcs <<XC
Sys_batch=1;
c@sedfile="$1";
open(1,sedfile);
memsize=0d0;
setdlm("/");
rewind(1);
search(1,"*/NHM=");readln(1,NHM);
memsize+=dble(NHM)*2d0;

rewind(1);
search(1,"*/NHDM=");readln(1,NHDM);
memsize+=dble(NHDM);

rewind(1);
search(1,"*/NBSM=");readln(1,NBSM);
memsize+=dble(NBSM)*2d0;

rewind(1);
search(1,"*/NGM=");readln(1,NGM);
rewind(1);
search(1,"*/NBGM=");readln(1,NBGM);
memsize+=dble(NBGM)*dble(NGM);

rewind(1);
search(1,"*/NBMM=");readln(1,NBMM);
memsize+=dble(NBMM)*14d0;

rewind(1);
search(1,"*/NBFM=");readln(1,NBFM);
memsize+=dble(NBFM)*3d0;

rewind(1);
search(1,"*/NCIM=");readln(1,NCIM);
rewind(1);
search(1,"*/NOBTM=");readln(1,NOBTM);
rewind(1);
search(1,"*/NSTM=");readln(1,NSTM);
search(1,"*/NLBLM=");readln(1,NLBLM);
memsize+=0.5d0*(dble(NOBTM)+1d0+dble(NLBLM)/8d0)*dble(NCIM)*dble(NSTM);

rewind(1);
search(1,"*/NPMAX=");readln(1,NPMAX);
memsize+=dble(NPMAX)*4d0;

rewind(1);
search(1,"*/NRECM=");readln(1,NRECM);
memsize+=dble(NRECM)*4d0;

close(1);

print("Memory size "
     +str([memsize*8d0/1024d0**2 + 7d0 ])+" (megabyte) is required");
print(" for a parameter set in a file ",sedfile);
XC
