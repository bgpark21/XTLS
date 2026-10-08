#
# retrive X-card from corresponding "obj" file
#    2004.10.31
#
foreach xobj ($*)
 if (-e ../xcards/$xobj:r ) then
   echo $xobj:r already exist
 else
   set count=`grep -c "XCRD:" $xobj`
   if ( $count >0 ) then 
     sed '/STOP:/,$d' $xobj > ../xcards/$xobj:r
     echo "STOP:" >> ../xcards/$xobj:r 
   else
     echo no original xcard is found in $xobj
   endif
 endif
end

