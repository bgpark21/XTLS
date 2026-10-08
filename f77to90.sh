#!/bin/sh
#
xcs <<XCODE
Sys_batch=1;
c@f77file="$1";
l@buffered =false;

get_line()=(
  global_l @ buffered;
  global_c @ (aline, line_buff);

  (buffered ?
    line_buff=aline;
    aline=nextln(1);
  );
  buffered=true;
  aline^
);

undo_line()=(
  global_c @ (aline, line_buff);
  global_l @ buffered;

  (buffered ?
    buffered=false;
    line_buff^
  #
    write(2,"Error! no line in the buffer");
    stop();
  );
);

hidden @ (aline, line_buff, buffered);

write72(aline)=(
  c @aline;
  ( (n=sizeof(aline)) >72 ?
    (aline[1] != " " ?
      write(2, aline);
    #
      (i=n # (i >= 1 ? aline[i] != "," # false ) # i-- ?? ); 
      (i> 0 ?
        write(2, aline[:i-1]+"&");
        write(2, "      "+aline[i:]);
      );
    );
  #
    write(2, aline);
  );
);

//
//  Replace Word1 by Word2 in a Text
//
ReplaceWord(Text,Word1,Word2)=(

  char@(Text,Word1,Word2);
  char@ Text2;

  n=sizeof(Text);
  nw=sizeof(Word1);
  
  (i=1 # i<=n ??
    ( (i <= n ? Text[i] != Word1[1] : false) ?? Text2 += Text[i]; i++; );
    ( i+nw-1 <= n ?
      (j=2 # (j <=nw ? Text[i+j-1] == Word1[j] : false) ?? j++ );
      (j > nw ? Text2 +=Word2; i+=nw; 
              # Text2 +=Text[i:i+j-1]; i+=j; );
    # i<=n ?
      Text2 += Text[i]; i++
    );
  );
  Text=Text2;
);


  
freeformat(aline)=(
   c @ (aline, aline2);
   i@txp=1;

   n=sizeof(aline);
   (n==0 ? 
     0^
   # aline[1]!=" " ?
     aline[1]="!";
     0^
   # 
    (txp=1 # (txp<=n ? aline[txp]==" " # false ) # txp++ ?? );
   );
//   aline2=get_line();
//   ( index(aline2,"     &") == 1 ?? 
//     aline=aline+aline2; 
//     aline2=get_line();
//   );
//   aline=undo_line();
   txp^
);
    dimension(TYPE,aline)=(
      c @(TYPE,aline,dim);
      ( (i=index(aline,"(")) !=0 ?
        j=index(aline,")");
        dim=aline[i:j]; 
        ndim=j-i+1;
        TYPE=TYPE+", DIMENSION"+dim;
      #
        {}^;
      );
      ( (i=index(aline,dim)) !=0 ??
        (i+ndim > sizeof(aline) ?
          aline=aline[:i-1];
        #
          aline=aline[:i-1]+aline[i+ndim:];
        );
      );
      ( index(aline,")") !=0 || index(aline,"(") !=0 ?
        write(2,"Wrong format in dimension");
        stop();
      );
    );
          
      transform(txp,aline,TYPE)=(
        c@(aline,TYPE,TYPE2);
        ntype=sizeof(TYPE);
        ( index(aline[txp:],TYPE) ==1 ?
          ( TYPE=="CHARACTER*" ?
            (aline[txp+10]=="(" ?
              i=index(aline[txp+11:],")");
              TYPE2="CHARACTER("+aline[txp+11:txp+11+i];
              ntype = 11+i;
            # 
              i=index(aline[txp+10:]," ");
              TYPE2="CHARACTER("+aline[txp+10:txp+10+i-2]+")";
              ntype = 9+i;
            );
          #
            TYPE2=TYPE;
          );
          aline = get_line();
          ( (txp2=freeformat(aline)) == 0?
            aline = undo_line();
            aline=aline[txp+ntype:];
          //  dimension(TYPE2,aline);
            write(2,(txp-1){" "}+TYPE2+" ::"+aline );
          # index(aline[txp2:],"PARAMETER") ==1 ?
            i=index(aline[txp2+9:],"(");
            n=sizeof(aline); 
            (aline[n] == ")" ?  n--; # aline=aline+"&"; n++ );
            write(2,(txp-1){" "}+TYPE2+", PARAMETER ::"+aline[txp2+9+i:n] );
            ( n=sizeof(aline); aline[n] != ")" ??
              aline=get_line();
              n=sizeof(aline); 
              (aline[n] == ")" ?  n--;
                aline[6] = " "; 
              #  
                aline=aline+"&"; n++ 
              );
              write(2,aline[:n]);
            );
          # 
            aline = undo_line();
            aline=aline[txp+ntype:];
          //  dimension(TYPE2,aline);
            write(2,(txp-1){" "}+TYPE2+" ::"+aline );
          );
          true^
        #
          false^
        )
      );
open(1,f77file,"old");
open(2,f77file+"90");

  c@(aline,aline2);
  i@txp;
  ( !eof(1) ??
    aline = get_line();
    ( (txp=freeformat(aline)) == 0 ? 
      write(2,aline);
    #
      (txp <= sizeof(aline) ?
        (transform(txp,aline,"INTEGER*8") ?
        #transform(txp,aline,"INTEGER*4") ?
        #transform(txp,aline,"INTEGER") ?
        #transform(txp,aline,"REAL*8") ?
        #transform(txp,aline,"REAL*4") ?
        #transform(txp,aline,"REAL") ?
        #transform(txp,aline,"COMPLEX*16") ?
        #transform(txp,aline,"COMPLEX*8") ?
        #transform(txp,aline,"COMPLEX") ?
        #transform(txp,aline,"CHARACTER*") ?
        #transform(txp,aline,"CHARACTER") ? 
        #transform(txp,aline,"LOGICAL") ? 
        #
          aline2=get_line();
          ( ( sizeof(aline2) >= 6 ? 
               aline2[1]==" " && aline2[6]=="&" # false ) ??
//            ReplaceWord(aline,"'(A,$)'","'(A)',ADVANCE='no'");
            write(2,aline+" &");
            aline2[6]=" ";
            aline=aline2;
            aline2=get_line();
          );
//          ReplaceWord(aline,"'(A,$)'","'(A)',ADVANCE='no'");
          write(2,aline);
          undo_line();
        );
      );
    );
  );
close(1);

XCODE
