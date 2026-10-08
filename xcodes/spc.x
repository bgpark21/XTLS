//////////////////////////////////////////////
//   Isotropic spectra                      //
//   latest modification : 2004.09.28       // 
//////////////////////////////////////////////
  Sys_batch=1; logical @ (Yes=true, No=false );

  char @ Input_file="../xobjs/Ni2+L_2pXPS.obj";
  char @ Output_file="temp.ps";
  char @ Title     ="    ";
         Title_size=5;

//////////////////////////////////////////
// Data file of experimental or theoretical (should end with ".obj") 
// spectra to compare.

  char   @ Input_fileC="";
  int    @ format={1,2,2};
 logical @ Auto_Height_fit=Yes;
   d     @ Height_magnify =1.0;
//////////////////////////////////////////
// (Vx0, Vy0): The location of the left-bottom corner of the frame 
//  Vw and Vh: The width and height of the frame (in units of mm).
//       (The size of A4 paper: 20.9cm(W) x 29.6cm(H) )

  logical @ LandScape = No;
  (Vx0,Vy0,Vw,Vh)=(30,50,140,140);
//////////////////////////////////////////
// Set Lorentian and Gaussian broadening widths by Gamma_l and 
// Gamma_g, respectively (Half width at half maximum value in eV)

  Gamma_l = {0.2}; E_l     ={};
  Gamma_g = 0.01; E_g     ={};
//////////////////////////////////////////
// Energy shift of the theoretical spectra.

  E_shift1 =0d0;
//////////////////////////////////////////
// Energy shift of the experimetal/theoretical spectra to compare.

  E_shift2 =0d0;
//////////////////////////////////////////
// System tempareture (in Kelvin)

  T_sys=300;
//////////////////////////////////////////
  logical @ Auto_Range_Adjust=Yes;
  (Emin,Emax)=(-7,25);

//////////////////////////////////////////
// Comment
  char @ Comment ="\RU Ni^2+^ ion site";
     d @ X_Y_Size={90,90,4d0};


  logical @ With_bars =Yes;
            bar_ratio =0.05;

  logical @ Data_Dump =No;
  char    @ XY_file="temp.txt";
  int     @ step   =1;

  char   @ Frame_para=" ";

///////////////////////////////////////////////////////
  include({Sys_XcodeLib,"pslib.x"});
  include({Sys_XcodeLib,"spclib.x"});
// HWHM to standard deviation for Gaussian broadening width 
  Gamma_g /= 1.177;  

///////////////////////////
//  main  
//
 char @ Mode;
 char @ X_title;

  ( Input_fileC =="" ?
  # Input_fileC[sizeof(Input_fileC)-3:] != ".obj" ?
    ( xC, ywC )= read_data( Input_fileC, format);
  #
    open(1,Input_fileC);
      get_spc(xC, yC, T_sys, X_title, ieph, ishl );
    close(1);
    ( xC, ywC, yC )=broaden1v(xC+E_shift2,yC,E_g,Gamma_g,E_l,Gamma_l,irange);
  );

 open(1,Input_file);
   get_spc(x,y,T_sys,X_title,ieph,ishl);
 close(1);
 irange={};
 (x,yw,y)=broaden1v(x+E_shift1,y,E_g,Gamma_g,E_l,Gamma_l,irange);

 pi=4d0*atan(1d0); nd=sizeof(y);
 print("Integrated intensity=",(x[2]-x[1])*( (y[1] + y[nd])/2d0 + sum(y[2:nd-1]) )/pi );

//
// Draw spectra 
//

  (Data_Dump ?  data_write1(XY_file, x, y, yw, step) );

  ps_open( Output_file  + ( LandScape ? ";a4l" # "" ) );

    draw_spc(x,yw,y
             ,Vx0,Vy0,Vw,Vh
             ,(Auto_Range_Adjust ? {x, xC } # {Emin,Emax}  ),yw, With_bars, bar_ratio
             ,Comment, X_Y_Size, X_Title,Frame_para);

    ( Input_fileC != "" ?
     line_dash({1,1});
      rmax=( Auto_Height_fit ? max(yw)/max(ywC) # Height_magnify );
      wline(xc,rmax*ywC);
    );

  ps_close();

  print("Xtls version 9.0 has been successfully installed!");



