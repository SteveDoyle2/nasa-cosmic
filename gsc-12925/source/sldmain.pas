  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'filkqq.inc' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'display.int' }
  { $INCLUDE : 'utility.int'}
  { $INCLUDE : 'ldb.int' }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'sld.int' }
  { $INCLUDE : 'sldetc.int' }

  Program sldmain (input,output);

  USES filkqq;

  USES getparam;

  USES vbpas;

  USES dspas;

  USES sfpas;

  USES dialog;

  USES display;

  USES ldb;

  USES execute;

  USES option;

  USES utility;

  USES sld;

  USES sldetc;

  const
    prompt_line = 22;

    var
        pname, fname  : lstring(14);
        f,g           : text;
        buf           : lstring(80);
        dum           : entity;
        err        : boolean;
        n, i, j    : integer;
        reply         : supinteger(2);
        msg           : suplstring(24);
        inkey         : byte;
        inchar        : twobyte;
        ch            : char;
        firstsld      : entity;
        current    : entity;
        current_node : byte;
        status     : mode;
        length     : byte;
        derr       : boolean;

  procedure pack ( var name : lstring; var err : boolean );

    var
      df : text(41);
      sf : text;
      sortarray : array [0..199] of integer;
      i  : integer;
      tname : lstring ( 14 );
      gnum  : string(3);
      inchar : twobyte;

    begin
      assign ( f, name );
      f.trap := true;
      f.errs := 0;
      rewrite ( f );
      if f.errs = 0 then
        begin
          copylst ( name, tname );
          tname.len := tname.len - 3;
          concat ( tname, 'ord' );
          assign ( sf, tname );
          sf.trap := true;
          sf.errs := 0;
          reset ( sf );
          if sf.errs <> 0 then
            begin
              there (0,prompt_line);
              writeln ('Directory not found.  Upload not possible.  Press any key to continue');
              get_input ( inchar );
              there (0,prompt_line);
              writeln('                                                                      ');
              err := true;
              sf.errs := 0;
              close ( sf );
            end
           else
            begin
              read(sf,sortarray[0]);
              for i := 1 to sortarray[0] do read(sf,sortarray[i]);
              tname.len := tname.len-3;
              concat ( tname, 'dir' );
              assign ( df, tname );
              df.trap := true;
              df.errs := 0;
              df.mode := direct;
              reset ( df );
          if df.errs <> 0 then
            begin
              there (0, prompt_line);
              writeln ('Directory not found.  Upload not possible.  Press any key to continue');
              get_input ( inchar );
              there (0, prompt_line);
              writeln('                                                                       ');
             err := true;
              df.errs := 0;
              close ( df );
            end
           else
            begin
              writeln ( f, sortarray[0]-1 );
              tname.len := tname.len - 3;
              for i := 2 to sortarray[0] do
                begin
                  seek ( df, sortarray[i] );
                  readln ( df, gnum );
                  writeln ( f, tname, gnum );
                end;
              err := false;
            end;
            end;
        end;
       if f.errs <> 0 then
        begin
          there (0, prompt_line);
          writeln('Disk full. Upload impossible. Press any key to continue.');
          get_input(inchar);
          there (0, prompt_line);
          writeln('                                                              ');
          err := true;
        end;
      f.errs := 0;
      close ( f );
    end;

    begin
      plchoice         := 0;
      pname         := 'sld.ctl';
      gtype         := 'sld';
      sldtype         := 'sld';
      pltype         := 'sld';
      cname         := 'Slide Show';

      get_equip;
      get_parameter ( pname );

        rtype := 2;
        copylst ( null, name );
        endflag := false;
        update        := false;
        repeat
            screen ( 3 );
            colors ( 0, 1 );
            n := 1;

msg[1] := '                                        ';
msg[1].len := (80-cname.len-6) div 2;
concat ( msg[1], cname );
concat ( msg[1], ' Chart' );
msg[2] := '                                        ';
msg[2].len := ((80-cname.len-6) div 2) - 8;
concat ( msg[1], msg[2] );
concat ( msg[1], 'ver 3.1' );
msg[2] := ' ';
msg[3] := 'Options';
msg[4] := ' ';
msg[5] := '         F1      CREATE/REVISE a Slide Show';
msg[6] := '         F2      VIEW a Chart or Slide Show';
msg[7] := '         F3      DIRECTORY of charts';
msg[8] := ' ';
msg[9] := ' ';
msg[10]:= '         F4      SEND a Chart or Slide Show to TELEMAIL';
msg[11]:= '         F5      RECEIVE a Chart or Slide Show from TELEMAIL';
msg[12]:= ' ';
msg[13]:= ' ';
msg[14]:= '         F6      PLOT a Chart or Slide Show';
msg[15]:= '         F7      PRINT a Chart or Slide Show';
msg[16]:= ' ';
msg[17]:= ' ';
msg[18]:= '         F8      CHANGE defaults (drive/printer)';
msg[19]:=' ';
msg[20]:= '         F9      HELP';
msg[21]:= '  ';
msg[22]:= '         F10     EXIT to DOS';
msg[23]:= '  ';
msg[24]:= 'Select option by pressing function key:';
            menu ( msg, 24, 0, 1, 10, 10, n, reply, 1);
          case n of
          1: begin
               inkey := 101;
               plotchoice := 0;
               sldperform ( inkey, sldname, current, current_node, firstsld);
               copylst ( null, name );
             end;
          2: begin
               inkey := 103;
               plotchoice := 0;
               sldperform ( inkey, sldname, current, current_node, firstsld);
               copylst ( null, name );
             end;
          3: begin
               ldbman ( inkey, sldtype, mnt, firstsld, sldname );
               copylst ( null, name );
             end;
{         4: begin
               ldbman ( inkey, gtype, ret, dum, name );
               if name <> null then
                 begin
                    assign ( f, name );
                    reset ( f );
                    name.len := name.len - 3;
                    concat ( name, '001' );
                    assign ( g, name );
                    rewrite ( g );
                    while not ( eof ( f ) ) do
                      begin
                        readln ( f, buf );
                        writeln ( g, buf );
                      end;
                    close ( f );
                    close ( g );
                   assign ( f, 'gohost.bat' );
                   rewrite( f );
                   writeln( f, 'hostcomm' );
                   writeln( f, 'graph.bat' );
                   close  ( f );
                   assign ( f, 'ctoh.dat' );
                   rewrite( f );
                   writeln( f, gtype );
                   writeln( f, 'ut' );
                   writeln( f, name );
                   close  ( f );
                   endflag := true;
                 end;
              end;}
{      4, 5 : begin
                name := gtype;
                concat ( name, '.001' );
                assign ( f, 'gohost.bat' );
                rewrite( f );
                writeln( f, 'hostcomm' );
                writeln( f, 'graph.bat' );
                close  ( f );
                assign ( f, 'ctoh.dat' );
                rewrite( f );
                writeln( f, gtype );
                 case n of
                   4 : writeln( f, 'ut' );
                   5 : writeln( f, 'dt' );
                end; 
                writeln( f, name );
                close  ( f );
                endflag := true;
              end;}
         4,5 : begin
                 name[1] := drive;
                 name[2] := ':';
                 name.len := 2;
                 concat ( name, sldtype );
                 if n = 4 then 
                   begin
                     concat(name,'.001');
                     msg[1] := null;
                     msg[2] := null;
                     repeat
                     inkey := 0;
                     incom ( prompt_line, msg[1], msg[2], -1, rtype,
                             0, 0, prompt_line+1, 4, 0, 22, 1, 1, 1, 1,
                             1, 1, status, msg[3], length, inkey );
                     if inkey = 2 then help ( 0 );
                     until inkey <> 2;
                     case inkey of
                       43 : pack ( name, err );
                       42 : begin
                              copylst ( name, msg[1] );
                              name := null;
                              ldbman ( inkey,sldtype,ret,dum,fname );
                              copylst ( name, fname );
                              copylst ( msg[1], name );
                              if fname <> null then
                                begin
                                  err := false;
                                  assign ( f, name );
                                  f.trap := true;
                                  f.errs := 0;
                                  rewrite ( f );
                                  writeln ( f, 0 );
                                  writeln ( f, fname );
                                  if f.errs <> 0 then
                                    begin
                                      there (0, prompt_line);
                                      writeln('Disk full. Upload impossible. Press any key to continue.');
                                      get_input(inchar);
                                      there (0, prompt_line);
                                      writeln('                                                         ');
                                      f.errs := 0;
                                      err := true;
                                    end;
                                  close ( f );
                                end
                               else err := true;
                            end;
                       otherwise err := true;
                       end;
                   end
                   else [concat ( name, '.dir' );
                        err := false;
                        assign ( f, name );
                        f.trap := true;
                         f.errs := 0;
                        reset ( f );
                        if f.errs = 0 then
                         begin
                          repeat
                          inkey := 0;
                          incom ( prompt_line, messages^[32],
                                  messages^[33], -1, rtype, 0, 0,
                                  prompt_line+1, 4, 0, 21, 1, 1, 1, 1,
                                  1, 1, status, msg[1], length, inkey );
                          if inkey = 2 then help ( 0 );
                          until inkey <> 2;
                          if inkey <> 41 then err := true;
                         end;
                         f.errs := 0;
                         f.trap := false;
                         close ( f );];
                 if not err then
                   begin
                     assign ( f, 'gohost.bat' );
                     f.trap := true;
                     f.errs := 0;
                     rewrite ( f );
                     writeln ( f, 'basic hostcomm' );
                     writeln ( f, 'graph.bat' );
                     if f.errs <> 0 then
                      begin
                       there(0,prompt_line);
                       write('Disk full error. SEND/RECEIVE impossible. Press any key to continue.');
                       get_input(inchar);
                       derr := true;
                        f.errs := 0;
                      end
                     else
                     begin
                     close ( f );
                     assign ( f, 'ctoh.dat' );
                     f.trap := true;
                     f.errs := 0;
                     rewrite ( f );
                     writeln ( f, pltype );
                     if n = 4 then writeln(f,'ut') else writeln(f,'dt');
                     writeln ( f, name );
                     if f.errs <> 0 then
                      begin
                       there(0,prompt_line);
                       write('Disk full error. SEND/RECEIVE impossible. Press any key to continue.');
                       get_input(inchar);
                       derr := true;
                       f.errs := 0;
                      end;
                      end;
                     close ( f );
                     if not derr then endflag := true;
                   end;
                  if err or derr then
                    copylst (null, name);
               end;
          6: begin
               inkey := 113;
               plotchoice := 0;
               sldperform ( inkey, sldname, current, current_node, firstsld);
               copylst ( null, name );
             end;
          7: begin
               inkey := 115;
               plotchoice := 0;
               sldperform ( inkey, sldname, current, current_node, firstsld);
               copylst ( null, name );
             end;
          8: cdefault;
          9: help(1);
         10: begin
               assign ( f, 'gohost.bat' );
               rewrite( f );
               close  ( f );
               endflag := true;
             end;
         otherwise
         end;
        until ( n = 10) or ( name <> null );
        screen ( 2 );

  end.
