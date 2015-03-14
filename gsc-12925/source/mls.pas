  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'utility.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'ldb.int' }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'mlsaltd.int' }
  { $INCLUDE : 'mlsexec.int' }
  { $INCLUDE : 'sldetc.int' }

  program mls ( input, output );

     USES sldetc;
  
     USES vbpas;

     USES option;

     USES getparam;

     USES utility;

     USES sfpas;

     USES dialog;

     USES ldb;

     USES execute;

     USES dspas;

     USES mlsaltd;

     USES mlsexec;

  var
      pname     : lstring(14);
      length    : byte;
      inkey            : byte;
      current_node     : byte;
      current, first   : entity;
      status           : mode;
      buffer           : lstring(127);
      reply            : char;
      i, k             : byte;
      buf,msg1,msg2   : lstring(80);


  (********************************************************)

  procedure mls_initialize;

  var
      i, j, k: integer;
      width  : integer;
      id1    : integer;
      id2    : integer;

    procedure init;

      var
        i         : integer;

      begin
         current := first^.up;
         delete (current);
         current := first;
         while current^.etype <> 16 do current := current^.down;
         for i := 1 to 19 do
             begin
               add_node ( current, 16 );
               current^.y := current^.up^.y + 16;
               current^.ytext := current^.up^.ytext + 1;
               if i >= 10 then
                 current^.xtext := node_data^[16].xtext + 41;
               if i = 10 then
                 current^.ytext := node_data^[16].ytext;
             end;
         current := current^.down;
         delete ( current );
         delete ( current );
         add_node ( current, 19 );
         current^.y := current^.up^.y + 8;
         current^.ytext := current^.up^.ytext + 2;
         add_node ( current, 19 );
         current^.y := current^.up^.y + 8;
         current^.ytext := current^.up^.ytext + 2;
         current := current^.down;
         enter ( current, first );
      end;

  begin
    month := 0;
    day   := 0;
    year  := 0;
    bar_location := 1;
    color := 1;
    skipfield := [17,18,20,21];
    saved := true;
    rtype := 2;
    add2 := false;
     weekdiv := true;
    promptupd := 2;

    if name = null then
       begin
        init;
        format ( inkey, current, first );
       end
         else
       begin
         current := first^.up;
         with current^ do
           begin
            skeleton^[1] := ord ( field^[1] ) - 48;
            skeleton^[2] := ord ( field^[2] ) - 48;
            skeleton^[3] := (ord(field^[3])-48)*10+ord(field^[4])-48;
            skeleton^[4] := (ord(field^[5])-48);
            skeleton^[5] := (ord(field^[6])-48)*10+ord(field^[7])-48;
            skeleton^[6] := (ord(field^[8])-48)*10+ord(field^[9])-48;
            skeleton^[7] := (ord(field^[10])-48);
            add2 := (field^[11] = '0');
            weekdiv := (field^[12] = '0');
            enter ( current, first );
           end;
       end;
    msg1 := null;
    msg2 := null;
    promptupd := 2;
    if (name <> null) and (not (inkey in [4,5])) then
      begin
        rtype := 0;
        draw_skeleton ( first ); { display menu and draw skeleton }
        picture ( current, first );
        reduce ( 2, skeleton^[1] );
        rtype := 2;
      end;
    if (not update) and (name <> null) and (not (inkey in [4,5])) then
      begin
        while first <> nil do delete ( first );
        initialize ( first );
        init;
        msg1 := messages^[19];
        msg2 := messages^[20];
        promptupd := 1;
      end;
  end;

  (********************************************************)

  procedure mls_update;

  begin
    if msg1 = messages^[19] then
      begin
        if inkey in [4,10,43] then
          inkey := 0
        if inkey in [7,41] then
          begin
            msg1 := null;
            msg2 := null;
          end;
      end;
  end;

  (********************************************************)

  begin


     pname := 'mls.ctl';
     gtype := 'mls';
     pltype := 'sld';
     cname := 'Milestone';
     sldtype := 'sldmls';

     get_equip;
     get_parameter ( pname );


   repeat
     select;
     if not endflag then
     begin

       dsinit;
       if name <> null then load_chart ( name, first )
                 else initialize ( first );
        inkey   := 0;

        mls_initialize;

        current := first;
        current_node  := current^.etype;
          while inkey <> 1 do
            begin
              inkey := 0;
              incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
                      1, 1, 1, 1, 1, status, buffer, length, inkey );

              mls_update;

              perform2 ( inkey,current,current_node,first );
              perform  ( inkey, current, current_node, first )
            end;
    end
    until endflag;
  end.
