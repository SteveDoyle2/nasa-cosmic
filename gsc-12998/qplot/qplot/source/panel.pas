[ IDENT   ('PANEL'),
  INHERIT ('SYSTEM','FIG','MATH','IO','QPLOT','READMATH','QSHOWSET')]
PROGRAM panel;
VAR
   key          : char;
   pt           : point;
   str          : VARYING [80] OF char;
   fillcolor    : color_type  := LIGHT_GRAY;
   bordercolor  : color_type  := DARK_GRAY;
   lim          : plotlimits            := ((-1.03,-1.03),(1.03,1.03));
   title        : VARYING [500] OF char := '';
   xlable       : VARYING [80] OF char  := '';
   ylable       : VARYING [80] OF char  := '';
BEGIN   
qsetterm ('/AUTO','');
clearscreen;
setx (false,unity,xlable,'',false,false);
sety (false,unity,ylable,'',false,false);
setc (black,black,black,black,black);
xymapit (lim,strtrunc(title,80));
REPEAT 
   readcursor (key,pt);
   CASE key OF
      'O':  BEGIN
            openpanel (fillcolor,bordercolor);
            scaleposition (pt);
            END;
      'D':  scaledraw (pt);
      'C':  BEGIN
            scaledraw (pt);
            closepanel;
            END;
      'F':  BEGIN
            writeline (out,'FILL COLOR IS ' + strofcolor(fillcolor));
            readvary ('ENTER FILL COLOR: ',str,strofcolor(fillcolor));
            fillcolor := colorofstr(str,drawable_colors);
            scaleposition (pt);
            grprint ('   ');
            END;
      'B':  BEGIN
            writeline (out,'BORDER COLOR IS ' + strofcolor(bordercolor));
            readvary ('ENTER BORDER COLOR: ',str,strofcolor(bordercolor));
            bordercolor := colorofstr(str,drawable_colors);
            scaleposition (pt);
            grprint ('   ');
            END;
      END;
   UNTIL key IN ['X','E'];
clearscreen;
END.
