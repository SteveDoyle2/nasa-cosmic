[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:PLOT'),
  ENVIRONMENT ('BLOCK')]
MODULE block;
{=============================================================================}
{-- BLOCK DIAGRAM DRAWING PROCEDURES -----------------------------------------}
{=============================================================================}
CONST
   DARKGRAY     = 'DARK GRAY GRAY';
{-----------------------------------------------------------------------------}
PROCEDURE drb_input (ch : char;  ix,iy : integer;  str : anystring);
{------------------------------}
PROCEDURE drawinputblock (ymid,ywidth : integer;  str1,str2 : anystring;
   line_color : color_type;  box_color : color_type);
BEGIN
setcolor (line_color);
position (ix+8000,iy+ymid);  draw (ix+10000,iy+ymid);  draw (ix+10000,iy);
setcolor (box_color);
drawbox  (ix, ix+8000, iy+ymid-ywidth, iy+ymid+ywidth);
setcolor ('RED');
position (ix+4000,iy+ymid+ywidth-1000);  centergrprint (str1);
setcolor ('MAGENTA');
position (ix+4000,iy+ymid-ywidth+1000);  centergrprint (str2);
END;
{------------------------------}
BEGIN
CASE ch OF
   ' ':  BEGIN
         setcolor ('WHITE');
         position (ix+4000,iy+14500);  centergrprint ('INPUT');
         position (ix+4000,iy+13000);  centergrprint ('FUNCTION');
         drawinputblock (10000,1000,'Impulse',    '',DARKGRAY,DARKGRAY);
         drawinputblock ( 6000,2000,'Step',       '',DARKGRAY,DARKGRAY);
         drawinputblock ( 1000,2000,'Ramp',       '',DARKGRAY,DARKGRAY);
         drawinputblock (-4000,2000,'Acc.',       '',DARKGRAY,DARKGRAY);
         drawinputblock (-9000,2000,'Combination','',DARKGRAY,DARKGRAY);
         drawinputblock(-14000,2000,'Oscillator', '',DARKGRAY,DARKGRAY);
         drawinputblock(-19000,2000,'User',       '',DARKGRAY,DARKGRAY);
         END;
   'I':  drawinputblock (10000,1000,'','' ,'GREEN','BLUE');
   'S':  drawinputblock ( 6000,2000,'',str,'GREEN','BLUE');
   'R':  drawinputblock ( 1000,2000,'',str,'GREEN','BLUE');
   'A':  drawinputblock (-4000,2000,'',str,'GREEN','BLUE');
   'C':  drawinputblock (-9000,2000,'',str,'GREEN','BLUE');
   'O':  drawinputblock(-14000,2000,'',str,'GREEN','BLUE');
   'U':  drawinputblock(-19000,2000,'',str,'GREEN','BLUE');
   END;
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_summer (ix,iy : integer);
BEGIN
setcolor ('BLUE');
position (ix+1000,iy-2000);   centergrprint ('-');
position (ix-2000,iy+1000);   centergrprint ('+');
circle (ix,iy,1000);
setcolor ('GREEN');
position (ix-5000,iy);  drawto (4000,0);  drawto (-1000,500);
    drawto  (0,-1000);  drawto (1000,500);   
position (ix,iy-3000);  drawto (0,2000);  drawto (500,-1000);
    drawto  (-1000,0);  drawto (500,1000);
position (ix+1000,iy);  drawto (1000,0);  
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_sampler (ch : char;  ix,iy : integer;  str : anystring);
VAR
   dir : integer;
BEGIN
CASE ch OF 
   'L':  dir := -1;  
   'R':  dir := 1;  
   END;
setcolor ('GREEN');
position (ix,iy);  drawto (1000*dir,0);     drawto (2000*dir,2000);
position (ix,iy);  moveto (4000*dir,0);     drawto (1000*dir,0);
position (ix,iy);  moveto (1500*dir,1300);  drawto (500*dir,-300);
                   drawto (300*dir,-500);   drawto (100*dir,-500);  
                   drawto (0*dir,-500);     drawto (200*dir,400);
                   drawto (-400*dir,0);     drawto (200*dir,-400);
setcolor ('MAGENTA');
moveto (0,-1500);  grprint (str);
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_gain (ch : char;  ix,iy : integer;  str : anystring);
{------------------------------}
PROCEDURE drawgainblock (ymid,ywidth : integer;  str1,str2 : anystring;
   line_color : color_type;  box_color : color_type);
BEGIN
setcolor (line_color);
position  (ix+1000,iy);   draw (ix+1000,iy+ymid);   draw (ix+3000,iy+ymid);
position (ix+15000,iy);  draw (ix+15000,iy+ymid);  draw (ix+13000,iy+ymid);
setcolor (box_color);
drawbox (ix+3000, ix+13000, iy+ymid-ywidth, iy+ymid+ywidth);
setcolor ('RED');
position (ix+8000,iy+ymid+ywidth-1000);  centergrprint (str1);
setcolor ('MAGENTA');
position (ix+8000,iy+ymid-ywidth+1000);  centergrprint (str2);
END;
{------------------------------}
BEGIN
CASE ch OF
   ' ':  BEGIN
         setcolor ('GREEN');
         position (ix,iy);  drawto (1000,0);
         moveto (14000,0);  drawto (1000,0);
         drawgainblock  (3000,2000,'One Gain',    '',DARKGRAY,DARKGRAY);
         drawgainblock (-3000,2000,'Sensitivity', '',DARKGRAY,DARKGRAY);
         END;
   'O':  drawgainblock  (3000,2000,'',str,'GREEN','BLUE');
   'S':  drawgainblock (-3000,2000,'',str,'GREEN','BLUE');
   END;
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_block (ch : char;  ix,iy,width : integer;  str1,str2 : anystring);
VAR
   dirx,diry : integer;
BEGIN
CASE ch OF
   'L':  BEGIN  dirx := -1;  diry :=  0;  END;
   'R':  BEGIN  dirx :=  1;  diry :=  0;  END;
   'U':  BEGIN  dirx :=  0;  diry :=  1;  END;
   'D':  BEGIN  dirx :=  0;  diry := -1;  END;
   END;
setcolor ('GREEN');
position (ix,iy);  drawto (1000*dirx,1000*diry);
moveto (width*2*dirx,4000*diry);  drawto (1000*dirx,1000*diry);
setcolor ('BLUE');
drawbox (ix + (1000+width)*dirx - width, ix + (1000+width)*dirx + width,
         iy + 3000*diry - 2000,iy + 3000*diry + 2000);
setcolor ('RED');
position (ix + 1000*dirx + width*dirx,iy + 3000*diry + 1000);
centergrprint (str1);
setcolor ('MAGENTA');
position (ix + 1000*dirx + width*dirx,iy + 3000*diry - 1000);
centergrprint (str2);
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_line (ch : char;  ix,iy,len : integer);
VAR
   dirx,diry : integer;
BEGIN
CASE ch OF
   'L':  BEGIN  dirx := -1;  diry :=  0;  END;
   'R':  BEGIN  dirx :=  1;  diry :=  0;  END;
   'U':  BEGIN  dirx :=  0;  diry :=  1;  END;
   'D':  BEGIN  dirx :=  0;  diry := -1;  END;
   END;
setcolor ('GREEN');
position (ix,iy);  drawto (len*dirx,len*diry);
finplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE drb_end (ix,iy,len : integer);
BEGIN
setcolor ('GREEN');
circle (ix,iy,100);
position (ix,iy);  drawto (len,0);    drawto (-1000,1000);
                   drawto (0,-2000);  drawto (1000,1000);
finplot;
END;
{=============================================================================}
END.
