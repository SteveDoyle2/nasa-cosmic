{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE colorsetup_4100 (colors,intensities : integer;  candoblink : boolean);
{------------------------------}
PROCEDURE bufferempty;
BEGIN
qiowwrite (env.buffer + CRLF);
env.buffer := '';
END;
{------------------------------}
BEGIN
initpalette (termpalette,colors,intensities,candoblink);
IF env.mode = M_TEXT
 THEN
  BEGIN
  getcolorindex_4100 (termpalette,'TEXTBACK',bufferempty);
  qiowwrite (ESC + 'TFA0' + '0' + strofcolor_4100 ('TEXTBACK',false)
                          + '1' + strofcolor_4100 ('TEXTBOX',false)
                          + '2' + strofcolor_4100 ('TEXTCHAR',false)
                          + '3' + strofcolor_4100 ('CURSOR_1',false));
  qiowwrite (ESC + 'TF4'  + '4' + strofcolor_4100 ('CURSOR_2',false));
  qiowwrite (ESC + 'LI210');
  qiowwrite (ESC + 'TD34');
  END
 ELSE
  BEGIN
  getcolorindex_4100 (termpalette,'PLOTBACK',bufferempty);
  qiowwrite (ESC + 'TFA0' + '0' + strofcolor_4100 ('BLACK',false)
                          + '1' + strofcolor_4100 ('OVERBOX',false)
                          + '2' + strofcolor_4100 ('OVERCHAR',false)
                          + '3' + strofcolor_4100 ('CURSOR_1',false));
  qiowwrite (ESC + 'TF4'  + '4' + strofcolor_4100 ('CURSOR_2',false));
  IF clearcolor ('OVERBOX') 
   THEN qiowwrite (ESC + 'LI200')
   ELSE qiowwrite (ESC + 'LI210');
  END;
END;
{=============================================================================}
