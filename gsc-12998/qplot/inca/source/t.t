{------------------------------}
PROCEDURE checklim (VAR min,max : real;  factor : real);
VAR
   dif   : real;
BEGIN
dif := max - min;
IF dif = 0 
 THEN factor := 0.1
 ELSE dif := rmax (dif,1d-6 * (abs(max) + abs(min)));

IF dif <> 0
 THEN BEGIN  max := max + dif * factor;  min := min - dif * factor;  END
ELSE IF max > 0
 THEN BEGIN  max := max + max * factor;  min := -max * factor;       END
ELSE IF max < 0
 THEN BEGIN  min := min + min * factor;  max := -min * factor;       END
 ELSE BEGIN  max := 1;                   min := -1;                  END;
END;
{------------------------------}
