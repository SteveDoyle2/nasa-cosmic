      SUBROUTINE fortrd (lun,str82)
      INTEGER lun
      BYTE    str82(82)
      READ (lun,1000) (str82(i),i=3,82)
 1000 FORMAT (80A1)
      str82(1)=80
      str82(2)=0
      RETURN
      END

      SUBROUTINE fortwr (lun,str134)
      INTEGER lun
      BYTE    str134(134)
      WRITE (lun,1000) (str134(i),i=3,str134(1)+2)
 1000 FORMAT (132A1)
      RETURN
      END
