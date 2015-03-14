$ PAS/LIST        'P1'
$ LIB QPLOT       'P1'
$ DELETE          'P1'.OBJ;*
$ TRIM            'P1'
$ APPEND          'P1'.LIS 'P2'.LLL
$ DELETE          'P1'.LIS;*
