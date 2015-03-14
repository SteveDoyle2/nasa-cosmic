$ PASCAL/LIST 'P1'
$ TRIM        'P1'
$ APPEND      'P1'.LIS 'P2'.LLL
$ DELETE      'P1'.LIS;*
