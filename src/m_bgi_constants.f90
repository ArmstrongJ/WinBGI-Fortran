module bgi_constants
use iso_c_binding
implicit none

    integer(kind=c_int), parameter:: KEY_HOME = 71
    integer(kind=c_int), parameter:: KEY_UP =   72
    integer(kind=c_int), parameter:: KEY_PGUP = 73
    integer(kind=c_int), parameter:: KEY_LEFT = 75
    integer(kind=c_int), parameter:: KEY_CENTER = 76
    integer(kind=c_int), parameter:: KEY_RIGHT = 77
    integer(kind=c_int), parameter:: KEY_END =  79
    integer(kind=c_int), parameter:: KEY_DOWN = 80
    integer(kind=c_int), parameter:: KEY_PGDN = 81
    integer(kind=c_int), parameter:: KEY_INSERT = 82
    integer(kind=c_int), parameter:: KEY_DELETE = 83
    integer(kind=c_int), parameter:: KEY_F1 =   59
    integer(kind=c_int), parameter:: KEY_F2 =   60
    integer(kind=c_int), parameter:: KEY_F3 =   61
    integer(kind=c_int), parameter:: KEY_F4 =   62
    integer(kind=c_int), parameter:: KEY_F5 =   63
    integer(kind=c_int), parameter:: KEY_F6 =   64
    integer(kind=c_int), parameter:: KEY_F7 =   65
    integer(kind=c_int), parameter:: KEY_F8 =   66
    integer(kind=c_int), parameter:: KEY_F9 =   67

    ! Line thickness settings
    integer(kind=c_int), parameter:: NORM_WIDTH = 1
    integer(kind=c_int), parameter:: THICK_WIDTH = 3

    ! Character Size and Direction
    integer(kind=c_int), parameter:: USER_CHAR_SIZE = 0
    integer(kind=c_int), parameter:: HORIZ_DIR = 0
    integer(kind=c_int), parameter:: VERT_DIR = 1


    ! Constants for closegraph
    integer(kind=c_int), parameter:: CURRENT_WINDOW = -1
    integer(kind=c_int), parameter:: ALL_WINDOWS = -2
    integer(kind=c_int), parameter:: NO_CURRENT_WINDOW = -3

    ! The standard Borland 16 colors
    integer(kind=c_int), parameter:: MAXCOLORS = 15
    
    ! No mouse click
    integer(kind=c_int), parameter:: NO_CLICK = -1
    
    enum, bind(c) ! fill styles
        enumerator :: EMPTY_FILL, SOLID_FILL, LINE_FILL, LTSLASH_FILL, &
                      SLASH_FILL, BKSLASH_FILL, LTBKSLASH_FILL, HATCH_FILL, & 
                      XHATCH_FILL, INTERLEAVE_FILL, WIDE_DOT_FILL, &
                      CLOSE_DOT_FILL, USER_FILL
    end enum

    enum, bind(c)
        enumerator :: DETECT, CGA, MCGA, EGA, EGA64, EGAMONO, IBM8514, &
                      HERCMONO, ATT400, VGA, PC3270
    end enum

    enum, bind(c)
        enumerator :: BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, BROWN, &
                      LIGHTGRAY, DARKGRAY, LIGHTBLUE, LIGHTGREEN, &
                      LIGHTCYAN, LIGHTRED, LIGHTMAGENTA, YELLOW, WHITE
    end enum
    
    enum, bind(c)
        enumerator :: SOLID_LINE, DOTTED_LINE, CENTER_LINE, DASHED_LINE, &
                      USERBIT_LINE
    end enum
    
    enum, bind(c) 
        enumerator :: grInvalidVersion = -18, grInvalidDeviceNum = -15, &
                      grInvalidFontNum, grInvalidFont, grIOerror, &
                      grError, grInvalidMode, grNoFontMem, grFontNotFound, &
                      grNoFloodMem, grNoScanMem, grNoLoadMem, &
                      grInvalidDriver, grFileNotFound, grNotDetected, &
                      grNoInitGraph, grOk
    end enum
    
end module bgi_constants