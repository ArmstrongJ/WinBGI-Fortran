! BGIf - Fortran Interface to the WinBGIm Library
! by Jeff Armstrong <jeff@approximatrix.com>
! Copyright 2013 Approximatrix, LLC 
!
! BGIf is free software: you can redistribute it and/or modify
! it under the terms of the GNU Library General Public License as 
! published by the Free Software Foundation, either version 3 of 
! the License, or (at your option) any later version.
!
! Foobar is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Library General Public License for more details.
!
! You should have received a copy of the GNU Library General 
! Public License along with Foobar.  If not, see 
! <http://www.gnu.org/licenses/>.

module bgi
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
    
    interface
        subroutine arc(x, y, stangle, endangle, radius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, stangle, endangle, radius
        end subroutine arc

        subroutine bar (left, top, right, bottom) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::left, top, right, bottom
        end subroutine bar
        
        subroutine bar3d (left, top, right, bottom, depth, topflag) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::left, top, right, bottom, depth, topflag
        end subroutine bar3d

        subroutine circle (x, y, radius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, radius
        end subroutine circle
        
        subroutine drawpoly (numpoints, polypoints) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::numpoints
            integer(kind=c_int), dimension(numpoints)::polypoints
        end subroutine drawpoly

        subroutine ellipse (x, y, stangle, endangle, xradius, yradius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, stangle, endangle, xradius, yradius
        end subroutine ellipse
               
        subroutine line (x1, y1, x2, y2) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x1, y1, x2, y2
        end subroutine line

        subroutine linerel (dx, dy) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::dx, dy
        end subroutine linerel

        subroutine lineto (x, y) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y
        end subroutine lineto

        subroutine moverel (dx, dy) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::dx, dy
        end subroutine moverel

        subroutine moveto (x, y) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y
        end subroutine moveto

        subroutine pieslice (x, y, stangle, endangle, radius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, stangle, endangle, radius
        end subroutine pieslice
         
        subroutine putpixel (x, y, color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, color
        end subroutine putpixel
         
        subroutine rectangle (left, top, right, bottom) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::left, top, right, bottom
        end subroutine rectangle
         
        subroutine sector (x,  y, stangle, endangle, xradius, yradius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x,  y, stangle, endangle, xradius, yradius
        end subroutine sector
    
        subroutine cleardevice () bind(c)
        end subroutine cleardevice

        subroutine clearmouseclick(mkind) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::mkind
        end subroutine clearmouseclick
        
        subroutine clearviewport () bind(c)
        end subroutine clearviewport
        
        subroutine closegraph (window) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::window
        end subroutine closegraph

        subroutine delay (millisec) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::millisec
        end subroutine delay 
        
        subroutine detectgraph (graphdriver, graphmode) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), intent(inout)::graphdriver, graphmode
        end subroutine detectgraph 
        
        subroutine graphdefaults () bind(c)
        end subroutine graphdefaults

        function graphresult() bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::graphresult
        end function graphresult

        subroutine initgraph (graphdriver, graphmode, pathtodriver) bind(c)
        use iso_c_binding, only: c_int, c_char
            integer(kind=c_int), intent(inout)::graphdriver, graphmode
            character(kind=c_char), dimension(*) :: pathtodriver
        end subroutine initgraph 

        function c_initwindow (width, height, title, left, top, dbflag, closeflag) bind(c, name="initwindow")
        use iso_c_binding, only: c_int, c_char, c_bool
            integer(kind=c_int), value::width, height
            character(kind=c_char), dimension(*) :: title
            integer(kind=c_int), value::left, top
            logical(kind=c_bool), value::dbflag, closeflag
            integer(kind=c_int)::c_initwindow
        end function c_initwindow

        function installuserdriver (dname, detect)  bind(c)
        use iso_c_binding, only: c_int, c_char, c_funptr
            character(kind=c_char), dimension(*) :: dname
            type(c_funptr)::detect
            integer(kind=c_int)::installuserdriver
        end function installuserdriver

        function installuserfont (fname)  bind(c)
        use iso_c_binding, only: c_int, c_char
            character(kind=c_char), dimension(*) :: fname
            integer(kind=c_int)::installuserfont
        end function installuserfont
        
        function ismouseclick(mkind) bind(c)
        use iso_c_binding, only: c_int, c_bool
            integer(kind=c_int), value::mkind
            logical(kind=c_bool)::ismouseclick
        end function ismouseclick 
        
        function  kbhit () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::kbhit
        end function kbhit 
        
        function registerbgidriver (driver) bind(c)
        use iso_c_binding, only: c_ptr, c_int
            type(c_ptr), value::driver
            integer(kind=c_int)::registerbgidriver
        end function registerbgidriver 

        function registerbgifont (font) bind(c)
        use iso_c_binding, only: c_ptr, c_int
            type(c_ptr), value::font
            integer(kind=c_int)::registerbgifont
        end function registerbgifont 

        subroutine registermousehandler (mkind, h) bind(c)
        use iso_c_binding, only: c_int, c_funptr
            integer(kind=c_int), value::mkind
            type(c_funptr)::h
        end subroutine registermousehandler 

        subroutine swapbuffers () bind(c)
        end subroutine swapbuffers
    
        function getactivepage () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getactivepage
        end function getactivepage

        !void getarccoords (struct arccoordstype *arccoords);

        subroutine getaspectratio (xasp, yasp) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), intent(inout)::xasp, yasp
        end subroutine getaspectratio

        function getbkcolor () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getbkcolor
        end function getbkcolor
        
        function getch () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getch
        end function getch

        function getcolor () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getcolor
        end function getcolor

        function getcurrentwindow () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getcurrentwindow
        end function getcurrentwindow

        ! struct palettetype* getdefaultpalette (void);

        function getdisplaycolor (color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::color
            integer(kind=c_int)::getdisplaycolor
        end function getdisplaycolor

        !char* getdrivername (void);

        !void getfillpattern (char *pattern); 

        !void getfillsettings (struct fillsettingstype *fillinfo);

        function getgraphmode () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getgraphmode
        end function getgraphmode
        
        subroutine getimage (left, top, right, bottom, bitmap) bind(c)
        use iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value::left, top, right, bottom
            type(c_ptr)::bitmap
        end subroutine getimage
        
        !void getlinesettings (struct linesettingstype *lineinfo);

        function getmaxcolor () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxcolor
        end function getmaxcolor

        function getmaxmode () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxmode
        end function getmaxmode

        function getmaxheight () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxheight
        end function getmaxheight

        function getmaxwidth () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxwidth
        end function getmaxwidth

        function getmaxx () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxx
        end function getmaxx

        function getmaxy () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getmaxy
        end function getmaxy
        
        !char* getmodename (int mode_number);

        subroutine getmoderange (graphdriver, lomode, himode) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::graphdriver
            integer(kind=c_int), intent(inout)::lomode, himode
        end subroutine getmoderange

        subroutine getmouseclick(mkind, x, y) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::mkind
            integer(kind=c_int), intent(inout)::x, y
        end subroutine getmouseclick

        !void getpalette (struct palettetype *palette);
                
        function getpalettesize () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getpalettesize
        end function getpalettesize

        function getpixel (x, y) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y
            integer(kind=c_int)::getpixel
        end function getpixel
        
        !void gettextsettings (struct textsettingstype *texttypeinfo);

        !void getviewsettings (struct viewporttype *viewport);

        function getvisualpage () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getvisualpage
        end function getvisualpage

        function getwindowheight () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getwindowheight
        end function getwindowheight

        function getwindowwidth () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getwindowwidth
        end function getwindowwidth

        function getx () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getx
        end function getx

        function gety () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::gety
        end function gety        
    
        subroutine setactivepage(page) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::page
        end subroutine setactivepage

        !void setallpalette (struct palettetype *palette); 

        subroutine setaspectratio(xasp, yasp) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::xasp, yasp
        end subroutine setaspectratio
        
        subroutine setbkcolor(color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::color
        end subroutine setbkcolor

        subroutine setcolor(color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::color
        end subroutine setcolor
               
        subroutine setcurrentwindow(window) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::window
        end subroutine setcurrentwindow

        subroutine setmousequeuestatus(mkind, lstatus) bind(c)
        use iso_c_binding, only: c_int, c_bool
            integer(kind=c_int), value::mkind
            logical(kind=c_bool), intent(in)::lstatus
        end subroutine setmousequeuestatus

        subroutine setfillpattern (upattern, color) bind(c)
        use iso_c_binding, only: c_int, c_char
            character(kind=c_char), dimension(*)::upattern
            integer(kind=c_int), value::color
        end subroutine setfillpattern
        
        subroutine setfillstyle(pattern, color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::pattern, color
        end subroutine setfillstyle

        !unsigned setgraphbufsize (unsigned bufsize); 

        subroutine setgraphmode(mode) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::mode
        end subroutine setgraphmode

        !void setlinestyle (int linestyle, unsigned upattern, int thickness);

        subroutine setpalette(colornum, color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::colornum, color
        end subroutine setpalette

        subroutine setrgbpalette(colornum, red, green, blue) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::colornum, red, green, blue
        end subroutine setrgbpalette

        subroutine settextjustify(horiz, vert) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::horiz, vert
        end subroutine settextjustify

        subroutine settextstyle(font, direction, charsize) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::font, direction, charsize
        end subroutine settextstyle

        subroutine setusercharsize(multx, divx, multy, divy) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::multx, divx, multy, divy
        end subroutine setusercharsize

        subroutine setviewport(left, top, right, bottom, clip) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::left, top, right, bottom, clip
        end subroutine setviewport

        subroutine setvisualpage(page) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::page
        end subroutine setvisualpage

        subroutine setwritemode(mode) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::mode
        end subroutine setwritemode
    
        subroutine outtext (textstring) bind(c)
        use iso_c_binding, only: c_char
            character(kind=c_char), dimension(*)::textstring
        end subroutine outtext 

        subroutine outtextxy (x, y, textstring) bind(c)
        use iso_c_binding, only: c_int, c_char
            character(kind=c_char), dimension(*)::textstring
            integer(kind=c_int), value::x, y
        end subroutine outtextxy
    
        function textheight (textstring) bind(c)
        use iso_c_binding, only: c_int, c_char
            character(kind=c_char), dimension(*)::textstring
            integer(kind=c_int)::textheight
        end function textheight

        function textwidth (textstring) bind(c)
        use iso_c_binding, only: c_int, c_char
            character(kind=c_char), dimension(*)::textstring
            integer(kind=c_int)::textwidth
        end function textwidth

        ! RGB COLOR Function
        function rgbcolor(r, g, b) bind(c, name="COLOR")
        use iso_c_binding, only: c_int
            integer(kind=c_int)::rgbcolor
            integer(kind=c_int), value::r, g, b
        end function rgbcolor
        
        function converttorgb(v) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::converttorgb
            integer(kind=c_int), value::v
        end function converttorgb
        
    end interface
    
    contains
    
    function initwindow (width, height, title, left, top, dbflag, closeflag) result(window_id)
    use iso_c_binding, only: C_NULL_CHAR, C_BOOL
    implicit none
    
        integer, intent(in)::width, height
        character(*), optional :: title
        integer, intent(in), optional::left, top
        logical, intent(in), optional::dbflag, closeflag
        integer::window_id
        
        integer::int_left = 0, int_top = 0
        logical::int_dbflag = .FALSE., int_closeflag = .TRUE.
        integer, parameter::max_title = 256
        character(max_title) :: int_title

        if(present(left)) then
            int_left = left
        end if
        
        if(present(top)) then
            int_top = top
        end if
        
        if(present(dbflag)) then
            int_dbflag = dbflag
        end if
        
        if(present(closeflag)) then
            int_closeflag = closeflag
        end if
        
        if(present(title)) then
            if(len_trim(title) .GE. max_title) then
                int_title = title(1:max_title-1)//C_NULL_CHAR
            else
                int_title(1:len_trim(title)) = trim(title)
                int_title(len_trim(title)+1:len_trim(title)+1) = C_NULL_CHAR
            end if
        else
            int_title = "WinBGI-Fortran"//C_NULL_CHAR
        end if

        window_id = c_initwindow(width, height, int_title, int_left, &
                                 int_top, logical(int_dbflag, C_BOOL), &
                                 logical(int_closeflag, C_BOOL))
    
        call setcurrentwindow(window_id)
    
    end function initwindow
    
    function isbgicolor(v)
    implicit none
        logical::isbgicolor
        integer, intent(in)::v
    
        isbgicolor = ((v .LT. 16) .AND. (v .GE. 0))
    
    end function isbgicolor
    
    function isrgbcolor(v)
    implicit none
        logical::isrgbcolor
        integer, intent(in)::v
        integer(kind=4)::bmask 
    
        data bmask / Z'03000000' /
    
        isrgbcolor = (IAND(v, bmask) .EQ. bmask)
    
    end function isrgbcolor
    
    function redvalue(v)
    implicit none
    
        integer::redvalue
        integer::v
        
        integer(kind=4)::bmask 

        if(isrgbcolor(v)) then
            redvalue = ibits(converttorgb(v), 0, 8)
        else
            redvalue = 0
        end if
    
    end function redvalue
    
    function greenvalue(v)
    implicit none
    
        integer::greenvalue
        integer::v
    
        if(isrgbcolor(v)) then
            greenvalue = ibits(converttorgb(v), 8, 8)
        else
            greenvalue = 0
        end if
    
    end function greenvalue
    
    function bluevalue(v)
    implicit none
    
        integer::bluevalue
        integer::v
    
        if(isrgbcolor(v)) then
            bluevalue = ibits(converttorgb(v), 16, 8)
        else
            bluevalue = 0
        end if
    
    end function bluevalue

end module bgi