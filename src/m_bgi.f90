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
use bgi_constants
use bgi_types
use iso_c_binding
implicit none

    ! NOTE: These C interfaces should not be accessible outside the
    !       module.  However, GNU Fortran will produce a warning here.
    !       See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=49111 for
    !       more information.
    private :: c_initwindow, getdefaultpalette_c, getdrivername_c, &
               getfillpattern_c, getmodename_c, strlen, &
               c_f_stringconvert, c_initgraph, c_closegraph, &
               drawpoly_c, malloc_c, free_c

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
        
        subroutine drawpoly_c (numpoints, polypoints) bind(c, name="drawpoly")
        use iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value::numpoints
            integer(kind=c_int), dimension(*)::polypoints
        end subroutine drawpoly_c

        subroutine fillpoly_c (numpoints, polypoints) bind(c, name="fillpoly")
        use iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value::numpoints
            integer(kind=c_int), dimension(*)::polypoints
        end subroutine fillpoly_c

        subroutine ellipse (x, y, stangle, endangle, xradius, yradius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, stangle, endangle, xradius, yradius
        end subroutine ellipse

        subroutine fillellipse (x, y, xradius, yradius) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, xradius, yradius
        end subroutine fillellipse
            
        subroutine floodfill (x, y, border) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y, border
        end subroutine floodfill
            
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
        
        subroutine c_closegraph (window) bind(c, name="closegraph")
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::window
        end subroutine c_closegraph

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

        subroutine c_initgraph (graphdriver, graphmode, pathtodriver) bind(c, name="initgraph")
        use iso_c_binding, only: c_int, c_char
            integer(kind=c_int), intent(inout)::graphdriver, graphmode
            character(kind=c_char), dimension(*) :: pathtodriver
        end subroutine c_initgraph 

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

        subroutine getarccoords (arccoords) bind(c)
        use bgi_types, only: arccoordstype
            type(arccoordstype) :: arccoords
        end subroutine getarccoords

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

        function getdefaultpalette_c() bind(c, name="getdefaultpalette")
        use iso_c_binding, only: c_ptr
            type(c_ptr) :: getdefaultpalette_c
        end function getdefaultpalette_c
        
        function getdisplaycolor (color) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::color
            integer(kind=c_int)::getdisplaycolor
        end function getdisplaycolor

        function getdrivername_c () bind(c, name="getdrivername")
        use iso_c_binding, only: c_ptr
            type(c_ptr) :: getdrivername_c
        endfunction getdrivername_c
        
        subroutine getfillpattern_c(cstr) bind(c, name="getfillpattern")
        use iso_c_binding, only: c_ptr
            type(c_ptr) :: cstr
        end subroutine getfillpattern_c
        
        subroutine getfillsettings(fillinfo) bind(c)
        use bgi_types, only: fillsettingstype
            type(fillsettingstype) :: fillinfo
        end subroutine getfillsettings

        function getgraphmode () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getgraphmode
        end function getgraphmode
        
        subroutine getimage (left, top, right, bottom, bitmap) bind(c)
        use iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value::left, top, right, bottom
            type(c_ptr), value::bitmap
        end subroutine getimage
        
        subroutine getlinesettings(lineinfo) bind(c)
        use bgi_types, only: linesettingstype
            type(linesettingstype)::lineinfo
        end subroutine getlinesettings
        
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
        
        function getmodename_c(mode_number) bind(c, name="getmodename")
        use iso_c_binding, only: c_int, c_ptr
            type(c_ptr)::getmodename_c
            integer(kind=c_int), value::mode_number
        end function getmodename_c

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

        subroutine getpalette(palette) bind(c)
        use bgi_types, only: palettetype
            type(palettetype)::palette
        end subroutine getpalette

        function getpalettesize () bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::getpalettesize
        end function getpalettesize

        function getpixel (x, y) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::x, y
            integer(kind=c_int)::getpixel
        end function getpixel
        
        subroutine gettextsettings(texttypeinfo) bind(c)
        use bgi_types, only: textsettingstype
            type(textsettingstype)::texttypeinfo
        end subroutine gettextsettings
        
        subroutine getviewsettings(viewport) bind(c)
        use bgi_types, only: viewporttype
            type(viewporttype)::viewport
        end subroutine getviewsettings

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

        subroutine setallpalette(palette) bind(c)
        use bgi_types, only: palettetype
            type(palettetype)::palette
        end subroutine setallpalette

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

        function setgraphbufsize(bufsize) bind(c) ! uses "unsigned"
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::bufsize
            integer(kind=c_int)::setgraphbufsize
        end function setgraphbufsize

        subroutine setgraphmode(mode) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::mode
        end subroutine setgraphmode

        !void setlinestyle (int linestyle, unsigned upattern, int thickness);
        subroutine setlinestyle(linestyle, upattern, thickness) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int), value::linestyle, upattern, thickness
        end subroutine setlinestyle

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
        
        function imagesize(left, top, right, bottom) bind(c)
        use iso_c_binding, only: c_int
            integer(kind=c_int)::imagesize
            integer(kind=c_int), value::left, top, right, bottom
        end function imagesize
        
        subroutine putimage(left, top, ptr, op) bind(c)
        use iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value::left, top
            type(c_ptr), value::ptr
            integer(kind=c_int), value::op
        end subroutine putimage
        
        function strlen(str) bind(c)
        use iso_c_binding, only: c_ptr, c_int
            integer(kind=c_int)::strlen
            type(c_ptr)::str
        end function strlen
        
        function malloc_c(memsize) bind(c, name="malloc")
        use iso_c_binding, only: c_ptr, c_int
            integer(kind=c_int), value::memsize
            type(c_ptr)::malloc_c
        end function malloc_c
        
        subroutine free_c(p) bind(c, name="free")
        use iso_c_binding, only: c_ptr, c_int
            type(c_ptr)::p
        end subroutine free_c
        
    end interface
    
    contains
    
    subroutine c_f_stringconvert(cstring, str)
    use iso_c_binding, only: c_ptr, c_char, c_int
    implicit none
    
        type(c_ptr)::cstring
        character(len=*)::str
        
        character(kind=c_char), dimension(:), pointer::farray
        integer::i
        
        call c_f_pointer(cstring, farray, [strlen(cstring)])
        
        str = repeat(' ', len(str))
        do i = 1, min(len(str), strlen(cstring))
            str(i:i) = farray(i)
        end do

    end subroutine c_f_stringconvert
    
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
    
    subroutine initgraph(graphdriver, graphmode, pathtodriver)
    implicit none
        integer, intent(inout)::graphdriver, graphmode
        character(*), optional::pathtodriver
        
        integer, parameter::max_path = 256
        character(max_path)::int_path
        
        if(present(pathtodriver)) then
            if(len_trim(pathtodriver) .GE. max_path) then
                int_path = pathtodriver(1:max_path-1)//C_NULL_CHAR
            else
                int_path(1:len_trim(pathtodriver)) = trim(pathtodriver)
                int_path(len_trim(pathtodriver)+1:len_trim(pathtodriver)+1) = C_NULL_CHAR
            end if
        else
            int_path = "."//C_NULL_CHAR
        end if
        
        call c_initgraph(graphdriver, graphmode, int_path)
        
    end subroutine initgraph
    
    subroutine closegraph(windows)
    implicit none
        integer, intent(in), optional::windows
        
        if(present(windows)) then
            call c_closegraph(windows)
        else
            call c_closegraph(ALL_WINDOWS)
        end if
        
    end subroutine closegraph
    
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
    
    function getdefaultpalette() result(fptr)
    use iso_c_binding, only: c_ptr
    implicit none
    
        type(palettetype), pointer::fptr
        type(c_ptr)::cptr
        
        cptr = getdefaultpalette_c()
        call c_f_pointer(cptr, fptr)
        
    end function getdefaultpalette

    subroutine getdrivername(str)
    implicit none
    
        character(*)::str
        call c_f_stringconvert(getdrivername_c(), str)
        
    end subroutine getdrivername

    subroutine getmodename(mode_number, str)
    implicit none
    
        integer, intent(in)::mode_number
        character(*)::str
        call c_f_stringconvert(getmodename_c(mode_number), str)
        
    end subroutine getmodename

    subroutine getfillpattern(pattern)
    implicit none
    
        character, dimension(8), target::pattern
        call getfillpattern_c(c_loc(pattern))
    
    end subroutine getfillpattern
    
    subroutine drawpoly(numpoints, points)
    use iso_c_binding
    implicit none
    
        integer(kind=c_int), intent(in)::numpoints
        integer, intent(in), dimension(numpoints,2)::points
        
        integer(kind=c_int), dimension(numpoints*2), target::oned
    
        oned = reshape(transpose(points), (/ 2*numpoints /))

        call drawpoly_c(numpoints, oned)

    end subroutine drawpoly
    
    subroutine fillpoly(numpoints, points)
    use iso_c_binding
    implicit none
    
        integer(kind=c_int), intent(in)::numpoints
        integer, intent(in), dimension(numpoints,2)::points
        
        integer(kind=c_int), dimension(numpoints*2), target::oned
    
        oned = reshape(transpose(points), (/ 2*numpoints /))

        call fillpoly_c(numpoints, oned)

    end subroutine fillpoly
    
    subroutine allocateimage(img, width, height)
    use bgi_types, only: imagetype
    implicit none
    
        type(imagetype), intent(out)::img
        integer::width, height
        
        integer::memsize
        
        memsize = imagesize(0, 0, width, height)
        
        img%width = width
        img%height = height
        img%image_ptr = malloc_c(memsize)
    
    end subroutine allocateimage
    
    subroutine freeimage(img)
    use bgi_types, only: imagetype
    implicit none
    
        type(imagetype), intent(inout)::img
        img%width = 0
        img%height = 0
        call free_c(img%image_ptr)
        
    end subroutine freeimage
    
    subroutine copyimage(left, top, img)
    use bgi_types, only: imagetype
    implicit none
        
        integer, intent(in)::left, top
        type(imagetype), intent(inout)::img
        
        call getimage(left, top, left+img%width, top+img%height, img%image_ptr)
        
    end subroutine copyimage

    subroutine pasteimage(left, top, img, op)
    use bgi_types, only: imagetype
    implicit none
        
        integer, intent(in)::left, top
        type(imagetype), intent(in)::img
        integer, intent(in)::op
        
        call putimage(left, top, img%image_ptr, op)
        
    end subroutine 

end module bgi