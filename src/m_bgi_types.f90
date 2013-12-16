module bgi_types
use bgi_constants
use iso_c_binding
implicit none

    type, bind(c) :: arccoordstype
        ! Center point of the arc
        integer(kind=c_int) :: x, y
        
        ! The starting position of the arc
        integer(kind=c_int) :: xstart, ystart
        
        ! The ending position of the arc.
        integer(kind=c_int) :: xend, yend
    end type arccoordstype

    type, bind(c) :: fillsettingstype
        ! Current fill pattern
        integer(kind=c_int) :: pattern
        
        ! Current fill color
        integer(kind=c_int) :: color
    end type fillsettingstype

    type, bind(c) :: linesettingstype
        ! Current line style
        integer(kind=c_int) :: linestyle
        
        ! 16-bit user line pattern (unsigned!)
        integer(kind=c_int) :: upattern
    
        ! Width of the line in pixels
        integer(kind=c_int) :: thickness
    end type linesettingstype

    type, bind(c) :: textsettingstype
        ! The font in use
        integer(kind=c_int) :: font
        
        ! Text direction
        integer(kind=c_int) :: direction
        
        ! Character size
        integer(kind=c_int) :: charsize
        
        ! Horizontal text justification
        integer(kind=c_int) :: horiz
        
        ! Vertical text justification
        integer(kind=c_int) :: vert
    end type textsettingstype


    type, bind(c) :: viewporttype
        ! Viewport bounding box
        integer(kind=c_int) :: left, top, right, bottom
        
        ! Whether to clip image to viewport
        integer(kind=c_int) :: clip
    end type viewporttype

    type, bind(c) :: palettetype
        integer(kind=c_signed_char) :: size
        integer(kind=c_signed_char), dimension(MAXCOLORS + 1) :: colors
    end type palettetype
    
    type :: imagetype
        ! Pointer to the data
        type(c_ptr)::image_ptr
        
        integer::width
        integer::height
        
    end type imagetype

end module bgi_types