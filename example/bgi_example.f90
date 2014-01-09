program bgi_example
use bgi
use bgi_types, only: imagetype
implicit none

integer, parameter::scr_width = 800
integer, parameter::scr_height = 600

integer::midx, midy, i, ignore

integer::w
character(len=40)::drivername

integer, dimension(4,2)::pp

type(imagetype)::img

    ! Initgraph will use the default driver at 640x480
    ! gdriver = DETECT
    ! call initgraph(gdriver, gmode, "")
    
    ! Instead, we'll use the more modern initwindow call
    w = initwindow(scr_width, scr_height, title="WinBGI Fortran Example")
    
    i = graphresult()
    if(i < 0) then
        Print *, "An error occurred: ", i
        stop
    else
        Print *, "Graphics ok!"
    end if
    
    call getdrivername(drivername)
    Print *, "My driver is called "//trim(drivername)
    
    midx = getmaxx() / 2;
    midy = getmaxy() / 2;

    ! Added for fun
    call arc(midx, midy, 0, 180, 150)
    
    pp = 0
    pp(1,:) = (/ 5, 100 /)
    pp(2,:) = (/ 55, 100 /)
    pp(3,:) = (/ 55, 130 /)
    pp(4,:) = (/ 5, 130 /)
    call drawpoly(4, pp)
    
    call allocateimage(img, 100, 100)
    
    do i = EMPTY_FILL, USER_FILL-1

        ! set the fill style
        call setfillstyle(i, RED);

        ! draw the 3-d bar
        call bar3d(midx-50, midy-50, midx+50, midy+50, 10, 1)
        ignore = getch()
        Print *, "Key Pressed: ", ignore
        
        call copyimage(midx-50, midy-50, img)
        call pasteimage(MOD((i-EMPTY_FILL)*100, scr_width), &
                        scr_height-100, img, COPY_PUT)
        
   end do
   
   call freeimage(img)
   
   ! clean up 
   call closegraph(ALL_WINDOWS)
   Print *, "Window Closed"

end program bgi_example