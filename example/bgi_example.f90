program bgi_example
use bgi
implicit none

integer::gdriver, gmode
integer::midx, midy, i, ignore

    gdriver = DETECT

    call initgraph(gdriver, gmode, "")
    
    i = graphresult()
    if(i < 0) then
        Print *, "An error occurred: ", i
        stop
    else
        Print *, "Graphics ok!"
    end if
    
    midx = getmaxx() / 2;
    midy = getmaxy() / 2;

    do i = EMPTY_FILL,USER_FILL-1

        ! set the fill style
        call setfillstyle(i, getmaxcolor());

        ! draw the 3-d bar
        call bar3d(midx-50, midy-50, midx+50, midy+50, 10, 1)
      
        ignore = getch()
        Print *, "Key Pressed: ", ignore
        
   end do
   
   ! clean up 
   call closegraph(ALL_WINDOWS)
   Print *, "Window Closed"

end program bgi_example