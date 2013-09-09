BGIf - Fortran Interface to the WinBGIm Library
===============================================

BGIf is a simple interface to the WinBGIm Library, a modern Windows
implementation of the Borland Graphics Interface.  The original library
can be downloaded from:

http://winbgim.codecutter.org/

The Fortran interface aims to provide all functionality available in 
the C++ version.

Installing
----------

You'll first need to compile and install WinBGIm itself.  If you've
already compiled that library, you'll be all set to build and install
this.

TODO: Instructions...

Usage
-----

When you're constructing Fortran programs using this interface, simply
pull in the bgi module:

    use bgi

To compile a program using this library, you'll need the following
library flags *in addition to the flags for including this and WinBGIm
itself*:

    -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32 -lstdc++
    
Note that libstdc++ is necessary for linking with GNU Fortran to pull
in the components needed by WinBGIm itself since it is a C++ library.

An example is included that shows how graphics are initialized and
minimally used.

Documentation
-------------

For now, you can consult the original C documentation:

http://winbgim.codecutter.org/V6_0/doc/index.html

The list of supported functions is maintained at:

https://github.com/ArmstrongJ/WinBGI-Fortran/wiki/Supported-Functions

Legal
-----

BGIf - Fortran Interface to the WinBGIm Library
by Jeff Armstrong <jeff@approximatrix.com>
Copyright 2013 Approximatrix, LLC 

BGIf is free software: you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as 
published by the Free Software Foundation, either version 3 of 
the License, or (at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General 
Public License along with Foobar.  If not, see 
<http://www.gnu.org/licenses/>.
