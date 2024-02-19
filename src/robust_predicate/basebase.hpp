
/*
------------------------------------------------------------
 * basic types, macros, compiler-settings, etc...
------------------------------------------------------------
 *
 * This program may be freely redistributed under the
 * condition that the copyright notices (including this
 * entire header) are not removed, and no compensation
 * is received through use of the software.  Private,
 * research, and institutional use is free.  You may
 * distribute modified versions of this code UNDER THE
 * CONDITION THAT THIS CODE AND ANY MODIFICATIONS MADE
 * TO IT IN THE SAME FILE REMAIN UNDER COPYRIGHT OF THE
 * ORIGINAL AUTHOR, BOTH SOURCE AND OBJECT CODE ARE
 * MADE FREELY AVAILABLE WITHOUT CHARGE, AND CLEAR
 * NOTICE IS GIVEN OF THE MODIFICATIONS.  Distribution
 * of this code as part of a commercial system is
 * permissible ONLY BY DIRECT ARRANGEMENT WITH THE
 * AUTHOR.  (If you are not directly supplying this
 * code to a customer, and you are instead telling them
 * how they can obtain it for free, then you are not
 * required to make any arrangement with me.)
 *
 * Disclaimer:  Neither I nor: Columbia University, The
 * Massachusetts Institute of Technology, The
 * University of Sydney, nor The National Aeronautics
 * and Space Administration warrant this code in any
 * way whatsoever.  This code is provided "as-is" to be
 * used at your own risk.
 *
------------------------------------------------------------
 *
 * Last updated: 02 March, 2020
 *
 * Copyright 2013-2020
 * Darren Engwirda
 * de2363@columbia.edu
 * https://github.com/dengwirda/
 *
------------------------------------------------------------
 */

#   pragma once

#   ifndef __BASEBASE__
#   define __BASEBASE__

#   include <utility>
#   include <cstddef>
#   include <cassert>

/*
------------------------------------------------------------
 * push compiler settings
------------------------------------------------------------
 */

#   if   defined(_MSC_VER)
#   pragma warning(disable:4127) // constant conditionals
#   pragma warning(disable:4503) // decorated name length

#   elif defined(__LLVM__)

#   elif defined(__GNUC__)

#   endif

#   define __assert assert

/*
------------------------------------------------------------
 * global data type alias
------------------------------------------------------------
 */

    typedef void    void_type ;
    typedef bool    bool_type ;
    typedef char    char_type ;

/*
------------------------------------------------------------
 * function call decorator
------------------------------------------------------------
 */

#   define __inline_call inline
#   define __normal_call
#   define __static_call static
#   define __friend_call friend
#   define __nocast_call explicit

/*
------------------------------------------------------------
 * copy // move forwarding
------------------------------------------------------------
 */

#   define __copy(T, x) std::forward<T const&>(x)
#   define __move(T, x) std::forward<T     &&>(x)

/*
------------------------------------------------------------
 * unused parameter macros
------------------------------------------------------------
 */

#   define __unreferenced(x) ((void) x)

/*
------------------------------------------------------------
 * no--alias pointer types
------------------------------------------------------------
 */

#   define __const_ptr(T) T const *__restrict
#   define __write_ptr(T) T       *__restrict

#   define __const_ref(T) T const &__restrict
#   define __write_ref(T) T       &__restrict

/*
------------------------------------------------------------
 * integer "flip" routines
------------------------------------------------------------
 */

#   define __isflip(__i) ( (__i) < 0)

#   define __doflip(__i) (-(__i) - 2)

#   define __unflip(__i) (((__i) < 0) \
         ? __doflip(__i) : (__i) )

/*
------------------------------------------------------------
 * integer "flip" routines
------------------------------------------------------------
 */

#   define __setbit(x,b) ((x)|=  (1ULL<<(b)))

#   define __popbit(x,b) ((x)&= ~(1ULL<<(b)))

#   define __flpbit(x,b) ((x)^=  (1ULL<<(b)))

#   define __chkbit(x,b) (!!((x)&(1ULL<<(b))) )


#   endif//__BASEBASE__



