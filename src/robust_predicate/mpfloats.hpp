
/*
------------------------------------------------------------
 * robust multi-precision floating-point expansions...
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
 * Last updated: 11 April, 2020
 *
 * Copyright 2020--
 * Darren Engwirda
 * de2363@columbia.edu
 * https://github.com/dengwirda/
 *
------------------------------------------------------------
 */

#   pragma once

#   ifndef __MP_FLOATS__
#   define __MP_FLOATS__

#   include "basebase.hpp"

    namespace mp_float
    {
    typedef double  real_type;
    typedef int     indx_type;
    }

#   include <algorithm>
#   include <cmath>
#   include <cfenv>

//  pragma STDC FENV_ACCESS ON

#   include "expansion/dd_float.hpp"
#   include "expansion/ia_float.hpp"
#   include "expansion/mp_float.hpp"

#   include "expansion/mp_utils.hpp"

#   endif//__MP_FLOATS__



