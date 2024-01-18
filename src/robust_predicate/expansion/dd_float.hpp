
/*
    --------------------------------------------------------
     * MPFLOAT: multi-precision floating-point arithmetic.
    --------------------------------------------------------
     *
     * "double-double" arithmetic. Here mp-expansion size
     * is capped at 2, with subsequent bits truncated:
     *
     * M. Joldes, J-M. Muller, V. Popescu (2017): Tight &
     * rigourous error bounds for basic building blocks of
     * double-word arithmetic. ACM Transactions on
     * Mathematical Software, ACM, 44 (2), pp. 1-27.
     *
     * Y. Hida, X. Li, and D. Bailey (2000): Quad-double
     * arithmetic: Algorithms, implementation, and
     * application. In the 15th IEEE Symposium on Computer
     * Arithmetic, pp. 155-162.
     *
    --------------------------------------------------------
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
    --------------------------------------------------------
     *
     * Last updated: 16 April, 2020
     *
     * Copyright 2020--
     * Darren Engwirda
     * de2363@columbia.edu
     * https://github.com/dengwirda/
     *
    --------------------------------------------------------
     */

#   pragma once

#   ifndef __DD_FLOAT__
#   define __DD_FLOAT__

#   include "mp_basic.hpp"

//  namespace mp_float {                    // hmmm no...

    /*
    --------------------------------------------------------
     * DD_FLT: (double-double) precision numbers
    --------------------------------------------------------
     */

#   define  REAL_TYPE mp_float::real_type
#   define  INDX_TYPE mp_float::indx_type

    class dd_flt;

    __inline_call dd_flt operator + (       // fwd. dec's
        dd_flt const&,
        REAL_TYPE    ) ;
    __inline_call dd_flt operator + (
        REAL_TYPE    ,
        dd_flt const&) ;
    __inline_call dd_flt operator + (
        dd_flt const&,
        dd_flt const&) ;

    __inline_call dd_flt operator - (
        dd_flt const&,
        REAL_TYPE    ) ;
    __inline_call dd_flt operator - (
        REAL_TYPE    ,
        dd_flt const&) ;
    __inline_call dd_flt operator - (
        dd_flt const&,
        dd_flt const&) ;

    __inline_call dd_flt operator * (
        dd_flt const&,
        REAL_TYPE    ) ;
    __inline_call dd_flt operator * (
        REAL_TYPE    ,
        dd_flt const&) ;
    __inline_call dd_flt operator * (
        dd_flt const&,
        dd_flt const&) ;

    __inline_call dd_flt operator / (
        dd_flt const&,
        REAL_TYPE    ) ;
    __inline_call dd_flt operator / (
        REAL_TYPE    ,
        dd_flt const&) ;
    __inline_call dd_flt operator / (
        dd_flt const&,
        dd_flt const&) ;

    class dd_flt
    {
/*------------------------------ doubledouble number type */
    public  :
    typedef REAL_TYPE           real_type;
    typedef INDX_TYPE           indx_type;

    indx_type static constexpr _size = 2 ;
    indx_type static constexpr _xlen = 2 ;

    real_type                  _xdat [ 2 ] ;

    public  :
/*------------------------------ access to expansion bits */
    __inline_call real_type&      hi (
        )
    {   return this->_xdat[1] ;
    }
    __inline_call real_type&      lo (
        )
    {   return this->_xdat[0] ;
    }

    __inline_call real_type const&hi (
        ) const
    {   return this->_xdat[1] ;
    }
    __inline_call real_type const&lo (
        ) const
    {   return this->_xdat[0] ;
    }

/*------------------------------ initialising constructor */
    __inline_call dd_flt (
        real_type _hi = real_type(+0.) ,
        real_type _lo = real_type(+0.)
        )
    {   this->_xdat[0] = _lo ;
        this->_xdat[1] = _hi ;
    }

    __inline_call dd_flt (                  // copy c'tor
        dd_flt const& _aa
        )
    {
        this->_xdat[0] = _aa.lo();
        this->_xdat[1] = _aa.hi();
    }

    __inline_call dd_flt& operator = (      // assignment
        dd_flt const& _aa
        )
    {
        this->_xdat[0] = _aa.lo();
        this->_xdat[1] = _aa.hi();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator = (      // assignment
        real_type _aa
        )
    {
        this->_xdat[0] = +0. ;
        this->_xdat[1] = (real_type)_aa;

        return ( *this ) ;
    }

/*---------------------------------------- cast operators */
    __inline_call operator real_type (
        ) const
    {   return (real_type)(hi()+lo());
    }

    __inline_call operator indx_type (
        ) const
    {   return (indx_type)(hi()+lo());
    }

/*---------------------------------------- math operators */
    __inline_call dd_flt  operator + (
        ) const
    {   return   dd_flt(+hi(), +lo());
    }

    __inline_call dd_flt  operator - (
        ) const
    {   return   dd_flt(-hi(), -lo());
    }

/*------------------------------ helper: init. from a + b */
    __inline_call void from_add (
        real_type  _aa, real_type  _bb
        )
    {
        mp_float::one_one_add_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a - b */
    __inline_call void from_sub (
        real_type  _aa, real_type  _bb
        )
    {
        mp_float::one_one_sub_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a * a */
    __inline_call void from_sqr (
        real_type  _aa
        )
    {
        mp_float::one_one_sqr_full(_aa,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a * b */
    __inline_call void from_mul (
        real_type  _aa, real_type  _bb
        )
    {
        mp_float::one_one_mul_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

    __inline_call dd_flt& operator+= (      // via double
        real_type _aa
        )
    {
        dd_flt _tt = *this + _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator-= (
        real_type _aa
        )
    {
        dd_flt _tt = *this - _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator*= (
        real_type _aa
        )
    {
        dd_flt _tt = *this * _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator/= (
        real_type _aa
        )
    {
        dd_flt _tt = *this / _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }

    __inline_call dd_flt& operator+= (      // via dd_flt
        dd_flt const& _aa
        )
    {
        dd_flt _tt = *this + _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator-= (
        dd_flt const& _aa
        )
    {
        dd_flt _tt = *this - _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator*= (
        dd_flt const& _aa
        )
    {
        dd_flt _tt = *this * _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call dd_flt& operator/= (
        dd_flt const& _aa
        )
    {
        dd_flt _tt = *this / _aa ;

        hi() = _tt.hi();
        lo() = _tt.lo();

        return ( *this ) ;
    }

    } ;

    /*
    --------------------------------------------------------
     * double-double a + b operators
    --------------------------------------------------------
     */

    __inline_call dd_flt operator + (
        dd_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_one_add_clip(
            _aa.hi(), _aa.lo(), _bb, _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    __inline_call dd_flt operator + (
        REAL_TYPE     _aa,
        dd_flt const& _bb
        )
    {   return ( +(_bb + _aa) ) ;
    }

    __inline_call dd_flt operator + (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_two_add_clip(
            _aa.hi(), _aa.lo(),
            _bb.hi(), _bb.lo(), _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    /*
    --------------------------------------------------------
     * double-double a - b operators
    --------------------------------------------------------
     */

    __inline_call dd_flt operator - (
        dd_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_one_sub_clip(
            _aa.hi(), _aa.lo(), _bb, _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    __inline_call dd_flt operator - (
        REAL_TYPE     _aa,
        dd_flt const& _bb
        )
    {   return ( -(_bb - _aa) ) ;
    }

    __inline_call dd_flt operator - (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_two_sub_clip(
            _aa.hi(), _aa.lo(),
            _bb.hi(), _bb.lo(), _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    /*
    --------------------------------------------------------
     * double-double a * b operators
    --------------------------------------------------------
     */

    __inline_call dd_flt operator * (
        dd_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_one_mul_clip(
            _aa.hi(), _aa.lo(), _bb, _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    __inline_call dd_flt operator * (
        REAL_TYPE     _aa,
        dd_flt const& _bb
        )
    {   return ( _bb * _aa ) ;
    }

    __inline_call dd_flt operator * (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_two_mul_clip(
            _aa.hi(), _aa.lo(),
            _bb.hi(), _bb.lo(), _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    /*
    --------------------------------------------------------
     * double-double a / b operators
    --------------------------------------------------------
     */

    __inline_call dd_flt operator / (
        dd_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_one_div_clip(
            _aa.hi(), _aa.lo(), _bb, _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    __inline_call dd_flt operator / (
        REAL_TYPE     _aa,
        dd_flt const& _bb
        )
    {   return ( dd_flt(_aa) / _bb ) ;
    }

    __inline_call dd_flt operator / (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {
        REAL_TYPE _x0, _x1;
        mp_float::two_two_div_clip(
            _aa.hi(), _aa.lo(),
            _bb.hi(), _bb.lo(), _x1, _x0
            ) ;

        return ( dd_flt(_x1, _x0) ) ;
    }

    /*
    --------------------------------------------------------
     * double-double equal operators
    --------------------------------------------------------
     */

    __inline_call  bool operator == (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {   return _aa.hi() == _bb.hi() &&
               _aa.lo() == _bb.lo() ;
    }

    __inline_call  bool operator != (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {   return _aa.hi() != _bb.hi() ||
               _aa.lo() != _bb.lo() ;
    }

    __inline_call  bool operator <  (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {   return _aa.hi() != _bb.hi() ?
               _aa.hi() <  _bb.hi() :
               _aa.lo() <  _bb.lo() ;
    }

    __inline_call  bool operator >  (
        dd_flt const& _aa,
        dd_flt const& _bb
        )
    {   return _aa.hi() != _bb.hi() ?
               _aa.hi() >  _bb.hi() :
               _aa.lo() >  _bb.lo() ;
    }

#   undef REAL_TYPE
#   undef INDX_TYPE


//  }

#   endif//__DD_FLOAT__



