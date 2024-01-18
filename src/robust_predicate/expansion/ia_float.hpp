
/*
    --------------------------------------------------------
     * MPFLOAT: multi-precision floating-point arithmetic.
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
     * Last updated: 10 April, 2020
     *
     * Copyright 2020--
     * Darren Engwirda
     * de2363@columbia.edu
     * https://github.com/dengwirda/
     *
    --------------------------------------------------------
     */

//  very simple, light-weight interval arithmetic for the
//  construction of "filtered" numerical predicates. Only
//  OP = {+, -, *,} implemented...


#   pragma once

#   ifndef __IA_FLOAT__
#   define __IA_FLOAT__

#   include "mp_basic.hpp"

//  namespace mp_float {                    // hmmm no...

    /*
    --------------------------------------------------------
     * IA-FLT: interval arithmetic
    --------------------------------------------------------
     */

#   define  REAL_TYPE mp_float::real_type
#   define  INDX_TYPE mp_float::indx_type

//  silliness with "volatile" to try to stop the compiler
//  from spuriously(!) optimising floating-point op's and
//  breaking rounding-mode behaviour...

//  really, proper compiler support is needed instead and
//  it's unclear whether this is actually reliable or not

    __normal_call REAL_TYPE add_up (        // for rnd up
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (+_aa) + (+_bb) ;
        return +_cc ;
    }

    __normal_call REAL_TYPE add_dn (
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (-_aa) + (-_bb) ;
        return -_cc ;
    }

    __normal_call REAL_TYPE sub_up (
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (+_aa) - (+_bb) ;
        return +_cc ;
    }

    __normal_call REAL_TYPE sub_dn (
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (+_bb) - (+_aa) ;
        return -_cc ;
    }

    __normal_call REAL_TYPE mul_up (
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (+_aa) * (+_bb) ;
        return +_cc ;
    }

    __normal_call REAL_TYPE mul_dn (
        REAL_TYPE _aa, REAL_TYPE _bb
        )
    {   REAL_TYPE volatile _cc = (+_aa) * (-_bb) ;
        return -_cc ;
    }

    class ia_flt;

    __inline_call ia_flt operator + (       // fwd. dec's
        ia_flt const&,
        REAL_TYPE    ) ;
    __inline_call ia_flt operator + (
        REAL_TYPE    ,
        ia_flt const&) ;
    __inline_call ia_flt operator + (
        ia_flt const&,
        ia_flt const&) ;

    __inline_call ia_flt operator - (
        ia_flt const&,
        REAL_TYPE    ) ;
    __inline_call ia_flt operator - (
        REAL_TYPE    ,
        ia_flt const&) ;
    __inline_call ia_flt operator - (
        ia_flt const&,
        ia_flt const&) ;

    __inline_call ia_flt operator * (
        ia_flt const&,
        REAL_TYPE    ) ;
    __inline_call ia_flt operator * (
        REAL_TYPE    ,
        ia_flt const&) ;
    __inline_call ia_flt operator * (
        ia_flt const&,
        ia_flt const&) ;

    class ia_rnd
    {
/*---------------------------------- interval FP-rnd type */
    public  :
    int volatile _rndstate = 0;

    public  :
/*---------------------------------- floating pt rounding */
    __normal_call  ia_rnd (
        )
    {
        _rndstate=fegetround();

        fesetround (FE_UPWARD);
    }
    __normal_call ~ia_rnd (
        )
    {
        fesetround (_rndstate);
    }
    } ;

    class ia_flt
    {
/*---------------------------------- interval number type */
    public  :
    typedef REAL_TYPE           real_type;
    typedef INDX_TYPE           indx_type;

    indx_type static constexpr _size = 2 ;
    indx_type static constexpr _xlen = 2 ;

    real_type                  _xdat [ 2 ] ;

    public  :
/*------------------------------ access to expansion bits */
    __inline_call real_type&      up (
        )
    {   return this->_xdat[1] ;
    }
    __inline_call real_type&      lo (
        )
    {   return this->_xdat[0] ;
    }

    __inline_call real_type const&up (
        ) const
    {   return this->_xdat[1] ;
    }
    __inline_call real_type const&lo (
        ) const
    {   return this->_xdat[0] ;
    }

/*------------------------------ initialising constructor */
    __inline_call ia_flt (
        real_type _lo = real_type(+0.) ,
        real_type _up = real_type(+0.)
        )
    {   this->_xdat[0] = _lo ;
        this->_xdat[1] = _up ;
    }

    __inline_call ia_flt (                  // copy c'tor
        ia_flt const& _aa
        )
    {
        this->_xdat[0] = _aa.lo();
        this->_xdat[1] = _aa.up();
    }

    __inline_call ia_flt& operator = (      // assignment
        ia_flt const& _aa
        )
    {
        this->_xdat[0] = _aa.lo();
        this->_xdat[1] = _aa.up();

        return ( *this ) ;
    }
    __inline_call ia_flt& operator = (      // assignment
        real_type _aa
        )
    {
        this->_xdat[0] = (real_type)_aa;
        this->_xdat[1] = (real_type)_aa;

        return ( *this ) ;
    }

/*---------------------------------------- set from float */
    __inline_call void_type from_add (
        real_type _aa,
        real_type _bb
        )
    {
        lo() = add_dn(_aa, _bb) ;
        up() = add_up(_aa, _bb) ;
    }

    __inline_call void_type from_sub (
        real_type _aa,
        real_type _bb
        )
    {
        lo() = sub_dn(_aa, _bb) ;
        up() = sub_up(_aa, _bb) ;
    }

    __inline_call void_type from_mul (
        real_type _aa,
        real_type _bb
        )
    {
        lo() = mul_dn(_aa, _bb) ;
        up() = mul_up(_aa, _bb) ;
    }

/*---------------------------------------- math operators */
    __inline_call ia_flt  operator + (
        ) const
    {   return   ia_flt(+lo(), +up());
    }

    __inline_call ia_flt  operator - (
        ) const
    {   return   ia_flt(-lo(), -up());
    }

    __inline_call ia_flt& operator+= (      // via double
        real_type _aa
        )
    {
        ia_flt _tt = *this + _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call ia_flt& operator-= (
        real_type _aa
        )
    {
        ia_flt _tt = *this - _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call ia_flt& operator*= (
        real_type _aa
        )
    {
        ia_flt _tt = *this * _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }

    __inline_call ia_flt& operator+= (      // via ia_flt
        ia_flt const& _aa
        )
    {
        ia_flt _tt = *this + _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call ia_flt& operator-= (
        ia_flt const& _aa
        )
    {
        ia_flt _tt = *this - _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }
    __inline_call ia_flt& operator*= (
        ia_flt const& _aa
        )
    {
        ia_flt _tt = *this * _aa ;

        up() = _tt.up();
        lo() = _tt.lo();

        return ( *this ) ;
    }

/*---------------------------------------- mid-rad. forms */
    __inline_call real_type mid (
        ) const
    {
        real_type _mm = lo() + up() ;

        if (!std::isfinite(_mm))
        {
            _mm  =
           (lo() / (real_type)+2.)+
           (up() / (real_type)+2.);
        }
        else
        {
            _mm /= (real_type)+2. ;
        }

        return _mm ;
    }

    __inline_call real_type rad (
        ) const
    {
        real_type _r1 = up() - mid() ;
        real_type _r2 = mid() - lo() ;

        return std::max(_r1, _r2) ;
    }

    } ;

    /*
    --------------------------------------------------------
     * interval-float a + b operators
    --------------------------------------------------------
     */

    __inline_call ia_flt operator + (
        ia_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = add_dn(_aa.lo(), _bb) ;
        _up = add_up(_aa.up(), _bb) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    __inline_call ia_flt operator + (
        REAL_TYPE     _aa,
        ia_flt const& _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = add_dn(_aa, _bb.lo()) ;
        _up = add_up(_aa, _bb.up()) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    __inline_call ia_flt operator + (
        ia_flt const& _aa,
        ia_flt const& _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = add_dn(_aa.lo(), _bb.lo()) ;
        _up = add_up(_aa.up(), _bb.up()) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    /*
    --------------------------------------------------------
     * interval-float a - b operators
    --------------------------------------------------------
     */

    __inline_call ia_flt operator - (
        ia_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = sub_dn(_aa.lo(), _bb) ;
        _up = sub_up(_aa.up(), _bb) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    __inline_call ia_flt operator - (
        REAL_TYPE     _aa,
        ia_flt const& _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = sub_dn(_aa, _bb.up()) ;
        _up = sub_up(_aa, _bb.lo()) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    __inline_call ia_flt operator - (
        ia_flt const& _aa,
        ia_flt const& _bb
        )
    {
        REAL_TYPE _lo, _up;

        _lo = sub_dn(_aa.lo(), _bb.up()) ;
        _up = sub_up(_aa.up(), _bb.lo()) ;

        return ( ia_flt(_lo, _up) ) ;
    }

    /*
    --------------------------------------------------------
     * interval-float a * b operators
    --------------------------------------------------------
     */

    __inline_call ia_flt operator * (
        ia_flt const& _aa,
        REAL_TYPE     _bb
        )
    {
        REAL_TYPE _lo, _up;

        if (_bb > (REAL_TYPE) +0.)
        {
            _lo = mul_dn(_aa.lo(), _bb) ;
            _up = mul_up(_aa.up(), _bb) ;
        }
        else
        if (_bb < (REAL_TYPE)+0.)
        {
            _lo = mul_dn(_aa.up(), _bb) ;
            _up = mul_up(_aa.lo(), _bb) ;
        }
        else
        {
            _lo = (REAL_TYPE)+0. ;
            _up = (REAL_TYPE)+0. ;
        }

        return ( ia_flt(_lo, _up) ) ;
    }

    __inline_call ia_flt operator * (
        REAL_TYPE     _aa,
        ia_flt const& _bb
        )
    {   return ( _bb * _aa ) ;
    }

    __normal_call ia_flt operator * (
        ia_flt const& _aa,
        ia_flt const& _bb
        )
    {
        REAL_TYPE _lo, _up;

        if (_aa.lo() < (REAL_TYPE)+0.)
        {
        if (_aa.up() > (REAL_TYPE)+0.)
        {
        if (_bb.lo() < (REAL_TYPE)+0.)
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // mix * mix
        {
            REAL_TYPE _l1, _l2;
            _l1 = mul_dn(_aa.lo(), _bb.up());
            _l2 = mul_dn(_aa.up(), _bb.lo());
            _lo = std::min(_l1, _l2);

            REAL_TYPE _u1, _u2;
            _u1 = mul_up(_aa.lo(), _bb.lo());
            _u2 = mul_up(_aa.up(), _bb.up());
            _up = std::min(_u1, _u2);
        }
        else                            // mix * -ve
        {
            _lo = mul_dn(_aa.up(), _bb.lo());
            _up = mul_up(_aa.lo(), _bb.lo());
        }
        }
        else
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // mix * +ve
        {
            _lo = mul_dn(_aa.lo(), _bb.up());
            _up = mul_up(_aa.up(), _bb.up());
        }
        else                            // mix * +0.
        {
            _lo = (REAL_TYPE)+0. ;
            _up = (REAL_TYPE)+0. ;
        }
        }
        }
        else
        {
        if (_bb.lo() < (REAL_TYPE)+0.)
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // -ve * mix
        {
            _lo = mul_dn(_aa.lo(), _bb.up());
            _up = mul_up(_aa.lo(), _bb.lo());
        }
        else                            // -ve * -ve
        {
            _lo = mul_dn(_aa.up(), _bb.up());
            _up = mul_up(_aa.lo(), _bb.lo());
        }
        }
        else
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // -ve * +ve
        {
            _lo = mul_dn(_aa.lo(), _bb.up());
            _up = mul_up(_aa.up(), _bb.lo());
        }
        else                            // -ve * +0.
        {
            _lo = (REAL_TYPE)+0. ;
            _up = (REAL_TYPE)+0. ;
        }
        }
        }
        }
        else
        {
        if (_aa.up() > (REAL_TYPE)+0.)
        {
        if (_bb.lo() < (REAL_TYPE)+0.)
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // +ve * mix
        {
            _lo = mul_dn(_aa.up(), _bb.lo());
            _up = mul_up(_aa.up(), _bb.up());
        }
        else                            // +ve * -ve
        {
            _lo = mul_dn(_aa.up(), _bb.lo());
            _up = mul_up(_aa.lo(), _bb.up());
        }
        }
        else
        {
        if (_bb.up() > (REAL_TYPE)+0.)  // +ve * +ve
        {
            _lo = mul_dn(_aa.lo(), _bb.lo());
            _up = mul_up(_aa.up(), _bb.up());
        }
        else                            // +ve * +0.
        {
            _lo = (REAL_TYPE)+0. ;
            _up = (REAL_TYPE)+0. ;
        }
        }
        }
        else                            // -ve * ???
        {
            _lo = (REAL_TYPE)+0. ;
            _up = (REAL_TYPE)+0. ;
        }
        }

        return ( ia_flt(_lo, _up) ) ;
    }

    /*
    --------------------------------------------------------
     * interval-float a ^ 2 operators
    --------------------------------------------------------
     */

    __normal_call ia_flt      sqr (
        ia_flt const& _aa
        )
    {
        REAL_TYPE _lo, _up;

        if (_aa.up() < (REAL_TYPE)+0.)
        {
            _lo = mul_dn(_aa.up(), _aa.up());
            _up = mul_up(_aa.lo(), _aa.lo());
        }
        else
        if (_aa.lo() > (REAL_TYPE)+0.)
        {
            _lo = mul_dn(_aa.lo(), _aa.lo());
            _up = mul_up(_aa.up(), _aa.up());
        }
        else
        {
        if (-_aa.lo() > +_aa.up())
        {
            _lo = (REAL_TYPE)+0.;
            _up = mul_up(_aa.lo(), _aa.lo());
        }
        else
        {
            _lo = (REAL_TYPE)+0.;
            _up = mul_up(_aa.up(), _aa.up());
        }
        }

        return ( ia_flt(_lo, _up) ) ;
    }

#   undef REAL_TYPE
#   undef INDX_TYPE


//  }

#   endif//__IA_FLOAT__



