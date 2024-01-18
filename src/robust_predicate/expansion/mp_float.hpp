
    /*
    --------------------------------------------------------
     * MPFLOAT: multi-precision floating-point arithmetic.
    --------------------------------------------------------
     *
     * These are the high-level multi-precision objects ---
     * computing elementary operations on "expansions" of
     * floating-point numbers such that rounding error is
     * eliminated. See Shewchuk for more detail:
     *
     * J. R. Shewchuk (1997), Adaptive Precision Floating-
     * Point Arithmetic & Fast Robust Geometric Predicates
     * Discrete & Computational Geometry, 18, pp. 305-363.
     *
     * This header provides a stack allocated, compile-time
     * "expansion" object that wraps Shewchuk's operators,
     * inspired by similar run-time constructs, e.g. Lévy:
     *
     * B. Lévy (2016), Robustness and efficiency of
     * geometric programs: The Predicate Construction Kit
     * (PCK). Computer-Aided Design, 72, pp. 03-12.
     *
     * Here, various compile-time techniques and template
     * patterns are used to build a "zero-overhead"
     * framework that doesn't require run-time stack/heap
     * manipulation or pointer indirection.
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
     * Last updated: 07 April, 2020
     *
     * Copyright 2020--
     * Darren Engwirda
     * de2363@columbia.edu
     * https://github.com/dengwirda/
     *
    --------------------------------------------------------
     */

#   pragma once

#   ifndef __MP_FLOAT__
#   define __MP_FLOAT__

#   include "mp_basic.hpp"

    namespace mp_float {

    /*
    --------------------------------------------------------
     * EXPANSION: multi-precision floating-point numbers.
    --------------------------------------------------------
     */

#   define  REAL_TYPE mp_float::real_type
#   define  INDX_TYPE mp_float::indx_type

    template <
    size_t   N = +1           // max. floats in expansion
             >
    class expansion
    {
/*-------------- a compile-time multi-precision expansion */
    public  :
    typedef REAL_TYPE           real_type;
    typedef INDX_TYPE           indx_type;

    indx_type static constexpr _size = N ;

    real_type                  _xdat [ N ] ;
    indx_type                  _xlen = 0 ;

    public  :
/*------------------------------ initialising constructor */
    __inline_call expansion ()
    {   // just default...
    }
    __inline_call expansion (
        REAL_TYPE  _xx
        )
    {   this->push(_xx) ;
    }

/*------------------------------ append bits to expansion */
    __inline_call void push (
        real_type  _xx
        )
    {   this->_xdat[this->_xlen++] = _xx ;
    }

/*------------------------------ query the expansion size */
    __inline_call indx_type count (
        ) const
    {   return  this->_xlen ;
    }
    __inline_call indx_type alloc (
        ) const
    {   return  this->_size ;
    }
    __inline_call bool      empty (
        ) const
    {   return  this->_xlen == +0 ;
    }

/*------------------------------ access to expansion bits */
    __inline_call real_type      & operator[] (
        indx_type  _ii
        )
    {
        assert ( _ii < this->_size &&
            "expansion: index out of bounds") ;

        return ( this->_xdat[_ii] ) ;
    }

    __inline_call real_type const& operator[] (
        indx_type  _ii
        ) const
    {
        assert ( _ii<= this->_size &&
            "expansion: index out of bounds") ;

        return ( this->_xdat[_ii] ) ;
    }

    public  :
/*------------------------------ helper: init. from a + b */
    __inline_call void from_add (
        real_type  _aa, real_type  _bb
        )
    {
        static_assert( _size >= 2,
            "from-add: insufficient alloc.!") ;

        this->_xlen =  +2 ;

        one_one_add_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a - b */
    __inline_call void from_sub (
        real_type  _aa, real_type  _bb
        )
    {
        static_assert( _size >= 2,
            "from-sub: insufficient alloc.!") ;

        this->_xlen =  +2 ;

        one_one_sub_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a * a */
    __inline_call void from_sqr (
        real_type  _aa
        )
    {
        static_assert( _size >= 2,
            "from-sqr: insufficient alloc.!") ;

        this->_xlen =  +2 ;

        one_one_sqr_full(_aa,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

/*------------------------------ helper: init. from a * b */
    __inline_call void from_mul (
        real_type  _aa, real_type  _bb
        )
    {
        static_assert( _size >= 2,
            "from-mul: insufficient alloc.!") ;

        this->_xlen =  +2 ;

        one_one_mul_full(_aa, _bb,
            this->_xdat[1],
            this->_xdat[0]) ;
    }

    } ;

    /*
    --------------------------------------------------------
     * shortcut utilities to construct basic expansions
    --------------------------------------------------------
     */

    __inline_call
        expansion<2> expansion_from_add (
        REAL_TYPE  _aa, REAL_TYPE  _bb
        )
    {
        expansion<2> _ex; _ex.from_add(_aa, _bb) ;
        return  _ex;
    }

    __inline_call
        expansion<2> expansion_from_sub (
        REAL_TYPE  _aa, REAL_TYPE  _bb
        )
    {
        expansion<2> _ex; _ex.from_sub(_aa, _bb) ;
        return  _ex;
    }

    __inline_call
        expansion<2> expansion_from_sqr (
        REAL_TYPE  _aa
        )
    {
        expansion<2> _ex; _ex.from_sqr(_aa) ;
        return  _ex;
    }

    __inline_call
        expansion<2> expansion_from_mul (
        REAL_TYPE  _aa, REAL_TYPE  _bb
        )
    {
        expansion<2> _ex; _ex.from_mul(_aa, _bb) ;
        return  _ex;
    }

    /*
    --------------------------------------------------------
     * alloc. requirements for operations on expansions
    --------------------------------------------------------
     */

    __inline_call INDX_TYPE constexpr add_alloc (
        INDX_TYPE _na,
        INDX_TYPE _nb
        )
    {   return _na + _nb ;
    }

    __inline_call INDX_TYPE constexpr sub_alloc (
        INDX_TYPE _na,
        INDX_TYPE _nb
        )
    {   return _na + _nb ;
    }

    __inline_call INDX_TYPE constexpr mul_alloc (
        INDX_TYPE _na,
        INDX_TYPE _nb
        )
    {   return _na * _nb * +2 ;
    }

    /*
    --------------------------------------------------------
     * add two multi-precision expansion, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NE, size_t NF, size_t NH
             >
    __normal_call void fast_expansion_add_zeroelim (
        expansion <NE> const& _ee ,
        expansion <NF> const& _ff ,
        expansion <NH> & _hh
        ) // adapted from:  fast_expansion_sum_zeroelim
    {
        REAL_TYPE _qq, _qn, _hx;
        REAL_TYPE _ex = _ee [0];
        REAL_TYPE _fx = _ff [0];
        INDX_TYPE _ei = +0, _fi = +0 ;

        _hh._xlen = 0;

        if((_fx > _ex) == (_fx > -_ex))
        {
            _qq = _ex;
            _ex = _ee[++_ei];
        }
        else
        {
            _qq = _fx;
            _fx = _ff[++_fi];
        }

        if((_ei < _ee._xlen) && (_fi < _ff._xlen))
        {
            if((_fx > _ex) == (_fx > -_ex))
            {
                one_one_add_fast(
                    _ex, _qq, _qn, _hx);
                _qq = _qn;
                _ex = _ee[++_ei];
            }
            else
            {
                one_one_add_fast(
                    _fx, _qq, _qn, _hx);
                _qq = _qn;
                _fx = _ff[++_fi];
            }
            if (_hx != +0.0) _hh.push (_hx) ;

            while ((_ei < _ee._xlen) &&
                   (_fi < _ff._xlen) )
            {
            if((_fx > _ex) == (_fx > -_ex))
            {
                one_one_add_full(
                    _qq, _ex, _qn, _hx);
                _qq = _qn;
                _ex = _ee[++_ei] ;
            }
            else
            {
                one_one_add_full(
                    _qq, _fx, _qn, _hx);
                _qq = _qn;
                _fx = _ff[++_fi] ;
            }
            if (_hx != +0.0) _hh.push (_hx) ;
            }
        }

        while (_ei < _ee._xlen)
        {
            one_one_add_full(_qq, _ex, _qn, _hx);
            _qq = _qn;
            _ex = _ee[++_ei];
            if (_hx != +0.0) _hh.push (_hx) ;
        }

        while (_fi < _ff._xlen)
        {
            one_one_add_full(_qq, _fx, _qn, _hx);
            _qq = _qn;
            _fx = _ff[++_fi];
            if (_hx != +0.0) _hh.push (_hx) ;
        }

        if((_qq != +0.0) || (_hh._xlen == +0))
        {
            _hh.push(_qq) ;
        }
    }

    template <
        size_t NA, size_t NB, size_t NC
             >
    __inline_call void      expansion_add (
        expansion <NA> const& _aa ,
        expansion <NB> const& _bb ,
        expansion <NC> & _cc
        ) // adapted from:  fast_expansion_sum_zeroelim
    {
        static_assert ( NC >= NA + NB ,
            "expansion-add: insufficient alloc.!");

        if (_aa._xlen == +1 &&      // 1-to-1 unrolling
            _bb._xlen == +1)
        {
            REAL_TYPE _t1, _t0;

            _cc._xlen = +0 ;

            one_one_add_full(
                _aa[0], _bb[0], _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +2 &&      // 2-to-1 unrolling
            _bb._xlen == +1)
        {
            REAL_TYPE _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_one_add_full(
                _aa[1], _aa[0], _bb[0], _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +1 &&      // 1-to-2 unrolling
            _bb._xlen == +2)
        {
            REAL_TYPE _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_one_add_full(
                _bb[1], _bb[0], _aa[0], _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +2 &&      // 2-to-2 unrolling
            _bb._xlen == +2)
        {
            REAL_TYPE _t3, _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_two_add_full(
                _aa[1], _aa[0],
                _bb[1], _bb[0], _t3, _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_t3 != +0.0) _cc.push (_t3) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else                        // the n-to-m loops
        {
            fast_expansion_add_zeroelim(_aa, _bb, _cc);
        }
    }

    /*
    --------------------------------------------------------
     * sub two multi-precision expansion, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NE, size_t NF, size_t NH
             >
    __normal_call void fast_expansion_sub_zeroelim (
        expansion <NE> const& _ee ,
        expansion <NF> const& _ff ,
        expansion <NH> & _hh
        ) // adapted from: fast_expansion_diff_zeroelim
    {
        REAL_TYPE _qq, _qn, _hx;
        REAL_TYPE _ex = _ee [0];
        REAL_TYPE _fx =-_ff [0];
        INDX_TYPE _ei = +0, _fi = +0 ;

        _hh._xlen = 0;

        if((_fx > _ex) == (_fx > -_ex))
        {
            _qq = _ex;
            _ex = _ee[++_ei];
        }
        else
        {
            _qq = _fx;
            _fx =-_ff[++_fi];
        }

        if((_ei < _ee._xlen) && (_fi < _ff._xlen))
        {
            if((_fx > _ex) == (_fx > -_ex))
            {
                one_one_add_fast(
                    _ex, _qq, _qn, _hx);
                _qq = _qn;
                _ex = _ee[++_ei];
            }
            else
            {
                one_one_add_fast(
                    _fx, _qq, _qn, _hx);
                _qq = _qn;
                _fx =-_ff[++_fi];
            }
            if (_hx != +0.0) _hh.push (_hx) ;

            while ((_ei < _ee._xlen) &&
                   (_fi < _ff._xlen) )
            {
            if((_fx > _ex) == (_fx > -_ex))
            {
                one_one_add_full(
                    _qq, _ex, _qn, _hx);
                _qq = _qn;
                _ex = _ee[++_ei] ;
            }
            else
            {
                one_one_add_full(
                    _qq, _fx, _qn, _hx);
                _qq = _qn;
                _fx =-_ff[++_fi] ;
            }
            if (_hx != +0.0) _hh.push (_hx) ;
            }
        }

        while (_ei < _ee._xlen)
        {
            one_one_add_full(_qq, _ex, _qn, _hx);
            _qq = _qn;
            _ex = _ee[++_ei];
            if (_hx != +0.0) _hh.push (_hx) ;
        }

        while (_fi < _ff._xlen)
        {
            one_one_add_full(_qq, _fx, _qn, _hx);
            _qq = _qn;
            _fx =-_ff[++_fi];
            if (_hx != +0.0) _hh.push (_hx) ;
        }

        if((_qq != +0.0) || (_hh._xlen == +0))
        {
            _hh.push(_qq) ;
        }
    }

    template <
        size_t NA, size_t NB, size_t NC
             >
    __inline_call void      expansion_sub (
        expansion <NA> const& _aa ,
        expansion <NB> const& _bb ,
        expansion <NC> & _cc
        ) // adapted from: fast_expansion_diff_zeroelim
    {
        static_assert ( NC >= NA + NB ,
            "expansion-sub: insufficient alloc.!");

        if (_aa._xlen == +1 &&      // 1-to-1 unrolling
            _bb._xlen == +1)
        {
            REAL_TYPE _t1, _t0;

            _cc._xlen = +0 ;

            one_one_sub_full(
                _aa[0], _bb[0], _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +2 &&      // 2-to-1 unrolling
            _bb._xlen == +1)
        {
            REAL_TYPE _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_one_sub_full(
                _aa[1], _aa[0], _bb[0], _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +2 &&      // 2-to-2 unrolling
            _bb._xlen == +2)
        {
            REAL_TYPE _t3, _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_two_sub_full(
                _aa[1], _aa[0],
                _bb[1], _bb[0], _t3, _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_t3 != +0.0) _cc.push (_t3) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else                        // the n-to-m loops
        {
            fast_expansion_sub_zeroelim(_aa, _bb, _cc);
        }
    }

    /*
    --------------------------------------------------------
     * add/sub multi-precision expansion utilities
    --------------------------------------------------------
     */

    template <
        size_t NA, size_t NC
             >
    __inline_call void      expansion_add (
        expansion <NA> const& _aa ,
        REAL_TYPE _bb ,
        expansion <NC> & _cc
        )                           // add --- from-one
    {
        expansion_add(
            _aa, expansion<1>(_bb), _cc ) ;
    }

    template <
        size_t NA, size_t NC
             >
    __inline_call void      expansion_sub (
        expansion <NA> const& _aa ,
        REAL_TYPE _bb ,
        expansion <NC> & _cc
        )                           // sub --- from-one
    {
        expansion_sub(
            _aa, expansion<1>(_bb), _cc ) ;
    }

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND
             >
    __inline_call void      expansion_add (
        expansion <NA> const& _aa,
        expansion <NB> const& _bb,
        expansion <NC> const& _cc,
        expansion <ND> & _dd
        )                           // 3-way add kernel
    {
        expansion<add_alloc(NA,  NB)> _ab ;
        expansion_add(_aa, _bb, _ab);

        expansion_add(_ab, _cc, _dd);
    }

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE
             >
    __inline_call void      expansion_add (
        expansion <NA> const& _aa,
        expansion <NB> const& _bb,
        expansion <NC> const& _cc,
        expansion <ND> const& _dd,
        expansion <NE> & _ee
        )                           // 4-way add kernel
    {
        expansion<add_alloc(NA,  NB)> _ab ;
        expansion_add(_aa, _bb, _ab);

        expansion<add_alloc(NC,  ND)> _cd ;
        expansion_add(_cc, _dd, _cd);

        expansion_add(_ab, _cd, _ee);
    }

    /*
    --------------------------------------------------------
     * scale a multi-precision expansion, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NE, size_t NH
             >
    __normal_call void scale_expansion_zeroelim (
        expansion <NE> const& _ee,
        REAL_TYPE _bb,
        expansion <NH> & _hh
        ) // adapted from:     scale_expansion_zeroelim
    {
        REAL_TYPE _bh, _bl, _t1, _t0 , _ss, _hx, _qq;
        one_split(_bb, _bh, _bl) ;

        _hh._xlen = +0 ;

        one_one_mul_full(
            _ee[ 0 ], _bb, _bh, _bl, _qq, _hx) ;

        if (_hx != +0.0) _hh.push (_hx) ;

        INDX_TYPE _ei;
        for (_ei = +1; _ei < _ee._xlen; ++_ei)
        {
            one_one_mul_full(_ee[_ei], _bb, _bh, _bl,
                _t1, _t0) ;

            one_one_add_full(
                _qq, _t0, _ss, _hx);

            if (_hx != +0.0) _hh.push (_hx) ;

            one_one_add_fast(
                _t1, _ss, _qq, _hx);

            if (_hx != +0.0) _hh.push (_hx) ;
        }
        if((_qq != +0.0) || (_hh._xlen == +0))
        {
            _hh.push(_qq) ;
        }
    }

    template <
        size_t NA, size_t NC
             >
    __inline_call void      expansion_mul (
        expansion <NA> const& _aa ,
        REAL_TYPE _bb,
        expansion <NC> & _cc
        ) // adapted from:     scale_expansion_zeroelim
    {
        static_assert ( NC >= NA * +2 ,
            "expansion-mul: insufficient alloc.!");

        if (_aa._xlen == +1)        // 1-to-1 unrolling
        {
            REAL_TYPE _t1, _t0;

            _cc._xlen = +0 ;

            one_one_mul_full(
                _aa[0], _bb, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else
        if (_aa._xlen == +2)        // 2-to-1 unrolling
        {
            REAL_TYPE _t3, _t2, _t1, _t0;

            _cc._xlen = +0 ;

            two_one_mul_full(
            _aa[1], _aa[0], _bb, _t3, _t2, _t1, _t0);

            if (_t0 != +0.0) _cc.push (_t0) ;
            if (_t1 != +0.0) _cc.push (_t1) ;
            if (_t2 != +0.0) _cc.push (_t2) ;
            if (_t3 != +0.0) _cc.push (_t3) ;
            if (_cc.empty()) _cc.push (+0.) ;
        }
        else                        // the n-to-1 loops
        {
            scale_expansion_zeroelim (_aa, _bb, _cc);
        }
    }

    /*
    --------------------------------------------------------
     * multi-precision expansion product, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t NR
             >
    __normal_call void      expansion_mul (
        expansion <NA> const& _aa ,
        expansion <NB> const& _bb ,
        INDX_TYPE _i1, INDX_TYPE _i2 ,
        expansion <NC> & _cc
        ) // see shewchuk:    block-wise "distillation"
    {
        INDX_TYPE _nr = _i2 - _i1 + 1;
        if (_nr >= +3)              // recursive splits
        {
            if constexpr ( NR >= +3 )
            {
            INDX_TYPE _im = _i1 + _nr / 2 ;

            INDX_TYPE constexpr R1 = NR / 2 ;
            INDX_TYPE constexpr R2 = NR - R1;

            INDX_TYPE constexpr
                N1 = mul_alloc (R1, NA) ;
            INDX_TYPE constexpr
                N2 = mul_alloc (R2, NA) ;

            expansion<N1> _c1;
            expansion_mul<NA, NB, N1, R1>(
                _aa, _bb, _i1, _im - 1, _c1);

            expansion<N2> _c2;
            expansion_mul<NA, NB, N2, R2>(
                _aa, _bb, _im + 0, _i2, _c2);

            expansion_add(_c1, _c2, _cc) ;
            }
            else
            {
            assert( false &&
                "expansion-mul: distill fail");
            }
        }
        else
        if (_nr == +2)
        {
            if constexpr ( NR >= +2 )
            {
            expansion<mul_alloc(NA, 1)> _c1 ;
            expansion<mul_alloc(NA, 1)> _c2 ;
            expansion_mul(
                _aa, _bb [_i1 + 0], _c1) ;
            expansion_mul(
                _aa, _bb [_i1 + 1], _c2) ;

            expansion_add(_c1, _c2, _cc) ;
            }
            else
            {
            assert( false &&
                "expansion-mul: distill fail");
            }
        }
        else
        if (_nr == +1)              // do 1-by-n direct
        {
            expansion_mul(_aa, _bb [_i1], _cc);
        }
    }

    template <
        size_t NA, size_t NB, size_t NC
             >
    __inline_call void      expansion_mul (
        expansion <NA> const& _aa ,
        expansion <NB> const& _bb ,
        expansion <NC> & _cc
        ) // see shewchuk:    block-wise "distillation"
    {
        if (_aa._xlen < _bb._xlen)
        {
            expansion_mul<NB, NA, NC, NA> (
                _bb, _aa, 0, _aa._xlen-1, _cc);
        }
        else
        {
            expansion_mul<NA, NB, NC, NB> (
                _aa, _bb, 0, _bb._xlen-1, _cc);
        }
    }

    /*
    --------------------------------------------------------
     * -ve for multi-precision expansion, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NN
             >
    __normal_call void      expansion_neg (
        expansion <NN> & _aa
        )
    {
        INDX_TYPE _ii;
        for (_ii = +0; _ii < _aa._xlen; ++_ii)
        {
            _aa[_ii] *= -1 ;
        }
    }

    /*
    --------------------------------------------------------
     * est. of multi-precision expansion, a'la shewchuk
    --------------------------------------------------------
     */

    template <
        size_t NN
             >
    __normal_call REAL_TYPE expansion_est (
        expansion <NN> const& _aa
        )
    {
        REAL_TYPE _rr = +0.;
        INDX_TYPE _ii;
        for (_ii = +0; _ii < _aa._xlen; ++_ii)
        {
            _rr += _aa[_ii];
        }

        return    _rr ;
    }

    /*
    --------------------------------------------------------
     * form dot-products for multi-precision expansions
    --------------------------------------------------------
     */

    template <
        size_t AX, size_t BX, size_t AY,
        size_t BY, size_t NP
             >
    __inline_call void      expansion_dot (
        expansion <AX> const& _xa,
        expansion <BX> const& _xb,
        expansion <AY> const& _ya,
        expansion <BY> const& _yb,
        expansion <NP> & _dp
        )                           // 2-dim dotproduct
    {
        expansion<mul_alloc(AX,  BX)> _xp ;
        expansion_mul(_xa, _xb, _xp);

        expansion<mul_alloc(AY,  BY)> _yp ;
        expansion_mul(_ya, _yb, _yp);

        expansion_add(_xp, _yp, _dp);
    }

    template <
        size_t AX, size_t BX, size_t AY,
        size_t BY, size_t AZ, size_t BZ,
        size_t NP
             >
    __inline_call void      expansion_dot (
        expansion <AX> const& _xa,
        expansion <BX> const& _xb,
        expansion <AY> const& _ya,
        expansion <BY> const& _yb,
        expansion <AZ> const& _za,
        expansion <BZ> const& _zb,
        expansion <NP> & _dp
        )                           // 3-dim dotproduct
    {
        expansion<mul_alloc(AX,  BX)> _xp ;
        expansion_mul(_xa, _xb, _xp);

        expansion<mul_alloc(AY,  BY)> _yp ;
        expansion_mul(_ya, _yb, _yp);

        expansion<mul_alloc(AZ,  BZ)> _zp ;
        expansion_mul(_za, _zb, _zp);

        expansion_add(_xp, _yp, _zp,  _dp);
    }


#   undef REAL_TYPE
#   undef INDX_TYPE


    }

#   endif//__MP_FLOAT__



