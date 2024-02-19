
    /*
    --------------------------------------------------------
     * PREDICATE-k: robust geometric predicates in E^k.
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
     * Last updated: 14 April, 2020
     *
     * Copyright 2020--
     * Darren Engwirda
     * de2363@columbia.edu
     * https://github.com/dengwirda/
     *
    --------------------------------------------------------
     */

    // from predicate_k.hpp...


    /*
    --------------------------------------------------------
     *
     * Compute an exact determinant using multi-precision
     * expansions, a'la shewchuk
     *
     *   | ax  ay  +1. |
     *   | bx  by  +1. |
     *   | cx  cy  +1. |
     *
     * This is the planar "orientation" predicate in E^2.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE orient2d_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
        bool_type &_OK
        )
    {
    /*--------------- orient2d predicate, "exact" version */
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_bc_;
        mp::expansion< 12> _d3full;

        _OK = true;

    /*-------------------------------------- 2 x 2 minors */
        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pb[ 0], _pb[ 1],
                        _d2_ab_ ) ;

        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_ac_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_bc_ ) ;

    /*-------------------------------------- 3 x 3 result */
        unitary_det_3x3(_d2_bc_, _d2_ac_,
                        _d2_ab_,
                        _d3full, +3) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d3full) ;
    }

    __normal_call REAL_TYPE orient2d_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
        bool_type &_OK
        )
    {
    /*--------------- orient2d predicate, "bound" version */
        ia_flt    _acx, _acy ;
        ia_flt    _bcx, _bcy ;
        ia_flt    _acxbcy, _acybcx ;

        ia_flt    _sgn;

        ia_rnd    _rnd;                   // up rounding!

        _acx.from_sub(_pa[0], _pc[0]) ;   // coord. diff.
        _acy.from_sub(_pa[1], _pc[1]) ;

        _bcx.from_sub(_pb[0], _pc[0]) ;
        _bcy.from_sub(_pb[1], _pc[1]) ;

        _acxbcy = _acx * _bcy ;
        _acybcx = _acy * _bcx ;

        _sgn = _acxbcy - _acybcx ;        // 2 x 2 result

        _OK  =
           _sgn.lo() >= (REAL_TYPE)0.
        || _sgn.up() <= (REAL_TYPE)0. ;

        return ( _sgn.mid() ) ;
    }

    __normal_call REAL_TYPE orient2d_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
        bool_type &_OK
        )
    {
    /*--------------- orient2d predicate, "float" version */
        REAL_TYPE static const _ER =
        +  4. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _acx, _acy ;
        REAL_TYPE _bcx, _bcy ;
        REAL_TYPE _acxbcy, _acybcx ;

        REAL_TYPE _ACXBCY, _ACYBCX ;

        REAL_TYPE _sgn, _FT;

        _acx = _pa [0] - _pc [0] ;        // coord. diff.
        _acy = _pa [1] - _pc [1] ;

        _bcx = _pb [0] - _pc [0] ;
        _bcy = _pb [1] - _pc [1] ;

        _acxbcy = _acx * _bcy ;
        _acybcx = _acy * _bcx ;

        _ACXBCY = std::abs(_acxbcy);
        _ACYBCX = std::abs(_acybcx);

        _FT  = _ACXBCY + _ACYBCX ;        // roundoff tol
        _FT *= _ER ;

        _sgn = _acxbcy - _acybcx ;        // 2 x 2 result

        _OK  =
          _sgn > +_FT || _sgn < -_FT ;

        return ( _sgn ) ;
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact determinant using multi-precision
     * expansions, a'la shewchuk
     *
     *   | ax  ay  az  +1. |
     *   | bx  by  bz  +1. |
     *   | cx  cy  cz  +1. |
     *   | dx  dy  dz  +1. |
     *
     * This is the planar "orientation" predicate in E^3.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE orient3d_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- orient3d predicate, "exact" version */
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_ad_,
                           _d2_bc_, _d2_bd_,
                           _d2_cd_;
        mp::expansion< 12> _d3_abc, _d3_abd,
                           _d3_acd, _d3_bcd;
        mp::expansion< 96> _d4full;

        _OK = true;

        mp::expansion< 1 > _pa_zz_(_pa[ 2]);
        mp::expansion< 1 > _pb_zz_(_pb[ 2]);
        mp::expansion< 1 > _pc_zz_(_pc[ 2]);
        mp::expansion< 1 > _pd_zz_(_pd[ 2]);

    /*-------------------------------------- 2 x 2 minors */
        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pb[ 0], _pb[ 1],
                        _d2_ab_ ) ;

        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_ac_ ) ;

        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_ad_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_bc_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_bd_ ) ;

        compute_det_2x2(_pc[ 0], _pc[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_cd_ ) ;

    /*-------------------------------------- 3 x 3 minors */
        unitary_det_3x3(_d2_cd_, _d2_bd_,
                        _d2_bc_,
                        _d3_bcd, +3) ;

        unitary_det_3x3(_d2_cd_, _d2_ad_,
                        _d2_ac_,
                        _d3_acd, +3) ;

        unitary_det_3x3(_d2_bd_, _d2_ad_,
                        _d2_ab_,
                        _d3_abd, +3) ;

        unitary_det_3x3(_d2_bc_, _d2_ac_,
                        _d2_ab_,
                        _d3_abc, +3) ;

    /*-------------------------------------- 4 x 4 result */
        compute_det_4x4(_d3_bcd, _pa_zz_,
                        _d3_acd, _pb_zz_,
                        _d3_abd, _pc_zz_,
                        _d3_abc, _pd_zz_,
                        _d4full, +3) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d4full) ;
    }

    __normal_call REAL_TYPE orient3d_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- orient3d predicate, "bound" version */
        ia_flt    _adx, _ady, _adz ,
                  _bdx, _bdy, _bdz ,
                  _cdx, _cdy, _cdz ;
        ia_flt    _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        ia_flt    _sgn;

        ia_rnd    _rnd;                   // up rounding!

        _adx.from_sub(_pa[0], _pd[0]) ;   // coord. diff.
        _ady.from_sub(_pa[1], _pd[1]) ;
        _adz.from_sub(_pa[2], _pd[2]) ;

        _bdx.from_sub(_pb[0], _pd[0]) ;
        _bdy.from_sub(_pb[1], _pd[1]) ;
        _bdz.from_sub(_pb[2], _pd[2]) ;

        _cdx.from_sub(_pc[0], _pd[0]) ;
        _cdy.from_sub(_pc[1], _pd[1]) ;
        _cdz.from_sub(_pc[2], _pd[2]) ;

        _bdxcdy = _bdx * _cdy ;           // 2 x 2 minors
        _cdxbdy = _cdx * _bdy ;
        _cdxady = _cdx * _ady ;
        _adxcdy = _adx * _cdy ;
        _adxbdy = _adx * _bdy ;
        _bdxady = _bdx * _ady ;

        _sgn =                            // 3 x 3 result
          _adz * (_bdxcdy - _cdxbdy)
        + _bdz * (_cdxady - _adxcdy)
        + _cdz * (_adxbdy - _bdxady);

        _OK  =
          _sgn.lo() >= (REAL_TYPE)0.
        ||_sgn.up() <= (REAL_TYPE)0.;

        return ( _sgn.mid() ) ;
    }

    __normal_call REAL_TYPE orient3d_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- orient3d predicate, "float" version */
        REAL_TYPE static const _ER =
        +  8. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _adx, _ady, _adz ,
                  _bdx, _bdy, _bdz ,
                  _cdx, _cdy, _cdz ;
        REAL_TYPE _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        REAL_TYPE _ADZ, _BDZ, _CDZ ;
        REAL_TYPE _BDXCDY, _CDXBDY ,
                  _CDXADY, _ADXCDY ,
                  _ADXBDY, _BDXADY ;

        REAL_TYPE _sgn, _FT;

        _adx = _pa [0] - _pd [0] ;        // coord. diff.
        _ady = _pa [1] - _pd [1] ;
        _adz = _pa [2] - _pd [2] ;

        _ADZ = std::abs (_adz) ;

        _bdx = _pb [0] - _pd [0] ;
        _bdy = _pb [1] - _pd [1] ;
        _bdz = _pb [2] - _pd [2] ;

        _BDZ = std::abs (_bdz) ;

        _cdx = _pc [0] - _pd [0] ;
        _cdy = _pc [1] - _pd [1] ;
        _cdz = _pc [2] - _pd [2] ;

        _CDZ = std::abs (_cdz) ;

        _bdxcdy = _bdx * _cdy ;           // 2 x 2 minors
        _cdxbdy = _cdx * _bdy ;
        _cdxady = _cdx * _ady ;
        _adxcdy = _adx * _cdy ;
        _adxbdy = _adx * _bdy ;
        _bdxady = _bdx * _ady ;

        _BDXCDY = std::abs (_bdxcdy) ;
        _CDXBDY = std::abs (_cdxbdy) ;
        _CDXADY = std::abs (_cdxady) ;
        _ADXCDY = std::abs (_adxcdy) ;
        _ADXBDY = std::abs (_adxbdy) ;
        _BDXADY = std::abs (_bdxady) ;

        _FT  =                            // roundoff tol
          _ADZ * (_BDXCDY + _CDXBDY)
        + _BDZ * (_CDXADY + _ADXCDY)
        + _CDZ * (_ADXBDY + _BDXADY) ;

        _FT *= _ER ;

        _sgn =                            // 3 x 3 result
          _adz * (_bdxcdy - _cdxbdy)
        + _bdz * (_cdxady - _adxcdy)
        + _cdz * (_adxbdy - _bdxady) ;

        _OK  =
          _sgn > +_FT || _sgn < -_FT ;

        return ( _sgn ) ;
    }



