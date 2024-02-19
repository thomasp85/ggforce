
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
     *   | ax  ay  dot(a, a)  +1. |
     *   | bx  by  dot(b, b)  +1. |
     *   | cx  cy  dot(c, c)  +1. |
     *   | dx  dy  dot(d, d)  +1. |
     *
     * This is the unweighted "in-ball" predicate in E^2.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE inball2d_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2d predicate, "exact" version */
        mp::expansion< 4 > _a_lift, _b_lift,
                           _c_lift, _d_lift;
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_ad_,
                           _d2_bc_, _d2_bd_,
                           _d2_cd_;
        mp::expansion< 12> _d3_abc, _d3_abd,
                           _d3_acd, _d3_bcd;
        mp::expansion<384> _d4full;

        _OK = true;

    /*-------------------------------------- lifted terms */
        mp::expansion_add(
            mp::expansion_from_sqr(_pa[ 0]),
            mp::expansion_from_sqr(_pa[ 1]),
            _a_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pb[ 0]),
            mp::expansion_from_sqr(_pb[ 1]),
            _b_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pc[ 0]),
            mp::expansion_from_sqr(_pc[ 1]),
            _c_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pd[ 0]),
            mp::expansion_from_sqr(_pd[ 1]),
            _d_lift ) ;

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
        compute_det_4x4(_d3_bcd, _a_lift,
                        _d3_acd, _b_lift,
                        _d3_abd, _c_lift,
                        _d3_abc, _d_lift,
                        _d4full, +3) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d4full) ;
    }

    __normal_call REAL_TYPE inball2d_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2d predicate, "bound" version */
        ia_flt    _adx, _ady, _adw ,
                  _bdx, _bdy, _bdw ,
                  _cdx, _cdy, _cdw ;
        ia_flt    _ali, _bli, _cli ;
        ia_flt    _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        ia_flt    _d33;

        ia_rnd    _rnd;                   // up rounding!

        _adx.from_sub(_pa[0], _pd[0]) ;   // coord. diff.
        _ady.from_sub(_pa[1], _pd[1]) ;
        _adw.from_sub(_pa[2], _pd[2]) ;

        _bdx.from_sub(_pb[0], _pd[0]) ;
        _bdy.from_sub(_pb[1], _pd[1]) ;
        _bdw.from_sub(_pb[2], _pd[2]) ;

        _cdx.from_sub(_pc[0], _pd[0]) ;
        _cdy.from_sub(_pc[1], _pd[1]) ;
        _cdw.from_sub(_pc[2], _pd[2]) ;

        _ali =  sqr (_adx) + sqr (_ady) ; // lifted terms

        _bli =  sqr (_bdx) + sqr (_bdy) ;

        _cli =  sqr (_cdx) + sqr (_cdy) ;

        _bdxcdy = _bdx * _cdy ;           // 2 x 2 minors
        _cdxbdy = _cdx * _bdy ;
        _cdxady = _cdx * _ady ;
        _adxcdy = _adx * _cdy ;
        _adxbdy = _adx * _bdy ;
        _bdxady = _bdx * _ady ;

        _d33 =                            // 3 x 3 result
          _ali * (_bdxcdy - _cdxbdy)
        + _bli * (_cdxady - _adxcdy)
        + _cli * (_adxbdy - _bdxady) ;

        _OK =
          _d33.lo() >= (REAL_TYPE)0.
        ||_d33.up() <= (REAL_TYPE)0. ;

        return ( _d33.mid() ) ;
    }

    __normal_call REAL_TYPE inball2d_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2d predicate, "float" version */
        REAL_TYPE static const _ER =
        + 11. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _adx, _ady, _ali ,
                  _bdx, _bdy, _bli ,
                  _cdx, _cdy, _cli ;
        REAL_TYPE _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        REAL_TYPE _BDXCDY, _CDXBDY ,
                  _CDXADY, _ADXCDY ,
                  _ADXBDY, _BDXADY ;

        REAL_TYPE _d33, _FT ;

        _adx = _pa [0] - _pd [0] ;        // coord. diff.
        _ady = _pa [1] - _pd [1] ;

        _bdx = _pb [0] - _pd [0] ;
        _bdy = _pb [1] - _pd [1] ;

        _cdx = _pc [0] - _pd [0] ;
        _cdy = _pc [1] - _pd [1] ;

        _ali = _adx * _adx + _ady * _ady; // lifted terms

        _bli = _bdx * _bdx + _bdy * _bdy;

        _cli = _cdx * _cdx + _cdy * _cdy;

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
          _ali * (_BDXCDY + _CDXBDY)
        + _bli * (_CDXADY + _ADXCDY)
        + _cli * (_ADXBDY + _BDXADY) ;

        _FT *= _ER ;

        _d33 =                            // 3 x 3 result
          _ali * (_bdxcdy - _cdxbdy)
        + _bli * (_cdxady - _adxcdy)
        + _cli * (_adxbdy - _bdxady) ;

        _OK  =
          _d33 > +_FT || _d33 < -_FT ;

        return ( _d33 ) ;
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact determinant using multi-precision
     * expansions, a'la shewchuk
     *
     *   | ax  ay  dot(a, a) - aw  +1. |
     *   | bx  by  dot(b, b) - bw  +1. |
     *   | cx  cy  dot(c, c) - cw  +1. |
     *   | dx  dy  dot(d, d) - dw  +1. |
     *
     * This is the weighted "in-ball" predicate in E^2.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE inball2w_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2w predicate, "exact" version */
        mp::expansion< 5 > _a_lift, _b_lift,
                           _c_lift, _d_lift;
        mp::expansion< 4 > _t_lift;
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_ad_,
                           _d2_bc_, _d2_bd_,
                           _d2_cd_;
        mp::expansion< 12> _d3_abc, _d3_abd,
                           _d3_acd, _d3_bcd;
        mp::expansion<480> _d4full;

        _OK = true;

    /*-------------------------------------- lifted terms */
        mp::expansion_add(
            mp::expansion_from_sqr(_pa[ 0]),
            mp::expansion_from_sqr(_pa[ 1]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pa[ 2] , _a_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pb[ 0]),
            mp::expansion_from_sqr(_pb[ 1]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pb[ 2] , _b_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pc[ 0]),
            mp::expansion_from_sqr(_pc[ 1]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pc[ 2] , _c_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pd[ 0]),
            mp::expansion_from_sqr(_pd[ 1]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pd[ 2] , _d_lift);

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
        compute_det_4x4(_d3_bcd, _a_lift,
                        _d3_acd, _b_lift,
                        _d3_abd, _c_lift,
                        _d3_abc, _d_lift,
                        _d4full, +3) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d4full) ;
    }

    __normal_call REAL_TYPE inball2w_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2w predicate, "bound" version */
        ia_flt    _adx, _ady, _adw ,
                  _bdx, _bdy, _bdw ,
                  _cdx, _cdy, _cdw ;
        ia_flt    _ali, _bli, _cli ;
        ia_flt    _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        ia_flt    _d33;

        ia_rnd    _rnd;                   // up rounding!

        _adx.from_sub(_pa[0], _pd[0]) ;   // coord. diff.
        _ady.from_sub(_pa[1], _pd[1]) ;
        _adw.from_sub(_pa[2], _pd[2]) ;

        _bdx.from_sub(_pb[0], _pd[0]) ;
        _bdy.from_sub(_pb[1], _pd[1]) ;
        _bdw.from_sub(_pb[2], _pd[2]) ;

        _cdx.from_sub(_pc[0], _pd[0]) ;
        _cdy.from_sub(_pc[1], _pd[1]) ;
        _cdw.from_sub(_pc[2], _pd[2]) ;

        _ali =  sqr (_adx) + sqr (_ady)   // lifted terms
             - _adw ;

        _bli =  sqr (_bdx) + sqr (_bdy)
             - _bdw ;

        _cli =  sqr (_cdx) + sqr (_cdy)
             - _cdw ;

        _bdxcdy = _bdx * _cdy ;           // 2 x 2 minors
        _cdxbdy = _cdx * _bdy ;
        _cdxady = _cdx * _ady ;
        _adxcdy = _adx * _cdy ;
        _adxbdy = _adx * _bdy ;
        _bdxady = _bdx * _ady ;

        _d33 =                            // 3 x 3 result
          _ali * (_bdxcdy - _cdxbdy)
        + _bli * (_cdxady - _adxcdy)
        + _cli * (_adxbdy - _bdxady) ;

        _OK =
          _d33.lo() >= (REAL_TYPE)0.
        ||_d33.up() <= (REAL_TYPE)0. ;

        return ( _d33.mid() ) ;
    }

    __normal_call REAL_TYPE inball2w_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
        bool_type &_OK
        )
    {
    /*--------------- inball2w predicate, "float" version */
        REAL_TYPE static const _ER =
        + 12. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _adx, _ady, _adw ,
                  _bdx, _bdy, _bdw ,
                  _cdx, _cdy, _cdw ;
        REAL_TYPE _ali, _bli, _cli ;
        REAL_TYPE _bdxcdy, _cdxbdy ,
                  _cdxady, _adxcdy ,
                  _adxbdy, _bdxady ;

        REAL_TYPE _ALI, _BLI, _CLI ;
        REAL_TYPE _BDXCDY, _CDXBDY ,
                  _CDXADY, _ADXCDY ,
                  _ADXBDY, _BDXADY ;

        REAL_TYPE _d33, _FT ;

        _adx = _pa [0] - _pd [0] ;        // coord. diff.
        _ady = _pa [1] - _pd [1] ;
        _adw = _pa [2] - _pd [2] ;

        _bdx = _pb [0] - _pd [0] ;
        _bdy = _pb [1] - _pd [1] ;
        _bdw = _pb [2] - _pd [2] ;

        _cdx = _pc [0] - _pd [0] ;
        _cdy = _pc [1] - _pd [1] ;
        _cdw = _pc [2] - _pd [2] ;

        _ali = _adx * _adx + _ady * _ady  // lifted terms
             - _adw ;

        _ALI = std::abs (_ali) ;

        _bli = _bdx * _bdx + _bdy * _bdy
             - _bdw ;

        _BLI = std::abs (_bli) ;

        _cli = _cdx * _cdx + _cdy * _cdy
             - _cdw ;

        _CLI = std::abs (_cli) ;

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
          _ALI * (_BDXCDY + _CDXBDY)
        + _BLI * (_CDXADY + _ADXCDY)
        + _CLI * (_ADXBDY + _BDXADY) ;

        _FT *= _ER ;

        _d33 =                            // 3 x 3 result
          _ali * (_bdxcdy - _cdxbdy)
        + _bli * (_cdxady - _adxcdy)
        + _cli * (_adxbdy - _bdxady) ;

        _OK  =
          _d33 > +_FT || _d33 < -_FT ;

        return ( _d33 ) ;
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact determinant using multi-precision
     * expansions, a'la shewchuk
     *
     *   | ax  ay  az  dot(a, a)  +1. |
     *   | bx  by  bz  dot(b, b)  +1. |
     *   | cx  cy  cz  dot(c, c)  +1. |
     *   | dx  dy  dz  dot(d, d)  +1. |
     *   | ex  ey  ez  dot(e, e)  +1. |
     *
     * This is the unweighted "in-ball" predicate in E^3.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE inball3d_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3d predicate, "exact" version */
        mp::expansion< 6 > _a_lift, _b_lift,
                           _c_lift, _d_lift,
                           _e_lift;
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_ad_, _d2_ae_,
                           _d2_bc_, _d2_bd_,
                           _d2_be_,
                           _d2_cd_, _d2_ce_,
                           _d2_de_;
        mp::expansion< 24> _d3_abc, _d3_abd,
                           _d3_abe,
                           _d3_acd, _d3_ace,
                           _d3_ade,
                           _d3_bcd, _d3_bce,
                           _d3_bde, _d3_cde;
        mp::expansion< 96> _d4abcd, _d4abce,
                           _d4abde, _d4acde,
                           _d4bcde;
        mp::expansion<5760>_d5full;

        _OK = true;

        mp::expansion< 1 > _pa_zz_(_pa[ 2]);
        mp::expansion< 1 > _pb_zz_(_pb[ 2]);
        mp::expansion< 1 > _pc_zz_(_pc[ 2]);
        mp::expansion< 1 > _pd_zz_(_pd[ 2]);
        mp::expansion< 1 > _pe_zz_(_pe[ 2]);

    /*-------------------------------------- lifted terms */
        mp::expansion_add(
            mp::expansion_from_sqr(_pa[ 0]),
            mp::expansion_from_sqr(_pa[ 1]),
            mp::expansion_from_sqr(_pa[ 2]),
            _a_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pb[ 0]),
            mp::expansion_from_sqr(_pb[ 1]),
            mp::expansion_from_sqr(_pb[ 2]),
            _b_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pc[ 0]),
            mp::expansion_from_sqr(_pc[ 1]),
            mp::expansion_from_sqr(_pc[ 2]),
            _c_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pd[ 0]),
            mp::expansion_from_sqr(_pd[ 1]),
            mp::expansion_from_sqr(_pd[ 2]),
            _d_lift ) ;

        mp::expansion_add(
            mp::expansion_from_sqr(_pe[ 0]),
            mp::expansion_from_sqr(_pe[ 1]),
            mp::expansion_from_sqr(_pe[ 2]),
            _e_lift ) ;

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

        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_ae_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_bc_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_bd_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_be_ ) ;

        compute_det_2x2(_pc[ 0], _pc[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_cd_ ) ;

        compute_det_2x2(_pc[ 0], _pc[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_ce_ ) ;

        compute_det_2x2(_pd[ 0], _pd[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_de_ ) ;

    /*-------------------------------------- 3 x 3 minors */
        compute_det_3x3(_d2_bc_, _pa_zz_,
                        _d2_ac_, _pb_zz_,
                        _d2_ab_, _pc_zz_,
                        _d3_abc, +3) ;

        compute_det_3x3(_d2_bd_, _pa_zz_,
                        _d2_ad_, _pb_zz_,
                        _d2_ab_, _pd_zz_,
                        _d3_abd, +3) ;

        compute_det_3x3(_d2_be_, _pa_zz_,
                        _d2_ae_, _pb_zz_,
                        _d2_ab_, _pe_zz_,
                        _d3_abe, +3) ;

        compute_det_3x3(_d2_cd_, _pa_zz_,
                        _d2_ad_, _pc_zz_,
                        _d2_ac_, _pd_zz_,
                        _d3_acd, +3) ;

        compute_det_3x3(_d2_ce_, _pa_zz_,
                        _d2_ae_, _pc_zz_,
                        _d2_ac_, _pe_zz_,
                        _d3_ace, +3) ;

        compute_det_3x3(_d2_de_, _pa_zz_,
                        _d2_ae_, _pd_zz_,
                        _d2_ad_, _pe_zz_,
                        _d3_ade, +3) ;

        compute_det_3x3(_d2_cd_, _pb_zz_,
                        _d2_bd_, _pc_zz_,
                        _d2_bc_, _pd_zz_,
                        _d3_bcd, +3) ;

        compute_det_3x3(_d2_ce_, _pb_zz_,
                        _d2_be_, _pc_zz_,
                        _d2_bc_, _pe_zz_,
                        _d3_bce, +3) ;

        compute_det_3x3(_d2_de_, _pb_zz_,
                        _d2_be_, _pd_zz_,
                        _d2_bd_, _pe_zz_,
                        _d3_bde, +3) ;

        compute_det_3x3(_d2_de_, _pc_zz_,
                        _d2_ce_, _pd_zz_,
                        _d2_cd_, _pe_zz_,
                        _d3_cde, +3) ;

    /*-------------------------------------- 4 x 4 minors */
        unitary_det_4x4(_d3_cde, _d3_bde,
                        _d3_bce, _d3_bcd,
                        _d4bcde, +4) ;

        unitary_det_4x4(_d3_cde, _d3_ade,
                        _d3_ace, _d3_acd,
                        _d4acde, +4) ;

        unitary_det_4x4(_d3_bde, _d3_ade,
                        _d3_abe, _d3_abd,
                        _d4abde, +4) ;

        unitary_det_4x4(_d3_bce, _d3_ace,
                        _d3_abe, _d3_abc,
                        _d4abce, +4) ;

        unitary_det_4x4(_d3_bcd, _d3_acd,
                        _d3_abd, _d3_abc,
                        _d4abcd, +4) ;

    /*-------------------------------------- 5 x 5 result */
        compute_det_5x5(_d4bcde, _a_lift,
                        _d4acde, _b_lift,
                        _d4abde, _c_lift,
                        _d4abce, _d_lift,
                        _d4abcd, _e_lift,
                        _d5full, +4) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d5full) ;
    }

    __normal_call REAL_TYPE inball3d_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3d predicate, "bound" version */
        ia_flt    _aex, _aey, _aez ,
                  _ali,
                  _bex, _bey, _bez ,
                  _bli,
                  _cex, _cey, _cez ,
                  _cli,
                  _dex, _dey, _dez ,
                  _dli;
        ia_flt    _aexbey, _bexaey ,
                  _aexcey, _cexaey ,
                  _bexcey, _cexbey ,
                  _cexdey, _dexcey ,
                  _dexaey, _aexdey ,
                  _bexdey, _dexbey ;
        ia_flt    _ab_, _bc_, _cd_, _da_,
                  _ac_, _bd_;
        ia_flt    _abc, _bcd, _cda, _dab;
        ia_flt    _d44;

        ia_rnd    _rnd;                   // up rounding!

        _aex.from_sub(_pa[0], _pe[0]) ;   // coord. diff.
        _aey.from_sub(_pa[1], _pe[1]) ;
        _aez.from_sub(_pa[2], _pe[2]) ;

        _bex.from_sub(_pb[0], _pe[0]) ;
        _bey.from_sub(_pb[1], _pe[1]) ;
        _bez.from_sub(_pb[2], _pe[2]) ;

        _cex.from_sub(_pc[0], _pe[0]) ;
        _cey.from_sub(_pc[1], _pe[1]) ;
        _cez.from_sub(_pc[2], _pe[2]) ;

        _dex.from_sub(_pd[0], _pe[0]) ;
        _dey.from_sub(_pd[1], _pe[1]) ;
        _dez.from_sub(_pd[2], _pe[2]) ;

        _ali = sqr (_aex) + sqr (_aey)    // lifted terms
             + sqr (_aez) ;

        _bli = sqr (_bex) + sqr (_bey)
             + sqr (_bez) ;

        _cli = sqr (_cex) + sqr (_cey)
             + sqr (_cez) ;

        _dli = sqr (_dex) + sqr (_dey)
             + sqr (_dez) ;

        _aexbey = _aex * _bey ;           // 2 x 2 minors
        _bexaey = _bex * _aey ;
        _ab_ = _aexbey - _bexaey ;

        _bexcey = _bex * _cey;
        _cexbey = _cex * _bey;
        _bc_ = _bexcey - _cexbey ;

        _cexdey = _cex * _dey;
        _dexcey = _dex * _cey;
        _cd_ = _cexdey - _dexcey ;

        _dexaey = _dex * _aey;
        _aexdey = _aex * _dey;
        _da_ = _dexaey - _aexdey ;

        _aexcey = _aex * _cey;
        _cexaey = _cex * _aey;
        _ac_ = _aexcey - _cexaey ;

        _bexdey = _bex * _dey;
        _dexbey = _dex * _bey;
        _bd_ = _bexdey - _dexbey ;

        _abc =                            // 3 x 3 minors
          _aez * _bc_ - _bez * _ac_
        + _cez * _ab_ ;

        _bcd =
          _bez * _cd_ - _cez * _bd_
        + _dez * _bc_ ;

        _cda =
          _cez * _da_ + _dez * _ac_
        + _aez * _cd_ ;

        _dab =
          _dez * _ab_ + _aez * _bd_
        + _bez * _da_ ;

        _d44 =                            // 4 x 4 result
          _dli * _abc - _cli * _dab
        + _bli * _cda - _ali * _bcd ;

        _OK =
          _d44.lo() >= (REAL_TYPE)0.
        ||_d44.up() <= (REAL_TYPE)0.;

        return ( _d44.mid() ) ;
    }

    __normal_call REAL_TYPE inball3d_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3d predicate, "float" version */
        REAL_TYPE static const _ER =
        + 17. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _aex, _aey, _aez ,
                  _ali,
                  _bex, _bey, _bez ,
                  _bli,
                  _cex, _cey, _cez ,
                  _cli,
                  _dex, _dey, _dez ,
                  _dli;
        REAL_TYPE _aexbey, _bexaey ,
                  _aexcey, _cexaey ,
                  _bexcey, _cexbey ,
                  _cexdey, _dexcey ,
                  _dexaey, _aexdey ,
                  _bexdey, _dexbey ;
        REAL_TYPE _ab_, _bc_, _cd_, _da_,
                  _ac_, _bd_;
        REAL_TYPE _abc, _bcd, _cda, _dab;

        REAL_TYPE _AEZ, _BEZ, _CEZ, _DEZ;
        REAL_TYPE _AEXBEY, _BEXAEY ,
                  _AEXCEY, _CEXAEY ,
                  _BEXCEY, _CEXBEY ,
                  _CEXDEY, _DEXCEY ,
                  _DEXAEY, _AEXDEY ,
                  _BEXDEY, _DEXBEY ;
        REAL_TYPE _AB_, _BC_, _CD_, _DA_,
                  _AC_, _BD_;
        REAL_TYPE _ABC, _BCD, _CDA, _DAB;

        REAL_TYPE _d44, _FT ;

        _aex = _pa [0] - _pe [0] ;        // coord. diff.
        _aey = _pa [1] - _pe [1] ;
        _aez = _pa [2] - _pe [2] ;

        _AEZ = std::abs (_aez) ;

        _bex = _pb [0] - _pe [0] ;
        _bey = _pb [1] - _pe [1] ;
        _bez = _pb [2] - _pe [2] ;

        _BEZ = std::abs (_bez) ;

        _cex = _pc [0] - _pe [0] ;
        _cey = _pc [1] - _pe [1] ;
        _cez = _pc [2] - _pe [2] ;

        _CEZ = std::abs (_cez) ;

        _dex = _pd [0] - _pe [0] ;
        _dey = _pd [1] - _pe [1] ;
        _dez = _pd [2] - _pe [2] ;

        _DEZ = std::abs (_dez) ;

        _ali = _aex * _aex + _aey * _aey  // lifted terms
             + _aez * _aez ;

        _bli = _bex * _bex + _bey * _bey
             + _bez * _bez ;

        _cli = _cex * _cex + _cey * _cey
             + _cez * _cez ;

        _dli = _dex * _dex + _dey * _dey
             + _dez * _dez ;

        _aexbey = _aex * _bey ;           // 2 x 2 minors
        _bexaey = _bex * _aey ;
        _ab_ = _aexbey - _bexaey ;

        _AEXBEY = std::abs (_aexbey) ;
        _BEXAEY = std::abs (_bexaey) ;
        _AB_ = _AEXBEY + _BEXAEY ;

        _bexcey = _bex * _cey;
        _cexbey = _cex * _bey;
        _bc_ = _bexcey - _cexbey ;

        _BEXCEY = std::abs (_bexcey) ;
        _CEXBEY = std::abs (_cexbey) ;
        _BC_ = _BEXCEY + _CEXBEY ;

        _cexdey = _cex * _dey;
        _dexcey = _dex * _cey;
        _cd_ = _cexdey - _dexcey ;

        _CEXDEY = std::abs (_cexdey) ;
        _DEXCEY = std::abs (_dexcey) ;
        _CD_ = _CEXDEY + _DEXCEY ;

        _dexaey = _dex * _aey;
        _aexdey = _aex * _dey;
        _da_ = _dexaey - _aexdey ;

        _DEXAEY = std::abs (_dexaey) ;
        _AEXDEY = std::abs (_aexdey) ;
        _DA_ = _DEXAEY + _AEXDEY ;

        _aexcey = _aex * _cey;
        _cexaey = _cex * _aey;
        _ac_ = _aexcey - _cexaey ;

        _AEXCEY = std::abs (_aexcey) ;
        _CEXAEY = std::abs (_cexaey) ;
        _AC_ = _AEXCEY + _CEXAEY ;

        _bexdey = _bex * _dey;
        _dexbey = _dex * _bey;
        _bd_ = _bexdey - _dexbey ;

        _BEXDEY = std::abs (_bexdey) ;
        _DEXBEY = std::abs (_dexbey) ;
        _BD_ = _BEXDEY + _DEXBEY ;

        _abc =                            // 3 x 3 minors
          _aez * _bc_ - _bez * _ac_
        + _cez * _ab_ ;
        _ABC =
          _AEZ * _BC_ + _BEZ * _AC_
        + _CEZ * _AB_ ;

        _bcd =
          _bez * _cd_ - _cez * _bd_
        + _dez * _bc_ ;
        _BCD =
          _BEZ * _CD_ + _CEZ * _BD_
        + _DEZ * _BC_ ;

        _cda =
          _cez * _da_ + _dez * _ac_
        + _aez * _cd_ ;
        _CDA =
          _CEZ * _DA_ + _DEZ * _AC_
        + _AEZ * _CD_ ;

        _dab =
          _dez * _ab_ + _aez * _bd_
        + _bez * _da_ ;
        _DAB =
          _DEZ * _AB_ + _AEZ * _BD_
        + _BEZ * _DA_ ;

        _FT  =                            // roundoff tol
          _dli * _ABC + _cli * _DAB
        + _bli * _CDA + _ali * _BCD ;

        _FT *= _ER ;

        _d44 =                            // 4 x 4 result
          _dli * _abc - _cli * _dab
        + _bli * _cda - _ali * _bcd ;

        _OK  =
          _d44 > _FT || _d44 < -_FT ;

        return ( _d44 ) ;
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact determinant using multi-precision
     * expansions, a'la shewchuk
     *
     *   | ax  ay  az  dot(a, a) - aw  +1. |
     *   | bx  by  bz  dot(b, b) - bw  +1. |
     *   | cx  cy  cz  dot(c, c) - cw  +1. |
     *   | dx  dy  dz  dot(d, d) - dw  +1. |
     *   | ex  ey  ez  dot(e, e) - ew  +1. |
     *
     * This is the weighted "in-ball" predicate in E^3.
     *
    --------------------------------------------------------
     */

    __normal_call REAL_TYPE inball3w_e (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3w predicate, "exact" version */
        mp::expansion< 7 > _a_lift, _b_lift,
                           _c_lift, _d_lift,
                           _e_lift;
        mp::expansion< 6 > _t_lift;
        mp::expansion< 4 > _d2_ab_, _d2_ac_,
                           _d2_ad_, _d2_ae_,
                           _d2_bc_, _d2_bd_,
                           _d2_be_,
                           _d2_cd_, _d2_ce_,
                           _d2_de_;
        mp::expansion< 24> _d3_abc, _d3_abd,
                           _d3_abe,
                           _d3_acd, _d3_ace,
                           _d3_ade,
                           _d3_bcd, _d3_bce,
                           _d3_bde, _d3_cde;
        mp::expansion< 96> _d4abcd, _d4abce,
                           _d4abde, _d4acde,
                           _d4bcde;
        mp::expansion<6720>_d5full;

        _OK = true;

        mp::expansion< 1 > _pa_zz_(_pa[ 2]);
        mp::expansion< 1 > _pb_zz_(_pb[ 2]);
        mp::expansion< 1 > _pc_zz_(_pc[ 2]);
        mp::expansion< 1 > _pd_zz_(_pd[ 2]);
        mp::expansion< 1 > _pe_zz_(_pe[ 2]);

    /*-------------------------------------- lifted terms */
        mp::expansion_add(
            mp::expansion_from_sqr(_pa[ 0]),
            mp::expansion_from_sqr(_pa[ 1]),
            mp::expansion_from_sqr(_pa[ 2]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pa[ 3] , _a_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pb[ 0]),
            mp::expansion_from_sqr(_pb[ 1]),
            mp::expansion_from_sqr(_pb[ 2]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pb[ 3] , _b_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pc[ 0]),
            mp::expansion_from_sqr(_pc[ 1]),
            mp::expansion_from_sqr(_pc[ 2]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pc[ 3] , _c_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pd[ 0]),
            mp::expansion_from_sqr(_pd[ 1]),
            mp::expansion_from_sqr(_pd[ 2]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pd[ 3] , _d_lift);

        mp::expansion_add(
            mp::expansion_from_sqr(_pe[ 0]),
            mp::expansion_from_sqr(_pe[ 1]),
            mp::expansion_from_sqr(_pe[ 2]),
            _t_lift ) ;
        mp::expansion_sub(
            _t_lift , _pe[ 3] , _e_lift);

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

        compute_det_2x2(_pa[ 0], _pa[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_ae_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pc[ 0], _pc[ 1],
                        _d2_bc_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_bd_ ) ;

        compute_det_2x2(_pb[ 0], _pb[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_be_ ) ;

        compute_det_2x2(_pc[ 0], _pc[ 1],
                        _pd[ 0], _pd[ 1],
                        _d2_cd_ ) ;

        compute_det_2x2(_pc[ 0], _pc[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_ce_ ) ;

        compute_det_2x2(_pd[ 0], _pd[ 1],
                        _pe[ 0], _pe[ 1],
                        _d2_de_ ) ;

    /*-------------------------------------- 3 x 3 minors */
        compute_det_3x3(_d2_bc_, _pa_zz_,
                        _d2_ac_, _pb_zz_,
                        _d2_ab_, _pc_zz_,
                        _d3_abc, +3) ;

        compute_det_3x3(_d2_bd_, _pa_zz_,
                        _d2_ad_, _pb_zz_,
                        _d2_ab_, _pd_zz_,
                        _d3_abd, +3) ;

        compute_det_3x3(_d2_be_, _pa_zz_,
                        _d2_ae_, _pb_zz_,
                        _d2_ab_, _pe_zz_,
                        _d3_abe, +3) ;

        compute_det_3x3(_d2_cd_, _pa_zz_,
                        _d2_ad_, _pc_zz_,
                        _d2_ac_, _pd_zz_,
                        _d3_acd, +3) ;

        compute_det_3x3(_d2_ce_, _pa_zz_,
                        _d2_ae_, _pc_zz_,
                        _d2_ac_, _pe_zz_,
                        _d3_ace, +3) ;

        compute_det_3x3(_d2_de_, _pa_zz_,
                        _d2_ae_, _pd_zz_,
                        _d2_ad_, _pe_zz_,
                        _d3_ade, +3) ;

        compute_det_3x3(_d2_cd_, _pb_zz_,
                        _d2_bd_, _pc_zz_,
                        _d2_bc_, _pd_zz_,
                        _d3_bcd, +3) ;

        compute_det_3x3(_d2_ce_, _pb_zz_,
                        _d2_be_, _pc_zz_,
                        _d2_bc_, _pe_zz_,
                        _d3_bce, +3) ;

        compute_det_3x3(_d2_de_, _pb_zz_,
                        _d2_be_, _pd_zz_,
                        _d2_bd_, _pe_zz_,
                        _d3_bde, +3) ;

        compute_det_3x3(_d2_de_, _pc_zz_,
                        _d2_ce_, _pd_zz_,
                        _d2_cd_, _pe_zz_,
                        _d3_cde, +3) ;

    /*-------------------------------------- 4 x 4 minors */
        unitary_det_4x4(_d3_cde, _d3_bde,
                        _d3_bce, _d3_bcd,
                        _d4bcde, +4) ;

        unitary_det_4x4(_d3_cde, _d3_ade,
                        _d3_ace, _d3_acd,
                        _d4acde, +4) ;

        unitary_det_4x4(_d3_bde, _d3_ade,
                        _d3_abe, _d3_abd,
                        _d4abde, +4) ;

        unitary_det_4x4(_d3_bce, _d3_ace,
                        _d3_abe, _d3_abc,
                        _d4abce, +4) ;

        unitary_det_4x4(_d3_bcd, _d3_acd,
                        _d3_abd, _d3_abc,
                        _d4abcd, +4) ;

    /*-------------------------------------- 5 x 5 result */
        compute_det_5x5(_d4bcde, _a_lift,
                        _d4acde, _b_lift,
                        _d4abde, _c_lift,
                        _d4abce, _d_lift,
                        _d4abcd, _e_lift,
                        _d5full, +4) ;

    /*-------------------------------------- leading det. */
        return mp::expansion_est(_d5full) ;
    }

    __normal_call REAL_TYPE inball3w_i (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3w predicate, "bound" version */
        ia_flt    _aex, _aey, _aez ,
                  _aew, _ali,
                  _bex, _bey, _bez ,
                  _bew, _bli,
                  _cex, _cey, _cez ,
                  _cew, _cli,
                  _dex, _dey, _dez ,
                  _dew, _dli;
        ia_flt    _aexbey, _bexaey ,
                  _aexcey, _cexaey ,
                  _bexcey, _cexbey ,
                  _cexdey, _dexcey ,
                  _dexaey, _aexdey ,
                  _bexdey, _dexbey ;
        ia_flt    _ab_, _bc_, _cd_, _da_,
                  _ac_, _bd_;
        ia_flt    _abc, _bcd, _cda, _dab;
        ia_flt    _d44;

        ia_rnd    _rnd;                   // up rounding!

        _aex.from_sub(_pa[0], _pe[0]) ;   // coord. diff.
        _aey.from_sub(_pa[1], _pe[1]) ;
        _aez.from_sub(_pa[2], _pe[2]) ;
        _aew.from_sub(_pa[3], _pe[3]) ;

        _bex.from_sub(_pb[0], _pe[0]) ;
        _bey.from_sub(_pb[1], _pe[1]) ;
        _bez.from_sub(_pb[2], _pe[2]) ;
        _bew.from_sub(_pb[3], _pe[3]) ;

        _cex.from_sub(_pc[0], _pe[0]) ;
        _cey.from_sub(_pc[1], _pe[1]) ;
        _cez.from_sub(_pc[2], _pe[2]) ;
        _cew.from_sub(_pc[3], _pe[3]) ;

        _dex.from_sub(_pd[0], _pe[0]) ;
        _dey.from_sub(_pd[1], _pe[1]) ;
        _dez.from_sub(_pd[2], _pe[2]) ;
        _dew.from_sub(_pd[3], _pe[3]) ;

        _ali =  sqr(_aex) +  sqr(_aey)    // lifted terms
             +  sqr(_aez) - _aew ;

        _bli =  sqr(_bex) +  sqr(_bey)
             +  sqr(_bez) - _bew ;

        _cli =  sqr(_cex) +  sqr(_cey)
             +  sqr(_cez) - _cew ;

        _dli =  sqr(_dex) +  sqr(_dey)
             +  sqr(_dez) - _dew ;

        _aexbey = _aex * _bey ;           // 2 x 2 minors
        _bexaey = _bex * _aey ;
        _ab_ = _aexbey - _bexaey ;

        _bexcey = _bex * _cey;
        _cexbey = _cex * _bey;
        _bc_ = _bexcey - _cexbey ;

        _cexdey = _cex * _dey;
        _dexcey = _dex * _cey;
        _cd_ = _cexdey - _dexcey ;

        _dexaey = _dex * _aey;
        _aexdey = _aex * _dey;
        _da_ = _dexaey - _aexdey ;

        _aexcey = _aex * _cey;
        _cexaey = _cex * _aey;
        _ac_ = _aexcey - _cexaey ;

        _bexdey = _bex * _dey;
        _dexbey = _dex * _bey;
        _bd_ = _bexdey - _dexbey ;

        _abc =                            // 3 x 3 minors
          _aez * _bc_ - _bez * _ac_
        + _cez * _ab_ ;

        _bcd =
          _bez * _cd_ - _cez * _bd_
        + _dez * _bc_ ;

        _cda =
          _cez * _da_ + _dez * _ac_
        + _aez * _cd_ ;

        _dab =
          _dez * _ab_ + _aez * _bd_
        + _bez * _da_ ;

        _d44 =                            // 4 x 4 result
          _dli * _abc - _cli * _dab
        + _bli * _cda - _ali * _bcd ;

        _OK =
          _d44.lo() >= (REAL_TYPE)0.
        ||_d44.up() <= (REAL_TYPE)0.;

        return ( _d44.mid() ) ;
    }

    __normal_call REAL_TYPE inball3w_f (
      __const_ptr(REAL_TYPE) _pa ,
      __const_ptr(REAL_TYPE) _pb ,
      __const_ptr(REAL_TYPE) _pc ,
      __const_ptr(REAL_TYPE) _pd ,
      __const_ptr(REAL_TYPE) _pe ,
        bool_type &_OK
        )
    {
    /*--------------- inball3w predicate, "float" version */
        REAL_TYPE static const _ER =
        + 18. * std::pow(mp::_epsilon, 1) ;

        REAL_TYPE _aex, _aey, _aez ,
                  _aew, _ali,
                  _bex, _bey, _bez ,
                  _bew, _bli,
                  _cex, _cey, _cez ,
                  _cew, _cli,
                  _dex, _dey, _dez ,
                  _dew, _dli;
        REAL_TYPE _aexbey, _bexaey ,
                  _aexcey, _cexaey ,
                  _bexcey, _cexbey ,
                  _cexdey, _dexcey ,
                  _dexaey, _aexdey ,
                  _bexdey, _dexbey ;
        REAL_TYPE _ab_, _bc_, _cd_, _da_,
                  _ac_, _bd_;
        REAL_TYPE _abc, _bcd, _cda, _dab;

        REAL_TYPE _AEZ, _BEZ, _CEZ, _DEZ;
        REAL_TYPE _ALI, _BLI, _CLI, _DLI;
        REAL_TYPE _AEXBEY, _BEXAEY ,
                  _CEXAEY, _AEXCEY ,
                  _BEXCEY, _CEXBEY ,
                  _CEXDEY, _DEXCEY ,
                  _DEXAEY, _AEXDEY ,
                  _BEXDEY, _DEXBEY ;
        REAL_TYPE _AB_, _BC_, _CD_, _DA_,
                  _AC_, _BD_;
        REAL_TYPE _ABC, _BCD, _CDA, _DAB;

        REAL_TYPE _d44, _FT ;

        _aex = _pa [0] - _pe [0] ;        // coord. diff.
        _aey = _pa [1] - _pe [1] ;
        _aez = _pa [2] - _pe [2] ;
        _aew = _pa [3] - _pe [3] ;

        _AEZ = std::abs (_aez) ;

        _bex = _pb [0] - _pe [0] ;
        _bey = _pb [1] - _pe [1] ;
        _bez = _pb [2] - _pe [2] ;
        _bew = _pb [3] - _pe [3] ;

        _BEZ = std::abs (_bez) ;

        _cex = _pc [0] - _pe [0] ;
        _cey = _pc [1] - _pe [1] ;
        _cez = _pc [2] - _pe [2] ;
        _cew = _pc [3] - _pe [3] ;

        _CEZ = std::abs (_cez) ;

        _dex = _pd [0] - _pe [0] ;
        _dey = _pd [1] - _pe [1] ;
        _dez = _pd [2] - _pe [2] ;
        _dew = _pd [3] - _pe [3] ;

        _DEZ = std::abs (_dez) ;

        _ali = _aex * _aex + _aey * _aey  // lifted terms
             + _aez * _aez - _aew ;

        _ALI = std::abs (_ali) ;

        _bli = _bex * _bex + _bey * _bey
             + _bez * _bez - _bew ;

        _BLI = std::abs (_bli) ;

        _cli = _cex * _cex + _cey * _cey
             + _cez * _cez - _cew ;

        _CLI = std::abs (_cli) ;

        _dli = _dex * _dex + _dey * _dey
             + _dez * _dez - _dew ;

        _DLI = std::abs (_dli) ;

        _aexbey = _aex * _bey ;           // 2 x 2 minors
        _bexaey = _bex * _aey ;
        _ab_ = _aexbey - _bexaey ;

        _AEXBEY = std::abs (_aexbey) ;
        _BEXAEY = std::abs (_bexaey) ;
        _AB_ = _AEXBEY + _BEXAEY ;

        _bexcey = _bex * _cey;
        _cexbey = _cex * _bey;
        _bc_ = _bexcey - _cexbey ;

        _BEXCEY = std::abs (_bexcey) ;
        _CEXBEY = std::abs (_cexbey) ;
        _BC_ = _BEXCEY + _CEXBEY ;

        _cexdey = _cex * _dey;
        _dexcey = _dex * _cey;
        _cd_ = _cexdey - _dexcey ;

        _CEXDEY = std::abs (_cexdey) ;
        _DEXCEY = std::abs (_dexcey) ;
        _CD_ = _CEXDEY + _DEXCEY ;

        _dexaey = _dex * _aey;
        _aexdey = _aex * _dey;
        _da_ = _dexaey - _aexdey ;

        _DEXAEY = std::abs (_dexaey) ;
        _AEXDEY = std::abs (_aexdey) ;
        _DA_ = _DEXAEY + _AEXDEY ;

        _aexcey = _aex * _cey;
        _cexaey = _cex * _aey;
        _ac_ = _aexcey - _cexaey ;

        _AEXCEY = std::abs (_aexcey) ;
        _CEXAEY = std::abs (_cexaey) ;
        _AC_ = _AEXCEY + _CEXAEY ;

        _bexdey = _bex * _dey;
        _dexbey = _dex * _bey;
        _bd_ = _bexdey - _dexbey ;

        _BEXDEY = std::abs (_bexdey) ;
        _DEXBEY = std::abs (_dexbey) ;
        _BD_ = _BEXDEY + _DEXBEY ;

        _abc =                            // 3 x 3 minors
          _aez * _bc_ - _bez * _ac_
        + _cez * _ab_ ;
        _ABC =
          _AEZ * _BC_ + _BEZ * _AC_
        + _CEZ * _AB_ ;

        _bcd =
          _bez * _cd_ - _cez * _bd_
        + _dez * _bc_ ;
        _BCD =
          _BEZ * _CD_ + _CEZ * _BD_
        + _DEZ * _BC_ ;

        _cda =
          _cez * _da_ + _dez * _ac_
        + _aez * _cd_ ;
        _CDA =
          _CEZ * _DA_ + _DEZ * _AC_
        + _AEZ * _CD_ ;

        _dab =
          _dez * _ab_ + _aez * _bd_
        + _bez * _da_ ;
        _DAB =
          _DEZ * _AB_ + _AEZ * _BD_
        + _BEZ * _DA_ ;

        _FT  =                            // roundoff tol
          _DLI * _ABC + _CLI * _DAB
        + _BLI * _CDA + _ALI * _BCD ;

        _FT *= _ER ;

        _d44 =                            // 4 x 4 result
          _dli * _abc - _cli * _dab
        + _bli * _cda - _ali * _bcd ;

        _OK  =
          _d44 > _FT || _d44 < -_FT ;

        return ( _d44 ) ;
    }



