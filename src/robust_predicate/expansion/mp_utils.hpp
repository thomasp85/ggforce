
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
     * Last updated: 03 March, 2020
     *
     * Copyright 2020--
     * Darren Engwirda
     * de2363@columbia.edu
     * https://github.com/dengwirda/
     *
    --------------------------------------------------------
     */

#   pragma once

#   ifndef __MP_UTILS__
#   define __MP_UTILS__

    namespace mp_float {

#   define REAL_TYPE mp_float::real_type
#   define INDX_TYPE mp_float::indx_type

    /*---------------- compute an exact 2 x 2 determinant */

    template <
        size_t NN
             >
    __inline_call void compute_det_2x2 (
        REAL_TYPE _aa, REAL_TYPE _bb ,
        REAL_TYPE _cc, REAL_TYPE _dd ,
        expansion <NN> & _final
        )
    {
        expansion< 2 >_mulad, _mulbc ;
        _mulad.from_mul (_aa, _dd);
        _mulbc.from_mul (_bb, _cc);

        expansion_sub(_mulad, _mulbc, _final);
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact 3 x 3 determinant.
     *
     *   | a1  a2  v1 |
     *   | b1  b2  v2 |
     *   | c1  c2  v3 |
     *
     * as the product of 2 x 2 minors about a pivot column
     * P, shown here for P = 3. The entry V1 is associated
     * with the minor
     *
     *   | b1  b2 | = D1
     *   | c1  c2 |
     *
     * and so on for (V2,D2), (V3,D3) etc.
     *
    --------------------------------------------------------
     */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE, size_t NF,
        size_t NG
             >
    __inline_call void compute_det_3x3 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _val1p ,
        expansion <NC> const& _det2p ,
        expansion <ND> const& _val2p ,
        expansion <NE> const& _det3p ,
        expansion <NF> const& _val3p ,
        expansion <NG> & _final ,
        INDX_TYPE        _pivot
        )
    {
    /*---------------------------------- products Vi * Di */
        INDX_TYPE
        constexpr N1 = mul_alloc (NA, NB) ;
        expansion<N1> _mul1p;
        expansion_mul(_det1p, _val1p, _mul1p);

        INDX_TYPE
        constexpr N2 = mul_alloc (NC, ND) ;
        expansion<N2> _mul2p;
        expansion_mul(_det2p, _val2p, _mul2p);

        INDX_TYPE
        constexpr N3 = mul_alloc (NE, NF) ;
        expansion<N3> _mul3p;
        expansion_mul(_det3p, _val3p, _mul3p);

    /*---------------------------------- sum (-1)^P * VDi */
        INDX_TYPE
        constexpr MM = sub_alloc (N1, N2) ;
        expansion<MM> _sum_1;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_mul2p, _mul1p, _sum_1);
        expansion_sub(_sum_1, _mul3p, _final);
        }
        else
        {
        expansion_sub(_mul1p, _mul2p, _sum_1);
        expansion_add(_sum_1, _mul3p, _final);
        }
    }

    /*--------------------- "unitary" case, with Vi = +1. */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND
             >
    __inline_call void unitary_det_3x3 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _det2p ,
        expansion <NC> const& _det3p ,
        expansion <ND> & _final ,
        INDX_TYPE        _pivot
        )
    {
        INDX_TYPE
        constexpr MM = sub_alloc (NA, NB) ;
        expansion<MM> _sum_1;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_det2p, _det1p, _sum_1);
        expansion_sub(_sum_1, _det3p, _final);
        }
        else
        {
        expansion_sub(_det1p, _det2p, _sum_1);
        expansion_add(_sum_1, _det3p, _final);
        }
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact 4 x 4 determinant.
     *
     *   | a1  a2  a3  v1 |
     *   | b1  b2  b2  v2 |
     *   | c1  c2  c3  v3 |
     *   | d1  d2  d3  v4 |
     *
     * as the product of 3 x 3 minors about a pivot column
     * P, shown here for P = 4. The entry V1 is associated
     * with the minor
     *
     *   | b1  b2  b3 |
     *   | c1  c2  c3 | = D1
     *   | d1  d2  d3 |
     *
     * and so on for (V2,D2), (V3,D3) etc.
     *
    --------------------------------------------------------
     */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE, size_t NF,
        size_t NG, size_t NH, size_t NI
             >
    __inline_call void compute_det_4x4 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _val1p ,
        expansion <NC> const& _det2p ,
        expansion <ND> const& _val2p ,
        expansion <NE> const& _det3p ,
        expansion <NF> const& _val3p ,
        expansion <NG> const& _det4p ,
        expansion <NH> const& _val4p ,
        expansion <NI> & _final ,
        INDX_TYPE        _pivot
        )
    {
    /*---------------------------------- products Vi * Di */
        INDX_TYPE
        constexpr N1 = mul_alloc (NA, NB) ;
        expansion<N1> _mul1p;
        expansion_mul(_det1p, _val1p, _mul1p);

        INDX_TYPE
        constexpr N2 = mul_alloc (NC, ND) ;
        expansion<N2> _mul2p;
        expansion_mul(_det2p, _val2p, _mul2p);

        INDX_TYPE
        constexpr N3 = mul_alloc (NE, NF) ;
        expansion<N3> _mul3p;
        expansion_mul(_det3p, _val3p, _mul3p);

        INDX_TYPE
        constexpr N4 = mul_alloc (NG, NH) ;
        expansion<N4> _mul4p;
        expansion_mul(_det4p, _val4p, _mul4p);

    /*---------------------------------- sum (-1)^P * VDi */
        INDX_TYPE
        constexpr M1 = sub_alloc (N1, N2) ;
        expansion<M1> _sum_1;

        INDX_TYPE
        constexpr M2 = sub_alloc (N3, N4) ;
        expansion<M2> _sum_2;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_mul2p, _mul1p, _sum_1);
        expansion_sub(_mul4p, _mul3p, _sum_2);
        }
        else
        {
        expansion_sub(_mul1p, _mul2p, _sum_1);
        expansion_sub(_mul3p, _mul4p, _sum_2);
        }

        expansion_add(_sum_1, _sum_2, _final);
    }

    /*--------------------- "unitary" case, with Vi = +1. */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE
             >
    __inline_call void unitary_det_4x4 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _det2p ,
        expansion <NC> const& _det3p ,
        expansion <ND> const& _det4p ,
        expansion <NE> & _final ,
        INDX_TYPE        _pivot
        )
    {
        INDX_TYPE
        constexpr M1 = sub_alloc (NA, NB) ;
        expansion<M1> _sum_1;

        INDX_TYPE
        constexpr M2 = sub_alloc (NC, ND) ;
        expansion<M2> _sum_2;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_det2p, _det1p, _sum_1);
        expansion_sub(_det4p, _det3p, _sum_2);
        }
        else
        {
        expansion_sub(_det2p, _det1p, _sum_1);
        expansion_sub(_det4p, _det3p, _sum_2);
        }

        expansion_add(_sum_1, _sum_2, _final);
    }

    /*
    --------------------------------------------------------
     *
     * Compute an exact 5 x 5 determinant.
     *
     *   | a1  a2  a3  a4  v1 |
     *   | b1  b2  b3  b4  v2 |
     *   | c1  c2  c3  c4  v3 |
     *   | d1  d2  d3  d4  v4 |
     *   | e1  e2  e3  e4  v5 |
     *
     * as the product of 4 x 4 minors about a pivot column
     * P, shown here for P = 5. The entry V1 is associated
     * with the minor
     *
     *   | b1  b2  b3  b4 |
     *   | c1  c2  c3  c4 | = D1
     *   | d1  d2  d3  d4 |
     *   | e1  e2  e3  e4 |
     *
     * and so on for (V2,D2), (V3,D3) etc.
     *
    --------------------------------------------------------
     */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE, size_t NF,
        size_t NG, size_t NH, size_t NI,
        size_t NJ, size_t NK
             >
    __inline_call void compute_det_5x5 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _val1p ,
        expansion <NC> const& _det2p ,
        expansion <ND> const& _val2p ,
        expansion <NE> const& _det3p ,
        expansion <NF> const& _val3p ,
        expansion <NG> const& _det4p ,
        expansion <NH> const& _val4p ,
        expansion <NI> const& _det5p ,
        expansion <NJ> const& _val5p ,
        expansion <NK> & _final ,
        INDX_TYPE        _pivot
        )
    {
    /*---------------------------------- products Vi * Di */
        INDX_TYPE
        constexpr N1 = mul_alloc (NA, NB) ;
        expansion<N1> _mul1p;
        expansion_mul(_det1p, _val1p, _mul1p);

        INDX_TYPE
        constexpr N2 = mul_alloc (NC, ND) ;
        expansion<N2> _mul2p;
        expansion_mul(_det2p, _val2p, _mul2p);

        INDX_TYPE
        constexpr N3 = mul_alloc (NE, NF) ;
        expansion<N3> _mul3p;
        expansion_mul(_det3p, _val3p, _mul3p);

        INDX_TYPE
        constexpr N4 = mul_alloc (NG, NH) ;
        expansion<N4> _mul4p;
        expansion_mul(_det4p, _val4p, _mul4p);

        INDX_TYPE
        constexpr N5 = mul_alloc (NI, NJ) ;
        expansion<N5> _mul5p;
        expansion_mul(_det5p, _val5p, _mul5p);

    /*---------------------------------- sum (-1)^P * VDi */
        INDX_TYPE
        constexpr M1 = sub_alloc (N1, N2) ;
        expansion<M1> _sum_1;

        INDX_TYPE
        constexpr M2 = sub_alloc (N3, N4) ;
        expansion<M2> _sum_2;

        INDX_TYPE
        constexpr M3 = sub_alloc (M1, N5) ;
        expansion<M3> _sum_3;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_mul2p, _mul1p, _sum_1);
        expansion_sub(_mul4p, _mul3p, _sum_2);
        expansion_sub(_sum_1, _mul5p, _sum_3);
        }
        else
        {
        expansion_sub(_mul1p, _mul2p, _sum_1);
        expansion_sub(_mul3p, _mul4p, _sum_2);
        expansion_add(_sum_1, _mul5p, _sum_3);
        }

        expansion_add(_sum_3, _sum_2, _final);
    }

    /*--------------------- "unitary" case, with Vi = +1. */

    template <
        size_t NA, size_t NB, size_t NC,
        size_t ND, size_t NE, size_t NF
             >
    __inline_call void unitary_det_5x5 (
        expansion <NA> const& _det1p ,
        expansion <NB> const& _det2p ,
        expansion <NC> const& _det3p ,
        expansion <ND> const& _det4p ,
        expansion <NE> const& _det5p ,
        expansion <NF> & _final ,
        INDX_TYPE        _pivot
        )
    {
        INDX_TYPE
        constexpr N1 = sub_alloc (NA, NB) ;
        expansion<N1> _sum_1;

        INDX_TYPE
        constexpr N2 = sub_alloc (NC, ND) ;
        expansion<N2> _sum_2;

        INDX_TYPE
        constexpr N3 = sub_alloc (N1, NE) ;
        expansion<N3> _sum_3;

        if (_pivot % 2 == +0)
        {
        expansion_sub(_det2p, _det1p, _sum_1);
        expansion_sub(_det4p, _det3p, _sum_2);
        expansion_sub(_sum_1, _det5p, _sum_3);
        }
        else
        {
        expansion_sub(_det1p, _det2p, _sum_1);
        expansion_sub(_det3p, _det4p, _sum_2);
        expansion_add(_sum_1, _det5p, _sum_3);
        }

        expansion_add(_sum_3, _sum_2, _final);
    }

#   undef REAL_TYPE
#   undef INDX_TYPE


    }

#   endif//__MP_UTILS__



