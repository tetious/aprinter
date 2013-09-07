/*
 * Copyright (c) 2013 Ambroz Bizjak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef AMBROLIB_LINEAR_PLANNER_H
#define AMBROLIB_LINEAR_PLANNER_H

#include <math.h>

#include <aprinter/base/Assert.h>
#include <aprinter/math/FloatTools.h>

#include <aprinter/BeginNamespace.h>

struct LinearPlannerSegmentData {
    double a_x;
    double max_v;
    double max_start_v;
    double a_x_rec;
    double two_max_v_minus_a_x;
};

struct LinearPlannerSegmentState {
    double end_v;
};

struct LinearPlannerSegmentResult {
    double const_start;
    double const_end;
    double const_v;
};

static double LinearPlannerPush (LinearPlannerSegmentData *segment, LinearPlannerSegmentState *s, double end_v)
{
    AMBRO_ASSERT(segment)
    AMBRO_ASSERT(FloatIsPosOrPosZero(segment->a_x))
    AMBRO_ASSERT(FloatIsPosOrPosZero(segment->max_v))
    AMBRO_ASSERT(FloatIsPosOrPosZero(segment->max_start_v))
    AMBRO_ASSERT(segment->max_start_v <= segment->max_v)
    AMBRO_ASSERT(FloatIsPosOrPosZero(end_v))
    
    s->end_v = fmin(segment->max_v, end_v);
    return fmin(segment->max_start_v, s->end_v + segment->a_x);
}

static double LinearPlannerPull (LinearPlannerSegmentData *segment, LinearPlannerSegmentState *s, double start_v, LinearPlannerSegmentResult *result)
{
    double end_v = s->end_v;
    
    if (end_v > start_v + segment->a_x) {
        end_v = start_v + segment->a_x;
        result->const_start = 1.0;
        result->const_end = 0.0;
        result->const_v = end_v;
    } else {
        if (start_v + end_v > segment->two_max_v_minus_a_x) {
            result->const_start = (segment->max_v - start_v) * segment->a_x_rec;
            result->const_end = (segment->max_v - end_v) * segment->a_x_rec;
            result->const_v = segment->max_v;
        } else {
            result->const_start = ((end_v + segment->a_x) - start_v) * segment->a_x_rec / 2;
            result->const_end = 1.0 - result->const_start;
            result->const_v = (start_v + end_v + segment->a_x) / 2;
        }
    }
    
    AMBRO_ASSERT(FloatIsPosOrPosZero(start_v))
    AMBRO_ASSERT(FloatIsPosOrPosZero(end_v))
    AMBRO_ASSERT(FloatIsPosOrPosZero(result->const_v))
    
    return end_v;
}

#include <aprinter/EndNamespace.h>

#endif