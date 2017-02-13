/*
 * Copyright (c) 2016 Ambroz Bizjak
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

#ifndef APRINTER_IPSTACK_IP_TCP_PROTO_OUTPUT_H
#define APRINTER_IPSTACK_IP_TCP_PROTO_OUTPUT_H

#include <stdint.h>
#include <stddef.h>

#include <aprinter/meta/MinMax.h>
#include <aprinter/base/Preprocessor.h>
#include <aprinter/base/Assert.h>
#include <aprinter/base/Hints.h>
#include <aprinter/base/OneOf.h>
#include <aipstack/misc/Buf.h>
#include <aipstack/misc/Chksum.h>
#include <aipstack/misc/Allocator.h>
#include <aipstack/proto/Tcp4Proto.h>
#include <aipstack/proto/TcpUtils.h>
#include <aipstack/ip/IpStack.h>

#include <aipstack/BeginNamespace.h>

template <typename TcpProto>
class IpTcpProto_output
{
    APRINTER_USE_TYPES1(TcpUtils, (FlagsType, SeqType, PortType, TcpState, TcpSegMeta,
                                   OptionFlags, TcpOptions))
    APRINTER_USE_VALS(TcpUtils, (seq_add, seq_diff, seq_lte, seq_lt, tcplen,
                                 can_output_in_state, accepting_data_in_state,
                                 snd_open_in_state))
    APRINTER_USE_TYPES1(TcpProto, (Context, Ip4DgramMeta, TcpPcb, PcbFlags, BufAllocator,
                                   Input, Clock, TimeType, RttType, RttNextType, Constants,
                                   OutputTimer, RtxTimer))
    APRINTER_USE_VALS(TcpProto, (RttTypeMax))
    APRINTER_USE_VALS(TcpProto::TheIpStack, (HeaderBeforeIp4Dgram))
    APRINTER_USE_ONEOF
    
public:
    // Check if our FIN has been ACKed.
    static bool pcb_fin_acked (TcpPcb *pcb)
    {
        return pcb->hasFlag(PcbFlags::FIN_SENT) && pcb->snd_una == pcb->snd_nxt;
    }
    
    // Calculate the offset of the current send buffer position.
    static size_t pcb_snd_offset (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->snd_buf_cur.tot_len <= pcb->sndBufLen())
        
        return pcb->sndBufLen() - pcb->snd_buf_cur.tot_len;
    }
    
    // Send SYN or SYN-ACK packet (in the SYN_SENT or SYN_RCVD states respectively).
    static void pcb_send_syn (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->state == OneOf(TcpState::SYN_SENT, TcpState::SYN_RCVD))
        
        // Include the MSS option.
        TcpOptions tcp_opts;
        tcp_opts.options = OptionFlags::MSS;
        tcp_opts.mss = pcb->rcv_mss;
        
        // Send the window scale option if needed.
        if (pcb->hasFlag(PcbFlags::WND_SCALE)) {
            tcp_opts.options |= OptionFlags::WND_SCALE;
            tcp_opts.wnd_scale = pcb->rcv_wnd_shift;
        }
        
        // The SYN and SYN-ACK must always have non-scaled window size.
        auto saved_rcv_wnd_shift = pcb->rcv_wnd_shift;
        pcb->rcv_wnd_shift = 0;
        uint16_t window_size = Input::pcb_ann_wnd(pcb);
        pcb->rcv_wnd_shift = saved_rcv_wnd_shift;
        
        // Send SYN or SYN-ACK flags depending on the state.
        FlagsType flags = Tcp4FlagSyn |
            ((pcb->state == TcpState::SYN_RCVD) ? Tcp4FlagAck : 0);
        
        // Send the segment.
        TcpSegMeta tcp_meta = {pcb->local_port, pcb->remote_port, pcb->snd_una, pcb->rcv_nxt,
                               window_size, flags, &tcp_opts};
        IpErr err = send_tcp(pcb->tcp, pcb->local_addr, pcb->remote_addr,
                             pcb->ip_send_flags, tcp_meta, IpBufRef{}, pcb);
        
        if (err == IpErr::SUCCESS) {
            // Have we sent the SYN for the first time?
            if (pcb->snd_nxt == pcb->snd_una) {
                // Start a round-trip-time measurement.
                pcb_start_rtt_measurement(pcb);
                
                // Bump snd_nxt.
                pcb->snd_nxt = seq_add(pcb->snd_nxt, 1);
            } else {
                // Retransmission, stop any round-trip-time measurement.
                pcb->clearFlag(PcbFlags::RTT_PENDING);
            }
        }
    }
    
    // Send an empty ACK (which may be a window update).
    static void pcb_send_empty_ack (TcpPcb *pcb)
    {
        TcpSegMeta tcp_meta = {pcb->local_port, pcb->remote_port, pcb->snd_nxt, pcb->rcv_nxt,
                               Input::pcb_ann_wnd(pcb), Tcp4FlagAck};
        send_tcp(pcb->tcp, pcb->local_addr, pcb->remote_addr,
                 pcb->ip_send_flags, tcp_meta, IpBufRef{}, pcb);
    }
    
    // Send an RST for this PCB.
    static void pcb_send_rst (TcpPcb *pcb)
    {
        bool ack = pcb->state != TcpState::SYN_SENT;
        
        send_rst(pcb->tcp, pcb->local_addr, pcb->remote_addr,
                 pcb->local_port, pcb->remote_port,
                 pcb->snd_nxt, ack, pcb->rcv_nxt);
    }
    
    static void pcb_need_ack (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->state != TcpState::CLOSED)
        
        // If we're in input processing just set a flag that ACK is
        // needed which will be picked up at the end, otherwise send
        // an ACK ourselves.
        if (pcb->tcp->m_current_pcb == pcb) {
            pcb->setFlag(PcbFlags::ACK_PENDING);
        } else {
            pcb_send_empty_ack(pcb);
        }
    }
    
    static void pcb_snd_buf_extended (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->state == TcpState::SYN_SENT || snd_open_in_state(pcb->state))
        
        if (pcb->state != TcpState::SYN_SENT) {
            // Start the output timer if not running.
            if (!pcb->tim(OutputTimer()).isSet(Context())) {
                pcb->tim(OutputTimer()).appendAfter(Context(), Constants::OutputTimerTicks);
            }
        }
    }
    
    static void pcb_end_sending (TcpPcb *pcb)
    {
        AMBRO_ASSERT(snd_open_in_state(pcb->state))
        
        // Make the appropriate state transition, effectively
        // queuing a FIN for sending.
        if (pcb->state == TcpState::ESTABLISHED) {
            pcb->state = TcpState::FIN_WAIT_1;
        } else {
            AMBRO_ASSERT(pcb->state == TcpState::CLOSE_WAIT)
            pcb->state = TcpState::LAST_ACK;
        }
        
        // Queue a FIN for sending.
        pcb->setFlag(PcbFlags::FIN_PENDING);
        
        // Push output.
        pcb_push_output(pcb);
    }
    
    static void pcb_push_output (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->state == TcpState::SYN_SENT || can_output_in_state(pcb->state))
        
        // Set the push index to the end of the send buffer.
        pcb->snd_psh_index = pcb->sndBufLen();
        
        if (pcb->state != TcpState::SYN_SENT) {
            // Schedule a call to pcb_output soon.
            if (pcb == pcb->tcp->m_current_pcb) {
                pcb->setFlag(PcbFlags::OUT_PENDING);
            } else {
                if (!pcb->tim(OutputTimer()).isSet(Context())) {
                    pcb->tim(OutputTimer()).appendAfter(Context(), Constants::OutputTimerTicks);
                }
            }
        }
    }
    
    // Check if there is any unacknowledged or unsent data or FIN.
    static bool pcb_has_snd_outstanding (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        return pcb->sndBufLen() > 0 || !snd_open_in_state(pcb->state);
    }
    
    // Determine if the rtx_timer needs to be running for retransmission
    // or window probe.
    static bool pcb_need_rtx_timer (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_outstanding(pcb))
        
        return (pcb->snd_wnd == 0) ? !pcb_may_delay_snd(pcb) : pcb_has_snd_unacked(pcb);
    }
    
    // Determine if there is any data or FIN which is no longer queued for
    // sending but has not been ACKed. This is NOT necessarily the same as
    // snd_una!=snd_nxt due to requeuing in pcb_rtx_timer_handler.
    static bool pcb_has_snd_unacked (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        return pcb->snd_buf_cur.tot_len < pcb->sndBufLen() ||
               (!snd_open_in_state(pcb->state) && !pcb->hasFlag(PcbFlags::FIN_PENDING));
    }
    
    /**
     * Transmits previously unsent data as permissible and controls the
     * rtx_timer. Returns whether something has been sent.
     */
    static bool pcb_output_queued (TcpPcb *pcb, bool no_delay = false)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        // Is there nothing outstanding to be sent or ACKed?
        if (!pcb_has_snd_outstanding(pcb)) {
            // Start timer for idle timeout unless already running for idle timeout.
            // NOTE: We might set the idle timer even if it has already expired and
            // nothing has been sent since, but this is not really a problem.
            if (!pcb->tim(RtxTimer()).isSet(Context()) || !pcb->hasFlag(PcbFlags::IDLE_TIMER)) {
                pcb->tim(RtxTimer()).appendAfter(Context(), pcb_rto_time(pcb));
                pcb->setFlag(PcbFlags::IDLE_TIMER);
            }
            return false;
        }
        
        // Get the cwnd.
        SeqType cwnd = pcb->cwnd;
        AMBRO_ASSERT(cwnd >= pcb->snd_mss)
        
        // Calculate how much more we are allowed to send, according
        // to the receiver window and the congestion window.
        SeqType full_wnd = APrinter::MinValue(pcb->snd_wnd, cwnd);
        SeqType rem_wnd = full_wnd - APrinter::MinValueU(full_wnd, pcb_snd_offset(pcb));
        
        // Will need to know if we sent anything.
        bool sent = false;
        
        // While we have something to send and some window is available...
        while ((pcb->snd_buf_cur.tot_len > 0 || pcb->hasFlag(PcbFlags::FIN_PENDING)) && rem_wnd > 0)
        {
            // If we have less than MSS of data left to send which is
            // not being pushed (due to sendPush or close), delay sending.
            // But force sending if called with no_delay==true.
            if (!no_delay && pcb_may_delay_snd(pcb)) {
                break;
            }
            
            // Send a segment.
            bool fin = pcb->hasFlag(PcbFlags::FIN_PENDING);
            SeqType seg_seqlen = pcb_output_segment(pcb, pcb->snd_buf_cur, fin, rem_wnd);
            AMBRO_ASSERT(seg_seqlen > 0 && seg_seqlen <= rem_wnd)
            
            // Advance snd_buf_cur over any data just sent.
            size_t data_sent = APrinter::MinValueU(seg_seqlen, pcb->snd_buf_cur.tot_len);
            pcb->snd_buf_cur.skipBytes(data_sent);
            
            // If we sent a FIN, clear the FIN_PENDING flag.
            if (seg_seqlen > data_sent) {
                AMBRO_ASSERT(pcb->hasFlag(PcbFlags::FIN_PENDING))
                AMBRO_ASSERT(seg_seqlen - 1 == data_sent)
                pcb->clearFlag(PcbFlags::FIN_PENDING);
            }
            
            // Update local state.
            rem_wnd -= seg_seqlen;
            sent = true;
        }
        
        // Start or stop the rtx_timer as needed.
        pcb_update_rtx_timer(pcb);
        
        return sent;
    }
    
    // Start or stop the rtx_timer for retransmission or window probe as needed.
    static void pcb_update_rtx_timer (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_outstanding(pcb))
        
        if (pcb_need_rtx_timer(pcb)) {
            // Start timer for retransmission or window probe, if not already
            // or if it was running for idle timeout.
            if (!pcb->tim(RtxTimer()).isSet(Context()) || pcb->hasFlag(PcbFlags::IDLE_TIMER)) {
                pcb->tim(RtxTimer()).appendAfter(Context(), pcb_rto_time(pcb));
                pcb->clearFlag(PcbFlags::IDLE_TIMER);
            }
        } else {
            // Stop the timer.
            pcb->tim(RtxTimer()).unset(Context());
        }
    }
    
    static void pcb_output_timer_handler (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        // Send any unsent data as permissible.
        pcb_output_queued(pcb);
    }
    
    static void pcb_rtx_timer_handler (TcpPcb *pcb)
    {
        // For any state change that invalidates can_output_in_state the timer is
        // also stopped (pcb_abort, pcb_go_to_time_wait).
        AMBRO_ASSERT(pcb->state == OneOf(TcpState::SYN_SENT, TcpState::SYN_RCVD) ||
                     can_output_in_state(pcb->state))
        
        // Is this an idle timeout?
        if (pcb->hasFlag(PcbFlags::IDLE_TIMER)) {
            // If the timer was set for idle timeout, the precondition !pcb_has_snd_unacked
            // could only be invalidated by sending some new data:
            // 1) pcb_output_queued will in any case set/unset the timer how it needs to be.
            // 2) pcb_rtx_timer_handler can obviously not send anything before this check.
            // 3) fast-recovery related sending (pcb_fast_rtx_dup_acks_received,
            //    pcb_output_handle_acked) can only happen when pcb_has_snd_unacked.
            AMBRO_ASSERT(pcb->state != OneOf(TcpState::SYN_SENT, TcpState::SYN_RCVD))
            AMBRO_ASSERT(!pcb_has_snd_unacked(pcb))
            
            // Reduce the CWND (RFC 5681 section 4.1).
            // Also reset cwnd_acked to avoid old accumulated value
            // from causing an undesired cwnd increase later.
            SeqType initial_cwnd = calc_initial_cwnd(pcb->snd_mss);
            if (pcb->cwnd >= initial_cwnd) {
                pcb->cwnd = initial_cwnd;
                pcb->setFlag(PcbFlags::CWND_INIT);
            }
            pcb->cwnd_acked = 0;
            
            // This is all, the remainder of this function is for retransmission.
            return;
        }
        
        // Double the retransmission timeout and restart the timer.
        RttType doubled_rto = (pcb->rto > RttTypeMax / 2) ? RttTypeMax : (2 * pcb->rto);
        pcb->rto = APrinter::MinValue(Constants::MaxRtxTime, doubled_rto);
        pcb->tim(RtxTimer()).appendAfter(Context(), pcb_rto_time(pcb));
        
        // If this for a SYN or SYN-ACK retransmission, retransmit and return.
        if (pcb->state == OneOf(TcpState::SYN_SENT, TcpState::SYN_RCVD)) {
            pcb_send_syn(pcb);
            return;
        }
        
        // If the timer was set for retransmission or window probe, the precondition
        // pcb_need_rtx_timer must still hold. Anything that would have invalidated
        // that would have stopped the timer.
        AMBRO_ASSERT(pcb_has_snd_outstanding(pcb))
        AMBRO_ASSERT(pcb_need_rtx_timer(pcb))
        
        if (pcb->snd_wnd == 0) {
            // Send a window probe.
            pcb_output_front(pcb);
        } else {
            // Check for first retransmission.
            if (!pcb->hasFlag(PcbFlags::RTX_ACTIVE)) {
                // Set flag to indicate there has been a retransmission.
                // This will be cleared upon new ACK.
                pcb->setFlag(PcbFlags::RTX_ACTIVE);
                
                // Update ssthresh (RFC 5681).
                pcb_update_ssthresh_for_rtx(pcb);
            }
            
            // Set cwnd to one segment (RFC 5681).
            // Also reset cwnd_acked to avoid old accumulated value
            // from causing an undesired cwnd increase later.
            pcb->cwnd = pcb->snd_mss;
            pcb->clearFlag(PcbFlags::CWND_INIT);
            pcb->cwnd_acked = 0;
            
            // Set recover.
            pcb->setFlag(PcbFlags::RECOVER);
            pcb->recover = pcb->snd_nxt;
            
            // Exit any fast recovery.
            pcb->num_dupack = 0;
            
            // Requeue all data and FIN.
            pcb->snd_buf_cur = (pcb->con != nullptr) ? pcb->con->m_snd_buf : IpBufRef{};
            if (!snd_open_in_state(pcb->state)) {
                pcb->setFlag(PcbFlags::FIN_PENDING);
            }
            
            // Retransmit using pcb_output_queued.
            // This will necessarily send something because there is data
            // outstanding and all qeued and we call with no_delay==true.
            bool sent = pcb_output_queued(pcb, true);
            AMBRO_ASSERT(sent)
        }
    }
    
    // This is called from Input when something new is acked, before the
    // related state changes are made (snd_una, snd_wnd, snd_buf*, state
    // transition due to FIN acked).
    static void pcb_output_handle_acked (TcpPcb *pcb, SeqType ack_num, SeqType acked)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_outstanding(pcb))
        
        // Stop the rtx_timer. Consider that the state changes done by Input
        // just after this might invalidate the asserts in pcb_rtx_timer_handler.
        pcb->tim(RtxTimer()).unset(Context());
        
        // Clear the RTX_ACTIVE flag since any retransmission has now been acked.
        pcb->clearFlag(PcbFlags::RTX_ACTIVE);
        
        // Handle end of round-trip-time measurement.
        if (pcb->hasFlag(PcbFlags::RTT_PENDING) && seq_lt(pcb->rtt_test_seq, ack_num, pcb->snd_una)) {
            // Update the RTT variables and RTO.
            pcb_end_rtt_measurement(pcb);
            
            // Allow more CWND increase in congestion avoidance.
            pcb->clearFlag(PcbFlags::CWND_INCRD);
        }
        
        // Not in fast recovery?
        if (AMBRO_LIKELY(pcb->num_dupack < Constants::FastRtxDupAcks)) {
            // Reset the duplicate ACK counter.
            pcb->num_dupack = 0;
            
            // Perform congestion-control processing.
            if (pcb->cwnd <= pcb->ssthresh) {
                // Slow start.
                pcb_increase_cwnd_acked(pcb, acked);
            } else {
                // Congestion avoidance.
                if (!pcb->hasFlag(PcbFlags::CWND_INCRD)) {
                    // Increment cwnd_acked.
                    pcb->cwnd_acked = (acked > UINT32_MAX - pcb->cwnd_acked) ? UINT32_MAX : (pcb->cwnd_acked + acked);
                    
                    // If cwnd data has now been acked, increment cwnd and reset cwnd_acked,
                    // and inhibit such increments until the next RTT measurement completes.
                    if (pcb->cwnd_acked >= pcb->cwnd) {
                        pcb_increase_cwnd_acked(pcb, pcb->cwnd_acked);
                        pcb->cwnd_acked = 0;
                        pcb->setFlag(PcbFlags::CWND_INCRD);
                    }
                }
            }
        }
        // In fast recovery
        else {
            // We had sent but unkacked data when fast recovery was started
            // and this must still be true. Because when all unkacked data is
            // ACKed we would exit fast recovery, just below (the condition
            // below is implied then because recover<=snd_nxt).
            AMBRO_ASSERT(pcb_has_snd_unacked(pcb))
            
            // If all data up to recover is being ACKed, exit fast recovery.
            if (!pcb->hasFlag(PcbFlags::RECOVER) || !seq_lt(ack_num, pcb->recover, pcb->snd_una)) {
                // Deflate the CWND.
                // Note, cwnd>=snd_mss is respected because ssthresh>=snd_mss.
                SeqType flight_size = seq_diff(pcb->snd_nxt, ack_num);
                AMBRO_ASSERT(pcb->ssthresh >= pcb->snd_mss)
                pcb->cwnd = APrinter::MinValue(pcb->ssthresh,
                    seq_add(APrinter::MaxValue(flight_size, (SeqType)pcb->snd_mss), pcb->snd_mss));
                
                // Reset num_dupack to indicate end of fast recovery.
                pcb->num_dupack = 0;
            } else {
                // Retransmit the first unacknowledged segment.
                pcb_output_front(pcb);
                
                // Deflate CWND by the amount of data ACKed.
                // Be careful to not bring CWND below snd_mss.
                AMBRO_ASSERT(pcb->cwnd >= pcb->snd_mss)
                pcb->cwnd -= APrinter::MinValue(seq_diff(pcb->cwnd, pcb->snd_mss), acked);
                
                // If this ACK acknowledges at least snd_mss of data,
                // add back snd_mss bytes to CWND.
                if (acked >= pcb->snd_mss) {
                    pcb_increase_cwnd(pcb, pcb->snd_mss);
                }
            }
        }
        
        // If the snd_una increment that will be done for this ACK will
        // leave recover behind snd_una, clear the recover flag to indicate
        // that recover is no longer valid and assumed <snd_una.
        if (AMBRO_UNLIKELY(pcb->hasFlag(PcbFlags::RECOVER)) &&
            !seq_lte(ack_num, pcb->recover, pcb->snd_una))
        {
            pcb->clearFlag(PcbFlags::RECOVER);
        }
    }
    
    // Called from Input when the number of duplicate ACKs has
    // reached FastRtxDupAcks, the fast recovery threshold.
    static void pcb_fast_rtx_dup_acks_received (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_unacked(pcb))
        AMBRO_ASSERT(pcb->num_dupack == Constants::FastRtxDupAcks)
        
        // If we have recover (>=snd_nxt), we must not enter fast recovery.
        // In that case we must decrement num_dupack by one, to indicate that
        // we are not in fast recovery and the next duplicate ACK is still
        // a candidate.
        if (pcb->hasFlag(PcbFlags::RECOVER)) {
            pcb->num_dupack--;
            return;
        }
        
        // Do the retransmission.
        pcb_output_front(pcb);
        
        // Set recover.
        pcb->setFlag(PcbFlags::RECOVER);
        pcb->recover = pcb->snd_nxt;
        
        // Update ssthresh.
        pcb_update_ssthresh_for_rtx(pcb);
        
        // Update cwnd.
        SeqType three_mss = 3 * (SeqType)pcb->snd_mss;
        pcb->cwnd = (three_mss > UINT32_MAX - pcb->ssthresh) ? UINT32_MAX : (pcb->ssthresh + three_mss);
        pcb->clearFlag(PcbFlags::CWND_INIT);
        
        // Schedule output due to possible CWND increase.
        pcb->setFlag(PcbFlags::OUT_PENDING);
    }
    
    // Called from Input when an additional duplicate ACK has been
    // received while already in fast recovery.
    static void pcb_extra_dup_ack_received (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_unacked(pcb))
        AMBRO_ASSERT(pcb->num_dupack > Constants::FastRtxDupAcks)
        
        // Increment CWND by snd_mss.
        pcb_increase_cwnd(pcb, pcb->snd_mss);
        
        // Schedule output due to possible CWND increase.
        pcb->setFlag(PcbFlags::OUT_PENDING);
    }
    
    // Update the RTO from RTTVAR and SRTT, or to InitialRtxTime if no RTT_VALID.
    static void pcb_update_rto (TcpPcb *pcb)
    {
        if (AMBRO_LIKELY(pcb->hasFlag(PcbFlags::RTT_VALID))) {
            int const k = 4;
            RttType k_rttvar = (pcb->rttvar > RttTypeMax / k) ? RttTypeMax : (k * pcb->rttvar);
            RttType var_part = APrinter::MaxValue((RttType)1, k_rttvar);
            RttType base_rto = (var_part > RttTypeMax - pcb->srtt) ? RttTypeMax : (pcb->srtt + var_part);
            pcb->rto = APrinter::MaxValue(Constants::MinRtxTime, APrinter::MinValue(Constants::MaxRtxTime, base_rto));
        } else {
            pcb->rto = Constants::InitialRtxTime;
        }
    }
    
    static TimeType pcb_rto_time (TcpPcb *pcb)
    {
        return (TimeType)pcb->rto << TcpProto::RttShift;
    }
    
    // Calculate the initial cwnd based on snd_mss.
    static SeqType calc_initial_cwnd (uint16_t snd_mss)
    {
        if (snd_mss > 2190) {
            return (snd_mss > UINT32_MAX / 2) ? UINT32_MAX : (2 * snd_mss);
        }
        else if (snd_mss > 1095) {
            return 3 * snd_mss;
        }
        else {
            return 4 * snd_mss;
        }
    }
    
    static void pcb_end_rtt_measurement (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->hasFlag(PcbFlags::RTT_PENDING))
        
        // Clear the flag to indicate end of RTT measurement.
        pcb->clearFlag(PcbFlags::RTT_PENDING);
        
        // Calculate how much time has passed, also in RTT units.
        TimeType time_diff = Clock::getTime(Context()) - pcb->rtt_test_time;
        RttType this_rtt = APrinter::MinValueU(RttTypeMax, time_diff >> TcpProto::RttShift);
        
        // Update RTTVAR and SRTT.
        if (!pcb->hasFlag(PcbFlags::RTT_VALID)) {
            pcb->setFlag(PcbFlags::RTT_VALID);
            pcb->rttvar = this_rtt/2;
            pcb->srtt = this_rtt;
        } else {
            RttType rtt_diff = APrinter::AbsoluteDiff(pcb->srtt, this_rtt);
            pcb->rttvar = ((RttNextType)3 * pcb->rttvar + rtt_diff) / 4;
            pcb->srtt = ((RttNextType)7 * pcb->srtt + this_rtt) / 8;
        }
        
        // Update RTO.
        pcb_update_rto(pcb);
    }
    
    // This is called from the lower layers when sending failed but
    // is now expected to succeed. Currently the mechanism is used to
    // retry after ARP resolution completes.
    static void pcb_send_retry (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->state != TcpState::CLOSED)
        
        if (pcb->state == OneOf(TcpState::SYN_SENT, TcpState::SYN_RCVD)) {
            pcb_send_syn(pcb);
        }
        else if (can_output_in_state(pcb->state)) {
            pcb_output_queued(pcb);
        }
    }
    
    // Update snd_mss and IpSendFlags::DontFragmentFlag based on the current
    // mtu_ref information. If check_change is true, it checks if the current
    // values in the PCB match the computed ones before updating and if so an
    // update is inhibited. Returns whether an update was done.
    static bool pcb_update_pmtu_base (TcpPcb *pcb, bool check_change)
    {
        AMBRO_ASSERT(pcb->mtu_ref.isSetup())
        
        // Get the PMTU and DF flag from the mtu_ref.
        uint16_t mtu;
        bool dont_fragment;
        pcb->mtu_ref.getMtuAndDf(pcb->tcp->m_stack, &mtu, &dont_fragment);
        AMBRO_ASSERT(mtu >= Ip4MinMtu)
        
        // Calculate the snd_mss from the MTU, bound to no more than base_snd_mss.
        uint16_t dgram_mtu = mtu - Ip4Header::Size;
        uint16_t snd_mss = APrinter::MinValue(pcb->base_snd_mss, TcpUtils::calc_mss_from_mtu(dgram_mtu));
        
        // We refuse to use snd_mss less than MinAllowedMss.
        // If the calculated value is less, force no DF flag.
        // The local interface will have no problems and weird
        // routers with so low MTU will fragment themselves.
        if (snd_mss < Constants::MinAllowedMss) {
            snd_mss = Constants::MinAllowedMss;
            dont_fragment = false;
        }
        
        // If this is not the initial update or snd_mss and dont_fragment match
        // the current PCB variables, there is nothing else to do.
        if (check_change && snd_mss == pcb->snd_mss &&
            dont_fragment == ((pcb->ip_send_flags & IpSendFlags::DontFragmentFlag) != 0))
        {
            return false;
        }
        
        // Update the snd_mss.
        pcb->snd_mss = snd_mss;
        
        // Update IpSendFlags::DontFragmentFlag.
        if (dont_fragment) {
            pcb->ip_send_flags |= IpSendFlags::DontFragmentFlag;
        } else {
            pcb->ip_send_flags &= ~IpSendFlags::DontFragmentFlag;
        }
        
        return true;
    }
    
    // Update the snd_mss and IpSendFlags::DontFragmentFlag based on the current
    // mtu_ref information, and possibly do any necessarily fixups like
    // cwnd and rtx_timer.
    static void pcb_update_pmtu_from_mtu_ref (TcpPcb *pcb)
    {
        AMBRO_ASSERT(pcb->mtu_ref.isSetup())
        
        bool can_output = can_output_in_state(pcb->state);
        
        // Update the snd_mss and DF flag based on the mtu_ref information.
        // In states where data output is not possible, this is called
        // with check_change==false in order to just set/clear the DF flag
        // as needed. In this case snd_mss is also set but that does not
        // matter in these states.
        bool mtu_updated = pcb_update_pmtu_base(pcb, can_output);
        
        // If no update was done or we are not in a state where output
        // is possible, then there is no need to perform the fixups.
        if (!mtu_updated || !can_output) {
            return;
        }
        
        // Make sure that ssthresh does not become lesser than snd_mss.
        if (pcb->ssthresh < pcb->snd_mss) {
            pcb->ssthresh = pcb->snd_mss;
        }
        
        if (pcb->hasFlag(PcbFlags::CWND_INIT)) {
            // Recalculate initial CWND (RFC 5681 page 5).
            pcb->cwnd = calc_initial_cwnd(pcb->snd_mss);
        } else {
            // The standards do not require updating cwnd for the new snd_mss,
            // but we have to make sure that cwnd does not become less than snd_mss.
            // We also set cwnd to snd_mss if we have done a retransmission from the
            // rtx_timer and no new ACK has been received since; since the cwnd would
            // have been set to snd_mss then, and should not have been changed since
            // (the latter is not trivial to see though).
            if (pcb->cwnd < pcb->snd_mss || pcb->hasFlag(PcbFlags::RTX_ACTIVE)) {
                pcb->cwnd = pcb->snd_mss;
            }
        }
        
        // The change of snd_mss might have broken the invariant that the
        // rtx_timer is started if and only if pcb_need_rtx_timer (ignoring
        // idle timeout). Note that snd_mss affects pcb_need_rtx_timer only
        // when snd_wnd==0 (to determine if window probing is needed), hence
        // the condition here.
        if (pcb_has_snd_outstanding(pcb) && pcb->snd_wnd == 0) {
            pcb_update_rtx_timer(pcb);
        }
    }
    
    // Send an RST as a reply to a received segment.
    // This conforms to RFC 793 handling of segments not belonging to a known
    // connection.
    static void send_rst_reply (TcpProto *tcp, Ip4DgramMeta const &ip_meta,
                                TcpSegMeta const &tcp_meta, size_t tcp_data_len)
    {
        SeqType rst_seq_num;
        bool rst_ack;
        SeqType rst_ack_num;
        if ((tcp_meta.flags & Tcp4FlagAck) != 0) {
            rst_seq_num = tcp_meta.ack_num;
            rst_ack = false;
            rst_ack_num = 0;
        } else {
            rst_seq_num = 0;
            rst_ack = true;
            rst_ack_num = tcp_meta.seq_num + tcplen(tcp_meta.flags, tcp_data_len);
        }
        
        send_rst(tcp, ip_meta.local_addr, ip_meta.remote_addr,
                 tcp_meta.local_port, tcp_meta.remote_port,
                 rst_seq_num, rst_ack, rst_ack_num);
    }
    
    static void send_rst (TcpProto *tcp, Ip4Addr local_addr, Ip4Addr remote_addr,
                          PortType local_port, PortType remote_port,
                          SeqType seq_num, bool ack, SeqType ack_num)
    {
        FlagsType flags = Tcp4FlagRst | (ack ? Tcp4FlagAck : 0);
        TcpSegMeta tcp_meta = {local_port, remote_port, seq_num, ack_num, 0, flags};
        send_tcp(tcp, local_addr, remote_addr, IpSendFlags::DontFragmentNow,
                 tcp_meta, IpBufRef{}, nullptr);
    }
    
private:
    // Determine if sending can be delayed in expectation of a larger segment.
    static bool pcb_may_delay_snd (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        return pcb->snd_buf_cur.tot_len < pcb->snd_mss &&
               pcb->snd_psh_index <= pcb_snd_offset(pcb) &&
               snd_open_in_state(pcb->state);
    }
    
    static SeqType pcb_output_segment (TcpPcb *pcb, IpBufRef data, bool fin, SeqType rem_wnd)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(data.tot_len <= pcb->sndBufLen())
        AMBRO_ASSERT(!fin || !snd_open_in_state(pcb->state))
        AMBRO_ASSERT(data.tot_len > 0 || fin)
        AMBRO_ASSERT(rem_wnd > 0)
        
        // Determine segment data length.
        size_t seg_data_len = APrinter::MinValueU(data.tot_len, APrinter::MinValueU(rem_wnd, pcb->snd_mss));
        
        // Determine offset from start of send buffer.
        size_t offset = pcb->sndBufLen() - data.tot_len;
        
        // Determine segment flags, calculate sequence length.
        FlagsType seg_flags = Tcp4FlagAck;
        SeqType seg_seqlen = seg_data_len;
        if (seg_data_len == data.tot_len && fin && rem_wnd > seg_data_len) {
            seg_flags |= Tcp4FlagFin|Tcp4FlagPsh;
            seg_seqlen++;
        }
        else if (pcb->snd_psh_index > offset && pcb->snd_psh_index <= offset + seg_data_len) {
            seg_flags |= Tcp4FlagPsh;
        }
        
        // Send the segment.
        SeqType seq_num = seq_add(pcb->snd_una, offset);
        TcpSegMeta tcp_meta = {pcb->local_port, pcb->remote_port, seq_num, pcb->rcv_nxt,
                               Input::pcb_ann_wnd(pcb), seg_flags};
        IpErr err = send_tcp(pcb->tcp, pcb->local_addr, pcb->remote_addr,
                             pcb->ip_send_flags, tcp_meta,
                             data.subTo(seg_data_len), pcb);
        
        // These things are needed only when a segment was sent.
        if (err == IpErr::SUCCESS) {
            // Calculate the end sequence number of the sent segment.
            SeqType seg_endseq = seq_add(seq_num, seg_seqlen);
            
            // Stop a round-trip-time measurement if we have retransmitted
            // a segment containing the associated sequence number.
            if (pcb->hasFlag(PcbFlags::RTT_PENDING) &&
                seq_lte(seq_num, pcb->rtt_test_seq, pcb->snd_una) &&
                seq_lt(pcb->rtt_test_seq, seg_endseq, pcb->snd_una))
            {
                pcb->clearFlag(PcbFlags::RTT_PENDING);
            }
            
            // Did we send anything new?
            if (seq_lt(pcb->snd_nxt, seg_endseq, pcb->snd_una)) {
                // Start a round-trip-time measurement if not already started.
                if (!pcb->hasFlag(PcbFlags::RTT_PENDING)) {
                    pcb_start_rtt_measurement(pcb);
                }
                
                // Bump snd_nxt.
                pcb->snd_nxt = seg_endseq;
            }
            
            // If we sent FIN set the FIN_SENT flag.
            if ((seg_flags & Tcp4FlagFin) != 0) {
                pcb->setFlag(PcbFlags::FIN_SENT);
            }
        }
        
        return seg_seqlen;
    }
    
    /**
     * Transmits one segment starting at the front of the send buffer.
     * Used for retransmission and window probing.
     */
    static void pcb_output_front (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        AMBRO_ASSERT(pcb_has_snd_outstanding(pcb))
        
        // Compute a maximum number of sequence counts to send.
        // We must not send more than one segment, but we must be
        // able to send at least something in case of window probes.
        SeqType rem_wnd = APrinter::MinValueU(pcb->snd_mss, APrinter::MaxValue((SeqType)1, pcb->snd_wnd));
        
        // Send a segment from the start of the send buffer.
        IpBufRef data = (pcb->con != nullptr) ? pcb->con->m_snd_buf : IpBufRef{};
        bool fin = !snd_open_in_state(pcb->state);
        SeqType seg_seqlen = pcb_output_segment(pcb, data, fin, rem_wnd);
        AMBRO_ASSERT(seg_seqlen > 0 && seg_seqlen <= rem_wnd)
    }
    
    static void pcb_increase_cwnd_acked (TcpPcb *pcb, SeqType acked)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        // Increase cwnd by acked but no more than snd_mss.
        SeqType cwnd_inc = APrinter::MinValueU(acked, pcb->snd_mss);
        pcb_increase_cwnd(pcb, cwnd_inc);
        
        // No longer have initial CWND.
        pcb->clearFlag(PcbFlags::CWND_INIT);
    }
    
    static void pcb_increase_cwnd (TcpPcb *pcb, SeqType cwnd_inc)
    {
        SeqType cwnd = pcb->cwnd;
        pcb->cwnd = (cwnd_inc > UINT32_MAX - cwnd) ? UINT32_MAX : (cwnd + cwnd_inc);
    }
    
    // Sets sshthresh according to RFC 5681 equation (4).
    static void pcb_update_ssthresh_for_rtx (TcpPcb *pcb)
    {
        AMBRO_ASSERT(can_output_in_state(pcb->state))
        
        SeqType half_flight_size = seq_diff(pcb->snd_nxt, pcb->snd_una) / 2;
        SeqType two_smss = 2 * (SeqType)pcb->snd_mss;
        pcb->ssthresh = APrinter::MaxValue(half_flight_size, two_smss);
    }
    
    static void pcb_start_rtt_measurement (TcpPcb *pcb)
    {
        // Set the flag, remember the sequence number and the time.
        pcb->setFlag(PcbFlags::RTT_PENDING);
        pcb->rtt_test_seq = pcb->snd_nxt;
        pcb->rtt_test_time = Clock::getTime(Context());
    }
    
    static IpErr send_tcp (TcpProto *tcp, Ip4Addr local_addr, Ip4Addr remote_addr,
                           uint8_t ip_flags, TcpSegMeta const &tcp_meta, IpBufRef data,
                           IpSendRetry::Request *retryReq)
    {
        // Compute length of TCP options.
        uint8_t opts_len = (tcp_meta.opts != nullptr) ? TcpUtils::calc_options_len(*tcp_meta.opts) : 0;
        
        // Allocate memory for headers.
        TxAllocHelper<BufAllocator, Tcp4Header::Size+TcpUtils::MaxOptionsWriteLen, HeaderBeforeIp4Dgram>
            dgram_alloc(Tcp4Header::Size+opts_len);
        
        // Caculate the offset+flags field.
        FlagsType offset_flags = ((FlagsType)(5+opts_len/4) << TcpOffsetShift) | tcp_meta.flags;
        
        // Write the TCP header.
        auto tcp_header = Tcp4Header::MakeRef(dgram_alloc.getPtr());
        tcp_header.set(Tcp4Header::SrcPort(),     tcp_meta.local_port);
        tcp_header.set(Tcp4Header::DstPort(),     tcp_meta.remote_port);
        tcp_header.set(Tcp4Header::SeqNum(),      tcp_meta.seq_num);
        tcp_header.set(Tcp4Header::AckNum(),      tcp_meta.ack_num);
        tcp_header.set(Tcp4Header::OffsetFlags(), offset_flags);
        tcp_header.set(Tcp4Header::WindowSize(),  tcp_meta.window_size);
        tcp_header.set(Tcp4Header::Checksum(),    0);
        tcp_header.set(Tcp4Header::UrgentPtr(),   0);
        
        // Write any TCP options.
        if (tcp_meta.opts != nullptr) {
            TcpUtils::write_options(*tcp_meta.opts, dgram_alloc.getPtr() + Tcp4Header::Size);
        }
        
        // Construct the datagram reference including any data.
        IpBufNode data_node;
        if (data.tot_len > 0) {
            data_node = data.toNode();
            dgram_alloc.setNext(&data_node, data.tot_len);
        }
        IpBufRef dgram = dgram_alloc.getBufRef();
        
        // Calculate TCP checksum.
        IpChksumAccumulator chksum_accum;
        chksum_accum.addWords(&local_addr.data);
        chksum_accum.addWords(&remote_addr.data);
        chksum_accum.addWord(APrinter::WrapType<uint16_t>(), Ip4ProtocolTcp);
        chksum_accum.addWord(APrinter::WrapType<uint16_t>(), dgram.tot_len);
        chksum_accum.addIpBuf(dgram);
        uint16_t calc_chksum = chksum_accum.getChksum();
        tcp_header.set(Tcp4Header::Checksum(), calc_chksum);
        
        // Add DontFragmentNow flag since we should never
        // need to fragment sent segments ourselves.
        ip_flags |= IpSendFlags::DontFragmentNow;
        
        // Send the datagram.
        Ip4DgramMeta meta = {local_addr, remote_addr, TcpProto::TcpTTL, Ip4ProtocolTcp};
        return tcp->m_stack->sendIp4Dgram(meta, dgram, retryReq, ip_flags);
    }
};

#include <aipstack/EndNamespace.h>

#endif
