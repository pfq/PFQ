/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * The full GNU General Public License is included in this distribution in
 * the file called "COPYING".
 *
 ****************************************************************/

#pragma once

#include <pfq/lang/lang.hpp>
#include <pfq/lang/details.hpp>

#include <linux/filter.h>
#include <arpa/inet.h>
#include <pcap/pcap.h>

#include <functional>

namespace pfq
{
namespace lang
{
    namespace
    {
        static int
        fix_program(struct bpf_program *program, struct sock_fprog *fcode, int is_mmapped)
        {
            size_t prog_size;
            struct bpf_insn *p;
            struct bpf_insn *f;
            int len, i;

            /*
             * Make a copy of the filter, and modify that copy if
             * necessary.
             */
            prog_size = sizeof(program->bf_insns) * program->bf_len;
            len = program->bf_len;
            f = (struct bpf_insn *)malloc(prog_size);
            if (f == NULL)
                throw std::runtime_error("malloc");

            memcpy(f, program->bf_insns, prog_size);
            fcode->len = len;
            fcode->filter = (struct sock_filter *) f;

            for (i = 0; i < len; ++i) {
                p = &f[i];
                /*
                 * What type of instruction is this?
                 */
                switch (BPF_CLASS(p->code)) {

                case BPF_RET:
                    /*
                     * It's a return instruction; are we capturing
                     * in memory-mapped mode?
                     */
                    if (!is_mmapped) {
                        /*
                         * No; is the snapshot length a constant,
                         * rather than the contents of the
                         * accumulator?
                         */
                        if (BPF_MODE(p->code) == BPF_K) {
                            /*
                             * Yes - if the value to be returned,
                             * i.e. the snapshot length, is
                             * anything other than 0, make it
                             * 65535, so that the packet is
                             * truncated by "recvfrom()",
                             * not by the filter.
                             *
                             * XXX - there's nothing we can
                             * easily do if it's getting the
                             * value from the accumulator; we'd
                             * have to insert code to force
                             * non-zero values to be 65535.
                             */
                            if (p->k != 0)
                                p->k = 65535;
                        }
                    }
                    break;

                case BPF_LD:
                case BPF_LDX:
                    /*
                     * It's a load instruction; is it loading
                     * from the packet?
                     */
                    switch (BPF_MODE(p->code)) {

                    case BPF_ABS:
                    case BPF_IND:
                    case BPF_MSH:
                        /*
                         * Yes; are we in cooked mode?
                         */
                        // if (handle->md.cooked) {
                        //     /*
                        //      * Yes, so we need to fix this
                        //      * instruction.
                        //      */
                        //     if (fix_offset(p) < 0) {
                        //         /*
                        //          * We failed to do so.
                        //          * Return 0, so our caller
                        //          * knows to punt to userland.
                        //          */
                        //         return 0;
                        //     }
                        // }
                        break;
                    }
                    break;
                }
            }
            return 1;	/* we succeeded */
        }


        auto bpf = [] (const std::string &filt) {
            struct bpf_program prog;
            struct sock_fprog fcode;
            pcap_t *handle;

            handle = pcap_open_dead(DLT_RAW, 65535);
            if(!handle)
                throw std::runtime_error("bpf: pcap_open_dead");

            if (pcap_compile(handle, &prog, filt.c_str(), 0, 0) == -1)
                throw std::runtime_error("bpf: pcap_compile");

            // convert it to sock_fprog...
            if (fix_program(&prog, &fcode, 1) != 1)
                throw std::runtime_error("bpf: code could not be set");

            pcap_close(handle);

            // free(fcode.filter);

            return mfunction1("bfp", fcode);
        };
    }

}  // namespace lang
}  // namespace pfq

