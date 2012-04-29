#!/bin/bash
#
#  (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
# 
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
# 
#  The full GNU General Public License is included in this distribution in
#  the file called "COPYING".
# 

set -e

PFQ_KCOMPAT=/usr/local/include/pfq/pfq_kcompat.h
PFQ_SYMVERS=/lib/modules/`uname -r`/kernel/net/pfq/Module.symvers
PFQ_OMATIC_VERSION=1.0

BOLD='\e[1m'
NC='\e[0m'

PFQ="${BOLD}[PFQ]${NC}"

terminate()
{
    echo -e $1
    exit 1
}


preconditions()
{
    test -r $PFQ_KCOMPAT || terminate "${PFQ} error: could not locate pfq-kcompat header (exit forced)!"
    test -r $PFQ_SYMVERS || terminate "${PFQ} error: could not locate pfq Module.symvers (exit forced)!"
    test -r Makefile     || terminate "${PFQ} error: Makefile not found!"
}


patch()
{
    for src in $*
    do
        if /bin/grep -q -E "netif_rx(.*)|netif_receive_skb(.*)|napi_gro_receive(.*,.*)" $src 
        then
            if [ ! -r "$src.orig" ]; then
                echo -e "${PFQ} patching " $src
                mv $src $src.orig
                echo "#include \"/usr/local/include/pfq/pfq_kcompat.h\"" > $src
                cat $src.orig >> $src
            else
                if ! /bin/grep -q pfq_kcompat $src
                then
                    terminate "${PFQ} Internal error. Aborted!"
                fi
                echo -e "${PFQ} ${src} is already patched :)"
            fi
        fi
    done
}


echo -e "${PFQ} pfq'omatic v${PFQ_OMATIC_VERSION}."

preconditions

files=`/usr/bin/find . -name \*.c`

echo -e "${PFQ} searching for source codes..."

patch $files

cp $PFQ_SYMVERS . 

echo -e "${PFQ} compiling the driver..."

make

echo -e "${PFQ} done."
