#!/bin/sh              

ifconfig $1 up
ethtool -A $1 autoneg off rx off tx off
ethtool -s $1 speed 10000 duplex full
irq-affinity -a naive $1
