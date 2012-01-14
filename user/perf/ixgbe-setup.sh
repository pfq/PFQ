#!/bin/sh
ethtool -A $1 autoneg off rx off tx off
ethtool -s $1 speed 10000 duplex full
