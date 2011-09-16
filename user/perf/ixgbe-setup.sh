#!/bin/sh
ethtool -A eth3 autoneg off rx off tx off
ethtool -s eth3 speed 10000 duplex full

