#!/bin/sh

WIFI_IFACE=$1
ETH_IFACE=$2

if ip link show "$WIFI_IFACE" | grep -q "state UP"; then
    echo ""
elif ip link show "$ETH_IFACE" | grep -q "state UP"; then
    echo ""
else
    echo ""
fi
