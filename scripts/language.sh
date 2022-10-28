#!/bin/bash
language=`xkblayout-state print "%n"`
echo ${language:0:2}