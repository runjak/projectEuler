#!/bin/bash
find p*|grep .hs|xargs wc -l|sort -n
