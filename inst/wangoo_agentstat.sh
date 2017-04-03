#!/bin/bash

Rscript /Users/benjamin/Github/stock_conspiracy/wangoo_agentstat.R $1 $2 $3
cat tmp.txt | awk '{gsub(/\\/,"")}1' | awk '{gsub(/{"code":"0","message":"","returnValues":"/,"")}1' | awk '{gsub(/}]"}/,"}]")}1' > tmp.json
/Users/benjamin/anaconda/bin/python /Users/benjamin/Github/stock_conspiracy/wangoo_agentstat.py $2 $3
