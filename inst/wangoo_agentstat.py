import pandas as pd
x=pd.read_json('tmp.json')
overbought=x.iloc[0:10,[1,3,5,9,11,13,7]]
oversold=x.iloc[0:10,[2,4,6,10,12,14,8]]

import sys
overbought['start']=sys.argv[1]
overbought['end']=sys.argv[2]
overbought['marked']=""
overbought['comment']=""
oversold['start']=sys.argv[1]
oversold['end']=sys.argv[2]
oversold['marked']=""
oversold['comment']=""
overbought.to_excel('overbought.xls',index=False)
oversold.to_excel('oversold.xls',index=False)
