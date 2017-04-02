import pandas as pd
x=pd.read_json('tmp.json')
overbought=x.iloc[:,[1,3,5,9,11,13,7]]
oversold=x.iloc[:,[2,4,6,10,12,14,8]]
overbought=pd.DataFrame(overbought.values)
oversold=pd.DataFrame(oversold.values)
overbought=overbought.append(oversold)

import sys
overbought['date']=sys.argv[1]
overbought['comment']=""

overbought.to_excel('buyer.xls',index=False)
