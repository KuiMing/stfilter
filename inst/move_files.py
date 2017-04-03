from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
from pydrive.drive import GoogleDrive
import pandas as pd
import sys
again=True
while again:
    gauth = GoogleAuth()
    gauth.LoadCredentialsFile("mycreds.txt")
    if gauth.credentials is None:
        # Authenticate if they're not there
        gauth.LocalWebserverAuth()
    elif gauth.access_token_expired:
        # Refresh them if expired
        gauth.Refresh()
    else:
        # Initialize the saved creds
        gauth.Authorize()
    drive = GoogleDrive(gauth)
    try:
        x=drive.auth.service.files()
        again=False
    except:
        again=True

newstock=pd.read_csv('newstock.csv')
fl={"daily":"0B7hNPqmD3n6eLUJnVlo0WU41ZVU",
       "recent":"0B7hNPqmD3n6eVmplbEZOeFI3QmM",
       "fil_recent":"0B7hNPqmD3n6ebHczY05KWjFrQm8",
       "fil_daily":"0B7hNPqmD3n6eRU9DSG1pd1dYS1E"}
folder=fl[sys.argv[1]]
def move_file(url):
    if url=="":
        return("")
    ID=url.replace('https://docs.google.com/spreadsheets/d/','').replace('/','')
    files=drive.CreateFile({'id':ID})
    title = files['title']
    info=x.copy(fileId=ID, body={"parents": [{"kind": "drive#fileLink",
                                 "id": folder}],"title":title}).execute()
    drive.CreateFile({'id': ID}).Delete()
    return("https://docs.google.com/spreadsheets/d/"+info['id'])



newstock['url']=list(map(move_file, newstock.url.values))
newstock.to_csv('newstock.csv',index=False)
