
import pyam

# to store your credentials, please run once
#    pyam.iiasa.set_config("login"", "password"")

def download_iiasa_meta_py(fileName, db, default_only = False):

    conn = pyam.iiasa.Connection(db)
    
    df = conn.properties(default=default_only)
    
    df.to_excel(fileName + ".xlsx")

