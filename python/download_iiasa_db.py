
import pyam

# to store your credentials, please run once
#    pyam.iiasa.set_config("login"", "password"")

def download_iiasa_db_py(db):

    conn = pyam.iiasa.Connection(db)

    df = conn.query(
        model=["Euro-Calliope 2.0","IMAGE 3.2","MEESA v1.1","MESSAGEix-GLOBIOM 1.2","OSeMBE v1.0.0","PRIMES 2022","PROMETHEUS 1.2","REMIND 2.1","TIAM-ECN 1.2","WITCH 5.1"],
        variable='*',
        region=["Europe", "Europe (incl. Turkey)", "Europe (excl. Turkey)", "EU27 & UK", "EU27", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "The Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom"],
        scenario=["DIAG-Base","DIAG-NPI","DIAG-NZero","DIAG-C80-gr5","DIAG-C0to80-gr5","DIAG-C400-lin","DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear","DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighElec-Supply","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff"]
    )

    from datetime import datetime
    now = datetime.now()
    filename = "data_" + now.strftime("%Y_%m_%d_%H.%M.%S")

    df.to_excel("./data/" + filename + ".xlsx")
    df.data.to_csv("./data/" + filename + ".csv", index = None, header=True)
