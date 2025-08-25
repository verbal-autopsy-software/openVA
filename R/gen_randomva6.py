from pycrossva.transform import transform
import pandas as pd

odk = pd.read_csv('RandomVA6.csv')
results = transform(("2016WHOv151", "InSilicoVA"), odk, "id")
results.to_csv("openva_rva6.csv", index=False)
