import openpyxl,glob,re,unicodedata,pandas as pd
D='/root/.claude/uploads/b737dfec-6303-5562-ad24-25a0dfa2a79b/'
BL=["cao nhất","thấp nhất","trung vị","thứ hạng","trung bình","tối đa","tối thiểu","ghi chú"]
OLDCOLS=[2,3,4,5,6,7,8,9,10]            # 2018-2020: 9 chỉ tiêu
NEWCOLS=[2,3,4,5,6,14,15,16,17]         # 2021-2024: 9 chỉ tiêu tương ứng trong 16
NAMES=['i1_peers_pay','i2_result','i3_harass','i4_acceptable','i5_inspect','i6_over10pct','i7_land','i8_bidcomm','i9_court']
files={}
for f in glob.glob(D+'*.xlsx'):
    y=int(re.search(r'(20\d\d)',f.split('-',2)[-1]).group(1)); files[y]=f
out=[]
for y in sorted(files):
    wb=openpyxl.load_workbook(files[y],read_only=True,data_only=True)
    rows=list(wb['Chi phí không chính thức'].iter_rows(values_only=True))
    cols=OLDCOLS if y<=2020 else NEWCOLS
    seen=set()
    for r in rows[1:]:
        p=r[0]
        if not p: continue
        p=unicodedata.normalize('NFC',str(p).strip())
        if any(b in p.lower() for b in BL) or p in seen: continue
        seen.add(p)
        out.append(dict(province=p,year=y,CSTP5_x=r[1],**{n:r[c] for n,c in zip(NAMES,cols)}))
df=pd.DataFrame(out); df.to_csv('run2/indicators_cstp5.csv',index=False)
print(df.groupby('year').size())
pan=pd.read_csv('repo/pci-informal-charges/data_processed/pci_panel_final.csv')
m=pan.merge(df,on=['province','year'],how='left')
print('merged',m.shape,'missing',m['i1_peers_pay'].isna().sum(),'max|CSTP5 diff|',(m.CSTP5-m.CSTP5_x).abs().max())
print(m[m.i1_peers_pay.isna()][['province','year']].head())
print(m[NAMES].describe().loc[['count','mean','min','max']].round(3))
m.to_csv('run2/panel_with_indicators.csv',index=False)
