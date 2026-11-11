import json,os,urllib.request as U
E=os.getenv('MAKEFUN_ENV_URL');V=[0,100,320,330,500,900,0];M=99999
KN=(-33,-31,-18,-14,14,18,31,33);DG=(-17,-15,15,17);ST=(-16,16,-1,1);AL=DG+ST
B=[0]*128;T=1;C=0;EP=-1;N=[0]
def VD(t):return -1<t<128 and not t&136
def PF(f):
 global B,T,C,EP
 B=[0]*128;i=0;p=f.split()
 for ch in p[0]:
  if ch=='/':i+=8
  elif ch.isdigit():i+=int(ch)
  else:
   v='PNBRQK'.index(ch.upper())+1;B[i]=v if ch.isupper() else -v;i+=1
 T=1 if p[1]=='w' else -1;C=sum({'K':1,'Q':2,'k':4,'q':8}.get(c,0)for c in p[2]);EP=-1 if p[3]=='-' else (8-int(p[3][1]))*16+ord(p[3][0])-97
def A(s,by):
 for dd in((15,17),(-15,-17))[by<0]:
  t=s+dd
  if VD(t)and B[t]==by:return 1
 for d in KN+AL:
  t=s+d
  if VD(t)and B[t]==by*(2 if d in KN else 6):return 1
 for ds,tp in((ST,4),(DG,3)):
  for d in ds:
   t=s+d
   while VD(t):
    v=B[t]
    if v:
     if v in(by*tp,by*5):return 1
     break
    t+=d
 return 0
def PS(a,i,sg):
 x=i&7;r=7-(i>>4)if sg>0 else i>>4;cx=abs(2*x-7);cy=abs(2*r-7)
 if a==1:return(r-1)*6
 if a==2:return 16-2*cx-cy
 if a==6:return(14-2*cx-2*cy)if EG else cx*2-r*4
 return 0
def EV():
 global EG;EG=sum(V[abs(p)]for p in B if abs(p)in(2,3,4,5))<2400;s=0
 for i,p in enumerate(B):
  if p:
   sg=1 if p>0 else -1;s+=sg*(V[abs(p)]+PS(abs(p),i,sg))
 return s*T
def G(cap=0):
 ms=[]
 for i,p in enumerate(B):
  if not p or(p>0)!=(T>0):continue
  a=abs(p)
  if a==1:
   d=-16*T;tg=[];t=i+d
   if not B[t]:
    tg.append(t)
    if(i>>4)==(6 if T>0 else 1)and not B[i+2*d]:ms.append(i|(i+2*d)<<7)
   for dd in(d-1,d+1):
    t=i+dd
    if VD(t)and((B[t]and(B[t]>0)!=(T>0))or t==EP):tg.append(t)
   for t in tg:
    if(t>>4)in(0,7):
     for pr in(5,2):ms.append(i|t<<7|pr<<14)
    elif not cap or B[t]or t==EP:ms.append(i|t<<7)
  else:
   for d in(0,0,KN,DG,ST,AL,AL)[a]:
    t=i+d
    while VD(t):
     v=B[t]
     if v:
      if(v>0)!=(T>0):ms.append(i|t<<7)
      break
     if not cap:ms.append(i|t<<7)
     if a%4==2:break
     t+=d
 if not cap and not A(B.index(6*T),-T):
  bs=116 if T>0 else 4;rr=(1,2)if T>0 else(4,8);e=-T
  for sg,bt in((2,rr[0]),(-2,rr[1])):
   if C&bt and not B[bs+sg//2]and not B[bs+sg]and(sg>0 or not B[bs-3])and not A(bs+sg//2,e)and not A(bs+sg,e):ms.append(bs|(bs+sg)<<7)
 return ms
def MV(m):
 global T,C,EP
 o=(B[:],T,C,EP);f=m&127;t=(m>>7)&127;pr=m>>14;p=B[f]
 if abs(p)==1 and t==EP and not B[t]and(f&7)!=(t&7):B[t+16 if p>0 else t-16]=0
 EP=(f+t)//2 if abs(p)==1 and abs(t-f)==32 else -1
 if abs(p)==6:C&=~(3 if p>0 else 12)
 for sq,bt in((119,1),(112,2),(7,4),(0,8)):
  if f==sq or t==sq:C&=~bt
 B[t]=pr*(1 if p>0 else -1)if pr else p;B[f]=0
 if abs(p)==6 and abs(t-f)==2:
  if t>f:B[f+1]=B[f+3];B[f+3]=0
  else:B[f-1]=B[f-4];B[f-4]=0
 T=-T;return o
def RS(o):
 global B,T,C,EP;B,T,C,EP=o
def L(cap=0):
 r=[]
 for m in G(cap):
  o=MV(m)
  if not A(B.index(-6*T),T):r.append(m)
  RS(o)
 return r
def O(m):
 f=m&127;t=(m>>7)&127;v=B[t];return(10*V[abs(v)]-V[abs(B[f])]//10 if v else 0)+V[m>>14]
def S(d,a,b,ply):
 N[0]+=1
 if N[0]>15000:1/0
 chk=A(B.index(6*T),-T)
 if d<=0 and not chk:
  e=EV()
  if e>=b:return e
  if e>a:a=e
  ms=L(1)
 else:ms=L()
 ms.sort(key=O,reverse=True)
 if not ms:return-M+ply if chk else(0 if d>0 else a)
 for m in ms:
  o=MV(m);s=-S(d-1 if d>0 else 0,-b,-a,ply+1);RS(o)
  if s>a:a=s
  if a>=b:break
 return a
def UC(m):
 f=m&127;t=(m>>7)&127;pr=m>>14
 s=chr(97+f%8)+str(8-f//16)+chr(97+t%8)+str(8-t//16);return s+'nq'[pr//3]if pr else s
def BEST(fen,legal=None):
 PF(fen);N[0]=0;ms=L()
 ms.sort(key=O,reverse=True);bm=ms[0]if ms else 0
 for d in range(1,9):
  try:
   a=-M;c=bm
   for m in ms:
    o=MV(m);sc=-S(d-1,-M,-a,1);RS(o)
    if sc>a:a=sc;c=m
   bm=c
  except Exception:break
 u=UC(bm)
 if legal and u not in legal:u=legal[0]
 return u
def POST(p,bdy):
 q=U.Request(E+p,data=json.dumps(bdy).encode(),headers={'content-type':'application/json'})
 with U.urlopen(q)as r:return json.load(r)
if __name__=='__main__'and E:
 while 1:
  g=POST('/game',{})
  if g['status']=='done':break
  while g['status']=='your_move':
   g=POST('/move',{'game':g['game'],'move':BEST(g['fen'],g.get('legal_moves'))})
