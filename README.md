# R_h
#include<cstdio>
#include<iostream>
#include<cstring>
#include<string>
#define MAXN 200010
#define inf 0x7fffffff
using namespace std;
struct node{
    int nxt,to;
}e[MAXN<<1];
int head[MAXN],son[MAXN],top[MAXN],dep[MAXN];
int n,m,siz[MAXN],f[MAXN],a[MAXN],val[MAXN];
int cnt,tot,id[MAXN],rk[MAXN],dt,rt;
inline void swap(int &x,int &y){x^=y^=x^=y;}
char s[10];
inline int max_(int a,int b){return a>b?a:b;}
inline int min_(int a,int b){return a<b?a:b;}
inline int read(){
    int s=0,w=1;
    char ch=getchar();
    while(ch<'0'||ch>'9'){
        if(ch=='-')w=-1;
        ch=getchar();
    }while(ch>='0'&&ch<='9'){
        s=(s<<1)+(s<<3)+(ch^48);
        ch=getchar();
    }return s*w;
}
struct Node{
    int ls,rs,sum,tag,maxn,minn,l,r;
}tr[MAXN<<2];
inline void add(int x,int y,int w){
    e[++tot].to=y;
    e[tot].nxt=head[x];
    head[x]=tot;
    a[tot]=w;
}
void dfs1(int u){
    siz[u]=1;
    for(int i=head[u];i;i=e[i].nxt){
        int v=e[i].to;
        if(v==f[u])continue;
        f[v]=u;
        val[v]=a[i];
        dep[v]=dep[u]+1;
        dfs1(v);
        siz[u]=siz[v]+1;
        if(siz[son[u]]<siz[v])son[u]=v;
    }
}void dfs2(int u,int t){
    top[u]=t;
    rk[id[u]=++dt]=u;
    if(!son[u])return;
    dfs2(son[u],t);
    for(int i=head[u];i;i=e[i].nxt){
        int v=e[i].to;
        if(v!=f[u]&&v!=son[u])dfs2(v,v);
    }
}
#define lc tr[x].ls
#define rc tr[x].rs
inline void pushup(int x){
    tr[x].sum=tr[lc].sum+tr[rc].sum;
    tr[x].maxn=max_(tr[lc].maxn,tr[rc].maxn);
    tr[x].minn=min_(tr[lc].minn,tr[rc].minn);
}
inline void pushdown(int x){
    if(tr[x].tag){
        tr[lc].sum=-tr[lc].sum,tr[lc].tag^=1;
        tr[rc].sum=-tr[rc].sum,tr[rc].tag^=1;
        int x1=tr[lc].maxn,y1=tr[lc].minn;
        int x2=tr[rc].maxn,y2=tr[rc].minn;
        tr[lc].maxn=-y1,tr[lc].minn=-x1;
        tr[rc].maxn=-y2,tr[rc].minn=-x2;
        tr[x].tag=0;
    }
}
void build(int li,int ri,int &x){
    x=++cnt;
    tr[x].l=li;tr[x].r=ri;
    if(li==ri){
        tr[x].minn=tr[x].maxn=tr[x].sum=val[rk[li]];
        return;
    }int mid=(li+ri)>>1;
    build(li,mid,lc);
    build(mid+1,ri,rc);
    pushup(x);
}
void change(int x,int val,int cur){
    if(tr[x].l==tr[x].r){
        tr[x].sum=tr[x].maxn=tr[x].minn=val;
        return;
    }pushdown(x);
    int mid=(tr[x].l+tr[x].r)>>1;
    if(cur<=mid)change(lc,val,cur);
    else change(rc,val,cur);
    pushup(x);
}
void modify(int li,int ri,int x){
    if(tr[x].l>=li&&tr[x].r<=ri){
        tr[x].sum=-tr[x].sum,tr[x].tag^=1;
        int x1=tr[x].maxn,y1=tr[x].minn;
        tr[x].maxn=-y1,tr[x].minn=-x1;
        return;
    }pushdown(x);
    int mid=(tr[x].l+tr[x].r)>>1;
    if(li<=mid)modify(li,ri,lc);
    if(mid<ri)modify(li,ri,rc);
    pushup(x);
}
int query_s(int li,int ri,int x){
    if(tr[x].l>ri||tr[x].r<li)return 0;
    if(tr[x].l>=li&&tr[x].r<=ri)return tr[x].sum;
    pushdown(x);
    int mid=(tr[x].l+tr[x].r)>>1,ans=0;
    ans=query_s(li,ri,lc)+query_s(li,ri,rc);
    return ans;
}
int query_x(int li,int ri,int x){
    if(tr[x].l>=li&&tr[x].r<=ri)return tr[x].maxn;
    pushdown(x);
    int mid=(tr[x].l+tr[x].r)>>1,ans=-inf;
    if(li<=mid)ans=max_(ans,query_x(li,ri,lc));
    if(mid<ri)ans=max_(ans,query_x(li,ri,rc));
    return ans;
}
int query_n(int li,int ri,int x){
    if(tr[x].l>=li&&tr[x].r<=ri){return tr[x].minn;}
    pushdown(x);int mid=(tr[x].l+tr[x].r)>>1,ans=inf;
    if(li<=mid)ans=min_(ans,query_n(li,ri,lc));
    if(mid<ri)ans=min_(ans,query_n(li,ri,rc));
    return ans;
}
void Segment_change(int x,int y){
    int fx=top[x],fy=top[y];
    while(fx!=fy){
        if(dep[fx]<dep[fy])swap(x,y),swap(fx,fy);
        modify(id[fx],id[x],rt);
        x=f[fx],fx=top[x];
    }if(id[x]>id[y])swap(x,y);
    modify(id[x]+1,id[y],rt);
}
int query(int x,int y,int ck){//ck 0sum 1max 2min
    int fx=top[x],fy=top[y],ans;
    if(ck==0)ans=0;else if(ck==1)ans=-inf;else ans=inf;
    while(fx!=fy){
        if(dep[fx]<dep[fy])swap(x,y),swap(fx,fy);
        if(ck==0)ans+=query_s(id[fx],id[x],rt);
        else if(ck==1)ans=max_(ans,query_x(id[fx],id[x],rt));
        else if(ck==2)ans=min_(ans,query_n(id[fx],id[x],rt));
        x=f[fx],fx=top[x];
    }if(id[x]>id[y])swap(x,y);
    if(ck==0)ans+=query_s(id[x]+1,id[y],rt);
    else if(ck==1)ans=max_(ans,query_x(id[x]+1,id[y],rt));
    else if(ck==2)ans=min_(ans,query_n(id[x]+1,id[y],rt));
    return ans;
}
int main(){dep[0]=1;
    n=read();
    for(register int i=1;i<n;++i){
        int u=read()+1,v=read()+1,w=read();
        add(u,v,w);add(v,u,w);
    }m=read();
    dfs1(1);
    dfs2(1,1);
    build(1,n,rt);
    for(register int i=1;i<=m;++i){
        scanf("%s",s);
        int x=read()+1,y=read()+1;
        if(s[0]=='C')change(rt,y-1,id[x]);
        else if(s[0]=='N')Segment_change(x,y);
        else if(s[0]=='S')printf("%d\n",query(x,y,0));
        else if(s[1]=='A')printf("%d\n",query(x,y,1));
        else if(s[1]=='I')printf("%d\n",query(x,y,2));
    }
    return 0;
}
