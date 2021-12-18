****************“一带一路”倡议对企业国际化程度的影响****************
*设定面板数据
xtset code year
*缩尾
winsor2 sale_por size age growth lev cash fixed roa salary herfin10 comp_por, replace cuts(1 99)
*设置控制变量
global control "lev fixed roa growth cash sale age size"
*双重差分+控制变量，
gen did = time*treat 

***********************平均处理效应***************** 
*r【r:表示使用聚类稳健标准误】
*没有使用固定效应，
reg sale_por did time treat $control ,r
est sto a1
*固定年度和行业效应
*fe表示固定效应估计量
*i.year表示根据变量year的不同取值来生成年度虚拟变量
*双向固定效应，既考虑了个体异质性，又考虑了时间异质性
xtreg sale_por did time treat $control i.year, fe cluster(ind)
est sto a2
*固定企业效应和年度
*r表示聚类稳健标准误，面板数据情况下，默认为企业
xtreg sale_por did time treat $control i.year, fe r
est sto a3
*输出
esttab a1 a2 a3 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

***************************动态边际效应*******************
*需要先生成2014-2018年份每个年份的虚拟变量
xtreg sale_por tt14 tt15 tt16 tt17 tt18  $control ,r
est sto b1
xtreg sale_por tt14  tt15 tt16 tt17 tt18  $control i.year, fe r
est sto b2
xtreg sale_por tt14  tt15 tt16 tt17 tt18  $control i.year, fe cluster(ind)
est sto b3
esttab b1 b2 b3 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps



****平行趋势检验部分（赵克杰）*******
gen period = year-2014
gen pre_3 = (period ==-3&treat==1)
gen pre_2= (period ==-2&treat==1)
gen pre_1= (period ==-1&treat==1)
gen current = (period ==0&treat==1)
gen post_1 = (period ==1&treat==1)
gen post_2= (period ==2&treat==1)
gen post_3= (period ==3&treat==1)
xtreg sale_por time treat pre_3 pre_2 pre_1 current post_1 post_2 post_3 i.year, fe
est sto reg
coefplot reg,keep(pre_3 pre_2 pre_1 current post_1 post_2 post_3) vertical recast(connect) yline(0)

****************分样本检验***************
***国有非国有
xtreg sale_por did time treat $control i.year if state==1, fe cluster(ind)
est sto c1
xtreg sale_por did time treat $control i.year if state==1, fe r
est sto c2
xtreg sale_por did time treat $control i.year if state==0, fe cluster(ind)
est sto c3
xtreg sale_por did time treat $control i.year if state==0, fe r
est sto c4
esttab c1 c2 c3 c4 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

*****董事有无海外背景
xtreg sale_por did time treat $control i.year if sea_ex==1, fe cluster(ind)
est sto d1
xtreg sale_por did time treat $control i.year if sea_ex==1, fe r
est sto d2
xtreg sale_por did time treat $control i.year if sea_ex==0, fe cluster(ind)
est sto d3
xtreg sale_por did time treat $control i.year if sea_ex==0, fe r
est sto d4
esttab d1 d2 d3 d4 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

*****两职是否分离
xtreg sale_por did time treat $control i.year if dual==1, fe cluster(ind)
est sto e1
xtreg sale_por did time treat $control i.year if dual==1, fe r
est sto e2
xtreg sale_por did time treat $control i.year if dual==0, fe cluster(ind)
est sto e3
xtreg sale_por did time treat $control i.year if dual==0, fe r
est sto e4
esttab e1 e2 e3 e4 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps
*************************888




****************其他稳健性检验
****倾向得分匹配
set  seed  10101
gen tmp = runiform()
sort  tmp
psmatch2 treat $control,outcome(sale_por) radius cal(0.01) logit
pstest $control, both graph

xtreg sale_por did time treat $control i.year if _support==1, fe cluster(ind)
est sto f3
esttab f3 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

****更换代理变量
xtreg comp_por did time treat $control i.year, fe cluster(ind)
est sto g1
esttab g1 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

****删除2013年数据
drop if year==2013
xtreg sale_por did time treat $control i.year, fe cluster(ind)
est sto h1
esttab h1 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps

*******安慰剂检验1**********
********************************随机抽取处理组的形式******************
cd D:\桌面\能使用的数据
* 系数矩阵
mat b = J(500,1,0)
* 标准误矩阵
mat se = J(500,1,0)
* P值矩阵
mat p = J(500,1,0)


forvalues i=1/500{
  * 循环500次
 use data.dta, clear
 xtset code year  //平衡面板数据
 keep if year == 2014
 sample 244, count
 keep code
 save matchcode.dta, replace
 merge 1:m code using data.dta //与原数据匹配
 replace treat = (_merge == 3) //将所抽取样本赋值为1，其余为0，得到地区处理变量
 replace time = (year >= 2014) //生成时间处理变量
 gen did1 = treat*time
 xtreg sale_por did1 $control i.year, fe cluster(ind)
  
  * 将回归结果赋值到对应矩阵的对应位置
 mat b[`i',1] = _b[did]
 mat se[`i',1] = _se[did]
  mat p[`i',1] = 2*ttail(e(df_r), abs(_b[did]/_se[did]))
}

  * 矩阵转化为向量
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)
* 删除空值并添加标签
drop if pvalue1 == .
label var pvalue1 p值
label var coef1 估计系数

*画图1【估计系数分布】
dpplot coef1  ,xline(0,lc(black*0.5)  ) xline(-14.414,lc(red*0.5) lp(dash) )
*画图2【估计系数分布和p值，但不会双轴，并且不会标签添加】
twoway (scatter pvalue1 coef1, xline(0 0.0477, lwidth(0.2) lp(shortdash)) xlabel(-0.05(0.01)0.1, grid) xtitle(估计系数) ytitle(p值) msymbol(smcircle_hollow) mcolor(orange) legend(off)) (kdensity coef1, title(安慰剂检验))
******************************************************************
*****安慰剂检验2，换数据
*数据为 安慰剂检验部分09-13
xtset code year
*缩尾
winsor2 sale_por size age growth lev cash fixed roa salary herfin10, replace cuts(1 99)
*设置控制变量
global control "lev fixed roa growth cash sale age size"
*双重差分+控制变量，
gen did2 = post*treat 
xtreg sale_por did2 post treat $control i.year, fe cluster(ind)
est sto i1
esttab i1 ,scalar(r2_w N)compress star(* 0.1 ** 0.05 *** 0.01)b(%6.4f)p(%6.4f) nogaps
