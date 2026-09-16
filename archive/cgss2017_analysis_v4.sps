* ============================================================ .
* CGSS 2017 修正版分析语法 v2 .
* 论文: Does Higher Education Still Promote Upward Mobility? .
* ============================================================ .
* 修正内容: .
* 1. ISEI 转换回退逻辑修复 (解决最小值=1的bug) .
* 2. 教育改为 dummy 变量 (参照组=大专) .
* 3. Cohort 模型移除 age (解决共线性) .
* 4. 加入 A27F (14岁常居地) 作为 robustness check .
* 5. 加入 VIF 共线性诊断 .
* 6. 加入样本构建过程的计数输出 .
* ============================================================ .

* ============================================================ .
* 第 0 步: 清理之前的派生变量 (如果重跑) .
* ============================================================ .
* 删除之前派生的变量 (首次运行会警告, 可忽略; 重跑时会成功清理) .
* 每个变量单独一行, 这样即使某个变量不存在, 其他的仍会被删除 .
DELETE VARIABLES female. 
DELETE VARIABLES age. 
DELETE VARIABLES cohort. 
DELETE VARIABLES edu_lvl. 
DELETE VARIABLES rural_origin. 
DELETE VARIABLES rural_origin_f. 
DELETE VARIABLES fa_edu_yrs. 
DELETE VARIABLES mo_edu_yrs. 
DELETE VARIABLES par_edu_yrs. 
DELETE VARIABLES isco_main. 
DELETE VARIABLES isei. 
DELETE VARIABLES in_sample.
DELETE VARIABLES edu_bachelor.
DELETE VARIABLES edu_postgrad.
DELETE VARIABLES bach_x_rural.
DELETE VARIABLES post_x_rural.
DELETE VARIABLES cohort2.
DELETE VARIABLES cohort3.
DELETE VARIABLES cohort4.
DELETE VARIABLES bach_x_c2.
DELETE VARIABLES bach_x_c3.
DELETE VARIABLES bach_x_c4.
DELETE VARIABLES post_x_c2.
DELETE VARIABLES post_x_c3.
DELETE VARIABLES post_x_c4.
EXECUTE.

* ============================================================ .
* 第 1 步: 基础变量重编码 .
* ============================================================ .

RECODE A2 (1=0) (2=1) INTO female.
VARIABLE LABELS female 'Female (0=male, 1=female)'.
VALUE LABELS female 0 'Male' 1 'Female'.

COMPUTE age = 2017 - A31.
VARIABLE LABELS age 'Age in 2017'.

RECODE A31 (1957 thru 1964=1) (1965 thru 1974=2) (1975 thru 1984=3) (1985 thru 1992=4)
  (ELSE=SYSMIS) INTO cohort.
VARIABLE LABELS cohort 'Birth cohort'.
VALUE LABELS cohort 1 '1957-64' 2 '1965-74' 3 '1975-84' 4 '1985-92'.

* 教育 3 层级 .
RECODE A7A (9 thru 10=1) (11 thru 12=2) (13=3) (ELSE=SYSMIS) INTO edu_lvl.
VARIABLE LABELS edu_lvl 'Higher education level'.
VALUE LABELS edu_lvl 1 'Junior college' 2 'Bachelor' 3 'Postgraduate'.

* 教育 dummy 变量 (参照组=大专) .
COMPUTE edu_bachelor = 0.
COMPUTE edu_postgrad = 0.
IF (edu_lvl = 2) edu_bachelor = 1.
IF (edu_lvl = 3) edu_postgrad = 1.
VARIABLE LABELS edu_bachelor 'Bachelor (ref=junior college)'.
VARIABLE LABELS edu_postgrad 'Postgraduate (ref=junior college)'.

* 城乡出身: 主变量 A27H (14岁户口登记地) .
RECODE A27H (1 thru 2=1) (3 thru 5=0) (ELSE=SYSMIS) INTO rural_origin.
VARIABLE LABELS rural_origin 'Rural origin at age 14 (hukou)'.
VALUE LABELS rural_origin 0 'Urban origin' 1 'Rural origin'.

* 城乡出身: Robustness check A27F (14岁常居地) .
RECODE A27F (1 thru 2=1) (3 thru 5=0) (ELSE=SYSMIS) INTO rural_origin_f.
VARIABLE LABELS rural_origin_f 'Rural origin at age 14 (residence, robustness)'.
VALUE LABELS rural_origin_f 0 'Urban origin' 1 'Rural origin'.

* 父母教育 .
RECODE A89B (1=0) (2=2) (3=6) (4=9) (5=12) (6=12) (7=12) (8=12)
  (9=15) (10=15) (11=16) (12=16) (13=19) (14=9) (ELSE=SYSMIS) INTO fa_edu_yrs.
RECODE A90B (1=0) (2=2) (3=6) (4=9) (5=12) (6=12) (7=12) (8=12)
  (9=15) (10=15) (11=16) (12=16) (13=19) (14=9) (ELSE=SYSMIS) INTO mo_edu_yrs.
VARIABLE LABELS fa_edu_yrs 'Father years of education'.
VARIABLE LABELS mo_edu_yrs 'Mother years of education'.

COMPUTE par_edu_yrs = MAX(fa_edu_yrs, mo_edu_yrs).
VARIABLE LABELS par_edu_yrs 'Highest parental education (years)'.
EXECUTE.

* ============================================================ .
* 第 2 步: ISCO-08 -> ISEI 转换 (修复版) .
* ============================================================ .
* 合并当前工作和最近非农工作 .
COMPUTE isco_main = isco08_a59.
IF (MISSING(isco_main) OR isco_main <= 0 OR isco_main >= 10000) isco_main = isco08_a60.
IF (isco_main <= 0 OR isco_main >= 10000) isco_main = $SYSMIS.
VARIABLE LABELS isco_main 'ISCO-08 code (current or recent non-farm)'.
EXECUTE.

* 初始化 ISEI 为系统缺失 .
COMPUTE isei = $SYSMIS.
EXECUTE.

* --- 4位精确匹配 (基于 Ganzeboom & Treiman 2010 ISEI-08 表, 核心职业) --- .
* Managers .
IF (isco_main=1111) isei=70.
IF (isco_main=1112) isei=77.
IF (isco_main=1120) isei=70.
IF (isco_main=1211) isei=69.
IF (isco_main=1212) isei=67.
IF (isco_main=1213) isei=73.
IF (isco_main=1219) isei=67.
IF (isco_main=1221) isei=65.
IF (isco_main=1222) isei=67.
IF (isco_main=1223) isei=67.
IF (isco_main=1311) isei=50.
IF (isco_main=1321) isei=56.
IF (isco_main=1322) isei=57.
IF (isco_main=1330) isei=71.
IF (isco_main=1341) isei=56.
IF (isco_main=1342) isei=65.
IF (isco_main=1345) isei=71.
IF (isco_main=1346) isei=65.
IF (isco_main=1411) isei=47.
IF (isco_main=1412) isei=39.
IF (isco_main=1420) isei=47.
IF (isco_main=1431) isei=47.
* Professionals .
IF (isco_main=2111) isei=80.
IF (isco_main=2112) isei=80.
IF (isco_main=2113) isei=78.
IF (isco_main=2114) isei=75.
IF (isco_main=2120) isei=76.
IF (isco_main=2131) isei=76.
IF (isco_main=2132) isei=65.
IF (isco_main=2141) isei=75.
IF (isco_main=2142) isei=73.
IF (isco_main=2143) isei=76.
IF (isco_main=2144) isei=75.
IF (isco_main=2145) isei=75.
IF (isco_main=2146) isei=75.
IF (isco_main=2149) isei=75.
IF (isco_main=2151) isei=76.
IF (isco_main=2152) isei=76.
IF (isco_main=2161) isei=76.
IF (isco_main=2162) isei=76.
IF (isco_main=2164) isei=75.
IF (isco_main=2165) isei=73.
IF (isco_main=2166) isei=67.
IF (isco_main=2211) isei=88.
IF (isco_main=2212) isei=88.
IF (isco_main=2221) isei=60.
IF (isco_main=2222) isei=60.
IF (isco_main=2230) isei=60.
IF (isco_main=2240) isei=60.
IF (isco_main=2250) isei=78.
IF (isco_main=2261) isei=76.
IF (isco_main=2262) isei=73.
IF (isco_main=2263) isei=65.
IF (isco_main=2310) isei=77.
IF (isco_main=2320) isei=71.
IF (isco_main=2330) isei=71.
IF (isco_main=2341) isei=67.
IF (isco_main=2342) isei=65.
IF (isco_main=2351) isei=70.
IF (isco_main=2352) isei=65.
IF (isco_main=2353) isei=60.
IF (isco_main=2359) isei=65.
IF (isco_main=2411) isei=71.
IF (isco_main=2412) isei=71.
IF (isco_main=2413) isei=71.
IF (isco_main=2421) isei=70.
IF (isco_main=2422) isei=70.
IF (isco_main=2423) isei=65.
IF (isco_main=2431) isei=65.
IF (isco_main=2432) isei=65.
IF (isco_main=2433) isei=65.
IF (isco_main=2511) isei=71.
IF (isco_main=2512) isei=71.
IF (isco_main=2513) isei=71.
IF (isco_main=2514) isei=71.
IF (isco_main=2519) isei=71.
IF (isco_main=2521) isei=65.
IF (isco_main=2522) isei=65.
IF (isco_main=2611) isei=85.
IF (isco_main=2612) isei=85.
IF (isco_main=2619) isei=85.
IF (isco_main=2621) isei=67.
IF (isco_main=2622) isei=67.
IF (isco_main=2631) isei=71.
IF (isco_main=2632) isei=71.
IF (isco_main=2633) isei=71.
IF (isco_main=2634) isei=71.
IF (isco_main=2635) isei=65.
IF (isco_main=2636) isei=65.
IF (isco_main=2641) isei=65.
IF (isco_main=2642) isei=65.
IF (isco_main=2643) isei=65.
IF (isco_main=2651) isei=56.
IF (isco_main=2652) isei=56.
IF (isco_main=2653) isei=56.
IF (isco_main=2654) isei=56.
IF (isco_main=2655) isei=56.
IF (isco_main=2656) isei=56.
IF (isco_main=2659) isei=56.
* Technicians .
IF (isco_main=3111) isei=56.
IF (isco_main=3112) isei=56.
IF (isco_main=3113) isei=56.
IF (isco_main=3114) isei=56.
IF (isco_main=3115) isei=56.
IF (isco_main=3117) isei=56.
IF (isco_main=3119) isei=56.
IF (isco_main=3131) isei=56.
IF (isco_main=3132) isei=56.
IF (isco_main=3139) isei=56.
IF (isco_main=3141) isei=56.
IF (isco_main=3142) isei=38.
IF (isco_main=3143) isei=38.
IF (isco_main=3151) isei=56.
IF (isco_main=3152) isei=56.
IF (isco_main=3153) isei=65.
IF (isco_main=3211) isei=56.
IF (isco_main=3212) isei=56.
IF (isco_main=3213) isei=56.
IF (isco_main=3221) isei=43.
IF (isco_main=3222) isei=43.
IF (isco_main=3230) isei=43.
IF (isco_main=3240) isei=43.
IF (isco_main=3251) isei=43.
IF (isco_main=3252) isei=43.
IF (isco_main=3254) isei=43.
IF (isco_main=3257) isei=43.
IF (isco_main=3258) isei=43.
IF (isco_main=3259) isei=43.
IF (isco_main=3311) isei=56.
IF (isco_main=3312) isei=56.
IF (isco_main=3313) isei=56.
IF (isco_main=3314) isei=56.
IF (isco_main=3315) isei=56.
IF (isco_main=3321) isei=56.
IF (isco_main=3322) isei=56.
IF (isco_main=3323) isei=47.
IF (isco_main=3324) isei=47.
IF (isco_main=3331) isei=56.
IF (isco_main=3332) isei=56.
IF (isco_main=3334) isei=56.
IF (isco_main=3339) isei=56.
IF (isco_main=3341) isei=47.
IF (isco_main=3342) isei=47.
IF (isco_main=3343) isei=47.
IF (isco_main=3344) isei=47.
IF (isco_main=3351) isei=56.
IF (isco_main=3352) isei=56.
IF (isco_main=3355) isei=56.
IF (isco_main=3359) isei=56.
IF (isco_main=3411) isei=47.
IF (isco_main=3412) isei=47.
IF (isco_main=3413) isei=47.
IF (isco_main=3421) isei=38.
IF (isco_main=3431) isei=47.
IF (isco_main=3432) isei=47.
IF (isco_main=3433) isei=47.
IF (isco_main=3434) isei=47.
IF (isco_main=3435) isei=47.
IF (isco_main=3511) isei=47.
IF (isco_main=3512) isei=47.
IF (isco_main=3513) isei=47.
IF (isco_main=3514) isei=47.
IF (isco_main=3521) isei=47.
IF (isco_main=3522) isei=47.
* Clerical .
IF (isco_main=4110) isei=45.
IF (isco_main=4120) isei=45.
IF (isco_main=4131) isei=38.
IF (isco_main=4132) isei=38.
IF (isco_main=4211) isei=38.
IF (isco_main=4212) isei=38.
IF (isco_main=4213) isei=38.
IF (isco_main=4214) isei=38.
IF (isco_main=4221) isei=38.
IF (isco_main=4222) isei=38.
IF (isco_main=4223) isei=38.
IF (isco_main=4224) isei=38.
IF (isco_main=4225) isei=38.
IF (isco_main=4226) isei=38.
IF (isco_main=4227) isei=38.
IF (isco_main=4229) isei=38.
IF (isco_main=4311) isei=45.
IF (isco_main=4312) isei=45.
IF (isco_main=4313) isei=45.
IF (isco_main=4321) isei=38.
IF (isco_main=4322) isei=38.
IF (isco_main=4323) isei=38.
IF (isco_main=4411) isei=38.
IF (isco_main=4412) isei=38.
IF (isco_main=4415) isei=38.
IF (isco_main=4419) isei=38.
* Service/Sales .
IF (isco_main=5111) isei=36.
IF (isco_main=5112) isei=36.
IF (isco_main=5113) isei=36.
IF (isco_main=5120) isei=30.
IF (isco_main=5131) isei=30.
IF (isco_main=5132) isei=30.
IF (isco_main=5141) isei=30.
IF (isco_main=5142) isei=30.
IF (isco_main=5151) isei=30.
IF (isco_main=5152) isei=30.
IF (isco_main=5153) isei=30.
IF (isco_main=5161) isei=30.
IF (isco_main=5162) isei=30.
IF (isco_main=5163) isei=30.
IF (isco_main=5164) isei=30.
IF (isco_main=5169) isei=30.
IF (isco_main=5211) isei=30.
IF (isco_main=5212) isei=30.
IF (isco_main=5221) isei=30.
IF (isco_main=5222) isei=36.
IF (isco_main=5223) isei=30.
IF (isco_main=5230) isei=30.
IF (isco_main=5241) isei=30.
IF (isco_main=5242) isei=30.
IF (isco_main=5243) isei=30.
IF (isco_main=5244) isei=30.
IF (isco_main=5245) isei=30.
IF (isco_main=5246) isei=30.
IF (isco_main=5249) isei=30.
IF (isco_main=5311) isei=30.
IF (isco_main=5312) isei=30.
IF (isco_main=5321) isei=25.
IF (isco_main=5322) isei=25.
IF (isco_main=5329) isei=25.
IF (isco_main=5411) isei=35.
IF (isco_main=5412) isei=50.
IF (isco_main=5413) isei=35.
IF (isco_main=5414) isei=35.
IF (isco_main=5419) isei=35.
* Craft/Trades .
IF (isco_main >= 7000 AND isco_main < 8000) isei=30.
* Plant Operators .
IF (isco_main >= 8000 AND isco_main < 9000) isei=25.
* Elementary .
IF (isco_main >= 9000 AND isco_main < 10000) isei=16.
IF (isco_main >= 9300 AND isco_main < 9400) isei=18.
* Armed forces .
IF (isco_main >= 100 AND isco_main < 400) isei=45.
EXECUTE.

* --- 修复版回退: 对于未精确匹配的 ISCO 编码, 按2位大类回退到合理 ISEI --- .
* 修复了 v1 的 bug: 之前错误地把 isei 赋值为 1-9 .
* 每一行都用嵌套 IF, 这样即使当前 isei 不缺失, 不会被覆盖 .
IF (MISSING(isei) AND isco_main >= 1100 AND isco_main < 1200) isei = 70.
IF (MISSING(isei) AND isco_main >= 1200 AND isco_main < 1300) isei = 68.
IF (MISSING(isei) AND isco_main >= 1300 AND isco_main < 1400) isei = 60.
IF (MISSING(isei) AND isco_main >= 1400 AND isco_main < 1500) isei = 45.
IF (MISSING(isei) AND isco_main >= 2100 AND isco_main < 2200) isei = 78.
IF (MISSING(isei) AND isco_main >= 2200 AND isco_main < 2300) isei = 75.
IF (MISSING(isei) AND isco_main >= 2300 AND isco_main < 2400) isei = 70.
IF (MISSING(isei) AND isco_main >= 2400 AND isco_main < 2500) isei = 70.
IF (MISSING(isei) AND isco_main >= 2500 AND isco_main < 2600) isei = 71.
IF (MISSING(isei) AND isco_main >= 2600 AND isco_main < 2700) isei = 70.
IF (MISSING(isei) AND isco_main >= 3100 AND isco_main < 3200) isei = 56.
IF (MISSING(isei) AND isco_main >= 3200 AND isco_main < 3300) isei = 50.
IF (MISSING(isei) AND isco_main >= 3300 AND isco_main < 3400) isei = 50.
IF (MISSING(isei) AND isco_main >= 3400 AND isco_main < 3500) isei = 47.
IF (MISSING(isei) AND isco_main >= 3500 AND isco_main < 3600) isei = 47.
IF (MISSING(isei) AND isco_main >= 4100 AND isco_main < 4200) isei = 45.
IF (MISSING(isei) AND isco_main >= 4200 AND isco_main < 4300) isei = 38.
IF (MISSING(isei) AND isco_main >= 4300 AND isco_main < 4400) isei = 45.
IF (MISSING(isei) AND isco_main >= 4400 AND isco_main < 4500) isei = 38.
IF (MISSING(isei) AND isco_main >= 5100 AND isco_main < 5200) isei = 30.
IF (MISSING(isei) AND isco_main >= 5200 AND isco_main < 5300) isei = 30.
IF (MISSING(isei) AND isco_main >= 5300 AND isco_main < 5400) isei = 28.
IF (MISSING(isei) AND isco_main >= 5400 AND isco_main < 5500) isei = 38.
EXECUTE.

VARIABLE LABELS isei 'ISEI-08 occupational status score'.

* ============================================================ .
* 第 3 步: 筛选分析样本 + Table 1 样本构建数据 .
* ============================================================ .

* Table 1: 样本构建 - 输出每一步的 N .
TEMPORARY.
SELECT IF (A7A >= 9 AND A7A <= 13).
DESCRIPTIVES A7A /STATISTICS=COUNT.

TEMPORARY.
SELECT IF (A7A >= 9 AND A7A <= 13 AND A31 >= 1957 AND A31 <= 1992).
DESCRIPTIVES A7A /STATISTICS=COUNT.

TEMPORARY.
SELECT IF (A7A >= 9 AND A7A <= 13 AND A31 >= 1957 AND A31 <= 1992 AND NOT MISSING(isei)).
DESCRIPTIVES A7A /STATISTICS=COUNT.

* 最终样本筛选 .
COMPUTE in_sample = 0.
IF (NOT MISSING(edu_lvl) AND NOT MISSING(cohort) AND NOT MISSING(isei) 
    AND NOT MISSING(rural_origin) AND NOT MISSING(par_edu_yrs)) in_sample = 1.
EXECUTE.

FILTER OFF.
USE ALL.
SELECT IF (in_sample = 1).
EXECUTE.

* ============================================================ .
* 第 4 步: 描述性统计 (Table 3) .
* ============================================================ .
DESCRIPTIVES VARIABLES = isei age par_edu_yrs fa_edu_yrs mo_edu_yrs
  /STATISTICS = MEAN STDDEV MIN MAX.

FREQUENCIES VARIABLES = female edu_lvl rural_origin cohort /ORDER=ANALYSIS.

* ============================================================ .
* 第 5 步: 关键描述表 (Table 4-6) .
* ============================================================ .

* Table 4: 城乡 × 教育层级 交叉表 (卡方) .
CROSSTABS /TABLES = rural_origin BY edu_lvl 
  /CELLS = COUNT ROW COLUMN /STATISTICS = CHISQ.

CROSSTABS /TABLES = cohort BY edu_lvl 
  /CELLS = COUNT ROW COLUMN /STATISTICS = CHISQ.

* Table 5: ISEI by education × origin .
MEANS TABLES = isei BY edu_lvl BY rural_origin
  /CELLS = MEAN COUNT STDDEV.

* Table 6: ISEI by education × cohort .
MEANS TABLES = isei BY edu_lvl BY cohort
  /CELLS = MEAN COUNT STDDEV.

* ============================================================ .
* 第 6 步: 回归 Table 7 - 城乡出身模型 .
* ============================================================ .

* Model 1: 基础模型 (只有教育 dummy + 性别 + 年龄) .
REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female age.

* Model 2: 加父母教育 .
REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female age par_edu_yrs.

* Model 3: 加城乡出身 .
REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female age par_edu_yrs rural_origin.

* Model 4: 加教育 × 城乡 交互项 .
COMPUTE bach_x_rural = edu_bachelor * rural_origin.
COMPUTE post_x_rural = edu_postgrad * rural_origin.
EXECUTE.

REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female age par_edu_yrs rural_origin 
    bach_x_rural post_x_rural.

* ============================================================ .
* 第 7 步: 回归 Table 8 - Cohort 模型 (不含 age) .
* ============================================================ .

* Cohort dummy (参照组 = 1957-64) .
COMPUTE cohort2 = 0.
COMPUTE cohort3 = 0.
COMPUTE cohort4 = 0.
IF (cohort = 2) cohort2 = 1.
IF (cohort = 3) cohort3 = 1.
IF (cohort = 4) cohort4 = 1.
EXECUTE.

* Model 5: cohort 主效应 (注意: 不含 age) .
REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female par_edu_yrs rural_origin 
    cohort2 cohort3 cohort4.

* Model 6: 加教育 × cohort 交互项 .
COMPUTE bach_x_c2 = edu_bachelor * cohort2.
COMPUTE bach_x_c3 = edu_bachelor * cohort3.
COMPUTE bach_x_c4 = edu_bachelor * cohort4.
COMPUTE post_x_c2 = edu_postgrad * cohort2.
COMPUTE post_x_c3 = edu_postgrad * cohort3.
COMPUTE post_x_c4 = edu_postgrad * cohort4.
EXECUTE.

REGRESSION /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female par_edu_yrs rural_origin 
    cohort2 cohort3 cohort4
    bach_x_c2 bach_x_c3 bach_x_c4 post_x_c2 post_x_c3 post_x_c4.

* ============================================================ .
* 第 8 步: Robustness check - 用 A27F 重跑 Model 3 .
* ============================================================ .
REGRESSION /STATISTICS COEFF OUTS R ANOVA
  /DEPENDENT = isei
  /METHOD = ENTER edu_bachelor edu_postgrad female age par_edu_yrs rural_origin_f.

* ============================================================ .
* 完成 .
* ============================================================ .
EXECUTE.
