SELECT AREA,                                                   --所属片区
       DEALER_NO,                                              --客户编号
       DEALER_NAME,                                            --客户名称
       WARE_NO,                                                --仓库编号
       WARE_NAME,                                              --仓库名称
       FSCL_DATE,                                              --第一单录入时间
       NVL(LOGIN_DAYS,0) LOGIN_DAYS,                           --登录天数
       BUSINESS_DAYS,                                     	   --总营业天数
       EXCLUDE_DAYS,                   						   --装修排除天数
           ((CASE WHEN NVL(BUSINESS_DAYS,0)=0 THEN '0%'
            ELSE ROUND(NVL(LOGIN_DAYS,0) / BUSINESS_DAYS * 100, 2)
           || '%' END)) LOGIN_DAYS_RATIO,                      --登录天数占比
           NVL(SALE_DAYS,0) SALE_DAYS,                         --录入销售天数
           BUSINESS_DAYS TTL_SALE_DAYS,                        --总销售天数
           NVL(RLB_QTY,0) RLB_QTY,                             --录入销售单数
       (
        (
          CASE
            WHEN NVL(BUSINESS_DAYS,0)=0 THEN '0%' --第一单录入时间大于查询结束时间
			WHEN BUSINESS_DAYS = -0.01439323 THEN '该时段未上线'
            WHEN BUSINESS_DAYS <>0 AND NVL((BUSINESS_DAYS-EXCLUDE_DAYS),0)=0 THEN '装修全满'
            ELSE 
				CASE
					WHEN (NVL(SALE_DAYS,0) / (BUSINESS_DAYS-EXCLUDE_DAYS)) > 1 THEN '100%' --装修期间仍在录销售
					ELSE ROUND(NVL(SALE_DAYS,0) / (BUSINESS_DAYS-EXCLUDE_DAYS) * 100, 2) || '%'
					END
            END
        )
      ) SALE_DAYS_RATIO,                  					   --录入销售天数占比
      
           NVL(NEG_SKU_QTY,0) NEG_SKU_QTY,                     --负库存SKU数量
           NVL(SKU_QTY,0) SKU_QTY,                             --SKU总量
           ((CASE WHEN NVL(SKU_QTY,0)=0 THEN '0%'
            ELSE
           TO_CHAR(ROUND(NVL(NEG_SKU_QTY,0)/NVL(SKU_QTY,0)*100, 2)) || '%' END)
           ) NEG_SKU_RATIO,                                    --负库存SKU数量占比
           NVL(PUC_QTY,0) PUC_QTY,                             --收货单据总数
           NVL(NIN_PUC_QTY,0) NIN_PUC_QTY,                     --收货超15天单据数
           ((CASE WHEN NVL(PUC_QTY,0)=0 THEN '0%'
            ELSE
           TO_CHAR(ROUND(NVL(NIN_PUC_QTY,0)/NVL(PUC_QTY,0)*100, 2)) || '%' END)
           ) NIN_PUC_RATIO,                                     --超时收货单占比
           NVL(TFN_QTY,0) TFN_QTY,                              --门店调拨收货单总数
           NVL(NIN_TFN_QTY,0) NIN_TFN_QTY,                      --收货超7天调拨单数
           ((CASE WHEN NVL(TFN_QTY,0)=0 THEN '0%'
            ELSE
           TO_CHAR(ROUND(NVL(NIN_TFN_QTY,0)/NVL(TFN_QTY,0)*100, 2)) || '%' END)
           ) NIN_TFN_RATIO,                                     --超时调拨单占比
DECODE(UNIT_STATUS, 'A', '活动', '非活动') STATUS
FROM (
  SELECT LSB.AREA,
        D.UNIT_CODE DEALER_NO,
        D.UNIT_NAME DEALER_NAME,
        U.UNIT_CODE WARE_NO,
        U.UNIT_NAME WARE_NAME,
        F.FSCL_DATE,
        C.LOGIN_DAYS,
		/*(CASE 
			WHEN F.FSCL_DATE <= :FROM_DATE
				THEN :TO_DATE - :FROM_DATE + 1
			WHEN F.FSCL_DATE >= :TO_DATE
				THEN 0
			ELSE
				:TO_DATE - F.FSCL_DATE + 1
			END) BUSINESS_DAYS,*/
		--(:TO_DATE-(CASE WHEN F.FSCL_DATE >= :FROM_DATE THEN F.FSCL_DATE ELSE :FROM_DATE END) + 1) BUSINESS_DAYS,
		(
		CASE
			WHEN F.FSCL_DATE > :TO_DATE		--第一单录入时间大于查询结束时间
				THEN -0.01439323
			WHEN F.FSCL_DATE < :FROM_DATE	--第一单录入时间小于查询开始时间
				THEN (:TO_DATE - :FROM_DATE) + 1
			ELSE							--未上线
				(:TO_DATE - F.FSCL_DATE) + 1
			END
		) BUSINESS_DAYS,
        R.SALE_DAYS,
        R.RLB_QTY,
        WS.NEG_SKU_QTY,
        WS.SKU_QTY,
        PU.PUC_QTY,
        PU.NIN_PUC_QTY,
        TF.TFN_QTY,
        TF.NIN_TFN_QTY,
        U.UNIT_STATUS,
	    (	
		CASE 
			WHEN F.FSCL_DATE > :TO_DATE		--第一大类 第一单录入时间大于查询结束时间 未上线使用
				THEN 0
			WHEN F.FSCL_DATE < :FROM_DATE	--第二大类 第一单录入时间小于查询开始时间 查询开始日期前已开始使用
				THEN
					CASE
						WHEN :FROM_DATE > COMPLT_DATE --查询开始时间在装修结束时间之后不产生排除日期
							THEN 0
						ELSE
							(
								CASE
									WHEN (START_DATE IS NOT NULL AND COMPLT_DATE IS NOT NULL) 
										THEN --装修已完成
											(
												CASE
													WHEN (START_DATE<=:FROM_DATE AND COMPLT_DATE<:TO_DATE)   THEN (COMPLT_DATE-:FROM_DATE)+1     --1
													WHEN (START_DATE>:FROM_DATE  AND COMPLT_DATE>=:TO_DATE)  THEN (:TO_DATE-START_DATE)+1        --2
													WHEN (START_DATE>:FROM_DATE  AND COMPLT_DATE<:TO_DATE)   THEN (COMPLT_DATE-START_DATE)+1  --3
													WHEN (START_DATE<=:FROM_DATE AND COMPLT_DATE>=:TO_DATE)  THEN (:TO_DATE-:FROM_DATE)+1          --4
													ELSE 0
												END
											)
									
									WHEN (START_DATE IS NOT NULL AND COMPLT_DATE IS NULL)
										THEN --装修未完成
											(
												CASE
													WHEN START_DATE<=:FROM_DATE THEN (:TO_DATE-:FROM_DATE)+1
													WHEN START_DATE>:FROM_DATE  THEN (:TO_DATE-START_DATE)+1
													ELSE  0
												END
											)
							
									ELSE 0
								END
							)
					END
				
			WHEN :FROM_DATE <= F.FSCL_DATE AND F.FSCL_DATE <= :TO_DATE		--第三大类 第一单录入时间介于查询日期范围内
				THEN
					CASE 
						WHEN :FROM_DATE > COMPLT_DATE --查询开始时间在装修结束时间之后 不产生排除日期
							THEN 0
						WHEN (:FROM_DATE >= START_DATE AND :FROM_DATE <= COMPLT_DATE)  --查开始询时间在装修期内
							THEN
								CASE 
									WHEN F.FSCL_DATE < COMPLT_DATE THEN (F.FSCL_DATE - :FROM_DATE) + 1	--装修期间录第一单 ？？？
									WHEN F.FSCL_DATE >= COMPLT_DATE THEN 0	--第一单录入日期在装修期结后 不产生排除日期
									ELSE 0
								END
						WHEN :FROM_DATE > START_DATE AND :TO_DATE < COMPLT_DATE --查询期间全部在装修
							THEN :TO_DATE - :FROM_DATE	-- 全部排出
						WHEN :TO_DATE < START_DATE
							THEN 0
						WHEN :TO_DATE >= START_DATE AND :TO_DATE <= COMPLT_DATE	--查开始日期小于装修开始日期，查询结束日期在装修期内
							THEN :TO_DATE - START_DATE
						ELSE
							(
								CASE
									WHEN START_DATE IS NOT NULL AND COMPLT_DATE IS NOT NULL	--装修已完成
										THEN
											(
												CASE
													WHEN F.FSCL_DATE < START_DATE  THEN (COMPLT_DATE-START_DATE)+1     --1
													WHEN (START_DATE <= F.FSCL_DATE  AND F.FSCL_DATE <= COMPLT_DATE)  THEN (COMPLT_DATE-START_DATE)+1	--2
													WHEN START_DATE < F.FSCL_DATE THEN (COMPLT_DATE-START_DATE)+1  --3
													ELSE 0
												END
											)
									
									WHEN (START_DATE IS NOT NULL AND COMPLT_DATE IS NULL)	--装修未完成
										THEN
											(
												CASE
													WHEN START_DATE < :FROM_DATE THEN (:TO_DATE - :FROM_DATE)+1
													WHEN START_DATE > :TO_DATE THEN (:FROM_DATE - :TO_DATE)+1
													WHEN :FROM_DATE <= START_DATE AND START_DATE <= :TO_DATE THEN (:TO_DATE-START_DATE)+1
													ELSE  0
												END
											)
							
									ELSE 0
								END
							)
				END
			ELSE 0
		END
			
		 ) EXCLUDE_DAYS  --门店装修排除
    FROM (
        SELECT WM_CONCAT(DISTINCT B.AREA) AREA, B.SHOP_ID FROM LUOLAI_SHOP_BRAND B
		WHERE B.STATUS='T' --按门店品牌状态查询
		AND <BRAND_ID>B.BRAND_ID IN (:BRAND_ID)</BRAND_ID>
		GROUP BY B.SHOP_ID
    ) LSB
    LEFT JOIN SYS_UNIT U
      ON LSB.SHOP_ID = U.UNIT_ID
    LEFT JOIN SYS_UNIT D
      ON U.CTRL_UNIT_ID = D.UNIT_ID
    LEFT JOIN (
       SELECT COUNT(DISTINCT TO_CHAR(H.LOGIN_TIME,'YYYYMMDD')) LOGIN_DAYS, C.SHOP_ID FROM CASH_REG C INNER JOIN SYS_SESSION_H H ON C.CASH_REG_ID=H.MACH_ID
        WHERE H.LOGIN_TIME BETWEEN :FROM_DATE AND :TO_DATE
        GROUP BY C.SHOP_ID
    ) C ON LSB.SHOP_ID=C.SHOP_ID
	LEFT JOIN (
      SELECT R.SHOP_ID, MIN(R.FSCL_DATE) FSCL_DATE FROM RLB R WHERE R.EFFECTIVE='T' GROUP BY R.SHOP_ID
    ) F ON LSB.SHOP_ID=F.SHOP_ID
    LEFT JOIN (
         SELECT R.SHOP_ID, COUNT(DISTINCT R.FSCL_DATE) SALE_DAYS, COUNT(R.RLB_NUM) RLB_QTY FROM RLB R WHERE R.EFFECTIVE='T'
         AND R.FSCL_DATE BETWEEN :FROM_DATE AND :TO_DATE
         GROUP BY R.SHOP_ID
    ) R ON LSB.SHOP_ID=R.SHOP_ID
  
  ----门店装修系统
  LEFT JOIN (
    SELECT DISTINCT RESU.UNIT_ID UNIT_ID, LRE.STORENUMBER UNIT_CODE, LRE.STARTDATE START_DATE, LRE.COMPLETIONDATE COMPLT_DATE
      FROM DRP_DEV.LUOLAI_RENOVATION_EVENT LRE
        LEFT JOIN SYS_UNIT RESU ON RESU.UNIT_CODE=LRE.STORENUMBER
  ) RE ON RE.UNIT_ID=LSB.SHOP_ID
  
    LEFT JOIN (
         SELECT WAREH_ID, SUM(NVL((CASE WHEN WS.STK_ON_HAND < 0 THEN 1 ELSE 0 END),0)) NEG_SKU_QTY, SUM(1) SKU_QTY FROM WAREH_STK WS
         GROUP BY WS.WAREH_ID
    ) WS ON LSB.SHOP_ID=WS.WAREH_ID
    LEFT JOIN (
         SELECT S.VDE_WAREH_ID, SUM(1) PUC_QTY, SUM(NVL((CASE WHEN ((SYSDATE - P.CHK_TIME + 1) > 15 AND REGEXP_LIKE(S.PROGRESS,'DD|RG')) THEN 1 ELSE 0 END),0)) NIN_PUC_QTY FROM PUC P
         INNER JOIN PSC S ON P.PSC_NUM=S.PSC_NUM
         WHERE S.EFFECTIVE='T' AND S.CANCELLED='F' AND S.PROGRESS IN ('DD','RG','RD')
    --AND P.CHK_TIME BETWEEN :FROM_DATE AND :TO_DATE  ##限制未入库单据时间范围##
         GROUP BY S.VDE_WAREH_ID
    ) PU ON LSB.SHOP_ID=PU.VDE_WAREH_ID
    LEFT JOIN (
         SELECT S.WAREH_ID, SUM(1) TFN_QTY, SUM(NVL((CASE WHEN ((SYSDATE - S.CHK_TIME + 1) > 7 AND REGEXP_LIKE(S.PROGRESS,'DD|RG')) THEN 1 ELSE 0 END),0)) NIN_TFN_QTY FROM (
           SELECT T.RCV_WAREH_ID WAREH_ID, T.UNIT_ID,  T.TFN_NUM, T.CHK_TIME,T.EFFECTIVE,T.CANCELLED,T.PROGRESS  FROM TFN T
           UNION ALL
           SELECT B.RCV_WAREH_ID, B.UNIT_ID, B.IBC_NUM, B.CHK_TIME,B.EFFECTIVE,B.CANCELLED,B.PROGRESS FROM IBC B
           UNION ALL
           SELECT C.RCV_WAREH_ID, C.UNIT_ID, C.IRC_NUM, C.CHK_TIME,C.EFFECTIVE,C.CANCELLED,C.PROGRESS FROM IRC C
         ) S
         WHERE EFFECTIVE='T' AND CANCELLED='F' AND S.PROGRESS IN ('DD','RG','RD')
    --AND S.CHK_TIME BETWEEN :FROM_DATE AND :TO_DATE ##限制未入库单据时间范围##
         GROUP BY S.WAREH_ID
    ) TF ON LSB.SHOP_ID=TF.WAREH_ID
   WHERE 1=1
   AND U.UNIT_NAME NOT LIKE '%临时%'
   AND U.UNIT_NAME NOT LIKE '%促销%'
   AND U.UNIT_NAME NOT LIKE '%展示%'
   AND U.UNIT_NAME NOT LIKE '%中庭%'
   AND U.UNIT_NAME NOT LIKE '%内买%'
   AND U.UNIT_NAME NOT LIKE '%内购%'
   AND U.UNIT_NAME NOT LIKE '%内卖%'
   AND U.UNIT_NAME NOT LIKE '%样品%'
   AND U.UNIT_NAME NOT LIKE '%残次%'
   AND U.UNIT_NAME NOT LIKE '%次品%'
   AND U.UNIT_NAME NOT LIKE '%团购%'
   AND U.UNIT_NAME NOT LIKE '%二等品%'
   AND U.UNIT_NAME NOT LIKE '%虚拟%'
   AND U.UNIT_NAME NOT LIKE '%活动%'
   AND U.UNIT_NAME NOT LIKE '%大仓%'
   AND U.UNIT_NAME NOT LIKE '%周转%'
   AND U.UNIT_NAME NOT LIKE '%库房%'
   AND U.UNIT_NAME NOT LIKE '%关店%'
   AND U.UNIT_NAME NOT LIKE '%办公%'
   AND U.UNIT_NAME NOT LIKE '%折扣%'
   AND U.UNIT_NAME NOT LIKE '%礼品%'
   AND U.UNIT_NAME NOT LIKE '%小库%'
   AND U.UNIT_NAME NOT LIKE '%禁用%'
   AND U.UNIT_NAME NOT LIKE '%特卖%'
   AND U.UNIT_NAME NOT LIKE '%商团%'
   AND U.UNIT_NAME NOT LIKE '%尛客%'
   AND U.UNIT_NAME NOT LIKE '%电视%'
   AND U.UNIT_NAME NOT LIKE '%京东%'
   AND U.UNIT_NAME NOT LIKE '%唯品会%'
   AND U.UNIT_NAME NOT LIKE '%飞牛%'
   AND U.UNIT_NAME NOT LIKE '%天猫%'
   AND U.UNIT_NAME NOT LIKE '%微商%'
   AND U.UNIT_NAME NOT LIKE '%官网%'
   AND U.UNIT_NAME NOT LIKE '%淘宝%'
   AND U.UNIT_NAME NOT LIKE '%京东%'
   AND U.UNIT_NAME NOT LIKE '%电商%'
   AND U.UNIT_NAME NOT LIKE '%官方%'
   AND U.UNIT_NAME NOT LIKE '%电购%'
   AND U.UNIT_NAME NOT LIKE '%优众%'
   AND U.UNIT_NAME NOT LIKE '%电购%'
   AND U.UNIT_NAME NOT LIKE '%悠美家居米%'
   AND U.UNIT_NAME NOT LIKE '%魅力惠%'
   AND U.UNIT_NAME NOT LIKE '%优车库米%'
   AND U.UNIT_NAME NOT LIKE '%安吉黄帽子%'
   AND U.UNIT_NAME NOT LIKE '%香氛KA项目%'
   AND U.UNIT_NAME NOT LIKE '%米兰菲丽线上%'
   AND U.UNIT_NAME NOT LIKE '%米兰菲丽线下%'
   AND U.UNIT_NAME NOT LIKE '%洽客微门店%'
   AND U.UNIT_NAME NOT LIKE '%微信%'
   AND U.UNIT_NAME NOT LIKE '%临促%'
   AND D.UNIT_CODE NOT IN ('885414')
   AND U.UNIT_STATUS = 'A'
   AND <UNIT_CODE>U.UNIT_CODE IN (:UNIT_CODE)</UNIT_CODE>
) O
