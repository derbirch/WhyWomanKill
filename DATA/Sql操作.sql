USE [OpenLaw]
select * from [new2019] WHERE 标题='杨某某、邓某某其他民事令'
DELETE from [new2019] WHERE 标题 is null


/*insert into [new2019] */
select * from [01010301] 

select * from [06251120] 

select * from [05010624] 

select * from [03160430]

select * from [03010315]


select * from new2019 WHERE 案件类型='刑事' AND 标题 LIKE '%杀%' AND 庭审过程 LIKE '%死亡%'
select * from sum2019 WHERE 当事人 LIKE '%原告%男%被告%女%' AND 当事人 NOT LIKE '%原告%女%被告%男%' AND 案件类型='刑事'

SELECT COLLATIONPROPERTY('Chinese_PRC_Stroke_CI_AI_KS_WS', 'CodePage') 