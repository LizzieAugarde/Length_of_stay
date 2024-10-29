/****************** Length of stay 2023 ******************

* Extracts multiple cohort of diagnoses for linkage to HES, following LOS SOP
* Each cohort uses a different year of diagnosis to assess the impact of 2020 and 2021 HES disruption on LOS 
* Will need to update these cohorts to come from AV2021 once it is available 
* Created November 2023 by Lizzie Augarde 

**********************************************************/

drop table los2014_cohort purge;

------2014 diagnoses------
create table los2014_cohort compress basic nologging as 
select * from
(select t.tumourid, 
        t.patientid,
        t.diagnosisdatebest, 
        p.deathdatebest,
        p.birthdatebest, 
        p.gender,
        p.ethnicity,
        t.site_icd10r4_o2_3char_from2013,
        t.stage_best,
        t.stage_best_system, 
        t.stage_pi,
        t.stage_pi_detail,
        i.imd19_decile_lsoas,
        i.imd19_quintile_lsoas,
        s.ndrs_main,
        r.final_route,
        t.diagnosisdatebest as follow_up_start, 
        coalesce(least(t.deathdatebest, t.diagnosisdatebest + 1825), t.diagnosisdatebest + 1825) as follow_up_end, --5 year follow up = 1825 days
        --need to rank by tumourid too as could have 2 tumours diagnosed on same day so both of these would have rank of 1 and be pulled through, but only want 1 tumour per patient so that tumour will be randomly picked based on which of the 2 tumour ids is smaller
        rank () over (partition by t.patientid order by t.diagnosisdatebest, t.tumourid asc) as rank
from av2021.at_tumour_england@casref01 t
left join av2021.at_patient_england@casref01 p on t.patientid = p.patientid
left join av2021.at_geography_england@casref01 g on t.tumourid = g.tumourid --adding IMD
left join imd.imd2019_equal_lsoas@casref01 i on g.lsoa11_code = i.lsoa11_code --adding IMD
left join analysispollyjeffrey.at_site_england@casref01 s on t.tumourid = s.tumourid --including site from new site table 
left join av2020.rtd2020 r on t.tumourid = r.tumourid --adding route to diagnosis
where t.diagnosisyear = 2014
and t.cascade_inci_flag = 1 --standard CAS exclusions
and t.dco = 'N' --excluding those diagnosed on death certificate only 
and t.dedup_flag = 1 --excluding duplicate records
and ((t.deathdatebest is null) or (t.diagnosisdatebest != t.deathdatebest)) --excluding those diagnosed on the same day as death (not all captured by DCO = N)
and ((t.deathdatebest is null) or (t.deathdatebest - t.diagnosisdatebest > 0)) --excluding those diagnosed after death (not all captured by DCO = N)
and substr(t.stage_best,1,1) in ('1','2','3','4') --excludes those diagnosed at stage 0 
and substr(t.site_icd10_o2_3char, 1, 1) = 'C' --include C codes only 
and t.site_icd10_o2_3char <> 'C44' --exclude skin cancer
and not exists(--excluding patients with a previous cancer diagnosis
        select 1 
        from av2021.at_tumour_england@casref01 t2
        where t.patientid = t2.patientid
        and t2.diagnosisyear < 2014
        and t2.diagnosisyear > 2000 -- limiting to 2001+ diagnoses as per counting cases SOP
        and t2.cascade_inci_flag = 1
        and t2.dedup_flag = 1
        and substr(t2.site_icd10_o2_3char, 1, 1) = 'C' 
        and t2.site_icd10_o2_3char <> 'C44'
        and t.tumourid <> t2.tumourid)
order by t.patientid)
where rank = 1;

CREATE BITMAP INDEX analysiselizabethaugarde.los2014_cohort_ix ON analysiselizabethaugarde.los2014_cohort (patientid)  NOLOGGING TABLESPACE analysisdata_ix;


